
*+GTCALF
	subroutine gtcalf(chatter,tele_str,instr_str,detnam_str,
     &  filt_str, codenam_str, strtdate, strtime, stpdate, stptime,
     &	expr_str,maxret,filenam,extno,online,
     &	nret,nfound, status)

	implicit none
	character*(*) tele_str, instr_str, detnam_str, filt_str
	character*(*) codenam_str, expr_str
	character*(*) strtdate, strtime, stpdate, stptime
	integer       maxret
	character*(*) filenam(maxret), online(maxret)
	integer       extno(maxret), nret, nfound, status
	integer       chatter
C
C Description:
C This routine returns the location of calibration datasets located in
C the Calibration Database.
C
C Selection of the appropriate calibration data is based on the values of the
C arguments TELE, INSTR, DETNAM, FILT, CODENAM, STRTDATE, STRTTIME,
C STPDATE, STPTIME, EXPR.
C These arguments respectively describe the mission or telescope, instrument,
C sub-detector, filter, type of dataset, start date & time, stop date & time
C and calibration boundaries for which the returned datasets should be valid.
C In addition to the arguments explicitly listed here, this routine also uses
C the values of the environment variables (logicals) CALDB, and CALDBCONFIG.
C See the Caldb user's guide for info on how these system variables should
C be set.
C
C The maximum number of datasets to return is given by the MAXRET argument.
C Any datasets which meet the selection criteria are returned through the
C FILENAM and EXTNO arrays.  Each element of the FILENAM array contains the
C complete system dependent path (including the filename) to the file where
C the calibration data resides.  The corresponding element of the EXTNO array
C contains the FITS extension number of the calibration data within the
C file.
C
C Arguments:
C chatter (i): Chattiness flag indicating how much output will be
C   	       dumped to STDOUT during execution. Values of 1-10 are
C	       recommended (except suring debugging), with high values
C              giving more verbose o/p. A value of zero will dump nothing
C              to STDOUT, with the exception of Fatal error messages, which
C              will also go to SDTERR.
C tele    (i): The name of the mission or telescope for which the
C              returned datasets should be valid.  Corresponds to the
C              TELESCOP keyword value found in the requested calibration
C              file(s).
C instr   (i): The name of the instrument for which the returned datasets
C              should be valid.  Corresponds to the INSTRUME keyword
C              value found in the requested calibration file(s).
C detnam  (i): The name of the detector for which the returned datasets
C              should be valid.  Corresponds to the DETNAM keyword value
C              found in the requested calibration file(s).  If a '-' is
C              passed, no selection will be made on detector names.
C filt    (i): The name of the filter for which the returned datasets
C              should be valid.  Corresponds to the FILTER keyword value
C              found in the requested calibration file(s).  If a '-' is
C              passed, no selection will be made on filter values.
C codenam (i): The OGIP codename for the requested dataset.  Corresponds
C              to the CCNMxxxx keyword value found in the requested
C              calibration file(s).
C strtdate(i): The start-date when the datasets should be valid. This value
C              should be in dd/mm/yy format.  If 'now' is passed, the
C              current system date is substituted.
C strtime(i):  The time of the day (strtdate) when the dataset should be
C              valid. This value should be in hh:mm:ss.s format.  If 'now' is
C              passed, the current system time is substituted.
C stpdate(i):  The end-date when the datasets should be valid. This value
C              should be in dd/mm/yy format.  If 'now' is passed, the
C              current system date is substituted.
C stptime(i):  The time of the day (stpdate) when the dataset should be
C              valid. This value should be in hh:mm:ss.s format.  If 'now' is
C              passed, the current system time is substituted.
C expr    (i): A boolean expression used to select on calibration
C              boundary parameters.  Calibration boundary parameters are
C              part of the CBDnxxxx keyword values found in the
C              requested calibration file(s).  (e.g. In the string
C              'ENERGY(2-12)keV', ENERGY is the boundary parameter.)
C              Currently, the expression syntax only contains the
C              arithmetic operator ".eq." and the logical operator
C              ".and.".  To request a dataset which is valid for an
C              off-axis angle of 10 arcmins and an energy of 5.0 keV,
C              one would pass an expr value of:
C                        theta.eq.10.and.energy.eq.5
C              If no expr selection is required, a '-' value should be
C              passed.
C
C              NOTE: The expr parser cannot distinguish between certain
C              valid and invalid expressions.  For example, expressions
C              such as:
C                    cor.eq.7.and.and.energy.eq.5
C              will be incorrectly interpreted as "cor=7" and
C              "and.energy=5".  A lex and yacc parser is planned for
C              this routine which will work much better.
C maxret  (i): An integer defining the number of elements in the filenam
C              and extno arguments and the number of datasets to return.
C filenam (i): A character array containing the complete system dependent
C              path(s) the to the file(s) in which the requested
C              calibration data is stored.  Dimensioned using the maxret
C              argument.
C extno   (i): An integer array containing the FITS extension numbers of
C              requested datasets.  Each extension number is valid for
C              the filename found in the corresonding element of the
C              filenam array.  Dimensioned using the maxret argument.
C online  (i): A character array which specifys the on-line/off-line
C              status of the corresponding file in the filenam argument.
C              Each element contains either 'ONLINE' or a string
C              describing the off-line status of the file.  If the
C              string 'ONLINE' is not encountered for a particular
C              filenam element, then this file has been moved to an
C              off-line media (e.g. magnetic tape) and the main program
C              will not be able to access the data.  This routine will
C              monitor the online argument and set a return status of
C              110 when returning if an off-line file is detected.
C nret    (r): The number of entries returned to the filenam extno
C              and online arguments.
C nfound  (r): The nfound argument reports the total number of datasets
C              found during the search which match the selection
C              criteria.  May be larger than maxret.
C status  (r): The status argument returns an integer specifying the
C              success of this routine.  A non-zero status flag
C              means that this routine was unable to return any datasets
C              to the main program, while a zero flag means that
C              everything executed normally.  Here is a complete list of
C              status flags currently in use:
C
C              STATUS     MEANING
C              1          CALDB environment variable (logical) not set
C              2          CALDBCONFIG environment variable (logical)
C                         not set
C              3          Input character strings too long
C              10         Error from rdcnfg subroutine
C                         Unable to read Caldb configuration file
C              40         CIF Does Not Exist
C              50         Unable to determine MJD from date string
C              60         Unable to determine fraction of day from time string
C              75         Unable to parse expr expression
C              79         Error from cifsl subroutine
C                         Unable to select data from CIF
C
C Origin:
C  Updated version of Ron's gtcal.f code
C
C Authors/Modification History:
C Ron Zellar, Feb  3 1993 -- Original Version (qzcif)
C Ron Zellar, Jan 20 1994 -- Moved routine out of quzcif.f
C                            Moved ftupch calls from quzcif
C                            Moved gcfdir call from quzcif
C Ron Zellar, Feb 24 1994 -- Removed day,month,year,hour,min,seconds
C                            arguments and replaced with date and time.
C                            Improved comments, error reporting
C Ron Zellar, May 31 1994 -- Added detnam and filt arguments
C                            Enhanced cifsel to select on filt and
C                            detnam if not ' '.
C                            Changed name of subroutine to gtcal.
C Ron Zellar, Jul 15 1994 -- Added 'now' feature to date and time
C                            Changed special character ' ' to '-' for
C                            detnam and filt.
C                            Added expr argument, parser and checker.
C                            Removed dev, dir arguments and modified
C                            filenam, extno to be arrays.
C                            Added maxret, quiet, and nfound args
C Ron Zellar, Jul 20 1994 -- Changed cifsel to cifsl
C Ron Zellar, Jul 21 1994 -- Changed cexp to evexp.
C Ron Zellar, Aug  3 1994 -- Added caldbvar argument
C Ron Zellar, Aug  3 1994 -- Added online argument
C Ron Zellar, Aug 20 1994 -- Modified to use CALDB environment variable
C Ron Zellar, Sep 12 1994 -- Added nret argument
C Ron Zellar, Nov 18 1994 -- Moved date/time parse and calculations to
C                            dt2mjd and tim2df subroutines.
c Ian M George (1.0.0: 96 Feb 01) RENAMED from gtcal.f, added chatter
c			as passed parameter, cosmetics and allowed '-'
c			for date, time
c Ian M George (1.0.1: 96 Feb 08) fixed start bug when
C                       strtdate/strtime = stpdate/stptime (exactly)
c Ian M George (1.0.2: 96 Jun 03) minor tweaks
c Ian M George (1.0.3: 96 Aug 13) chatter.LT.0 = no o/p
c Peter D Wilson (1.0.4: 98 Mar 01) Copy string parameters to local
c                                   variables before calling ftupch
c Ning Gan (1.0.5: 98 Jul 08) Changed the length of date/time string
c                             to 68.
c Jeff Guerber (1.0.6: 1999-02-08) Commented out the CIF existence inquiry,
c    which interferes with new cfitsio capabilities; rely on cfitsio's
c    error checking (via cifsl2) for this.
c Peter D Wilson (1.1.0: 1999-08-04)
c    Download any FTP files to local directory, returning the local paths.
c    Use any pre-existing files located there if possible.
c Peter D Wilson (1.1.1: 1999-08-11)
c    More tweeks to previous enhancement
c
c
C MFC (1.2, 2008-12-09)
C    removed automatic download of remote files accessed via ftp:// or http://
C     filenames and fixed problems reported by CM, 12-16-08
C
        character(7) version
        parameter (version = '1.2')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'gtcalf')
	character(80)  contxt
	character(160) cif,instdir
	character(256) tele, instr, detnam, filt, codenam, expr
C       character(8)   startdate, startime, stopdate, stoptime
	character(68)   startdate, startime, stopdate, stoptime
	integer       errstat
	integer       fcstln, landpt, randpt, lun
	integer       eqpt,i,min
	logical       last
	double precision djm, startreftime, stopreftime, dayfrac
	logical quiet

C Initialize status flags
	status = 0
	errstat = 0
	startdate = strtdate
	startime = strtime
	stopdate = stpdate
	stoptime = stptime
	last = .false.

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

C       Copy string parameters to local variables
	if ( fcstln(tele_str).gt.256 .or.
     &       fcstln(instr_str).gt.256 .or.
     &       fcstln(codenam_str).gt.256 .or.
     &       fcstln(detnam_str).gt.256 .or.
     &       fcstln(filt_str).gt.256 .or.
     &       fcstln(expr_str).gt.256 ) then
	   status = 3
	   call ftpmsg('Input parameter string is longer ' //
     &                 'than 256 characters')
	   return
	endif
	tele    = tele_str
	instr   = instr_str
	codenam = codenam_str
	detnam  = detnam_str
	filt    = filt_str
	expr    = expr_str

C Convert tele, instr, detnam, filt, codenam and expr into uppercase
	call ftupch(tele)
	call ftupch(instr)
	call ftupch(codenam)
	call ftupch(detnam)
	call ftupch(filt)
	call ftupch(expr)

c Check that the CALDB is defined & available to the user
        call caldb_info(chatter, 'INST', tele, instr, errstat)
        if(errstat.NE.0) then
           contxt = 'CALDB not defined/available'
           if(chatter.GE.0) call wterrm(subname, version,contxt)
           contxt = 'CALDB must be both defined '//
     &          'available in order to run'
           call wtinfo(chatter,1,1, contxt)
	   status = 1
           goto 999
        endif

C Get the location of the CIF
	if(chatter.gt.20) quiet = .false.
	call rdcnfg(tele,instr,quiet,cif,instdir,errstat)
	if (errstat .ne. 0) then
	   if(chatter.GE.0) call wterrm(subname, version,
     &		'Problem reading caldbconfig file')
	   status = 10
	   goto 999
	endif

C	ciflen = fcstln(cif)

C Check to see if CIF exists before proceeding
c	inquire(FILE=cif,EXIST=exst)
c MJT 03July96: changed to .eqv. from .eq.
c	if (exst .eqv. .false.) then
c	  contxt= 'CIF Does Not Exist '//cif(:ciflen)
c	  if(chatter.GE.0) call wterrm(subname, version, contxt)
c	  contxt='Unable to find the specified Calibration Index'//
c     &	     'File'
c	  call wtinfo(chatter,5,2,contxt)
c	  contxt='Contact your Caldb administrator'
c	  call wtinfo(chatter,5,3,contxt)
c	  status = 40
c	  goto 999
c	endif


C Calculate MJD from START date string
	if(startdate.EQ.'-') then
	  djm = -99
	else
	  call dt2mjd(startdate,quiet,djm,errstat)
	  if (errstat .ne. 0) then
	    if(chatter.GE.0) call wterrm(subname, version,
     &	     'Unable to determine MJD from startdate string')
	    status = 50
	    goto 999
	  endif
	endif
C .... Calculate fraction of day
	if(startime.EQ.'-') then
	  dayfrac = 0
	else
	  call tim2df(startime,quiet,dayfrac,errstat)
	  if (errstat .ne. 0) then
	    if(chatter.GE.0) call wterrm(subname, version,
     &	     'Unable to determine fraction of day from startime string')
	    status = 60
	    goto 999
	  endif
	endif
C .... Calculate the requested Reference time
	startreftime = djm+dayfrac


C Calculate MJD from STOP date string
	if(stopdate.EQ.'-') then
	  djm = -99
	else
	  call dt2mjd(stopdate,quiet,djm,errstat)
	  if (errstat .ne. 0) then
	    if(chatter.GE.0) call wterrm(subname, version,
     &	     'Unable to determine MJD from stopdate string')
	    status = 50
	    goto 999
	  endif
	endif
C .... Calculate fraction of day
	if(stoptime.EQ.'-') then
	  dayfrac = 0
	else
	  call tim2df(stoptime,quiet,dayfrac,errstat)
	  if (errstat .ne. 0) then
	    if(chatter.GE.0) call wterrm(subname, version,
     &	     'Unable to determine fraction of day from startime string')
	    status = 60
	    goto 999
	  endif
	endif
C .... Calculate the requested Reference time
	stopreftime = djm+dayfrac

C make sure expr is readable
	if (expr.ne.'-') then
	     landpt = 0
200	     continue

C	     Find the next '.AND.'
	     randpt=index(expr(landpt+1:),'.AND.') + landpt
	     if (randpt.eq.landpt) then
	          randpt=fcstln(expr)+1
	          last = .true.
	     endif

	     eqpt=index(expr(landpt+1:),'.EQ.')+landpt

	     if (.not.((landpt.lt.eqpt).and.(eqpt.lt.randpt))) then
	    	  if(chatter.GE.0) call wterrm(subname, version,
     &	          'Unable to parse expr expression')
		  contxt = 'problematic expr is: '//expr
		  call wtinfo(chatter,1,2,contxt)
	          status = 75
	          goto 999
	     endif

	     if (.not.last) then
	          landpt = randpt + 4
	          goto 200
	     endif
	endif

C Select valid entries
	call cifsl2(chatter,cif,tele,instr,detnam,filt,codenam,
     &	   startreftime,
     &     stopreftime, expr,maxret,filenam,extno,online,
     &	   nfound,errstat)

C If errstat is not zero, then an error occurred
	if ((errstat .ne. 0)) then
	  status = 79
	  goto 999
	else if (nfound.le.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'No datasets found matching selection criteria')
	  goto 999
	endif

C Assign the number of entries found to the nret argument
	nret = min(maxret,nfound)

C
C Test that each file is 'ONLINE', flagging a nonfatal error, if not.
C Also, if the file is an ftp URL, copy it to the local directory first.
C
C       First, grab the local directory path and allocate a unit number
C	curDir = ' '
C	dirLen = fcstln(curDir)
	call ftgiou(lun,errstat)

C       Now, test each file in sequence
	Do 500 i=1,nret
	   if (online(i).ne.'ONLINE') then
	      status = -2
C   
C  check for urls of form ftp:// or http://
C
!	   else if ( ( filenam(i)(1:6) .eq. 'ftp://') .or. 
!     &	   (filenam(i)(1:7) .eq. 'http://')) then
!
!C       Locate the final '/' in path
!	      fnameLen = fcstln( filenam(i) )
!	      do 300 endPos=fnameLen,1,-1
!		 if( filenam(i)(endPos:endPos).eq.'/' ) goto 400
! 300	      continue
! 400	      continue
!	      rootName = filenam(i)(endPos+1:fnameLen)
!	      rootLen  = fnameLen - endPos
!
!C       Does this file already exist here?
!	      INQUIRE(FILE=rootName, EXIST=exst)
!	      if( exst ) then
!		 call wtwarm(subname, version, chatter, 10,
!     &	                    'Local file exists. ' //
!     &                      'Using it instead of ftp version.')
!		 contxt = ' Using local '//rootName
!		 call wtinfo(chatter,10,2,contxt)
!
!	      else
!
!		 contxt = ' Downloading '//rootName
!		 call wtinfo(chatter,10,2,contxt)
!
!C       Build the CFITSIO extended filename for opening/copying it
!!		 openedFile = filenam(i)(1:fnameLen)//
!!     &                       '('//rootName(1:rootLen)//')'
!		 openedFile = filenam(i)(1:fnameLen)
!
!                write(*,*) 'OpenedFile = ',openedFile
!		 call ftopen(lun,openedFile,1,blocksz,errstat)
!		 call ftclos(lun,errstat)
!		 if( errstat.ne.0 ) then
!		    if( chatter.ge.0 ) then
!		       call wterrm(subname, version, 'Unable to ' //
!     &                             'download calibration file')		       
!		       contxt = ' Problem file is '//openedFile
!		       write(*,*) openedFile
!		       call wtinfo(chatter,1,1,contxt)
!		    endif
!		    goto 501
!		 endif
!	      endif
!
!	      filenam(i) = rootName(1:rootLen)
	   endif
500	continue
C   501	continue
	i = 0
	call ftfiou(lun,i)


c Final error checking
999	if (((errstat.ne.0).or.(status.gt.0)).AND.(chatter.GE.0)) then
          call wterrm(subname, version, ' Fatal - aborting')
        endif

	return
	end

C***********************************************************************
C*   GTCAL wrapper
C
C    Redirect calls to GTCAL to GTCALF
C
C***********************************************************************

        subroutine gtcal(tele_str, instr_str, detnam_str, filt_str,
     &            codenam_str, date, time, expr_str, quiet, maxret,
     &            filenam, extno, online, nret, nfound, status)

        implicit none
        character*(*) tele_str, instr_str, detnam_str, filt_str
        character*(*) codenam_str, expr_str
        character*(*) date, time
        logical       quiet
        integer       maxret
        character*(*) filenam(maxret), online(maxret)
        integer       extno(maxret), nret, nfound, status

	integer       chatter

	if( quiet ) then
	   chatter = 0
	else
	   chatter = 5
	endif

	call gtcalf(chatter, tele_str, instr_str, detnam_str,
     &              filt_str, codenam_str, date, time, date, time,
     &	            expr_str, maxret, filenam, extno, online,
     &	            nret, nfound, status)

        return
        end
