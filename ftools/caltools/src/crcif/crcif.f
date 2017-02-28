*+CRCIF
	subroutine crcif

	IMPLICIT NONE
c 
c Description:
c     Task to create an empty Calibration Index File with the name
c given by the filename parameter
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None ... isolated GCRCIF (below)
c
c COMMONS/INCLUDES etc
c  common TASK		       : (FTOOLS) standard fatal error message thingy
c 
c Called routines
c  subroutine GCRCIF 	      : (below) Gets parameters from XPI par file
c  subroutine CRTCIF          : (CALLIB) Does the job
c  
c Compilation:
c  subroutines require XPI, CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ron Zellar       (1.0.0:94 Jan 14) Original Version
c  Ron Zellar       (1.1.0:94 Sep 28) crtcif subroutine to CALLIB
c  Ian M George     (2.0.0:95 Jul 11), cosmetics + CALLIB-ization
c  Lorraine Breedon (2.1.0:98 Jun 23), y2k adaptions 

	character(7) version
	parameter (version = '2.1.0')
*- 
c Internals 
	character(160) file
	integer errstat, chatter
	character(25) context
        character(40) taskname
	character(80) message

c Initialize
        COMMON/task/taskname
        taskname ='CRCIF '//version
        errstat = 0

        message = '** CRCIF '//version
        call fcecho(message)
 

C	Get the filename of the CIF to be created
	call gcrcif(file,chatter, errstat)
	if (errstat .ne. 0) goto 999

C	Create the empty Calibration Index File
	call crtcif(chatter,file,errstat)
	
c
999     if(errstat.NE.0) then
          context = 'Incomplete Execution'
          message = '** CRCIF '//version//' ERROR : '// context
          call fcecho(message)
          call fcerr(context)
        else
          message = '** CRCIF '//version//' Finished'
          call fcecho(message)
        endif

	return
	end

C------- End of CRCIF subroutine ---------------------------------------

C-----------------------------------------------------------------------
*+GCRCIF
	subroutine gcrcif(file, chatter, ierr)

	implicit none
	character*(*) file
	integer ierr, chatter

C Description: Gets the parameters from the crcif.par parameter file
c  Gets the parameters from the crcif.par parameter file
c
c User i/ps required (prompted for):
c  FILE        - Input Filename
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  IERR	       - Error Flag (zero = OK)
c
c Origin:
c  Original
c
c Called Routines
c  Incomplete
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ron Zellar       (1.0.0:1994 jan 14) Original Version
c  Ian M George     (2.0.0:1995 Jul 11) Added chatter as passed and stuff
	character(7) version
	parameter (version = '2.0.0')
*- 
c Internals
	character(30)  errstr, wrnstr
	character(80)  message
c Initialize
	ierr = 0
	errstr = '** GCRCIF '//version//' ERROR: '
	wrnstr = '** GCRCIF '//version//' WARNING: '

c Get the CIF filename
	call uclgst('filename',file,ierr)
	if(ierr.NE.0) then
	        message = errstr // 'Getting FILENAME parameter'
	        call fcecho(message)
		return
	endif	

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
	if(ierr.NE.0) then
	        message = errstr // 'Getting CHATTER parameter'
	        call fcecho(message)
		ierr = 0 
		message = ' ... setting CHATTER = 9'
		call fcecho(message)
		chatter = 9
	endif	

c Give user info if requested
	if(chatter.GE.20) then	
	 message = ' ... using GCRCIF ' // version
	 call fcecho(message)
	endif

	return
	end
c -------------------------------------------------------------------------

*+CRTCIF
	subroutine crtcif(chatter, file, ierr)

	implicit none
	integer chatter, ierr
	character*(*) file

c Description:
c   Creates a CIF with name given by file
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  FILE          i   : name of the CIF to be created
c  IERR            o : Error Flag (ierr = 0 on successful completion)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c subroutine CGETLUN      : (CALLIB) Get free FORTRAN logical unit
c subroutine FTBDEF       : (FITSIO) Define BINTABLE stuff
c subroutine FTCLOS       : (FITSIO) Close the FITS file
c subroutine FTCRHD       : (FITSIO) Create a new extension
c subroutine FTPHBN       : (FITSIO) Write header of BINTABLE
c subroutine FTPKYx       : (FITSIO) Write keyword/value
c subroutine OPNPA        : (FITSIO) Open & write null P.array
c subroutine WTINFO       : (FTOOLS) Write message to STDOUT
c subroutine WTERRM       : (FTOOLS) Write internal error message
c subroutine WTFERR       : (FTOOLS) Write internal + fitsio error message
c
c Compilation & Linking
c  link with FITSIO & CALLIB
c
c Origin:
c  Written for the Calibration Database 
c
c Authors/Modification History:
c  Ron Zellar       (94 Jan 14:1.0.0) Original Version
c  Ron Zellar       (94 Sep 28:1.0.1) Split crtcif routine from crcif task
c  Ron Zellar       (94 Oct 13:1.1.0) initialization of errstat
c  Ian M George     (95 Jul 11:2.0.0),cosmetics + chatter & ierr passed
c  Ian M George     (95 Dec 19:2.0.1) added wtinfo & friends
c  Ian M George     (95 Dec 21:2.0.1) call cfreluns at end
c  Lorraine Breedon (98 Jun 23:2.1.0) increase vector length of 
C                                     CAL_VSD and CAL_DATE columns
c                                     to cope with yyyy-mm-dd format
	character(7) version
	parameter (version = '2.1.0')
*- 
c Internals
        character(6) subname
        parameter (subname = 'crtcif')
c ... This parameter defines the number of columns in the CIF
	integer ncols
c ... This parameter defines the CIF format version number
	character(4) vrsnum

	parameter (ncols = 18, vrsnum = ' 1.1')

	character(8) ttype(18),tform(18),tunit(18),extname
	character(80) message,comm
	integer lun,errstat,nrows
	integer tfields,varidat,idx

c Initialize
	ierr = 0
	errstat = 0

c Set up defaults
	nrows = 0
	tfields = ncols
	extname = 'CIF'
	varidat = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c Get a free FORTRAN unit
	call cgetlun(lun)

c Column set-up
	ttype(1) = 'TELESCOP'
	tform(1) = '10A'
	ttype(2) = 'INSTRUME'
	tform(2) = '10A'
	ttype(3) = 'DETNAM'
	tform(3) = '20A'
	ttype(4) = 'FILTER'
	tform(4) = '10A'
	ttype(5) = 'CAL_DEV'
	tform(5) = '20A'
	ttype(6) = 'CAL_DIR'
	tform(6) = '70A'
	ttype(7) = 'CAL_FILE'
	tform(7) = '40A'
	ttype(8) = 'CAL_CLAS'
	tform(8) = '3A'
	ttype(9) = 'CAL_DTYP'
	tform(9) = '4A'
	ttype(10) = 'CAL_CNAM'
	tform(10) = '20A'
	ttype(11) = 'CAL_CBD'
	tform(11) = '630A70'
	ttype(12) = 'CAL_XNO'
	tform(12) = 'I'
	ttype(13) = 'CAL_VSD'
	tform(13) = '10A'
	ttype(14) = 'CAL_VST'
	tform(14) = '8A'
	ttype(15) = 'REF_TIME'
	tform(15) = 'D'
	ttype(16) = 'CAL_QUAL'
	tform(16) = 'I'
	ttype(17) = 'CAL_DATE'
	tform(17) = '10A'
	ttype(18) = 'CAL_DESC'
	tform(18) = '70A'

	Do 500 idx=1,ncols
		tunit(idx) = ' '
500	continue


c Open the CIF & write a null Primary array
        call opnpa(file,chatter,lun,.false.,errstat)
        if(errstat.NE.0) then
		ierr = 1
		goto 998
        endif

C Create the CIF extension
	call ftcrhd(lun,errstat)
        if(errstat.NE.0) then
		call wtferr(subname, version, errstat,
     &			'Problem creating new extension')
		ierr = 1
		goto 998
        endif


C Write the required binary table keywords
	call ftphbn(lun,nrows,tfields,ttype,tform,tunit,extname,
     &       varidat,errstat)
        if(errstat.NE.0) then
                call wtferr(subname, version, errstat,
     &                  'Problem writing BINTABLE keywords')
		ierr = 1
		goto 998
        endif


C Write the CIF version number
	comm = 'Version of CIF format'
	call ftpkys(lun,'CIFVERSN',vrsnum,comm,errstat)
        if(errstat.NE.0) then
                call wtferr(subname, version, errstat,
     &                  'Problem writing CIFVERSN keyword')
		call wtinfo(0,1,1,'continuing regardless')
		errstat = 0
        endif



C Define the binary table data
	call ftbdef(lun,tfields,tform,varidat,nrows,errstat)
        if(errstat.NE.0) then
                call wtferr(subname, version, errstat,
     &                  'Defining BINTABLE')
		ierr = 1
		goto 998
        endif


C Close the file
	call ftclos(lun,errstat)
        if(errstat.NE.0) then
                call wtferr(subname, version, errstat,
     &                  'Closing the CIF file')
		ierr = 1
		goto 998
        endif

c Final error check
998	if(ierr.NE.0) then
          call wterrm(subname, version, ' Fatal - aborting')
	else
	  call wtinfo(chatter,20,1,'CIF created successfully')
	endif

c Free up the bugger
	call cfrelun(lun)

	return
	end

