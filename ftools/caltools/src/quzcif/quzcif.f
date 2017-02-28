*+QUZCIF
       subroutine quzcif()
       implicit none

C-----------------------------------------------------------------------
C Description: Displays entries from a Calibration Index File which are
C              valid for the mission, instrument, codename, date, and
C              time specified by the user.  In addition, the user may
C              conduct more restrictive searches using a boolean
C              expression which selects on calibration boundary
C              parameters.
C
C Arguments:    None
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Feb 3), original version
C               Ron Zellar (1993 Jun 14), added trinst subroutine
C               Ron Zellar (1993 Jun 14), added prscreen param
C               Ron Zellar (1994 Jan 20), Moved qzcif out of quzcif
C                                         Moved ftupch calls to qzcif
C                                         Moved gcfdir call to qzcif
C               Ron Zellar (1994 Jul 15), Reconfigured to use gtcal
C                                         subroutine.  Moved "now"
C                                         feature to gtcal, added params
C                                         detector, filter, expr, maxret
C                                         and nfound.  Removed params
C                                         prscreen, path, and extno.
C
C              Lorraine Breedon 24 Jun 1998 - modifications for y2k
C                                             problem  version 1.2
C              Jeff Guerber (1999-03-26), Removed Lorraine's date parameter
C                   format check and reprompt loop (gpqzcf), which rejected
C                   old-style dates and looped forever if the bad date had
C                   been entered on the command line; lower-level routines
C                   (eg. dt2mjd) check the format anyway.  Increased sizes
C                   of date and time strings.  Version 1.3.
C              PDW (1999-08-11), Replaced gtcal with gtcalf and added
C                   chatter parameter
C
C               MFC (2008-12-09) VERSION 1.3
C                   added "retrieve" parameter to allow user option of
C                   downloading remote files via ftp or http
C               MFC (2009-09-15) version 1.4.1
C                    if retrieve="yes" fecho the name of the local file
C                       (with the http://... stripped off)
C               MFC (2009-10-07) version 1.4.2
C                   fixed bug where retrieve="yes" gave an empty string
C                   if local caldb used
C               MFC (2009-10-09) version 1.4.3 
C                   initialized "lun" variable to fix a segfault in 
C                   Centos linux
C               MFC (2009-10-12) version 1.4.3.1 
C                   restored behavior from 1.4.1 which was lost in 1.4.2 and 
C                   1.4.3, namely:
C                   retrieve+ with remote access: downloads file, reports 
C                        root file name (with url stripped off)
C                   retrieve- with remote access: does not download but
C                        reports full url to file
C                   retrieve+, local access: does not download, reports
C                        full path to file
C                   retrieve-, local access: no download, reports
C                        full path to file
C               MFC (2009-10-13) version 1.4.4 - added clobber param
C-----------------------------------------------------------------------
*-Version 1.4.4

	character(10) tele, instr, filter
	character(20) codenam,detnam
	character(80) contxt
	character(68) time, date
	character(160) expr
	integer errstat, maxret, nfound, chatter
        logical retrieve, delexist
	character(40) taskname
	common /task/ taskname

	taskname = 'quzcif 1.4.3.1'

	date = ' '
	time = ' '

C	Get the parameters from the par file
	call gpqzcf(tele,instr,detnam,filter,codenam,date,time,
     &  expr,maxret,retrieve,chatter,delexist,errstat)

C	If there's an error getting parameters then return
	if (errstat .ne. 0) return

C	The qcif routine has allocated a 50 element array to store
C	the filenames. Make sure maxret is < 50.
        if (maxret.gt.50) then
	     contxt='Warning: maxret cannot be greater than 50'
	     call fcecho(contxt)
	     contxt='Resetting maxret to 50'
	     call fcecho(contxt)
	     maxret=50
	endif

C	Get the requested calibration filenames and display them
	call qcif(tele,instr,detnam,filter,codenam,date,time,
     &  expr,chatter,maxret,retrieve,delexist,nfound,errstat)

	call ppqzcf(nfound)

	return
	end

C-----------------------------------------------------------------------

*+GPQZCF
	subroutine gpqzcf(tele,instr,detnam,filter,codenam,date,
     &  time,expr,maxret,retrieve,chatter,delexist,status)

	implicit none
        character*(*) tele,instr,detnam,filter,codenam,date,time,expr
	integer maxret,status,chatter
        logical retrieve, delexist

C---------------------------------------------------------------------
C Description:  Gets the parameters for QUZCIF from the parameter
C               file.  If date or time is input as 'NOW', the system
C               date or time is returned from this routine.
C
C Arguments:    tele     (r) : The value of the 'mission' param
C               instr    (r) : The value of the 'instrument' param
C               detnam   (r) : The value of the 'detector' param
C               filter   (r) : The value of the 'filter' param
C               codenam  (r) : The value of the 'codename' param
C               date     (r) : The value of the 'date' param
C               time     (r) : The value of the 'time' param
C               expr     (r) : The value of the 'expr' param
C               maxret   (r) : The maximum number of files to return
C               retrieve (r) : download files if true
C               chatter  (r) : chattiness of operations
C               status   (r) : The status of the subroutine
C                              = 0  --> OK
C                              = 1  --> Problem getting prameters
C
C Origin:       Written for the Calibrtion Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Feb 3), original version
C               Ron Zellar (1993 Jun 14), added prscreen param
C               Ron Zellar Jun 28, 1994 -- removed prscreen param,
C                                          path, and extno params.
C                                          Added detnam, filter, expr
C                                          and maxret params.  Removed
C                                          "now" feature from date and
C                                          time parameters.
C               Jeff Guerber 1999-03-26 -- Removed date param format check
C                                          and reprompt loop, which didn't
C                                          work.
C
C               MFC 2008-12-09  -- added retrieve parameter (ver. 1.2)
C               MFC 2009-10-13  -- added clobber parameter (ver. 1.3)
C---------------------------------------------------------------------
*-Version 1.3

        character(50) contxt
        integer errstat

C	Set Status flag to 'no problem!'
        status = 0
c       3Feb00 (MJT) need to initialize errstat
        errstat = 0

C	Get mission parameters
        call uclgst('mission', tele, errstat)
C	If there's an error getting mission, return
        if(errstat.ne.0)then
             contxt = 'cannot get mission parameter'
             call fcerr(contxt)
             status = 1
             return
        endif

C	Get instrument parameter
        call uclgst('instrument', instr, errstat)
C	If there's an error getting instrument, return
        if(errstat.ne.0)then
             contxt = 'cannot get instrument parameter'
             call fcerr(contxt)
             status = 1
             return
        endif

C	Get detector parameter
	call uclgst('detector',detnam,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get detector parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Get filter parameter
	call uclgst('filter',filter,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get filter parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Get codename parameter
        call uclgst('codename', codenam, errstat)
C	If there's an error getting codename, return
        if(errstat.ne.0)then
             contxt = 'Cannot get codename parameter'
             call fcerr(contxt)
             status = 1
             return
        endif

C	Get date parameter
        call uclgst('date', date, errstat)
C	If there's an error getting date, return
        if(errstat.ne.0)then
             contxt = 'Cannot get date parameter'
             call fcerr(Contxt)
             status = 1
             return
        endif

C	Get time parameter
        call uclgst('time', time, errstat)
C	If there's an error getting time, return
        if(errstat.ne.0)then
             contxt = 'Cannot get time parameter'
             call fcerr(contxt)
             status = 1
             return
        endif

C	Get the expr parameter
	call uclgst('expr',expr,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get the expr parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Get the maxret parameter
	call uclgsi('maxret',maxret,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get the maxret parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Get the retrieve parameter
	call uclgsb('retrieve',retrieve,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get the retrieve parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif
     
c       Get the clobber parameter
        call uclgsb('clobber', delexist, errstat)
        if (errstat.ne.0) then
             contxt = 'Cannot get the clobber parameter'
             call fcerr(contxt)
             status = 1
             return
        endif

C	Get the chatter parameter
	call uclgsi('chatter',chatter,errstat)
	if (errstat.ne.0) then
	     contxt = 'Cannot get the chatter parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

        return
        end

C-----------------------------------------------------------------------
*+QCIF
	subroutine qcif(tele,instr,detnam,filt,codenam,date,time,
     &  expr,chatter,maxret,retrieve,delexist,nfound,errstat)

	implicit none
	character*(*)tele,instr,detnam,filt,codenam,date,time,expr
	integer maxret,nfound,errstat,chatter
        logical retrieve,delexist


C-----------------------------------------------------------------------
C Description: locates the requested files using the Caldb access
C              software, then displays them to STDOUT.
C
C Arguments:   tele    (i): the name of the mission
C              instr   (i): the name of the instrument
C              detnam  (i): the name of the detector
C              filt    (i): the name of the filter
C              codenam (i): the OGIP name of the dataset
C              date    (i): the date for which dataset should be valid
C              time    (i): the time for which dataset should be valid
C              expr    (i): boolean expression for selecting CBD values
C              chatter (i): level of chattiness when working
C              maxret  (i): the maximum number of files to return
C              retrieve (L): retrieve files via remote access if true
C              delexist (L): clobbers file if set to true.
C              nfound  (r): the actual number of datasets found which
C                           meet the selection criteria
C              errstat (r): the status code (0 = OK)
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jun 28, 1994 -- Original Version
C              MFC 12/16/08  added ability to download remote files (version 1.1)
C-----------------------------------------------------------------------

        character(160) filenam(50),contxt,fname,contxt1
	character(20) online(50)
	character(4) cextno
	integer     extno(50),i,fillen,fcstln,nret
        
        character(256) rootName, openedFile
        character(6) subname
        parameter (subname = 'QCIF')
        character(7) version
        parameter (version = '1.1')
        integer fnamelen,rootlen, endpos,lun,blocksz

C Initialize
	nret=0
	lun=10

C	Interrogate the CIF for requested calibration
	call gtcalf(chatter,tele,instr,detnam,filt,codenam,date,time,
     &  date,time,
     &  expr,maxret,filenam,extno,online,nret,nfound,errstat)
	if ((errstat .ne. 0).and.(errstat.ne.110)) return

	if ((nfound.gt.maxret).and.(chatter.gt.10)) then
	     contxt='Warning: More files were found than are shown here'
	     call fcecho(contxt)
	     contxt='Use the maxret parameter to increase '//
     &       'the number of files shown'
	     call fcecho(contxt)
	endif

	Do 500 i=1,nret
C	  Get values needed for display
	  fillen = fcstln(filenam(i))
          fname=filenam(i)
	  write(cextno,'(I4)')extno(i)

C	  Set up display format for file and extno.
C	  If an element is not 'ONLINE' prepend 'OFFLINE' and
C         offline value to display, otherwise use standard display
	  if(online(i).ne.'ONLINE') then

	    contxt='OFFLINE: '//online(i)(:fcstln(online(i)))//' '
     &      //filenam(i)(:fillen)//cextno

	  else
          	 contxt=filenam(i)(:fillen)//cextno
	  endif
	  fnameLen = fcstln( filenam(i) )
C         set rootName to the entire filenam string (including http:// or ftp:// path if present)
          rootLen= fnameLen
          rootName=filenam(i)

C   
C  download remote files if needed
C  check for urls of form ftp:// or http://
C
C        retrieve=.TRUE.
        if (retrieve) then
            if ( ( filenam(i)(1:6) .eq. 'ftp://') .or. 
     &       (filenam(i)(1:7) .eq. 'http://')) then
C            Locate the final '/' in path
	      do 305 endPos=fnameLen,1,-1
		 if( filenam(i)(endPos:endPos).eq.'/' ) goto 405
 305	         continue
 405	      continue
C             set rootName equal to the stripped off filename (stuff after the final "/"
	      rootName = filenam(i)(endPos+1:fnameLen)
	      rootLen  = fnameLen - endPos
         
              if (delexist) then
                  contxt1='Deleting '//rootName
                  call wtinfo(chatter,10,2,contxt1)
                  CALL CLOBBER(rootName,errstat)
                  IF(errstat.NE.0) THEN
                  contxt1='Cannot clobber file '//rootName
                  call fcecho(contxt1)
                  endif
              endif

              contxt = ' Downloading '//rootName
	      call wtinfo(chatter,10,2,contxt)

C       Build the CFITSIO extended filename for opening/copying it
		 openedFile = filenam(i)(1:fnameLen)//
     &                       '('//rootName(1:rootLen)//')'
C		 openedFile = filenam(i)(1:fnameLen)

		 call ftopen(lun,openedFile,1,blocksz,errstat)
		 call ftclos(lun,errstat)
		if( errstat.ne.0 ) then
		       call wterrm(subname, version, 'Unable to ' //
     &                             'download calibration file')
		       contxt = ' Problem file is '//rootName(1:rootLen)
		       call wtinfo(chatter,0,1,contxt)
                       contxt='Does File already Exist in'
     &                          //' Working Directory?'
                       call wtinfo(chatter,0,1,contxt)
	               endif
	        endif
            filenam(i) = rootName(1:rootLen)
        endif
C	  Write info to STDOUT
        contxt=rootName(:rootLen)//cextno
	call fcecho(contxt)
	
500	continue

	return
	end

C---------------------------------------------------------------------

*+PPQZCF
        subroutine ppqzcf(nfound)

	implicit none
	integer nfound

C---------------------------------------------------------------------
C Description:  Returns the parameters for QUZCIF to the parameter
C               file.
C
C Arguments:    nfound (i): the number of entries matching inputs
C
C Origin:       Written for the Calibrtion Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Feb 3), original version
C               Ron Zellar Jun 28, 1994 -- path and extno are no
C                                          longer returned parameters
C                                          Param nfound added
C---------------------------------------------------------------------
*-Version 1.1

	character(80) contxt
	integer errstat

C	Return the nfound param to par file
c       11Jan00 (MJT) add errstat initialization
        errstat=0
	call uclpsi('nfound',nfound,errstat)
	if(errstat.ne.0) then
	     contxt = 'Problem returning nfound param'
	     call fcerr(contxt)
	endif

	return
	end

C******************************************************************************
C SUBROUTINE:
C     clobber
C
C DESCRIPTION:
C     This routine clears the way to use the file named filenam.
C     It deletes the file named filenam (under VMS, it deletes
C     the highest numbered version of filenam.) Thus, it leaves
C     filenam available for an OPEN(...,STATUS='NEW') statement.
C
C AUTHOR/DATE:
C     Lawrence Brown 7/12/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C     To add clobber (overwrite) capability to an ftool, put lines like
C     the following in the parameter fetching routine:
C
C      LOGICAL DELEXIST
C      character(160) OUTFILE
C      INTEGER STATUS
C      CALL UCLGSB('CLOBBER', DELEXIST, STATUS)
C      IF (STATUS .NE. 0) THEN
CC     Probably means there wasn't a clobber field in the .par file
C         STATUS=0
C      ELSE IF(DELEXIST) THEN
C         CALL CLOBBER(OUTFILE,STATUS)
C         IF(STATUS.NE.0) THEN
CC     Do something appropriate. outfile is probably read only.
C      ENDIF
C
C    Then add:
C
C    clobber,b,h,no,,,"Overwrite existing output file? (CAUTION)"
C
C    to the par file.
C
C USAGE:
C     call clobber(filenam,status)
C
C ARGUMENTS:
C     filenam - the file to be "clobbered"
C     status  - returned error status
C
C PRIMARY LOCAL VARIABLES:
C     exists - logical for inquire statements
C     lun - logical unit number for clobbering
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine clobber(filenam,status)
      character*(*) filenam
      integer status
C
      logical exists,opened
      integer lun

      if(status.ne.0) return

      inquire(file=filenam,exist=exists)
      if(exists) then
C     get rid of it
C     first look for a free logical unit number to use to commit the act with
         do 10 lun=99,10,-1
            inquire(lun,opened=opened)
            if(.not.opened) goto 20
 10      continue
C     failed to find free lun
         status=1
         goto 999
 20      continue
         open(lun,file=filenam,status='old',err=30)
         close(lun,status='delete',err=40)
C     we're done
         goto 999
 30      continue
C     error opening file
         status=2
         goto 999
 40      continue
C     error closing and deleting file (This could really mess up the main
C     code, so check for it
         status=3
      endif
 999  continue
      return
      end
