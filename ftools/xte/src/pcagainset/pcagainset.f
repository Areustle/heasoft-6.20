C ***************************************************************************
C SELECTOR TASK
C      pcagainset
C
C FILE:
C      pcagainset.f
C
C DESCRIPTION:      
C     Write the pca/eds gain and offset values into an input PCA .pha file
C      
C AUTHOR:
C      James Lochner  5/95
C      
C MODIFICATION HISTORY:
C    July 26, 1996 - Modified to work with single EDS gain/offset file which
C          contains complete history of the values used throughout the
C          mission.
C     Sept 30, 1997 - if ck_xtepha returns obs_time='UNKNOWN', set
C         obs_time='00:00:00'.  This makes phase binner tools work.     
C    July 6, 1998 - by ZG to modify the string length of obs_date, obs_time,
C		and refdate from 16 to 68 for new date/time string format in
C		cfitsio functions.
C    Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      phafile	 - name of input PCA .pha file
C      gainfile  - name of file containing the gain & offset values
C      update    - update existing values ?
C      chatter   - amount to tell user
C      
C CALLED ROUTINES:
C     subroutine gpardescr  - gets parameters from environment
c     subroutine gtdescr    - gets & interprets info from pha file
C ************************************************************************** 

      Subroutine PCAGAT

c start with initial declarations
      character(160) phafile, gainfile
      integer chatter
      logical update
      
      logical abort
        
      character(40) taskname
      common /task/ taskname

      taskname = 'pcagainset v1.12'
      abort = .false.
        
c get the parameters from the par file
      call gpargain(phafile, gainfile, update, chatter)

c Perform the Algorithm:   
      call ckgain(phafile, gainfile, update, chatter)

c  Exit subroutine

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gpargain
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  5/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      gpargain uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gpargain(phafil, chanfil, update, chatter)
C      
C ARGUMENTS:
C     phafile	- name of input PCA pha file
C     gainfile  - name of file containing the gain & offset values
C     update    - update existing values ?
C     chatter   - amount to tell user
C      
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE gpargain(phafile,gainfile,update,chatter)


c start with the declarations
      character*(*) phafile, gainfile
      integer chatter
      logical update
      
      character(80) context
      integer  status
      
      status = 0

      
c get the name of the input pha file
	call uclgst('phafile',phafile,status)
	if (status .ne. 0) then
	    context = 'could not get PHAFILE parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the gain-offset  file
	call uclgst('gainfile',gainfile,status)
	if (status .ne. 0) then
	    context = 'could not get GAINFILE parameter'
	    call fcerr(context)
	    go to 999
	endif
        
C     get the update flag
        call uclgsb('update',update,status)
	if (status .ne. 0) then
	    context = 'could not get UPDATE parameter'
	    call fcerr(context)
	    go to 999
	endif
        
c get the name of the gain-offset  file
	call uclgsi('chatter',chatter,status)
	if (status .ne. 0) then
	    context = 'could not get CHATTER parameter'
	    call fcerr(context)
	    go to 999
	endif
        
c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end
C*****************************************************************
C SUBROUTINE:
C      ckgain
C
C DESCRIPTION:      
C     Looks for GAINAPP keyword in a XTE PCA .pha file.  If True, write the
C      gain and offset values appropriate for the observation into the
C      .pha header.
C      
C AUTHOR:
C      James Lochner  9/95
C
C MODIFICATION HISTORY:
C     July 26, 1996 - Modified to work with single EDS gain/offset file
C         containing all the values used in the mission.  If file specified
C         with extension or as 'caldb', valid date in extension checked
C         with observation date.  A warning is issued if found inconsistent.
C         If only the file name specified, the file is searched for the
C         appropriate extension.
C         N.B.  Logic assumes EDS gain/offset file is in increasing
C         chronological order, i.e. is starts at Dec 30, 1995
C     Sept 30, 1997 - if ck_xtepha returns obs_time='UNKNOWN', set
C         obs_time='00:00:00'.  This makes phase binner tools work.      
C     Aug 18, 1999 - Replace gtcal with gtcalf (PDW)
C
C NOTES:
C
C USEAGE:      
C      call ckgain(phafile, gainfile, update, chat)
C      
C ARGUMENTS:
C     phafile	 - name of input PCA pha file
C     gainfile   - name of file containing the gain & offset values
C     update     - update existing values ?
C     chat       - amount to tell user
C     
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE ckgain(phafile,gainfile,update,chat)


c start with the declarations
      character*(*) phafile, gainfile
      integer chat
      logical update

      character(8) telescop,instrume,detnam,filter
C      character(16) obs_date, obs_time, refdate
	character(68) obs_date, obs_time, refdate
      character(80) msg, comment
      character(160) gaintbl(1)
      character(6) online(1)

      double precision mjd_obs, mjd_eds, mjd_eds_start, mjd_eds_stop
      integer ftstatus, ierr
      integer iunit, iunit2, pcu0gain
      integer n, maxret, extno(1), nfound, nret, ireadwrite
      integer block, hdutype
      logical gainapp, found, quiet

      gainapp = .false.
      ftstatus = 0
      
C Open the XTE PHA file
      ireadwrite = 1
      ierr=0
      call ck_xtepha(chat, phafile, iunit, ireadwrite, 
     &   telescop, instrume,
     &     detnam, filter, obs_date, obs_time, ierr)
      if (obs_time .eq. 'UNKNOWN') obs_time = '00:00:00'
      
C Look for GAINAPP keyword. If keyword not found (ftstatus = 202), assume F
      call ftgkyl(iunit,'GAINAPP',gainapp,comment,ftstatus)
      if (ftstatus .eq. 202) ftstatus = 0
      if (ftstatus .ne. 0) then
         msg = 'unable to obtain GAINAPP keyword value'
         call fcerr(msg)
         go to 999
      endif

C If GAINAPP = 'T',
C     First reckon the name of the eds gain file using caldb if necessary
C     (Force detnam = ALL since we're writing all the gain values,
C       irregardless of which detectors.  Besides, the detnam values are
C       mostly meaningless as they stand now.)
      
      if (gainapp) then

         if (gainfile(1:5) .eq. 'CALDB' .or.
     $        gainfile(1:5) .eq. 'caldb') then
            filter = '-'
            quiet = .false.
            maxret = 1
            detnam = 'ALL'
            call gtcalf(chat, telescop, instrume, detnam, filter,
     $           'EDS_GCOR', obs_date, obs_time, obs_date, obs_time,
     $           '-', maxret, gaintbl, extno, online,
     $            nret, nfound, ierr)
            if (ierr .ne. 0) then
               msg = 'unable to get EDS gain file from caldb'
               go to 999
            endif

C if caldb not specified, translate name by taking off any extension
c     specified by the user

         else
            call fcpars(gainfile,gaintbl(1),extno(1),ftstatus)
            if (ftstatus .ne. 0) then
               msg = 'Problem parsing gainfile expression'
               call fcecho(msg)
               msg = '... will search all extensions'
               call fcecho(msg)
               extno(1) = -99
            endif
         endif

C Check that the date for the extension is correct for the date
C  of the observation

C     Convert the pha obsdate into mjd
         call dt2mjd(obs_date,quiet,mjd_obs,ftstatus)
         if (ftstatus .ne. 0) then
            msg = 'cannot determine MJD from pha obs_date'
            call fcerr(msg)
            goto 999
         endif

C     Open the eds gain/offset file
         ftstatus = 0
         call cgetlun(iunit2)
         call ftopen(iunit2,gaintbl(1),0,block,ftstatus)
         if (ftstatus .ne. 0) then
            msg = 'cannot open EDS gain/offset file'
            call fcerr(msg)
            goto 999
         endif
         
         if (extno(1) .ge. 0) then
C     if extension number for eds file already known, check the dates.
c       Read valid start date from current extension
            call ftmahd(iunit2,extno(1)+1,hdutype,ftstatus)
            if (ftstatus .ne. 0) then
               msg = 'cannot move to requested extension in EDS '
     $              //'gain/offset file'
               call fcerr(msg)
               goto 999
            endif
            call ftgkys(iunit2,'CVSD0001',refdate,comment,ftstatus)
            if (ftstatus .ne. 0) then
               msg = 'cannot get CVSD0001 value from EDS gain/offset '//
     $              'file'
               call fcerr(msg)
               goto 999
            endif
            call dt2mjd(refdate,quiet,mjd_eds_start,ftstatus)
            if (ftstatus .ne. 0) then
               msg = 'cannot determine MJD from EDS start date'
               call fcerr(msg)
               goto 999
            endif

C       Infer valid stop date from next extension
            call ftmahd(iunit2,extno(1)+2,hdutype,ftstatus)
            if (ftstatus .eq. 0) then
               call ftgkys(iunit2,'CVSD0001',refdate,comment,ftstatus)
               if (ftstatus .ne. 0) then
                  msg = 'cannot get CVSD0001 value from EDS '//
     $                 'gain/offset file'
                  call fcerr(msg)
                  goto 999
               endif
               call dt2mjd(refdate,quiet,mjd_eds_stop,ftstatus)
               if (ftstatus .ne. 0) then
                  msg = 'cannot determine MJD from EDS end date'
                  call fcerr(msg)
                  goto 999
               endif
               
C      Compare observation date with valid start & stop dates
               if (mjd_obs .lt. mjd_eds_start .or.
     $              mjd_obs .gt. mjd_eds_stop) then
                  msg = 'WARNING - Observation date does not match'//
     $                 ' validity'
                  call fcerr(msg)
                  msg = ' dates for this extension of EDS '//
     $                 'gain/offset file.'
                  call fcerr(msg)
                  msg = ' Check for appropriate file or extension.'
                  call fcerr(msg)
               endif
               
            else if (ftstatus .eq. 107) then
C          -  If no next extension, check just start time
               ftstatus = 0
               msg = 'WARNING - Can only check observation and '
               call fcecho(msg)
               msg = '  EDS files for valid start time'
               call fcecho(msg)
               if (mjd_obs .lt. mjd_eds_start) then
                  msg = 'WARNING - Observation date preceeds validity'
                  call fcerr(msg)
                  msg = 'date for EDS gain/offset values.  Check for'
                  call fcerr(msg)
                  msg = 'appropriate file or extension.'
                  call fcerr(msg)
               endif

            else if (ftstatus .ne. 0) then
               msg = 'cannot move to requested extension in EDS file'
               call fcerr(msg)
               goto 999
            endif
                              
         else
         
C     if no extension specified, search extensions for appropriate date.
c       (assume extensions in data file are in ascending time order)
            n = 1
            found = .false.
c    MJT 16Sept - fixing logical comparison...
            do while (.not. found)
               call ftmahd(iunit2,n+1,hdutype,ftstatus)
C          if you get to end of file, then last extension must be right one
               if (ftstatus .eq. 107) then
                  found = .true.
                  extno(1) = n - 1
                  ftstatus = 0
               endif               
               if (ftstatus .ne. 0) then
                  msg = 'cannot move to extension in EDS gain/offset '//
     $                 'file'
                  call fcerr(msg)
                  goto 999
               endif
               call ftgkys(iunit2,'CVSD0001',refdate,comment,ftstatus)
               if (ftstatus .ne. 0) then
                  msg = 'cannot get CVSD0001 value from EDS '//
     $                 'gain/offset file'
                  call fcerr(msg)
                  goto 999
               endif
               call dt2mjd(refdate,quiet,mjd_eds,ftstatus)
               if (ftstatus .ne. 0) then
                  msg = 'cannot determine MJD from eds date'
                  call fcerr(msg)
                  goto 999
               endif
               if (mjd_obs .le. mjd_eds) then
                  found = .true.
                  extno(1) = n - 1
               endif
               
               n = n + 1
            end do

         endif

         call ftclos(iunit2,ftstatus)
         call ftfiou(iunit2,ftstatus)
         
C     Now check whether values already exist in pha file and check update value.
C     Write the gain & offset values to the .pha file if values don't exist, or if
C     values exist with update = yes
         call ftgkyj(iunit,'PCU0GAIN',pcu0gain,comment,ftstatus)
         if (ftstatus .eq. 0) then
            if (update) call wrtgain(iunit,gaintbl(1),extno(1))
         else if (ftstatus .eq. 202) then
            ftstatus = 0
            call wrtgain(iunit,gaintbl(1),extno(1))
         else
            msg = 'unable to determine whether values already'
     $           //' written'
            call fcerr(msg)
            go to 999
         endif
      endif
      
c Exit subroutine
999   continue
      call ftclos(iunit,ftstatus)
      call ftfiou(iunit,ftstatus)

      if (ierr .ne. 0) call fcecho(msg)
      if (ftstatus .ne. 0) call fcerrm(ftstatus)

      return
      end
C*****************************************************************
C SUBROUTINE:
C      wrtgain
C
C DESCRIPTION:      
C     Writes the gain and offset values for each pcu into an input
C      XTE PCA .pha file.
C      
C AUTHOR:
C      James Lochner  9/95
C
C MODIFICATION HISTORY:
C     July 26, 1996 - added extno to argument list.  Now move to
C                     requested extension.  Also, output extension #
C     Sept 13, 1996 - added updating of pha file CHECKSUM & DATASUM
C                     keyword values
C NOTES:
C
C USEAGE:      
C      call wrtgain(iunit, gainfile)
C      
C ARGUMENTS:
C     iunit	 - unit number for the PCA pha file
C     gainfile   - name of file containing the gain & offset values
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE wrtgain(iunit,gainfile,extno)


c start with the declarations
      character*(*) gainfile
      integer iunit, extno
      
      character(120) comment
      character(80) context
      character(20) n20, ext20
      character(8) gainkey, offkey
      character(2) extchar
      character(1) nchar
      integer n, iunit2, gain(0:4), offset(0:4)
      integer ftstatus, ierr, block, hdutype, frow, felem, nrows
      integer nullval, fcstln
      logical anyf
      
      ierr = 0
      ftstatus = 0
      nullval = 0

C Open the gain-offset file
      call ftgiou(iunit2,ftstatus)
      call ftopen(iunit2,gainfile,0,block,ftstatus)
       if (ftstatus .ne. 0) then
         context = 'cannot open gainfile'
         call fcerr(context)
         go to 999
      endif

C Move to the requested extension
      call ftmahd(iunit2,extno+1,hdutype,ftstatus)
       if (ftstatus .ne. 0) then
         context = 'cannot move to proper extension'
         call fcerr(context)
         go to 999
      endif

C Read the second row of columns 2-11
      frow = 2
      felem = 1
      nrows = 1
      do n = 0,4         
         call ftgcvj(iunit2,n+2,frow,felem,nrows,nullval,
     $        gain(n),anyf,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'cannot get gain value'
            call fcerr(context)
            go to 999
         endif
         
         call ftgcvj(iunit2,n+7,frow,felem,nrows,nullval,
     $        offset(n),anyf,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'cannot get offset value'
            call fcerr(context)
            go to 999
         endif
      end do

C Now write these values to the .pha file
      do n = 0,4
         
C     convert the integer into a character
         call fti2c(n,n20,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not convert integer to character'
            call fcerr(context)
            go to 999
         endif
         nchar = n20(20:20)
         
C     write the PCUnGAIN and PCUnOFST values
         gainkey = 'PCU'//nchar//'GAIN'
         offkey = 'PCU'//nchar//'OFST'
         comment = 'Gain for PCU '//nchar
         call ftukyj(iunit,gainkey,gain(n),comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not write '//gainkey//'keyword'
            call fcerr(context)
            go to 999
         endif
         comment = 'Offset for PCU '//nchar
         call ftukyj(iunit,offkey,offset(n),comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not write '//offkey//'keyword'
            call fcerr(context)
            go to 999
         endif

      end do

C Write name of gain file to .pha header
      call fti2c(extno,ext20,ftstatus)
      if (extno .le. 9) extchar = ext20(20:20)
      if (extno .ge. 10) extchar = ext20(19:20)
      comment = 'Gain/Offset values from file '//
     $     gainfile(1:fcstln(gainfile))//'['//extchar//']'
      call ftpcom(iunit,comment,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write gain file name to .pha file'
         call fcerr(context)
         go to 999
      endif

C update the CHECKSUM and DATASUM keyword values of the pha file
      call ftpcks(iunit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not update PHA file CHECKSUM & DATASUM '//
     $        'keywords'
         call fcerr(context)
         go to 999
      endif
      
      
c     Exit subroutine
999   continue
      call ftclos(iunit2,ftstatus)
      call ftfiou(iunit2,ftstatus)

      if (ierr .ne. 0) call fcerrm(ierr)
      if (ftstatus .ne. 0) call fcerrm(ftstatus)

      return
      end


