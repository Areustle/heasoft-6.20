C ********************************************************************
C SUBROUTINE:
C     rd_quatrns2
C
C DESCRIPTION:
C     obtain the average value of the quaternions from the XTEFILT file
C     between the input start and stop times
C
C AUTHOR:
C     James Lochner 5/95
C
C MODIFICATION HISTORY:
C  Oct 30, 1995 - converted jitter criterion from std dev in the quaternions
C                to degrees
C  Feb 8, 1996  - check ACSESTQ(1-4) values for null values, allowing the
C                XTE FILTER file to read.  Also included chatter messages.
C  Feb 13, 1996 - Change jitter criterion to arc seconds.
C  Mar 23, 1996 - When comparing input time to TSTART & TSTOP in quats,
C                eliminated setting ierr=1 when WARNING condition found.
C                 Left ERROR condition at ierr=2, and ability to re-instate
C                ierr=1 as WARNING (which resets to ierr=0 upon exit)
C  May 16, 1996 - fixed minor bug by reading TIMEZERO from header and
C                  adding it to quaternion times.
C  Oct 17, 1996 - read acsestq(1-4) as doubles instead of reals
C  Sep 25, 1997 - Bug fix: expand format for time allow output of
C                times greater than 1.0E08;
c  Jul 08, 1998 - (MJT) fixed bug where tzero wasn't being used in 
c                  determining overlap of tstarts and tstops
C  Oct 19, 1998- Bug fix so that check for null value uses nlvald
C                  (instead of nlvale) in accumulating avg and std dev.
C  Aug 25, 1999 - fixed bug in accumulation of quaternions to include
C                  value after tstop_in (so interpolation works later).
C NOTES:
C 
C USEAGE:
C     call rd_quatrns2(tstart_in, tstop_in, mjdref_in, xtefilt,
C                        jitter, qtime, quats, nquats, delquat, ierr)
C
C ARGUMENTS:
C     tstart_in    - desired start time
c     tstop_in     - desired stop time
C     mjdref_in    - mjd reference value for start and stop times
C     xtefilt      - name of xtefilter file for the observation
C     jitter       - acceptable pointing jitter
C     qtime        - time value(s) for quaternion array
C     quats        - output array for quaternions
C     nquats       - size of quaternion output array.
C                     if nquats = 1, the avg quaternion is returned
C     delquat      - the minimum time sampling in the quaternion file
C     ierr         - error flag
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C     ck_quats     - compute the quaternion jitter in degress
C     
C ******************************************************************

      SUBROUTINE rd_quatrns2(tstart_in, tstop_in, mjdref_in, xtefilt,
     $     jitter, qtime, quats, nquats, delquat, chat, ierr)

      character*(*) xtefilt
      double precision tstart_in, tstop_in, mjdref_in, qtime(nquats)
      double precision delquat
      real quats(250000,4), jitter
      integer nquats, ierr, chat
      
      integer qsize
      parameter(qsize=10000)

      logical anynull, abort
      character(8) acsqname(4)
      character(80) message, context, errstr, comment
      character(160) ephfiles(5)
      integer i, j, ephnofiles
      integer iunit, frow, felem, block, ftstatus, htype, nstep
      integer acscol(4), timecol, ntimes, npts, nelem
      double precision acsestq1(qsize), acsestq2(qsize)
      double precision acsestq3(qsize), acsestq4(qsize)
c      real acsestq1(qsize), acsestq2(qsize)
c      real acsestq3(qsize), acsestq4(qsize)
      real avgquat(4), quat_jitter
      double precision sumquat(4), sqrquat(4), sdquat(4)
      double precision tstart, tstop, mjdref, time(qsize)
      double precision nlvald, tzero

      ierr = 0
      ftstatus = 0
      frow = 1
      felem = 1
      nlvald = -99.0

C     Open the xte filter file
      call ftgiou(iunit,ftstatus)
      call fcgcls(xtefilt,ephfiles,ephnofiles,abort)
      if (ephnofiles .gt. 1) then
         call fcecho('More than one Filter/Attitude file !')
         call fcecho('Using only the first one !')
      endif            
      call ftopen(iunit,ephfiles(1),0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open xte filter file'
         call fcerr(context)
         goto 999
      endif
c      inopen = .true.   
      call ftmrhd(iunit,1,htype,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'error moving to extension '
         call fcerr(context)
         goto 999
      endif

C     get the TSTART, TSTOP and MJDREF times in the filter file
      call startnstop(iunit,xtefilt,tstart,tstop,mjdref,ierr)
      if (chat .ge. 15) then
         write(message,'(a,f16.6,a,f16.6)')
     $        'Start/Stop times in light curve:   ',
     $        tstart_in,' - ',tstop_in
         call fcecho(message)
         write(message,'(a,f16.6,a,f16.6)')
     $        'Start/Stop times in pointing file: ',
     $        tstart,' - ',tstop
         call fcecho(message)
      endif

C     get the TIMEZERO value in the filter file
      call xftgkyd(iunit,'TIMEZERO',tzero,comment,ftstatus)
      if (ftstatus .ne. 0) then
         errstr = 'error getting TIMEZERO value from '//
     $        'quaternion file'
         call fcerr(errstr)
         goto 999
      endif

C     determine the number of data points via NAXIS2
        call ftgkyj(iunit,'NAXIS2',npts,comment,ftstatus)
        if (ftstatus .ne. 0) then
           errstr = 'error getting number of points from '//
     $          'quaternion file'
           call fcerr(errstr)
           goto 999
        endif
           
C     compare filter start/stop times with input start/stop times
C     if they don't overlap, exit with fatal error
c     for now, assume tstart_in and tstart are in the same units.
c      if not, then you'd have to get them in the same units by
c     comparing the timeunit keywords from both pha and filt file
c
c     08July1998 (MJT) added tzero to all tstart/tstop in the
c     following block of code to ensure that overlap is correct

        if (tstop_in .lt. tstart+tzero .or. 
     $       tstart_in .gt. tstop+tzero) then
           ierr = 2
           message = 'ERROR: Times in XTE Filter file do not overlap'//
     $          ' integration times in light curve file.'
           go to 999
        elseif (tstart_in .lt. tstart+tzero .and. 
     $          tstop_in .gt. tstop+tzero) then
           message = 'WARNING: Times in quaternion file cover only a'//
     $          ' portion'
           call fcecho(message)
           message = ' of times in light curve file.  Using only '//
     $          ' overlap.'
           call fcecho(message)
           tstart_in = tstart+tzero
           tstop_in = tstop+tzero
        elseif (tstart_in .lt. tstart+tzero) then
           message = 'WARNING: Start time in light curve file '//
     $          ' preceeds start time in XTE Filter file.'
           call fcecho(message)
           message = '         Using only overlap.'
           call fcecho(message)
           tstart_in = tstart+tzero
        elseif (tstop_in .gt. tstop+tzero) then
           message = 'WARNING: Stop time in light curve file exceeds'//
     $          ' stop time in XTE Filter file.'
           call fcecho(message)
           message = '         Using only overlap.'
           call fcecho(message)
           tstop_in = tstop+tzero
        endif
      
                
C     Determine the column numbers for the TIME and ASCESTQ values
        acsqname(1) = 'ACSESTQ1'
        acsqname(2) = 'ACSESTQ2'
        acsqname(3) = 'ACSESTQ3'
        acsqname(4) = 'ACSESTQ4'
        call ftgcno(iunit,.false.,'TIME',timecol,ftstatus)
        if (ftstatus .ne. 0) then
           errstr = 'error getting time column from quaternion file'
           call fcerr(errstr)
           goto 999
        endif
        do i = 1,4
           call ftgcno(iunit,.false.,acsqname(i),acscol(i),ftstatus)
           if (ftstatus .ne. 0) then
              errstr = 'error getting quaternion columns from file'
              call fcerr(errstr)
              goto 999
           endif
        end do
        
C     Read the quaternion file in blocks, and accumulate relevant
C     quaternions and averages


        do j = 1,4
           sumquat(j) = 0
           sqrquat(j) = 0
           avgquat(j) = 0
           sdquat(j) = 0
        end do        
        nstep = min(qsize,npts)
        i = 0
        delquat = 1000.
        
        do frow = 1,npts,nstep
           nelem = min(nstep,npts-frow+1)

C     read TIME column, and ACSESTQ(1,2,3,4) columns in filter file
c     (i.e. Use Estimated Quaternions)
           call ftgcvd(iunit,timecol,frow,felem,nelem,nlvald,time,
     +          anynull,ftstatus)
           if (ftstatus .ne. 0) then
              errstr = 'error reading the time column from '//
     $             'quaternion file'
              call fcerr(errstr)
              goto 999
           endif
           call ftgcvd(iunit,acscol(1),frow,felem,nelem,nlvald,
     $          acsestq1,anynull,ftstatus)
           if (ftstatus .ne. 0) then
              errstr = 'error reading the quaternions from '//
     $             'quaternion file'
              call fcerr(errstr)
              goto 999
           endif
           call ftgcvd(iunit,acscol(2),frow,felem,nelem,nlvald,
     $          acsestq2,anynull,ftstatus)
           call ftgcvd(iunit,acscol(3),frow,felem,nelem,nlvald,
     $          acsestq3,anynull,ftstatus)
           call ftgcvd(iunit,acscol(4),frow,felem,nelem,nlvald,
     $          acsestq4,anynull,ftstatus)
           delquat = time(2) - time(1)

C     Compute average and std dev of quaternions within tstart/tstop
           j = 1
c  JCL - 8/25/99 - changed ".le. tstop_in)" to ".le. tstop_in + delquat)
            do while (j .le. nelem .and.
     $                 time(j) + tzero .le. tstop_in + delquat)
              if (time(j) + tzero .ge. tstart_in - delquat .and.
     $                              acsestq1(j) .ne. nlvald) then
                 i = i + 1
                 qtime(i) = time(j) + tzero
                 delquat = min(qtime(i)-qtime(i-1),delquat)
                 quats(i,1) = acsestq1(j)
                 sumquat(1) = sumquat(1) + acsestq1(j)
                 sqrquat(1) = sqrquat(1) + acsestq1(j) * acsestq1(j)
                 quats(i,2) = acsestq2(j)
                 sumquat(2) = sumquat(2) + acsestq2(j)
                 sqrquat(2) = sqrquat(2) + acsestq2(j) * acsestq2(j)
                 quats(i,3) = acsestq3(j)
                 sumquat(3) = sumquat(3) + acsestq3(j)
                 sqrquat(3) = sqrquat(3) + acsestq3(j) * acsestq3(j)
                 quats(i,4) = acsestq4(j)
                 sumquat(4) = sumquat(4) + acsestq4(j)
                 sqrquat(4) = sqrquat(4) + acsestq4(j) * acsestq4(j)
              endif           
              j = j + 1
           end do
        end do
        
        ntimes = i
        if (chat .ge. 15) then
        write(message,'(a,i6)') 'number of accepted quaternions = ',
     $          ntimes
           call fcecho(message)
           write(message,'(a,f8.4,a)')
     $          'time sampling of quaternions = ',delquat,' s' 
           call fcecho(message)
        endif
        
C compute the avg quaternions and std devs
        do j = 1,4
           avgquat(j) = sumquat(j) / ntimes
           if (ntimes .gt. 1) then
              sdquat(j) = (sqrquat(j) - sumquat(j)*sumquat(j)/ntimes)
     $             /(ntimes - 1)
              if (sdquat(j) .gt. 0) then
                 sdquat(j) = dsqrt(sdquat(j))
              else
                 sdquat(j) = 0.
              endif
           end if
c           write(6,*) j,sumquat(j),sqrquat(j),avgquat(j),sdquat(j)
        end do
        
C Compute the jitter in degrees
        call ck_quats(avgquat,sdquat,quat_jitter)

C Convert the jitter to arc seconds
        quat_jitter = 3600.0 * quat_jitter

C Compare quaternion jitter with the input jitter criterion        
C     If jitter exceeded, then return entire array,
C     If jitter not exceeded, return just avg quat.
        if (quat_jitter .gt. jitter) then
           nquats = ntimes
           if (chat .ge. 5) then
           write(message,'(a,f7.1,a,f5.1,a)')
     $     'Pointing jitter of ',quat_jitter,
     $     ' arc sec is larger than nominal value of ',jitter,
     $     ' arc sec'
              call fcecho(message)
              message = ' ... will compute response for each time stamp'
              call fcecho(message)
           endif
        else
           nquats = 1
           quats(1,1) = avgquat(1)
           quats(1,2) = avgquat(2)
           quats(1,3) = avgquat(3)
           quats(1,4) = avgquat(4)
           if (chat .ge. 5) then
              message = 'Pointing jitter within jitter criterion'
              call fcecho(message)
              message = ' ... will use average pointing'
              call fcecho(message)
           endif           
        endif
        
C
999     if (ierr .ne. 0) call fcecho(message)
        if (ftstatus .ne. 0) then
           call fcerrm(ftstatus)
           ierr = 2
        endif        
        if (ierr .eq. 1) ierr = 0

C     Close the filter/quaternion file

        call ftclos(iunit,ftstatus)
        call ftfiou(iunit,ftstatus)
        return
        end
