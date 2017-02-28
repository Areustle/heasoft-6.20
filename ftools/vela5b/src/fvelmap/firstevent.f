C Finds the time of the very first event recorded matching the selection 
C criteria in one channel.
C
C Author: Jesse S. Allen
C History:
C  Vers. 1.0  15 Mar 1994  First draft
C        1.1   9 Nov 1995  Reject background points for which no background
C                           was available from the NOS side.
C        1.2   5 Feb 1996  Modified rejection criteria to reject data with
C                           bad absolute values

      subroutine firstevent(inunit, channel, boxlist, rowlist,  
     +           numofboxes, firsttime, status)

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1, 
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,  
     +       stimbin, backopt, iwp, jwp, spincheck, 
     +       pointcheck, weight
      
      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

      common /TASK/ taskname
      character(40) taskname

C Local variable declarations

      logical anyf

      integer channel, boxlist(400), rowlist(400), numofboxes
      integer i, inunit, rwmode, nhdu, hdutype, blocksize, status
      integer row, ipos, stabflag, pntflag

      real spinper, counts, bkg(3), bkgvar(3)
      real rawcnts, backgnd, r, eradd

      double precision firsttime, obstime

      character(16) obs_id, telescop
      character(20) boxname
      character(70) comment
      character(80) message


      rwmode = 0
      nhdu = 1
      firsttime = 50000.0D0

      do 300 i = 1, numofboxes
         write(boxname,'(''b'',I5.5,''.raw'')') boxlist(i)
         call ftopen(inunit, boxname, rwmode, blocksize, status)
         if (status .ne. 0) then
            write(message, '('' Error opening '', A20)') boxname
            call fcecho(message)
            call ftgmsg(message)
            call fcecho(message)
            write(message, '('' FITSIO status = '', i3)') status
            call fcecho(message)
            call ftclos(inunit, status)
            go to 999
         endif

C Check that the data file is a Vela 5B raw data file

         call ftgkys(inunit, 'OBS_ID', obs_id, comment, status)
         call ftgkys(inunit, 'TELESCOP', telescop, comment, status)
         if ((obs_id .ne. boxname(2:6)) .or. 
     +       (telescop .ne. 'Vela 5B ')) then
            message = ' Attempted to read in an inappropriate file'
            call fcecho(message)
            call ftclos(inunit, status)
            if (status .eq. 0) status = -100
            go to 999
         endif

C Move to the binary table extension and  retrieve the first data time
C which matchs all the user-specified criteria

         nhdu = 1
         call ftmrhd(inunit, nhdu, hdutype, status)
         if (hdutype .ne. 2) then
            message = ' File does not have a binary table...'
            call fcecho(message)
            call ftclos(inunit, status)
            if (status .eq. 0) status = -200
            go to 999
         endif
         call ftgkyj(inunit, 'NAXIS2', rowlist(i), comment, status)
         do 100 row = 1, rowlist(i)
            call ftgcvd(inunit, 4,row,1,1,0.D0, obstime, anyf, status)
            call ftgcvj(inunit,10,row,1,1,0, ipos, anyf, status)
            call ftgcvj(inunit,19,row,1,1,0, stabflag, anyf, status)
            call ftgcvj(inunit,20,row,1,1,0, pntflag, anyf, status)
            call ftgcve(inunit,21,row,1,1,0, spinper, anyf, status)
            if (channel .eq. 1) then
               call ftgcve(inunit, 7,row,1,1,0, counts, anyf, status)
               call ftgcve(inunit, 8,row,1,3,0, bkg, anyf, status)
               call ftgcve(inunit, 9,row,1,3,0, bkgvar, anyf, status)
            else if (channel .eq. 2) then
               call ftgcve(inunit,12,row,1,1,0, counts, anyf, status)
               call ftgcve(inunit,13,row,1,3,0, bkg, anyf, status)
               call ftgcve(inunit,14,row,1,3,0, bkgvar, anyf, status)
            endif
            if (status .ne. 0) then
               write(message,'('' Error reading '', A20, 
     +              '', exiting '')') boxname
               call fcecho(message)
               call ftgmsg(message)
               call fcecho(message)
               write(message, '('' FITSIO status = '', i3)') status
               call ftclos(inunit, status)
               call fcerr(message)
               go to 999
            endif

C Skip data flagged for stability or pointing error problems (if requested)
C and data which does not fall within the requested time range.

            if ((spincheck) .and. (stabflag .ne. 0)) go to 100
            if ((pointcheck) .and. (pntflag .ne. 0)) go to 100
            if ((obstime .lt. begintime) .or. (obstime .gt. endtime))
     +         go to 100

C Process the counter, background, and variance data
C Reject points with zero counts and points with invalid background
C values (absolute value is 32.75 for sin and cos terms, 
C 327.5 for linear, or less than 0 for linear)

            if ((counts .eq. 0) .or. 
     +          (ABS(bkg(1)) .ge. 32.74) .or. 
     +          (ABS(bkg(2)) .ge. 32.74) .or. 
     +          (bkg(3) .ge. 327.5) .or.
     +          (bkg(3) .lt. 0.0))
     +          go to 100

            r = 1.0
            eradd = 0.0
            rawcnts = counts
            call background(counts, bkg, bkgvar, eradd, backopt,
     +           ipos, spinper)
            counts = counts / r
            backgnd = (rawcnts + eradd) / (r**2)
            if ((counts .lt. minflux) .or. (counts .gt. maxflux) 
     +          .or. (backgnd .gt. maxerr)) go to 100

C Check if the current observation time is less than the earliest observation
C time.  If so, reset the earliest observation time and move on to the 
C next box.  if not, move on to the next box.

            if (obstime .lt. firsttime) 
     +         firsttime = obstime
            go to 200

 100     continue
 200     call ftclos(inunit, status)
         if (status .ne. 0) then
            write(message,'('' Error closing '', A20)') boxname
            call fcecho(message)
            call ftgmsg(message)
            call fcecho(message)
            write(message,'('' FITSIO status = '', I3)') status
            call fcecho(message)
            go to 999
         endif
 300  continue

 999  return

      end

