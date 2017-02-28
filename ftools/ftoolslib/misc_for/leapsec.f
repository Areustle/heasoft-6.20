
C******************************************************************************
C SUBROUTINE:
C      leapsec
C
C DESCRIPTION:
C      Determine the number of leap seconds between the two input times
C       by reading the leapsec.fits file.  The input time (in seconds)
C       is assumed to include the leap seconds being counted
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       October, 1993
C
C MODIFICATION HISTORY:
C       4/22/94 EAG problem with putting in leap dyas in the wrong directions
C       1998-07-03 Jeff Guerber.  Use fcislpyr().
C       1998-07-14 Jeff Guerber.  Only read leapfile once.
C       1998-12-24 Peter Wilson.  Count only leapseconds after reference
C                                 time; use LEAPSECS column to get the size
C                                 of the adjustment (could be neg); count
C                                 seconds correctly right near a leapsecond
C
C NOTES:
C
C USAGE:
C
C ARGUMENTS:
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
      integer function leapsec (time, refyear, leapfile, status)

      double precision time
      integer refyear, status
      character*(*) leapfile

      integer maxleaps
      parameter (maxleaps = 50)

      integer iunit, block, htype, nrows, lyear, colno
      integer ystart, ystop, i, refleaps
      character(80) context
      logical anyf, fcislpyr, fileread
      data fileread/.false./
      double precision leaptime(maxleaps), leapsecs(maxleaps), delta
      double precision refsecs, timesecs
      save  fileread, leaptime, leapsecs, lyear, nrows

      leapsec = 0
      if ( status.ne.0 ) return

C Read in the leapsecond file... once!

      if ( .not. fileread ) then
          call ftgiou(iunit,status)
          call ftopen (iunit, leapfile, 0, block, status)
          if (status .ne. 0) then
              context = ' Error finding leapsecond file' // leapfile
              call fcerr (context)
              goto 999
          endif
          call ftmahd (iunit, 2, htype, status)
          call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
          call ftgkyj (iunit, 'REFYEAR', lyear, context, status)
          call ftgcno (iunit, .true., 'SECONDS', colno, status)
          call ftgcvd (iunit, colno, 1, 1, nrows, -99.D0, leaptime,
     &         anyf, status)
          call ftgcno (iunit, .true., 'LEAPSECS', colno, status)
          call ftgcvd (iunit, colno, 1, 1, nrows, -99.D0, leapsecs,
     &         anyf, status)
          call ftclos (iunit, status)
          if (status .ne. 0) then
              context = ' Error in reading leapsecond file'
              call fcerr (context)
              call fcerrm (status)
              goto 999
          endif

          fileread = .true.
          call ftfiou(iunit,status)
      endif

C Convert the reference year to reference seconds relative to table,
C including leapseconds up to the reference year

      if (refyear .ne. lyear) then
         refsecs = (refyear - lyear) * 31536000.00
         if (refyear .gt. lyear) then
            ystart = lyear
            ystop = refyear - 1
            delta = +86400.0
         else
            ystart = refyear
            ystop = lyear - 1
            delta = -86400.0
         endif
         do 10 i = ystart, ystop
            if ( fcislpyr(i) ) refsecs = refsecs + delta
 10      continue
         do 20 i = 1, nrows
C                Must include -lsecs here to catch last second on prev day
            if ( refsecs .ge. (leaptime(i)-leapsecs(i)) )
     &           refsecs = refsecs + leapsecs(i)
 20      continue
      else
         refsecs = 0.D0
      endif
      timesecs = time + refsecs

C Count leap seconds between refsecs and timesecs

      refleaps = 0
      do 30 i = 1, nrows
         if ( timesecs.ge.leaptime(i) ) leapsec = leapsec + leapsecs(i)
         if ( refsecs.ge.leaptime(i) ) refleaps = refleaps + leapsecs(i)
 30   continue
      leapsec = leapsec - refleaps

 999  continue

      return
      end
