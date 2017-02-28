

C******************************************************************************
C subroutine:
C      colparse
C
C FILE:
C      colparse.f
C
C DESCRIPTION:
C       This routine parses input array specifications and row ranges.
C       It has the advantage over fcgrgs (the original row range parser) in
C       that a status is returned if the row range cannot be parsed.
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       November, 1993
C
C MODIFICATION HISTORY:
C       2/15/94 EAG 1.1 Fixed some problems
C       9/12/94 EAG 1.2 Allow for enclosing ()
C
C NOTES:
C
C CALLING SEQUENCE:
C       call colparse (subset, ndims, dimmax, ranges, startrange,
C                            stoprange, incrange, status)
C
C ARGUMENTS:
C       subset     - the input string containing the column subset specification
C       ndims      - number of dimensions in this column
C       dimmax     - the maximum value of each dimension of this column
C       ranges     - returned number of ranges in each dimension
C       startrange - the starting element of each range for each dimension
C       stoprange  - the stopping element of each range for each dimension
C       incrange   - the increment for each range for each dimension
C       status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine colparse (subset, ndims, dimmax, ranges, startrange,
     &     stoprange, incrange, status)

      integer maxranges
      parameter (maxranges = 15)

      integer ndims, dimmax(ndims), ranges(ndims), status
      integer startrange(maxranges,ndims), stoprange(maxranges,ndims)
      integer incrange(maxranges,ndims)
      character*(*) subset

      character(160) context
      character(3) version
      integer starpos, dashpos, semipos, colon1pos, colon2pos
      integer commapos
      integer dim, firstchar, lastchar, fcstln, i
      logical finished

      version = '1.2'

C initialize variables
      if ((subset .eq. ' ') .or. (subset .eq. '[]') .or.
     &     (subset .eq. '()')) then
         do 1 i = 1, ndims
            ranges(i) = 1
            startrange(1, i) = 1
            stoprange(1, i) = dimmax(i)
            incrange(1, i) = 1
 1       continue
         goto 999
      endif

      firstchar = 1
      lastchar = fcstln(subset)

      do 2 i = 1, ndims
         ranges(i) = 0
 2    continue

C Ignore enclosing [], if any
      if ((subset(1:1) .eq. '[') .or. (subset(1:1) .eq. '('))
     &     firstchar = 2
      if ((subset(lastchar:lastchar) .eq. ']') .or.
     &     (subset(lastchar:lastchar) .eq. ')')) lastchar = lastchar - 1
      finished = .false.
      dim = 0

C  Find the next semi-colon
 10   semipos = index(subset(firstchar:), ';') + firstchar - 1
      if (semipos .lt. firstchar) then
         semipos = lastchar + 1
         finished = .true.
      endif
      dim = dim + 1
      if (dim .gt. ndims) then
         context = 'COLPARSE'//version//': Too many dimensions '
     &        // 'requested'
         call fcerr (context)
         status = 12
         goto 999
      endif

C find next comma
 20   commapos = index(subset(firstchar:semipos), ',') + firstchar - 1
      if (commapos .le. firstchar) commapos = semipos
      colon1pos = index(subset(firstchar:commapos), ':') + firstchar-1
      if (colon1pos .le. firstchar) then
         colon1pos = commapos
         colon2pos = commapos
      else
         colon2pos=index(subset(colon1pos+1:commapos),':')+colon1pos
         if (colon2pos .le. colon1pos+1) colon2pos = commapos
      endif
      dashpos = index(subset(firstchar:commapos), '-') + firstchar - 1
      if (dashpos .lt. firstchar) dashpos = 0
      starpos = index(subset(firstchar:commapos), '*') + firstchar - 1
      if (starpos .lt. firstchar) starpos = 0

C parse this range
      ranges(dim) = ranges(dim) + 1

C check for too many ranges
      if (ranges(dim) .gt. maxranges) then
         context = 'COLPARSE'//version//': Too many ranges requested'
     &        // subset
         call fcerr (context)
         status = 11
         goto 999
      endif

C initialize default values
      startrange(ranges(dim), dim) = 1
      stoprange(ranges(dim), dim) = dimmax(dim)
      incrange(ranges(dim), dim) = 1

C deal with a *
      if (starpos .gt. 0) then
C default start and stop are correct but need to get incrange
         if (colon1pos .ne. commapos)
     &        read (subset(colon1pos+1:commapos-1), * , err=900)
     &        incrange(ranges(dim), dim)

C deal with a -
      else if (dashpos .gt. 0) then
C check for number before dash
         if (dashpos .ne. firstchar)
     &        read (subset(firstchar:dashpos-1), * , err=900)
     &        startrange(ranges(dim), dim)

C check for number after dash
         if (dashpos .ne. colon1pos-1)
     &        read (subset(dashpos+1:colon1pos-1), * , err=900)
     &        stoprange(ranges(dim), dim)

C and deal the with increment, if needed
         if (colon1pos .ne. commapos)
     &        read (subset(colon1pos+1:commapos-1), * , err=900)
     &        incrange(ranges(dim), dim)

C deal with two colons
      else if (colon2pos .ne. commapos) then
         read (subset(firstchar:colon1pos-1), * , err=900)
     &        startrange(ranges(dim), dim)
         read (subset(colon1pos+1:colon2pos-1), * , err=900)
     &        stoprange(ranges(dim), dim)
         read (subset(colon2pos+1:commapos-1), * , err=900)
     &        incrange(ranges(dim), dim)

C deal with one colon
      else if (colon1pos .ne. commapos) then
         read (subset(firstchar:colon1pos-1), * , err=900)
     &        startrange(ranges(dim), dim)
         read (subset(colon1pos+1:commapos-1), * , err=900)
     &        stoprange(ranges(dim), dim)

C deal with a single number
      else
         read (subset(firstchar:commapos-1), * , err=900)
     &        startrange(ranges(dim), dim)
         stoprange(ranges(dim), dim) = startrange(ranges(dim), dim)
      endif

C check for reasonable input
      if (startrange(ranges(dim), dim) .le. 0)
     &     startrange(ranges(dim), dim) = 1
      if (stoprange(ranges(dim), dim) .gt. dimmax(dim))
     &     stoprange(ranges(dim), dim) = dimmax(dim)

C loop back for next range
      firstchar = commapos + 1
      if (commapos .ne. semipos) goto 20

C loop back for next dimension
      if (.not. finished) goto 10
      if (dim .ne. ndims) then
         context = 'COLPARSE'//version//': Incorrect number of '
     &        // 'dimensions specified' // subset
         call fcerr (context)
         status = 12
      endif

      goto 999

 900  status = 10
      context = 'COLPARSE'//version//': Error parsing requested '
     &     //'subset '//subset
      call fcerr (context)


 999  return
      end
