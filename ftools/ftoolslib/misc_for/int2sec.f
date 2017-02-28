
C******************************************************************************
C SUBROUTINE:
C      int2sec
C
C DESCRIPTION:
C      Convert from an integer date (year, day, month, hour, etc) to seconds
C       from midnight January 1 of a reference year, including leapseconds
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       October, 1993
C
C MODIFICATION HISTORY:
C       4/22/94 EAG fixed bug with leapseconds pushing to another leapsecond
C       Jeff Guerber 1998-06-23.  All 2-digit years refer to 1900, as required
C           by the new FITS standard.  Formerly 00-49 were 2000-2049.
C       Jeff Guerber 1998-06-27.  Fix leapyear calculation.
C       Jeff Guerber 1998-07-03.  Use fcislpyr().
C       Peter Wilson 1998-12-98.  Fix call to leapsecond to take into account
C                                 the possibility of falling right on a
C                                 leap second
C
C NOTES:
C       Years with only 2 digits are assumed to be between 1900 and 1999
C       2000 is a leap year (every 4th century is a leap year)
C
C USAGE:
C       call int2sec (year, month, day, hour, minute, second,
C                           refyear, leapfile, abssecs, status)
C
C ARGUMENTS:
C       year     - the integer year, either 2 or 4 digits
C       month    - the integer month
C       day      - integer day of the month
C       hour     - integer hour
C       minute   - integer minute
C       second   - real seconds
C       refyear  - reference year for time since to be calculated
C       leapfile - the path to the leapsecond file
C       abssecs  - the absolute seconds of the input time from refyear
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine int2sec (year, month, day, hour, minute, second,
     &     refyear, leapfile, abssecs, status)

      integer year, month, day, hour, minute
      double precision second, abssecs
      integer refyear, status, leapsec
      integer dostart, dostop, step
      character*(*) leapfile

      character(80) context
      integer locrefyear, daysinmonth(12), i, locyear
      integer leaps, newleaps
      logical fcislpyr

      data daysinmonth /31,28,31,30,31,30,31,31,30,31,30,31/

C check that year and refyear contain the appropriate 4 digits
      locyear = year
      if (locyear .lt. 100) then
         locyear = locyear + 1900
      endif

      locrefyear = refyear
      if (locrefyear .lt. 100) then
         locrefyear = locrefyear + 1900
      endif

C calculate seconds from ref year without leap seconds
      abssecs = (locyear - locrefyear) * 365.

C need to take into account which direction
      if (abssecs .lt. 0.) then
         dostart = locyear
         dostop = locrefyear - 1
         step = -1
      else
         dostart = locrefyear
         dostop = locyear - 1
         step = 1
      endif

      do 10 i = dostart, dostop
          if ( fcislpyr(i) ) then
              abssecs = abssecs + step
          endif
 10   continue

      do 20 i = 1, month-1
         abssecs = abssecs + daysinmonth(i)
 20   continue
      if ((month .gt. 2) .and. fcislpyr(locyear)) then
          abssecs = abssecs + 1
      endif

      abssecs = (abssecs + day - 1)*24.
      abssecs = (abssecs + hour)*60.
      abssecs = (abssecs + minute)*60. + second

C now figure the leap seconds
      if (locrefyear .lt. 1969) then
         context = 'WARNING: cannot determine leapseconds correctly'
         call fcecho (context)
      endif

      leaps = leapsec(abssecs,locrefyear,leapfile,status)

C check if this gives us another leapsecond... include an extra second
C in case we land right on a leap second... for positive offsets only

      if (leaps.gt.0) leaps = leaps + 1
      newleaps = leapsec(abssecs+leaps,locrefyear,leapfile,status)
      abssecs = abssecs + newleaps

 999  continue

      return
      end
