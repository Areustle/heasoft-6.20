

C******************************************************************************
C SUBROUTINE:
C      sec2int
C
C DESCRIPTION:
C       determine the yr, day, hour, etc based on the seconds from a reference
C       (PDW: input seconds should *include* any leap seconds since the refrnc)
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       November, 1993
C
C       Routine rewritten by Bryan Irby, L-3 GSI, December 2005
C
C MODIFICATION HISTORY:
C      3/30/94 EAG For times before the reference time INT() truncates
C                  in the wrong direction.  Add another year to make up.
C      1998-07-03 Jeff Guerber.  Use fcislpyr().
C      1998-12-24 Peter Wilson.  Preserve input seconds; Make it the inverse
C                                of int2sec, by assuming input seconds includes
C                                any leap seconds.
C      1998-12-28 Peter Wilson.  Bug fix for leap years: don't count ref year
C                                when time offset is negative
C      2005-12-23 Bryan Irby.    Rewrote algorithm to fix 24-hour discrepancy
C                                which occurs around the end of leap years,
C                                when the # of days between the date and 12/31
C                                is <= the # of leap days to account for
C                                between the date and 1993.
C
C NOTES:
C
C USAGE:
C
C ARGUMENTS:
C       status  - status of operation
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine sec2int (inSecs, refyear, leapfile, year, month,
     &     day, hour, minute, second, status)

      double precision inSecs, seconds, second
      integer refyear, year, month, day, hour, minute, status
      character*(*) leapfile

      integer leaps, leapsec, daysinmonth(12), maxdays, step
      logical fcislpyr

      data daysinmonth /31,28,31,30,31,30,31,31,30,31,30,31/

C Copy input to a local variable
      seconds = inSecs

C find the number of leap seconds between refyear and seconds
C PDW 12/24/98:
C     The +1 forces leap seconds to create two :59s instead of two :00s

      leaps = leapsec(seconds+1, refyear, leapfile, status)
      seconds = seconds - leaps

C     Determine direction (positive or negative):
C     -------------------------------------------
      if (seconds .ge. 0) then
         step = 1
         year = refyear
         month = 1
         day = 1
      else
         step = -1
         year = refyear - 1
         month = 12
         day = 31
      endif

C     Decrement seconds until year, month, and 24-hour period are determined:
C     -----------------------------------------------------------------------
      do while (step*seconds .ge. 86400.)
         seconds = seconds - step*86400.
         day = day + step
         if ( fcislpyr(year) .and. (month .eq. 2) ) then
            maxdays = 29
         else
            maxdays = daysinmonth(month)
         endif
C        If moving forwards in time [step=1]:
         if (day .gt. maxdays) then
            month = month + 1
            day = 1
         endif
         if (month .gt. 12) then
            month = 1
            year = year + 1
         endif
C        If moving backwards in time [step=-1]:
         if (day .eq. 0) then
            month = month - 1
            if (month .eq. 0) then
               month = 12
               year = year - 1
            endif
            if ( fcislpyr(year) .and. (month .eq. 2) ) then
               day = 29
            else
               day = daysinmonth(month)
            endif
         endif
      enddo

C     Handle remaining seconds (-86400 < seconds < 86400).
C     ----------------------------------------------------
C     Negative direction [step=-1] (-86400 < seconds <= 0):
      if ( (seconds .le. 0) .and. (step .eq. -1) ) then
         seconds = seconds + 86400.
C        Now 0 < seconds <= 86400).
         hour = int(seconds/3600.)
C        If seconds was = 0 and is now = 86400, reset hour and increment day.
C        Increment month & year if necessary.
         if (hour .eq. 24) then
            seconds = seconds - 3600*hour
            hour = 0
            day = day + 1
            if ( fcislpyr(year) .and. (month .eq. 2) ) then
               maxdays = 29
            else
               maxdays = daysinmonth(month)
            endif
            if (day .gt. maxdays) then
               month = month + 1
               day = 1
            endif
            if (month .gt. 12) then
               month = 1
               year = year + 1
            endif
C        If hour < 24 (seconds < 86400):
         else
            seconds = seconds - 3600*hour
         endif
C     Positive direction [step=1] (0 <= seconds < 86400):
      else
         hour = int(seconds/3600.)
         seconds = seconds - 3600*hour
      endif

      minute = int(seconds/60.)
      seconds = seconds - 60*minute
      second = seconds

      return
      end
