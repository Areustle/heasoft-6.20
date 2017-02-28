cccccccccc
c     asca2ut --- convert the ASCA time to UT
c     Ken Ebisawa NASA/GSFC 94/04/21
c     Jeff Guerber, GSFX/RSTX 1998-10-21: Updated leapseconds and use loop.
c           Note: Really, this ought to be replaced by something that reads
c           the leapsec.fits file, eg. sec2int (but that has problems).
ccccccccc
      subroutine asca2ut(ascatime, day, month, year, hour,
     $     minute, second)

      implicit none
c     Input -- asca time == elapsed time from the beginning of 1993
      double precision ascatime
c     Output
      integer day, month, year, hour, minute
      real second

C     daysyr: number of days in the year (function of the year)
      integer daysyr

c     The leap second at July, 1st 93, 00:00:00
c     double precision leap1/15638401.0D0/
c     The leap second at July, 1st 94, 00:00:00
c     double precision leap2/47174401.0D0/

c     All the leapseconds, in seconds since 1993-01-01T00:00:00
      integer nleap, i
      parameter (nleap = 5)
      double precision leap(nleap)
      data leap
     &    / 15638401.0D0,  47174402.0D0,  94608003.0D0,
     &     141868804.0D0, 189302405.0D0 /

c     dummy variable (so that ascatime is not changed due to leap seconds)
      double precision dm_ascatime

c      write(*,*) ascatime, day, hour, minute, second

      dm_ascatime = ascatime

c      if (ascatime.ge.leap1) then
c         dm_ascatime = dm_ascatime - 1.0D0
c         if (ascatime.ge.leap2) then
c            dm_ascatime = dm_ascatime - 1.0D0
c         endif
c      endif

      do i = 1, nleap
          if (ascatime .ge. leap(i)) then
c             Note: assumes positive leap seconds
              dm_ascatime = dm_ascatime - 1.0D0
          endif
      enddo

      day = dm_ascatime/86400
      hour = (dm_ascatime - day*86400) / 3600
      minute = (dm_ascatime - day*86400 - hour*3600)/60
      second = dm_ascatime - day*86400 - hour*3600 - minute*60

c      write(*,*) ascatime, day, hour, minute, second

      year = 1993
      do while (day .gt. daysyr(year))
         year = year + 1
         day = day - daysyr(year)
      end do
c      write(*,*) year, day
      call day2month(year, day, month)
c      write(*,*) day, month
C     Actually, the first day of the year is Jan. 1, not Jan. 0 !
      day = day + 1
c      write(*,*) day, month
      end

cccccccccc
c     function daysyr --- returns the number of days in the year
c     Ken Ebisawa NASA/GSFC 94/04/21
ccccccccc
      integer function daysyr(year)
      integer year

      if (mod(year,4).eq.0) then
         if (mod(year,100).eq.0) then
            if (mod(year,400).eq.0) then
               daysyr = 366
            else
               daysyr= 365
            endif
         else
            daysyr = 366
         endif
      else
         daysyr = 365
      endif

      end

cccccccccc
c     day2month ---- convert the day of the year to month and date
c     Ken Ebisawa NASA/GSFC 1994/04/21
ccccccccc
      subroutine day2month(year, day, month)
C     Input year
      integer year
C     Input/Output day=day of the year/day of the month
C     Output month = month (1-12)
      integer day, month

      integer days_in_month(12)
      data days_in_month/31,28,31,30,31,30,31,31,30,31,30,31/
      if(year .eq. 1996 .or. year .eq. 2000) days_in_month(2) = 29

      month = 1

      do while( day .gt. days_in_month(month) )
         day = day - days_in_month(month)
         month  = month + 1
c         write(*,*) month, day
      end do

      end
