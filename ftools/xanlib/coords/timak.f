      SUBROUTINE timak(tarr,key)
c        i*4 array to shf key  <810918>
c
c modified by nick, 26/7/89 to start from 1976.
c modified by paul, 21/6/90 to start from 1971.
c nick, 14/10/92 modified to cover 1971-2000, 
c                corrected error for 1999
c modified by pat, 4/1/93 to start from 1968.
c modified by micah, 7/10/98 to extend to 2007
c modified by bryan, 1/04/08 to have no expiration date
c
      INTEGER*4 key, year, day, days_in_year, sec_in_year
      INTEGER*4 tarr(5), year_step, start_year, year_adjust
c
c     Zero out the day number (seconds are calculated starting from
c     day/tarr(2)=0, hour/tarr(3)=0, min/tarr(4)=0, sec/tarr(5)=0):
      IF ( tarr(2).NE.0 ) then
         day = tarr(2) - 1
      ELSE
         day = tarr(2)
      END IF

c     Initial condition is key = zero on 01 Jan 1980 00:00:00:
      if ( tarr(1) .lt. 1980 ) then
         start_year = 1979
         year_step = -1
         year_adjust = -1
         sec_in_year = 365*86400
      else
         start_year = 1980
         year_step = 1
         year_adjust = 0
         sec_in_year = 0
      endif

      do year = start_year, tarr(1), year_step

        key = ((day*24+tarr(3))*60+tarr(4))*60 + tarr(5) +
     &          year_step*sec_in_year

c       Compute seconds in this year for use next trip through the loop:
        if ((mod(year+year_adjust,4).eq.0) .and.
     &      ((mod(year+year_adjust,100).ne.0).or.
     &       (mod(year+year_adjust,400).eq.0))) then
          days_in_year=366
        else
          days_in_year=365
        endif

        sec_in_year = sec_in_year + days_in_year*86400

      end do
c
      RETURN
      END
