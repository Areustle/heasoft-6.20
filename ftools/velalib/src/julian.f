C This function converts a year, day of year (doy), and seconds of day (sod)
C  into a julian date.  To conserve programming energy [ ;-) ], the
C  function was designed to use existing julian date calculation code
C  rather than to compute in the most elegant way.
C
C The algorithms are adapted from ACM Algorithm 199 by Robert Tantzen,
C  Comm.ACM, 6, 8, Aug.1963, p.444.
C
C Author: Jesse S. Allen
C History:
C  Vers 1.0   7 Dec 1994  Created for Vela 5B FTOOLS
C       1.1   7 Mar 1995  Altered leap year calculation to improve performance
C       1.11 21 Feb 1996  Correct FDAY error for Jan 1 (!= Jan 0)
C       1.2  12 Aug 1998  Jeff Guerber RSTX/GSFC  Fixed for impending year 2000
C            jd2mysub- ret full year as in JDATE, also use if/then/else
C            dmy2jdsub- use JDAY not KDAY, check 2-digit years, use int args
C            ut2jd- pass int args to dmy2jdsub
C            jd2ut- jd2mysub now returnss full year so don't add 1900
C            julian- do leap years right
C
C NOTE: year may now be the full year OR the last two digits (e.g. 68 or 1968)

       double precision function julian(year, doy, sod)

       integer year, doy, month, day, hour, min
       integer fday(12)

       real sec, sod

       data fday/1,32,60,91,121,152,182,213,244,274,305,335/

       if ( ((mod(year,4) .eq. 0) .and. (mod(year,100) .ne. 0) )
     &     .or. (mod(year,400) .eq. 0) ) then
          do month = 3, 12
             fday(month) = fday(month) + 1
          end do
       endif
       do month = 1, 11
          day = doy - fday(month+1) + 1
          if (day .le. 0) then
             day = doy - fday(month) + 1
             go to 100
          endif
       end do
       month = 12
 100   hour = INT(sod / 3600.0)
       min = INT((sod - (hour * 3600)) / 60.0)
       sec = sod - REAL((hour * 3600) + (min * 60))
       call ut2jd(julian, year, month, day, hour, min, sec)

       return

       end

C---------------------------------------------------------------------

       subroutine jd2ut(julian, year, month, day, hour, min, sec)

C   programmer: F. Schwab and EWG
C   Adapted from ACM algorithm No. 199.
C   Modified by Jesse Allen (04 Oct 1994)

       INTEGER*2 itime(6)
       INTEGER year, month, day, hour, min
       REAL sec
       DOUBLE PRECISION julian

       call jd2dmysub(julian,itime)
       year = itime(1)
       month = itime(2)
       day = itime(3)
       hour = itime(4)
       min = itime(5)
       sec = itime(6)

       RETURN

       END


       subroutine ut2jd(julian, year, month, day, hour, min, sec)

C   programmer: F. Schwab and EWG
C   Adapted from ACM algorithm No. 199.
C   Modified by Jesse Allen (04 Oct 1994)

       INTEGER year, month, day, hour, min
       REAL sec
       DOUBLE PRECISION julian, fracday

       fracday = (DBLE(hour)/24.D0) + (DBLE(min)/3600.D0) +
     &   (DBLE(sec)/86400.D0)
       CALL dmy2jdsub(year, month, day, julian)
       julian = julian + fracday

       RETURN

       END


      subroutine jd2dmysub (jd, it)
c   jd2dmysub converts a real*8 Julian day number to the calendar date
c   and time in the form yy, mm, dd, hh, mm, ss
c   inputs: jd      r*8   Julian date.
c   output: it(6)   i*2   calender day and time.
c   programmer: F. Schwab and EWG
c   Adapted from ACM algorithm No. 199, routine JDATE

      integer*2 it(6), id, im, iy
      real*8    jd, j, y, d, m, dint

      j = dint (jd + 0.5d0 - 1721119.d0)
      y = dint ((4.d0*j - 1.0d0) / 146097.d0)
      j = 4.d0*j - 1.0d0 - 146097.d0*y
      d = dint (j * 0.25d0)
      j = dint ((4.0d0*d + 3.0d0) / 1461.0d0)
      d = 4.0d0*d + 3.0d0 - 1461.0d0*j
      d = dint ((d+4.0d0) * 0.25d0)
      m = dint ((5.0d0*d - 3.0d0) / 153.0d0)
      id = 5.0d0*d - 3.0d0 - 153.0d0*m
      id = (id + 5) / 5
      iy = 100 * y + j
      im = m
      if (im .lt. 10) then
         im = im + 3
      else
         im = im - 9
         iy = iy + 1
      endif
      it(1) = iy
      it(2) = im
      it(3) = id
      j = dint (jd + 0.5d0)
      j = (jd + 0.5d0 - j) * 24.0d0
      it(4) = j
      j = (j - it(4)) * 60.0d0
      it(5) = j
      it(6) = (j - it(5)) * 60.0d0 + 0.5d0
 999  return
      end


	subroutine dmy2jdsub (yy, mm, dd, jd)
C   DMY2JDSUB converts a character encoded Gregorian calender date
C   into a R*8 Julian day number.
C   Inputs:
C       YY, MM, DD   Integer Year, Month, Day
C   Output:
C       JD      R*8 Julian date.
C   Algorithm from ACM Algorithm number 199, routine JDAY
C   This routine is good for any Gregorian date.

	integer   dd, mm, yy
        real*8    jd, d, m, ya, c
c                                       Convert to r*8
        ya = yy
        if (yy .lt. 100) ya = ya + 1900.0d0
        m  = mm
        d  = dd
        if (m .gt. 2.0d0) then
        	m = dint (m - 3.0d0)
	else
		m = dint (m + 9.0d0)
		ya = dint (ya - 1.0d0)
	end if
        c = dint(ya / 100.0d0)
        ya = ya - 100 * c
	jd = dint(146097.0d0 * c * 0.25d0) +
     &      dint (1461.0d0 * ya * 0.25d0) +
     &      dint ((153.0d0 * m + 2.0d0) * 0.2d0) + d + 1721118.5
c
	return
	end

