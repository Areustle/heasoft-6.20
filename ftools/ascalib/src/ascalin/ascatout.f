C******************************************************************************
C FUNCTION:
C
C     ascatout
C        
C DESCRIPTION:
C
C     ascatime to UT.
C
C AUTHOR/DATE:
C     
C     Eric Gotthelf, March 1994.
C     ASCA GOF, NASA/GSFC
C     
C MODIFICATION HISTORY:
C     
C USAGE:
C
C     call ascatout(ascatime,year,month,day,hour,mins,sec)
C
C ARGUMENTS:
C
C     ascatime                   - ASCA time, E1993.0D0 (seconds)
C     year,month,day,hour,mins   - integers
C     sec                        - real
C
C REFERENCE:
C
C     Astronomy on your pocket calculator, 
C
C******************************************************************************

      subroutine ascatout(ascatime,year,month,day,hour,mins,sec)
      implicit none

      double precision ascatime
      integer month,day,year,hour,mins
      real sec

      integer i, na, nb, nc, nd, ne, ng
      double precision jd, fd, fh, fm, f, jd_zero
      parameter (jd_zero = 2448988.5d0)
      
      jd = ascatime/86400.0d0 + jd_zero

      i = int(jd+0.5D0)
      f = (jd+0.5D0)-dble(i)

      nb = i
      if (i.gt.2299160) then
         na = int((float(i)-1867216.25)/36524.25)
         nb = i + 1 + na - int(na/4)
      else
         na = i
      end if

      nc = nb + 1524
      nd = int((real(nc)-122.1)/365.25)
      ne = int(365.25*real(nd))
      ng = int(real(nc-ne)/30.6001)

      fd = f + dble(nc - ne - int(30.6001*real(ng)))
      day = int(fd)

      fh = (fd - dble(day)) * 24.0D0
      hour = int(fh)

      fm = (fh - dble(hour)) * 60.0D0
      mins = int(fm)

      sec = (fm - dble(mins)) * 60.0

      if (real(ng) .gt. 13.5) then
         month = ng - 13
      else
         month = ng - 1
      end if

      if (real(month) .gt. 2.5) then
         year = nd - 4716
      else
         year = nd - 4715
      end if

      end


