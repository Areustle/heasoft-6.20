      SUBROUTINE equ_gal(a,d,lii,bii,n)
      IMPLICIT NONE
C----------------------------------------------------------------------
C astro	utility routine
C	converts (a,d) (1950.0) in lii,bii
C	units are radians
C----------------------------------------------------------------------
      REAL*8 a(1), d(1), lii(1), bii(1)
      INCLUDE '../include/pi.inc'
      REAL*8 aa, cosa, sina, cosd, sind, expr, c626, s626
      PARAMETER (c626=0.4601997847838517D0,s626=0.8878153851364013D0)
      INTEGER i, n
*
* formulaes are in degrees (lang k.r. page 504)
*
*	sin(bii) 	     = sin(d)cos(62.6) - cos(d)sin(a-282.25)sin(62.6)
*	cos(bii)*cos(lii-33) = cos(d)cos(a-282.25)
*	cos(bii)*sin(lii-33) = cos(d)sin(a-282.25)cos(62.6) + sin(d)sin(62.6)
*
      DO i = 1, n
         aa = a(i) - 282.25D0*pi/180.D0
         cosa = cos(aa)
         sina = sin(aa)
         cosd = cos(d(i))
         sind = sin(d(i))
         expr = cosd*sina
         bii(i) = asin(sind*c626-expr*s626)
         lii(i) = atan2((expr*c626+sind*s626),cosd*cosa)
     &             + 33.D0*pi/180.D0
      END DO
      END
