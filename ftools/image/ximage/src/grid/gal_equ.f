      SUBROUTINE gal_equ(lii,bii,a,d,n)
      IMPLICIT NONE
C----------------------------------------------------------------------
C astro	utility routine
C	converts lii,bii to (a,d) (1950.0)
C	units are radians
C----------------------------------------------------------------------
      REAL*8 a(1), d(1), lii(1), bii(1)
      INCLUDE '../include/pi.inc'
      REAL*8 ll, sinb, cosb, sinl, cosl, expr, c626, s626
      PARAMETER (c626=0.4601997847838517D0,s626=0.8878153851364013D0)
      INTEGER i, n
*
* the conversion formulae are
*	cos(d)sin(a-282.25) = cos(bii)sin(lii-33)cos(62.6) - sin(bii)sin(62.6)
*	cos(d)cos(a-282.25) = cos(bii)cos(lii-33)
*	sin(d)		    = cos(bii)sin(lii-33)sin(62.6) + sin(bii)cos(62.6)
*
      DO i = 1, n
         ll = lii(i) - 33.D0*pi/180.D0
         sinb = sin(bii(i))
         cosb = cos(bii(i))
         sinl = sin(ll)
         cosl = cos(ll)
         expr = cosb*sinl
         d(i) = asin(expr*s626+sinb*c626)
         a(i) = atan2(expr*c626-sinb*s626,cosb*cosl)
     &           + 282.25D0*pi/180.D0
      END DO
      END
