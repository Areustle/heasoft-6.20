      FUNCTION dell(z,q0,h0)
c      IMPLICIT REAL*8(a-h,o-z)
      real*8 z, h, dell
      REAL*4 q0, h0
c       returns luminosity distance dl at redshift z
c       units are megaparsecs
      h = h0/50.
      dell = 6000.D0*(1.D0-q0+q0*z+(q0-1.D0)*sqrt(2.D0*q0*z+1.D0))
     &       /q0/q0*h
      RETURN
      END
