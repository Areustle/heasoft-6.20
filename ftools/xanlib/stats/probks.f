      real*4 FUNCTION probks(alam)
C     this subroutine has been taken from numerical recepies w.press et al.
C     alam=sqrt(a)*d
C     a=n1*n2/(n1+n2)   d=max distance of two cumulative distributions
c
      integer*4 j
      real*4 alam, a2, fac, termbf, term
      a2 = -2.*alam**2
      fac = 2.
      probks = 0.
      termbf = 0.
      DO j = 1, 100
        term = fac*exp(a2*j**2)
        probks = probks + term
        IF ( abs(term).LT.0.001*termbf ) RETURN
        fac = -fac
        termbf = abs(term)
      END DO
      probks = 1.
      RETURN
      END
