**==cthi.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION CTHI(N,J,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL CTHI , T , t4
      INTEGER J , N
C*** End of declarations inserted by SPAG
C  SCOTT'S RATES FOR   H II  +  J    TO    HI + J+1
C  THROUGH ARGON
      t4 = AMAX1(.0001*T,0.1)
      t4 = AMIN1(t4,10.)
      CTHI = 0.
      IF ( J.GT.2 ) RETURN
      IF ( J.NE.2 ) THEN
         IF ( N.EQ.6 ) CTHI = 2.5E-15
         IF ( N.EQ.7 ) CTHI = 3.3E-13*t4**.12
         IF ( N.EQ.8 ) CTHI = 3.3E-10
         IF ( N.EQ.16 ) CTHI = 6.7E-10
         RETURN
      ENDIF
      IF ( N.EQ.12 ) CTHI = 2.6E-11*EXP(-6.0/t4)
      IF ( N.EQ.14 ) CTHI = 9.6E-10*EXP(-2.74/t4)
C  NEUFELD'S FE II - FE III RATES
      IF ( N.EQ.26 ) CTHI = 1.0E-9*1.666*(1.25+.25*ALOG10(t4))
     &                      *EXP(-3.005/t4)
      RETURN
      END
 
