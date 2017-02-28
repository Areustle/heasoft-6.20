**==cther.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION CTHER(N,J,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , cr , CTHER , T , t4
      INTEGER in , J , N
C*** End of declarations inserted by SPAG
C  FITS TO SCOTT'S RATES FOR  HE I + J   TO   HE II + J-1
C  1000  TO 100,000 K THROUGH ARGON
      DIMENSION a(2,9)
      DATA a/4*0. , .059 , .00001 , 1.1 , 0.7 , .00001 , 1.7 , .075 , 
     &     2.2 , .096 , 1.2 , 1.1 , .0008 , .00001 , 1.0/
C
      CTHER = 0.
      IF ( J.EQ.1 ) RETURN
      t4 = AMAX1(.0001*T,0.1)
      t4 = AMIN1(t4,10.)
      cr = 5.4E-10*t4
      IF ( J.EQ.2 .OR. N.GT.18 ) RETURN
      CTHER = AMAX1(cr,(-.1+.4*J)*1.0E-9)
      IF ( J.GE.6 ) RETURN
      IF ( J.EQ.3 ) THEN
         CTHER = 0.
C  INCLUDE ALBERT'S RATE TO 1D OF N+
         IF ( N.EQ.7 ) CTHER = 3.0E-10*(1.+.26*t4) + 6.2E-11
         IF ( N.EQ.8 ) CTHER = 3.3E-10*t4**.7
         IF ( N.EQ.18 ) CTHER = 1.3E-10
         RETURN
      ELSEIF ( N.NE.7 ) THEN
         in = N/2
         CTHER = a(J-3,in)*1.0E-9
         IF ( J.NE.5 ) THEN
            IF ( N.EQ.6 ) CTHER = 5.1E-11*t4**1.46
            IF ( N.EQ.14 ) CTHER = 9.6E-10*t4**.55
            IF ( N.EQ.16 ) CTHER = 1.1E-9*t4**.63
            RETURN
         ENDIF
      ELSE
         CTHER = 1.1E-10
         IF ( J.EQ.5 ) CTHER = 2.0E-9
         RETURN
      ENDIF
      IF ( N.EQ.18 ) CTHER = 1.0E-9*t4**.91
      RETURN
      END
 
