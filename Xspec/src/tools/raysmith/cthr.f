**==cthr.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION CTHR(N,J,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , b , CONce , CTHR , DNE , GNDrec , HENeut , HEPlus , 
     &     PCOol , PM , POT , POU , POWer , RE , RHY , T , t4 , TU
      INTEGER J , N
C*** End of declarations inserted by SPAG
C  SCOTT'S RATES FOR H I + J TO H II + (J-1)
C  THROUGH ARGON : NEUFELT'S FE II - FE III
C
      DIMENSION a(30) , b(30)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      DATA a/0. , 0. , .00001 , 0. , 5.9 , 6.2 , 7.8 , 9.4 , 11. , 13. , 
     &     14. , 16.1 , 17.6 , 19. , 20.4 , 21.8 , 23.3 , 13*0./
      DATA b/4*0. , .25 , .18 , .13 , .1 , .08 , .05 , .04 , 19*0./
C
      CTHR = 0.
      IF ( J.EQ.1 ) RETURN
      t4 = AMAX1(T*.0001,0.1)
      t4 = AMIN1(t4,10.)
C  NEUFELD'S FE II - FE III, FE III-FE IV, AND NI II - NI III RATES
      IF ( N.EQ.26 .AND. J.EQ.3 ) CTHR = 1.0E-9*(1.25+.25*ALOG10(t4))
      IF ( N.EQ.26 .AND. J.EQ.4 ) CTHR = 3.4E-9*SQRT(t4)
      IF ( N.EQ.28 .AND. J.EQ.3 ) CTHR = 1.0E-9*(.34+1.86*t4)
      IF ( N.GT.18 ) RETURN
      CTHR = a(J)*(1.+b(J)*t4)*1.0E-9
      IF ( J.GE.6 ) RETURN
      IF ( N.EQ.2 ) THEN
         CTHR = 1.9E-15
         IF ( J.EQ.3 ) CTHR = 1.7E-13
         RETURN
      ELSEIF ( N.EQ.6 ) THEN
C  ALBERT'S C III TRIPLET P
         IF ( J.EQ.3 ) CTHR = 2.5E-9*PM(1)*EXP(-15000./T)
         IF ( J.EQ.4 ) CTHR = 2.9E-9
         IF ( J.EQ.5 ) CTHR = 7.6E-10*t4**1.48
         RETURN
      ELSEIF ( N.EQ.7 ) THEN
         CTHR = 1.1E-12/(1.+.1*t4)
         IF ( J.EQ.3 ) CTHR = 5.2E-10
         IF ( J.EQ.4 ) CTHR = 2.7E-9*t4**.926
         IF ( J.EQ.5 ) CTHR = 1.7E-10*t4**1.40
C  ALBERT'S N VI
         IF ( J.EQ.6 ) CTHR = 6.6E-10
         RETURN
      ELSEIF ( N.EQ.8 ) THEN
         CTHR = 4.0E-10
         IF ( J.EQ.3 ) CTHR = 7.7E-10*SQRT(t4)
         IF ( J.EQ.4 ) CTHR = 2.1E-9
         IF ( J.EQ.5 ) CTHR = 1.4E-10*(1.+t4)
C  ALBERT'S O VII : NEED O VI
         IF ( J.EQ.7 ) CTHR = 5.4E-8
         RETURN
      ELSEIF ( N.EQ.10 ) THEN
         IF ( J.EQ.4 ) CTHR = 3.8E-9*SQRT(t4)
         RETURN
      ELSEIF ( N.EQ.12 ) THEN
         IF ( J.EQ.4 ) CTHR = 4.4E-9*(1.+.37*t4)
         RETURN
      ELSEIF ( N.EQ.14 ) THEN
         IF ( J.EQ.3 ) CTHR = 4.0E-9*t4**.23
         IF ( J.EQ.4 ) CTHR = 4.1E-10
         IF ( J.EQ.5 ) CTHR = 2.2E-9*(1.+.1*t4)
         RETURN
      ELSEIF ( N.EQ.16 ) THEN
         IF ( J.EQ.4 ) CTHR = 2.5E-9
         IF ( J.EQ.5 ) CTHR = 7.0E-9
         RETURN
      ELSEIF ( N.EQ.18 ) THEN
         IF ( J.EQ.4 ) CTHR = 4.4E-8*t4**.27
         RETURN
      ENDIF
      RETURN
      END
 
