**==cthei.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION CTHEI(N,J,T,Dene)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL CTHEI , d1 , Dene , p3 , qdw , qup , r21 , T , t4
      INTEGER J , N
C*** End of declarations inserted by SPAG
C  SCOTT'S RATES FOR HE+  + J  TO HE0  + J+1
C  THROUGH ARGON  0.1 < T4 < 10.
      CTHEI = 0.
      t4 = 0.0001*T
      IF ( J.EQ.1 .OR. J.GE.4 ) RETURN
      IF ( J.NE.3 ) THEN
         IF ( N.EQ.6 ) CTHEI = 5.3E-10*EXP(-13.2/t4)
C  ALBERT'S RATES FOR N+ FROM 1D LEVEL
         IF ( N.EQ.7 ) THEN
            qdw = 5.14E-06*Dene/SQRT(T)
            qup = 2.86E-6*EXP(-21826./T)
            r21 = qup/(qdw+.0041)
            d1 = r21/(1.+r21)
            p3 = 1. - d1
            CTHEI = p3*4.1E-10*EXP(-10.4/t4) + d1*1.1E-10*EXP(-3.59/t4)
         ENDIF
         IF ( N.EQ.14 ) CTHEI = 2.9E-10*EXP(-8.7/t4)
         IF ( N.EQ.18 ) CTHEI = 1.1E-10*EXP(-3.57/t4)
         RETURN
      ENDIF
      IF ( N.EQ.14 ) CTHEI = 3.4E-9*EXP(-10.5/t4)
      IF ( N.EQ.16 ) CTHEI = 4.8E-10*EXP(-15.7/t4)
      IF ( N.EQ.26 ) CTHEI = 1.2E-9*SQRT(t4)*EXP(-12./t4)
      RETURN
      END
 
