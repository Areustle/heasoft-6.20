**==expf.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION EXPF(X)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL EXPF , X
C*** End of declarations inserted by SPAG
      EXPF = 1.0E-37
      IF ( X.GE.-75. .AND. X.LE.75. ) EXPF = EXP(X)
      IF ( X.GT.75. ) EXPF = 1.0E37
      RETURN
      END
 
