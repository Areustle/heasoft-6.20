**==fable.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION FABLE(X,Y,Z)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL duck , EXPF , FABLE , X , Y , Z
C*** End of declarations inserted by SPAG
      IF ( X.NE.Y ) THEN
         duck = (EXPF(-X*Z)-EXPF(-Y*Z))/(Y-X)
      ELSE
         duck = Z*EXP(-X*Z)
      ENDIF
      FABLE = duck
      RETURN
      END
 
