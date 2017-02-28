      FUNCTION LEVER_X(D,D0)         !  HW  March 1992  XIMAGE 
C----------------------------------------------------------------------
C	Lever d'une etoile de declinaison D a latitude D0
C
C	PI/2-D0 < D	Circumpolaire
C	D0-PI/2 < D < PI/2-D0	H = ACOS (TAN(D)/TAN(D0-PI/2))
C	D < D0-PI/2	Toujours couchee...
C
C Formule applicable a la partie visible du ciel en projection
C sur un plan
C----------------------------------------------------------------------
      REAL*8 D,D0,LEVER_X,DD,D1,D2
      include '../include/pi.inc'
      REAL*8 PRECISION
      PARAMETER (PRECISION=1.0D-10)
*
      IF (D0.LT.0.D0) THEN
         D2 = -D
         D1 = -D0
      ELSE
         D2 = D
         D1 = D0
      ENDIF
      DD = D1-PI*0.5D0
      IF (D2.GT.-DD+PRECISION) THEN
         LEVER_X = 2.D0*PI
      ELSEIF (D2.LT.DD) THEN
         LEVER_X = -PI
      ELSEIF (ABS(D2+DD).LE.PRECISION) THEN
         LEVER_X = PI
      ELSE
         LEVER_X = ACOS (TAN(D2)/TAN(DD))
      ENDIF
      END
