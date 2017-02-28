      SUBROUTINE DEC_TO_EXP(Value,Mm,Pp,Sig)
      IMPLICIT NONE
c
c calculates mm and pp where:
c mm*10**pp ~ value and
c sig is the minimum number of significant digits to be savedin mm
c
c  Values are used by PGNUMB to create plottable string from number
c
      REAL*4 Value
      INTEGER*4 Mm , Pp , Sig
 
      IF ( Value.GT.0 ) THEN
         Pp = LOG10(Value) - Sig
      ELSEIF ( Value.LT.0 ) THEN
         Pp = LOG10(ABS(Value)) - Sig
      ELSE
         Pp = 0
      ENDIF
 
      Mm = NINT(Value/10.**Pp)
 
      RETURN
      END
