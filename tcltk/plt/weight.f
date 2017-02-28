      REAL FUNCTION WEIGHT(Ydat, Mxrow, Iery)
      REAL      Ydat(*)
      INTEGER   Mxrow, Iery
C---
C Compute the statistical weight for point Y(1).  If Iery>0 then
C error is assumed to be at location Y(1+Mxrow).
C---
C Ydat      I
C Mxrow     I
C Iery      I    <0 Poisson
C                =0 No weighting
C                >0 Explicit error
C---
C [AFT]
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
C---
      WEIGHT=0.
      IF ( Ydat(1).EQ.NO ) RETURN
C      IF ( Iery ) 10,20,30
      IF ( Iery.GT.0 ) GOTO 30
      IF ( Iery.EQ.0 ) GOTO 20
C---
   10 CONTINUE
      IF ( Iery.EQ.-2 ) THEN
C Gehrels (ApJ 1986, 303, p336)
         WEIGHT=1./(1.+SQRT(0.75+ABS(Ydat(1))))**2
      ELSE
C Poisson
         WEIGHT=1./MAX(1.0,ABS(Ydat(1)))
      END IF
      RETURN
C---
C No weighting
   20 CONTINUE
      WEIGHT=1.
      RETURN
C---
C Use formal errors
   30 CONTINUE
      IF ( Iery.GE.4 ) THEN
C Special case, error is the weight.
         WEIGHT=Ydat(1+Mxrow)
         RETURN
      END IF
      IF(Ydat(1+Mxrow).GT.0.) WEIGHT=1./(Ydat(1+Mxrow)*Ydat(1+Mxrow))
      RETURN
      END
