c *************************************************************
c  real function to return Gaussian response value for a photon
c  of energy, E, falling within the boundaries, BOUNDS, for a
c  detector with energy resolution SIGMA.
 
      FUNCTION GRSPVL(Energy,Lbound,Ubound,Sigma)

      IMPLICIT NONE

      REAL GRSPVL, Energy , Lbound, Ubound, Sigma
 
      REAL xh, xl, gxh, gxl
      REAL elow, ehi

      IF ( Ubound .GE. Lbound ) THEN
         elow = Lbound
         ehi = Ubound
      ELSE
         elow = Ubound
         ehi = Lbound
      ENDIF
 
      xh = ABS(ehi-Energy)/Sigma
      xl = ABS(elow-Energy)/Sigma
      CALL AGAUSS(xh,gxh)
      CALL AGAUSS(xl,gxl)
      IF ( Energy.GT.elow .AND. Energy.LE.ehi ) THEN
         GRSPVL = ABS(gxl+gxh)/2.
      ELSE
         GRSPVL = ABS(gxl-gxh)/2.
      ENDIF
 
      END
 
 
