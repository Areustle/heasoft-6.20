      REAL FUNCTION THERM(E, T)
      REAL e, t, fbg, cfac, alf, gf
      IF (T.GT.20.) THEN
         GOTO 100
      ENDIF
      THERM = FBG(E, T, 1.) + .34*FBG(E, T, 2.)
      RETURN
 100  CFAC = 1./(.31968*EXP(-.3485*E)+.7840)
      ALF = .37*(30./T)**0.15
      GF = 0.90*(T/E)**ALF
      THERM = CFAC*GF/E*EXP(-E/T)
      RETURN
      END
