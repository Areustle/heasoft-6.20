      SUBROUTINE xsbape(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(*),photar(ne), photer(ne)

C Runs the APED interpolation with thermal and velocity broadening

c            parameters (param):
c            (1):      kT temperature in keV
c            (2):      Metal abundances (He fixed at cosmic)
c            (3):      redshift z
c            (4):      gaussian velocity broadening
c            Norm = (4 * pi * 1e14)^-1 * Int(n^2)dV / D^2 where n is
c            in cm^-3 and D is the distance in cm.

      INTEGER i
      REAL rparm(16)

c set the parameters required xsbvpe - takes the individual abundances.

      rparm(1) = param(1)
      rparm(2) = 1.
      DO i = 3, 14
         rparm(i) = param(2)
      ENDDO
      rparm(15) = param(3)
      rparm(16) = param(4)

      CALL xsbvpe(ear, ne, rparm, ifl, photar, photer)

      RETURN
      END
