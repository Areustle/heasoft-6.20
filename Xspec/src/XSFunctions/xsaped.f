      SUBROUTINE xsaped(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(*),photar(ne), photer(ne)

C Runs the APED interpolation

c            parameters (param):
c            (1):      kT temperature in keV
c            (2):      Metal abundances (H and He fixed at cosmic)
c            (3):      redshift z
c            Norm = (4 * pi * 1e14)^-1 * Int(n^2)dV / D^2 where n is
c            in cm^-3 and D is the distance in cm.


      INTEGER i
      REAL rparm(16)

c set the parameters required xsvape - takes the individual abundances.

      rparm(1) = param(1)
      rparm(2) = 1.0
      DO i = 3, 14
         rparm(i) = param(2)
      ENDDO
      rparm(15) = param(3)

      CALL xsvape(ear, ne, rparm, ifl, photar, photer)

      RETURN
      END
