      SUBROUTINE xsrays(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne),param(*),photar(ne), photer(ne)

c            fwj haberl      6 april 1987
c            raymond smith model for XSPEC using data from model file
c            for this model. The file is in the general format for
c            model files.
c            parameters (param):
c            (1):      kT temperature in keV
c            (2):      Metal abundances (He fixed at cosmic)
c            (3):      redshift z
c            Norm = (4 * pi * 1e14)^-1 * Int(n^2)dV / D^2 where n is
c            in cm^-3 and D is the distance in cm.


      INTEGER i
      REAL rparm(14)

c set the parameters required by rdpfil - takes the individual
c abundances.

      rparm(1) = param(1)
      rparm(2) = 1.
      DO i = 3, 13
         rparm(i) = param(2)
      ENDDO
      rparm(14) = param(3)

      CALL xsvrys(ear, ne, rparm, ifl, photar, photer)

      RETURN
      END
