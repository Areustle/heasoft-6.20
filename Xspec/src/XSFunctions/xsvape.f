      SUBROUTINE xsvape(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(15), photar(ne), photer(ne)


C Runs the interpolation of the APED model.

c            parameters (param):
c            (1):      kT temperature
c            (2..14):  abundances
c            (15):     redshift z

      INTEGER status

      status = 0

      CALL sumdem(4, 2, ear, ne, param(2), 1., param(15), 1, param(1),
     &            1., ifl, .FALSE., 0.0, photar, photer, status)

      RETURN
      END

