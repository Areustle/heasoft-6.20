      SUBROUTINE xsvvap(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(32), photar(ne), photer(ne)


C Runs the interpolation of the APED model with all 30 elements allowed to be
C free parameters

c            parameters (param):
c            (1):      kT temperature
c            (2..31):  abundances
c            (32):     redshift z

      INTEGER status

      status = 0

      CALL sumdem(5, 2, ear, ne, param(2), 1., param(32), 1, param(1),
     &            1., ifl, .FALSE., 0.0, photar, photer, status)

      RETURN
      END

