      SUBROUTINE xsbvvp(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(33), photar(ne), photer(ne)


C Runs the interpolation of the APED model with all 30 elements allowed to be
C free parameters and with velocity and thermal broadening

c            parameters (param):
c            (1):      kT temperature
c            (2..31):  abundances
c            (32):     redshift z
c            (33):     gaussian velocity width

      INTEGER status

      status = 0

      CALL sumdem(5, 2, ear, ne, param(2), 1., param(32), 1, param(1),
     &            1., ifl, .TRUE., param(33), photar, photer, status)

      RETURN
      END

