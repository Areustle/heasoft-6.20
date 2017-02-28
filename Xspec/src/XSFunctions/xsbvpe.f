      SUBROUTINE xsbvpe(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(16), photar(ne), photer(ne)


C Runs the interpolation of the APED model with velocity and thermal 
C broadening.

c            parameters (param):
c            (1):      kT temperature
c            (2..14):  abundances
c            (15):     redshift z
c            (16):     gaussian velocity width

      INTEGER status

      CALL sumdem(4, 2, ear, ne, param(2), 1., param(15), 1, param(1),
     &            1., ifl, .TRUE., param(16), photar, photer, status)

      RETURN
      END

