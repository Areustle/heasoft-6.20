      SUBROUTINE xsvrys(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(14), photar(ne), photer(ne)


c            fwj haberl      5 april 1987
c            additive model for XSPEC. Raymond-Smith with all
c            abundances free.
c            parameters (param):
c            (1):      kT temperature
c            (2..13): abundances
c            (14):      redshift z

c  kaa  10/11/96  Now just calls the general routine for plasma emission
c                 models.

      INTEGER status

      CALL sumdem(1, 1, ear, ne, param(2), 1., param(14), 1, param(1),
     &            1., ifl, .FALSE., 0., photar, photer, status)

      RETURN
      END

