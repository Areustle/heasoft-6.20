      SUBROUTINE xsvmkl(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(18), photar(ne), photer(ne)

c XSPEC subroutine to calculate the new Mewe-Kaastra-Liedahl
c plasma emission spectrum.

c parameters :
c       1.........kT (keV)
c	2.........nH (cm^-3)           fixed at 1 for most applications
c       3.........He abundance
c       4.........C     "
c       5.........N     "
c       6.........O     "
c       7.........Ne    "
c       8.........Na    "
c       9.........Mg    "
c      10.........Al    "
c      11.........Si    "
c      12.........S     "
c      13.........Ar    "
c      14.........Ca    "
c      15.........Fe    "
c      16.........Ni    "
c      17.........Redshift
c      18.........Switch  (0=calculate, 1=interpolate)

      INTEGER status, switch

      switch = NINT(param(18))

      CALL sumdem(2, switch, ear, ne, param(3), param(2), param(17),
     &            1, param(1), 1., ifl, .FALSE., 0., photar, photer,
     &            status)

      RETURN
      END

