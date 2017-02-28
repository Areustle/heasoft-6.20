
      SUBROUTINE xsmekl(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(5), photar(ne), photer(ne)

c XSPEC subroutine to calculate the new Mewe-Kaastra-Liedahl
c plasma emission spectrum.

c parameters :
c       1.........kT (keV)
c	2.........nH (cm^-3)           fixed at 1 for most applications
c       3.........Heavy metal abundance
c       4.........Redshift
c       5.........Switch (0=calculate, 1=interpolate)

      REAL vparam(18)
      INTEGER i

c Set the parameters for the call to the subroutine with
c variable abundances.

      vparam(1) = param(1)
      vparam(2) = param(2)
      vparam(3) = 1.
      DO i = 4, 16
         vparam(i) = param(3)
      ENDDO
      vparam(17) = param(4)
      vparam(18) = param(5)

c Call the MEKAL subroutine for variable abundances.

      CALL xsvmkl(ear, ne, vparam, ifl, photar, photer)

      RETURN
      END

