      SUBROUTINE xsmeka(e, ne, param, ifl, flx, flxe)

      INTEGER ne, ifl
      REAL e(0:ne),param(4),flx(ne),flxe(ne)

c XSPEC subroutine to calculate the new Mewe-Gronenschild
c plasma emission spectrum.

c parameters :
c       1.........kT (keV)
c	2.........nH (cm^-3)           fixed at 1 for most applications
c       3.........Heavy metal abundance
c       4.........Redshift

      REAL vparam(17)
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

c Call the MEKA subroutine for variable abundances.

      CALL xsvmek(e, ne, vparam, ifl, flx, flxe)

      RETURN
      END

