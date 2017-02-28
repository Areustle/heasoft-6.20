      SUBROUTINE xsfdcu(ear, ne, param, worksp, photar)
      INTEGER*4 ne
      REAL*4 ear(0:ne), param(2), worksp(1), photar(ne)
C---
C XSPEC model subroutine:
C Fermi-Dirac high energy cutoff
C---
C see MULMOD for parameter descriptions
C number of model parameters: 2
C	1	Ec: The cutoff energy
C	2	Ef: The folding energy
C model form A(E) = 1/( 1 + exp((E-E0)/E1))
C---
C 24 sep 1996 - peter kretschmar
C---
      REAL*4 ec, ef, a, b
      REAL*8 dep
      INTEGER*4 i
C---
      ec = param(1)
      ef = param(2)
      dep = dble((ear(0)-ec)/ef)
      IF (dep .ge. 85.0d0) THEN                    ! precaution against
        a = 0.0                                    ! floating overflow
      ELSE
        a = real( 1.0d0 / (1.0d0 + exp(dep)) )
      ENDIF
      DO i = 1, ne
        dep = dble((ear(i)-ec)/ef)
        IF (dep .ge. 85.0d0) THEN
          b = 0.0
        ELSE
          b = real(1.0d0 / ( 1.0d0 + exp(dep)) )
         ENDIF
         photar(i) = 0.5*(a+b)
         a = b
      ENDDO
      RETURN
      END

