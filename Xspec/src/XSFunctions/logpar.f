
      SUBROUTINE logpar(ear, ne, param, ifl, photar)

      INTEGER ne, ifl
      REAL ear(0:ne), param(3), photar(ne)
      real      alpha,beta,e0,a,b,c,ea,h
      integer      i

      parameter (h=0.5/3.)  ! constant for Simpson's rule

C---
C linear power law spectral model for XSPEC programs
C---
C see ADDMOD for parameter descriptions
C number of parameters: 2
C      1      PHOTON power law index: 'index', no range limit
C               nominal values 1.5, 0.0, 5.0, 0.001
C      2      PHOTON power law index derivative : 'beta', no range limit
C      3      Pivot energy - must be fixed and is best set to close to
C               lower energy of detector range.
C no intrinsic energy range limit
C no special meaning to coefficient
C model form N(E) = (E/E0)**(-alpha-beta*log(E/E0))
C---
C 1 nov 1997 - Segreto
C---

c suppress a warning message from the compiler
      i = ifl

      alpha = param(1)
      beta = param(2)
      e0 = param(3)

      a = (ear(0)/e0)**(-alpha-beta*Alog10(ear(0)/e0))
      DO i = 1, ne
         ea=0.5*(ear(i-1)+ear(i))/e0
         b = ea**(-alpha-beta*Alog10(ea))
         c = (ear(i)/e0)**(-alpha-beta*Alog10(ear(i)/e0))
         photar(i)=h*(ear(i)-ear(i-1))*(a+c+4*b) ! integration with Simpson's rule
         a=c 
      ENDDO

      RETURN
      END
