      subroutine xsstep(ear,ne,param,ifl, photar, photer)

      integer ne, ifl
      real ear(0:ne),param(2),photar(ne), photer(ne)

C---
C XSPEC model subroutine
C Step function convolved with a gaussian profile
C---
C see ADDMOD for parameter descriptions
C number of model parameters:2
C      1      EL      start energy (in energy units, e.g. keV)
C      2      W      gaussian wiflh (sigma) (in energy units)
C intrinsic energy range: none
C algorithm:
C      n(E) = (1/2)*(1-erf((E-EL)/sqrt(2)*W))
C---
C 2 May 1990 - kaa
C 1.0:  simple implementation to test partial charge collection
C      model in BBXRT
C---

      real el, w, winv, erf
      integer ie

c suppress a warning message from the compiler
      ie = ifl

c this model does not calculate errors
      DO ie = 1, ne
         photer(ie) = 0.0
      ENDDO


      el=param(1)
      w=max(0.,param(2))
      winv=1/w/sqrt(2.)

      do ie=1, ne
         photar(ie) = 0.5*(1-erf(((ear(ie-1)+ear(ie))/2-el)*winv))
     &                     *(ear(ie)-ear(ie-1))
      end do

      return
      end
