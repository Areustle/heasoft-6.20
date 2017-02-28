      SUBROUTINE dolsmt (ear, ne, param, ifl, photar, tmp)

      INTEGER ne, ifl
      REAL ear(0:ne), param(2), photar(ne), tmp(ne)

c Subroutine to smooth the model spectrum by a variable-width Lorentzian.
c The width is assumed to vary with param(2) power of the energy. param(1)
c is the width (sigma) at 6 keV. The contribution from a Lorentzian with
c central energy E0 and width W to the energy range (E1,E2) is
c
c        L(E1,E2) = (1/pi) ( arctan(2(E2-E0)/W) - arctan(2(E1-E0)/W) )

c Arguments :
c    ear      r        i: Energy ranges
c    ne       i        i: Number of elements in photar array
c    param    r        i: Model parameters
c    ifl      i        i: Data set
c    photar   r      i/r: Model flux
c    tmp      r        i: Temporary work array
c
c   Renamed from xslsmt to dolsmt, passing tmp array from C++ wrapper
c   rather than doing dynamic allocation from here - Mar 09


      REAL INVPI
      PARAMETER (INVPI=0.3183099)

      REAL energy, fact, sigma, sum

      INTEGER ie, je

c suppress a warning message from the compiler
      ie = ifl

c Trap out a silly value of the sigma.

      IF ( param(1) .LE. 0. ) RETURN

      DO ie = 1, ne
         tmp(ie) = 0.
      ENDDO

c Loop over energy ranges for the current dataset
      
      DO ie = 1, ne
         
         sum = 0.
         energy = 0.5*(ear(ie-1)+ear(ie))
         sigma = param(1)*(energy/6.)**param(2)

c Sum in the contributions to energy bins below the current energy

         fact = 1.
         je = ie
         DO WHILE ( fact .GT. 1.e-5 .AND. je. GE. 1 )

            fact = INVPI * ( atan(2*(ear(je)-energy)/sigma)
     &                      -atan(2*(ear(je-1)-energy)/sigma) )

            tmp(je) = tmp(je) + fact*photar(ie)
            sum = sum + fact
            je = je - 1

         ENDDO

c Then sum in the contributions to energy bins above the current energy

         je = ie + 1
         fact = 1.
         DO WHILE ( fact .GT. 1.e-5 .AND. je .LE. ne )

            fact = INVPI * ( atan(2*(ear(je)-energy)/sigma)
     &                      -atan(2*(ear(je-1)-energy)/sigma) )

            tmp(je) = tmp(je) + fact*photar(ie)
            sum = sum + fact
            je = je + 1

         ENDDO

      ENDDO

      DO ie = 1, ne
         photar(ie) = tmp(ie)
      ENDDO

      RETURN
      END
