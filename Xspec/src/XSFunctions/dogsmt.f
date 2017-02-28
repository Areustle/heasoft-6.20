      SUBROUTINE dogsmt (ear, ne, param, ifl, photar, tmp)

      INTEGER ne, ifl
      REAL ear(0:ne), param(2), photar(ne), tmp(ne)

c Subroutine to smooth the model spectrum by a variable-width gaussian.
c The width is assumed to vary with param(2) power of the energy. param(1)
c is the width (sigma) at 6 keV. 

c Arguments :
c    ear      r        i: Energy ranges
c    ne       i        i: Number of elements in photar array
c    param    r        i: Model parameters
c    ifl      i        i: Data set
c    photar   r      i/r: Model flux
c    tmp      r        i: Temporary work array
c
c   Renamed from xsgsmt to dogsmt, passing tmp array from C++ wrapper
c   rather than doing dynamic allocation from here - Mar 09


      REAL energy, fact, sigma, sigmafactor
      REAL erflo, erfhi

      INTEGER ie, je

      REAL erfintpl
      EXTERNAL erfintpl

c suppress a warning message from the compiler
      ie = ifl

c Trap out a silly value of the sigma.

      IF ( param(1) .LE. 0. ) RETURN

      DO ie = 1, ne
         tmp(ie) = 0.
      ENDDO

c Loop over energy ranges for the current dataset
      
      DO ie = 1, ne
         
         energy = 0.5*(ear(ie-1)+ear(ie))
         sigma = param(1)*(energy/6.)**param(2)
         sigmafactor = 1./(sigma*sqrt(2.0))

c Sum in the contributions to energy bins below the current energy

         fact = 1.
         je = ie
         erfhi = erfintpl( (energy-ear(je))*sigmafactor )
         DO WHILE ( fact .GT. 1.e-5 .AND. je. GE. 1 )

            erflo = erfintpl( (energy-ear(je-1))*sigmafactor )
            fact = 0.5*(erflo-erfhi)

            tmp(je) = tmp(je) + fact*photar(ie)
            erfhi = erflo
            je = je - 1

         ENDDO

c Then sum in the contributions to energy bins above the current energy

         je = ie + 1
         fact = 1.
         erflo = erfintpl( (ear(je-1)-energy)*sigmafactor )
         DO WHILE ( fact .GT. 1.e-5 .AND. je .LE. ne )

            erfhi = erfintpl( (ear(je)-energy)*sigmafactor )
            fact = 0.5*(erfhi-erflo)

            tmp(je) = tmp(je) + fact*photar(ie)
            erflo = erfhi
            je = je + 1

         ENDDO

      ENDDO

      DO ie = 1, ne
         photar(ie) = tmp(ie)
      ENDDO

      RETURN
      END

c     Interpolated error function

      REAL FUNCTION erfintpl(x)
      IMPLICIT NONE

      REAL x

c     Function to calculate interpolated error function
c     This is much faster than the default routine
c     Precalculates data on first call
c     Returns 1 if x is greater than maximum number of sigma

      INTEGER NUMVALS
      PARAMETER (NUMVALS=200)
      REAL MAXERF
      PARAMETER (MAXERF=3.2)
      REAL DELTA
      PARAMETER (DELTA=MAXERF/NUMVALS)

      REAL precalc(0:NUMVALS)

      INTEGER i, index
      REAL findex, frac, retn

      LOGICAL qfirst

      REAL erf
      EXTERNAL erf

      SAVE precalc

      DATA qfirst /.TRUE./

c Precalculate error function values on first call

      IF ( qfirst ) THEN
         qfirst = .FALSE.
         DO i=0, numvals
            precalc(i) = erf(delta*i)
         ENDDO
      ENDIF

c Round down to nearest index in array (use ABS so we can do erf(<0))

      findex = ABS(x) * (1./delta)
      
c It's possible findex exceeds the machine's largest allowable integer 
c value.  So test it against the array size BEFORE converting it to 
c an int.
      IF ( findex .GE. NUMVALS) THEN
         index = NUMVALS
      ELSE
         index = INT(findex)
      ENDIF
            
      IF ( index .LT. numvals ) THEN
         frac = findex - index
         retn = (1.-frac)*precalc(index) + frac*precalc(index+1)
      ELSE
         retn = 1.
      ENDIF

c Use -erf(x) = -erf(-x)
      IF ( x .GE. 0. ) THEN
         erfintpl = retn
      ELSE
         erfintpl = -retn
      ENDIF

      RETURN
      END

