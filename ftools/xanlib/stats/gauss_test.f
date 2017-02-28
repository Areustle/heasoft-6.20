C
C
      SUBROUTINE gauss_test(data,sigma,npts,chisq,status)
* description:
*  does a simple likelihood test to see if the data are gaussianly
*  distributed. if chisq is set to be negative on entry the mean
*  is calculated from the data, otherwise chisq is the mean.
* author :
*  andy pollock (estec:am.exo)
* history :
*  22 may 1987 : original
 
* import :
      INTEGER*4 npts                   ! no of points
      REAL*4 data(npts)
      REAL*4 sigma(npts)               ! data errors
* import/export :
      REAL chisq                       ! chisq statistic
* status :
      INTEGER status
* local variables :
*      REAL mean                        ! ..of the data
      REAL x, w                        ! datum and its weight
      REAL sw, swx, swxx               ! moments of the data
      INTEGER i
*-
      status = 0
 
      sw = 0.
      swx = 0.
      swxx = 0.
 
      DO i = 1, npts
        x = data(i)
        IF ( sigma(i).GT.0. ) THEN
          w = 1./(sigma(i)*sigma(i))
        ELSE
          w = 0.
        END IF
        sw = sw + w
        swx = swx + w*x
        swxx = swxx + w*x*x
      END DO
 
      IF ( sw.LE.0. ) THEN
        status = 1
        chisq = 0.
      ELSE IF ( chisq.LE.0. ) THEN
        chisq = (swxx-swx*swx/sw)
      ELSE
        chisq = (swxx-2.*chisq*swx+chisq*chisq*sw)
      END IF
      END
