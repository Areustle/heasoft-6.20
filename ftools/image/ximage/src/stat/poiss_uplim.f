      real*4 function poiss_uplim(S,n)
c 
c Poisson Upper Limit Aproximate expression (small numbers)
c See Equation (9) Astrophysical Journal, 303:336-346, 1986 April 1
c
c import :
c     S = Confidence level
c     n = number of events
c
      REAL*4 S
      REAL*4 n
c
c output :
c pois_uplim = Eq (9) result 
c
c local variables :
c
      REAL*4 tmpn,den1,den2
c
      den1 = 0.
      den2 = 0.
      tmpn = n+1.

      IF (tmpn.GT.-1) then
         den1 = 9.*(tmpn)
         den2 = 3.*SQRT(tmpn)
         poiss_uplim = (tmpn)*((1.-1./den1+S/den2)**3)  
      ELSE
         poiss_uplim = 0.0
      ENDIF

      return
      END
