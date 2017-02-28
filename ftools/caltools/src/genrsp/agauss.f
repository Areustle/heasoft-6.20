**==agauss.spg  processed by SPAG 4.50J  at 17:33 on 20 Jan 1996
c ****************************************************************
c  subroutine to calculate area under Gaussian between -X and X.
c
 
      SUBROUTINE AGAUSS(X,Erf)
      IMPLICIT NONE
c
      REAL X , Erf
      REAL z , term , sum , y2 , denom
 
C  SHOULD USE DBL PREC FOR Y2, Z, TERM, SUM & DENOM
 
      z = X
      Erf = 0.
 
C  DONT BOTHER TO EVALUATE GAUSSIAN DEVIATIONS GT. 7 SIGMA
 
      IF ( z.GT.7 ) THEN
         Erf = 1
         RETURN
      ENDIF
 
      IF ( z.GT.0. ) THEN
 
         term = 0.5*z*SQRT(2.)
         sum = term
         y2 = (z**2)/2.
         denom = 1.
 
C   ACCUMULATE SUM OF TERMS
 
         DO WHILE ( term/sum.GT.1.E-6 )
            denom = denom + 2.
            term = term*(y2*2./denom)
            sum = sum + term
         ENDDO
 
         Erf = 1.12838*sum*EXP(-y2)
 
      ENDIF
c
      RETURN
      END
 
