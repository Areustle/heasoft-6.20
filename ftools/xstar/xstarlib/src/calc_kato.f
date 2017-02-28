c_______________________________________________________________
      real*8 function calc_kato(coll_type,par,z,te)
      
c /* This fit comes from Kato & Nakazaki, 1989, Atomic Data and Nuclear 
c           Data Tables 42, 2, 313 */
        
      real*8 result
      real*8 dE, kT
      real*8 A, B, C, D, E, P, Q, X1
      real*8 y,exint_n,pow
      real*8 E1y, E1Xy
      real*8 ups_nr, ups_r
      real*8 term1, term2, term3
      real*8 par(10),te
      integer z,coll_type
     
      dE = par(1)
c       /* in keV */
      A  = par(2)
      B  = par(3)
      C  = par(4)
      D  = par(5)
      E  = par(6)
      P  = par(7)
      Q  = par(8)
      X1 = par(9)
c        
      kT = KBOLTZ*Te
      y = dE/kT
      
      go to (1,2,3)coll_type
 1      continue
c        case (1):  /* Simple case (eq 6, in above reference) */
          E1y = exint_n(y,-1.d0,1)
          term1 = A/y + C + (D/2)*(1-y)
          term2 = B - C*y + (D/2)*y*y + E/y
          result = y*(term1 + exp(y)*E1y*term2)
          go to 9000

 2        continue
c        case (2):  /* More complex case (eq 10-12 in above reference) */
          E1Xy = exint_n(X1*y,-1.d0,1)
          term3 = exp(y*(1-X1))
          
          term1 = A/y + C/X1 + (D/2)*(1/(X1*X1) - y/X1) + (E/y)*log(X1)
          term2 = exp(y*X1)*E1Xy*(B - C*y + D*y*y/2 + E/y)
      
          ups_nr = y*term3*( term1 + term2 )
          ups_r = P*((1 + 1/y) - term3*(X1 + 1/y)) + Q*(1-term3)
      
          result = ups_nr + ups_r
          go to 9000
 3        continue
 9000     continue
          
      calc_kato=result      
c
      return
      end
