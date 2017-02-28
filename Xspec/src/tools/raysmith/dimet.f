**==dimet.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION DIMET(N,J,T,B,Dd)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , B , Dd , DIMET , e , ebar , f , T , x , z
      INTEGER ij , J , N , no , noj
C*** End of declarations inserted by SPAG
      DIMENSION noj(30) , e(12,3) , f(12,3)
      DATA noj/0 , 1 , 3*0 , 2 , 3 , 4 , 0 , 5 , 0 , 6 , 0 , 7 , 0 , 8 , 
     &     0 , 9 , 0 , 10 , 5*0 , 11 , 0 , 12 , 2*0/
      DATA e/0 , 10.55 , 13.4 , 16.3 , 22.1 , 27.9 , 40.0 , 40.5 , 
     &     46.7 , 52.8 , 76.3 , 86.2 , 0. , 12.28 , 16.06 , 19.8 , 
     &     27.4 , 35.0 , 42.7 , 51.0 , 60.9 , 73.5 , 122. , 145. , 
     &     5*0. , 4.46 , 9.55 , 14.5 , 19.5 , 24.5 , 40.7 , 51./
      DATA f/0. , .26 , .21 , .18 , .16 , .12 , .10 , .091 , .081 , 
     &     .075 , .003 , .002 , 0. , .16 , .17 , .14 , .084 , .076 , 
     &     .071 , .067 , .063 , .059 , .049 , .046 , 5*0. , .61 , .564 , 
     &     0.458 , .408 , .366 , .30 , .25/
      ij = N - J - 2
      IF ( ij.EQ.9 ) ij = 3
      z = J - 1
      no = noj(N)
      x = e(no,ij)/(J*13.6)
      a = SQRT(x)/(1.+.105*x+.015*x*x)
      ebar = e(no,ij)/(1.+.015*z*z*z/(J*J))
      DIMET = .003*T**(-1.5)*a*B*f(no,ij)*EXP(-11590.*ebar/T)*Dd
      RETURN
      END
