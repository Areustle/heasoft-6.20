**==grec.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION GREC(N,J,E,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL deg , drat , E , E3 , EA , EXINT1 , F , g , GREC , S2 , S3 , 
     &     S4 , S5 , T , V , WAVe , x1 , x12 , x14
      INTEGER J , LL , N
C*** End of declarations inserted by SPAG
      COMMON /DAT   / V(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      DIMENSION drat(30)
      DATA drat/2. , .5 , 2. , .5 , 6. , 2.5 , 2.22 , 2.25 , .67 , 
     &     .167 , 2. , .5 , 6. , 2.5 , 2.22 , 2.25 , .67 , .167 , 12*0./
      deg = drat(N-J+1)
      x1 = E*11590./T
      x12 = x1*x1
      x14 = x12*x12
      g = 5.238E-23*T**1.5*deg*(S2(J)*x12+S4(J)*x12*x1+x12*x1*S5(J)
     &    *(0.5-x1/2.)+EXINT1(x1,2)
     &    *(S3(J)*x12*x1-x14*S4(J)+x1*x14*S5(J)/2.))
      GREC = g
      IF ( x1.GE.300. ) GREC = 5.238E-23*T**1.5*deg*(S2(J)+S3(J)+S4(J)
     &                         +S5(J))*x12
      RETURN
      END
 
