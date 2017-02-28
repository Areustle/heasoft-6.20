**==gncrch.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
 
      FUNCTION GNCRCH(R,L,Y,Cc,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , b , c , Cc , d , GNCRCH , R , Y
      INTEGER il , L , N
C*** End of declarations inserted by SPAG
      DIMENSION R(1)
      il = 8*(L-1)
      a = R(il+1) + R(il+2)*N
      b = R(il+3) + R(il+4)*N
      c = R(il+5) + R(il+6)*N
      d = R(il+7) + R(il+8)*N
      GNCRCH = a + b*Y*Cc + c*(Y-Y*Y*Cc) + d*Cc
      RETURN
      END
 
