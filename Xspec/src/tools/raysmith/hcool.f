**==hcool.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION HCOOL(T,Rhy,Alter)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ALFly , ALFlyc , Alter , beta , EXIt , HALf , HALfc , HBEt , 
     &     HBEtc , HCOOL , Rhy , st , T , TWOp
C*** End of declarations inserted by SPAG
      COMMON /HLN   / EXIt(11) , HALf , HALfc , ALFly , ALFlyc , HBEt , 
     &                HBEtc , TWOp
      beta = 157890./T
      st = SQRT(T)
      CALL HLINE(Rhy,0,T,st,beta,Alter)
      HCOOL = EXIt(1) + EXIt(2) + EXIt(3) + EXIt(4) + EXIt(8) + EXIt(9)
     &        + EXIt(10) + EXIt(11)
      RETURN
      END
