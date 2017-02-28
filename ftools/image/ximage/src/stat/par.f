 
C
C
      FUNCTION PAR(X,Ag,Bg,Cg)
      REAL*4 Ag , Bg , Cg , X , PAR
      PAR = Ag + Bg*X + Cg*X*X
      RETURN
      END
