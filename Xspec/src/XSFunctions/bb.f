      REAL FUNCTION BB(E, T)
      
C **  VERSION 12.11.99, I. HALM
      
      REAL E, T, X, EX
      
      X = E/T
      BB = X*X*X
      EX = EXP(-X)
      BB = BB*EX/(1-EX)
      BB = BB/E
      RETURN
      END
