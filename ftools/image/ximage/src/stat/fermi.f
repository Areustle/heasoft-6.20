C
C
      FUNCTION FERMI(R,C1,C2,C3,C4)
      REAL*4 C1 , C2 , R , C3 , C4
      REAL*4 FERMI
C       approximates integral cma psf value at off-axis distance r
      FERMI = C1 + C2/(EXP((R-C3)/C4)+1.)
      RETURN
      END
