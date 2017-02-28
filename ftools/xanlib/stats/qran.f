C
      SUBROUTINE qran(r1,ir1)
C
C subroutine to generate quasi random numbers (r1) in the range 0,1
C
C ir1 = initial seed
C
C
      INTEGER*4 ir1, i1, j1, r1
      real*4 centr1, range1
C      data i1,j1/1499,617/
      DATA i1, j1/199999, 69857/
C      data centr1/749.5/,range1/1500./
      DATA centr1/99999.5/, range1/200000./
      ir1 = ir1 + j1
      IF ( ir1.GT.i1 ) ir1 = ir1 - i1
      r1 = (centr1-ir1)/range1
      r1 = r1 + 0.5
      RETURN
      END
