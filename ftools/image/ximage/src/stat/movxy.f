C
C
      SUBROUTINE MOVXY(Xin,Yin)
      IMPLICIT NONE
      REAL*4 Xin(*) , Yin(*)
      INTEGER*4 i
      DO 100 i = 1 , 3
         Xin(i) = Xin(i+1)
         Yin(i) = Yin(i+1)
 100  CONTINUE
      RETURN
      END
