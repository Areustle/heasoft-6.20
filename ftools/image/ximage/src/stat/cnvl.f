      SUBROUTINE CNVL(S,D,N,R,M)
      IMPLICIT NONE
c
c     +,1-D CONVOLUTION
c
c      implicit integer*2 (i-n)
c
      INTEGER*4 i , N , M , m1 , m2
      REAL*4 S(*) , D(*) , R(*)
c
      DO 100 i = 1 , N
         m1 = MAX0(i-M,1)
         m2 = MIN0(i+M,N)
c
         CALL VDOT(D(i),S,m1,1,R,i+M+1-m1,-1,m2-m1+1)
c
 100  CONTINUE
      RETURN
      END
