c
c
      SUBROUTINE PARIN(Xin,Yin,Ifclou,Agood,Bgood,Cgood)
      IMPLICIT NONE
c
c   this subroutine is to interpolate data with weighted parabola
c   method.
c   ifclou: -1 for the first couple, 1 for the last, 0 for all the others
c
      REAL*4 Xin(*) , Yin(*) , Agood , Cgood , Bgood , w , dk0 , djk
      REAL*4 a(0:2,0:2,-1:2) , x(0:2) , aa(2) , bb(2) , cc(2)
      INTEGER*4 jjstrt , jjstop , Ifclou , jj , i , j , k
c
      jjstrt = 0
      jjstop = 1
      IF ( Ifclou.EQ.-1 ) jjstrt = 1
      IF ( Ifclou.EQ.1 ) jjstop = 0
c
      DO 200 jj = jjstrt , jjstop
         DO 50 i = 1 , 3
            x(i-1) = Xin(i+jj)
            a(i-1,0,0) = Yin(i+jj)
 50      CONTINUE
c
         DO 100 i = 1 , 2
            DO 80 j = 1 , i
               DO 60 k = 0 , j
                  dk0 = 1.0
                  djk = 1.0
                  IF ( k.EQ.0 ) dk0 = 0.0
                  IF ( j.EQ.k ) djk = 0.0
                  a(i,j,k) = 1./(x(i)-x(j-1))
     &                       *(dk0*(a(i,j-1,k-1)-a(j-1,j-1,k-1))
     &                       +djk*(a(j-1,j-1,k)*x(i)-a(i,j-1,k)*x(j-1)))
 60            CONTINUE
 80         CONTINUE
 100     CONTINUE
c
         cc(jj+1) = a(2,2,2)
         bb(jj+1) = a(2,2,1)
         aa(jj+1) = a(2,2,0)
 200  CONTINUE
c
      w = ABS(cc(2))/(ABS(cc(2))+ABS(cc(1)))
      IF ( cc(1).EQ.0. .AND. cc(2).EQ.0. ) w = 0.0
      IF ( Ifclou.EQ.-1 ) w = 0.0
      IF ( Ifclou.EQ.1 ) w = 1.0
      Agood = w*aa(1) + (1.-w)*aa(2)
      Bgood = w*bb(1) + (1.-w)*bb(2)
      Cgood = w*cc(1) + (1.-w)*cc(2)
c
      RETURN
      END
