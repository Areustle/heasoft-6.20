**==noson.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION NOSON(A,X,L,Lmax)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL A , f , X
      INTEGER i , j1 , j2 , k , k1 , k2 , ki , L , Lmax , max , n , 
     &        NOSON
C*** End of declarations inserted by SPAG
C     RUDOLF LOESER, 28 JULY 1965
C     NOSON IS A MATRIX INVERSION ROUTINE, USING THE METHOD OUTLINED ON
C     P 434 OF HILDEBRAND, INTRODUCTION TO NUMERICAL ANALYSIS,
C     (NEW YORK,1956).
C     A IS A MATRIX OF ORDER L, WITH COLUMN DIMENSION LMAX, ITS ELEMENTS
C     ARE ASSUMED TO BE STORED COLUMNWISE, IN THE USUAL FORTRAN MANNER,
C     X IS WORKING STORAGE OF LENGTH L.
C     THE INVERSE OF A WILL REPLACE A.
C     UPON RETURN, NOSON=1 IF INVERSION WENT OK, =0 IF A DIVISOR IS
C     ZERO (IN WHICH CASE A MAY CONTAIN GARBAGE UPON RETURN).
      DIMENSION A(1) , X(1)
      n = L - 1
      max = n*Lmax + L
      DO 100 i = 1 , L
         X(i) = 1.
 100  CONTINUE
      k1 = -Lmax
      DO 300 k = 1 , L
         k1 = k1 + Lmax
         k2 = k1 + k
         IF ( A(k2).EQ.0 ) GOTO 500
         DO 150 i = 1 , L
            j1 = k1 + i
            IF ( A(j1).NE.0 ) THEN
               f = 1./A(j1)
               X(i) = X(i)*f
               DO 110 j1 = i , max , Lmax
                  A(j1) = A(j1)*f
 110           CONTINUE
            ENDIF
 150     CONTINUE
         A(k2) = X(k)
         X(k) = 1.
         DO 200 i = 1 , L
            ki = k - i
            IF ( ki.NE.0 ) THEN
               j1 = k1 + i
               IF ( A(j1).NE.0 ) THEN
                  A(j1) = 0.
                  DO 155 j2 = i , max , Lmax
                     j1 = j2 + ki
                     A(j2) = A(j2) - A(j1)
 155              CONTINUE
               ENDIF
            ENDIF
 200     CONTINUE
 300  CONTINUE
      DO 400 i = 1 , n
         IF ( X(i).EQ.0 ) GOTO 500
         f = 1./X(i)
         DO 350 j1 = i , max , Lmax
            A(j1) = A(j1)*f
 350     CONTINUE
 400  CONTINUE
      NOSON = 1
      RETURN
 500  NOSON = 0
      RETURN
      END
