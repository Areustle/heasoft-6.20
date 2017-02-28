**==atrd.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE ATRD(Ilun,No)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , BINmin , BINsyz , E , E3 , EA , F , S2 , S3 , 
     &     S4 , S5 , WAVe
      INTEGER ix , iy , j , k , l , LL , n , NBIn , NJ , No, Ilun
C*** End of declarations inserted by SPAG
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      INTEGER aa
C     READ ATOMIC DATA
      ix = 0
 100  READ (Ilun,99001) n , j , E(j) , EA(j) , S2(j) , S3(j) , S4(j) , 
     &               S5(j) , LL(j)
      iy = LL(j)
      IF ( iy.GT.0 ) THEN
         DO 150 l = 1 , iy
            aa = ix + 3*l - 3
            READ (Ilun,99002) (WAVe(aa+k),E3(aa+k),F(aa+k),k=1,3)
 150     CONTINUE
         ix = ix + 3*iy
      ENDIF
      IF ( n.GT.j ) GOTO 100
      NJ(No) = n
      RETURN
99001 FORMAT (2I5,F5.0,5F7.0,I5)
99002 FORMAT (9F6.0)
      END
 
