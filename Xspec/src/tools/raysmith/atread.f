**==atread.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE ATREAD(Ilun,Num)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , BINmin , BINsyz , C1J , C2J , E , E3 , E3J , 
     &     EA , EAJ , EJ , F , FJ , PWR , S2 , S2J , S3 , S3J , S4
      REAL S4J , S5 , S5J , WAVe , WJ
      INTEGER j , l , LL , LLJ , n , NBIn , NJ , no , Num, Ilun
C*** End of declarations inserted by SPAG
 
C  CALLS ATRD NUM TIMES TO READ IN ATOMIC DATA
 
      COMMON /FEL   / WJ(12,220) , FJ(12,220) , E3J(12,220) , 
     &                PWR(12,220) , C1J(12,30) , C2J(12,30) , EJ(12,30)
     &                , EAJ(12,30) , S2J(12,30) , LLJ(12,30) , 
     &                S3J(12,30) , S4J(12,30) , S5J(12,30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
C
      DO 200 no = 1 , Num
         CALL ATRD(Ilun,no)
         n = NJ(no)
         DO 50 j = 1 , n
            EJ(no,j) = E(j)
            EAJ(no,j) = EA(j)
            S2J(no,j) = S2(j)
            LLJ(no,j) = LL(j)
            S3J(no,j) = S3(j)
            S4J(no,j) = S4(j)
            S5J(no,j) = S5(j)
 50      CONTINUE
         DO 100 l = 1 , 220
            E3J(no,l) = E3(l)
            FJ(no,l) = F(l)
            WJ(no,l) = WAVe(l)
 100     CONTINUE
 200  CONTINUE
      RETURN
      END
 
