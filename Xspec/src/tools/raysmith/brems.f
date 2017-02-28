**==brems.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE BREMS(T,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , BINmin , BINsyz , BRMev , chrgsm , CONce , 
     &     DNE , ebk , en1 , en2 , exprod , FBG , GNDrec , hc , HENeut , 
     &     HEPlus , PCOol , PM , POT
      REAL POU , POWer , q , RE , RECev , RHY , T , t6 , TU , TUF , x , 
     &     y , z
      INTEGER i , i2 , j , j1 , j2 , N , NBIn , NJ
C*** End of declarations inserted by SPAG
      REAL kt
 
      INCLUDE 'rayspec.inc'
 
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /CONTIN/ BRMev(MXBINS) , RECev(MXBINS) , TUF(MXBINS)
      DIMENSION x(30)
      hc = 12399.
      j2 = N + 1
      j1 = 2
      t6 = T/1.E6
      kt = 86.17*t6
      ebk = EXP(BINsyz/kt)
      q = 10.**(ABUnd-12.)*86.17*SQRT(t6)*20.4*(ebk-1.)*EXP(-BINmin/kt)
     &    /hc
      ebk = 1./ebk
      DO 100 j = j1 , j2
         x(j) = 0.
         IF ( CONce(j).GT.1.E-5 ) x(j) = CONce(j)*q*(j-1)**2
 100  CONTINUE
      i2 = MIN0(NBIn,INT(46.*kt/BINsyz)+1)
      exprod = 1.
      DO 200 i = 1 , i2
         en2 = BINsyz*i + BINmin
         en1 = en2 - BINsyz
         y = (en2+en1)*.5/kt
         chrgsm = 0.
         DO 150 j = j1 , j2
            IF ( x(j).NE.0. ) THEN
               z = (j-1)**2*.158/t6
               chrgsm = chrgsm + x(j)*FBG(y,z)
            ENDIF
 150     CONTINUE
         exprod = exprod*ebk
         BRMev(i) = BRMev(i) + chrgsm*exprod
 200  CONTINUE
      RETURN
      END
 
