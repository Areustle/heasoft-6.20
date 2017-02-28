**==autoin.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION AUTOIN(N,J,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL AUTOIN , et , om , T
      INTEGER i , J , N
C*** End of declarations inserted by SPAG
C  INNERSHELL EXCITATION FOLLOWED BY AUTOIONIZATION FOR NA,MG,AL,SI
C  SEQUENCES.  CRANDALL ET AL PRA 25,143 FOR MG,SI.  MANN FOR FE* .75.
C  COMPROMISE BETWEEN COWAN AND MANN AND DOUBLE THAT.
C  USE 2S AT 2P ENERGY, ETC FOR B,C,... SEQUENCES
      DIMENSION et(20) , om(20)
      DATA et/0. , 55. , 0. , 115. , 0. , 190. , 0. , 280. , 0. , 375. , 
     &     5*0. , 802. , 0. , 1002. , 0. , 0./
      DATA om/0. , .34 , 0. , .16 , 0. , .18 , 0. , .20 , 0. , .22 , 
     &     5*0. , .29 , 0. , .3 , 0. , 0./
      AUTOIN = 0.
      IF ( N.LE.10 ) RETURN
      i = N - J + 1
      IF ( i.LE.10 .OR. i.GE.15 ) RETURN
      AUTOIN = 8.63E-6*om(N-10)*EXP(-11590.*et(N-10)/T)/SQRT(T)
C  ASSUMES THAT NUMBER OF 32,3P ELECTRONS DOESN'T MATTER
      RETURN
      END
 
