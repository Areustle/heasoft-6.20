**==hyseq.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE HYSEQ(N,J,T,Dene,Ix)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL abu , ABUnd , ABUnj , ALF , BINmin , BINsyz , C , cc , cn , 
     &     CONce , crit , Dene , DNE , E , E3 , en , ES , F , gbar , 
     &     GNDrec
      REAL HENeut , HEPlus , om , p2ph , PCOol , PM , POT , POU , 
     &     POWer , rc , RE , RHY , S , SIG , T , tp , TU , wav2 , WAVe , 
     &     y
      INTEGER Ix , J , LL , N , NBIn , NJ
C*** End of declarations inserted by SPAG
C  HYDROGENIC IONS : ADDS RECOMBINATION, DENSITY DEPENDENCE
C  HAYES AND SEATON 2S EXCITATION,  HYDROGENIC RECOMBINATION
 
      COMMON /DAT   / E(30) , S(30) , C(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , SIG(30) , ALF(30) , ES(30)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      IF ( CONce(J).GE..00001 ) THEN
         abu = 10.**(ABUnd-12.)
         en = N
         cn = CONce(J)*abu
         y = E3(Ix+1)*11590./T
         cc = ALOG((y+1)/y) - 0.4/(y+1)**2
         gbar = 0.047
c     GBAR = 0.20 * Y * CC + 0.276 * CC
         IF ( N.EQ.1 ) gbar = .017 + .1*cc
C  1.2 FOR CASCADES : TU IS DIRECT EXCITATION TWO PHOTON
         om = 1.2*14.5*.416*gbar*13.6/E3(Ix+1)
         p2ph = (8.63E-6*om/SQRT(T))*EXP(-y)*E3(Ix+1)*1.6E11*cn
         TU = TU + p2ph
 
C  RADIATIVE RECOMBINATION : CASE A : exponent revised Aug. '86
         tp = .0001*T/N**2
         cn = CONce(J+1)*abu
         rc = N*16.7E-14*tp**(-0.91)
         POWer(Ix+1) = POWer(Ix+1) + rc*cn*E3(Ix+1)*1.6E11
         rc = N*3.61E-14*tp**(-0.72)
         POWer(Ix+2) = POWer(Ix+2) + rc*cn*E3(Ix+2)*1.6E11
         rc = N*1.40E-14*tp**(-0.757)
         POWer(Ix+3) = POWer(Ix+3) + rc*cn*E3(Ix+3)*1.6E11
         rc = N*.693E-14*tp**(-.750)
         POWer(Ix+4) = POWer(Ix+4) + rc*cn*E3(Ix+4)*1.6E11
         rc = N*7.84E-14*tp**(-1.04)
         IF ( N.EQ.2 ) rc = 2*11.8E-14*tp**(-1.02)
C  CASE B FOR HELIUM PLUS
         POWer(Ix+5) = POWer(Ix+5) + rc*cn*1.6E11*12399./WAVe(Ix+5)
         rc = N*3.18E-14*tp**(-.610)
         p2ph = p2ph + rc*cn*1.6E11*E3(Ix+1)
 
C  DENSITY DEPENDENCE OF METASTABLE LEVEL MEWE AND GRONENSCHILD
         crit = 100.*en**8*SQRT(T)
         POWer(Ix+1) = POWer(Ix+1) + p2ph*Dene/(Dene+crit)
         p2ph = p2ph*crit/(Dene+crit)
         wav2 = WAVe(Ix+1)
         IF ( NBIn.NE.0 ) CALL TWOPH(wav2,p2ph,0)
         POWer(Ix+5) = POWer(Ix+5) + POWer(Ix+2)*.11*WAVe(Ix+2)
     &                 /WAVe(Ix+5)
         POWer(Ix+2) = .89*POWer(Ix+2)
         POWer(Ix+3) = .86*POWer(Ix+3)
         POWer(Ix+4) = .78*POWer(Ix+4)
      ENDIF
      RETURN
      END
