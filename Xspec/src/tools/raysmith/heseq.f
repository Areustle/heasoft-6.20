**==heseq.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE HESEQ(N,J,T,Dene,Ix)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL abu , ABUnd , ABUnj , addit , ADH , ALF , BINmin , BINsyz , 
     &     br , branch , C , cc , cionize , cn , CONce , cr , crit , 
     &     crit1 , crit2 , crit3
      REAL critm , Dene , DNE , E , E3 , effecn , eis , en , ES , F , 
     &     fc , gbar , GNDrec , HENeut , HEPlus , HETwop , om , p2dum , 
     &     p2ph , PCOol
      REAL pdum , PM , POT , POU , POWer , RE , recrat , RHY , S , 
     &     SEATON , SIG , T , TU , wav2 , WAVe , xn , y , z , zf
      INTEGER Ix , J , LL , N , NBIn , NJ
C*** End of declarations inserted by SPAG
 
C  HELIUM-LIKE IONS  :  RECOMBINATION TO EXCITED LEVELS, INNER-SHELL
C  IONIZATION OF LI-LIKE IONS  AND DENSITY DEPENDENCE USING PRADHAN,
C  MEWE & SCHRIJVER,  BERRINGTON, FON & KINGSTON, DRAKE
C
      COMMON /DAT   / E(30) , S(30) , C(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , SIG(30) , ALF(30) , ES(30)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /TWOSV / HETwop
C
      DIMENSION br(30) , cr(3,30)
      DATA br/0. , 1.0 , 3*0. , .89 , .78 , .71 , 0. , .67 , 0. , .63 , 
     &     0. , .58 , 0. , .50 , 0. , .40 , .0 , .33 , 5*0. , .20 , 0. , 
     &     .18 , 0. , 0./
      DATA cr/3*0. , 7.6E6 , 4540. , 2210. , 9*0. , 1.9E10 , 1.3E11 , 
     &     9.6E11 , 4.9E10 , 6.5E11 , 2.3E12 , 1.25E11 , 3.3E12 , 
     &     5.8E12 , 3*0. , 2.4E12 , 8.4E13 , 1.4E14 , 3*0. , 2.3E13 , 
     &     1.1E15 , 1.9E15 , 3*0. , 2.3E14 , 1.5E15 , 2.7E16 , 3*0. , 
     &     9.5E14 , 7.6E16 , 1.3E17 , 3*0. , 3.9E15 , 3.9E17 , 6.2E17 , 
     &     3*0. , 1.6E16 , 2.0E18 , 3.0E18 , 15*0. , 4.5E17 , 5.3E19 , 
     &     8.2E19 , 3*0. , 1.0E18 , 1.0E20 , 2.0E20 , 6*0./
      IF ( CONce(J).GE..00001 ) THEN
         abu = 10.**(ABUnd-12.)
         TU = 0.
         en = N
C  EXCITATION OF 1S2S 1S LEVEL PRADHAN ; BALUJA,CALLAWAY,HENRY: 1.2 FOR CASCADES
         y = 11590.*E3(Ix+1)/T
         cc = ALOG((y+1.)/y) - 0.4/(y+1)**2
         gbar = (.06168+.0002522*N) + (-.02404-.001948*N)
     &          *y*cc + (-.007604+.002416*N)*(y-y*y*cc)
c         GBAR = (.03014+.0009855*N)+(.2392-.007019*N)*Y*CC
c    1  +(-.1292+.003543*N)*(Y-Y*Y*CC)
         IF ( N.EQ.2 ) gbar = .048*(T/10000.)**(.201)
         om = 1.2*14.5*F(Ix+1)*gbar*13.6/E3(Ix+1)
         p2ph = (8.63E-6*om/SQRT(T))*EXP(-y)*abu*CONce(N-1)*E3(Ix+1)
     &          *1.6E11
         TU = p2ph
 
C  INNERSHELL IONIZATION OF LI-LIKE ION FROM MEWE AND SCHRIJVER
         IF ( N.NE.2 ) THEN
            eis = 13.6*en*en*3.**(.24-.43/ALOG10(en))
            cionize = 2.5E-8*SQRT(T)*EXP(-eis*11590./T)/eis**2
            addit = CONce(J-1)*cionize*E3(Ix+2)*1.6E11*abu
            POWer(Ix+2) = POWer(Ix+2) + addit*.75
            p2ph = p2ph + addit*.25
         ENDIF
 
C RADIATIVE RECOMBINATION
         cn = CONce(J+1)*abu
         z = N - 1.
         POWer(Ix+2) = POWer(Ix+2) + (5.9E-13*z**1.8*T**(-0.4))
     &                 *(1.+17*z**0.4*T**(-0.2))*E3(Ix+2)*1.6E11*cn
         POWer(Ix+3) = POWer(Ix+3)
     &                 + (3.6E-11*z**2.4*T**(-0.7)+3.6E-10*z**2.8*T**
     &                 (-.9))*1.6E11*E3(Ix+3)*cn
         POWer(Ix+1) = POWer(Ix+1) + 1.6E11*cn*E3(Ix+1)
     &                 *1.4E-11*z**2.52*T**(-.76)
     &                 *(1.+10.*z**0.4*T**(-0.2))
         p2ph = p2ph + cn*1.6E11*E3(Ix+1)*(5.4E-13*z*z*T**(-0.5))
     &          *(1.+17.*z**(0.4)*T**(-0.2))
 
         HETwop = p2ph
 
         zf = N - 1
         effecn = SQRT(zf*zf*13.6/(E(J)-E3(Ix+7)))
         xn = 157890.*zf*zf/(T*effecn*effecn)
         recrat = 5.2E-14*zf*SQRT(xn)*SEATON(xn,effecn)
         addit = (5./12.)*recrat*cn*(12399./WAVe(Ix+7))*1.6E11
C HYDROGENIC RECOMBINATION FOR 3D LINES EXCEPT HELIUM FROM ROBBINS
         IF ( N.EQ.2 ) addit = cn*5.00E-14*(.0001*T)**(-1.333)
     &                         *1.6E11*12399./WAVe(Ix+7)
         POWer(Ix+7) = POWer(Ix+7) + addit
         POWer(Ix+8) = POWer(Ix+8) + .3333*addit*WAVe(Ix+7)/WAVe(Ix+8)
C DIELECTRONIC RECOMB
         POWer(Ix+1) = POWer(Ix+1) + 1.6E11*E3(Ix+1)*cn*ADH(1,N,T)
         POWer(Ix+2) = POWer(Ix+2) + 1.6E11*E3(Ix+2)*cn*ADH(2,N,T)
         POWer(Ix+3) = POWer(Ix+3) + 1.6E11*E3(Ix+3)*cn*ADH(3,N,T)
         p2ph = p2ph + 1.6E11*E3(Ix+1)*cn*ADH(4,N,T)
 
C DENSITY DEPENDENCE :  PRADHAN; BERRINGTON, FON & KINGSTON FOR HE I
         crit1 = cr(1,N)
         crit2 = cr(2,N)
         crit3 = cr(3,N)
         critm = 1./(1./crit1+1./crit2+1./crit3)
         crit = 5000.*z**8*SQRT(T)
         IF ( N.EQ.2 ) crit = 3.1E5*SQRT(T)
         p2dum = p2ph
         p2ph = (p2ph+POWer(Ix+2)*(critm/crit3)*Dene/(Dene+critm))
     &          *crit/(Dene+crit)
 
         POWer(Ix+1) = POWer(Ix+1) + p2dum*Dene/(Dene+crit)
     &                 + POWer(Ix+2)*(critm/crit2)*Dene/(Dene+critm)
         branch = br(N)
         pdum = POWer(Ix+3)
         POWer(Ix+3) = POWer(Ix+3)*(1.-branch) + POWer(Ix+2)
     &                 *(critm/crit1)*(Dene/(Dene+critm))
         fc = branch*(1.-branch)
         IF ( N.EQ.2 ) fc = 2.03E-5
         POWer(Ix+6) = pdum*branch*WAVe(Ix+3)/WAVe(Ix+6) + POWer(Ix+2)
     &                 *(WAVe(Ix+2)/WAVe(Ix+6))*fc*(Dene/crit1)
     &                 /(1.+Dene/critm)
         POWer(Ix+2) = (POWer(Ix+2)+branch*pdum)*critm/(Dene+critm)
C
         wav2 = WAVe(Ix+1)
         IF ( NBIn.NE.0 ) CALL TWOPH(wav2,p2ph,0)
      ENDIF
      RETURN
      END
 
