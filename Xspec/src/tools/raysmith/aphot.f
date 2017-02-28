**==aphot.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE APHOT(N,Dene,Icont)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a2 , a2p , ABIn , ABUnd , ABUnj , b2 , b2p , BIN , BINmin , 
     &     BINsyz , C1 , C2 , CA , CNC , CONce , CTH , Dene , DNE , E , 
     &     E3
      REAL EA , eks , F , fact , fluor , HE , HEA , HEAting , HENeut , 
     &     HEPlus , hnu , pa1 , pa2 , PCOol , phot1 , PM , POT , POU , 
     &     POWer , PTOt
      REAL q , RE , RF , RHY , riso , rn , S2 , S3 , S4 , S5 , sg , 
     &     sss , TAU , TAUhe , TPLus , TU , WAVe , x
      INTEGER i , Icont , ik , imax , imin , iso , j , LL , N , NBIn , 
     &        NJ , nn
C*** End of declarations inserted by SPAG
C  AUGER PHOTOIONIZATION : N=1 AND N=2 SHELLS
C  USES COX & DALTABUIT AND REILMAN & MANSON CROSS SECTIONS
 
      INCLUDE 'rayspec.inc'
 
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      COMMON /HOT   / HE , HEA(2)
      COMMON /HETOT / HEAting
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /PT    / RF(500) , TAU(150) , TAUhe(150) , TPLus(150)
      COMMON /RESULT/ CONce(30) , CA(30) , POWer(220) , RHY , HENeut , 
     &                HEPlus , DNE , PCOol , POU , POT , RE , TU , PM(4)
      COMMON /RATES / C1(30) , C2(30) , CTH
      DIMENSION a2p(20,20) , b2p(20,20)
      DATA a2p/20*0 , 12.60 , 20.58 , 38*0. , 4.520 , 3.945 , 5.546 , 
     &     7.213 , 36*0. , 1.286 , 1.316 , 1.843 , 2.556 , 3.363 , 
     &     3.654 , 34*0. , .6109 , .6256 , .8755 , 1.111 , 1.254 , 
     &     1.578 , 1.919 , 2.307 , 32*0. , .4075 , .4438 , .4801 , 
     &     .5164 , .6798 , .8431 , .9660 , 1.089 , 1.288 , 1.488 , 
     &     110*0. , .1339 , .1531 , .1722 , .1909 , .2095 , .2320 , 
     &     .2545 , .3022 , .3499 , .3628 , .3756 , .4630 , .5504 , 
     &     .5820 , .6136 , .6679 , 24*0. , .1145 , .1294 , .1409 , 
     &     .1525 , .1644 , .1763 , .1797 , .1831 , .2005 , .2179 , 
     &     .2425 , .2670 , .3393 , .4116 , .4266 , .4415 , .5339 , 
     &     .6263 , 42*0./
      DATA b2p/20*0. , .1887 , -7.098 , 38*0. , 1.349 , 5.109 , 4.291 , 
     &     6.750 , 36*0. , 2.408 , 2.546 , 3.116 , 3.060 , 2.981 , 
     &     5.273 , 34*0. , 1.377 , 1.456 , 1.782 , 1.930 , 2.384 , 
     &     2.572 , 3.023 , 3.451 , 32*0. , .7788 , .9714 , 1.164 , 
     &     1.356 , 1.488 , 1.620 , 1.869 , 2.118 , 2.261 , 2.405 , 
     &     110*0. , .3454 , .3781 , .4107 , .4565 , .5023 , .5590 , 
     &     .6157 , .6283 , .6409 , .7392 , .8375 , .8258 , .8142 , 
     &     .9381 , 1.062 , 1.207 , 24*0. , .2541 , .2787 , .3087 , 
     &     .3387 , .3770 , .4152 , .4907 , .5661 , .6055 , .6499 , 
     &     .6729 , .7010 , .6410 , .5809 , .6970 , .8130 , .7768 , 
     &     .7406 , 42*0./
C
      HE = 0.
      DO 100 j = 1 , 30
         CA(j) = 0.
 100  CONTINUE
      rn = N
      nn = N - 2
      IF ( N.LE.2 ) RETURN
      DO 200 j = 1 , nn
         fact = 1.6E-12*1.E23*10.**(ABUnd-12.)*CONce(j)
         iso = N - j + 1
         riso = iso
         pa1 = 0.
         pa2 = 0.
         eks = 13.6*rn*rn*riso**(.24-.43/ALOG10(rn))
         ik = INT((eks-BINmin)/BINsyz)
         imax = NBIn
         IF ( iso.GE.11 ) imax = MIN0(ik,NBIn)
         imin = INT((EA(j)-BINmin)/BINsyz + 1)
         IF ( EA(j).LT.BINmin+(imin+0.5)*BINsyz ) imin = imin + 1
         imin = MAX0(1,imin)
         IF ( imin.LT.NBIn ) THEN
C
            IF ( iso.GT.10 ) THEN
               a2 = a2p(iso-10,N-10)
               b2 = b2p(iso-10,N-10)
               DO 110 i = imin , imax
                  hnu = BINmin + BINsyz*(i-0.5)
                  x = EA(j)/hnu
                  sg = a2*x*x + b2*x*x*x
                  phot1 = sg*(1.0E-18/1.6E-12)*RF(i)*ABIn(i)/hnu
                  pa1 = pa1 + phot1
                  HE = HE + phot1*hnu*fact
 110           CONTINUE
            ENDIF
            imin = MAX0(ik+1,1)
            IF ( imin.LT.NBIn ) THEN
               sss = 1.66*eks**(0.071)
               DO 120 i = imin , NBIn
                  hnu = BINmin + BINsyz*(i-0.5)
                  x = eks/hnu
                  sg = (295./eks)*x**sss
                  phot1 = sg*(1.0E-18/1.6E-12)*RF(i)*ABIn(i)/hnu
                  pa2 = pa2 + phot1
                  HE = HE + phot1*hnu*fact
 120           CONTINUE
            ENDIF
            fluor = 0.5*(rn/26.)**4
            CA(j) = (pa1+pa2)*(1.-fluor)/Dene
            C1(j) = C1(j) + (pa1+pa2)*fluor/Dene
            IF ( iso.EQ.3 ) THEN
               C1(j) = C1(j) + pa2/Dene
               CA(j) = 0.
            ENDIF
            IF ( iso.EQ.11 ) THEN
               CA(j) = (1.-fluor)*pa2/Dene
               C1(j) = C1(j) + (pa2*fluor+pa1)/Dene
            ENDIF
         ENDIF
 200  CONTINUE
c
      HEAting = HEAting + HE
c
      IF ( Icont.NE.0 ) RETURN
C
C  AVRETT'S METHOD FOR INCLUDING AUTOIONIZATION IN EQUILIBRIUM
C
      q = 0.
      x = CA(1)
      nn = N - 1
      DO 300 j = 1 , nn
         C1(j) = C1(j) + x
         IF ( C1(j).LE.0. ) GOTO 400
         q = C2(j+1)*CA(j)/C1(j)
         x = CA(j+1) + q
 300  CONTINUE
 400  RETURN
      END
 
