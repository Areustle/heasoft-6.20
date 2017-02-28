**==phot.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION PHOT(N,J,E,T,Dene)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , ainner , BIN , BINmin , binner , 
     &     BINsyz , cn , CNC , CONce , Dene , DNE , E , E3 , EA , EE , 
     &     ein , einner , F
      REAL fact , fd1 , GNDrec , HE , HEA , HEAting , HENeut , HEPlus , 
     &     hnu , om , PCOol , PHOT , phot1 , PM , POT , POU , POWer , 
     &     PTOt , RE , RF
      REAL RHY , S2 , S3 , S4 , S5 , sg , ss2 , ss3 , ss4 , ss5 , T , 
     &     t4 , TAU , TAUhe , TPLus , TU , WAVe , x , x2
      INTEGER i , ia , iin , iiso , ily , imax , imin , inner , iso , 
     &        J , LL , N , NBIn , NJ , no , noj
C*** End of declarations inserted by SPAG
 
C  USES REILMAN & MANSON FOR INNER SUBSHELL PHOTOIONIZATION:  2S FOR B,C,N
C  SEQUENCES,; 3S FOR AL,SI,P SEQUENCES AND 3P FOR K,CA,SC SEQUENCES
C  ASSUMES THAT OUTER SHELL IONIZATION IS IN FOUR TERM POLYNOMIAL,
C  S2*X**2 + S3*X**3 + S4*X**4 + S5*X**5
 
      INCLUDE 'rayspec.inc'
 
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      COMMON /HOT   / HE , HEA(2)
      COMMON /HETOT / HEAting
      COMMON /DAT   / EE(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /PT    / RF(500) , TAU(150) , TAUhe(150) , TPLus(150)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      DIMENSION inner(30) , ainner(12,9) , binner(12,9) , einner(12,9)
      DATA inner/4*0 , 1 , 2 , 3 , 5*0 , 4 , 5 , 6 , 3*0 , 7 , 8 , 9 , 
     &     9*0/
      DATA einner/0. , 30.9 , 55.8 , 87.6 , 172. , 283. , 422. , 589. , 
     &     784. , 1006. , 1842. , 2178. , 0. , 15.6 , 36.7 , 63.8 , 
     &     139. , 241. , 371. , 528. , 713. , 925. , 1731. , 2056. , 
     &     2*0. , 20.3 , 42.6 , 108. , 201. , 321. , 469. , 644. , 
     &     847. , 1622. , 1938. , 6*0. , 22.9 , 57.6 , 105.2 , 165. , 
     &     421. , 531. , 6*0. , 13.5 , 43.8 , 87.6 , 144. , 388. , 
     &     493. , 7*0. , 30.7 , 70.4 , 123. , 356. , 458. , 9*0. , 28. , 
     &     213. , 296. , 9*0. , 38. , 190. , 271. , 10*0. , 169. , 246./
      DATA ainner/0. , 2.357 , 2.745 , 1.443 , .6024 , .3391 , .2058 , 
     &     .1385 , .09804 , .0764 , .04443 , .0376 , 0. , 18.26 , 
     &     6.950 , 2.817 , .9583 , .5001 , .2672 , .1685 , .1184 , 
     &     .0913 , .04837 , .0407 , 2*0. , 27.80 , 8.139 , 1.630 , 
     &     .7415 , .3621 , .2394 , .1478 , .112 , .05442 , .0455 , 
     &     6*0. , 4.961 , 2.695 , 1.401 , .8416 , .2929 , .2254 , 6*0. , 
     &     10.489 , 5.507 , 1.575 , 1.409 , .4378 , .3263 , 7*0. , 
     &     12.06 , 4.726 , 2.351 , .6321 , .4799 , 9*0. , 29.6 , 2.452 , 
     &     1.788 , 9*0. , 53.8 , 3.265 , 2.070 , 10*0. , 4.078 , 2.351/
      DATA binner/0. , -.7352 , .0201 , .4321 , .4094 , .2605 , .1935 , 
     &     .1423 , .111 , .0865 , .04050 , .0343 , 0. , -6.256 , 
     &     -.2887 , 1.463 , 1.029 , .5726 , .4080 , .2885 , .2132 , 
     &     .164 , .07817 , .0654 , 2*0. , -14.93 , 1.194 , 2.298 , 
     &     1.106 , .7338 , .4742 , .3510 , .267 , .1244 , .104 , 6*0. , 
     &     -4.607 , -1.863 , -.6435 , -.2594 , -.0225 , -.0111 , 6*0. , 
     &     -10.412 , -4.613 , -1.118 , -.4648 , -.0221 , .0105 , 7*0. , 
     &     -11.09 , -3.357 , -1.014 , -.0255 , -.0078 , 9*0. , -28.3 , 
     &     -.386 , -.360 , 9*0. , -45.8 , -.610 , -.178 , 10*0. , 
     &     -.834 , .0037/

      ein = 0.
      iin = 0
      PHOT = 0.
      HE = 0.
      DO 100 noj = 1 , 12
         no = noj
         IF ( N.LE.NJ(noj) ) GOTO 200
 100  CONTINUE
 200  IF ( J.EQ.1 ) THEN
         IF ( N.EQ.12 ) PHOT = 1.0E-10
         IF ( N.EQ.16 ) PHOT = 2.0E-10
         IF ( N.EQ.26 ) PHOT = 3.0E-10
         IF ( N.EQ.6 ) THEN
C  PHOTOIONIZATION OF C I FROM 1D LEVEL
            t4 = AMIN1(.0001*T,3.)
            om = 1.16*t4*(1.085-.07507*t4-.02150*t4*t4)
            cn = SQRT(T)*5.*3.26E-4/(8.63E-6*om)
            fd1 = 0.4*EXP(-1.45/t4)*Dene/(Dene+cn)
            ily = INT((10.2-BINmin)/BINsyz + 1)
            PHOT = 2.5E-10
            IF ( ily.GE.1 ) PHOT = PHOT + fd1*1.03E-17*6.12E10*ABIn(ily)
         ENDIF
      ENDIF
      imin = INT((E-BINmin)/BINsyz + 1)
      IF ( E.LT.BINmin+(imin+0.5)*BINsyz ) imin = imin + 1
      IF ( imin.GE.NBIn ) RETURN
      imin = MAX0(imin,1)
      IF ( N.NE.1 ) THEN
         ss2 = S2(J)
         ss3 = S3(J)
         ss4 = S4(J)
         ss5 = S5(J)
         fact = 1.6E-12*1.E23*10.**(ABUnd-12.)*CONce(J)
      ELSE
         ss2 = 0.00
         ss3 = 8.44
         ss4 = -2.14
         ss5 = 0.00
         fact = 1.6E-12*1.E23/(1.+RHY)
      ENDIF
      iso = N - J + 1
      ia = INT((EA(J)-BINmin)/BINsyz)
      ia = MAX0(ia,1)
      imax = NBIn
      IF ( iso.GE.3 ) imax = MIN0(NBIn,ia)
      iiso = inner(iso)
      IF ( iiso.NE.0 ) THEN
         ein = einner(no,iiso)
         iin = INT((ein-BINmin)/BINsyz + 1)
         IF ( ein.LT.BINmin+(iin+0.5)*BINsyz ) iin = iin + 1
         iin = MAX0(iin,1)
         imax = MIN0(iin-1,NBIn)
      ENDIF
      hnu = BINmin + (imin-0.5)*BINsyz
      DO 300 i = imin , imax
         hnu = hnu + BINsyz
         x = E/hnu
         x2 = x*x
         sg = ss2*x2 + ss3*x2*x + ss4*x2*x2 + ss5*x2*x2*x
         phot1 = sg*(1.E-18/1.6E-12)*RF(i)*ABIn(i)/hnu
         PHOT = PHOT + phot1
         HE = HE + phot1*hnu*fact
 300  CONTINUE
      IF ( N.EQ.2 ) HEA(J) = HE
C   INNER SUBSHELL
      IF ( iiso.NE.0 ) THEN
         IF ( iin.LT.NBIn ) THEN
            imax = MIN0(ia,NBIn)
            ss2 = ainner(no,iiso)
            ss3 = binner(no,iiso)
            DO 320 i = iin , imax
               hnu = BINmin + BINsyz*(i-0.5)
               x = ein/hnu
               sg = ss2*x*x + ss3*x*x*x
               phot1 = sg*(1.E-18/1.6E-12)*RF(i)*ABIn(i)/hnu
               PHOT = PHOT + phot1
               HE = HE + phot1*hnu*fact
 320        CONTINUE
         ENDIF
      ENDIF
      IF ( N.GE.6 ) HEAting = HEAting + HE
      RETURN
      END
 
