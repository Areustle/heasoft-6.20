**==popmet.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION POPMET(N,Ij,Emet,Dene,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a21 , b21 , bee , bm1 , bmm , c , c21 , cm1 , cmm , cn , 
     &     CONce , cpr , Dene , DNE , e1 , e2 , e3 , ebe , ecm , Emet
      REAL emg , eres , fc , GNDrec , HENeut , HEPlus , PCOol , PLN , 
     &     PM , POPMET , pops , POT , POU , POWer , r , r5 , rb5 , rbe , 
     &     rc5 , RE
      REAL REDuc , RHY , rm1 , rm5 , rmm , rres , st , sum , sumc , T , 
     &     TLN , TU , v , x
      INTEGER i , Ij , j , N , nj , nn , no , NOSON , np
C*** End of declarations inserted by SPAG
C  RETURNS POPULATION OF METASTABLE LEVEL OF BE,B,C, OR MG-LIKE ION;  CARBON-NICKEL
C  RESONANCE CONTRIBUTIONS CALC FOR HIGH Z WITH SMITH ET AL METHOD
C  PASSES REDUC BACK TO GAUNT FOR INTERCOMBINATION LINES
 
      REAL mgmm , mgm1 , mgm5 , mg21 , mg5 , mg1
 
      DIMENSION rm5(12) , ebe(12) , pops(4)
      DIMENSION mgmm(3,7) , mgm1(7) , mg5(7) , mg21(2,7) , emg(7)
      DIMENSION nj(30) , a21(2,12) , b21(3,12) , c21(3,12) , rm1(12)
      DIMENSION rmm(3,12) , bm1(12) , bmm(3,12) , cm1(3,12) , cmm(2,12)
      DIMENSION ecm(2,12) , r(3,3) , x(3) , c(3) , cpr(3)
      DIMENSION rb5(12) , rc5(12) , rres(4,12) , eres(4,12)
      DIMENSION rbe(12) , bee(12)
 
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /INTERC/ REDuc(4)
      COMMON /METLN / PLN(4,3,8) , TLN(4,3,8)
 
      DATA rm5/0. , 18.6 , 11.7 , 6.09 , 2.79 , 1.54 , .93 , .61 , .41 , 
     &     .31 , .13 , .10/
      DATA ebe/0. , 6.20 , 7.87 , 9.51 , 12.81 , 16.1 , 19.5 , 23.11 , 
     &     26.9 , 28.9 , 55.5 , 63.1/
      DATA mg21/430. , 5.8E-4 , 22000. , .008 , 1.63E+5 , .000 , 9.E+5 , 
     &     .12 , 3.3E+6 , .26 , 4.5E+7 , 1.00 , 8.E+7 , 2./
      DATA mg5/0.2 , 0.5 , .26 , .035 , .026 , .0058 , .0039/
 
C  GILES QUOTED BY MENDOZA FOR S V, BALUJA FOR SI III
 
      DATA mgm1/.045 , .35 , .072 , .026 , .016 , .0069 , .0047/
      DATA mgmm/.5 , .38 , 1.5 , 1.4 , 1.06 , 4.1 , .25 , .39 , 1.32 , 
     &     .29 , .21 , .82 , .17 , .13 , .51 , .033 , .25 , .43 , .02 , 
     &     .13 , .30/
      DATA emg/1.63 , 3.72 , 5.50 , 7.13 , 8.72 , 13.9 , 17./
      DATA nj/0 , 1 , 0 , 0 , 0 , 2 , 3 , 4 , 0 , 5 , 0 , 6 , 0 , 7 , 
     &     0 , 8 , 0 , 9 , 0 , 10 , 5*0 , 11 , 0 , 12 , 0 , 0/
      DATA a21/2*0. , 77.2 , .00282 , 471. , .00629 , 1880. , .0118 , 
     &     16200. , .0306 , 84500. , .0629 , 320000. , .112 , 972600. , 
     &     .1823 , 2.546E+6 , .2781 , 5.914E+6 , .4029 , 7.3E+7 , 1.21 , 
     &     1.4E+8 , 1.8/
      DATA b21/3*0. , 332. , 130. , 82. , 1183. , 130. , 347. , 4210. , 
     &     499. , 1470. , 28400. , 3170. , 8910. , 152000. , 19800. , 
     &     56200. , 705000. , 102000. , 288000. , 43790. , 11613. , 
     &     27300. , 4.28E+06 , 7.08E+05 , 2.42E+06 , 1.02E+07 , 
     &     1.55E+06 , 5.62E+06 , 8.E+7 , 1.E+7 , 5.E+7 , 2.E+8 , 2.E+7 , 
     &     1.E+8/
      DATA c21/3*0. , .00031 , .0026 , 32. , .0041 , .0342 , 190. , 
     &     .022 , .16 , 385. , .599 , 4.64 , 6910. , 4.67 , 37.1 , 
     &     45200. , 26.1 , 200. , 197000. , 115. , 856. , 680000. , 
     &     424. , 3000. , 2.0E+06 , 1370. , 9090. , 5.32E+06 , 29400. , 
     &     128000. , 6.7E+7 , 95000. , 380000. , 1.7E+8/
 
C  REDUCED COLLISION RATES * 10. ** 7
 
      DATA rm1/0. , 9.384 , 4.944 , 2.814 , 1.340 , .7265 , .4718 , 
     &     .3164 , .2243 , .1732 , .08 , .062/
 
C  INCLUDES PROTON RATES FROM MUNRO THROUGH SI, EXT F OR S, NONE ABOVE A
 
      DATA rmm/3*0. , 29.9 , 13.4 , 50.8 , 17.7 , 8.41 , 32.2 , 11.2 , 
     &     6.00 , 21.8 , 6.48 , 4.19 , 13.9 , 3.96 , 3.04 , 9.68 , 
     &     2.91 , 2.67 , 7.96 , 2.19 , 2.42 , 6.83 , 1.25 , 0.40 , 
     &     1.79 , .533 , .317 , 1.40 , .046 , .16 , .67 , .020 , .12 , 
     &     .52/
      DATA bm1/0. , 14. , 8.1 , 5.3 , 4.1 , 3.01 , 2.0 , 1.5 , 1.1 , 
     &     .95 , .67 , .58/
      DATA bmm/3*0. , 23. , 7.9 , 31. , 17.1 , 5.73 , 20.8 , 12.3 , 
     &     4.17 , 14.1 , 6.52 , 2.39 , 7.91 , 3.84 , 1.47 , 3.01 , 
     &     2.65 , 1.01 , 3.16 , 18.6 , 13.9 , 38. , 1.68 , .57 , 1.9 , 
     &     1.34 , .43 , 1.45 , .69 , .18 , .69 , .55 , .14 , .54/
      DATA cm1/3*0. , 12.1 , 12.9 , 10. , 51. , 29.5 , 23. , 12.0 , 
     &     8.6 , 4.18 , 9.1 , 6.4 , 2.92 , 6.75 , 4.72 , 2.1 , 5.39 , 
     &     3.86 , 1.55 , 4.17 , 3.04 , 1.19 , 2.84 , 2.02 , 1.00 , 
     &     2.43 , 1.57 , 0.90 , 1.1 , .75 , .60 , 1.0 , .56 , .54/
      DATA cmm/2*0. , 12.9 , .0005 , 32.5 , .001 , 29. , .0015 , 17.5 , 
     &     .00205 , 11.2 , .00173 , 7.42 , .00173 , 5.44 , .00518 , 
     &     4.1 , .0069 , 3.52 , .0138 , 1.7 , .03 , 1.5 , .05/
      DATA ecm/2*0. , 1.26 , 2.68 , 1.90 , 4.05 , 2.42 , 5.34 , 3.76 , 
     &     7.93 , 5.09 , 10.6 , 6.58 , 13.4 , 8.34 , 16.5 , 10.6 , 
     &     20.1 , 13.5 , 24.5 , 30.1 , 45. , 51. , 62./
      DATA rb5/0. , 25. , 21.6 , 16.9 , 8.80 , 5.23 , 3.64 , 15.5 , 
     &     4*0./
      DATA rc5/12*0./
      DATA bee/0. , 10.5 , 13.4 , 16.3 , 22.1 , 28. , 34. , 40.5 , 4*0./
      DATA rbe/0. , 115. , 95.4 , 78.4 , 47.7 , 32.1 , 24.6 , 18.2 , 
     &     4*0./
      DATA eres/4*0. , 12.7 , 9.25 , 7.94 , 0. , 16.3 , 12.5 , 11.5 , 
     &     0. , 19.7 , 15.8 , 14.9 , 0. , 27.0 , 22.3 , 21.9 , 0. , 
     &     33.8 , 28.8 , 28.9 , 4.35 , 40.9 , 35.8 , 36.3 , 10.3 , 
     &     48.4 , 11.6 , 44.1 , 15.8 , 16*0./
      DATA rres/4*0. , 327. , 95. , 5.9 , 0. , 306. , 111. , 74.9 , 0. , 
     &     229. , 97.8 , 139. , 0. , 141. , 60.7 , 111. , 0. , 96. , 
     &     41.3 , 70.3 , 68.0 , 71.7 , 30.3 , 37.1 , 956. , 52.5 , 
     &     22.0 , 26.5 , 64.3 , 16*0./

      np = 0
      r5 = 0.0
      mgm5 = 0.0

      DO 100 i = 1 , 3
         DO 50 j = 1 , 3
            r(i,j) = 0.
 50      CONTINUE
 100  CONTINUE
c  avoid overflow for low t
 
      REDuc(Ij) = 0.
      POPMET = 0.
      IF ( Emet*11590./T.GE.30. ) RETURN
      st = SQRT(T)
      no = nj(N)
      v = Dene*1.E-7/st
      IF ( Ij.EQ.2 ) THEN
C  B-LIKE IONS
         cn = CONce(N-4)
         r5 = rb5(no)*v*EXP(-11590.*(eres(2,no)-Emet)/T)
         r(1,1) = -(bm1(no)+bmm(1,no)*2.+bmm(2,no)*3.)*v - b21(1,no)
         r(2,2) = -(bm1(no)+bmm(1,no)+bmm(3,no)*1.5)*v - b21(2,no)
         r(3,3) = -(bm1(no)+bmm(2,no)+bmm(3,no))*v - b21(3,no)
         r(2,1) = v*bmm(1,no)*2.
         r(3,1) = v*bmm(2,no)*3.
         r(1,2) = v*bmm(1,no)
         r(1,3) = v*bmm(2,no)
         r(3,2) = v*1.5*bmm(3,no)
         r(2,3) = v*bmm(3,no)
         c(1) = -bm1(no)*EXP(-Emet*11590./T)*v/3.
         c(2) = c(1)*2.
         c(3) = c(1)*3.
      ELSEIF ( Ij.EQ.3 ) THEN
C  C-LIKE IONS
         cn = CONce(N-5)
         r5 = rc5(no)*v*EXP(-11590.*(eres(3,no)-Emet)/T)
         e1 = EXP(-ecm(1,no)*11590./T)
         e2 = EXP(-ecm(2,no)*11590./T)
         e3 = EXP(-Emet*11590./T)
         r(1,1) = -(cm1(1,no)+cmm(1,no)*.2*e2/e1+cmm(2,no)*e3/e1)
     &            *v - c21(1,no)
         r(2,2) = -(cm1(2,no)+cmm(1,no))*v - c21(2,no)
         r(3,3) = -(cm1(3,no)+cmm(2,no))*v - c21(3,no)
         r(2,1) = v*cmm(1,no)*.2*e2/e1
         r(1,2) = v*cmm(1,no)
         r(3,1) = v*cmm(2,no)*e3/e1
         r(1,3) = v*cmm(2,no)
         r(3,2) = 0.
         r(2,3) = 0.
         c(1) = -cm1(1,no)*v*5.*e1/9.
         c(2) = -cm1(2,no)*v*e2/9.
         c(3) = -cm1(3,no)*v*e3*5./9.
      ELSEIF ( Ij.EQ.4 ) THEN
         cn = CONce(N-11)
         v = 8.63E-6*Dene/st
         np = no - 5
         mg1 = v*mgm1(np)
         mgm5 = v*mg5(np)*EXP(-11590.*emg(np)/T)
         IF ( N.EQ.14 ) mgm5 = mgm5/(1.+8.6E-6*T)
         IF ( N.EQ.14 ) mg1 = mg1/(1.+T*3.5E-6)
         r(1,1) = -mg1 - mgm5 - mgmm(1,np)*v - mgmm(2,np)*v
         r(2,2) = -mg1 - mgm5 - mgmm(1,np)*v/3 - mgmm(3,np)
     &            *v/3 - mg21(1,np)
         r(3,3) = -mg1 - mgm5 - mg21(2,np) - mgmm(2,np)
     &            *v/5. - mgmm(3,np)*v/5.
         r(2,1) = v*mgmm(1,np)
         r(3,1) = v*mgmm(2,np)
         r(1,2) = v*mgmm(1,np)/3.
         r(1,3) = v*mgmm(2,np)/5.
         r(3,2) = v*mgmm(3,np)/3.
         r(2,3) = v*mgmm(3,np)/5.
         c(1) = -mg1*EXP(-11590.*Emet/T)
         c(2) = c(1)*3.
         c(3) = c(1)*5.
      ELSE
         cn = CONce(N-3)
         r(1,1) = -(rm1(no)+rmm(1,no)*3.+rmm(2,no)*5.)*v
         r(2,2) = -a21(1,no) - v*(rm1(no)+rmm(1,no)+rmm(3,no)*1.6666667)
         r(3,3) = -a21(2,no) - v*(rm1(no)+rmm(2,no)+rmm(3,no))
         r5 = rm5(no)*v*EXP(-11590.*ebe(no)/T)
         r(1,1) = r(1,1) - r5
         r(2,2) = r(2,2) - r5
         r(3,3) = r(3,3) - r5
         r(2,1) = v*rmm(1,no)*3.
         r(3,1) = v*rmm(2,no)*5.
         r(1,2) = v*rmm(1,no)
         r(1,3) = v*rmm(2,no)
         r(3,2) = v*1.666667*rmm(3,no)
         r(2,3) = v*rmm(3,no)
         c(1) = -rm1(no)*EXP(-Emet*11590./T)*v
         c(2) = c(1)*3.
         c(3) = c(1)*5.
      ENDIF
      nn = NOSON(r,x,3,3)
      IF ( nn.EQ.0 ) PRINT 99001
      DO 200 i = 1 , 3
         cpr(i) = 0.
         DO 150 j = 1 , 3
            cpr(i) = cpr(i) + r(i,j)*c(j)
 150     CONTINUE
 200  CONTINUE
      sum = 1. + cpr(1) + cpr(2) + cpr(3)
      pops(1) = 1./sum
      pops(2) = cpr(1)/sum
      pops(3) = cpr(2)/sum
      pops(4) = cpr(3)/sum
      POPMET = (cpr(1)+cpr(2)+cpr(3))/sum
      IF ( Ij.EQ.3 ) POPMET = cpr(3)/sum
      fc = 1.E23*1.6E-12*cn/Dene
      IF ( Ij.EQ.2 ) THEN
         PLN(2,1,no) = (pops(1)*rres(2,no)*v*EXP(-11590.*eres(2,no)/T)
     &                 +POPMET*r5)*fc*eres(2,no)
         PLN(2,3,no) = (pops(2)*b21(1,no)+pops(3)*b21(2,no)+pops(4)
     &                 *b21(3,no))*fc*Emet
         sumc = -c(1) - c(2) - c(3)
         REDuc(2) = (pops(2)*b21(1,no)+pops(3)*b21(2,no)+pops(4)
     &              *b21(3,no))/sumc
         RETURN
      ELSEIF ( Ij.EQ.3 ) THEN
         PLN(3,1,no) = (pops(1)*rres(3,no)*v*EXP(-11590.*eres(3,no)/T)
     &                 +POPMET*r5)*fc*eres(3,no)
         PLN(3,3,no) = pops(4)*c21(3,no)*fc*Emet
         REDuc(3) = -pops(4)*c21(3,no)/c(3)
         RETURN
      ELSEIF ( Ij.NE.4 ) THEN
C  ERGS * 10-23 / S / ATOM
         PLN(1,1,no) = (pops(1)*rres(1,no)*v*EXP(-11590.*eres(1,no)/T)
     &                 +POPMET*r5)*fc*eres(1,no)
         PLN(1,2,no) = POPMET*v*rbe(no)*EXP(-11590.*bee(no)/T)
     &                 *fc*bee(no)
         PLN(1,3,no) = pops(3)*a21(1,no)*Emet*fc
         sumc = -c(1) - c(2) - c(3)
         REDuc(1) = (pops(3)*a21(1,no)+pops(4)*a21(2,no))/sumc
         RETURN
      ENDIF
      PLN(4,1,no) = (pops(1)*rres(4,no)*v*EXP(-11590.*eres(4,no)/T)
     &              +POPMET*mgm5)*fc*eres(4,no)
      PLN(4,3,no) = pops(3)*mg21(1,np)*fc*Emet
      sumc = -c(1) - c(2) - c(3)
      REDuc(4) = (pops(3)*mg21(1,np)+pops(4)*mg21(2,np))/sumc
      RETURN
99001 FORMAT (' BAD INVERSION')
      END
 
