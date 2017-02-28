**==cion.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION CION(N,J,E,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , a0 , a1 , a2 , a3 , alpha , b , b0 , b1 , b2 , b3 , 
     &     beta , c , c0 , c1 , c2 , c3 , ch , ch2 , ch3
      REAL chi , chir , CION , d , d0 , d1 , d2 , d3 , E , fchi , T
      INTEGER iso , J , j2 , j3 , N
C*** End of declarations inserted by SPAG
 
C  SM YOUNGER JQSRT 26, 329; 27, 541; 29, 61   WITH MOORES FOR UNDONE
C  A0 FOR B-LIKE ION HAS TWICE 2S PLUS ONE 2P  AS IN SUMMERS ET AL
C  CHI = kT / I
 
      DIMENSION a0(30) , a1(30) , a2(30) , a3(30) , b0(30) , b1(30) , 
     &          b2(30) , b3(30) , c0(30) , c1(30) , c2(30) , c3(30) , 
     &          d0(30) , d1(30) , d2(30) , d3(30)
 
      DATA a0/13.5 , 27.0 , 9.07 , 11.80 , 20.2 , 28.6 , 37.0 , 45.4 , 
     &     53.8 , 62.2 , 11.7 , 38.8 , 37.27 , 46.7 , 57.4 , 67.0 , 
     &     77.8 , 90.1 , 106. , 120.8 , 135.6 , 150.4 , 165.2 , 180.0 , 
     &     194.8 , 209.6 , 224.4 , 239.2 , 154.0 , 268.8/
      DATA a1/ - 14.2 , -60.1 , 4.30 , 27*0./
      DATA a2/40.6 , 140. , 7.69 , 27*0./
      DATA a3/ - 17.1 , -89.8 , -7.53 , 27*0./
 
      DATA b0/ - 4.81 , -9.62 , -2.47 , -3.28 , -5.96 , -8.64 , -11.32 , 
     &     -14.00 , -16.68 , -19.36 , -4.29 , -16.7 , -14.58 , -16.95 , 
     &     -19.93 , -23.05 , -26.00 , -29.45 , -34.25 , -38.92 , 
     &     -43.59 , -48.26 , -52.93 , -57.60 , -62.27 , -66.94 , 
     &     -71.62 , -76.29 , -80.96 , -85.63/
      DATA b1/9.77 , 33.1 , -3.78 , 27*0./
      DATA b2/ - 28.3 , -82.5 , -3.59 , 27*0./
      DATA b3/11.4 , 54.6 , 3.34 , 27*0./
 
      DATA c0/1.85 , 3.69 , 1.34 , 1.64 , 2.31 , 2.984 , 3.656 , 4.328 , 
     &     5.00 , 5.672 , 1.061 , 1.87 , 3.26 , 5.07 , 6.67 , 8.10 , 
     &     9.92 , 11.79 , 7.953 , 8.408 , 8.863 , 9.318 , 9.773 , 
     &     10.228 , 10.683 , 11.138 , 11.593 , 12.048 , 12.505 , 12.96/
      DATA c1/0. , 4.32 , .343 , 27*0./
      DATA c2/0. , -2.527 , -2.46 , 27*0./
      DATA c3/0. , .262 , 1.38 , 27*0./
 
      DATA d0/ - 10.9 , -21.7 , -5.37 , -7.58 , -12.66 , -17.74 , 
     &     -22.82 , -27.9 , -32.98 , -38.06 , -7.34 , -28.8 , -24.87 , 
     &     -30.5 , -37.9 , -45.3 , -53.8 , -64.6 , -54.54 , -61.70 , 
     &     -68.86 , -76.02 , -83.18 , -90.34 , -97.50 , -104.66 , 
     &     -111.82 , -118.98 , -126.14 , -133.32/
      DATA d1/8.90 , 42.5 , -12.4 , 27*0./
      DATA d2/ - 35.7 , -131. , -8.09 , 27*0./
      DATA d3/16.5 , 87.4 , 1.23 , 27*0./
 
      CION = 0.
      chir = T/(11590.*E)
      IF ( chir.LE..0115 ) RETURN
      chi = AMAX1(chir,0.1)
      ch2 = chi*chi
      ch3 = ch2*chi
      alpha = (.001193+.9764*chi+.6604*ch2+.02590*ch3)
     &        /(1.0+1.488*chi+.2972*ch2+.004925*ch3)
      beta = (-.0005725+.01345*chi+.8691*ch2+.03404*ch3)
     &       /(1.0+2.197*chi+.2457*ch2+.002503*ch3)
      j2 = J*J
      j3 = j2*J
      iso = N - J + 1
 
      a = a0(iso) + a1(iso)/J + a2(iso)/j2 + a3(iso)/j3
      b = b0(iso) + b1(iso)/J + b2(iso)/j2 + b3(iso)/j3
      c = c0(iso) + c1(iso)/J + c2(iso)/j2 + c3(iso)/j3
      d = d0(iso) + d1(iso)/J + d2(iso)/j2 + d3(iso)/j3
 
C  FE II EXPERIMENTAL IONIZATION MONTAGUE ET AL: D. NEUFELD FIT
      IF ( N.EQ.26 .AND. J.EQ.2 ) THEN
         a = -13.825
         b = -11.0395
         c = 21.07262
         d = 0.
      ENDIF
 
      ch = 1./chi
      fchi = 0.3*ch*(a+b*(1.+ch)+(c-(a+b*(2.+ch))*ch)*alpha+d*beta*ch)
      IF ( iso.GE.4 .AND. iso.LE.10 ) fchi = fchi*1.59
c  correct Younger JQSRT 27, 541 from table to graphs
      CION = 2.2E-6*SQRT(chir)*fchi*EXP(-1./chir)/(E*SQRT(E))
      RETURN
      END
**==alphadi.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION ALPHADI(N,J,L,Ln,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , ALF , ALPHADI , b , C , DELT , dl , E , E3 , ebar , ES , 
     &     F , S , SIG , T , twork , WAVe , x , z12 , zf
      INTEGER J , L , LL , Ln , N
C*** End of declarations inserted by SPAG
C  DIELECTRONIC RECOMBINATION : BURGESS AND TWORKOWSKI FOR H-LIKE
      COMMON /DAT   / E(30) , S(30) , C(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , SIG(30) , ALF(30) , ES(30)
 
C  BURGESS AND TWORKOWSKI
      z12 = J - 12.
      twork = .84 + .5/J**2 + .03*z12/(1.+4.5E-5*z12**3)
      IF ( N.NE.J ) twork = 1.
      ALPHADI = 0.
      dl = DELT(N,J,L)
      IF ( dl.LE.0. ) RETURN
      zf = J - 1.
      b = SQRT(zf)*(zf+1.)**2.5/SQRT(zf*zf+13.4)
      x = E3(Ln)/((zf+1.)*13.6)
      a = SQRT(x)/(1.+.105*x+.015*x*x)
      ebar = E3(Ln)/(1.+.015*zf**3/(zf+1.)**2)
      IF ( N-J.EQ.1 ) THEN
         b = (0.5*zf/SQRT(zf))*b
         x = .75*J
         a = SQRT(x)*0.5/(1.+0.21*x+0.03*x*x)
      ENDIF
c  younger jqsrt 29, 67 for he-like ions
 
      ALPHADI = .0030*T**(-1.5)*a*b*F(Ln)*twork*dl*EXP(-ebar*11590./T)
      RETURN
      END
 
