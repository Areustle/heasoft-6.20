**==gaunt.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION GAUNT(T,E,N,J,L,Dene)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , ah , b , bespec , bh , c , cc , cn , CONce , Dene , DNE , 
     &     E , em , ex , EXINT1 , gal , gal2 , GAUNT , gb , gbe
      REAL gbe1 , gbe2 , gbe3 , gbe4 , gbe5 , gc , gca , gf , ghe , gk , 
     &     gmg , gmgii , gmgp , gmshell , gn , gna , gna4 , GNCRCH , 
     &     GNDrec , gne
      REAL gnt , gox , HENeut , HEPlus , PCOol , PM , POT , POU , 
     &     POWer , RE , REDuc , RHY , st , T , t4 , tl , TU , x , y
      INTEGER i , ii , J , L , ll , m , N
C*** End of declarations inserted by SPAG
 
C     MAY '82 VERSION RELYING HEAVILY ON MANN AND ROBB CALCULATIONS,
C     BHATIA FOR DELTA N = 0 AND BHATIA AND MASON DELTA N = 1
 
      DIMENSION ah(5) , bh(5) , ghe(24) , gbe(8,5)
      DIMENSION gbe1(4) , gbe2(4) , gbe3(11) , gbe4(11) , gbe5(11) , 
     &          bespec(4,3)
      DIMENSION gb(8,6) , gc(8,9) , gn(8,4) , gox(8,8) , gf(8,9) , 
     &          gne(5,12)
      DIMENSION gna(8,2) , gna4(3,3) , gmgii(12)
      DIMENSION gmg(10) , gmgp(8,2)
      DIMENSION gal(10) , gal2(10) , a(10) , b(10) , c(10)
      DIMENSION gmshell(6,4) , gk(10) , gca(10)
 
      COMMON /INTERC/ REDuc(4)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
 
      DATA ah/0. , .08 , .08 , .08 , 0./
      DATA bh/.22 , .16 , .19 , .20 , 0./
      DATA ghe/.03785 , .0002038 , .04214 , -.001901 , -.03263 , 
     &     .004462 , .28 , 0. , .02327 , -.0007424 , -.06369 , .001865 , 
     &     .07711 , .000058 , 0. , 0. , -.03168 , .0008439 , .182 , 
     &     -.007289 , -.05873 , .009605 , 0. , 0./
      DATA gbe/.7114 , .00558 , -.9277 , -.00153 , .2711 , .00523 , 
     &     0.0 , 0.0 , 0.0 , 0.0 , -.044 , .00735 , .482 , -.01855 , 
     &     0.0 , 0.0 , .44 , 0.0 , -.010 , 5*0.0 , .077 , .00666 , 
     &     6*0. , -.2943 , -.00084 , .2574 , .01167 , .04852 , -.00777 , 
     &     .2671 , .00555/
      DATA gbe1/ - .0264 , 0.03 , 0.0 , 0.20/
      DATA gbe2/.0881 , 0.10 , .117 , 0.10/
      DATA gbe3/0.0 , .064 , .069 , .048 , .035 , .0303 , .025 , .02 , 
     &     .02 , .032 , .032/
      DATA gbe4/0.0 , .342 , .39 , .192 , .103 , .0836 , .063 , .043 , 
     &     .043 , 0.0 , 0.0/
      DATA gbe5/0.0 , 3*0.0 , 55000. , 64000. , 86000. , 97000. , 
     &     108000. , 156000. , 168000./
      DATA bespec/.00069 , .122 , .0059 , .0063 , .0016 , .13 , .0073 , 
     &     .0058 , .0017 , .127 , .0087 , .0040/
      DATA gb/.2392 , .00543 , .3723 , .0335 , .0321 , -.0253 , .4591 , 
     &     -.00232 , .2317 , .00654 , -.1479 , .03186 , .3038 , 
     &     -.02130 , .3904 , -.00302 , .0811 , .01318 , -.2523 , 
     &     .04591 , .2122 , -.02192 , .2304 , .00639 , -.112 , .0063 , 
     &     .050 , .011 , .0808 , -.005 , .239 , .0023 , .126 , 0.0 , 
     &     .287 , 0.0 , 0.0 , 0.0 , .28 , 0.0 , .128 , -.025 , -.46 , 
     &     .133 , .138 , -.023 , -.70 , .128/
      DATA gc/.3539 , .00039 , .2314 , .02314 , .0358 , -.01534 , 
     &     .5449 , -.00858 , .2005 , .00395 , -.3012 , .04332 , 
     &     -.09599 , .01606 , .3727 , -.001517 , .3229 , -.002883 , 
     &     .0212 , .02481 , .0783 , -.01326 , .4671 , -.008106 , -.112 , 
     &     .0063 , .050 , .011 , .0808 , -.005 , .239 , .0023 , .128 , 
     &     -.025 , -.46 , .133 , .138 , -.023 , -.70 , .128 , -.269 , 
     &     -.00117 , .0318 , .0233 , -.0102 , -.0075 , .235 , .0172 , 
     &     .22 , .0056 , 6*0. , .198 , .0069 , 6*0. , 8*0./
      DATA gn/ - .01876 , .0011 , .0083 , .00182 , .0135 , -.00083 , 
     &     .040 , .00040 , .51 , -.1 , -1.84 , .53 , .55 , -.091 , 
     &     -2.80 , .51 , .128 , -.025 , -.46 , .133 , .138 , -.023 , 
     &     -.70 , .128 , -.07504 , .0042 , .033 , .00726 , .054 , 
     &     -.0033 , .159 , .0015/
C
      DATA gox/.69 , 0.0 , 4*0.0 , .46 , 0.0 , -.112 , .0063 , .050 , 
     &     .011 , .0808 , -.005 , .2391 , .0023 , -.112 , .0063 , .050 , 
     &     .011 , .0808 , -.005 , .2391 , .0023 , .128 , -.025 , -.46 , 
     &     .133 , .138 , -.023 , -.70 , .128 , .51 , -.1 , -1.84 , .53 , 
     &     .55 , -.091 , -2.80 , .51 , .22 , .0056 , 6*0.0 , 8*0. , 
     &     .1261 , 0.0 , .2869 , 0.0 , 0.0 , 0.0 , .28 , 0.0/
      DATA gf/.2246 , .0075 , .3055 , .0062 , -.0575 , -.0017 , .4248 , 
     &     -.0049 , -.063 , 0. , .407 , 0. , -.0238 , 0. , .478 , 0. , 
     &     -.0157 , 0. , .188 , 0. , .0195 , 0. , .283 , 0. , -.001 , 
     &     0. , .134 , 0. , .123 , 0. , .158 , 0. , .51 , -.1 , -1.84 , 
     &     .53 , .55 , -.091 , -2.8 , .51 , .039 , .0154 , 6*0. , 8*0. , 
     &     .22 , .056 , 6*0. , 8*0./
      DATA gne/ - .72117 , 1.2406 , 11.746 , 8.2169 , -7.7772 , 1.0227 , 
     &     .70828 , 4.5400 , 4.1450 , -3.8657 , -1.04290 , .84411 , 
     &     6.7031 , 3.1927 , -3.1693 , -.039582 , .26837 , .25803 , 
     &     .086929 , -.086641 , -.017323 , .29514 , .29301 , .13223 , 
     &     -.13071 , -.071593 , .15504 , .12598 , -.000449 , .000523 , 
     &     .040202 , .25113 , .14858 , .030780 , -.030745 , .45761 , 
     &     .38477 , .52142 , .92153 , -.91649 , -.17862 , .32249 , 
     &     .28172 , .040677 , -.040639 , -.062670 , .14921 , 1.5354 , 
     &     1.0586 , -1.0219 , -.057871 , .030701 , .36471 , .14784 , 
     &     -.14293 , .093106 , -.001108 , -.067652 , -.021663 , .021657/
C
      DATA gna/.1586 , .007375 , .1866 , .04156 , .02403 , -.02416 , 
     &     .3100 , -.00098 , -.3245 , 0.0 , .5548 , 0.0 , -.1562 , 0.0 , 
     &     .266 , 0./
      DATA gna4/1.153 , -.0333 , 0.0 , .001 , .0053 , .058 , .67 , 
     &     -.0133 , 0.0/
      DATA gmgii/.24 , 37.3 , 68.3 , 96. , 96. , 2.39 , 5.99 , 5.99 , 
     &     0. , 0. , 0. , .142/
      DATA gmg/.9 , .2 , .25 , .23 , .23 , .2 , .2 , .2 , .14 , .0094/
      DATA gmgp/ - .1333 , .0155 , -.6758 , .0577 , .5057 , -.0323 , 
     &     .314 , 0.0 , -.294 , 0.0 , .5043 , 0.0 , -.1619 , 0.0 , 
     &     .2606 , 0.0/
      DATA gal/0.0139 , .01 , .06 , 0. , 0.2 , 3*0. , 1.64 , .11/
      DATA gal2/0.0 , .28 , .28 , 0. , 0. , 3*0. , .00 , .28/
      DATA a/.022 , .625 , .660 , .240 , .061 , .256 , .0368 , .271 , 
     &     .833 , .696/
      DATA b/.0035 , .360 , .432 , .019 , .364 , .354 , .343 , .794 , 
     &     .404 , .387/
      DATA c/.1261 , .1261 , .1261 , .477 , .162 , .0108 , .138 , 
     &     .0719 , .1261 , .1261/
      DATA gmshell/.453 , .91 , .13 , .93 , .2 , .2 , .7 , .98 , .13 , 
     &     .93 , 0. , 0. , .59 , .95 , .13 , .93 , 0. , 0. , .51 , .95 , 
     &     .20 , .29 , 0. , 0./
      DATA gk/.35 , .35 , 1.1 , .092 , .91 , .97 , 1.1 , 3*0./
      DATA gca/.35 , .21 , 43. , .14 , .12 , .43 , 37. , .20 , .11 , 4./
 
C  SHOULD PUT IN SENSIBLE F'S FOR POTASSIUM ISOSEQUENCE
 
      t4 = T/10000.
      st = SQRT(T)
      GAUNT = 0.2
      y = E*11590./T
      cc = EXINT1(y,2)
      IF ( J.EQ.1 .AND. N.GT.2 ) THEN
         GAUNT = .01
         IF ( y.LE.10. ) GAUNT = AMIN1(EXP(-.7*ALOG(y)-2.3),.28*cc)
      ELSE
C
         ii = N - J + 1
         IF ( ii.GE.18 ) THEN
            IF ( ii.LE.18 ) THEN
               RETURN
            ELSEIF ( ii.GT.19 ) THEN
               IF ( L.LE.3 ) GAUNT = AMAX1(.2,-.606*ALOG10(y)-.052)
               RETURN
            ELSE
               GAUNT = gk(L)
               IF ( N.EQ.20 ) GAUNT = gca(L)
               RETURN
            ENDIF
         ELSEIF ( ii.EQ.2 ) THEN
C  HELIUM-LIKE IONS:  PRADHAN+CASCADES FOR N=2, MEWE N=3,4
C  NEUTRAL HE FROM Berrington et al J Phys B 18,4135; Aggarwal Ap J 278,874
C  L = 9 HAS GAUNT = 0.0,  SATELLITE LINES : L = 6 DONE BY BRANCH FROM L=3
            IF ( N.EQ.2 ) THEN
               IF ( L.EQ.1 ) GAUNT = AMIN1(.00656*t4**(.89),.30*cc)
               IF ( L.EQ.4 .OR. L.EQ.5 ) GAUNT = .30*cc
               IF ( L.EQ.2 ) GAUNT = AMIN1(.0283*t4**(-.10),.359*t4**(-
     &                               .569))
               IF ( L.EQ.3 ) GAUNT = AMIN1(.0105*t4**(.30),.095*t4**(-
     &                               .479))
               IF ( L.EQ.6 ) GAUNT = 0.0
               IF ( L.EQ.7 ) GAUNT = 4.08E-7*T**.941
               IF ( L.EQ.8 ) GAUNT = 2.46E-8*T**1.22
               IF ( L.EQ.9 ) GAUNT = 0.0
            ELSEIF ( L.GT.3 ) THEN
               IF ( L.EQ.4 ) GAUNT = (1.+7./N)
     &                               *(.053+.022*(y*y*y*cc-y*y+y)
     &                               +.276*cc)
               IF ( L.EQ.5 ) GAUNT = (1+1.5/N)
     &                               *(.053+.022*(y*y*y*cc-y*y+y)
     &                               +.276*cc)
               IF ( L.EQ.6 ) GAUNT = 0.
               IF ( L.EQ.7 ) GAUNT = .04*(y-y*y*cc)
               IF ( L.EQ.8 ) GAUNT = .02
               IF ( L.EQ.9 ) GAUNT = 0.
               RETURN
            ELSE
               GAUNT = GNCRCH(ghe,L,y,cc,N)
               RETURN
            ENDIF
         ELSEIF ( ii.EQ.3 ) THEN
C  LITHIUM-LIKE IONS: MEWE MODIFIED TO FIT CLOSE-COUPLING CALCULATIONS OF ROBB, HENRY
            x = 1./(N-3.)
            IF ( L.EQ.1 ) GAUNT = (0.68+.02*N)
     &                            *((.7+.35*x)+((1.-.8*x)*y+(.5-.5*x)
     &                            *y*y+.28)*cc-(.5-.5*x)*y)
            IF ( L.EQ.2 ) GAUNT = .053 + .16*cc
            IF ( L.EQ.3 .OR. L.EQ.4 ) GAUNT = -(.16+.32*x)
     &           + ((.8-.56*x)*y+.2*y*y+.28)*cc - .2*y
            IF ( L.EQ.5 ) GAUNT = (.19+.25*x) + .079*cc
            IF ( L.EQ.6 ) GAUNT = .31 - .1*y*cc
            IF ( L.EQ.7 ) GAUNT = .096 + .32*x
            IF ( L.EQ.8 ) GAUNT = .13
         ELSEIF ( ii.EQ.4 ) THEN
C  BERYLLIUM-LIKE IONS:  QUB FOR N=2 UP THROUGH NE; MANN, ROBB & SAMPSON PLUS RESONANCES FOR OTHERS
C  MANN AND MALINOVSKY FOR N = 3 AND QUB O V N=3 FROM WIDING
C  N = 4 FROM JOHNSTON & KUNZE, MASON & STOREY (FE XXII) AND LI-LIKE
            i = N - 5
            IF ( N.GE.10 ) i = N/2 - 1
            IF ( N.GE.26 ) i = N/2 - 3
            IF ( L.EQ.1 ) THEN
               tl = ALOG10(T)
               GAUNT = .54 + .0125*N + .135*cc
               IF ( N.LE.10 ) GAUNT = gbe1(i) + gbe2(i)*tl
            ELSEIF ( L.NE.2 ) THEN
               IF ( L.LE.7 ) GAUNT = GNCRCH(gbe,L-2,y,cc,N)
               IF ( L.EQ.8 ) GAUNT = .1261 + .2869*y*cc + .276*cc
               IF ( N.LE.8 ) THEN
                  em = -4.67 + 1.86*N
                  ex = EXP(-em*11590./T)
                  m = N - 5
                  IF ( L.EQ.10 ) GAUNT = bespec(1,m)*(1.-PM(1))
     &                 *ex + bespec(2,m)*PM(1)
                  IF ( L.EQ.11 ) GAUNT = bespec(3,m)*(1.-PM(1))
     &                 *ex + bespec(4,m)*PM(1)
               ENDIF
            ELSE
               gnt = gbe3(i)/(1.+gbe4(i)/y) + gbe5(i)/T
               IF ( N.EQ.6 ) gnt = .046/(1.+T*T/6.25E10)
               GAUNT = gnt*REDuc(1)
            ENDIF
         ELSEIF ( ii.EQ.5 ) THEN
C  BORON ISOSEQUENCE: N=2 INTERPOLATED FROM O IV AND FE XXII OF ROBB; GOOD TO 10% WRT FLOWER & NUSSBAUMER
C  NA-S, DERE ET AL AR, MANN C II;  OVEREST BY 36% NEAR THRESHOLD WRT ROBB CC FOR C II
C  N = 3 INTERPOLATED C II AND FE XXII FROM MANN; 2S-3L SCALED FROM BE-, F- & NE-LIKE IONS, AGREES W MASON & STOREY
C  TO 8% FOR 2S-3P.  N = 4 FROM MASON & STOREY
C  L = 9 INTERCOMBINATION LINE
            IF ( L.LE.6 ) GAUNT = GNCRCH(gb,L,y,cc,N)
            IF ( L.EQ.9 ) GAUNT = REDuc(2)
            IF ( L.EQ.12 ) GAUNT = 0.
         ELSEIF ( ii.EQ.6 ) THEN
C  CARBON ISOSEQUENCE: N=2 INTERP FROM MANN AND ROBB;  AGREES TO 5-10% WITH BHATIA AR, MG
C  N=3,4 GENERAL B-F; AGREES 3-10% WITH MANN 2P-3D; L=6 INTERCOMB
            IF ( L.LE.5 ) GAUNT = GNCRCH(gc,L,y,cc,N)
            IF ( L.EQ.7 ) GAUNT = .198 + .0069*N
            IF ( L.EQ.8 ) GAUNT = .22 + .0056*N
            IF ( L.EQ.9 ) GAUNT = .1261 + (.2869*y+.28)*cc
            IF ( L.EQ.6 ) GAUNT = REDuc(3)
            IF ( L.EQ.6 .AND. N.EQ.6 ) GAUNT = SQRT(.0001*T)*REDuc(3)
            IF ( L.EQ.12 ) GAUNT = 0.
         ELSEIF ( ii.EQ.7 ) THEN
C  NITROGEN SEQUENCE:   N=2 FROM MANN FE XX AND MASON & BHATIA MG,SI,S,AR,CA
C  N=3 AND 4 GENERAL B-F
            IF ( L.EQ.1 ) GAUNT = (1.086-1.71/J)
     &                            *(.3642+.9358*y*cc-.3758*(y-y*y*cc)
     &                            +.3586*cc)
            IF ( L.GE.2 .AND. L.LE.5 ) GAUNT = GNCRCH(gn,L-1,y,cc,N)
            IF ( L.EQ.12 ) GAUNT = 0.
            IF ( L.EQ.8 ) GAUNT = .22 + .0056*N
            IF ( L.EQ.9 ) GAUNT = .1261 + .2869*y*cc + .276*cc
         ELSEIF ( ii.EQ.8 ) THEN
C  OXYGEN ISOSEQUENCE:  N=2 FROM MANN, ROBB FE XIX AND FROM BHATIA,F&D SI, S, AR
C  OTHERS GENERIC B-F
            IF ( L.LE.8 ) GAUNT = GNCRCH(gox,L,y,cc,N)
            IF ( L.EQ.10 ) GAUNT = .16 + .0015*N
            IF ( L.EQ.15 ) GAUNT = 0.
         ELSEIF ( ii.EQ.9 ) THEN
C  FLUORINE SEQUENCE:   N=2 FROM MANN AL V AND ROBB FE XVIII;   OTHERS GENERIC B-F ISO; N=3 FROM MANN
            IF ( L.LE.8 ) GAUNT = GNCRCH(gf,L,y,cc,N)
            IF ( L.EQ.9 ) GAUNT = .1261 + (.2869*y+.28)*cc
         ELSEIF ( ii.EQ.10 ) THEN
C     NEON SEQUENCE: SMITH ET AL INCLUDING CASCADES AND RESONANCES
C     ASSUME THEY ARE ALL LIKE FE XVII
            IF ( L.LE.12 ) GAUNT = gne(1,L)
     &                             + (gne(2,L)+gne(3,L)*y+gne(4,L)*y*y)
     &                             *EXINT1(y,2) + gne(5,L)*y
         ELSEIF ( ii.EQ.11 ) THEN
C     SODIUM SEQUENCE:  MANN, FLOWER & NUSSBAUMER, AND BLAHA
            IF ( N.EQ.12 ) THEN
               GAUNT = gmgii(L)
               IF ( L.EQ.1 ) GAUNT = .112 + (.0269*y-.0998*y*y+.318)
     &                               *cc + .0998*y
               IF ( L.EQ.2 ) GAUNT = 141 + (59.3*y-671*y*y+.858)
     &                               *cc + 671*y
               RETURN
            ELSEIF ( L.LE.2 ) THEN
               GAUNT = GNCRCH(gna,L,y,cc,N-10)
               IF ( N.EQ.14 .AND. L.EQ.2 ) GAUNT = -.0172 + 
     &              (.832*y+.029*y*y+.3513)*cc - .029*y
               RETURN
            ELSEIF ( L.GT.5 ) THEN
               IF ( L.EQ.6 ) GAUNT = -.16 + .8*y*cc - .2*(y-y*y*cc)
     &                               + .276*cc
               IF ( L.EQ.7 ) GAUNT = .44 - 0.1*y*cc
               IF ( L.EQ.8 ) GAUNT = .15 - .05*y*cc
               IF ( L.EQ.9 .OR. L.EQ.10 ) GAUNT = 0.03
               IF ( L.EQ.11 ) GAUNT = 0.07
               IF ( L.EQ.12 ) GAUNT = 0.15
               RETURN
            ELSE
               ll = L - 2
               GAUNT = gna4(1,ll) + gna4(2,ll)*N + gna4(3,ll)*cc
               RETURN
            ENDIF
         ELSEIF ( ii.EQ.12 ) THEN
C     MAGNESIUM SEQUENCE:   INTERPOLATE SI TO FE FOR 3P, 4P EXCITATIONS;
C     USE MANN FE GAUNT FACTORS FOR 3D, 4S, 4F, AND INTERCOMB.  ASSUME
C     G FOR 4D = G FOR 4F;  L=10 IS INTERCOMBINATION LINE
            GAUNT = gmg(L)
            IF ( L.LE.2 ) GAUNT = GNCRCH(gmgp,L,y,cc,N)
            IF ( L.EQ.10 ) GAUNT = REDuc(4)
            IF ( L.EQ.10 .AND. N.EQ.14 ) GAUNT = REDuc(4)*(20000./T)
     &           **(.406)
            RETURN
         ELSEIF ( ii.EQ.13 ) THEN
C     ALUMINUM ISO-SEQUENCE:   SI II FROM ROBERTS, S IV 3P FROM MANN, 3D FROM BHATIA USED FOR AR & CA
C     FE XIV FROM BLAHA USED FOR NI AND FOR ALL N=4
C     RESONANCES INCLUDED FOR METASTABLE USING BHATIA COLLISION STRENGTHS
C     SI II 1814 LINES FROM BROWN, FERRAZ AND JORDAN
C     FE XIV RESONANCES COMPUTED AS IN SMITH ET AL FROM BLAHA OMEGAS
            IF ( L.LT.4 .OR. L.GT.8 ) THEN
               IF ( N.LE.14 ) THEN
                  GAUNT = gal(L) + gal2(L)*cc
                  IF ( L.EQ.1 ) GAUNT = gal(1)*1.9E7*st/(Dene+st*1.9E7)
                  RETURN
               ELSEIF ( N.LE.20 ) THEN
                  cn = st*2.7*10.**(7.+J/2.)
                  IF ( L.EQ.1 ) GAUNT = ((.128+(.3449*y+.3544*y*y)*cc-
     &                                  .3544*y)*1.8/(1.+.25/y))
     &                                  *.069*cn/(Dene+cn)
                  IF ( L.EQ.2 ) GAUNT = .0405 + 
     &                                  (.2052*y+.0328*y*y+.2311)
     &                                  *cc - .0328*y
                  IF ( L.EQ.3 ) GAUNT = .142 + .147*cc
                  IF ( L.EQ.9 ) GAUNT = .1330 + 
     &                                  (.3833*y+.0934*y*y+.1611)
     &                                  *cc - .0934*y
                  IF ( L.EQ.10 ) GAUNT = .1684 + 
     &                 (.2432*y+.0484*y*y+.2638)*cc - .0484*y
                  RETURN
               ENDIF
            ENDIF
            GAUNT = a(L) + b(L)*(1.-1./(1.+c(L)/y))
            IF ( L.EQ.1 ) GAUNT = (.0121+.0036/(1.+.1261/y))
     &                            *6.0E17/(Dene+6.0E17)
            RETURN
         ELSEIF ( ii.EQ.14 .OR. ii.EQ.15 .OR. ii.EQ.16 .OR. ii.EQ.17 )
     &            THEN
C
C  SILICON THROUGH CHLORINE SEQUENCES
            GAUNT = gmshell(L,ii-13)
            IF ( L.EQ.3 .AND. ii.NE.17 ) GAUNT = gmshell(L,ii-13)
     &           - .114/(1.+.138/y)
            RETURN
         ELSE
C
C  HYDROGENIC IONS: MEWE; HAYES AND SEATON: 3S,3D FROM THOMAS; OTHERS MEWE
            GAUNT = ah(L) + bh(L)*y*cc + .276*cc
            IF ( L.EQ.5 ) GAUNT = .212 - .203*(y-y*y*cc) + .0256*cc
            IF ( N.GT.2 ) RETURN
            IF ( L.EQ.5 ) GAUNT = .1198*y*cc - .1194*(y-y*y*cc)
     &                            + .0328*cc
         ENDIF
      ENDIF
      RETURN
      END
