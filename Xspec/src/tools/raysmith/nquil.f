**==nquil.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE NQUIL(T,N,Dene,Jcont,Iphot,Idens,Icx)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL abhe , ABUnd , ABUnj , ALFLO , ALPHADI , apple , AUTOIN , b , 
     &     be , beta , BINmin , BINsyz , C1 , C2 , c5 , c6 , CA , cd1 , 
     &     cd2 , chi
      REAL CION , CONce , CTH , CTHEI , CTHER , CTHI , CTHR , dd , dei , 
     &     Dene , denh , DIMET , DNE , E , E3 , EA , EFFN , EFFNEW , 
     &     ei , eion
      REAL em , emet , emetj , F , fmet , fplus , GND , GREC , HENeut , 
     &     HEPlus , orange , over , P , PCOol , PHOT , plum , PM , 
     &     POPMET , POT , POU
      REAL POWer , prod , qm , rat , RE , recn , rgnd , rho , RHY , S2 , 
     &     S3 , S4 , S5 , SEATON , sec , st , starn , sum , T , t6
      REAL TU , WAVe , worl , zeff , zf
      INTEGER i , Icx , Idens , imet , imeta , Iphot , ix , iy , j , 
     &        Jcont , jlow , jmax , jp , jtop , jump , k , kmax , ktop , 
     &        l , lion
      INTEGER LL , ln , N , NBIn , NJ , nn , nqm , nval
C*** End of declarations inserted by SPAG
C   ************************  METASTABLE POPULATION DATA
 
      DIMENSION emetj(30,4) , cd1(28) , cd2(28)
      DIMENSION rat(30) , prod(30) , c5(30) , c6(30) , over(30) , 
     &          imeta(30)
      DIMENSION sec(30)
 
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /RATES / C1(30) , C2(30) , CTH
      COMMON /RESULT/ CONce(30) , CA(30) , POWer(220) , RHY , HENeut , 
     &                HEPlus , DNE , PCOol , POU , POT , RE , TU , PM(4)
 
      DATA emetj/5*0. , 6.5 , 8.35 , 10.2 , 0. , 14. , 0. , 17.6 , 0. , 
     &     21.6 , 0. , 25.3 , 0. , 29.0 , 0. , 32.7 , 5*0. , 47.0 , 0. , 
     &     51.8 , 0. , 0. , 5*0. , 5.34 , 7.10 , 8.87 , 0. , 12.5 , 0. , 
     &     16.0 , 0. , 19.8 , 0. , 24.0 , 0. , 28.6 , 0. , 34.2 , 5*0. , 
     &     57.1 , 0. , 64.8 , 0. , 0. , 5*0. , 4.18 , 5.78 , 7.48 , 0. , 
     &     11.0 , 0. , 14.7 , 0. , 18.6 , 0. , 23.0 , 0. , 28.0 , 0. , 
     &     33.9 , 5*0. , 60.4 , 0. , 69.2 , 0. , 0. , 11*0. , 2.72 , 
     &     0. , 6.55 , 0. , 10.4 , 0. , 14.2 , 0. , 18.1 , 5*0. , 29.7 , 
     &     0. , 38.9 , 2*0./
C
      DATA imeta/0 , 0 , 0 , 1 , 2 , 3 , 5*0 , 4 , 18*0/
      DATA cd1/.0024 , -.0005 , .00 , .061 , .027 , .011 , .005 , .005 , 
     &     0. , .0107 , .09 , .13 , .11 , .081 , .075 , .066 , .051 , 
     &     11*0./
      DATA cd2/0. , .01485 , .300 , .108 , .107 , .024 , .075 , .051 , 
     &     .054 , .0167 , .36 , 17*0./
      DATA sec/4*1. , .9 , .9 , 6*1. , .6 , .5 , 1. , .7 , .9 , 13*1./

      fmet=0
      nval=0
      ix = 0
      abhe = 10.**(ABUnj(1)-12.)
      DO 100 i = 1 , 4
         PM(i) = 0.
 100  CONTINUE
      orange = 1.
      c5(1) = 0.
      c5(N+1) = 0.
      jlow = 1
      jmax = N + 1
      jtop = N + 1
      c6(1) = 0
      t6 = T*1.0E-6
      st = SQRT(T)
      DO 200 j = 1 , N
C  IONIZATION RATE FROM YOUNGER
         eion = E(j)
         C1(j) = CION(N,j,eion,T)
         IF ( N-j.GT.1 ) THEN
C  IONIZATION FROM METASTABLE LEVEL : BE,B,C,MG SEQUENCES
            IF ( Idens.NE.0 ) THEN
               imet = imeta(N-j+1)
               IF ( imet.NE.0 ) THEN
                  emet = emetj(N,imet)
                  fmet = POPMET(N,imet,emet,Dene,T)
                  PM(imet) = fmet
                  em = E(j) - emet
                  C1(j) = C1(j)*(1.-fmet) + fmet*CION(N,j,em,T)
               ENDIF
            ENDIF
C  INNERSHELL IONIZATION
            IF ( N-j.GT.1 ) THEN
               eion = EA(j)
               jp = N - 1
               IF ( N-j.GE.10 ) jp = N - 9
               C1(j) = C1(j) + CION(N,jp,eion,T)
            ENDIF
         ENDIF
C  IONIZATION FOLLOWING INNERSHELL EXCITATION  :  COWAN AND MANN
C  INCREASED FOR FE
         C1(j) = C1(j) + AUTOIN(N,j,T)
C  ****************      PHOTOIONIZATION
         IF ( Iphot.NE.0 ) C1(j) = C1(j) + PHOT(N,j,E(j),T,Dene)/Dene
C  ****************      RADIATIVE RECOMBINATION
         zeff = j
         beta = zeff*zeff/(6.34*t6)
         C2(j+1) = 2.07E-11*zeff*zeff*(0.4288+0.5*ALOG(beta)
     &             +0.469*(beta**(-1./3.)))/SQRT(T)
         apple = C2(j+1)
         plum = 0
         recn = 0.
         rgnd = 0.
         IF ( N.NE.j ) THEN
            nval = INT(GND(N-j) + 0.1)
            starn = SQRT(13.6/E(j))*zeff
            DO 120 nqm = 1 , nval
               qm = nqm
               be = beta/(qm*qm)
               C2(j+1) = C2(j+1) - 5.197E-14*j*SQRT(be)*SEATON(be,qm)
 120        CONTINUE
            IF ( C2(j+1).LT.0 ) C2(j+1) = 0
            i = N - j
C  RECOMBINATION TO GROUND STATE
            IF ( i.LE.17 ) rgnd = GREC(N,j,E(j),T)
            IF ( i.GT.1 ) THEN
               IF ( i.LT.4 .OR. i.GT.9 ) THEN
                  ei = E(j)
                  IF ( i.LT.17 ) THEN
C  RECOMBINATION TO OTHER STATES IN SAME SHELL
                     lion = 1
                     IF ( i.EQ.12 ) lion = 3
                     ei = ei - E3(ix+lion)
                  ENDIF
                  starn = SQRT(13.595/ei)*zeff
                  be = beta/(starn*starn)
                  recn = EFFNEW(N-j,T,zeff,N)*SEATON(be,starn)
     &                   *2.599E-14*j*SQRT(ei*11590./T)/(starn*starn)
               ENDIF
            ENDIF
         ENDIF
         C2(j+1) = C2(j+1) + recn + rgnd
         IF ( beta.GE.0.5 ) THEN
            chi = 0.5*(0.735+ALOG(beta)+1./(3.*beta))
         ELSE
            chi = 1.2*beta + (0.55+1.04*ALOG(beta))*beta*beta + 
     &            (-0.43+1.01*ALOG(beta))*beta**3
         ENDIF
         DO 150 nqm = 1 , nval
            qm = nqm
            chi = chi - P(beta,qm)
 150     CONTINUE
         chi = chi + EFFN(N-j,zeff,T)*P(beta,starn)/(2.*starn*starn)
         c6(j+1) = E(j)*1.6027E-12*(C2(j+1)+4.22E-22*SQRT(T/1.037E-11)
     &             *chi)
C  DIELECTRONIC RECOMBINATION ***** DENSITY DEPENDENCE AND SECONDARY
C                             ***** AUTOIONIZATION
         iy = LL(j)*3
         IF ( j.NE.1 ) THEN
            zeff = j - 1
            rho = (Dene/zeff**7.)**.2
            dd = 1.
            IF ( rho.GE.3 ) dd = 1./(1.+(cd1(N-j+1)+cd2(N-j+1)/zeff)
     &                           *rho)
            DO 160 l = 1 , iy
               ln = ix + l
               plum = plum + ALPHADI(N,j,l,ln,T)*AMIN1(dd,sec(N-j+1))
 160        CONTINUE
C  DIELECTRONIC RECOMBINATION FOR METASTABLE LEVELS OF BE,B,C,MG ISO
            imet = imeta(N-j+1)
            IF ( imet.NE.0 ) THEN
               zf = j - 1
               b = SQRT(zf)*(zf+1.)**2.5/SQRT(zf*zf+13.4)
               plum = plum*(1.-fmet)
               plum = plum + fmet*DIMET(N,j,T,b,dd)
            ENDIF
C   *************** ADD DIELECTRONIC RECOMB AND STOREY'S LOW T DIELEC REC
            C2(j) = C2(j) + plum + ALFLO(N,j,T)
         ENDIF
         over(j) = plum/orange
         IF ( j.NE.1 ) c5(j) = plum*E(j-1)*10.**(ABUnd-1.)*1.6027
         ix = ix + iy
         orange = apple
 200  CONTINUE
      IF ( Icx.NE.0 ) THEN
C  CHARGE TRANSFER CONTRIBUTION
         fplus = RHY/(1.+RHY)
         denh = 1./(.003+fplus+abhe*HEPlus+abhe*2.*(1.-HENeut-HEPlus))
         C1(N+1) = 0.
         C2(1) = 0.
         nn = N + 1
C  **********************************  S. Butler Charge Transfer Rates
         DO 250 j = 1 , N
            C1(j) = C1(j) + CTHI(N,j,T)*denh*fplus + CTHEI(N,j,T,Dene)
     &              *HEPlus*abhe
            C2(j+1) = C2(j+1) + CTHR(N,j+1,T)*denh*(1.-fplus)
     &                + CTHER(N,j+1,T)*HENeut*abhe
 250     CONTINUE
      ENDIF
C  AUGER IONIZATION
      IF ( Iphot.EQ.2 ) CALL APHOT(N,Dene,Jcont)
      IF ( Jcont.EQ.1 ) GOTO 700
      DO 300 j = 1 , N
         C2(j+1) = ABS(C2(j+1))
         IF ( C1(j)/C2(j+1).LE.0. ) THEN
            rat(j) = 1.0E+6
         ELSE
            rat(j) = C2(j+1)/C1(j)
            IF ( j.EQ.1 .AND. T.GE.1.0E8 ) rat(j) = AMIN1(rat(j),1.0E4)
         ENDIF
         IF ( rat(j).LT.1.0E-5 ) jlow = j + 1
         IF ( rat(j).GE.1.0E+5 ) THEN
            jmax = j
            GOTO 400
         ENDIF
 300  CONTINUE
      IF ( jlow.EQ.jmax ) THEN
         sum = 1.0000
         GOTO 500
      ENDIF
 400  jump = jmax - 1
      prod(jump) = rat(jump)
      ktop = jump - jlow
      sum = 1.00000 + prod(jump)
      IF ( ktop.NE.0 ) THEN
         DO 450 k = 1 , ktop
            prod(jump-k) = rat(jump-k)*prod(jmax-k)
            prod(jump-k) = AMIN1(prod(jump-k),1.0E30)
            sum = sum + prod(jump-k)
 450     CONTINUE
      ENDIF
 500  DO 600 j = 1 , jtop
         CONce(j) = 0
         c5(j) = 0.
 600  CONTINUE
      CONce(jmax) = 1.000/sum
      kmax = jmax - jlow
      IF ( kmax.NE.0 ) THEN
         DO 650 k = 1 , kmax
            CONce(jmax-k) = prod(jmax-k)*CONce(jmax)
 650     CONTINUE
      ENDIF
 700  IF ( N.EQ.2 ) DNE = 0.
      worl = 0.
      dei = 0.
      DO 800 j = 1 , jtop
         DNE = (j-1)*CONce(j)*10.**(ABUnd-12.) + DNE
         c6(j) = c6(j)*CONce(j)*10.0**(11.+ABUnd)
         RE = RE + c6(j)
         c5(j) = c5(j)*CONce(j)
         PCOol = PCOol + c5(j)
         PCOol = PCOol + c6(j)
 800  CONTINUE
      RETURN
      END
