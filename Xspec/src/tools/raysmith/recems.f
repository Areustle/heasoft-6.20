**==recems.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE RECEMS(T,N,Iprint,Jprint)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ab , ABUnd , ABUnj , alfx , BINmin , binn , binnow , binsum , 
     &     BINsyz , BRMev , chi , chitry , CONce , DNE , drat , E , E3 , 
     &     e3x , EA , ebk
      REAL ECEN , edg , edge , edglim , edgtry , en , esmin3 , exprod , 
     &     F , fimin , gftr , GFUNC , gmax , gmxtry , GNDrec , HENeut , 
     &     HEPlus , PCOol , PM , POT
      REAL POU , POWer , q , qgrec , qgrecpr , qhyd , qhydpr , qprime , 
     &     qtk , qtkpr , RE , RECev , RHY , S2 , s2x , S3 , s3x , S4 , 
     &     s4x , S5
      REAL s5x , T , t6 , thresh , TU , TUF , WAVe , x , x2 , y , z , 
     &     zeta
      INTEGER i , ib , IBN , ie , iedge , iemax , iep , iesave , ignd , 
     &        imap , imax , imin , imsave , ip , ipnt , Iprint , iso , 
     &        ix , ixl , j
      INTEGER jb , jedge , jp , Jprint , k , l , lion , LL , ln , N , 
     &        NBIn , NJ
C*** End of declarations inserted by SPAG
      REAL kt
      LOGICAL notgrc(30) , done(30) , switch
 
      DIMENSION drat(20) , lion(2,20) , zeta(2,20) , iedge(31) , 
     &          edge(30) , chi(30) , esmin3(30) , alfx(30) , jedge(30) , 
     &          imap(30)
      DIMENSION s2x(30) , s3x(30) , s4x(30) , s5x(30)
 
      INCLUDE 'rayspec.inc'
 
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /CONTIN/ BRMev(MXBINS) , RECev(MXBINS) , TUF(MXBINS)
 
      DATA drat/2. , .5 , 2. , .5 , 6. , 2.5 , 2.22 , 2.25 , .667 , 
     &     .167 , 2. , .5 , 6. , 2.5 , 2.22 , 2.25 , .667 , .167 , 2. , 
     &     .5/
      DATA lion/0 , 0 , 3 , 7 , 1 , 5 , 2 , 4 , 4 , 0 , 4 , 0 , 5 , 0 , 
     &     2 , 0 , 2 , 0 , 3 , 0 , 1 , 5 , 1 , 4 , 3 , 5 , 1 , 3 , 1 , 
     &     4 , 1 , 3 , 1 , 3 , 1 , 2 , 4 , 0 , 6 , 0/
      DATA zeta/0. , 0. , 2. , 3. , 2. , 3. , 1.75 , 3. , 3. , 0. , 3. , 
     &     0. , 3. , 0. , 3. , 0. , 3. , 0. , 3. , 0. , 3. , 4. , 2.83 , 
     &     4. , 2.67 , 4. , 2.5 , 4. , 2.33 , 4. , 2.17 , 4. , 2. , 4. , 
     &     1.83 , 4. , 4. , 0. , 4. , 0./
 
      t6 = T/1.E6
      kt = 86.17*t6
C UPPER LIMIT TO BINNING IS CHOSEN WHERE EXP = 1.E-20
      imax = NBIn
      IF ( imax.LT.1 ) RETURN
      ab = 10.**(ABUnd-12.)
      ebk = EXP(BINsyz/kt)
      q = ab*kt*(ebk-1.)*EXP(-BINmin/kt)/t6**1.5
      qprime = ab*kt/t6**1.5
      qgrec = q*1.31E-8
      qtk = q*6.52/12399.
      z = N
      qhyd = q*5.28E-4*(z**4)
      qgrecpr = qprime*1.31E-8
      qtkpr = qprime*6.52/12399.
      qhydpr = qprime*5.28E-4*(z**4)
      ebk = 1./ebk
C LIMIT TO SIGNIFICANT CONTRIB FROM AN EDGE; ASSUMES #IONS PRESENT
C  GOES LIKE SQRT(Z); COMPARE THRESH RECEMS TO EDGLIM*(CURRENT CONTIN)
      edglim = 0.01/SQRT(z)
C FIRST CONSTRUCT TABLE OF SIGNIFICANT EDGES
      ix = 0
      j = 1
      iso = N
 100  IF ( iso.LE.20 ) THEN
         iemax = 0
 150     IF ( iso.EQ.1 ) THEN
            IF ( CONce(N+1).GE.1.E-6 ) THEN
               IF ( CONce(N+1).GE.1.E-4 .OR. N.EQ.2 ) THEN
                  DO 155 k = 1 , 2
                     y = k
                     chitry = 13.6*(z/y)**2
                     ignd = IBN(chitry)
                     IF ( ignd.LT.NBIn ) THEN
                        IF ( ignd.LT.1 ) ignd = 1
                        en = ECEN(ignd)
                        x = en/chitry
                        gftr = GFUNC(x,y)
                        IF ( x.GT.100. ) gftr = 7./SQRT(x)
                        gmxtry = 1.1 - 1./(10.*y**1.5)
                        IF ( gftr.GT.gmxtry ) gftr = gmxtry
c
                        IF ( chitry/kt.GT.46. ) RECev(ignd)
     &                       = RECev(ignd) + qhydpr*CONce(N+1)*gftr/y**3
                        IF ( chitry/kt.LE.46. ) THEN
c
                           edgtry = qhyd*CONce(N+1)*EXP(chitry/kt)/y**3
                           edg = edgtry*gftr*ebk**ignd
                           binnow = BRMev(ignd) + RECev(ignd)
                           IF ( edg.GE.edglim*binnow ) THEN
                              iemax = iemax + 1
                              iedge(iemax) = ignd
                              edge(iemax) = edgtry
                              chi(iemax) = chitry
                              jedge(iemax) = j
                              esmin3(iemax) = gmxtry
                              alfx(iemax) = y
                              notgrc(iemax) = .TRUE.
                              done(iemax) = .FALSE.
                              IF ( iemax.EQ.30 ) GOTO 200
                           ENDIF
                        ENDIF
                     ENDIF
 155              CONTINUE
               ENDIF
            ENDIF
            GOTO 300
         ELSE
            IF ( CONce(j+1).GE.1.E-4 ) THEN
               chitry = E(j)
               ignd = IBN(chitry)
               IF ( ignd.LT.NBIn ) THEN
                  IF ( ignd.LT.1 ) ignd = 1
                  thresh = S2(j) + S3(j) + S4(j) + S5(j)
c
                  IF ( chitry/kt.GT.46. ) RECev(ignd) = RECev(ignd)
     &                 + qgrecpr*CONce(j+1)*drat(iso)*(chitry**3)*thresh
                  IF ( chitry/kt.LE.46. ) THEN
c
                     edgtry = qgrec*CONce(j+1)*drat(iso)*(chitry**3)
     &                        *thresh*EXP(chitry/kt)
                     en = ECEN(ignd)
                     x = chitry/en
                     x2 = x*x
                     edg = edgtry*(ebk**ignd)
     &                     *(S2(j)/x+S3(j)+S4(j)*x+S5(j)*x2)/thresh
                     binnow = BRMev(ignd) + RECev(ignd)
                     IF ( edg.LT.edglim*binnow ) GOTO 180
                     iemax = iemax + 1
                     iedge(iemax) = ignd
                     edge(iemax) = edgtry
                     chi(iemax) = chitry
                     jedge(iemax) = j
                     notgrc(iemax) = .FALSE.
                     s2x(iemax) = S2(j)/thresh
                     s3x(iemax) = S3(j)/thresh
                     s4x(iemax) = S4(j)/thresh
                     s5x(iemax) = S5(j)/thresh
                     IF ( iemax.EQ.30 ) GOTO 200
                  ENDIF
               ENDIF
               ln = LL(j)*3
               IF ( ln.NE.0 ) THEN
                  DO 160 k = 1 , 2
                     l = lion(k,iso)
                     IF ( l.NE.0 .AND. l.LE.ln ) THEN
                        e3x = E3(ix+l)
                        IF ( e3x.GE.1. ) THEN
                           ixl = ix + l
                           chitry = E(j) - E3(ixl)
                           ignd = IBN(chitry)
                           IF ( ignd.LT.NBIn ) THEN
                              IF ( ignd.LT.1 ) ignd = 1
c
                              IF ( chitry/kt.GT.46. ) RECev(ignd)
     &                             = RECev(ignd) + qtkpr*CONce(j+1)
     &                             *zeta(k,iso)*((chitry/13.6)**2)
                              IF ( chitry/kt.LE.46. ) THEN
c
                                 edgtry = qtk*CONce(j+1)*zeta(k,iso)
     &                              *((chitry/13.6)**2)*EXP(chitry/kt)
                                 en = ECEN(ignd)
                                 edg = edgtry*ebk**ignd
                                 binnow = BRMev(ignd) + RECev(ignd)
                                 IF ( edg.GE.edglim*binnow ) THEN
                                    iemax = iemax + 1
                                    iedge(iemax) = ignd
                                    edge(iemax) = edgtry
                                    chi(iemax) = chitry
                                    jedge(iemax) = j
                                    notgrc(iemax) = .TRUE.
                                    done(iemax) = .TRUE.
                                    IF ( iemax.EQ.30 ) GOTO 200
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
 160              CONTINUE
               ENDIF
            ENDIF
 180        ix = ix + 3*LL(j)
            j = j + 1
            iso = iso - 1
            GOTO 150
         ENDIF
 200     PRINT 99001 , N
      ELSE
         ix = ix + 3*LL(j)
         iso = iso - 1
         j = j + 1
         GOTO 100
      ENDIF
 300  IF ( Iprint.GT.0 ) PRINT 99002 , N , T , iemax
      IF ( iemax.EQ.0 ) RETURN
C  BUBBLE SORT TO PUT IEDGE IN INCREASING ORDER; PARALLEL SORT OF IMAP
      DO 400 ie = 1 , 30
         imap(ie) = ie
 400  CONTINUE
 500  switch = .FALSE.
      iedge(iemax+1) = 0
      IF ( iemax.LT.30 ) iedge(iemax+2) = 0
      IF ( iemax.EQ.1 ) GOTO 700
      ie = 1
 600  iesave = iedge(ie)
      IF ( iedge(ie+1).LT.iesave ) THEN
         imsave = imap(ie)
         iedge(ie) = iedge(ie+1)
         imap(ie) = imap(ie+1)
         iedge(ie+1) = iesave
         imap(ie+1) = imsave
         IF ( ie.GT.1 ) switch = .TRUE.
      ENDIF
      ie = ie + 1
      IF ( ie.LT.iemax ) GOTO 600
      IF ( switch ) GOTO 500
C  END SORT, PRINT TABLE OF EDGES SIGNIFICANT ENOUGH TO BE INCLUDED
 700  IF ( Jprint.NE.0 ) THEN
         PRINT 99003
         iep = iemax/2 + 1
         DO 750 ie = 1 , iep
            jb = ie*2
            ib = jb - 1
            jp = imap(jb)
            ip = imap(ib)
            IF ( iedge(ib).NE.0 ) THEN
               IF ( iedge(jb).EQ.0 ) THEN
                  PRINT 99004 , jedge(ip) , chi(ip) , edge(ip) , 
     &                  notgrc(ip) , done(ip)
               ELSE
                  PRINT 99004 , jedge(ip) , chi(ip) , edge(ip) , 
     &                  notgrc(ip) , done(ip) , jedge(jp) , chi(jp) , 
     &                  edge(jp) , notgrc(jp) , done(jp)
               ENDIF
            ENDIF
 750     CONTINUE
      ENDIF
C  BEGIN BINNING, USING LIST IEDGE TO GOVERN NUMBER OF EDGES INCLUDED
      imin = iedge(1)
      iemax = 1
      fimin = imin - 1
      exprod = ebk**fimin
      DO 900 i = imin , NBIn
 800     IF ( i.NE.iedge(iemax+1) ) THEN
            en = ECEN(i)
            binsum = 0.
            DO 820 ie = 1 , iemax
               ipnt = imap(ie)
               binn = edge(ipnt)
               IF ( .NOT.(notgrc(ipnt)) ) THEN
                  x = chi(ipnt)/en
                  binn = binn*(s2x(ipnt)/x+s3x(ipnt)+s4x(ipnt)
     &                   *x+s5x(ipnt)*x*x)
               ELSEIF ( .NOT.(done(ipnt)) ) THEN
                  y = alfx(ipnt)
                  x = en/chi(ipnt)
                  gftr = 7./SQRT(x)
                  IF ( x.LT.100. ) gftr = GFUNC(x,y)
                  gmax = esmin3(ipnt)
                  IF ( gftr.GT.gmax ) gftr = gmax
                  binn = binn*gftr
               ENDIF
               binsum = binsum + binn
 820        CONTINUE
            exprod = exprod*ebk
            RECev(i) = RECev(i) + binsum*exprod
         ELSE
            iemax = iemax + 1
            GOTO 800
         ENDIF
 900  CONTINUE
      RETURN
99001 FORMAT (1X//' IEMAX OVERRUN ON ELEMENT N =',I3//)
99002 FORMAT (1X//' RECEMS FOR ELEMENT N =',I3,'  AT T =',1PE10.3,
     &        ',   WITH',I3,'  SIGNIF. EDGES'/)
99003 FORMAT (2(11X,'J     CHI',8X,'EDGE',7X,'N GRC   SHRT')/)
99004 FORMAT (2(10X,I2,0PF10.1,1PE12.2,2L8))
      END

      REAL FUNCTION ECEN(i)
      INTEGER i

      REAL ABUnj, ABUnd, BINmin, BINsyz
      INTEGER NJ, NBIn

      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn

      ECEN = BINmin + (i-0.5)*BINsyz
      RETURN
      END

      REAL FUNCTION GFUNC(x,y)
      REAL x,y

      GFUNC = 1. + 0.1728*(y*x)**(-2./3.)*(x-2.) - 0.0496*(y*x)
     &             **(-4./3.)*(x*x-x*2./3.+2./3.)
      RETURN
      END

