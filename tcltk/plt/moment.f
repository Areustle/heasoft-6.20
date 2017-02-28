      SUBROUTINE MOMENT(Iprnt, Y, Iery, Mxrow, Igroup, Iy0,
     :   Npts, Pmin, Pmax, Tot, Ier)
      INTEGER   Iprnt, Iery, Mxrow, Igroup, Iy0, Npts, Ier
      REAL      Pmin(*), Pmax(*), Y(*), Tot(14)
C---
C Computes moments of array Y.  Makes two passes through the data
C to generate accurate results.
C---
C Tot( 1)       O  Sum of 1           =Npts
C Tot( 2)       O  Average Y          =YBAR
C Tot( 3)       O  Sum of (Y-YBAR)**2 =(Npts-1)*VAR(Y)
C Tot( 4)       O  Sum of (Y-YBAR)**3 =(Npts-1)*SKEW(Y)
C Tot( 5)       O  Average X          =XBAR
C Tot( 6)       O  Sum of (X-XBAR)**2 =(Npts-1)*VAR(X)
C Tot( 7)       O  Sum of (X-XBAR)*(Y-YBAR)
C Tot( 8.. 9)   O  YMIN, YMAX
C Tot(10..13)   O  Sum of W, W*Y, W*Y*Y, W*Y*Y*Y
C Tot(14)       O  Sum of Y*XDEL (i.e., the integral)
C---
      INTEGER   MXDIM
      PARAMETER (MXDIM=2)
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      REAL      WEIGHT
C---
      CHARACTER cbuf*80
      DOUBLE PRECISION dyint, sumx, sumx2, sumxy
      DOUBLE PRECISION sumwy, sumwy2, sumwy3, sumw
      DOUBLE PRECISION sumy, sumy2, sumy3
      DOUBLE PRECISION dxbar, dybar, dwybar
      REAL      ym(6), xc(MXDIM)
      REAL      FRAC, xdel, TMP, XT, xm, ydel, YMAX, YMIN, xp
      REAL      YT, y2, y3, W
      INTEGER   I, ipxer, itmp, iyi, iyoff, K, N, ndim
C---
C
C Query the X error plotting status
      CALL PLTXCQ(Igroup, 1, ipxer)
C We use the X error to integrate, so make sure it will be returned.
      CALL PLTXCP(Igroup, 1, 1)
      DO I=1,6
         ym(I)=0.
      END DO
      N=0
      sumy =0.
      sumx =0.
      YMIN= 1.E37
      YMAX=-1.E37
      sumw  =0.
      sumwy =0.
      dyint =0.
      CALL PLTXCC(Y, 1, Igroup, xc, ndim, iyoff)
      IF ( ndim.LE.1 ) THEN
         ydel = 1.0
      ELSE
C First we check the y-delta which is only used if we have a 2D group.
         CALL PLTXCQ(Igroup, 2, itmp)
         CALL PLTXCP(Igroup, 2, 1)
         CALL PLTXCE(Y, 1, Igroup, 2, xm, xp)
         ydel = xp - xm
         CALL PLTXCP(Igroup, 2, itmp)
      END IF
      DO 150 I=1, Npts
         CALL PLTXCC(Y, i, igroup, xc, ndim, iyoff)
         IF ( xc(1).EQ.NO ) GOTO 150
         iyi = Iy0 + iyoff
         IF ( xc(1).LT.Pmin(1) .OR. xc(1).GT.Pmax(1) ) GOTO 150
C If ndim.LE.1 then xc(2), pmin(2), pmax(2) are all undefined, so
C make sure they are not evaluted.
         IF ( ndim.GT.1 ) THEN
            IF ( xc(2).LT.Pmin(2) .OR. xc(2).GT.Pmax(2) ) GOTO 150
         END IF
         W=WEIGHT(Y(iyi),Mxrow,Iery)
         IF(W.GT.0.) THEN
            N    =N+1
            sumy =sumy+Y(iyi)
            sumx =sumx+xc(1)
            YMIN =MIN(YMIN,Y(iyi))
            YMAX =MAX(YMAX,Y(iyi))
            sumw =sumw +W
            sumwy=sumwy+W*Y(iyi)
            CALL PLTXCE(Y, i, igroup, 1, xm, xp)
            xdel = xp - xm
            dyint=dyint+Y(iyi)*XDEL*ydel
         END IF
  150 CONTINUE
      IF ( N.LE.0 ) THEN
         CALL PTBUF('ERROR--MOMENT no data.',-1)
         Ier=2
         RETURN
      END IF
      dybar =sumy/N
      dxbar =sumx/N
      dwybar=sumwy/sumw
C---
      sumy2=0.
      sumy3=0.
      sumx2=0.
      sumxy=0.
      sumwy2=0.
      sumwy3=0.
      DO 190 I=1,Npts
         CALL PLTXCC(Y, i, igroup, xc, ndim, iyoff)
         IF ( xc(1).EQ.NO ) GOTO 190
         iyi = iy0 + iyoff
         IF ( xc(1).LT.Pmin(1) .OR. xc(1).GT.Pmax(1) ) GOTO 190
         IF ( ndim.GT.1 ) THEN
            IF ( xc(2).LT.Pmin(2) .OR. xc(2).GT.Pmax(2) ) GOTO 190
         END IF
         W=WEIGHT(Y(iyi),Mxrow,Iery)
         IF ( W.GT.0. ) THEN
            XT=xc(1)-dxbar
            YT=Y(iyi)-dybar
            Y2=YT*YT
            Y3=Y2*YT
            sumy2 =sumy2 +Y2
            sumy3 =sumy3 +Y3
            sumx2 =sumx2 +XT*XT
            sumxy =sumxy +XT*YT
            YT=Y(iyi)-dwybar
            Y2=YT*YT
            Y3=Y2*YT
            sumwy2=sumwy2+W*Y2
            sumwy3=sumwy3+W*Y3
         END IF
  190 CONTINUE
      Tot( 1) = N
      Tot( 2) = dybar
      Tot( 3) = sumy2
      Tot( 4) = sumy3
      Tot( 5) = dxbar
      Tot( 6) = sumx2
      Tot( 7) = sumxy
      Tot( 8) = YMIN
      Tot( 9) = YMAX
      Tot(10) = sumw
      Tot(11) = dwybar
      Tot(12) = sumwy2
      Tot(13) = sumwy3
      Tot(14) = dyint
      IF(N.LE.1) THEN
         CALL PTBUF('MOMENT--Not enough data.',-1)
         Ier=1
         RETURN
      END IF
      dybar=sumy/N
      FRAC=N/(N-1.)
      ym(1)=Tot(2)
      ym(2)=Tot(3)/(N-1.)
      ym(3)=Tot(4)/(N-1.)
      dwybar=sumwy/sumw
      ym(4)=Tot(11)
      ym(5)=FRAC*Tot(12)/sumw
      ym(6)=FRAC*Tot(13)/sumw
      IF(Iprnt.NE.0) THEN
         WRITE(cbuf,351)
  351    FORMAT('      \   YBAR',8X,'YVAR',8X,'Y3M',9X,'SUMW',8X,
     &       'YMIN',8X,'YMAX')
         CALL PTBUF(cbuf, -1)
         WRITE(cbuf,361) 'UNWTD',(ym(K),K=1,3),FLOAT(N),YMIN,YMAX
  361    FORMAT(A6,1P,4G12.4,2G12.4)
         CALL PTBUF(cbuf, -1)
         IF(Iery.NE.0) THEN
C If we have errors, print out the weighted values, plus CHI^2 which
C is effectively the weighted variance.
            WRITE(cbuf,361) '  WTD',(ym(K),K=4,6),sumw
            CALL PTBUF(cbuf, -1)
            IF(N.GT.1) THEN
               TMP=Tot(12)/(N-1.)
               IF(TMP.LT.9999.9) THEN
                  WRITE(cbuf,381) Tot(12),TMP
  381             FORMAT('  WCHI=',1PG11.3,',  WRED=',0PF9.3)
                  CALL PTBUF(cbuf, -1)
               ELSE
                  WRITE(cbuf,391) Tot(12),TMP
  391             FORMAT('  WCHI=',1PG11.3,',  WRED=',G11.3)
                  CALL PTBUF(cbuf, -1)
               END IF
            END IF
         END IF
      END IF
      CALL PLTXCP(Igroup, 1, ipxer)
      RETURN
      END
