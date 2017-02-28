C- PGPLOT.  Contains device dependent routines.
C- This version drives PGPLOT routines.
C*********
      SUBROUTINE PLTCLR
C---
C Clear workstation.
C---
      CALL PGADVANCE
      RETURN
      END
C*********
      SUBROUTINE PLTCS(SCHAR)
      REAL      SCHAR
C---
C Set character size.  Default size is 1.0.
C---
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      CALL PGSCH(SCHAR)
      SIZCHR=SCHAR
      RETURN
      END
C**********
      SUBROUTINE PLTFON(CBUF)
      CHARACTER CBUF*(*)
C---
C Set default font.  If CBUF(1:1) is not a legal font character,
C then display possible fonts on terminal.
C---
      CALL UPC(CBUF(1:1))
      IF(CBUF(1:1).EQ.'N') THEN
         CALL PGSCF(1)
      ELSE IF(CBUF(1:1).EQ.'R') THEN
         CALL PGSCF(2)
      ELSE IF(CBUF(1:1).EQ.'I') THEN
         CALL PGSCF(3)
      ELSE IF(CBUF(1:1).EQ.'S') THEN
         CALL PGSCF(4)
      ELSE
         WRITE(*,101)
  101   FORMAT(' Legal fonts are: Normal, Roman, Italic, Script')
        RETURN
      END IF
      RETURN
      END
C**********
      SUBROUTINE PLTHAR(CDEV)
      CHARACTER CDEV*(*)
C---
C Returns the name of a hardcopy device
C---
      INTEGER   LDEV
C---
      CALL TRLOG('PLT_HARDCOPY',12,CDEV,LDEV)
      IF(LDEV.EQ.0) CDEV='/PS'
      RETURN
      END
C*********
      SUBROUTINE PLTMAR
C---
C Based on PGEX8.  Show the user the standard PGPLOT marker types.
C---
      CHARACTER LABEL*2
      REAL      xzero(1), yzero(1)
      REAL      X, X1, X2, XOFF, Y, Y1, Y2, YOFF, DX, DY
      INTEGER   NX, NY, N, IX, JY, icisav
      DATA xzero,yzero/0.0,0.0/
C---
      CALL PGQCI(icisav)
      CALL PGVPORT(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
C
      NX = 8
      NY = 4
      DX = MIN(X/NX, 0.95*Y/NY)
      DY = DX
      IX = NX
      JY = 1
      XOFF = X1 + (X-NX*DX)*0.5
      YOFF = Y1 + (0.95*Y-NY*DY)*0.5
      CALL PGBBUF
      CALL PGSCI(1)
C
C Each symbol will be drawn in a standard window; the window is moved
C by manipulating the viewport.
C
      CALL PGWINDOW(-1.,1.,-1.,1.)
C
C Loop through all known symbols (N=0-31).
C
      DO N=0,31
          WRITE (LABEL,'(I2)') N
C
C Define window and viewport. The loop allows the plot to extend over
C more than one page if necessary; each page is labelled at the top.
C
          IX = IX+1
          IF (IX.GT.NX) THEN
            IX = 1
            JY = JY-1
          END IF
          IF (JY.LT.1) THEN
            JY = NY
            CALL PLTCLR
            CALL PGSCH(1.2)
            CALL PGVSIZE(XOFF, XOFF+NX*DX, YOFF, YOFF+NY*DY)
            CALL PGMTEXT('T', 1.0, 0.5, 0.5, 'PGPLOT Marker Symbols')
          END IF
          CALL PGVSIZE(XOFF+(IX-1)*DX, XOFF+IX*DX,
     1                 YOFF+(JY-1)*DY, YOFF+JY*DY)
C
C Call PGBOX to draw a box and PGMTEXT to label it.
C
          CALL PGBOX('BC',10.0,0,'BC',10.0,0)
          CALL PGSCH(1.0)
          CALL PGMTEXT('T',-1.5,0.05,0.0,LABEL)
C
C Call PGPOINT to draw the symbol.
C
          CALL PGSCH(1.5)
C NAG compiler likes args 2 and 3 to be consistent, i.e., an array.
          CALL PGPOINT(1,xzero,yzero,N)
      END DO
C
      CALL PGEBUF
      CALL PGSCI(icisav)
      RETURN
      END
C*********
      SUBROUTINE PLTOPE(Cdev, Ibcol, Scrcol, Cfont, Pgpapw, Pgpapa, Ier)
      CHARACTER Cdev*(*), Cfont*(*)
      INTEGER   Ibcol, Ier
      REAL      Scrcol(3,0:15), Pgpapw, Pgpapa
C---
C Open workstation.
C---
C Cdev    I
C Ibcol   I
C Scrcol  I
C Cfont   I
C Pgpapw  I
C Pgpapa  I
C Ier       O
C---
      REAL       NO
      PARAMETER (NO = -1.2E-34)
      INTEGER PGBEG
C
      INTEGER   i
C---
      IF(PGBEG(0,Cdev,1,1).NE.1) GOTO 900
      CALL PGASK(.FALSE.)
      IF(CFONT.NE.' ') CALL PLTFON(CFONT)
      IF ( PGPAPW.GT.0.0 ) THEN
         CALL PGPAP(Pgpapw/2.54,Pgpapa)
      END IF
      CALL PLTOP1(ibcol)
      DO i=0,15
         IF ( Scrcol(1,i).NE.NO ) THEN
            CALL PGSCR(i,Scrcol(1,i),Scrcol(2,i),Scrcol(3,i))
         END IF
      END DO
      Ier = 0
      RETURN
C---
  900 IER=1
      RETURN
      END
C*********
      SUBROUTINE PLTOP1(ibcol)
      INTEGER   ibcol
C---
C This routine should only be called if plot device is open.
C Allows user to specify black or white to be the default background color.
C---
C ibcol I    =0 ignore, <0 black background, >0 white background
C---
      IF ( ibcol.LT.0 ) THEN
C Force black background
         CALL PGSCR(0,0.0,0.0,0.0)
         CALL PGSCR(1,1.0,1.0,1.0)
         CALL PGUPDT
      ELSE IF ( ibcol.GT.0 ) THEN
C Force white background
         CALL PGSCR(0,1.0,1.0,1.0)
         CALL PGSCR(1,0.0,0.0,0.0)
         CALL PGUPDT
      END IF
      RETURN
      END
C*********
      SUBROUTINE PLTPM(NUM, XRAY, YRAY)
      INTEGER   NUM
      REAL      XRAY(1),YRAY(1)
C---
C Plot Polymarker.
C---
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      CALL PGPOINT(NUM,XRAY,YRAY,MARK)
      RETURN
      END
C*********
      SUBROUTINE PLTPRO(CPFILE, IER)
      CHARACTER CPFILE*(*)
      INTEGER   IER, LPFILE
C---
C Prompt for PGPLOT device name.
C---
  100 CALL GTBUF('PGPLOT file/type:',IER)
      IF(IER.LT.0) GOTO 900
      CALL GTREST(CPFILE, LPFILE)
      IF(CPFILE(1:1).EQ.'?') THEN
         CALL PGLDEV
         GOTO 100
      END IF
      IER=0
      RETURN
C---
  900 IER=-1
      RETURN
      END
C*********
      SUBROUTINE PLTSCI(ICOL)
      INTEGER   ICOL
C---
C Set color index.  0=background, 1=foreground
C---
      IF(ICOL.LT.0) THEN
         WRITE(*,101)
  101    FORMAT(' PGPLOT colors are:'/
     :    '  0=Backg,     1=Foreg,       2=Red,         3=Green,'/
     :    '  4=Blue,      5=Light blue,  6=Magenta,     7=Yellow,'/
     :    '  8=Orange,    9=Yel.+Green, 10=Green+Cyan, 11=Blue+Cyan,'/
     :    ' 12=Blue+Mag, 13=Red+Mag,    14=Dark Grey,  15=Light Grey')
         RETURN
      ELSE
         CALL PGSCI(ICOL)
      END IF
      RETURN
      END
C*********
      SUBROUTINE PLTSLS(LS)
      INTEGER   LS
C---
C Set line style.
C---
      INTEGER   ITMP
C---
      IF(LS.LT.0) THEN
         WRITE(*,101)
  101    FORMAT(' PGPLOT line styles are:'/
     :    ' 1=Solid, 2=Dash, 3=Dash-dot, 4=Dot, 5=Dash-dot-dot-dot')
      ELSE
         ITMP=MOD(LS-1,5)+1
         CALL PGSLS(ITMP)
      END IF
      RETURN
      END
C*********
      SUBROUTINE PLTSLW(WIDTH)
      REAL      WIDTH
C---
C Set line width.
C---
      CALL PGSLW(NINT(WIDTH))
      RETURN
      END
C*********
      SUBROUTINE PLTSMK(IMARK)
      INTEGER   IMARK
C---
C Set marker type.
C---
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      MARK=IMARK
      RETURN
      END
C*********
      SUBROUTINE PLTTEX(X1, Y1, ANGLE, IJUS, LOC, CTEXT)
      REAL      X1, Y1, ANGLE
      INTEGER   IJUS, LOC
      CHARACTER CTEXT*(*)
C---
C X1, Y1    I    Location of text in window coordinates
C ANGLE     I    Angle of text in degrees from X-axis
C IJUS      I    =1 X1,Y1 is left of string
C                =2 X1,Y1 is in center of string
C                =3 X1,Y1 is right of string
C LOC       I    =1 X1,Y1 is at Top of text.
C                =2 X1,Y1 is at Cap of text.
C                =3 X1,Y1 is at Half of text.
C                =4 X1,Y1 is at Base of text.
C                =5 X1,Y1 is at Bottom of text.
C CTEXT     I    The text string
C---
      REAL      FJUST(3), VOFF(5), YC
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
      DATA FJUST/0.0,0.5,1.0/
      DATA VOFF/-0.020,-0.012,-0.008,0,+.006/
C---
      YC=Y1+VOFF(LOC)*SIZCHR*(WYMAX-WYMIN)/(VYMAX-VYMIN)
      CALL PGPTEXT(X1,YC,ANGLE,FJUST(IJUS),CTEXT)
      RETURN
      END
C*********
      SUBROUTINE PLTVTW(VX, VY, WX, WY)
      REAL      VX, VY, WX, WY
C---
C Convert viewport coordinates to window coordinates
C---
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      WX=WXMIN+(VX-VXMIN)*(WXMAX-WXMIN)/(VXMAX-VXMIN)
      WY=WYMIN+(VY-VYMIN)*(WYMAX-WYMIN)/(VYMAX-VYMIN)
      RETURN
      END
C*********
      SUBROUTINE PLTWTV(WX, WY, VX, VY)
      REAL      WX, WY, VX, VY
C---
C Convert window coordinates to viewport coordinates
C---
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      VX=VXMIN+(WX-WXMIN)*(VXMAX-VXMIN)/(WXMAX-WXMIN)
      VY=VYMIN+(WY-WYMIN)*(VYMAX-VYMIN)/(WYMAX-WYMIN)
      RETURN
      END
C*********
      SUBROUTINE PLTSVW(BOXVP, WINLOC, XYSCAL, LOGX, LOGY, IADJ, IWNUM)
      REAL      BOXVP(4,*), WINLOC(4,*), XYSCAL(4,*)
      INTEGER   IWNUM, LOGX(*), LOGY(*), IADJ(*)
C---
C Set both viewport and window scale for the current window number.  If
C a window adjustment occurs, then on return BOXVP reflects the new value.
C If IWNUM=0 then the full screen is used and window adjustment is ignored.
C---
C BOXVP   I/O  Size of box in viewport coordinates
C WINLOC  I    Location of selected window in device independent coordinates
C XYSCAL  I    World coordinates for box
C LOGX    I    <>0 for Log scale on X-axis
C LOGY    I    <>0 for Log scale on Y-axis
C IADJ    I    <>0 to call PGWNAD
C IWNUM   I    Window number
C---
C- The minimum number that can be LOG'ed.
      REAL      RMNLOG
      PARAMETER (RMNLOG=1.E-36)
C
      REAL      TMP
C
      REAL      VXMIN,VXMAX,VYMIN,VYMAX
      REAL      WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR
      INTEGER   MARK
      COMMON /PLTCMN/ VXMIN,VXMAX,VYMIN,VYMAX,
     :   WXMIN,WXMAX,WYMIN,WYMAX,SIZCHR,MARK
C---
      IF(IWNUM.LE.0) THEN
         VXMIN=0.
         VXMAX=1.
         VYMIN=0.
         VYMAX=1.
      ELSE
         TMP=WINLOC(3,IWNUM)-WINLOC(1,IWNUM)
         VXMIN=WINLOC(1,IWNUM)+TMP*BOXVP(1,IWNUM)
         VXMAX=WINLOC(1,IWNUM)+TMP*BOXVP(3,IWNUM)
         TMP=WINLOC(4,IWNUM)-WINLOC(2,IWNUM)
         VYMIN=WINLOC(2,IWNUM)+TMP*BOXVP(2,IWNUM)
         VYMAX=WINLOC(2,IWNUM)+TMP*BOXVP(4,IWNUM)
      END IF
      CALL PGVPORT( VXMIN, VXMAX, VYMIN, VYMAX)
C---
      IF(IWNUM.LE.0) THEN
         WXMIN=0.
         WXMAX=1.
         WYMIN=0.
         WYMAX=1.
      ELSE
         WXMIN=XYSCAL(1,IWNUM)
         WYMIN=XYSCAL(2,IWNUM)
         WXMAX=XYSCAL(3,IWNUM)
         WYMAX=XYSCAL(4,IWNUM)
         IF(LOGX(IWNUM).NE.0) THEN
            WXMIN=LOG10(MAX(WXMIN,RMNLOG))
            WXMAX=LOG10(MAX(WXMAX,RMNLOG))
         END IF
         IF(LOGY(IWNUM).NE.0) THEN
            WYMIN=LOG10(MAX(WYMIN,RMNLOG))
            WYMAX=LOG10(MAX(WYMAX,RMNLOG))
         END IF
      END IF
      IF((WXMIN.NE.WXMAX) .AND. (WYMIN.NE.WYMAX)) THEN
         IF ( iwnum.EQ.0 .OR. Iadj(iwnum).EQ.0 ) THEN
            CALL PGWINDOW(WXMIN,WXMAX,WYMIN,WYMAX)
         ELSE
C Do window adjust, and reset BOXVP to be correct value.
            CALL PGWNAD(WXMIN,WXMAX,WYMIN,WYMAX)
            CALL PGQVP(0, VXMIN, VXMAX, VYMIN, VYMAX)
            TMP=WINLOC(3,IWNUM)-WINLOC(1,IWNUM)
            BOXVP(1,IWNUM) = (VXMIN-WINLOC(1,IWNUM))/TMP
            BOXVP(3,IWNUM) = (VXMAX-WINLOC(1,IWNUM))/TMP
            TMP=WINLOC(4,IWNUM)-WINLOC(2,IWNUM)
            BOXVP(2,IWNUM) = (VYMIN-WINLOC(2,IWNUM))/TMP
            BOXVP(4,IWNUM) = (VYMAX-WINLOC(2,IWNUM))/TMP
         END IF
      ELSE
         WRITE(*,*) 'ERROR, Bad window size',WXMIN,WXMAX,WYMIN,WYMAX
         WRITE(*,*) '- IWNUM=',IWNUM
      END IF
      RETURN
      END
