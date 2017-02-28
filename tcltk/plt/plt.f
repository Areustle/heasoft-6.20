      SUBROUTINE PLTVER(Cvers, Lvers)
      CHARACTER Cvers*(*)
      INTEGER   Lvers
C---
C Return the current PLT version number.
C---
C Cvers     O  The version number
C Lvers     O  Number of valid characters in cvers
C---
C 1998-05-28 - extracted from PLT - [AFT]
C---
      Cvers = '2015-06-12'
      Lvers = 10
      END
C*********
      SUBROUTINE PLT(Yray, Iery, Mxrow, Npts, Nvec, Cmd, Ncmd, Ier)
      IMPLICIT NONE
      REAL      Yray(*)
      INTEGER   Iery(*), Mxrow, Npts, Nvec, Ncmd, Ier
      CHARACTER Cmd(*)*(*)
C---
C General plot subroutine.
C---
C Yray(*) I    The data array.  The array should be dimensioned
C              Y(Mxrow,MXCOL) where Mxrow and MXCOL are the actual
C              sizes of the arrays in the calling program.
C              MXCOL = Nvec+NSERR+2*NTERR  where NSERR is the number
C              of vectors that have symmetric errors and NTERR
C              is the number of vectors that have two-sided errors.
C Iery(*) I    = -1 assume errors are SQRT(Y)
C              = 0 no errors.
C              = +1 explicit symmetric errors.
C              = +2 for two-sided errors
C Mxrow   I    The actual first dimension of the Y array.
C Npts    I    The number of points to plot (Npts< = Mxrow).
C Nvec    I    The number of vectors to be plotted.
C Cmd(*)  I    Array of commands.
C Ncmd    I    Number of commands.
C Ier       O  Error flag, = -1 if user entered EOF, = 0 otherwise.
C---
C [AFT]
C---
C- The NO data flag.
      REAL       NO
      PARAMETER (NO = -1.2E-34)
C- Only allow up to two dimensional data.
      INTEGER   MXDIM
      PARAMETER (MXDIM=2)
C- The minimum number that can be LOG'ed.
      REAL      RMNLOG
      PARAMETER (RMNLOG = 1.E-36)
C- Max number of model parameters.
      INTEGER   MXPAR
      PARAMETER (MXPAR = 120)
C- Number of independent plot groups (colors).
C- NOTE, pltxc.f must also define MXGRP to be equal to this value.
      INTEGER   MXGRP
      PARAMETER (MXGRP = 500)
C- Max number of numbered labels.
      INTEGER   MXLAB
      PARAMETER (MXLAB = 600)
C- Max number of simultaneous models that can be plotted
      INTEGER   MXMOD
      PARAMETER (MXMOD = 4)
C- Max number of 2D (contour) plots.  Must be less than MXGRP.
      INTEGER   MX2D
      PARAMETER (MX2D = 10)
C- Max number of contour levels
      INTEGER   MXLEV
      PARAMETER (MXLEV = 32)
C- Max number of windows
      INTEGER   MXWIN
      PARAMETER (MXWIN = 40)
C Derived value
      INTEGER    MXVAL
      PARAMETER  (MXVAL=MXPAR*MXMOD)
C
      CHARACTER CPARM*2
      REAL      FPNUM, FNFIT
      INTEGER   IAND, IFGRP, IOFSET, IOR, ISNUM, LENACT
C
C*** For 2D plots
      CHARACTER cctnam(MX2D)*128
      REAL      rlvcon(MXLEV,MX2D), rlwcon(MXLEV,MX2D)
      REAL      tr(6), zscale(2,MX2D), rota(MX2D)
      INTEGER   icont(MX2D), image(MX2D)
      INTEGER   icocon(MXLEV,MX2D),ilscon(MXLEV,0:MX2D),isuba(4,0:MX2D)
      INTEGER   lctnam(MX2D), icbar(MX2D), itfun(MX2D)
C*** For each group
      CHARACTER cglab(MXGRP)*80
      REAL      flimit(MXGRP), szmark(MXGRP)
      REAL      widlin(MXGRP)
C The actual data MIN and MAX values
      REAL      xymnmx(4,MXGRP)
C Data MIN and MAX values including errors.
      REAL      ermnmx(4, MXGRP)
      INTEGER   icol(MXGRP), igrpos(3,MXGRP), imark(MXGRP)
      INTEGER   ipmark(MXGRP)
      INTEGER   ipyer(MXGRP), ipwin(MXGRP), ispecg(MXGRP)
      INTEGER   line(MXGRP), lsty(MXGRP)
      INTEGER   ipmod(MXGRP)
C*** For each numbered label
      CHARACTER CLABEL(MXLAB)*80
      REAL      FLABEL(7,MXLAB)
      INTEGER   ILABEL(7,MXLAB)
C*** For each model
      REAL      PVAL(MXPAR,MXMOD), PLIM(3,MXPAR,MXMOD)
      INTEGER   ICOMP(2*MXPAR,MXMOD), nterm(MXMOD)
      SAVE      PVAL, PLIM, ICOMP, nterm
      INTEGER   ifitg(MXMOD)
C*** For each window
      CHARACTER CTLAB(MXWIN)*80,  CXLAB(MXWIN)*80,  CYLAB(MXWIN)*80
      CHARACTER COTLAB(MXWIN)*80, COXLAB(MXWIN)*80, COYLAB(MXWIN)*80
      CHARACTER CFNAM(MXWIN)*80
      CHARACTER CXOPT(MXWIN)*10, CYOPT(MXWIN)*10
      REAL      BOXVP(4,MXWIN), WINLOC(4,MXWIN), XYSCAL(4,MXWIN)
      REAL      GRIDX(MXWIN), GRIDY(MXWIN)
      INTEGER   imaster(2,MXWIN)
      INTEGER   iactw(MXWIN), IWADJ(MXWIN)
      INTEGER   LOGX(MXWIN), LOGY(MXWIN), NSUBX(MXWIN), NSUBY(MXWIN)
C---
      CHARACTER ctmp*255, ctok*128, CSCR1*128, CSCR2*128
      CHARACTER CRLAB*80
      CHARACTER CFILE*72, CHLIB*128
      CHARACTER CHARD*256, cpfile*256, CPSAV*256
      CHARACTER CPROM*16
      CHARACTER CDISK*256, CDIR*12, CEXT*12
      INTEGER LCDISK
      CHARACTER CXOPT1*10, CYOPT1*10, CFONT*8
      DOUBLE PRECISION dsum
      REAL      TOT(16)
C-
C For now only allow 2D arrays
      REAL      pmin(2), pmax(2), XT(2), X1(2)
C For SCR
      REAL      scrcol(3,0:15)
C-
      REAL      RED, GRN, BLU
      REAL      CA, COR, CSIZE, DEM, FCOL, Fnew
      REAL      OFF, PGPAPA, PGPAPW, PYLAB
      REAL      RGAP, RMARK, RTD, SA, SLOP
      REAL      TMP, TMP1, TMP2, WIDTH
      REAL      TXMAX, TXMIN, TYMAX, TYMIN
      REAL      XLAS, XHI, xtmp, XLO
      REAL      XCEN, XDEL, XH, XL, xmerr, xperr, XRANGE, XSPAC
      REAL      YCEN, YDEL, YH, YL, YMERR, YPERR, YRANGE, YSPAC, YT
      REAL      YOFF, YSLOP, YLONG
      REAL      VIEWX, VIEWY, VXLAS, VYLAS
      REAL      WINX, WINY, WXMIN, WXMAX, WYMIN, WYMAX
C-
      INTEGER   I, I1, I2, i2dind, i2drow, IBAD, ibcol, IC
      INTEGER   ic2dg, iclab, iclear, icmd, ICMOD, ICNT, icsub
      INTEGER   icwin, icyvec, idoall, idoend, IGAP
      INTEGER   idolin, IDOER, IDOWIN, IDOX, IDOY
      INTEGER   IE, IFIRST, iftg, ig, igroup, IGCOL, IGLS
      INTEGER   ILO, IHI, im, imnmx, imnum, IMX, IND, INITAK
      INTEGER   ioff, ION, IOPEN, IOS, ipall, IPLAB, IPZERO
      INTEGER   IS, iscr, ISIGN, iskip, ISTART, ITIME, itmp, ITRANS
      INTEGER   iwnum, ixvec, iy0, iyi, iyoff, iystep
      INTEGER   J1, J2, K, LERX, lery, LFILE, LOOP
      INTEGER   LPFILE, LPROM, LSCR1, LSCR2, ltmp, ltok, LUN
      INTEGER   NASUB, NDIG, ndim, nfpl, new, newmod, ngroup
      INTEGER   NMAX, NROW, NSTOP, NVERT, nxstep, nystep, LASWIN
      LOGICAL   QALL,QHARD
C
      DATA PVAL/MXVAL*1./
      DATA RTD/57.29577951/
C The directory where the help libraries are located
C     DATA CDISK/'$XANTOP'/, CDIR/'doc'/
      DATA CDIR/'.'/
C LHEA_HELP points directly to the directory containing the plt
C help file, so "CDIR" is "."
C LEB 5/7/97

C going to try to allow environment variables to determine
C CHLIB. will try XANBIN first and if that's not set we'll
C use FTOOLS; the idea is for this to work for both unix
C and vms
C    jdd 1-22-97

      CALL TRLOG('LHEA_HELP', 9, CDISK, LCDISK)

C Test to see that we got something back -- jdd
      IF ( LCDISK .EQ. 0 ) THEN
         WRITE (*,*) 'ERROR in plt/plt.f:'
         WRITE (*,*) '   Environment variable $LHEA_HELP not defined.'
      END IF
C---
   11 FORMAT(A)
C Turn of GTBUF's standalone processing, we want $ and @ to work
      CALL GTBUFNOTSTAND
C---
C- No errors (yet!)
      Ier = 0
C- Create help library name
      CHLIB = 'plt.hlp'
      CALL PTEND(CDISK, CDIR, CHLIB)
C---
C- Set the XPARSE default extension
      CALL XGETIN(CEXT)
      CALL XSETIN('pco')
C---
C- Default is one group per vector
      ngroup = MIN(Nvec,MXGRP-MXMOD)
      CALL PLTXCI(Mxrow)
C- Default Gap = 2.5%, IGAP=0 don't include errors.
      RGAP = 0.025
      IGAP = 0
C- Paper size = 0.0 means use PGPLOT default
      PGPAPW = 0.0
      PGPAPA = 1.0
C- No active 2D subarrays.
      NASUB = 0
C- The current 2D group has not been defined yet.  This is the default
C- group for both CONtour and IMAge commands.
      ic2dg = 0
      DO ic = 1, MX2D
C- No rotation
         rota(ic)=0.0
C- No contour and/or image plots
         icont(ic) = 0
         image(ic) = 0
         lctnam(ic) = 0
C- linear image transform function
         itfun(ic) = 0
C No color bar
         icbar(ic) = 0
C- Default contour settings
         DO I = 1, MXLEV
            rlvcon(I,ic) = NO
            icocon(I,ic) = I
            ilscon(I,ic) = 1
            rlwcon(I,ic) = 1.
            zscale(1,ic) = 0.
            zscale(2,ic) = 1.
         END DO
      END DO
C Special solid line style for default model contours.
      DO I=1, MXLEV
         ilscon(I,0) = 1
      END DO
C
C Scan error array for largest possible contour plot.  Number of rows is
C easy.  The problem is to find the largest group of adjacent columns
C that all have the same error.  Isuba(2,x) and Isuba(4,x) is the
C number of the vector (not the column),
      isuba(1,0) = 1
      isuba(3,0) = Npts
      itmp = Iery(1)
      ICNT = 1
      IMX = 0
      ISTART = 1
      isuba(2,0) = 1
      isuba(4,0) = Nvec
      DO I = 2,Nvec
         IF ( Iery(I).EQ.itmp ) THEN
            ICNT = ICNT + 1
            IF ( ICNT.GT.IMX ) THEN
               isuba(2,0) = ISTART
               isuba(4,0) = I
               IMX = ICNT
            END IF
         ELSE
            ICNT = 0
            itmp = Iery(I)
            ISTART = I
         END IF
      END DO
C- Commands like R X affect all the windows.
      idoall = 1
C- Set Skip flag to zero.  Can skip either Single, Double, or Off.
      iskip = 0
C- Have not tried to translate PGPLOT_TYPE logical name
      ITRANS = 0
C- LASWIN<>0 if Plot Vert is active
      LASWIN = 0
      CALL PGQINF('STATE', ctok, ltok)
      IF(ctok(1:4).EQ.'OPEN') THEN
C- Plot device open, leave it open when PLT exits.
         IOPEN = 1
         idoend = 0
         CALL PGASK(.FALSE.)
         CALL PGQINF('DEV/TYPE', cpfile, LPFILE)
      ELSE
C- Plot device closed, do pgend when PLT exits.
         IOPEN = 0
         idoend = 1
         cpfile = ' '
         LPFILE = 0
      END IF
C- Default font
      CFONT = ' '
C- Don't plot vectors that have color index zero.
      IPZERO = 0
C- Default to background color of white.
      ibcol = 0
C- Set imnmx = iskip to indicate that data MIN and MAX values have been
C- set for that value of iskip.
      imnmx = -1
C- BIT0<>0 plot labels, BIT1<>0 plot parm labels
      IPLAB = 3
C- Grid color and lstyle
      IGCOL = 1
      IGLS = 1
C- Position of the Y label
      PYLAB = 2.0
C- Do not plot NO data values.
      QALL = .FALSE.
C- Default character size
      CSIZE = 1.0
C- Default line width
      WIDTH = 1.0
C- Time stamp on
      ITIME = 1
C- Current window is number 1
      icwin = 1
C- All windows fill the screen
      DO I = 1,MXWIN
C Assume most windows are inactive.  Note, iactw must be updated every
C time the user does something that might change the number of windows
C being plotted, such as COlor ON|OFF, YAXA, IMAG, CONT, PLOT V, PLOT O.
         iactw(I) = 0
C Default viewport
         BOXVP(1,I) = 0.1
         BOXVP(2,I) = 0.1
         BOXVP(3,I) = 0.9
         BOXVP(4,I) = 0.9
C- Init number of major and minor tic marks.  Zero means use default.
         GRIDX(I) = 0.
         GRIDY(I) = 0.
         NSUBX(I) = 0
         NSUBY(I) = 0
C- No window adjust
         IWADJ(I) = 0
C- Title
         CTLAB(I) = ' '
         COTLAB(I) = ' '
C- File
         CFNAM(I) = ' '
C- Label X-axis tic marks, CXOPT(6:6) = 'L'|' ', CXOPT(7:7) = 'G'|' '
         CXOPT(I) = 'BCSTN'
         CXLAB(I) = ' '
         COXLAB(I) = ' '
C- Label Y-axis tic marks, CYOPY(8:8) = 'V'|' '
         CYOPT(I) = 'BCSTN'
         CYLAB(I) = ' '
         COYLAB(I) = ' '
C- linear X and Y
         LOGX(I) = 0
         LOGY(I) = 0
C- All windows fill the screen
         WINLOC(1,I) = 0.
         WINLOC(2,I) = 0
         WINLOC(3,I) = 1.
         WINLOC(4,I) = 1.
         XYSCAL(1,I) = NO
         XYSCAL(2,I) = NO
         XYSCAL(3,I) = NO
         XYSCAL(4,I) = NO
C- No master/slave windows (yet!).  =0 independent, =-1 master, >0 owner
         imaster(1,i) = 0
         imaster(2,i) = 0
      END DO
C Default first window is active.
      iactw(1) = 1
C---
C--- The label options are (where i = group number):
C ILABEL(1,i)   =0 inactive, =n (n>0) window n coords, =-1 viewport
C ILABEL(2,i)   1:3 Justification (Left, center, right)
C ILABEL(3,i)   1,3,5 Center (Base, Center, Top)
C ILABEL(4,i)   0:15 Color
C ILABEL(5,i)   1:5  line style.
C ILABEL(6,i)   -1:30 Marker type (-1 = no marker)
C ILABEL(7,i)   =0 normal, =1 FLABEL(5,i) FLABEL(6,i) specify line endpoint
C---
C FLABEL(1,i)   Character X-position (default = 0.0)
C FLABEL(2,i)   Character Y-position (default = 0.0)
C FLABEL(3,i)   Rotatation angle of label (default = 0.0)
C FLABEL(4,i)   Character size (default = 1.0)
C FLABEL(5,i)   line angle
C FLABEL(6,i)   line length in viewport units (default = 0.0 no line)
C FLABEL(7,i)   Marker size
C---
      DO i=1,MXLAB
         ILABEL(1,i) = 0
         ILABEL(2,i) = 2
         ILABEL(3,i) = 3
         ILABEL(4,i) = 1
         ILABEL(5,i) = 1
         ILABEL(6,i) = -1
         ILABEL(7,i) = 0
         FLABEL(1,i) = 0.0
         FLABEL(2,i) = 0.0
         FLABEL(3,i) = 0.0
         FLABEL(4,i) = 1.0
         FLABEL(6,i) = 0.0
         FLABEL(7,i) = 1.0
      END DO
C- The following does a virtual "Xaxis 1".
      ixvec = 1
C-
      DO ig=1,MXGRP
C- Zero out the plot window array
         ipwin(ig) = 0
C- No group labels
         cglab(ig) = ' '
C- Default colors (PGPLOT only defines color indices up to 15).
         IF ( ig.LE.15 ) THEN
            icol(ig) = ig
         ELSE
            icol(ig) = 1
         END IF
C- Don't plot markers, but if you do, default marker type is 2
         ipmark(ig) = 0
         imark(ig) = 2
C- line =0 line off, =1 line on, =-1 Stepped line plot, >1 smooth line.
         line(ig) = 0
C- Solid line
         lsty(ig) = 1
C- No upper limits
         flimit(ig) = 0.0
C- Default marker size
         szmark(ig) = 1.0
C- Use global line width
         widlin(ig) = 0.0
C- No model has been defined
         ipmod(ig) = 0
C- Standard groups
         ispecg(ig) = 0
C- Default to all groups being undefined.
         igrpos(1,ig) = -1
      END DO
      newmod = 0
C-
      DO ig=1,ngroup
C- Plot all active groups in the first window.
         ipwin(ig) = 1
C The default X coordinate is the first vector in the array hence 1D.
         CALL PLTXCN(ig, 1, 0)
         itmp = iofset(ixvec, Iery, ixvec, Mxrow)
         CALL PLTXCG(ig, 1, ixvec, itmp, Iery)
C igrpos contains info on how to find plot groups in the Y array.
C igrpos(1,ig) is location of the zeroth point of the group
C igrpos(2,ig) is the number of points in the group
C igrpos(3,ig) is the vector number from which the group was created
         igrpos(1,ig) = IOFSET(ig, Iery, Nvec, Mxrow)
         igrpos(2,ig) = Npts
         igrpos(3,ig) = ig
C- Plot errors, = -1 Root, = 0 no errors, = 1 bars, = 2 diamonds
         ipyer(ig) = MIN(Iery(ig),1)
      END DO
C- The previous loop plots all groups, turn off the xaxis group.
      ipwin(ixvec) = -ABS(ipwin(ixvec))
C- Make sure no models are defined.
      DO i = 1,MXMOD
         nterm(i) = 0
         ifitg(i) = 0
C The last MXMOD groups are hardwired to plot models.
         icol(MXGRP-MXMOD+1) = 1
      END DO
C- Default current model
      ICMOD = 1
C- Plot model at data values.
      nfpl = 0
C- Default to 200 steps for integrate model
      nxstep = 200
      nystep = 200
C SCR
      DO iscr=0,15
         DO i=1,3
            scrcol(i,iscr)=NO
         END DO
      END DO
C- Default labels.
      CRLAB = ' '
C- Default prompt
      CPROM = 'PLT>'
C- Get default hardcopy name
      CALL PLTHAR(CHARD)
      qhard = .FALSE.
      icmd = 0
C---
   90 CONTINUE
C Load the GTBUF command list
      IF ( icmd.LT.Ncmd ) THEN
         IF ( LENACT(Cmd(icmd+1)).LE.0 ) THEN
            icmd = icmd+1
            GOTO 90
         END IF
         CALL STWARN(1)
         CALL LDBUF1(Cmd(icmd+1),Ier)
         icmd = icmd+1-Ier
      ELSE
C All commands loaded, go produce plot
         icmd = -1
         GOTO 600
      END IF
C
  100 CONTINUE
      CALL GTBUF(CPROM,Ier)
      IF(Ier.NE.0) THEN
C EOF
         IF(Ier.LT.0) GOTO 900
C More commands to be loaded
         IF(0.LE.icmd .AND. icmd.LE.Ncmd) GOTO 90
      END IF
      CALL GTCHAR(ctok,ltok)
      IF(ltok.LE.0) GOTO 100
      CALL UPC(ctok)
C
  110 CONTINUE
C---
C- Ajdust window -----------------------------------------------------
      IF ( ctok(1:1).EQ.'A' ) THEN
         CALL GTCHAR(ctok,ltok)
         IF ( ltok.LE.0 ) THEN
            WRITE(*,*) 'Syntax: Ajust ON|OFF wlist'
            GOTO 100
         END IF
         CALL UPC(ctok)
C- Scan for ON/OFf
         IF(ctok(1:2).EQ.'ON') THEN
            ISIGN = +1
         ELSE IF(ctok(1:2).EQ.'OF') THEN
            ISIGN = 0
         END IF
C
         CALL IRANGE(ctok,ltok,1,MXWIN,ILO,IHI,Ier)
         ILO = MAX(1,MIN(ILO,MXWIN))
         IHI = MAX(1,MIN(IHI,MXWIN))
         DO I = ILO,IHI
            IWADJ(icwin) = ISIGN
         END DO
C---
      ELSE IF(ctok(1:1).EQ.'C') THEN
C- CLear device ------------------------------------------------------
         IF(ctok(1:2).EQ.'CL') THEN
            CALL PLTCLR
C---
C- CONtour -----------------------------------------------------------
         ELSE IF(ctok(1:3).EQ.'CON') THEN
            CALL GTCHAR(ctok,ltok)
            IF ( ISNUM(ctok,ltok).NE.0 ) THEN
C CONtour # to change the currently active contour/image group.
               itmp = FPNUM(ctok, ltok, Ier)
               IF ( itmp.LE.0 .OR. itmp.GT.MX2D ) THEN
                  WRITE(*,*) 'Contour number must be in the range 1 to',
     :                  MX2D
                  GOTO 100
               END IF
               CALL PLTXCC(Yray, 1, itmp, xt, ndim, iyoff)
               IF ( ndim.EQ.1 ) THEN
                  WRITE(*,121) itmp
  121             FORMAT(' Plot group ',I3,' is only one dimensional.')
                  GOTO 100
               END IF
               ic2dg = itmp
               CALL GTCHAR(ctok,ltok)
            END IF
            IF ( ic2dg.EQ.0 ) THEN
C User has failed to define a 2D subarray.  Overwrite the first group
C with the largest possible 2D array.
               ic2dg = 1
               isuba(1,ic2dg) = isuba(1,0)
               isuba(2,ic2dg) = isuba(2,0)
               isuba(3,ic2dg) = isuba(3,0)
               isuba(4,ic2dg) = isuba(4,0)
               igrpos(1,ic2dg) = IOFSET(isuba(2,0), Iery, Nvec, Mxrow) +
     &               isuba(1,0) - 1
               igrpos(2,ic2dg) = (isuba(3,0)-isuba(1,0)+1)*
     &              (isuba(4,0)-isuba(2,0)+1)
               igrpos(3,ic2dg) = isuba(2,0)
               ispecg(ic2dg) = 3
               CALL PLTXCN(ic2dg, 2, isuba(3,0)-isuba(1,0)+1)
               CALL PLTXCL(ic2dg, 1, NO, NO)
               CALL PLTXCL(ic2dg, 2, NO, NO)
               imnmx = -1
            END IF
            ioff = 0
C
C This is the main loop for parsing the CONtour command.  NOTE, it
C is important that the next argument is parsed, before jumping back
C here, otherwise an infinite loop results.
  130       CONTINUE
            IF ( ltok.EQ.0 ) THEN
C No more sub-commands.
               IF ( ioff.EQ.0 ) THEN
C Activate the selected window.
                  icont(ic2dg) = 1
                  IF ( ipwin(ic2dg).LE.0 ) ipwin(ic2dg) = icwin
               END IF
               CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
               GOTO 100
            END IF
C
C Now parse sub-commands.
            CALL UPC(ctok)
            IF(ctok(1:1).EQ.'?') THEN
               WRITE(*,*) 'Possible sub-commands are:'
               WRITE(*,*) 'COlor, LEvels, lstyle, LWidth, OFf'
            ELSE IF(ctok(1:2).EQ.'CO') THEN
C CONtour COlor
               DO I = 1,MXLEV
                  CALL GTCHAR(ctok,ltok)
                  IF(ltok.GT.0) THEN
                     IF(ISNUM(ctok, ltok).EQ.0) GOTO 130
                     icocon(I,ic2dg) = FPNUM(ctok,ltok,Ier)
                  END IF
               END DO
            ELSE IF(ctok(1:2).EQ.'LE') THEN
C CONtour LEvel
               DO I = 1,MXLEV
                  CALL GTCHAR(ctok,ltok)
                  IF(ltok.GT.0) THEN
                     IF(ISNUM(ctok, ltok).EQ.0) GOTO 130
                     rlvcon(I,ic2dg) = FPNUM(ctok,ltok,Ier)
                  END IF
               END DO
            ELSE IF(ctok(1:2).EQ.'LS') THEN
C CONtour lstyle
               DO I = 1,MXLEV
                  CALL GTCHAR(ctok,ltok)
                  IF(ltok.GT.0) THEN
                     IF(ISNUM(ctok, ltok).EQ.0) GOTO 130
                     ilscon(I,ic2dg) = FPNUM(ctok,ltok,Ier)
                  END IF
               END DO
            ELSE IF(ctok(1:2).EQ.'LW') THEN
C CONtour LWidth
               DO I = 1,MXLEV
                  CALL GTCHAR(ctok,ltok)
                  IF(ltok.GT.0) THEN
                     IF(ISNUM(ctok, ltok).EQ.0) GOTO 130
                     rlwcon(I,ic2dg) = FPNUM(ctok,ltok,Ier)
                  END IF
               END DO
            ELSE IF(ctok(1:2).EQ.'OF') THEN
C CONtour OFF (turn off all images in the current window)
               ioff = 1
               icont(ic2dg) = 0
               IF ( icol(ic2dg).LE.0 .AND. image(ic2dg).LE.0 )
     &              ipwin(ic2dg) = -ABS(ipwin(ic2dg))
            ELSE
               WRITE(*,591) ctok(:ltok)
            END IF
            CALL GTCHAR(ctok,ltok)
            GOTO 130
C---
C- COlor -------------------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'CO') THEN
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
            FCOL = NO
            IF(ctok(1:1).EQ.'?') THEN
               CALL PLTSCI(-1)
            ELSE IF(ctok(1:1).EQ.'M') THEN
               CALL GTINT(icol(MXGRP-MXMOD+ICMOD),Ier)
            ELSE IF(ISNUM(ctok,ltok).NE.0) THEN
               FCOL = FPNUM(ctok,ltok,Ier)
               CALL GTCHAR(ctok,ltok)
               CALL UPC(ctok)
            END IF
C- Scan for ON/OFf
            IF(ctok(1:2).EQ.'ON') THEN
               ISIGN = +1
            ELSE IF(ctok(1:2).EQ.'OF') THEN
               ISIGN = -1
            END IF
C- Find which vectors to color
  160       CALL GTCHAR(ctok,ltok)
            IF(ltok.LE.0) THEN
               CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
            ELSE
               CALL UPC(ctok)
               IF(ctok(1:1).EQ.'G') THEN
                  IGCOL = FCOL
               ELSE IF(ctok(1:1).EQ.'M') THEN
                  icol(MXGRP-MXMOD+ICMOD) = FCOL
               ELSE
                  CALL IRANGE(ctok,ltok,1,MXGRP-MXMOD,ILO,IHI,Ier)
                  ILO = MAX(1,MIN(ILO,MXGRP-MXMOD))
                  IHI = MAX(1,MIN(IHI,MXGRP-MXMOD))
                  DO I = ILO,IHI
                     IF(FCOL.EQ.NO) THEN
                        IF(ISIGN.LT.0) THEN
                           icol(I) = -ABS(icol(I))
                           ipwin(I) = -ABS(ipwin(I))
                        ELSE
                           icol(I) = ABS(icol(I))
                           ipwin(I) = ABS(ipwin(I))
                        END IF
                     ELSE
                        IF(ISIGN.LT.0) THEN
                           icol(I) = -FCOL
                           ipwin(I) = -ABS(ipwin(I))
                        ELSE
                           icol(I) = FCOL
                           ipwin(I) = ABS(ipwin(I))
                        END IF
                     END IF
                  END DO
               END IF
               GOTO 160
            END IF
         ELSE IF(ctok(1:3).EQ.'CPD') THEN
C- CPD device --------------------------------------------------------
            CALL GTCHAR(cpfile, ltok)
            IF ( ltok.EQ.0 ) THEN
               cpfile = ' '
            ELSE IF ( cpfile(1:1).EQ.'?' ) THEN
               CALL PGLDEV
               GOTO 100
            END IF
            CALL PGEND
            IF ( IOPEN.LT.0 ) CALL PLTTER('A')
            IOPEN = 0
C---
C- Clear and Quit ----------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'CQ' .OR. ctok(1:2).EQ.'CE') THEN
            CALL PLTCLR
            GOTO 950
C---
C- Character Size ----------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'CS') THEN
            CALL GTREAL(TMP, Ier)
            IF(TMP.GT.0.) CSIZE = TMP
C---
C- CUrsor ------------------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'CU') THEN
            loop = 1
            LUN = 0
  170       CONTINUE
            CALL GTCHAR(ctok, ltok)
            IF ( ltok.GT.0 ) THEN
               CALL UPC(ctok(:ltok))
               IF ( ctok(1:3).EQ.'WXY' ) THEN
                  CFILE = ' '
                  CALL GTCHAR(CFILE,LFILE)
C WHead or WEnviron
                  CALL XTEND(CFILE,'CUR')
                  CALL GETLUN(LUN)
                  CALL OPENWR(LUN,CFILE,'new',' ','LIST',0,0,IOS)
                  IF(IOS.NE.0) THEN
                     WRITE(*,*) 'PLT--Unable to open '//
     :                    ctmp(:LENACT(ctmp))
                     GOTO 100
                  END IF
               ELSE IF ( ctok(1:1).EQ.'N' ) THEN
                  CALL GTINT(loop, Ier)
               ELSE
                  GOTO 590
               END IF
               GOTO 170
            END IF
            VXLAS = -1.
            VYLAS = -1.
  180       CONTINUE
               CALL PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN, LOGX, LOGY,
     :          iactw, iwadj, iwnum, VIEWX, VIEWY, WINX, WINY, ctmp)
               IF ( loop.GE.0 ) THEN
                  CALL UPC(ctmp(1:1))
                  IF ( INDEX('QX',ctmp(1:1)).GT. 0 .OR.
     :             VIEWX.EQ.VXLAS .AND. VIEWY.EQ.VYLAS ) THEN
                     IF ( LUN.GT.0 ) THEN
                        CLOSE(UNIT = LUN)
                        CALL FRELUN(LUN)
                     END IF
                     GOTO 100
                  END IF
               END IF
C Write out information
               WRITE(*,*) 'iwnum = ',iwnum
               WRITE(*,*) 'VPOS = ',VIEWX,VIEWY
               WRITE(*,*) 'WPOS = ',WINX,WINY
               IF ( LUN.GT.0 .AND. ctmp(1:1).NE.'?' ) THEN
                  WRITE(LUN,*) WINX, WINY
               END IF
               IF ( ABS(loop).EQ.1 ) THEN
                  IF ( LUN.GT.0 ) THEN
                     CLOSE(UNIT = LUN)
                     CALL FRELUN(LUN)
                  END IF
                  GOTO 100
               END IF
C Decrement loop counter
               IF ( ctmp(1:1).NE.'?' ) THEN
                  IF ( loop.GT.0 ) THEN
                     loop = loop - 1
                  ELSE
                     loop = loop + 1
                  END IF
                  VXLAS = VIEWX
                  VYLAS = VIEWY
               END IF
            GOTO 180
C End 'C' commands
         END IF
C---
C- DGroup ------------------------------------------------------------
      ELSE IF ( ctok(1:2).EQ.'DG' ) THEN
         CALL GTPEEK(ctmp,ltmp)
         IF ( isnum(ctmp, ltmp).NE.0 ) THEN
            CALL GTINT(icsub, ier)
            IF ( icsub.LE.0 .OR. MXGRP-MXMOD.LT.icsub ) THEN
               WRITE(*,*) 'The SUBarray group number must be ',
     &            'in the range  1 to ',MXGRP-MXMOD,'.'
               GOTO 100
            END IF
         ELSE
            ctok = ' '
            CALL GTCHAR(ctok, ltok)
            ltok = MAX(1,ltok)
C Scan to see if name already exists
            cscr1 = ctok(:ltok)
            CALL UPC(cscr1(:ltok))
            icsub = 0
            DO ig= 1,ngroup
               IF ( igrpos(1,ig).GE.0 ) THEN
                  cscr2 = cglab(ig)(:ltok)
                  CALL UPC(cscr2(:ltok))
                  IF ( cscr1(:ltok).EQ.cscr2(:ltok) ) THEN
                     icsub = ig
                  END IF
               END IF
            END DO
            IF ( icsub.LE.0 ) THEN
               WRITE(*,*) 'The DGroup command requires a group.'
               GOTO 100
            END IF
         END IF
C
         CALL GTPEEK(ctmp,ltmp)
         CALL UPC(ctmp)
         IF ( ctmp(1:1).EQ.'M' .OR. ctmp(1:1).EQ.'R' ) THEN
            IF ( ctmp(1:1).EQ.'M' ) THEN
C DGroup # Model
               ipmod(icsub) = icmod
               ispecg(icsub) = 1
            ELSE
C DGroup # Res
               ipmod(icsub) = -icmod
               ispecg(icsub) = 2
            END IF
            CALL GTCHAR(ctok, ltok)
            CALL PLTXCD(ifitg(icmod), icsub)
            igrpos(1, icsub) = igrpos(1, ifitg(icmod))
            igrpos(2, icsub) = igrpos(2, ifitg(icmod))
            igrpos(3, icsub) = igrpos(3, ifitg(icmod))
            ipyer(icsub) = ipyer(ifitg(icmod))
            IF ( icsub.LE.MX2D .AND. ifitg(icmod).LE.MX2D ) THEN
               isuba(1,icsub) = isuba(1, ifitg(icmod))
               isuba(2,icsub) = isuba(2, ifitg(icmod))
               isuba(3,icsub) = isuba(3, ifitg(icmod))
               isuba(4,icsub) = isuba(4, ifitg(icmod))
            END IF
            GOTO 185
         ELSE
C Now read locations of two corners
            I1 = 1
            J1 = 1
            CALL GTINT(I1,Ier)
            CALL GTPEEK(ctmp,ltmp)
            IF ( ltmp.LE.0 ) THEN
C If only one number is on the line then assume that is a column number.
               j1 = i1
               j1 = MIN(MAX(1,j1),Nvec)
               j2 = j1
               i1 = 1
               i2 = Npts
            ELSE
               CALL GTINT(J1,Ier)
               I1 = MIN(MAX(1,I1),Npts)
               J1 = MIN(MAX(1,J1),Nvec)
               I2 = Npts
               J2 = Nvec
               CALL GTINT(I2,Ier)
               CALL GTINT(J2,Ier)
               I2 = MIN(MAX(1,I2),Npts)
               J2 = MIN(MAX(1,J2),Nvec)
               itmp = Iery(j1)
               DO I = J1, J2
                  IF ( Iery(I).NE.itmp ) THEN
                     WRITE(*,*)
     :                'The errors are not the same on all vectors.'
                     WRITE(*,*) 'Not allowed to contour.'
                     GOTO 100
                  END IF
               END DO
            END IF
            ispecg(icsub) = 3
         END IF
C Got group and two corners, now define the subarray
         IF ( J1.EQ.J2 ) THEN
C User defined a 1D array
            CALL PLTXCN(icsub, 1, 0)
            IF ( ixvec.NE.0 ) THEN
               itmp = iofset(ixvec, Iery, ixvec, Mxrow)+I1-1
               CALL PLTXCG(icsub, 1, ixvec, itmp, Iery)
            END IF
            igrpos(1,icsub) = IOFSET(j1, Iery, Nvec, Mxrow) + I1 - 1
            igrpos(2,icsub) = I2-I1+1
            igrpos(3,icsub) = j1
         ELSE
C Must be 2D
            CALL PLTXCN(icsub, 2, i2-i1+1)
            isuba(1,icsub) = I1
            isuba(2,icsub) = J1
            isuba(3,icsub) = I2
            isuba(4,icsub) = J2
            igrpos(1,icsub) = IOFSET(J1, Iery, Nvec, Mxrow) + I1 - 1
            igrpos(2,icsub) = (I2-I1+1)*(J2-J1+1)
            igrpos(3,icsub) = isuba(2,icsub)
            CALL PLTXCL(icsub, 1, NO, NO)
            CALL PLTXCL(icsub, 2, NO, NO)
C Use the most recently defined 2D subarray as the default to contour/image
            ic2dg = icsub
         END IF
  185    CONTINUE
         ngroup = MAX(ngroup, icsub)
C Since we have (possibly) redefined the groups, force the reevaluation
C of data min/max values.
         imnmx = -1
C- Device ------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'D' ) THEN
         CALL GTCHAR(cpfile, ltok)
         IF ( ltok.EQ.0 ) THEN
            cpfile = ' '
         ELSE IF ( cpfile(1:1).EQ.'?' ) THEN
            CALL PGLDEV
            GOTO 100
         END IF
         CALL PGEND
         IF ( IOPEN.LT.0 ) CALL PLTTER('A')
         IOPEN = 0
C---
C- EXit or Quit ------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'EX' .OR. ctok(1:1).EQ.'Q') THEN
         GOTO 950
C---
C- Error -------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'E') THEN
         IDOX = 0
         IDOY = 0
  190    CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF(ctok(1:1).EQ.'X') THEN
            IDOX = 1
            GOTO 190
         ELSE IF(ctok(1:1).EQ.'Y') THEN
            IDOY = 1
            GOTO 190
         END IF
         IF ( IDOX.EQ.0 .AND. IDOY.EQ.0 ) THEN
            IDOX = 1
            IDOY = 1
         END IF
         IF(ctok(1:1).EQ.'D') THEN
            new = 2
         ELSE IF(ctok(1:1).EQ.'G') THEN
C Gehrels
            new = -2
         ELSE IF(ctok(1:2).EQ.'ON') THEN
            new = 1
         ELSE IF(ctok(1:2).EQ.'OF') THEN
            new = 0
         ELSE IF(ctok(1:1).EQ.'S') THEN
C Poisson.
            new = -1
         ELSE
            GOTO 590
         END IF
C- Now get [glist]
         CALL GTCHAR(ctok,ltok)
  210    CONTINUE
         CALL UPC(ctok)
         CALL IRANGE(ctok,ltok,1,ngroup,ILO,IHI,Ier)
         IF(ILO.LT.1 .OR. IHI.LT.1) GOTO 590
         ILO = MAX(1,MIN(ILO,MXGRP))
         IHI = MAX(1,MIN(IHI,MXGRP))
         DO igroup = ILO, IHI
            IF ( igrpos(1,igroup).GE.0 ) THEN
               IF ( IDOX.NE.0 ) CALL PLTXCP(igroup, 1, new)
               IF ( IDOY.NE.0 ) THEN
                  lery = Iery(igrpos(3,igroup))
                  IF ( lery.EQ.0 ) THEN
C- If originally no errors, only plot statistical or no errors.
                     IF ( new.LE.0 ) ipyer(igroup) = new
                  ELSE
                     ipyer(igroup) = new
                  END IF
               END IF
            END IF
         END DO
         CALL GTCHAR(ctok,ltok)
         IF(ltok.GT.0) GOTO 210
C---
C- FNY ---------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'FN') THEN
         IF(nterm(ICMOD).LE.0) THEN
            WRITE(*,*) 'No model defined.'
         ELSE
            CALL GTREAL(xt(1),Ier)
            xt(2) = 0.0
            CALL GTREAL(xt(2),Ier)
            WRITE(*,*) FNFIT(XT,ICOMP(1,ICMOD),
     :        PVAL(1,ICMOD),nterm(ICMOD))
         END IF
C---
C- FOnt --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'FO') THEN
         CALL GTCHAR(ctok,ltok)
         IF(ltok.GT.0) THEN
            IF(ctok(1:1).EQ.'?') THEN
               CALL PLTFON(ctok)
            ELSE
               CFONT = ctok
               IF(IOPEN.NE.0) CALL PLTFON(CFONT)
            END IF
         ELSE
            CFONT = ' '
         END IF
C---
C- FReeze ------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'FR') THEN
         IF(nterm(ICMOD).LE.0) THEN
            WRITE(*,411)
         ELSE
            CALL GTREST(ctok,ltok)
            ctmp = 'FR '//ctok
            CALL MODEL(ctmp,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     &       ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
         END IF
C---
C- Fit ---------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'F') THEN
         IF ( nterm(ICMOD).LE.0 ) THEN
            WRITE(*,*) 'PLT--You must define a model first.'
            GOTO 100
         END IF
C
         CALL DSCALE(Yray, Iery, Mxrow, MXWIN, MX2D, ngroup, MXPAR,
     &    igap, rgap, idoall, iskip, newmod, iactw, logx, logy, imaster,
     &    igrpos, ipwin, icont, image, ipmod, icomp, pval, nterm,
     &    imnmx, xymnmx, ermnmx, xyscal)
C
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF ( ctok(1:2).EQ.'ON' ) THEN
C Fit ON (plot the model in the same window as the group being fitted).
            CALL GTINT(itmp,Ier)
            IF ( itmp.GT.0 .AND. itmp.LE.ngroup ) THEN
               ipwin(MXGRP-MXMOD+icmod) = ipwin(itmp)
               ifitg(icmod) = itmp
            ELSE
               ipwin(MXGRP-MXMOD+icmod) = ABS(ipwin(MXGRP-MXMOD+icmod))
            END IF
            GOTO 100
         ELSE IF ( ctok(1:2).EQ.'OF' ) THEN
C Fit OFf
            ipwin(MXGRP-MXMOD+icmod) = -ABS(ipwin(MXGRP-MXMOD+icmod))
            GOTO 100
         ELSE IF ( ctok(1:1).EQ.'P' ) THEN
C Fit Plot #
            CALL GTINT(nfpl,Ier)
            GOTO 100
         ELSE IF ( ctok(1:1).EQ.'E' ) THEN
C Obsolete Fit Error command
            ctok = 'UNCER'
            GOTO 110
         END IF
C
         ctok(ltok+1:) = ' '
         CALL GTREST(ctok(ltok+2:),ltmp)
         IF ( IOPEN.LT.0 ) CALL PLTTER('A')
         CALL FIT(ctok, ifitg(icmod), Yray, Mxrow, ngroup, icwin,
     :    ipwin, ipyer, ipwin, igrpos, XYSCAL,
     :    ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
         ipwin(MXGRP-MXMOD+icmod) = ipwin(ifitg(icmod))
         ipall = 0
         ICLEAR = 0
         newmod = icmod
         GOTO 650
C---
C- G -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'G') THEN
         IF(ctok(1:2).EQ.'GA') THEN
C- GAp ---------------------------------------------------------------
  230       CONTINUE
            CALL GTCHAR(ctok,ltok)
            IF ( ltok.GE.1 ) THEN
               CALL UPC(ctok(:ltok))
               IF ( ctok(1:1).EQ.'E' ) THEN
C Gap Error
                  IGAP = +1
                  CALL GTCHAR(ctok,ltok)
               ELSE IF ( ctok(1:1).EQ.'N' ) THEN
C Gap Noerror
                  IGAP = 0
                  CALL GTCHAR(ctok,ltok)
               ELSE IF ( ISNUM(ctok,ltok).NE.0 ) THEN
C Gap #
                  RGAP = FPNUM(ctok, ltok, Ier)
               END IF
               GOTO 230
            END IF
         ELSE IF(ctok(1:1).EQ.'G') THEN
C- Grid --------------------------------------------------------------
  240       CALL GTCHAR(ctok,ltok)
            IF(ltok.LE.0) GOTO 100
            CALL UPC(ctok)
            IF(ctok(1:2).EQ.'ON') THEN
C Grid ON
               IF(idoall.NE.0) THEN
                  DO i=1,MXWIN
                     CXOPT(i)(7:7) = 'G'
                     CYOPT(i)(7:7) = 'G'
                  END DO
               ELSE
                  CXOPT(icwin)(7:7) = 'G'
                  CYOPT(icwin)(7:7) = 'G'
               END IF
            ELSE IF(ctok(1:2).EQ.'OF') THEN
C Grid OFf
               IF(idoall.NE.0) THEN
                  DO i=1,MXWIN
                     CXOPT(i)(7:7) = ' '
                     CYOPT(i)(7:7) = ' '
                  END DO
               ELSE
                  CXOPT(icwin)(7:7) = ' '
                  CYOPT(icwin)(7:7) = ' '
               END IF
            ELSE IF(ctok(1:1).EQ.'X') THEN
C Grid X
               CALL GTPEEK(ctmp, ltmp)
               IF ( ISNUM(ctmp, ltmp).NE.0 ) THEN
                  CALL GTREAL(TMP, Ier)
                  IF(TMP.LT.0.) THEN
                     CXOPT(icwin)(4:4) = ' '
                  ELSE
                     CXOPT(icwin)(4:4) = 'T'
                     GRIDX(icwin) = TMP
                  END IF
                  itmp = -1
                  CALL GTINT(itmp,Ier)
                  IF(itmp.LT.0) THEN
                     CXOPT(icwin)(3:3) = ' '
                  ELSE
                     CXOPT(icwin)(3:3) = 'S'
                     NSUBX(icwin) = itmp
                  END IF
                  IF(idoall.NE.0) THEN
                     DO i=1,MXWIN
                        CXOPT(i)(4:4) = CXOPT(icwin)(4:4)
                        GRIDX(i) = GRIDX(icwin)
                        CXOPT(i)(3:3) = CXOPT(icwin)(3:3)
                        NSUBX(i) = NSUBX(icwin)
                     END DO
                  END IF
               ELSE
                  CALL GTCHAR(ctok, ltok)
                  CALL UPC(ctok(:ltok))
                  IF ( ctok(1:2).EQ.'ON') THEN
C Grid X ON
                     IF(idoall.NE.0) THEN
                        DO i=1,MXWIN
                           CXOPT(i)(7:7) = 'G'
                        END DO
                     ELSE
                        CXOPT(icwin)(7:7) = 'G'
                     END IF
                  ELSE IF ( ctok(1:2).EQ.'OF') THEN
C Grid X OFf
                     IF(idoall.NE.0) THEN
                        DO i=1,MXWIN
                           CXOPT(i)(7:7) = ' '
                        END DO
                     ELSE
                        CXOPT(icwin)(7:7) = ' '
                     END IF
                  END IF
               END IF
            ELSE IF(ctok(1:1).EQ.'Y') THEN
C Grid Y
               CALL GTPEEK(ctmp, ltmp)
               IF ( ISNUM(ctmp, ltmp).NE.0 ) THEN
                  IF(ltok.GT.1) THEN
                     itmp = FPNUM(ctok(2:ltok),ltok-1,Ier)
                     iwnum = MAX(1,MIN(itmp,MXWIN))
                  ELSE
                     iwnum = icwin
                  END IF
                  CALL GTREAL(TMP,Ier)
                  IF(TMP.LT.0.) THEN
                     CYOPT(iwnum)(4:4) = ' '
                  ELSE
                     CYOPT(iwnum)(4:4) = 'T'
                     GRIDY(iwnum) = TMP
                  END IF
                  itmp = -1
                  CALL GTINT(itmp,Ier)
                  IF(itmp.LT.0) THEN
                     CYOPT(iwnum)(3:3) = ' '
                  ELSE
                     CYOPT(iwnum)(3:3) = 'S'
                     NSUBY(iwnum) = itmp
                  END IF
                  IF(idoall.NE.0) THEN
                     DO i=1,MXWIN
                        CYOPT(i)(4:4) = CYOPT(icwin)(4:4)
                        GRIDY(i) = GRIDY(icwin)
                        CYOPT(i)(3:3) = CYOPT(icwin)(3:3)
                        NSUBY(i) = NSUBY(icwin)
                     END DO
                  END IF
               ELSE
                  CALL GTCHAR(ctok, ltok)
                  CALL UPC(ctok(:ltok))
                  IF ( ctok(1:2).EQ.'ON') THEN
C Grid Y ON
                     IF(idoall.NE.0) THEN
                        DO i=1,MXWIN
                           CYOPT(i)(7:7) = 'G'
                        END DO
                     ELSE
                        CYOPT(icwin)(7:7) = 'G'
                     END IF
                  ELSE IF ( ctok(1:2).EQ.'OF') THEN
C Grid Y OFf
                     IF(idoall.NE.0) THEN
                        DO i=1,MXWIN
                           CYOPT(i)(7:7) = ' '
                        END DO
                     ELSE
                        CYOPT(icwin)(7:7) = ' '
                     END IF
                  END IF
               END IF
            ELSE
               GOTO 590
            END IF
            GOTO 240
         END IF
C---
C- HElp --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'HE' .OR. ctok(1:1).EQ.'?') THEN
         CALL GTREST(ctmp,ltmp)
         IF(IOPEN.LT.0) CALL PLTTER('A')
         ctmp(ltmp+1:) = ' '
         CALL F_IHF(CHLIB, CTMP)
C---
C- Hardcopy ----------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'H') THEN
         CPSAV = cpfile
         CALL GTCHAR(ctok,ltok)
         IF(ctok(1:1).EQ.'?') THEN
            WRITE(*,*) 'Current hardcopy device is: ',
     :       CHARD(:LENACT(CHARD))
            GOTO 100
         END IF
         IF(ltok.GT.0) CHARD = ctok(:ltok)
         CALL PGEND
         IF(IOPEN.LT.0) CALL PLTTER('A')
         IOPEN = 0
         cpfile = CHARD
         QHARD = .TRUE.
         ICLEAR = 0
         GOTO 610
C---
      ELSE IF ( ctok(1:1).EQ.'I' ) THEN
C---
         IF ( ctok(1:3).EQ.'IMA' ) THEN
C- IMAge -------------------------------------------------------------
            CALL GTCHAR(ctok,ltok)
            IF ( ISNUM(ctok,ltok).NE.0 ) THEN
C IMAge # to change the currently active contour/image group.
               itmp = FPNUM(ctok, ltok, Ier)
               IF ( itmp.LE.0 .OR. itmp.GT.MX2D ) THEN
                  WRITE(*,*) 'Image number must be in the range 1 to',
     :                  MX2D
                  GOTO 100
               END IF
               CALL PLTXCC(Yray, 1, itmp, xt, ndim, iyoff)
               IF ( ndim.EQ.1 ) THEN
                  WRITE(*,121) itmp
                  GOTO 100
               END IF
               ic2dg = itmp
               CALL GTCHAR(ctok,ltok)
            END IF
            IF ( ic2dg.EQ.0 ) THEN
C User has failed to define a 2D subarray.  Overwrite the first group
C with the largest possible 2D array.
               ic2dg = 1
               isuba(1,ic2dg) = isuba(1,0)
               isuba(2,ic2dg) = isuba(2,0)
               isuba(3,ic2dg) = isuba(3,0)
               isuba(4,ic2dg) = isuba(4,0)
               igrpos(1,ic2dg) = IOFSET(isuba(2,0), Iery, Nvec, Mxrow) +
     &               isuba(1,0) - 1
               igrpos(2,ic2dg) = (isuba(3,0)-isuba(1,0)+1)*
     &              (isuba(4,0)-isuba(2,0)+1)
               igrpos(3,ic2dg) = isuba(2,0)
               ispecg(ic2dg) = 3
               CALL PLTXCN(ic2dg, 2, isuba(3,0)-isuba(1,0)+1)
               CALL PLTXCL(ic2dg, 1, NO, NO)
               CALL PLTXCL(ic2dg, 2, NO, NO)
               imnmx = -1
            END IF
            ioff = 0
C
C This is the main loop for parsing the image command.  NOTE, it
C is important that the next argument is parsed, before jumping back
C here, otherwise an infinite loop results.
  245       CONTINUE
            IF(ltok.EQ.0) THEN
C No more sub-commands.
               IF ( ioff.EQ.0 ) THEN
C Activate the selected window.
                  image(ic2dg) = 1
                  IF ( ipwin(ic2dg).LE.0 ) ipwin(ic2dg) = icwin
               END IF
               CALL ACTWIN(ipwin, ngroup, MXWIN, iactw)
               GOTO 100
            END IF
            CALL UPC(ctok(:ltok))
            IF(ctok(1:1).EQ.'H') THEN
C IMA Histo Not currently supported!
               itfun(ic2dg) = -1
            ELSE IF(ctok(1:2).EQ.'LI') THEN
C IMA LIn
               itfun(ic2dg) = 0
            ELSE IF(ctok(1:2).EQ.'LO') THEN
C IMA LOg
               itfun(ic2dg) = 1
            ELSE IF(ctok(1:2).EQ.'SQ') THEN
C IMA SQrt
               itfun(ic2dg) = 2
            ELSE IF(ctok(1:2).EQ.'MA') THEN
C IMA MAx #
               CALL GTREAL(zscale(2,ic2dg), Ier)
            ELSE IF(ctok(1:2).EQ.'MI') THEN
C IMA MIn #
               CALL GTREAL(zscale(1,ic2dg), Ier)
            ELSE IF(ctok(1:2).EQ.'RO') THEN
C IMA ROt #
               CALL GTREAL(rota(ic2dg), Ier)
            ELSE IF(ctok(1:2).EQ.'CB') THEN
C IMAge CBar ON|OFF (turn color bar on or off)
               CALL GTCHAR(ctok, ltok)
               CALL UPC(ctok)
               IF(ctok(1:2).EQ.'OF') THEN
                  icbar(ic2dg) = 0
               ELSE
                  icbar(ic2dg) = 1
               END IF
            ELSE IF(ctok(1:1).EQ.'C') THEN
C IMAge CCT (change color table)
               CALL GTCHAR(ctmp, ltmp)
               IF ( ctmp(ltmp:ltmp).EQ.'?' ) THEN
                  CALL PLTCCT(ctmp, ltmp)
               ELSE
                  cctnam(ic2dg) = ctmp
                  lctnam(ic2dg) = ltmp
               END IF
            ELSE IF(ctok(1:2).EQ.'OF') THEN
C IMAge OFf
               ioff = 1
               image(ic2dg) = 0
               IF ( icol(ic2dg).LE.0 .AND. icont(ic2dg).LE.0 )
     &              ipwin(ic2dg) = -ABS(ipwin(ic2dg))
            ELSE IF(ctok(1:2).EQ.'ON') THEN
C IMAge ON (since most of the work upon exit, this is mostly a placeholder).
               image(ic2dg) = 1
            ELSE
               WRITE(*,591) ctok(:ltok)
            END IF
            CALL GTCHAR(ctok,ltok)
            GOTO 245
         ELSE IF ( ctok(1:3).EQ.'IMO' ) THEN
C- Integrate model ---------------------------------------------------
            IF(nterm(ICMOD).LE.0) THEN
               WRITE(*,*) 'No model defined.'
               GOTO 100
            END IF
            XL = pmin(1)
            CALL GTREAL(XL,Ier)
            XH = pmax(1)
            CALL GTREAL(XH,Ier)
            CALL GTINT(nxstep,Ier)
            IF(nxstep.LE.1) nxstep = 200
            XDEL = (XH-XL)/(nxstep-1.)
C
            CALL PLTXCC(Yray, 1, ifitg(icmod), xt, ndim, iyoff)
            IF ( ndim.EQ.1 ) THEN
               yl = 0.0
               yh = 0.0
               nystep = 1
               ydel = 1.0
            ELSE
               yl = pmin(2)
               CALL GTREAL(yl,Ier)
               yh = pmax(2)
               CALL GTREAL(yh,Ier)
               CALL GTINT(nystep,Ier)
               IF(nystep.LE.1) nystep = 200
               ydel = (yh-yl)/(nystep-1.)
            END IF
C
            dsum = 0.0d0
            DO iystep=1, nystep
               xt(2) = yl + (iystep-1)*ydel
               DO i = 1,nxstep
                  XT(1) = XL+(i-1.)*XDEL
                  dsum = dsum+XDEL*ydel*
     :            FNFIT(XT,ICOMP(1,ICMOD),PVAL(1,ICMOD),nterm(ICMOD))
               END DO
            END DO
            IF ( ndim.EQ.1 ) THEN
               WRITE(*,251) XL,XH,dsum
  251          FORMAT(' Model from ',1PG12.5,' to ',G12.5,' is ',G11.5)
            ELSE
               WRITE(*,261) xl,xh,yl,yh,dsum
  261          FORMAT(' Model from ',1PG12.5,' to ',G12.5,/,
     &                ' and Y from ',1PG12.5,' to ',G12.5,' is ',G11.5)
            END IF
         ELSE
C Info command
            ctok = ' '
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF(ctok(1:1).EQ.'?') THEN
               WRITE(*,*) 'Possible sub-commands are:'
               WRITE(*,*) 'All, Call, Groups, Scales, Windows'
               GOTO 100
            END IF
            IF(ltok.EQ.0) THEN
               ctok = 'S'
               ltok = 1
            END IF
            IF(ctok(1:1).EQ.'C' .OR. ctok(1:1).EQ.'A') THEN
C Info Call
               WRITE(*,*)
               WRITE(*,*) 'Calling sequence:'
               itmp = Nvec
               DO I = 1,Nvec
                  IF(Iery(I).GT.0) itmp = itmp+Iery(I)
               END DO
               WRITE(*,281) Mxrow, Npts, itmp, Nvec
  281          FORMAT(' Max num of rows = ',I9/
     :          ' Num of points = ',I7,', Num of columns = ',I5,
     :          ', Num of vectors = ',I5)
               WRITE(*,*)
               WRITE(*,*) 'Ncmd=',Ncmd,', Cmd='
               DO I = 1,Ncmd
                  ltmp = LENACT(Cmd(I))
                  WRITE(*,*) Cmd(I)(:ltmp)
               END DO
            END IF
            IF(ctok(1:1).EQ.'G' .OR. ctok(1:1).EQ.'A') THEN
C Info Groups
               WRITE(*,*)
               WRITE(*,*) 'Groups:'
               WRITE(*,*)  'Grp, icol, Iery, ipyer, '//
     :          'line, lsty, MARK, PMAR, SZMAR, FLIM, ipmod'
               DO ig=1, ngroup
                  IF ( igrpos(1,ig).GE.0 ) THEN
                     lery = Iery(igrpos(3,ig))
                     WRITE(*,291) ig,icol(ig),lery,ipyer(ig),line(ig),
     :                lsty(ig),imark(ig),ipmark(ig),
     :                szmark(ig),flimit(ig),ipmod(ig)
  291                FORMAT(I3,I6,I6,I7,I7,3I6,F7.1,F6.1,I6)
                  END IF
               END DO
            END IF
            IF(ctok(1:1).EQ.'I' .OR. ctok(1:1).EQ.'A') THEN
C Info Internals
               WRITE(*,*)
               WRITE(*,*) 'Internals:'
               WRITE(*,*)  'Grp,ipwin, igrpos(1), igrpos(2), igrpos(3)'
               DO ig=1, ngroup
                  IF ( igrpos(1,ig).GE.0 ) THEN
                     WRITE(*,351) ig,ipwin(ig),
     :                  igrpos(1,ig),igrpos(2,ig),igrpos(3,ig)
  351                FORMAT(I3,I6,3I10)
                  END IF
               END DO
            END IF
            IF(ctok(1:1).EQ.'S' .OR. ctok(1:1).EQ.'A') THEN
               IF ( ctok(2:2).EQ.'U' ) THEN
                  WRITE(*,*)
                  WRITE(*,*) 'Subarrays:'
                  WRITE(*,*) 'Grp  Icont  Image'
                  DO ig=1,MX2D
                     WRITE(*,311) ig,icont(ig),image(ig)
  311                FORMAT(I3,I6,I6)
                  END DO
               ELSE
C Info Scales
                  CALL PTBUF(' ', 1)
                  CALL PTBUF('Scales:',-1)
                  CALL PTBUF('Grp  Wind    Label     XData Min'//
     &               '    XData Max    YData Min    YData Max',-1)
                  DO igroup = 1,ngroup
                     IF ( igrpos(1,igroup).GE.0 ) THEN
                        WRITE(ctmp,321) igroup,ipwin(igroup),
     :                   cglab(igroup),
     :                   xymnmx(1,igroup),xymnmx(3,igroup),
     &                   xymnmx(2,igroup),xymnmx(4,igroup)
  321                   FORMAT(I3,I6,2X,A10,1P,2(' :',G11.4,', ',G11.4))
                        CALL PTBUF(ctmp, -1)
                     END IF
                  END DO
               END IF
            END IF
            IF(ctok(1:1).EQ.'W' .OR. ctok(1:1).EQ.'A') THEN
C Info Windows
               WRITE(*,*)
               WRITE(*,*) 'Windows:'
               WRITE(*,*)  'Win, logx, logy,  gridx,   gridy,'//
     &               '  nsubx, nsuby, iwadj'
               DO i=1,MXWIN
                  IF ( iactw(i).NE.0 ) THEN
                     WRITE(*,331) i,LOGX(i),LOGY(i),gridx(i),gridy(i),
     &                 NSUBX(i), NSUBY(i), IWADJ(i)
  331                FORMAT(I3,I6,I6,2F9.2,I7,I7,I7)
                  END IF
               END DO
            END IF
         END IF
C---
C- Label -------------------------------------------------------------
      ELSE IF ( ctok(1:2).EQ.'LA' ) THEN
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF ( ctok(1:2).EQ.'OF' ) THEN
C LAbel OFf
            IPLAB = IAND(IPLAB,-2)
         ELSE IF ( ctok(1:2).EQ.'ON' ) THEN
C LAbel ON
            IPLAB = IOR(IPLAB,1)
         ELSE IF ( ctok(1:2).EQ.'NX' ) THEN
C LAbel NX [OF|ON]
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF(ctok(1:2).EQ.'OF') THEN
               ION = 0
            ELSE
               ION = 1
            END IF
            CALL GTPEEK(ctmp, ltmp)
            IF ( ltmp.EQ.0 ) THEN
               IF ( ION.EQ.0 ) THEN
                  CXOPT(icwin)(5:5) = ' '
               ELSE
                  CXOPT(icwin)(5:5) = 'N'
               END IF
            ELSE
  340          itmp = 0
               CALL GTINT(itmp, Ier)
               IF(1.LE.itmp .AND. itmp.LE.MXWIN) THEN
                  IF(ION.EQ.0) THEN
                     CXOPT(itmp)(5:5) = ' '
                  ELSE
                     CXOPT(itmp)(5:5) = 'N'
                  END IF
                  GOTO 340
               END IF
               GOTO 100
            END IF
         ELSE IF ( ctok(1:2).EQ.'NY' ) THEN
C LAbel NY [OF|ON]
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF ( ctok(1:2).EQ.'OF' ) THEN
               ION = 0
            ELSE
               ION = 1
            END IF
            CALL GTPEEK(ctmp, ltmp)
            IF(ltmp.EQ.0) THEN
               IF(ION.EQ.0) THEN
                  CYOPT(icwin)(5:5) = ' '
               ELSE
                  CYOPT(icwin)(5:5) = 'N'
               END IF
            ELSE
  350          itmp = 0
               CALL GTINT(itmp, Ier)
               IF(1.LE.itmp .AND. itmp.LE.MXWIN) THEN
                  IF(ION.EQ.0) THEN
                     CYOPT(itmp)(5:5) = ' '
                  ELSE
                     CYOPT(itmp)(5:5) = 'N'
                  END IF
                  GOTO 350
               END IF
               GOTO 100
            END IF
         ELSE IF(ctok(1:2).EQ.'PA') THEN
C LAbel PArameter [ON|OF]
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
            IF(ctok(1:2).EQ.'OF') THEN
               IPLAB = IAND(IPLAB,-3)
            ELSE IF(ctok(1:2).EQ.'ON') THEN
               IPLAB = IOR(IPLAB,2)
            ELSE
               GOTO 590
            END IF
         ELSE IF(ctok(1:2).EQ.'PO') THEN
C LAbel POsition
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF(ctok(1:1).EQ.'Y') THEN
               CALL GTREAL(PYLAB, Ier)
               GOTO 100
            END IF
            GOTO 590
         ELSE IF(ctok(1:2).EQ.'OT') THEN
C LAbel OT
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(COTLAB(icwin),ltmp)
            ELSE
               CALL GTREST(COTLAB(icwin),ltmp)
            END IF
         ELSE IF(ctok(1:2).EQ.'OX') THEN
C LAbel OX
            CALL GTPEEK(ctmp,ltmp)
            IF ( laswin.NE.0 ) THEN
               itmp = laswin
            ELSE
               itmp = icwin
            END IF
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(COXLAB(itmp),ltmp)
            ELSE
               CALL GTREST(COXLAB(itmp),ltmp)
            END IF
         ELSE IF(ctok(1:2).EQ.'OY') THEN
C LAbel OY
            IF(ltok.GT.2) THEN
               itmp = FPNUM(ctok(3:ltok),ltok-2,Ier)
               iwnum = MAX(1,MIN(itmp,MXWIN))
            ELSE
               iwnum = icwin
            END IF
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(COYLAB(iwnum),ltmp)
            ELSE
               CALL GTREST(COYLAB(iwnum),ltmp)
            END IF
         ELSE IF(ctok(1:1).EQ.'R') THEN
C LAbel Rotate
            IF(CYOPT(1)(8:8).EQ.'V') THEN
               DO iwnum = 1,MXWIN
                  CYOPT(iwnum)(8:8) = ' '
               END DO
            ELSE
               DO iwnum = 1,MXWIN
                  CYOPT(iwnum)(8:8) = 'V'
               END DO
            END IF
         ELSE IF(ctok(1:1).EQ.'F') THEN
C LAbel File
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(CFNAM(icwin),ltmp)
            ELSE
               CALL GTREST(CFNAM(icwin),ltmp)
            END IF
         ELSE IF(ctok(1:1).EQ.'T') THEN
C LAbel Top
            IF(ltok.GT.2) THEN
               itmp = FPNUM(ctok(3:ltok),ltok-2,Ier)
               iwnum = MAX(1,MIN(itmp,MXWIN))
            ELSE
               iwnum = icwin
            END IF
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(CTLAB(iwnum),ltmp)
            ELSE
               CALL GTREST(CTLAB(iwnum),ltmp)
            END IF
         ELSE IF(ctok(1:1).EQ.'B' .OR. ctok(1:1).EQ.'X') THEN
C LAbel X (Bottom)
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(ctok,ltok)
            ELSE
               CALL GTREST(ctok,ltok)
            END IF
            IF ( laswin.NE.0 ) THEN
               CXLAB(laswin) = ctok
            ELSE
               CXLAB(icwin) = ctok
            END IF
         ELSE IF(ctok(1:1).EQ.'L' .OR.
     :         (ltok.EQ.1 .AND. ctok(1:1).EQ.'Y')) THEN
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(CYLAB(icwin),ltmp)
            ELSE
               CALL GTREST(CYLAB(icwin),ltmp)
            END IF
         ELSE IF(ctok(1:1).EQ.'G' .OR. ctok(1:1).EQ.'Y') THEN
            IF(ltok.GT.1) THEN
               igroup = FPNUM(ctok(2:ltok),ltok-1,Ier)
               igroup = MAX(1,MIN(igroup,MXGRP-MXMOD))
            ELSE
               igroup = icwin
            END IF
            CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(cglab(igroup),ltmp)
            ELSE
               CALL GTREST(cglab(igroup),ltmp)
            END IF
         ELSE IF(ISNUM(ctok,ltok).NE.0) THEN
C LAbel #
            iclab = FPNUM(ctok,ltok,Ier)
            IF(iclab.LE.0 .OR. iclab.GT.MXLAB) GOTO 590
  360       CALL GTPEEK(ctmp,ltmp)
            IF(ctmp(1:1).EQ.' ') GOTO 100
            CALL UPC(ctmp)
            IF(ctmp(1:2).EQ.'TE') THEN
C Skip the command
               CALL GTCHAR(ctmp,itmp)
C but get everything else on line
               CALL GTREST(CLABEL(iclab),itmp)
               IF(itmp.LE.0) THEN
                  ILABEL(1,iclab) = 0
               ELSE IF(ILABEL(1,iclab).EQ.0) THEN
                  ILABEL(1,iclab) = icwin
               END IF
            ELSE IF(ctmp(1:1).EQ.'"') THEN
               CALL GTCHAR(CLABEL(iclab),itmp)
               IF(itmp.LE.0) THEN
                  ILABEL(1,iclab) = 0
               ELSE IF(ILABEL(1,iclab).EQ.0) THEN
                  ILABEL(1,iclab) = icwin
               END IF
            ELSE
               CALL GTCHAR(ctok,ltok)
               CALL UPC(ctok)
               IF(ctok(1:2).EQ.'CE') THEN
                  CALL GTCHAR(ctok,ltok)
                  CALL UPC(ctok)
                  IF(ctok(1:1).EQ.'T') THEN
                     ILABEL(3,iclab) = 1
                  ELSE IF(ctok(1:1).EQ.'C') THEN
                     ILABEL(3,iclab) = 2
                  ELSE IF(ctok(1:1).EQ.'H') THEN
                     ILABEL(3,iclab) = 3
                  ELSE IF(ctok(1:2).EQ.'BA') THEN
                     ILABEL(3,iclab) = 4
                  ELSE IF(ctok(1:2).EQ.'BO') THEN
                     ILABEL(3,iclab) = 5
                  ELSE
                     GOTO 590
                  END IF
               ELSE IF(ctok(1:2).EQ.'CO') THEN
                  CALL GTINT(ILABEL(4,iclab),Ier)
               ELSE IF(ctok(1:2).EQ.'CS') THEN
                  FLABEL(4,iclab) = 1.0
                  CALL GTREAL(FLABEL(4,iclab),Ier)
               ELSE IF(ctok(1:1).EQ.'J') THEN
                  CALL GTCHAR(ctok,ltok)
                  CALL UPC(ctok)
                  IF(ctok(1:1).EQ.'C') THEN
                     ILABEL(2,iclab) = 2
                  ELSE IF(ctok(1:1).EQ.'L') THEN
                     ILABEL(2,iclab) = 1
                  ELSE IF(ctok(1:1).EQ.'R') THEN
                     ILABEL(2,iclab) = 3
                  ELSE
                     GOTO 590
                  END IF
               ELSE IF(ctok(1:2).EQ.'LS') THEN
                  CALL GTINT(ILABEL(5,iclab),Ier)
               ELSE IF(ctok(1:1).EQ.'L') THEN
                  ilabel(7,iclab) = 0
                  CALL GTCHAR(ctok,ltok)
                  IF(ISNUM(ctok,ltok).EQ.0) THEN
                     ILABEL(2,iclab) = 2
                     ILABEL(3,iclab) = 3
                     FLABEL(6,iclab) = 0.
                  ELSE
                     FLABEL(5,iclab) = FPNUM(ctok,ltok,Ier)
                     CA = COS(FLABEL(5,iclab)/RTD)
                     SA = SIN(FLABEL(5,iclab)/RTD)
                     IF(CA.GT..707) THEN
                        ILABEL(2,iclab) = 1
                     ELSE IF(CA.GT.-.707) THEN
                        ILABEL(2,iclab) = 2
                     ELSE
                        ILABEL(2,iclab) = 3
                     END IF
                     IF(SA.GT..707) THEN
                        ILABEL(3,iclab) = 5
                     ELSE IF(SA.GT.-.707) THEN
                        ILABEL(3,iclab) = 3
                     ELSE
                        ILABEL(3,iclab) = 1
                     END IF
C- Get optional length.
                     FLABEL(6,iclab) = .08
                     CALL GTPEEK(ctmp,ltmp)
                     IF(ISNUM(ctmp,ltmp).NE.0) THEN
                        CALL GTREAL(FLABEL(6,iclab),Ier)
                     END IF
                  END IF
               ELSE IF(ctok(1:2).EQ.'MS') THEN
                  CALL GTREAL(FLABEL(7,iclab), Ier)
                  FLABEL(7,iclab) = MAX(0.,MIN(FLABEL(7,iclab),5.))
               ELSE IF(ctok(1:1).EQ.'M') THEN
                  ILABEL(6,iclab) = -1
                  CALL GTINT(ILABEL(6,iclab),Ier)
                  IF(ILABEL(6,iclab).LT.0) THEN
                     ILABEL(2,iclab) = 2
                  ELSE
                     ILABEL(2,iclab) = 1
                  END IF
               ELSE IF(ctok(1:1).EQ.'P') THEN
                  CALL GTPEEK(ctmp,ltmp)
                  CALL UPC(ctmp)
                  ILABEL(1,iclab) = icwin
                  IF(ctmp(1:1).EQ.'C') THEN
CLAbel # Pos C
                     CALL GTCHAR(ctmp,ltmp)
                     CALL PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN,
     :                 LOGX, LOGY, iactw, iwadj, iwnum,
     :                 VIEWX, VIEWY, WINX, WINY, ctmp)
                     FLABEL(1,iclab) = WINX
                     FLABEL(2,iclab) = WINY
                  ELSE
                     CALL GTPEEK(ctmp,ltmp)
C Special case trap for a real number entered as #e... where # is a single integer
C because GTPEEK only returns the first two characters of the token and ISNUM
C checks for a number by checking that the first and last characters are integers.
                     IF(ISNUM(ctmp, ltmp).EQ.0 
     &                    .AND. ctmp(2:2).NE.'e' .AND. ctmp(2:2).NE.'E'
     &                    ) THEN
                      CALL PTBUF('Position expects two numbers, got "'//
     &                     ctmp(:ltmp)//'".',-1)
                        CALL GTCHAR(ctmp,ltmp)
                        GOTO 360
                     END IF
                     CALL GTREAL(FLABEL(1,iclab),Ier)
                     CALL GTREAL(FLABEL(2,iclab),Ier)
                  END IF
               ELSE IF(ctok(1:1).EQ.'R') THEN
                  CALL GTREAL(FLABEL(3,iclab),Ier)
               ELSE IF(ctok(1:1).EQ.'T') THEN
                  ilabel(7,iclab) = 1
                  CALL GTREAL(FLABEL(5,iclab),Ier)
                  CALL GTREAL(FLABEL(6,iclab),Ier)
               ELSE IF(ctok(1:1).EQ.'V') THEN
                  CALL GTPEEK(ctmp,ltmp)
                  CALL UPC(ctmp)
                  ILABEL(1,iclab) = -1
                  IF(ctmp(1:1).EQ.'C') THEN
C- Read cursor (flush current token)
                     CALL GTCHAR(ctmp,ltmp)
                     CALL PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN,
     :                 LOGX, LOGY, iactw, iwadj, iwnum,
     :                 VIEWX, VIEWY, WINX, WINY, ctmp)
                     FLABEL(1,iclab) = VIEWX
                     FLABEL(2,iclab) = VIEWY
                  ELSE
                     CALL GTREAL(FLABEL(1,iclab),Ier)
                     CALL GTREAL(FLABEL(2,iclab),Ier)
                  END IF
               ELSE
                  CALL PTBUF('Bad sub-command: "'//ctok(:ltok)//'"',-1)
               END IF
            END IF
            GOTO 360
         ELSE
            GOTO 590
         END IF
C---
C- L -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'L') THEN
         IF(ctok(1:3).EQ.'LOC') THEN
C- LOCation ----------------------------------------------------------
            DO I = 1,4
               CALL GTREAL(WINLOC(I,icwin), Ier)
            END DO
         ELSE IF(ctok(1:2).EQ.'LI') THEN
C- line --------------------------------------------------------------
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
            IF(ctok(1:1).EQ.'S')  THEN
C line Step
               itmp = -1
            ELSE IF(ctok(1:2).EQ.'ON') THEN
C line ON
               itmp = +1
            ELSE IF(ctok(1:2).EQ.'OF') THEN
C line OFF
               itmp = 0
            ELSE IF(ISNUM(ctok,ltok).NE.0) THEN
C line #
               itmp = FPNUM(ctok, ltok, Ier)
               IF(ABS(itmp).LE.1) GOTO 590
               CALL GTPEEK(ctok,ltok)
            ELSE
               GOTO 590
            END IF
C get next token.
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
            IF(ltok.GE.2 .AND. ctok(1:2).EQ.'ON') THEN
C Skip optional ON, as in line Step ON..., or line # ON...
               CALL GTCHAR(ctok,ltok)
            END IF
  370       CALL IRANGE(ctok,ltok,1,MXGRP-MXMOD,ILO,IHI,Ier)
            ILO = MAX(1,MIN(ILO,MXGRP-MXMOD))
            IHI = MAX(1,MIN(IHI,MXGRP-MXMOD))
            DO ig= ILO,IHI
               line(ig) = itmp
            END DO
            CALL GTCHAR(ctok,ltok)
            IF(ltok.GT.0) GOTO 370
         ELSE IF(ctok(1:2).EQ.'LO') THEN
C- LOg ---------------------------------------------------------------
            CALL PLOGER(ctok, ltok, idoall, icwin, MXWIN,
     :        CXOPT, LOGX, CYOPT, LOGY, Ier)
            IF(Ier.NE.0) GOTO 590
         ELSE IF(ctok(1:2).EQ.'LS') THEN
C- LS (line Style) ---------------------------------------------------
            CALL GTCHAR(ctok,ltok)
            IF ( ctok(1:1).EQ.'?' ) THEN
               CALL PLTSLS(-1)
               GOTO 100
            END IF
            new = 1
            IF ( ltok.GT.0 ) new = FPNUM(ctok,ltok,Ier)
            CALL GTPEEK(ctok,ltok)
            CALL UPC(ctok)
            IF ( ctok(1:1).EQ.'O' ) THEN
C Skip optional ON/OFf.
               CALL GTCHAR(ctok,ltok)
            END IF
C LS # [ON]
  380       CALL GTCHAR(ctok, ltok)
            IF(ltok.GT.0) THEN
               CALL UPC(ctok)
               IF(ctok(1:1).EQ.'G') THEN
                  IGLS = new
               ELSE IF(ctok(1:1).EQ.'M') THEN

                  lsty(MXGRP-MXMOD+ICMOD) = new
               ELSE
                  CALL IRANGE(ctok,ltok,1,MXGRP-MXMOD,ILO,IHI,Ier)
                  ILO = MAX(1,MIN(ILO,MXGRP-MXMOD))
                  IHI = MAX(1,MIN(IHI,MXGRP-MXMOD))
                  DO I = ILO, IHI
                     lsty(I) = new
                  END DO
               END IF
               GOTO 380
            END IF
C---
C- LW (line Width) ---------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'LW') THEN
            TMP = 0.
            CALL GTREAL(TMP,Ier)
            IF(TMP.LT.0. .AND. 15.LT.TMP) THEN
               WRITE(*,*) 'LW not changed, must be in range of 1 to 15.'
               GOTO 100
            END IF
            CALL GTPEEK(ctok,ltok)
            IF(ltok.EQ.0) THEN
               WIDTH = TMP
            ELSE
               CALL UPC(ctok)
               IF(ctok(1:1).EQ.'O') THEN
C Skip optional ON/OFf.
                  CALL GTCHAR(ctok,ltok)
               END IF
C LW # [ON] #
  390          CALL GTCHAR(ctok,ltok)
               IF(ltok.GT.0) THEN
                  CALL UPC(ctok)
                  IF(ctok(1:1).EQ.'M') THEN
                     widlin(MXGRP-MXMOD+ICMOD) = TMP
                  ELSE
                     CALL IRANGE(ctok,ltok,1,MXGRP-MXMOD,ILO,IHI,Ier)
                     ILO = MAX(1,MIN(ILO,MXGRP-MXMOD))
                     IHI = MAX(1,MIN(IHI,MXGRP-MXMOD))
                     DO I = ILO, IHI
                        widlin(I) = TMP
                     END DO
                  END IF
                  GOTO 390
               END IF
            END IF
         END IF
C---
C- M -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'M') THEN
         IF(ctok(1:2).EQ.'MA') THEN
C- MArker ------------------------------------------------------------
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
C- First get the marker type.
            IF(ISNUM(ctok,ltok).NE.0) THEN
               RMARK = FPNUM(ctok,ltok,Ier)
               CALL GTCHAR(ctok,ltok)
               CALL UPC(ctok)
            ELSE IF(ctok(1:1).EQ.'S') THEN
C MArker Size
               CALL GTREAL(TMP,Ier)
               TMP = MAX(0.,MIN(TMP,5.))
               CALL GTCHAR(ctok,ltok)
               CALL UPC(ctok)
               ILO = 1
               IHI = ngroup
               IF(ltok.GE.2 .AND. ctok(1:2).EQ.'ON') THEN
                  CALL GTINT(ILO, Ier)
                  IHI = ILO
                  CALL GTINT(IHI, Ier)
               END IF
               DO ig = ILO,IHI
                  szmark(ig) = TMP
               END DO
               GOTO 100
            ELSE IF(ctok(1:1).EQ.'?') THEN
               IF(IOPEN.NE.0) THEN
                  CALL PLTMAR
               ELSE
                  WRITE(*,*) 'No graphics device available.'
               END IF
               GOTO 100
            ELSE
               RMARK = NO
            END IF
C- Now get ON/OFf flag.
            IF(ctok(1:2).EQ.'ON') THEN
               itmp = 1
            ELSE IF(ctok(1:2).EQ.'OF') THEN
               itmp = 0
            ELSE
               GOTO 590
            END IF
C- Finally the vectors to mark
            CALL GTCHAR(ctok,ltok)
C---
  400       CONTINUE
            CALL IRANGE(ctok,ltok,1,MXGRP-MXMOD,ILO,IHI,Ier)
            ILO = MAX(1,MIN(ILO,MXGRP-MXMOD))
            IHI = MAX(1,MIN(IHI,MXGRP-MXMOD))
            DO ig= ILO,IHI
               ipmark(ig) = itmp
               IF(RMARK.NE.NO) imark(ig) = RMARK
            END DO
            CALL GTCHAR(ctok,ltok)
            IF(ltok.GT.0) GOTO 400
C---
C- MMaster -----------------------------------------------------------
         ELSE IF ( ctok(1:2).EQ.'MM' ) THEN
C MMaster iwin X|Y|XY wlist
            iwnum = 0
            CALL GTINT(iwnum, ier)
            IF ( iwnum.LE.0 .OR. MXWIN.LT.iwnum ) THEN
               WRITE(*,401) MXWIN
  401          FORMAT(' Window number must be in range 1 to ',I5,'.')
               GOTO 100
            END IF
            ctok = ' '
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF ( ctok(1:1).EQ.'O' ) THEN
               DO i=1,MXWIN
                  IF ( imaster(1,i).EQ.iwnum ) imaster(1,i) = 0
                  IF ( imaster(2,i).EQ.iwnum ) imaster(2,i) = 0
               END DO
               imaster(1,iwnum) = 0
               imaster(2,iwnum) = 0
               GOTO 100
            END IF

C- Find the list of slave windows
  410       CALL GTCHAR(cscr1, lscr1)
            IF ( lscr1.LE.0 ) THEN
               IF ( INDEX(ctok,'X').GT.0 ) THEN
                  imaster(1, iwnum) = -1
               END IF
               IF ( INDEX(ctok,'Y').GT.0 ) THEN
                  imaster(2, iwnum) = -1
               END IF
            ELSE
               CALL IRANGE(cscr1,lscr1,1,MXWIN,ILO,IHI,Ier)
               ILO = MAX(1,MIN(ILO,MXWIN))
               IHI = MAX(1,MIN(IHI,MXWIN))
               DO I = ILO,IHI
                  IF ( i.NE.iwnum ) THEN
                     IF ( INDEX(ctok,'X').GT.0 ) THEN
                        imaster(1, i) = iwnum
                        xyscal(1,i) = xyscal(1,iwnum)
                        xyscal(3,i) = xyscal(3,iwnum)
                     END IF
                     IF ( INDEX(ctok,'Y').GT.0 ) THEN
                        imaster(2, i) = iwnum
                        xyscal(2,i) = xyscal(2,iwnum)
                        xyscal(4,i) = xyscal(4,iwnum)
                     END IF
                  END IF
               END DO
               GOTO 410
            END IF
C---
C- MOdel -------------------------------------------------------------
         ELSE IF(ctok(1:2).EQ.'MO') THEN
            IF ( IOPEN.LT.0 ) CALL PLTTER('A')
            CALL GTPEEK(ctok,ltok)
            IF ( ISNUM(ctok,ltok).NE.0 ) THEN
               CALL GTINT(ICMOD, Ier)
               ICMOD = MIN(MAX(1,ICMOD),MXMOD)
               CALL GTPEEK(ctok, ltok)
               IF(ltok.LE.0) GOTO 100
            END IF
C
C Figure out MIN/MAX in both X and Y for window containing fit group.
            CALL FITVIS(ipwin, ngroup, ifitg(icmod))
            pmin(1) = MIN(xyscal(1,ipwin(ifitg(icmod))),
     &         XYSCAL(3,ipwin(ifitg(icmod))))
            pmax(1) = MAX(xyscal(1,ipwin(ifitg(icmod))),
     &         XYSCAL(3,ipwin(ifitg(icmod))))
            pmin(2) = MIN(xyscal(2,ipwin(ifitg(icmod))),
     &         XYSCAL(4,ipwin(ifitg(icmod))))
            pmax(2) = MAX(xyscal(2,ipwin(ifitg(icmod))),
     &         XYSCAL(4,ipwin(ifitg(icmod))))
C
C Pass subcommend to MODEL routine
            CALL GTREST(ctok,ltok)
            ctmp = 'MO '//ctok
            CALL MODEL(ctmp,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     :       ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
            IF ( nterm(ICMOD).GT.0 ) THEN
               ipwin(MXGRP-MXMOD+icmod) = ipwin(ifitg(icmod))
               ipmod(MXGRP-MXMOD+icmod) = icmod
               igrpos(1, MXGRP-MXMOD+icmod) = igrpos(1, ifitg(icmod))
               igrpos(2, MXGRP-MXMOD+icmod) = igrpos(2, ifitg(icmod))
               igrpos(3, MXGRP-MXMOD+icmod) = igrpos(3, ifitg(icmod))
               ipyer(MXGRP-MXMOD+icmod) = ipyer(ifitg(icmod))
               newmod = icmod
            END IF
         END IF
C---
C- Newpar ------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'N') THEN
C- If NO is combined with a operation (* / + -) then it will
C- be treated as a command.  Warn user by checking here.
         IF(ltok.GT.1 .AND. ctok(2:2).NE.'E') GOTO 590
         IF(nterm(ICMOD).LE.0) THEN
            WRITE(*,411)
  411       FORMAT(' No model defined.')
         ELSE
            IF(IOPEN.LT.0) CALL PLTTER('A')
            CALL GTREST(ctok,ltok)
            ctmp = 'NE '//ctok
            CALL MODEL(ctmp,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     &       ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
            newmod = icmod
         END IF
C---
C- Overlay -----------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'O') THEN
         ICLEAR = 0
         GOTO 610
C---
C- P -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'P') THEN
         IF(ctok(1:2).EQ.'PA') THEN
C- PAper -------------------------------------------------------------
            CALL GTREAL(PGPAPW, Ier)
            CALL GTREAL(PGPAPA, Ier)
            IF ( iopen.NE.0 ) THEN
               CALL PGPAP(PGPAPW/2.54,PGPAPA)
            END IF
         ELSE IF(ctok(1:2).EQ.'PR') THEN
C- PRompt ------------------------------------------------------------
            CALL GTCHAR(CPROM,LPROM)
C- Plot --------------------------------------------------------------
         ELSE
            CALL GTCHAR(ctok,ltok)
            CALL UPC(ctok)
            ICLEAR = 1
            IF(ltok.LE.0         ) GOTO 610
            IF(ctok(1:1).EQ.'A') THEN
               QALL = .TRUE.
            ELSE IF(ctok(1:1).EQ.'G') THEN
               QALL = .FALSE.
            ELSE IF(ctok(1:1).EQ.'O') THEN
C- Plot Overlay.  Plot all groups in window 1, full screen, numeric labs
               DO ig= 1,ngroup
                  IF ( ipwin(ig).GT.0 ) ipwin(ig) = 1
               END DO
               CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
               WINLOC(2,1) = 0.
               WINLOC(4,1) = 1.
               CXOPT(1)(5:5) = 'N'
C Restore top label of top window, to be master top label
               IF(icwin.NE.1 .AND. LENACT(CTLAB(icwin)).NE.0) THEN
                  CTLAB(1) = CTLAB(icwin)
                  CTLAB(icwin) = ' '
               END IF
C Restore file label of top window, to be master top label
               IF(icwin.NE.1 .AND. LENACT(CFNAM(icwin)).NE.0) THEN
                  CFNAM(1) = CFNAM(icwin)
                  CFNAM(icwin) = ' '
               END IF
C Restore X label of bottom window, to be master X label
               IF(icwin.NE.1 .AND. laswin.NE.0 ) THEN
                  IF ( LENACT(CXLAB(laswin)).NE.0 ) THEN
                     CXLAB(1) = CXLAB(laswin)
                     CXLAB(icwin) = ' '
                  END IF
               END IF
               icwin = 1
               LASWIN = 0
            ELSE IF(ctok(1:1).EQ.'V') THEN
C- Plot Vert.  NVERT counts the number of active windows.
               NVERT = 0
               itmp = 0
C Assign one window to each group that is being plotted.  For
C compatibility number the windows by the group being plotted in that
C window. Default lastwin to 1 in case there are no active windows.
               laswin = 1
               DO igroup = 1,MIN(ngroup,MXWIN)
                  IF ( ipwin(igroup).GT.0 ) THEN
                     IF(itmp.EQ.0) itmp = igroup
                     ipwin(igroup) = igroup
                     NVERT = NVERT+1
                     LASWIN = igroup
                  END IF
               END DO
C Move top label to top label of the top window.
               IF(icwin.NE.itmp .AND. LENACT(CTLAB(icwin)).NE.0) THEN
                  CTLAB(itmp) = CTLAB(icwin)
                  CTLAB(icwin) = ' '
               END IF
C Move file label to top label of the top window.
               IF(icwin.NE.itmp .AND. LENACT(CFNAM(icwin)).NE.0) THEN
                  CFNAM(itmp) = CFNAM(icwin)
                  CFNAM(icwin) = ' '
               END IF
C Move X label to be X label of bottom window.
               IF(icwin.NE.itmp .AND. LENACT(CXLAB(icwin)).NE.0) THEN
                  CXLAB(laswin) = CXLAB(icwin)
                  CXLAB(icwin) = ' '
               END IF
C Generate active window list.
               CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
C new current window
               icwin = itmp
               TMP = (BOXVP(4,1)-BOXVP(2,1))/NVERT
               itmp = NVERT
               DO iwnum = 1,MXWIN
                  IF(iactw(iwnum).GT.0) THEN
                     TMP1 = BOXVP(2,1)+TMP*itmp-TMP/2.
                     WINLOC(4,iwnum) = TMP1+1./(2.*NVERT)
                     WINLOC(2,iwnum) = TMP1-1./(2.*NVERT)
                     IF(itmp.GT.1) itmp = itmp-1
                     CXOPT(iwnum)(5:5) = ' '
                  END IF
               END DO
               CXOPT(LASWIN)(5:5) = 'N'
            ELSE IF(ctok(1:1).EQ.'Z') THEN
C Plot Zero
               CALL GTCHAR(ctok,ltok)
               CALL UPC(ctok)
               IF(ctok(1:2).EQ.'ON') THEN
                  IPZERO = 1
               ELSE IF(ctok(1:2).EQ.'OF') THEN
                  IPZERO = 0
               ELSE
                  GOTO 590
               END IF
            ELSE
               GOTO 590
            END IF
         END IF
C---
C- Rescale -----------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'R') THEN
         ICLEAR = 1
         CALL GTPEEK(ctok,ltok)
         CALL UPC(ctok)
         IF(ctok(1:1).EQ.'?') THEN
            IF(IOPEN.LT.0) CALL PLTTER('A')
            IF ( IGAP.EQ.0 ) THEN
               WRITE(*,421) RGAP,'NoErrors'
            ELSE
               WRITE(*,421) RGAP,'Errors'
            END IF
  421       FORMAT(' Current Gap = ',F7.3,5X,A,/,'Window',3X,
     :       'XLAB      XMIN        XMAX',9X,
     :       'YLAB      YMIN        YMAX')
            DO 440 iwnum = 1,MXWIN
               IF(iactw(iwnum).LE.0) GOTO 440
               ctok = CXLAB(iwnum)
               IF ( LENACT(ctok).LE.0 ) THEN
                  IF ( ixvec.NE.0 ) ctok = cglab(ixvec)
               END IF
               ctmp = CYLAB(iwnum)
               IF ( LENACT(ctmp).LE.0 ) THEN
                  igroup = IFGRP(ipwin, MXGRP-MXMOD, iwnum)
                  IF(igroup.EQ.0) igroup = 2
                  ctmp = cglab(igroup)
               END IF
               WRITE(*,431) iwnum,ctok,XYSCAL(1,iwnum),XYSCAL(3,iwnum),
     :           ctmp,XYSCAL(2,iwnum),XYSCAL(4,iwnum)
  431          FORMAT(1X,I3,' : ',A10,1PG11.4,',',G11.4,' : ',
     :            A10,G11.4,',',G11.4)
  440       CONTINUE
            GOTO 100
         ELSE IF(ctok(1:1).EQ.'X') THEN
C- R X
            CALL GTCHAR(ctok,ltok)
            iwnum = icwin
            IF(ltok.GT.1) THEN
C- R X#
               itmp = FPNUM(ctok(2:ltok),ltok-1,Ier)
               IF(1.LE.itmp .AND. itmp.LE.MXWIN) iwnum = itmp
            END IF
            CALL GTPEEK(ctmp,ltmp)
            CALL UPC(ctmp)
            IF(ltmp.LE.0) THEN
C- R X <CR> resets X-limits.
               XYSCAL(1,iwnum) = NO
               XYSCAL(3,iwnum) = NO
            ELSE IF(ctmp(1:1).EQ.'C') THEN
C- R X Curs
                CALL PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN,
     :                 LOGX, LOGY, iactw, iwadj, iwnum,
     :                 VIEWX, VIEWY, WINX, WINY, ctmp)
               XYSCAL(1,iwnum) = WINX
               CALL PLTCUR(WINLOC, BOXVP, XYSCAL, MXWIN,
     :                 LOGX, LOGY, iactw, iwadj, itmp,
     :                 VIEWX, VIEWY, WINX, WINY, ctmp)
               XYSCAL(3,iwnum) = WINX
            ELSE
C- R X # #
               CALL GTREAL(XYSCAL(1,iwnum),Ier)
               CALL GTREAL(XYSCAL(3,iwnum),Ier)
            END IF
            IF(idoall.NE.0) THEN
               DO I = 1,MXWIN
                  XYSCAL(1,I) = XYSCAL(1,iwnum)
                  XYSCAL(3,I) = XYSCAL(3,iwnum)
               END DO
            END IF
            IF(ltok.GT.1) GOTO 100
         ELSE IF(ctok(1:1).EQ.'Y') THEN
            CALL GTCHAR(ctok,ltok)
            iwnum = icwin
            IF(ltok.GT.1) THEN
C- R Y#
               itmp = FPNUM(ctok(2:ltok),ltok-1,Ier)
               IF(1.LE.itmp .AND. itmp.LE.MXWIN) iwnum = itmp
            END IF
            CALL GTPEEK(ctmp,ltmp)
            IF(ltmp.LE.0) THEN
C- R Y <CR>
               XYSCAL(2,iwnum) = NO
               XYSCAL(4,iwnum) = NO
            ELSE
C- R Y # #
               CALL GTREAL(XYSCAL(2,iwnum),Ier)
               CALL GTREAL(XYSCAL(4,iwnum),Ier)
            END IF
            IF(idoall.NE.0 .AND. iwnum.EQ.ixvec .AND. ltok.GT.1) THEN
               DO I = 1,MXWIN
                  XYSCAL(1,I) = XYSCAL(2,iwnum)
                  XYSCAL(3,I) = XYSCAL(4,iwnum)
               END DO
            END IF
            IF(ltok.GT.1) GOTO 100
         ELSE IF(ltok.LE.0) THEN
C- R <CR> resets both X and Y limits.
            XYSCAL(1,icwin) = NO
            XYSCAL(2,icwin) = NO
            XYSCAL(3,icwin) = NO
            XYSCAL(4,icwin) = NO
         ELSE
C- R # # # #
            CALL GTREAL(XYSCAL(1,icwin),Ier)
            CALL GTREAL(XYSCAL(3,icwin),Ier)
            CALL GTREAL(XYSCAL(2,icwin),Ier)
            CALL GTREAL(XYSCAL(4,icwin),Ier)
         END IF
         GOTO 600
C---
C- SCr ---------------------------------------------------------------
C Set Color Representation.
         ELSE IF(ctok(1:2).EQ.'SC') THEN
            CALL GTPEEK(ctmp, ltmp)
            IF ( ISNUM(ctmp, ltmp).EQ.0 ) THEN
               CALL GTCHAR(ctok, ltok)
               CALL UPC(ctok(:ltok))
               IF ( ctok(1:1).EQ.'B' ) THEN
                  ibcol = -1
               ELSE IF ( ctok(1:1).EQ.'D' ) THEN
                  ibcol = 0
               ELSE IF ( ctok(1:1).EQ.'W' ) THEN
                  ibcol = +1
               ELSE
                  GOTO 590
               END IF
               IF ( iopen.NE.0 ) THEN
                  CALL PLTOP1(ibcol)
               END IF
               GOTO 100
            END IF
            CALL GTINT(IND,Ier)
            CALL GTREAL(RED,Ier)
            CALL GTREAL(GRN,Ier)
            CALL GTREAL(BLU,Ier)
C If device has not been opened then force it open.
            IF ( IOPEN.EQ.0 ) THEN
               IF ( cpfile.EQ.' ' ) THEN
                  IF ( ITRANS.EQ.0 ) THEN
                     ITRANS = 1
                     CALL TRLOG('PGPLOT_TYPE',11,cpfile,LPFILE)
                  END IF
               END IF
               CALL PLTOPE(cpfile,ibcol,scrcol,cfont,pgpapw,pgpapa,Ier)
               IF(Ier.NE.0) THEN
                  cpfile = ' '
                  LPFILE = 0
                  GOTO 100
               END IF
               IOPEN = 1
               CALL PGQINF('TERMINAL',ctok,ltok)
               IF(ctok(1:1).EQ.'Y') IOPEN = -1
            END IF
            IF ( red.NE.NO ) THEN
C In case we have a pseudo color device, we update immediately.
               CALL PGSCR(IND,RED,GRN,BLU)
            END IF
            IF ( 0.LE.ind .AND. ind.LE.15 ) THEN
               scrcol(1,ind)=red
               scrcol(2,ind)=grn
               scrcol(3,ind)=blu
            END IF
            CALL PGUPDT
C---
C- SEt  --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'SE') THEN
         CALL GTCHAR(ctok, ltok)
         CALL UPC(ctok(:ltok))
         IF ( ctok(1:2).EQ.'LE' ) THEN
            CALL GTINT(itmp, ier)
            CALL wrqsl(itmp)
         ENDIF
C---
C- SHow --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'SH') THEN
         CALL wrqdl(itmp)
C---
C- SKip --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'SK') THEN
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF(ctok(1:1).EQ.'S' .OR. ctok(1:2).EQ.'ON') THEN
C- SKip Single
            IF ( ixvec.EQ.0 ) THEN
               WRITE(*,441)
  441          FORMAT(' Error: Not allowed to SKip linear plots.')
               GOTO 100
            END IF
            iskip = 1
            ipwin(ixvec) = ABS(ipwin(ixvec))
         ELSE IF(ctok(1:1).EQ.'D') THEN
C- SKip Double
            IF ( ixvec.EQ.0 ) THEN
               WRITE(*,441)
               GOTO 100
            END IF
            iskip = 2
            ipwin(ixvec) = ABS(ipwin(ixvec))
         ELSE IF(ctok(1:1).EQ.'O') THEN
C- SKip Off
            ipwin(ixvec) = -ipwin(ixvec)
            iskip = 0
         ELSE
            GOTO 590
         END IF
C---
C If skip actually changed, then make re-initialize data.
         IF ( iskip.NE.imnmx ) THEN
            DO I = 1,MXWIN
               XYSCAL(2,I) = NO
               XYSCAL(4,I) = NO
            END DO
C- If SKIP is on, then scan through X vector looking for breaks.
            IF ( iskip.NE.0 ) THEN
               CALL PLTSKP(Yray, Iery, Mxrow, Npts, Nvec, MXGRP-MXMOD,
     &             iskip, ixvec, ngroup, igrpos, ipyer, ipwin)
               DO I = 1,MXWIN
                  XYSCAL(1,I) = NO
               END DO
            ELSE
C Skip has been turn off
               ngroup = MIN(Nvec,MXGRP-MXMOD)
               DO ig= 1,ngroup
                  IF ( ipwin(ig).LT.0 .AND. ig.NE.ixvec ) THEN
                     ipwin(ig)=ABS(ipwin(ig))
                  END IF
                  ipyer(ig) = MIN(Iery(ig),1)
                  CALL PLTXCN(ig, 1, 0)
                  itmp = iofset(ixvec, Iery, ixvec, Mxrow)
                  CALL PLTXCG(ig, 1, ixvec, itmp, iery)
                  igrpos(1,ig) = IOFSET(ig, Iery, Nvec, Mxrow)
                  igrpos(2,ig) = Npts
                  igrpos(3,ig) = ig
               END DO
            END IF
            CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
         END IF
C---
C- STatistic ----------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'ST') THEN
C default to the group being fitted.
         itmp = ifitg(icmod)
         CALL FITVIS(ipwin, ngroup, itmp)
         CALL GTINT(itmp,Ier)
         CALL PLTXCC(Yray, 1, itmp, xt, ndim, iyoff)
         itmp = MIN( MAX(1,itmp), MXGRP-MXMOD)
         pmin(1) = MIN(XYSCAL(1,ipwin(itmp)),XYSCAL(3,ipwin(itmp)))
         pmax(1) = MAX(XYSCAL(1,ipwin(itmp)),XYSCAL(3,ipwin(itmp)))
         pmin(2) = MIN(XYSCAL(2,ipwin(itmp)),XYSCAL(4,ipwin(itmp)))
         pmax(2) = MAX(XYSCAL(2,ipwin(itmp)),XYSCAL(4,ipwin(itmp)))
         WRITE(*,451) itmp,pmin(1),pmax(1)
  451    FORMAT(' Group',I3,', from ',1PG11.4,' to ',G11.4)
         IF ( ndim.GT.1 ) WRITE(*,501) pmin(2),pmax(2)
  501    FORMAT('    and in Y by ',1PG11.4,' to ',G11.4)
         WRITE(*,*)
C
         CALL PLTXCE(Yray, 1, itmp, 1, xmerr, xperr)
         IF ( xperr.NE.0.0 ) THEN
            lerx = 1
         ELSE
            lerx = 0
         END IF
         lery = ipyer(itmp)
         CALL MOMENT(1, Yray, lery, Mxrow, itmp, igrpos(1,itmp),
     &         igrpos(2,itmp), pmin, pmax, TOT, Ier)
         WRITE(*,*)
         IF ( ixvec.EQ.0 .OR. lerx.GT.0 ) THEN
            IF ( ndim.EQ.1 ) THEN
               WRITE(*,*) '     Sum of Y*XDEL = ',TOT(14)
            ELSE
               WRITE(*,*) '     Sum of Y*XDEL*YDEL = ',TOT(14)
            END IF
         END IF
         IF ( TOT(1).GT.1.0 .AND. ndim.EQ.1 ) THEN
            DEM = (TOT(1)*TOT(6))*(TOT(1)*TOT(3))
            IF(DEM.LE.0.) THEN
               WRITE(*,*) 'ERROR--DEM = ',DEM
            ELSE
               COR = TOT(1)*TOT(7)/SQRT(DEM)
               WRITE(*,*) 'Correlation coeff. = ',COR
            END IF
         END IF
         WRITE(*,*)
C---
C- THaw --------------------------------------------------------------
      ELSE IF(ctok(1:2).EQ.'TH') THEN
         IF(nterm(ICMOD).LE.0) THEN
            WRITE(*,411)
         ELSE
            CALL GTREST(ctok,ltok)
            ctmp = 'TH '//ctok
            CALL MODEL(ctmp,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     &       ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
         END IF
C---
C- Time --------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'T') THEN
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF(ctok(1:2).EQ.'ON') ITIME = 1
         IF(ctok(1:2).EQ.'OF') ITIME = 0
C---
C- U -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'U') THEN
         IF(ctok(1:2).EQ.'UP') THEN
C- Upper -------------------------------------------------------------
            CALL GTREAL(Fnew, Ier)
C Skip the ON keyword
            CALL GTCHAR(ctmp, ltmp)
C- Now get [glist]
  500       CONTINUE
            CALL GTCHAR(ctok,ltok)
            IF(ltok.LE.0) GOTO 100
            CALL UPC(ctok)
            CALL IRANGE(ctok,ltok,1,ngroup,ILO,IHI,Ier)
            IF(ILO.LT.1 .OR. IHI.LT.1) GOTO 590
            ILO = MAX(1,MIN(ILO,ngroup))
            IHI = MAX(1,MIN(IHI,ngroup))
            DO ig = ILO, IHI
               flimit(ig) = Fnew
            END DO
            GOTO 500
         ELSE
C- Uncertainty -------------------------------------------------------
            CALL GTREST(ctok,ltok)
            ctmp = 'U '//ctok
            IF ( IOPEN.LT.0 ) CALL PLTTER('A')
C Check that user has fit something prior.
            IF ( nterm(icmod).LE.0 ) THEN
               WRITE(*,*) 'ERROR--Must first fit the data.'
               GOTO 100
            END IF
            CALL FIT(ctmp, ifitg(icmod), Yray, Mxrow, ngroup, icwin,
     :        ipwin, ipyer, ipwin, igrpos, XYSCAL, ICOMP(1,ICMOD),
     :        PVAL(1,ICMOD), PLIM(1,1,ICMOD), nterm(ICMOD))
         END IF
C---
C- V -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'V') THEN
         IF(ctok(1:2).EQ.'VE') THEN
C- VErsion -----------------------------------------------------------
            IF(IOPEN.LT.0) CALL PLTTER('A')
            CALL PLTVER(ctok, ltok)
            WRITE(*,*) 'PLT  version: '//ctok(:ltok)
            CALL PGQINF('VERSION',ctok,ltok)
            WRITE(*,*) 'PGPLOT  ver.: '//ctok(:ltok)
         ELSE
C- Viewport ----------------------------------------------------------
            TMP = BOXVP(1,icwin)
            CALL GTREAL(TMP, Ier)
            IF ( TMP.LT.0. .OR. TMP.GE.1. ) THEN
               WRITE(*,*) 'Viewport coordinates must in range 0. to 1.'
               GOTO 100
            END IF
            BOXVP(1,icwin) = TMP
C
            TMP = BOXVP(2,icwin)
            CALL GTREAL(TMP, Ier)
            IF ( TMP.LT.0. .OR. TMP.GE.1. ) TMP = BOXVP(1,icwin)
            BOXVP(2,icwin) = TMP
C
            TMP = 1.-BOXVP(1,icwin)
            CALL GTREAL(TMP, Ier)
            BOXVP(3,icwin) = TMP
            IF(BOXVP(3,icwin).LE.BOXVP(1,icwin)) BOXVP(3,icwin) = 1.
C
            TMP = 1.-BOXVP(2,icwin)
            CALL GTREAL(TMP, Ier)
            BOXVP(4,icwin) = TMP
            IF(BOXVP(4,icwin).LE.BOXVP(2,icwin)) BOXVP(4,icwin) = 1.
            IF(idoall.NE.0) THEN
               DO iwnum = 1,MXWIN
                  BOXVP(1,iwnum) = BOXVP(1,icwin)
                  BOXVP(2,iwnum) = BOXVP(2,icwin)
                  BOXVP(3,iwnum) = BOXVP(3,icwin)
                  BOXVP(4,iwnum) = BOXVP(4,icwin)
               END DO
            END IF
         END IF
C---
C- W -----------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'W') THEN
         IF(ctok(1:2).EQ.'WI') THEN
C- WIndow ------------------------------------------------------------
            CALL GTCHAR(ctok, ltok)
            CALL UPC(ctok)
            IF(ISNUM(ctok,ltok).NE.0) THEN
               itmp = FPNUM(ctok, ltok, Ier)
               IF(itmp.LE.0 .OR. itmp.GT.MXWIN) THEN
                  WRITE(*,*) 'Window number must lie in range 1 to',
     :              MXWIN
                  GOTO 100
               END IF
               idoall = 0
               icwin = itmp
               LASWIN = 0
               GOTO 100
            ELSE IF(ctok(1:1).EQ.'A') THEN
C Window ALl, set the current window to be the lowest numbered window.
               DO i=MXWIN,1,-1
                  IF ( iactw(i).NE.0 ) icwin=i
               END DO
               idoall = 1
            ELSE
               GOTO 590
            END IF
         ELSE IF(ctok(1:2).EQ.'WM') THEN
C- WModel ------------------------------------------------------------
            IF(nterm(ICMOD).LE.0) THEN
               WRITE(*,411)
            ELSE
               CALL GTREST(ctmp,ltmp)
               ctok = 'WM '//ctmp
               CALL MODEL(ctok,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     & ICOMP(1,ICMOD),PVAL(1,ICMOD),PLIM(1,1,ICMOD),nterm(ICMOD))
            END IF
         ELSE IF(ctok(1:2).EQ.'WH' .OR. ctok(1:2).EQ.'WD' .OR.
     :          ctok(1:2).EQ.'WE') THEN
C- WData, or WEnviron, or WHead --------------------------------------
            CFILE = ' '
            CALL GTCHAR(CFILE,LFILE)
            NDIG = 0
            CALL GTINT(NDIG,Ier)
            IF ( ctok(1:2).NE.'WD' ) THEN
C WHead or WEnviron
               ctmp = CFILE
               CALL XTEND(ctmp,'.pco')
               CALL GETLUN(LUN)
               CALL OPENWR(LUN,ctmp,'new',' ','LIST',0,0,IOS)
               IF(IOS.NE.0) THEN
                  WRITE(*,*) 'PLT--Unable to open '//
     :                    ctmp(:LENACT(ctmp))
                  GOTO 100
               END IF
C First create the QDP commands
               IF(iskip.EQ.1) THEN
                  WRITE(LUN,11) 'SKIP SING'
               ELSE IF(iskip.EQ.2) THEN
                  WRITE(LUN,11) 'SKIP DOUB'
               END IF
C Now the global commands
               IF(CSIZE.NE.1.0) WRITE(LUN,551) 'CSIZ ',CSIZE
  551          FORMAT(A,F5.2)
               itmp = LENACT(CFONT)
               IF(itmp.GT.0) WRITE(LUN,11) 'FONT '//CFONT(:itmp)
               IF(RGAP.NE.0.025) WRITE(LUN,551) 'GAP  ',RGAP
               IF(IGAP.NE.0)     WRITE(LUN,551) 'GAP  Errors'
               IF( IAND(IPLAB,2).EQ.0 ) WRITE(LUN,551) 'LAB  PArm OFF'
               IF ( PYLAB.NE.2.0 ) WRITE(LUN,551) 'LAB  POS Y ',PYLAB
               IF(CYOPT(1)(8:8).EQ.'V') WRITE(LUN,11) 'LAB  ROT'
               IF(WIDTH.NE.1.0) WRITE(LUN,511) 'LWIDTH ',WIDTH
  511          FORMAT(A,F4.0)
               IF(ITIME.EQ.0)   WRITE(LUN,11) 'TIME OFF'
C Numbered labels in viewport coordinates.
               DO I = 1,MXLAB
                  IF ( ILABEL(1,I).LT.0 ) THEN
                     CALL PLTLAB(LUN, I, CLABEL, FLABEL, ILABEL)
                  END IF
               END DO
C For each model...  Note, we need to write out the model before any
C DGroup commands, since DG can point at models.
               DO im=1,MXMOD
                  IF ( ipwin(MXGRP-MXMOD+im).NE.0 ) THEN
                     WRITE(LUN,541) 'MODEL',im
                     WRITE(ctmp,541) 'WLUN ',LUN
                     CALL MODEL(ctmp,pmin,pmax,MXPAR,Cmd,Ncmd,Icmd,
     & ICOMP(1,im),PVAL(1,im),PLIM(1,1,im),nterm(im))
                     IF ( icol(MXGRP-MXMOD+im).NE.1 )
     :                WRITE(LUN,541) 'COL',icol(MXGRP-MXMOD+im),'ON MOD'
                     IF ( lsty(MXGRP-MXMOD+im).NE.1 )
     :                WRITE(LUN,541) 'LS ',lsty(MXGRP-MXMOD+im),'ON MOD'
                     IF ( widlin(MXGRP-MXMOD+im).NE.0.0 )
     :        WRITE(LUN,531) 'LW',widlin(MXGRP-MXMOD+im),'ON MOD'
                  END IF
               END DO
C For each group...
               CALL WRTCOL(Lun, icol, ngroup)
               DO ig=1, ngroup
                  IF ( ispecg(ig).EQ.3 ) THEN
                     WRITE(Lun, 513) ig,(isuba(k,ig),k=1,4)
  513                FORMAT('DG   ',I3,1X,4I7)
                  END IF
                  itmp = LENACT(cglab(ig))
                  IF ( itmp.GT.0 ) THEN
                     ltmp = 0
                     CALL CRAMI(ig, ctmp, ltmp)
                     WRITE(Lun, 521) ctmp(:ltmp), cglab(ig)(:itmp)
  521                FORMAT('LAB  G',A,1X,A)
                  END IF
                  IF(line(ig).NE.0) THEN
                     IF(line(ig).EQ. 1) THEN
                        WRITE(LUN,541) 'LIne ON',ig
                     ELSE IF(line(ig).EQ.-1) THEN
                        WRITE(LUN,541) 'LIne Step',ig
                     ELSE
                        WRITE(LUN,541) 'LIne',line(ig),'ON',ig
                     END IF
                  END IF
                  IF(lsty(ig).NE.1) WRITE(LUN,541) 'LS',lsty(ig),'ON',ig
                  IF ( widlin(ig).GT.0. )
     :               WRITE(LUN,531) 'LW',widlin(ig),'ON',ig
  531             FORMAT(A,1X,F5.1,1X,A,I4)
                  IF(ipmark(ig).GT.0) WRITE(LUN,541) 'MArk',
     :               imark(ig),  'ON', ig
                  IF(szmark(ig).NE.1.0) WRITE(LUN,531) 'MArk Size',
     :               szmark(ig), 'ON', ig
  541             FORMAT(A,I5,1X,A,I4)
               END DO
C
               DO iscr=0,15
                  IF ( scrcol(1,iscr).NE.NO ) THEN
                     WRITE(LUN,546) iscr,(scrcol(i,iscr),i=1,3)
  546                FORMAT('SCR ',I5,3F10.4)
                  END IF
               END DO
C
               i2dind = 0
               DO ic = 1,MX2D
                  IF ( icont(ic).NE.0 ) THEN
                     CALL WRTCON(LUN, ic, rlvcon(1,ic), icocon(1,ic),
     :                  ilscon(1,ic), rlwcon(1,ic), MXLEV, ctmp)
                     i2dind = ic
                  END IF
                  IF ( image(ic).NE.0 ) THEN
                     CALL WRTIMA(lun, ic, itfun(ic), icbar(ic),
     &         zscale(1,ic), cctnam(ic), ctmp)
                     i2dind = ic
                  END IF
               END DO
C Count number of active windows.
               IDOWIN = 0
               DO iwnum = 1,MXWIN
                  IF(iactw(iwnum).GT.0) THEN
                     IDOWIN = IDOWIN + 1
                  END IF
               END DO
               IF ( idowin.EQ.1 .AND. ixvec.EQ.0 ) THEN
                  ctmp = 'XAX  LIN '
                  ltmp = 10
                  CALL PLTXCC(Yray, 1, 1, x1, ndim, iyoff)
                  CALL PLTXCC(Yray, 2, 1, xt, ndim, iyoff)
                  CALL CRAMF(X1(1),ctmp,ltmp)
                  ltmp = ltmp+1
                  XDEL = xt(1)-x1(1)
                  CALL CRAMF(XDEL,ctmp,ltmp)
                  WRITE(LUN,11) ctmp(:ltmp)
                  IF ( ndim.GT.1 ) THEN
                     i1=isuba(3,i2dind)-isuba(1,i2dind)+1
                     CALL PLTXCC(Yray,1+i1,i2dind,xt,ndim,iyoff)
                     ctmp = 'YAX  LIN '
                     ltmp = 9
                     CALL CRAMF(x1(2), ctmp, ltmp)
                     ltmp = ltmp+1
                     CALL CRAMF(xt(2)-x1(2), ctmp, ltmp)
                     WRITE(LUN,11) ctmp(:ltmp)
                  END IF
                  DO im=1,MXMOD
                     IF ( ipwin(MXGRP-MXMOD+im).NE.0 ) THEN
                        WRITE(LUN,541) 'FIT ON',ifitg(im)
                     END IF
                  END DO
               END IF
C
               CSCR1 = 'LOG  X ON'
               LSCR1 = 9
               CSCR2 = 'LOG  Y ON'
               LSCR2 = 9
C For each window...
               DO 560 iwnum = 1,MXWIN
                  IF(iactw(iwnum).LE.0) GOTO 560
                  DO ig=1,ngroup
                     IF ( ipwin(ig).EQ.iwnum ) THEN
                        IF ( ispecg(ig).EQ.1 ) THEN
                           WRITE(Lun, 512) ig,'Model'
  512                      FORMAT('DG   ',I3,3X,A)
                        ELSE IF ( ispecg(ig).EQ.2 ) THEN
                           WRITE(Lun, 512) ig,'Resid'
                        END IF
                     END IF
                  END DO
                  IF ( IDOWIN.GT.1 ) THEN
                     WRITE(LUN,541) 'WIN ',iwnum
C write YPLOT command
                     ctmp = 'YPLOT '
                     ltmp = 6
                     i2dind=0
                     DO ig= 1, ngroup
                        IF(ipwin(ig).EQ.iwnum) THEN
                           ltmp = ltmp + 1
                           CALL CRAMI(ig,ctmp,ltmp)
                           IF ( ig.LE.MX2D .AND. i2dind.EQ.0 ) i2dind=ig
                        END IF
                     END DO
                     WRITE(LUN,11) ctmp(:ltmp)
                     IF ( i2dind.NE.0 .AND. ixvec.EQ.0 ) THEN
                        ctmp = 'XAX  LIN '
                        ltmp = 10
                        CALL PLTXCC(Yray, 1, i2dind, x1, ndim, iyoff)
                        CALL PLTXCC(Yray, 2, i2dind, xt, ndim, iyoff)
                        XDEL = xt(1)-x1(1)
                        CALL CRAMF(X1(1),ctmp,ltmp)
                        ltmp = ltmp+1
                        CALL CRAMF(XDEL,ctmp,ltmp)
                        WRITE(LUN,11) ctmp(:ltmp)
                        IF ( ndim.GT.1 ) THEN
                           i1=isuba(3,i2dind)-isuba(1,i2dind)+1
                           CALL PLTXCC(Yray,1+i1,i2dind,xt,ndim,iyoff)
                           ctmp = 'YAX  LIN '
                           ltmp = 9
                           CALL CRAMF(x1(2), ctmp, ltmp)
                           ltmp = ltmp+1
                           CALL CRAMF(xt(2)-x1(2), ctmp, ltmp)
                           WRITE(LUN,11) ctmp(:ltmp)
                        END IF
                     END IF
                     DO im=1,MXMOD
                        IF ( ipwin(MXGRP-MXMOD+im).EQ.iwnum ) THEN
                           WRITE(LUN,541) 'FIT ON',ifitg(im)
                        END IF
                     END DO
                  END IF
                  IF ( nfpl.NE.0 ) THEN
                     WRITE(LUN,541) 'FIT Plot',nfpl
                  END IF
C write LOCation command
                  ctmp = 'LOC '
                  ltmp = 4
                  DO i=1,4
                     ltmp = ltmp + 1
                     CALL CRAMF(WINLOC(i,iwnum),ctmp,ltmp)
                  END DO
                  WRITE(LUN,11) ctmp(:ltmp)
C write Viewport command
                  IF ( BOXVP(1,icwin).NE.0.1 .OR.
     :                 BOXVP(2,icwin).NE.0.1 .OR.
     :                 BOXVP(3,icwin).NE.0.9 .OR.
     :                 BOXVP(4,icwin).NE.0.9 ) THEN
                     ctmp = 'Vie '
                     ltmp = 4
                     DO i=1,4
                        ltmp = ltmp + 1
                        CALL CRAMF(boxvp(i,iwnum),ctmp,ltmp)
                     END DO
                     WRITE(LUN,11) ctmp(:ltmp)
                  END IF
C
                  IF(LOGX(iwnum).NE.0) THEN
                     LSCR1 = LSCR1+1
                     CALL CRAMI(iwnum, CSCR1, LSCR1)
                  END IF
                  IF(LOGY(iwnum).NE.0) THEN
                     LSCR2 = LSCR2+1
                     CALL CRAMI(iwnum, CSCR2, LSCR2)
                  END IF
C Numbered labels
                  DO I = 1,MXLAB
                     IF ( ILABEL(1,I).EQ.iwnum ) THEN
                        CALL PLTLAB(LUN, I, CLABEL, FLABEL, ILABEL)
                     END IF
                  END DO
C
                  IF (CXOPT(iwnum)(5:5).EQ.' ')
     :               WRITE(LUN,11) 'LAB  NX OFF'
C
                  itmp = LENACT(CFNAM(iwnum))
                  IF(itmp.GT.0) THEN
                     WRITE(LUN,11) 'LAB  F  '//CFNAM(iwnum)(:itmp)
                  ELSE
                     WRITE(LUN,11) 'LAB  F'
                  END IF
                  itmp = LENACT(CTLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  T  '//
     :               CTLAB(iwnum)(:itmp)
                  itmp = LENACT(COTLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  OT '//
     :              COTLAB(iwnum)(:itmp)
                  itmp = LENACT(CXLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  X  '//
     :              CXLAB(iwnum)(:itmp)
                  itmp = LENACT(COXLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  OX '//
     :              COXLAB(iwnum)(:itmp)
                  itmp = LENACT(CYLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  Y  '//
     :              CYLAB(iwnum)(:itmp)
                  itmp = LENACT(COYLAB(iwnum))
                  IF(itmp.GT.0) WRITE(LUN, 11) 'LAB  OY '//
     :              COYLAB(iwnum)(:itmp)
                  IF(idoall.EQ.0) THEN
                     CALL RESCAL('X', iwnum, XYSCAL(1,iwnum),
     :                 XYSCAL(3,iwnum), ctmp, ltmp)
                     WRITE(LUN,11) ctmp(:ltmp)
                  END IF
                  IF ( iwnum.NE.ixvec ) THEN
                     CALL RESCAL('Y', iwnum, XYSCAL(2,iwnum),
     :                 XYSCAL(4,iwnum), ctmp, ltmp)
                  ELSE
                     CALL RESCAL('Y', 0, XYSCAL(2,iwnum),
     :                 XYSCAL(4,iwnum), ctmp, ltmp)
                  END IF
                  WRITE(LUN,11) ctmp(:ltmp)
  560          CONTINUE
               IF ( IDOWIN.GT.1 ) WRITE(LUN,11) 'WIN  ALL'
               IF(idoall.NE.0) THEN
                  CALL RESCAL('X', 0, XYSCAL(1,1), XYSCAL(3,1),
     :              ctmp, ltmp)
                  WRITE(LUN,11) ctmp(:ltmp)
               END IF
               IF(LSCR1.GT.9) WRITE(LUN,11) CSCR1(:LSCR1)
               IF(LSCR2.GT.9) WRITE(LUN,11) CSCR2(:LSCR2)
               CLOSE(UNIT = LUN)
               CALL FRELUN(LUN)
            END IF
C---
            IF(ctok(1:2).NE.'WH') THEN
C WData or WEnviron
               ctmp = CFILE
               CALL XTEND(ctmp,'qdp')
               CALL GETLUN(LUN)
               CALL OPENWR(LUN,ctmp,'new',' ','LIST',0,0,IOS)
               IF(IOS.NE.0) THEN
                  WRITE(*,*) 'PLT--Unable to open '//
     :                    ctmp(:LENACT(ctmp))
                  GOTO 100
               END IF
C- First write out the READ xERR commands.
               ctok = ' '
               ltok = 0
               ctmp = ' '
               ltmp = 0
               DO igroup = 1,Nvec
                  IF(Iery(igroup).GT.0) THEN
                     IF(Iery(igroup).EQ.1) THEN
                        IF(ltok.EQ.0) THEN
                           ctok = 'READ SERR'
                           ltok = 9
                        END IF
                        ltok = ltok+1
                        CALL CRAMI(igroup,ctok,ltok)
                     ELSE IF(Iery(igroup).EQ.2) THEN
                        IF(ltmp.EQ.0) THEN
                           ctmp = 'READ TERR'
                           ltmp = 9
                        END IF
                        ltmp = ltmp+1
                        CALL CRAMI(igroup,ctmp,ltmp)
                     END IF
                  END IF
               END DO
               IF(ltok.GT.0)   WRITE(LUN,11) ctok(:ltok)
               IF(ltmp.GT.0)   WRITE(LUN,11) ctmp(:ltmp)
               IF(LFILE.GT.0) THEN
                  ctmp = CFILE
                  CALL XTEND(ctmp,'.pco')
                  WRITE(LUN,11) '@'//ctmp(:LENACT(ctmp))
               END IF
               WRITE(LUN,11) '!'
C- Now write out the data.
               DO 570 I = 1,Npts
C                  CALL PLTXCC(Yray, 1, 1, xt, ndim, iyoff)
C                  IF ( iskip.EQ.0 .OR. xt(1).NE.NO ) THEN
CC- Do not jump to 570 if iskip.NE.0 (SKip ON) and X.EQ.NO
C                     IF(xt(1).LT.pmin .OR. xt(1).GT.pmax) GOTO 570
C                  END IF
                  CALL WRQDAT(Lun, Ndig, Yray(I), Iery, Mxrow, Nvec)
  570          CONTINUE
               CLOSE(UNIT = LUN)
               CALL FRELUN(LUN)
            END IF
         END IF
C---
C- Xaxis -------------------------------------------------------------
      ELSE IF(ctok(1:1).EQ.'X') THEN
         CALL GTCHAR(ctok,ltok)
         IF ( ISNUM(ctok,ltok).NE.0 ) THEN
            itmp = FPNUM(ctok,ltok,Ier)
            IF ( itmp.LE.0 .OR. itmp.GT.Nvec ) THEN
               WRITE(*,*) 'Xaxis must be a vector in range 1 to',
     &          Nvec,'.'
               GOTO 590
            END IF
            IF ( ixvec.EQ.itmp ) GOTO 100
            IF ( igrpos(1,itmp).LT.0 ) GOTO 590
C%%% This should be allowed...
            IF ( ipmod(itmp).NE.0 ) THEN
               WRITE(*,*) 'Model groups cannot be used here.'
               GOTO 100
            END IF
            IF ( ixvec.GT.0 .AND. icol(ixvec).GT.0 )
     :        ipwin(ixvec) = ABS(ipwin(ixvec))
            ixvec = itmp
            IF ( iskip.EQ.0 ) THEN
               ipwin(ixvec) = -ABS(ipwin(ixvec))
               ngroup = MIN(Nvec,MXGRP-MXMOD)
               CALL GTCHAR(ctmp, ltmp)
               IF ( ltmp.LE.0 ) THEN
C XAX #  means do all groups
                  DO igroup = 1,ngroup
                     CALL PLTXCG(igroup,1,ixvec,igrpos(1,ixvec),Iery)
                     CALL PLTXCP(igroup, 1, ipyer(ixvec))
                     xymnmx(1,igroup) = xymnmx(2,ixvec)
                     xymnmx(3,igroup) = xymnmx(4,ixvec)
                     ermnmx(1,igroup) = ermnmx(3,ixvec)
                     ermnmx(2,igroup) = ermnmx(4,ixvec)
                  END DO
               ELSE
C XAX # ON list  means only do the listed groups
  573             CONTINUE
                  CALL GTCHAR(ctmp, ltmp)
                  IF ( ltmp.GT.0 ) THEN
                     igroup = FPNUM(ctmp, ltmp, ier)
                     IF ( ier.NE.0 .OR. igroup.GT.ngroup ) GOTO 573
                     CALL PLTXCG(igroup,1,ixvec,igrpos(1,ixvec),Iery)
                     CALL PLTXCP(igroup, 1, ipyer(ixvec))
                     xymnmx(1,igroup) = xymnmx(2,ixvec)
                     xymnmx(3,igroup) = xymnmx(4,ixvec)
                     ermnmx(1,igroup) = ermnmx(3,ixvec)
                     ermnmx(2,igroup) = ermnmx(4,ixvec)
                     GOTO 573
                  END IF
               END IF
C Reset X-scales that may have changed
               IF ( idoall.NE.0 ) THEN
                  DO iwnum = 1,MXWIN
                     XYSCAL(1,iwnum) = XYSCAL(2,ixvec)
                     XYSCAL(3,iwnum) = XYSCAL(4,ixvec)
                  END DO
               ELSE
                  XYSCAL(1,icwin) = XYSCAL(2,ixvec)
                  XYSCAL(3,icwin) = XYSCAL(4,ixvec)
               END IF
            ELSE
               CALL PLTSKP(Yray, Iery, Mxrow, Npts, Nvec, MXGRP-MXMOD,
     &            iskip, ixvec, ngroup, igrpos, ipyer, ipwin)
               DO iwnum = 1,MXWIN
                  XYSCAL(1,iwnum) = NO
               END DO
               imnmx = -1
            END IF
         ELSE
            CALL UPC(ctok)
            IF(ctok(1:1).EQ.'L') THEN
C Xaxis linear
               OFF = NO
               SLOP = NO
               CALL GTREAL(OFF,Ier)
               CALL GTREAL(SLOP,Ier)
               ngroup = MIN(Nvec,MXGRP-MXMOD)
               IF ( idoall.NE.0 ) THEN
                  DO ig= 1,ngroup
                     CALL PLTXCL(ig, 1, off, slop)
                  END DO
               ELSE
C Only change the X-axis for the plot groups that appear in the
C current window.
                  DO ig= 1,ngroup
                     IF ( ipwin(ig).EQ.icwin ) THEN
                        CALL PLTXCL(ig, 1, off, slop)
                     END IF
                  END DO
               END IF
               IF ( ixvec.GT.0 .AND. icol(ixvec).GT.0 )
     :           ipwin(ixvec) = ABS(ipwin(ixvec))
               ixvec = 0
               iskip = 0
               IF(idoall.NE.0) THEN
                  DO iwnum = 1,MXWIN
                     XYSCAL(1,iwnum) = NO
                  END DO
               ELSE
                  XYSCAL(1,icwin) = NO
               END IF
               imnmx = -1
            ELSE IF(ctok(1:1).EQ.'O') THEN
C Xaxis Offset
               TMP = 0.
               CALL GTREAL(TMP,Ier)
               DO ig= 1,ngroup
                  CALL PLTXCO(ig, 1, TMP)
               END DO
            ELSE
               GOTO 590
            END IF
         END IF
C---
      ELSE IF(ctok(1:1).EQ.'Y') THEN
C- YAxis -------------------------------------------------------------
         IF ( ctok(1:2).EQ.'YA' ) THEN
            ctok(1:2) = ' '
            CALL GTPEEK(ctok, ltok)
            CALL UPC(ctok)
            IF ( ctok(1:1).EQ.'L' ) THEN
C YAxis LIN # #
               yoff = NO
               yslop = NO
               CALL GTCHAR(ctok,ltok)
               CALL GTREAL(YOFF,Ier)
               CALL GTREAL(YSLOP,Ier)
               IF ( idoall.NE.0 ) THEN
                  DO ig= 1,ngroup
                     CALL PLTXCL(ig, 2, yoff, yslop)
                  END DO
               ELSE
C Only change the Y-axis for the plot groups that appear in the
C current window.
                  DO ig= 1,ngroup
                     IF ( ipwin(ig).EQ.icwin ) THEN
                        CALL PLTXCL(ig, 2, yoff, yslop)
                     END IF
                  END DO
               END IF
               imnmx = -1
            ELSE
C YAxis [ON] [glist] (no longer a good idea)
               WRITE(*,*) 'Please use the Yplot instead of YAxis!'
               IF(ctok(1:2).EQ.'ON') CALL GTCHAR(ctok, ltok)
  575          CALL GTCHAR(ctmp, ltmp)
               IF(ltmp.GT.0) THEN
                  itmp = FPNUM(ctmp, ltmp, Ier)
                  IF(itmp.GT.0 .AND. itmp.LE.MXGRP-MXMOD) THEN
                     ipwin(itmp) = icwin
                     CALL PLTXCC(Yray, 1, itmp, xt, ndim, iyoff)
                     IF ( ndim.EQ.1 ) THEN
                        icol(itmp) = ABS(icol(itmp))
                     ELSE
                        ic2dg = itmp
                     END IF
                     GOTO 575
                  END IF
               END IF
               CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
               GOTO 100
            END IF
         ELSE
C- Yplot -------------------------------------------------------------
            CALL GTPEEK(ctok, ltok)
            CALL UPC(ctok)
            new = icwin
            IF ( ctok(1:2).EQ.'OF' ) THEN
C Yplot OFf (skip the OFf token)
               CALL GTCHAR(ctok, ltok)
               new = 0
            ELSE IF ( ctok(1:2).EQ.'ON' ) THEN
C Yplot ON (skip the ON token)
               CALL GTCHAR(ctok, ltok)
            END IF
            CALL GTCHAR(ctok, ltok)
C repeat ... until
  580       CONTINUE
            CALL UPC(ctok)
            CALL IRANGE(ctok,ltok,1,ngroup,ILO,IHI,Ier)
            IF ( ILO.LT.1 .OR. IHI.LT.1 ) GOTO 590
            ILO = MAX(1,MIN(ILO,MXGRP))
            IHI = MAX(1,MIN(IHI,MXGRP))
            DO igroup = ILO, IHI
               IF ( igrpos(1,igroup).GE.0 ) THEN
                  ipwin(igroup) = new
                  IF ( new.GT.0 ) THEN
                     icol(igroup) = ABS(icol(igroup))
                  ELSE
                     icol(igroup) = -ABS(icol(igroup))
                  END IF
                  CALL PLTXCC(Yray, 1, igroup, xt, ndim, iyoff)
                  IF ( ndim.GT.1 ) THEN
C Define the default contour and image plot group
                     ic2dg = igroup
                  END IF
               END IF
            END DO
            CALL GTCHAR(ctok,ltok)
            IF(ltok.GT.0) GOTO 580
            CALL ACTWIN(ipwin,ngroup,MXWIN,iactw)
C End 'Y' commands
         END IF
      ELSE
         GOTO 590
      END IF
      GOTO 100
C---
C- tough luck --------------------------------------------------------
  590 WRITE(*,591) ctok(:ltok)
  591 FORMAT(' Illegal command.   At token = ',A/
     :    ' Type HELP to get command list.')
      GOTO 100
C---
C*********************************************************************
  600 CONTINUE
      IF(icmd.GE.0) GOTO 100
      ICLEAR = 1
C---
  610 CONTINUE
      IF ( cpfile.EQ.' ' ) THEN
         IF ( ITRANS.EQ.0 ) THEN
            ITRANS = 1
            CALL TRLOG('PGPLOT_TYPE',11,cpfile,LPFILE)
         END IF
         IF ( LPFILE.EQ.0 ) THEN
            ctok = CXLAB(icwin)
            ltok = LENACT(ctok)
            IF ( ltok.LE.0 .AND. ixvec.GT.0 ) THEN
               ctok = cglab(ixvec)
               ltok = LENACT(ctok)
            END IF
            IF ( ltok.LE.0 .AND. laswin.NE.0 ) THEN
               ctok = CXLAB(laswin)
               ltok = LENACT(ctok)
            END IF
            ltok = MIN(ltok,24)
            iwnum = icwin
            DO I = MXWIN,1,-1
               IF ( iactw(iwnum).GT.0 ) iwnum = I
            END DO
            ctmp = CYLAB(iwnum)
            ltmp = LENACT(ctmp)
            IF ( ltmp.LE.0 ) THEN
               igroup = IFGRP(ipwin, MXGRP-MXMOD, iwnum)
               IF ( igroup.GT.0 ) THEN
                  ctmp = cglab(igroup)
                  ltmp = LENACT(ctmp)
               END IF
            END IF
            ltmp = MIN(ltmp,24)
            IF ( ltmp.GT.0 .OR. ltok.GT.0 ) THEN
               WRITE(*,621) ctmp(:ltmp),ctok(:ltok)
  621          FORMAT(' To plot ',A,' vs. ',A,', please enter')
            ELSE
               WRITE(*,631)
  631          FORMAT(' To produce plot, please enter')
            END IF
            CALL PLTPRO(cpfile,Ier)
            IF(Ier.NE.0) GOTO 900
            IF(cpfile.EQ.' ') GOTO 950
            QHARD = .FALSE.
         END IF
      END IF
C---
      CALL DSCALE(Yray, Iery, Mxrow, MXWIN, MX2D, ngroup, MXPAR,
     &    igap, rgap, idoall, iskip, newmod, iactw, logx, logy, imaster,
     &    igrpos, ipwin, icont, image, ipmod, icomp, pval, nterm,
     &    imnmx, xymnmx, ermnmx, xyscal)
      ipall = 1
C---
C No open plot device, go back to prompt.
  650 CONTINUE
      IF ( cpfile(1:1).EQ.'>' .OR. cpfile.EQ.' ' ) GOTO 100
C---
      IF ( IOPEN.EQ.0 ) THEN
         CALL PLTOPE(cpfile,ibcol,scrcol,cfont,pgpapw,pgpapa,Ier)
         IF(Ier.NE.0) THEN
            cpfile = ' '
            LPFILE = 0
            GOTO 610
         END IF
         IOPEN = 1
         CALL PGQINF('TERMINAL',ctok,ltok)
         IF(ctok(1:1).EQ.'Y') IOPEN = -1
      END IF
      CALL PGBBUF
      IF ( IOPEN.LT.0 ) CALL PLTTER('G')
      IF(ICLEAR.NE.0) CALL PLTCLR
C---
C- Plot line, error bars or symbols.
      DO 790 iwnum = 1,MXWIN
         IF ( iactw(iwnum).LE.0 ) GOTO 790
         IF ( XYSCAL(2,iwnum).EQ.XYSCAL(4,iwnum) ) THEN
            WRITE(*,661) 'YMIN = YMAX',iwnum
  661       FORMAT(' ERROR--',A,' in window',I4)
            GOTO 790
         END IF
         IF ( XYSCAL(1,iwnum).EQ.XYSCAL(3,iwnum )) THEN
            WRITE(*,661) 'XMIN = XMAX',iwnum
            GOTO 790
         END IF
         CALL PLTSVW(BOXVP, WINLOC, XYSCAL, LOGX, LOGY, IWADJ, iwnum)
         WXMIN = XYSCAL(1,iwnum)
         WYMIN = XYSCAL(2,iwnum)
         WXMAX = XYSCAL(3,iwnum)
         WYMAX = XYSCAL(4,iwnum)
         pmin(1) = MIN(WXMIN,WXMAX)
         pmax(1) = MAX(WXMIN,WXMAX)
         IF ( GRIDX(iwnum).GT.0. ) THEN
            XSPAC = (WXMAX-WXMIN)/GRIDX(iwnum)
         ELSE
            XSPAC = 0.
         END IF
         IF ( GRIDY(iwnum).GT.0. ) THEN
            YSPAC = (WYMAX-WYMIN)/GRIDY(iwnum)
         ELSE
            YSPAC = 0.
         END IF
         CALL PLTSCI(1)
         CALL PLTSLS(1)
         CALL PLTCS(CSIZE)
         CALL PLTSLW(WIDTH)
         IF ( ipall.NE.0 ) THEN
            IF ( (IGCOL.EQ.1 .AND. IGLS.EQ.1 ) .OR.
     :       CXOPT(iwnum)(7:7).NE.'G') THEN
               CALL PGBOX(CXOPT(iwnum),XSPAC,NSUBX(iwnum),
     :                    CYOPT(iwnum),YSPAC,NSUBY(iwnum))
            ELSE
               CXOPT1 = CXOPT(iwnum)
               CXOPT1(7:7) = ' '
               CYOPT1 = CYOPT(iwnum)
               CYOPT1(7:7) = ' '
               CALL PGBOX(CXOPT1,XSPAC,NSUBX(iwnum),
     :                    CYOPT1,YSPAC,NSUBY(iwnum))
               CXOPT1 = 'G'
               CYOPT1 = 'G'
               CALL PLTSCI(IGCOL)
               CALL PLTSLS(IGLS)
               CALL PGBOX(CXOPT1,XSPAC,NSUBX(iwnum),
     :                    CYOPT1,YSPAC,NSUBY(iwnum))
            END IF
         END IF
C
C We now loop over all groups, looking for the groups to plot in the
C current window.
         DO 770 igroup = 1,MXGRP
            IF ( ipwin(igroup).NE.iwnum ) GOTO 770
C Note, i2drow is used by both 1D and 2D plots.
            icyvec = igrpos(3,igroup)
            IF ( Iery(icyvec).GT.0 ) THEN
               i2drow = Mxrow*(Iery(icyvec)+1)
            ELSE
               i2drow = Mxrow
            END IF
C- If only plotting model skip ahead.
            IF ( ipall.EQ.0 .OR. ipmod(igroup).GT.0 ) GOTO 700
C- If this group is to plotted as contour/image then don't do 1D plot.
            IF ( igroup.LE.MX2D ) THEN
               IF ( icont(igroup).GT.0 .OR.
     &           image(igroup).GT.0 ) GOTO 700
            END IF
C- If not doing the 1D plot then skip over.
            IF ( IPZERO.EQ.0 .AND. icol(igroup).LE.0 ) GOTO 700
            IF ( widlin(igroup).EQ.0.0 ) THEN
               CALL PLTSLW(WIDTH)
            ELSE
               CALL PLTSLW(widlin(igroup))
            END IF
C---
            idolin = line(igroup)
            CALL PLTXCE(Yray, 1, igroup, 1, xmerr, xperr)
            lery = ipyer(igroup)
            IDOER = 0
            IF ( xperr.NE.0 .OR. lery.NE.0 ) IDOER = 1
            IF ( IDOER.EQ.0 .AND. idolin.EQ.0 .AND.
     :       ipmark(igroup).EQ.0 ) idolin = 1
            CALL PLTSCI(icol(igroup))
            CALL PLTSLS(lsty(igroup))
C---
C- line plot.
            IF ( idolin.NE.0 ) THEN
               IF ( ABS(idolin).GT.1 ) THEN
C Plot a smooth line
                  IF ( idolin.LT.0 ) THEN
                     XLO = xymnmx(1,igroup)
                     XHI = xymnmx(3,igroup)
                  ELSE
                     XLO = pmin(1)
                     XHI = pmax(1)
                  END IF
                  IF(LOGX(iwnum).NE.0) XLO = LOG10(MAX(XLO,RMNLOG))
                  IF(LOGX(iwnum).NE.0) XHI = LOG10(MAX(XHI,RMNLOG))
                  XDEL = (XHI-XLO)/ABS(idolin)
                  new = 1
                  XCEN = XLO
                  INITAK = 1
                  DO I = 0,ABS(idolin)
                     XT(1) = XCEN
                     IF(LOGX(iwnum).NE.0 .AND.
     :                 XCEN.LT.1.E35) XT(1) = 10.**XCEN
                     CALL AKINTE(XCEN, Yray, igroup, igrpos(1,igroup),
     &                  igrpos(2,igroup), 0, INITAK, YT)
                     IF(LOGY(iwnum).NE.0) YT = LOG10(MAX(YT,RMNLOG))
                     IF(new.NE.0) THEN
                        new = 0
                        CALL PGMOVE(XCEN,YT)
                     ELSE
                        CALL PGDRAW(XCEN,YT)
                     END IF
                     XCEN = XCEN+XDEL
                  END DO
               ELSE
C Plot line only at grid points
                  IFIRST = 1
                  CALL PLTXCC(Yray, 1, igroup, x1, ndim, iyoff)
                  CALL PLTXCC(Yray, 2, igroup, xt, ndim, iyoff)
                  XLAS = 2.*x1(1)-xt(1)
                  iy0 = igrpos(1,igroup)
                  DO 680 I= 1,igrpos(2,igroup)
                     CALL PLTXCC(Yray, i, igroup, xt, ndim, iyoff)
                     XCEN = xt(1)
                     YCEN = Yray(iy0+iyoff)
                     IF ( ipmod(igroup).LT.0 .AND. ycen.NE.NO ) THEN
C ipmod.LT.0 means plot residuals
                        itmp = ABS(ipmod(igroup))
                        ycen = ycen - FNFIT(xcen,ICOMP(1,itmp),
     &                   PVAL(1,itmp),nterm(itmp))
                     END IF
                     IBAD = 0
                     IF ( .NOT.QALL .AND.
     :                (YCEN.EQ.NO .OR. XCEN.EQ.NO) ) IBAD = 1
                     IF ( XCEN.LT.pmin(1) .OR. XCEN.GT.pmax(1) ) IBAD=1
                     IF ( IBAD.NE.0 ) THEN
                        IFIRST = 1
                        GOTO 680
                     END IF
                     YT = YCEN
                     IF(LOGY(iwnum).NE.0) YT = LOG10(MAX(YCEN,RMNLOG))
                     IF ( idolin.EQ.1 ) THEN
C Do the line ON command
                        XT(1) = XCEN
                        IF ( LOGX(iwnum).NE.0 ) XT(1) =
     &                              LOG10(MAX(XT(1),RMNLOG))
                        IF ( IFIRST.NE.0 ) THEN
                           CALL PGMOVE(XT(1),YT)
                           IFIRST = 0
                        ELSE
                           CALL PGDRAW(XT(1),YT)
                        END IF
                     ELSE
C Do the line Stepped command
                        CALL PLTXCE(Yray, i, igroup, 1, xmerr, xperr)
                        IF ( xperr.LE.0.0 ) THEN
C If PLTXCE does not return an error, then we must guess an appropiate
C step size.  We use half the distance between the data points, with
C obvious care that we don't fall off the ends of the array.
                           xmerr = 0.0
                           IF ( i.LT.igrpos(2,igroup) ) THEN
                              CALL PLTXCC(Yray,i+1,igroup,x1,ndim,iyoff)
                              IF ( x1(1).NE.NO ) THEN
                                 IF ( x1(1).GT.xt(1) ) THEN
                                    xperr = (x1(1) - xt(1))/2.
                                 ELSE
                                    xmerr = (x1(1) - xt(1))/2.
                                 END IF
                              END IF
                           END IF
                           IF ( i.GT.1 ) THEN
                              CALL PLTXCC(Yray,i-1,igroup,x1,ndim,iyoff)
                              IF ( x1(1).NE.NO ) THEN
                                 IF ( xt(1).GT.x1(1) ) THEN
                                    xmerr = (x1(1) - xt(1))/2.
                                 ELSE
                                    IF ( xperr.LE.0.0 ) THEN
                                       xperr = (x1(1) - xt(1))/2.
                                    END IF
                                 END IF
                              END IF
                           END IF
                           IF ( xperr.LE.0.0 ) xperr=-xmerr
                           IF ( xmerr.GE.0.0 ) xmerr=-xperr
                        END IF
                        IF ( XCEN.GT.XLAS ) THEN
                           XT(1) = XCEN+xmerr
                        ELSE
C Allow for case where X coordinate decreases.
                           XT(1) = XCEN+xperr
                        END IF
C Allow up to +/-10 percent slop in the spacing, before breaking the line.
                        IF ( xperr.NE.0.0 .AND.
     &                   ABS(XT(1)-XLAS).GT.0.2*xperr ) THEN
                           IFIRST = 1
                        END IF
                        IF ( LOGX(iwnum).NE.0 ) XT(1) =
     &                                 LOG10(MAX(XT(1),RMNLOG))
                        IF ( IFIRST.NE.0 ) THEN
                           CALL PGMOVE(XT(1),YT)
                           IFIRST = 0
                        ELSE
                           CALL PGDRAW(XT(1),YT)
                        END IF
                        IF ( XCEN.GT.XLAS ) THEN
                           XT(1) = XCEN+xperr
                        ELSE
                           XT(1) = XCEN+xmerr
                        END IF
                        XLAS = XT(1)
                        IF ( LOGX(iwnum).NE.0 ) XT(1) =
     &                                 LOG10(MAX(XT(1),RMNLOG))
                        CALL PGDRAW(XT(1),YT)
                     END IF
  680             CONTINUE
               END IF
            END IF
C---
C- Plot Markers or errors.
            IF ( ipmark(igroup).GT.0 .OR. IDOER.NE.0 ) THEN
               YPERR = 0.
               YMERR = 0.
               IF(flimit(igroup).GT.0.) THEN
                  CALL PLTVTW(0.5, 0.50, TMP, TMP1)
                  CALL PLTVTW(0.5, 0.46, TMP, TMP2)
                  YLONG = TMP1-TMP2
               END IF
               CALL PLTCS(szmark(igroup))
               CALL PLTSMK(imark(igroup))
               iy0 = igrpos(1,igroup)
               DO 690 I= 1,igrpos(2,igroup)
                  CALL PLTXCC(Yray, i, igroup, xt, ndim, iyoff)
                  iyi = iy0 + iyoff
C%%% Fails for 2D case?
                  XCEN = xt(1)
                  YCEN = Yray(iyi)
                  IF ( ipmod(igroup).LT.0 .AND. ycen.NE.NO ) THEN
C ipmod.LT.0 means plot residuals
                     itmp = ABS(ipmod(igroup))
                     ycen = ycen - FNFIT(xcen,ICOMP(1,itmp),
     &                 PVAL(1,itmp),nterm(itmp))
                  END IF
                  IBAD = 0
                  IF ( .NOT.QALL .AND.
     &               (YCEN.EQ.NO .OR. XCEN.EQ.NO) ) IBAD = 1
                  IF ( XCEN.LT.pmin(1) .OR. XCEN.GT.pmax(1) ) IBAD = 1
                  IF ( IBAD.NE.0 ) GOTO 690
                  IF ( ipmark(igroup).NE.0 ) THEN
                     XT(1) = XCEN
                     IF(LOGX(iwnum).NE.0) XT(1) =
     &                              LOG10(MAX(XT(1),RMNLOG))
                     YT = YCEN
                     IF(LOGY(iwnum).NE.0) YT = LOG10(MAX(YT,RMNLOG))
                     CALL PLTPM(1,XT(1),YT)
                  END IF
                  IF ( IDOER.NE.0 ) THEN
C- Plot errors.
                     CALL PLTXCE(Yray, i, igroup, 1, xmerr, xperr)
                     XL = XCEN+xmerr
                     XH = XCEN+xperr
                     IF(LOGX(iwnum).NE.0) THEN
                        XL = LOG10(MAX(XL,RMNLOG))
                        XH = LOG10(MAX(XH,RMNLOG))
                        XCEN = LOG10(MAX(XCEN,RMNLOG))
                     END IF
C
                     IF ( lery.NE.0 ) THEN
                        IF ( lery.EQ.-2 ) THEN
                           YPERR = 1.+SQRT(0.75+ABS(YCEN))
                           YMERR = -YPERR
                        ELSE IF ( lery.EQ.-1 ) THEN
                           YPERR = SQRT(MAX(YCEN,1.))
                           YMERR = -YPERR
                        ELSE IF ( Iery(icyvec).EQ.1 ) THEN
                           YPERR = Yray(iyi+Mxrow)
                           YMERR = -YPERR
                        ELSE IF ( Iery(icyvec).EQ.2 ) THEN
                           YPERR = Yray(iyi+Mxrow)
                           YMERR = Yray(iyi+2*Mxrow)
                        END IF
                     END IF
C
                     IF ( flimit(igroup).GT.0. ) THEN
                        TMP = flimit(igroup)*YPERR
                        IF ( YCEN.LT.TMP ) THEN
                           YH = TMP
                           IF ( LOGY(iwnum).NE.0 ) THEN
                              YH = LOG10(MAX(YH,RMNLOG))
                              CALL PLTWTV(XCEN, YH, vxlas, vylas)
                              vylas=vylas-0.05
                              CALL PLTVTW(vxlas, vylas, tmp1, tmp2)
                              YL = tmp2
                           ELSE
                              YL = YH-MIN(TMP, YLONG)
                           END IF
                           CALL PGMOVE(XL,YH)
                           CALL PGDRAW(XH,YH)
                           CALL PGMOVE(XCEN,YH)
                           CALL PGDRAW(XCEN,YL)
                           GOTO 690
                        END IF
                     END IF
C
                     YL = YCEN+YMERR
                     YH = YCEN+YPERR
                     IF ( LOGY(iwnum).NE.0 ) THEN
                        YL = LOG10(MAX(YL,RMNLOG))
                        YH = LOG10(MAX(YH,RMNLOG))
                        YCEN = LOG10(MAX(YCEN,RMNLOG))
                     END IF
C
                     CALL PGMOVE(XL,YCEN)
                     IF(ipyer(igroup).LE.1) THEN
                        CALL PGDRAW(XH,YCEN)
                        CALL PGMOVE(XCEN,YL)
                        CALL PGDRAW(XCEN,YH)
                     ELSE
                        CALL PGDRAW(XCEN,YH)
                        CALL PGDRAW(XH,YCEN)
                        CALL PGDRAW(XCEN,YL)
                        CALL PGDRAW(XL,YCEN)
                     END IF
C End plot errors
                  END IF
  690          CONTINUE
               CALL PLTCS(CSIZE)
            END IF
C---
C Now do any 2D plots in the current window
  700       CONTINUE
            IF ( igroup.GT.MX2D ) GOTO 750
            IF ( icont(igroup).GT.0 .OR.
     &           image(igroup).GT.0 ) THEN
               CALL PLTXCC(Yray, 0, igroup, xt, ndim, iyoff)
               CALL PLTXCC(Yray, 1, igroup, x1, ndim, iyoff)
               tmp = x1(1)-xt(1)
               ca=COS(rota(igroup)/rtd)
               sa=SIN(rota(igroup)/rtd)
               tr(1) = xt(1) - (isuba(1,igroup)-1)*tmp
               tr(2) = tmp*ca
               tr(3) =-tmp*sa
C Find index of first point in second column.
               itmp = isuba(3,igroup)-isuba(1,igroup)+2
               CALL PLTXCC(Yray, itmp, igroup, xt, ndim, iyoff)
               yslop = xt(2)-x1(2)
               tr(4) = x1(2)-yslop - (isuba(2,igroup)-1)*yslop
               tr(5) = yslop*sa
               tr(6) = yslop*ca
               CALL PGQWIN(TXMIN, TXMAX, TYMIN, TYMAX)
               I1 = NINT((TXMIN-tr(1))/tr(2))
               I2 = NINT((TXMAX-tr(1))/tr(2))
               IF ( I1.GT.I2 ) THEN
                  itmp = I1
                  I1 = I2
                  I2 = itmp
               END IF
               J1 = NINT((TYMIN-tr(4))/tr(6))
               J2 = NINT((TYMAX-tr(4))/tr(6))
               IF ( J1.GT.J2 ) THEN
                  itmp = J1
                  J1 = J2
                  J2 = itmp
               END IF
               I1 = MIN(MAX(isuba(1,igroup),I1),isuba(3,igroup))
               I2 = MIN(MAX(isuba(1,igroup),I2),isuba(3,igroup))
               J1 = MIN(MAX(isuba(2,igroup),J1),isuba(4,igroup))
               J2 = MIN(MAX(isuba(2,igroup),J2),isuba(4,igroup))
               IF ( ipall.EQ.0 ) GOTO 750
               IF ( image(igroup).GT.0 ) THEN
                  IF ( ipmod(igroup).EQ.0 ) THEN
C Only image the data
                     IF(zscale(1,igroup).EQ.zscale(2,igroup))
     :                zscale(2,igroup) = zscale(1,igroup)+1.
                     IF(lctnam(igroup).GT.0) THEN
                        CALL PLTCCT(cctnam(igroup), lctnam(igroup))
                     END IF
                     IF ( itfun(igroup).GE.0 ) THEN
                        CALL PGSITF(itfun(igroup))
                     ELSE
                        CALL PLTHIS(Yray, i2drow, Nvec, I1, I2, J1, J2,
     :                   zscale(1,igroup), zscale(2,igroup) )
                     END IF
                     CALL PGIMAG(Yray, i2drow, Nvec, I1, I2, J1, J2,
     :                zscale(1,igroup), zscale(2,igroup), tr)
                     CALL PGBOX('BCST',XSPAC,NSUBX(iwnum),
     :                       'BCST',YSPAC,NSUBY(iwnum))
                     IF ( CXOPT(iwnum)(7:7).NE.'G') THEN
                        CALL PGBOX(CXOPT(iwnum),XSPAC,NSUBX(iwnum),
     :                             CYOPT(iwnum),YSPAC,NSUBY(iwnum))
                     ELSE
                        CXOPT1 = CXOPT(iwnum)
                        CXOPT1(7:7) = ' '
                        CYOPT1 = CYOPT(iwnum)
                        CYOPT1(7:7) = ' '
                        CALL PGBOX(CXOPT1,XSPAC,NSUBX(iwnum),
     :                             CYOPT1,YSPAC,NSUBY(iwnum))
                        CXOPT1 = 'G'
                        CYOPT1 = 'G'
                        CALL PLTSCI(IGCOL)
                        CALL PLTSLS(IGLS)
                        CALL PGBOX(CXOPT1,XSPAC,NSUBX(iwnum),
     :                             CYOPT1,YSPAC,NSUBY(iwnum))
                     END IF
                     IF ( icbar(igroup).NE.0 ) THEN
                        CALL PGWEDG('RI',1.4,3.0,
     :                   zscale(1,igroup),zscale(2,igroup),' ')
                     END IF
                  END IF
               END IF
               IF ( icont(igroup).GT.0 ) THEN
                  IF ( ipmod(igroup).LE.0 ) THEN
                     IF ( ipmod(igroup).NE.0 ) THEN
C Plotting residuals
                        imnum = ABS(ipmod(igroup))
                     ELSE
C Just plot the data, use a dummy model number
                        imnum = 1
                     END IF
                     CALL PLCONB(Yray, i2drow, Nvec, I1, I2, J1, J2,
     &   rlvcon(1,igroup), MXLEV, tr, NO, npts, ipmod(igroup), igroup,
     &   icomp(1,imnum), pval(1,imnum), nterm(imnum),
     &   icocon(1,igroup), ilscon(1,igroup), rlwcon(1,igroup) )
                     CALL PLTSCI(1)
                     CALL PLTSLS(1)
                     CALL PLTSLW(WIDTH)
                  END IF
               END IF
            END IF
C
C- Plot model.
  750       CONTINUE
            IF ( ipmod(igroup).GT.0 ) THEN
               imnum = ipmod(igroup)
               IF ( igroup.GT.MXGRP-MXMOD ) THEN
C For the default model, we contour using the same contour levels as
C the fitted data group, but with forced solid lines.
                  iftg = ifitg(ipmod(igroup))
                  itmp = 0
                  DO i=1, MXLEV
                     IF ( ilscon(i,iftg).EQ.1 ) THEN
                        ilscon(i,0) = 4
                     ELSE
                        ilscon(i,0) = 1
                     END IF
                  END DO
               ELSE
                  iftg = igroup
                  itmp = iftg
               END IF
               IF ( iftg.LE.MX2D .AND. icont(iftg).NE.0 ) THEN
                  CALL PLCONB(Yray, i2drow, Nvec, I1, I2, J1, J2,
     &   rlvcon(1,iftg), MXLEV, tr, NO, npts, +1, iftg,
     &   icomp(1,imnum), pval(1,imnum), nterm(imnum),
     &   icocon(1,iftg), ilscon(1,itmp), rlwcon(1,iftg) )
                  GOTO 770
               END IF
               CALL PLTSCI(icol(igroup))
               CALL PLTSLS(lsty(igroup))
               IF ( widlin(igroup).EQ.0.0) THEN
                  CALL PLTSLW(WIDTH)
               ELSE
                  CALL PLTSLW(widlin(igroup))
               END IF
               IF ( nfpl.EQ.0 ) THEN
C Plot model only at the x-coordinates of the group that was fitted.
                  iftg = ifitg(ipmod(igroup))
                  new = 1
                  DO 760 I= 1,igrpos(2,iftg)
                     CALL PLTXCC(Yray, i, iftg, xt, ndim, iyoff)
                     IF ( XT(1).EQ.NO .OR. XT(1).LT.pmin(1) .OR.
     &                 XT(1).GT.pmax(1) ) THEN
                        new = 1
                        GOTO 760
                     END IF
                     YT = FNFIT(XT,ICOMP(1,ipmod(igroup)),
     &                  PVAL(1,ipmod(igroup)),nterm(ipmod(igroup)))
                     IF ( YT.EQ.NO ) THEN
                        new = 1
                        GOTO 760
                     END IF
                     IF ( LOGX(iwnum).NE.0 ) XT(1) =
     &                                 LOG10(MAX(XT(1),RMNLOG))
                     IF ( LOGY(iwnum).NE.0 ) YT = LOG10(MAX(YT,RMNLOG))
                     IF ( new.NE.0 ) THEN
                        new = 0
                        CALL PGMOVE(XT(1),YT)
                     ELSE
                        CALL PGDRAW(XT(1),YT)
                     END IF
  760             CONTINUE
               ELSE
C Plot model at evenly spaced locations.
C %%% only works in 1D case.
                  IF ( nfpl.LT.0 ) THEN
                     iftg = ifitg(ipmod(igroup))
                     XLO = xymnmx(1,iftg)
                     XHI = xymnmx(3,iftg)
                  ELSE
                     XLO = pmin(1)
                     XHI = pmax(1)
                  END IF
                  IF ( LOGX(iwnum).NE.0 ) XLO = LOG10(MAX(XLO,RMNLOG))
                  IF ( LOGX(iwnum).NE.0 ) XHI = LOG10(MAX(XHI,RMNLOG))
                  XDEL = (XHI-XLO)/ABS(nfpl)
                  new = 1
                  XCEN = XLO
                  XT(2) = 0.0
                  DO I = 0,ABS(nfpl)
                     XT(1) = XCEN
                     IF ( LOGX(iwnum).NE.0 .AND.
     :                 XCEN.LT.1.E35 ) XT(1) = 10.**XCEN
                     YT = FNFIT(XT,ICOMP(1,ipmod(igroup)),
     &                 PVAL(1,ipmod(igroup)),nterm(ipmod(igroup)))
                     IF ( YT.EQ.NO ) THEN
                        new = 1
                     ELSE
                        IF ( LOGY(iwnum).NE.0 )
     &                     YT = LOG10(MAX(YT,RMNLOG))
                        IF ( new.NE.0 ) THEN
                           new = 0
                           CALL PGMOVE(XCEN,YT)
                        ELSE
                           CALL PGDRAW(XCEN,YT)
                        END IF
                     END IF
                     XCEN = XCEN+XDEL
                  END DO
               END IF
            END IF
C End loop over plot groups
  770    CONTINUE
C
C Now plot labels, if LAbel ON.  (LAbel OFf dates from the days when
C graphics devices were slow.)
         IF ( IAND(IPLAB,1).NE.0 .AND. ipall.NE.0) THEN
            CALL PLTSCI(1)
            CALL PLTCS(CSIZE)
            CALL PLTSLW(WIDTH)
            IF ( LOGX(iwnum).NE.0 ) THEN
               WXMIN = LOG10(MAX(WXMIN,RMNLOG))
               WXMAX = LOG10(MAX(WXMAX,RMNLOG))
            END IF
            IF ( LOGY(iwnum).NE.0 ) THEN
               WYMIN = LOG10(MAX(WYMIN,RMNLOG))
               WYMAX = LOG10(MAX(WYMAX,RMNLOG))
            END IF
            XCEN = (WXMIN+WXMAX)/2.
            TMP1 = WINLOC(3,iwnum)-WINLOC(1,iwnum)
            TMP2 = BOXVP(3,iwnum)- BOXVP(1,iwnum)
            XDEL = .025*CSIZE*(WXMAX-WXMIN)/(TMP1*TMP2)
            YCEN = (WYMIN+WYMAX)/2.
            TMP1 = WINLOC(4,iwnum)-WINLOC(2,iwnum)
            TMP2 = BOXVP(4,iwnum)- BOXVP(2,iwnum)
            YDEL = .025*CSIZE*(WYMAX-WYMIN)/(TMP1*TMP2)
            ctmp = CYLAB(iwnum)
            IF ( LENACT(ctmp).LE.0 ) THEN
               igroup = IFGRP(ipwin, MXGRP-MXMOD, iwnum)
               IF(igroup.EQ.0) igroup = 2
               ctmp = cglab(igroup)
            END IF
            CALL PLTTEX(WXMIN-PYLAB*XDEL,YCEN,  90.,2,4,ctmp)
            CALL PLTTEX(WXMIN-3.0*XDEL,YCEN,  90.,2,4,COYLAB(iwnum))
            IF(CXOPT(iwnum)(5:5).EQ.'N') THEN
               IF(LENACT(CXLAB(iwnum)).NE.0) THEN
                  CALL PLTTEX(XCEN,WYMIN-2.5*YDEL, 0.0,2,4,CXLAB(iwnum))
               ELSE IF ( ixvec.GT.0 ) THEN
                  CALL PLTTEX(XCEN,WYMIN-2.5*YDEL, 0.0,2,4,cglab(ixvec))
               END IF
               CALL PLTTEX(XCEN,WYMIN-3.7*YDEL, 0.0,2,4,COXLAB(iwnum))
            END IF
            CALL PLTTEX(WXMIN,WYMAX+0.5*YDEL, 0.0,1,4,CFNAM(iwnum))
            CALL PLTTEX( XCEN,WYMAX+1.8*YDEL, 0.0,2,4,CTLAB(iwnum))
            CALL PLTTEX( XCEN,WYMAX+3.0*YDEL, 0.0,2,4,COTLAB(iwnum))
            DO I = 1,MXLAB
               IF ( ILABEL(1,I).EQ.iwnum ) THEN
                  xtmp = FLABEL(1,I)
                  IF(LOGX(iwnum).NE.0) xtmp = LOG10(MAX(xtmp,RMNLOG))
                  YT = FLABEL(2,I)
                  IF(LOGY(iwnum).NE.0) YT = LOG10(MAX(YT,RMNLOG))
                  XRANGE = WXMAX-WXMIN
                  YRANGE = WYMAX-WYMIN
                  CALL PLTSCI(ILABEL(4,I))
                  CALL PGMOVE(xtmp,YT)
                  IF ( ILABEL(6,I).GE.0 ) THEN
                     CALL PLTCS(FLABEL(7,I))
                     CALL PLTSMK(ILABEL(6,I))
                     CALL PLTPM(1,xtmp,YT)
                     IF(ILABEL(2,I).EQ.1) THEN
                        xtmp = xtmp+0.020*FLABEL(7,I)*XRANGE
                     ELSE IF(ILABEL(2,I).EQ.3) THEN
                        xtmp = xtmp-0.020*FLABEL(7,I)*XRANGE
                     END IF
                  END IF
                  IF ( ILABEL(7,i).GT.0 ) THEN
                     CALL PLTSLS(ILABEL(5,I))
                     xtmp = FLABEL(5,i)
                     IF(LOGX(iwnum).NE.0) xtmp = LOG10(MAX(xtmp,RMNLOG))
                     YT = FLABEL(6,i)
                     IF(LOGY(iwnum).NE.0) YT = LOG10(MAX(YT,RMNLOG))
                     CALL PGDRAW(xtmp,yt)
                     IF(ILABEL(5,I).NE.1) CALL PLTSLS(1)
                  ELSE IF ( FLABEL(6,I).GT.0. ) THEN
C- Draw line from marked position outward to label
                     CALL PLTSLS(ILABEL(5,I))
                     xtmp = xtmp+FLABEL(6,I)*XRANGE*COS(FLABEL(5,I)/RTD)
                     YT   =   YT+FLABEL(6,I)*YRANGE*SIN(FLABEL(5,I)/RTD)
                     CALL PGDRAW(xtmp,YT)
                     IF(ILABEL(5,I).NE.1) CALL PLTSLS(1)
                  END IF
                  CALL PLTCS(FLABEL(4,I))
                  CALL PLTTEX(xtmp,YT,FLABEL(3,I),
     &                  ILABEL(2,I),ILABEL(3,I),CLABEL(I))
               END IF
            END DO
            CALL PLTCS(CSIZE)
         END IF
C End loop over all windows
  790 CONTINUE
C---
      IF ( ipall.EQ.0 ) THEN
         CALL PGEBUF
         CALL PGUPDT
         GOTO 100
      END IF
C---
C- Put various labels on plot.
      IF ( IAND(IPLAB,1).NE.0 ) THEN
C---
C- Numbered labels in viewport coodinates
         CALL PLTSVW(BOXVP, WINLOC, XYSCAL, LOGX, LOGY, IWADJ, 0)
         DO I = 1,MXLAB
            IF(ILABEL(1,I).LT.0) THEN
               xtmp = FLABEL(1,I)
               YT = FLABEL(2,I)
               CALL PLTSCI(ILABEL(4,I))
               IF(ILABEL(6,I).LE.0) THEN
                  CALL PGMOVE(xtmp,YT)
               ELSE
                  CALL PLTCS(FLABEL(7,I))
                  CALL PLTSMK(ILABEL(6,I))
                  CALL PLTPM(1,xtmp,YT)
                  IF(ILABEL(2,I).EQ.1) THEN
                     xtmp = xtmp+0.020*FLABEL(7,I)
                  ELSE IF(ILABEL(2,I).EQ.3) THEN
                     xtmp = xtmp-0.020*FLABEL(7,I)
                  END IF
               END IF
               IF(FLABEL(6,I).GT.0.) THEN
C- Draw line from marked position outward to label
                  CALL PLTSLS(ILABEL(5,I))
                  xtmp = xtmp+FLABEL(6,I)*COS(FLABEL(5,I)/RTD)
                  YT = YT+FLABEL(6,I)*SIN(FLABEL(5,I)/RTD)
                  CALL PGDRAW(xtmp,YT)
                  IF(ILABEL(5,I).NE.1) CALL PLTSLS(1)
               END IF
               CALL PLTCS(FLABEL(4,I))
               CALL PLTTEX(xtmp,YT,FLABEL(3,I),ILABEL(2,I),ILABEL(3,I),
     :            CLABEL(I))
            END IF
         END DO
C---
C- Write model parameters on right edge.
         IF ( ipwin(MXGRP-MXMOD+icmod).GT.0 .AND.
     &         IAND(IPLAB,2).NE.0 ) THEN
            CALL PLTSCI(1)
            CALL PLTSLW(1.0)
            CALL PLTCS(0.75)
            XDEL = .0225
            NSTOP = nterm(ICMOD)
            IF ( PVAL(NSTOP+1,ICMOD).GE.0. ) NSTOP = NSTOP+2
            NROW = 1+(NSTOP-1)/5
            NMAX = INT((0.992-BOXVP(3,icwin))/XDEL)
            NMAX = MIN(NMAX,NROW)
C- X position in viewport coordinates
            TMP = 0.992-(NMAX-1)*XDEL
            DO I = 1,NMAX
               IS = 1+5*(I-1)
               IE = MIN(IS+4, NSTOP)
               WRITE(CRLAB,831) (CPARM(ICOMP(1,ICMOD),K,nterm(ICMOD)),
     :                             PVAL(K,ICMOD),K = IS,IE)
  831          FORMAT(1P,5(A,'=',G11.4,:,', '))
               CALL PLTTEX(TMP,0.06,90.,1,4,CRLAB)
               TMP = TMP+XDEL
C- Fortran 8x EXIT statment
               IF(TMP.GE.1.0 .OR. IE.GE.NSTOP) GOTO 850
            END DO
  850       CONTINUE
         END IF
C- Plot the date.
         IF ( ITIME.NE.0 ) THEN
            CALL PGIDEN
         END IF
      END IF
      CALL PLTCS(CSIZE)
C---
      CALL PGEBUF
      CALL PGUPDT
      IF ( QHARD ) THEN
         CALL PGEND
         IOPEN = 0
         QHARD = .FALSE.
         cpfile = CPSAV
      END IF
      GOTO 100
C---
  900 Ier = -1
C---
C Restore original state.
  950 CONTINUE
      IF ( idoend.NE.0 ) THEN
         CALL PGEND
      ELSE
         IF ( IOPEN.EQ.0 ) THEN
            CALL PLTOPE(cpfile,ibcol,scrcol,cfont,pgpapw,pgpapa,Ier)
         END IF
         DO iwnum = 1,MXWIN
            IF ( iactw(iwnum).GT.0 ) THEN
               CALL PLTSVW(BOXVP,WINLOC,XYSCAL,LOGX,LOGY,
     &            IWADJ,iwnum)
               GOTO 960
            END IF
         END DO
      END IF
  960 CONTINUE
      IF ( IOPEN.LT.0 ) CALL PLTTER('A')
      CALL STWARN(0)
      IOPEN = 0
      CALL XSETIN(CEXT)
      RETURN
      END
