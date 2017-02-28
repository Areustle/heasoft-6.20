C FIT.FOR
C- Contains entry points for:
C MODEL
C CPARM
C FRETHA
C FIT
C FITVIS
C UNCERT
C FITLIM
C FNFIT
C MDERIV
C---
       BLOCK DATA FITBLK
C---
C Number of terms per model.
C---
C AFT
C---
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
      SAVE /FITCMN/
      DATA ISTAT/0/
      DATA NTER/ 1,     1,     1,     1,     1,     1,     2,
     1    3,       3,       3,      3,
     2    3,       4,
     3    4,       5,
     4    3,       3,       2,
     5    3,       4,       4,      6,    6,   1,
     6    0,     0,       0,    0/
      END
C*********
      SUBROUTINE MODEL(CBUF, Xmini, Xmaxi, MXPAR, Cmd, Ncmd, Icmd,
     :     ICOMP, PVAL, PLIM, NTERM)
      CHARACTER CBUF*(*)
      CHARACTER Cmd(*)*(*)
      REAL      Xmini(2), Xmaxi(2), PVAL(*), PLIM(3,*)
      INTEGER   MXPAR, Ncmd, Icmd, NTERM
      INTEGER   ICOMP(*)
C---
C This routines handles the PLT commands, FReeze, MOdel, NEwpar, THaw,
C and WModel.
C---
C CBUF      I    Sub-command string
C Xmini     I    Lower limit for spline models
C Xmaxi     I    Upper limit for spline models
C MXPAR     I    Maximun number of terms allowed
C Cmd       I    PLT CMD array
C Ncmd      I    Number of valid commands in CMD
C Icmd      I/O  Number of commands actually used
C ICOMP     I/O  Component number
C PVAL        O  Parameter values
C PLIM        O  Parameter errors/limits
C NTERM     I/O  Number of parameters
C---
C AFT
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
C
      CHARACTER CPARM*4
      REAL      FNCLOA, FPNUM
      INTEGER   ISNUM, LENACT
C
      CHARACTER CNAM*256, ctmp*256, CTOK*256, ctoku*256
      CHARACTER CCOD(MXCOD)*72
      SAVE      CCOD
      CHARACTER CLAB(MXCNUM)*4
      REAL      TMP, DX
      INTEGER   I, ICNUM, IE, IER, IFIRST, IHIT, ILOCAL, INEW, IOLD
      INTEGER   IS, ITMP, IOS
      INTEGER   K, KP, KP1, LKP
      INTEGER   LNAM, LOCLAB, ltmp, LTOK, LUN, NCOD, NCTER, NT, NKNOT
      SAVE IFIRST
C
      EXTERNAL FITBLK
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
C- The model labels.
      DATA CLAB/ 'CONS', 'LINR', 'QUAD','CUBI','X4  ','X5  ','POWR',
     1   'SIN ', 'GAUS', 'NGAU', 'EXP ',
     2   'AEXP', 'BURS',
     3   'SBUR', 'PEAR',
     4   'WIND', 'KING', 'LN  ',
     5   'LORE', 'CGAU', 'NCGA',  'EGAU',  'NEGA', 'LY  ',
     6   'USER', 'SPLN', 'AKIM'/
      DATA IFIRST/1/
C---
   11 FORMAT(A)
C---
      IF(IFIRST.NE.0) THEN
         IFIRST=0
         CALL UINFO(0, CLAB(MXCNUM-2), NTER(MXCNUM-2))
         CALL UPC(CLAB(MXCNUM-2))
      END IF
      ctmp=CBUF
      ltmp=LENACT(ctmp)
      KP=0
      LUN=0
      CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
      CALL UPC(CTOK)
      IF(CTOK(1:1).EQ.'F') THEN
C- Freeze sub-command
         CALL FRETHA(-1., ctmp, ltmp, KP, PLIM, NTERM)
         RETURN
      ELSE IF(CTOK(1:1).EQ.'M') THEN
C- Model sub-command
         CALL ALFSKS(ctmp, ltmp, KP)
         IF(ctmp(KP+1:KP+1).EQ.'?') THEN
            GOTO 890
         ELSE IF(ctmp(KP+1:KP+1).EQ.'@') THEN
            KP=KP+1
            CALL ALF(ctmp, ltmp, KP, CTOK, LTOK)
            NTERM=0
            CNAM='default'
            IF(LTOK.GT.1) CNAM=CTOK(:LTOK)
            CALL XTEND(CNAM,'mod')
            CALL GETLUN(LUN)
            CALL OPENWR(LUN,CNAM,'OLD',' ',' ',0,1,IOS)
            IF(IOS.NE.0) GOTO 930
            READ(LUN,11,ERR=930,END=930) ctmp
            KP=0
            ltmp=LENACT(ctmp)
         END IF
      ELSE IF(CTOK(1:1).EQ.'N') THEN
C- Newpar sub-command
         GOTO 500
      ELSE IF(CTOK(1:1).EQ.'T') THEN
C- Thaw sub-command
         CALL FRETHA( 0., ctmp, ltmp, KP, PLIM, NTERM)
         RETURN
      ELSE IF(CTOK(1:1).EQ.'W') THEN
C- Wmodel sub-command
         IF(CTOK(2:2).EQ.'L') THEN
C WLun LUN
            CALL ALF(ctmp, ltmp, KP, CTOK, LTOK)
            LUN=FPNUM(CTOK, LTOK, IER)
            ctmp='MODEL '
            ltmp=6
            ILOCAL=0
         ELSE
C Wmodel filename
            CNAM=' '
            CALL ALF(ctmp, ltmp, KP, CNAM, LNAM)
            CALL XTEND(CNAM, 'MOD')
            CALL GETLUN(LUN)
            CALL OPENWR(LUN, CNAM, 'NEW', '  ', 'L', 0, 0, IOS)
            IF(IOS.NE.0) THEN
               ITMP=LENACT(CNAM)
               WRITE(*,*) 'FIT--Error, unable to open file ',CNAM(:ITMP)
               CALL FRELUN(LUN)
               RETURN
            END IF
            ctmp=' '
            ltmp=0
            ILOCAL=1
         END IF
         NT=1
  150    CONTINUE
         IF ( ICOMP(NT).LE.MXCNUM ) THEN
C- Standard component
            WRITE(ctmp(ltmp+1:),151) CLAB(ICOMP(NT))
  151       FORMAT(1X,A)
            ltmp=LENACT(ctmp)
         ELSE
C- COD file
            ITMP=ICOMP(NT)-MXCNUM
            WRITE(ctmp(ltmp+1:),151) CCOD(ITMP)(:LENACT(CCOD(ITMP)))
            ltmp=LENACT(ctmp)
         END IF
C- Special treatment for components that require a parameter
         IF ( ICOMP(NT).EQ.MXCNUM-1 ) THEN
C- Spline
            WRITE(ctmp(ltmp+1:),161) NTER(ICOMP(NT))/2
  161       FORMAT(1X,I3)
            ltmp=ltmp+5
         ELSE IF ( ICOMP(NT).EQ.MXCNUM ) THEN
C- Akima
            WRITE(ctmp(ltmp+1:),161) NTER(ICOMP(NT))/2
            ltmp=ltmp+5
         END IF
         NT=NT+NTER(ICOMP(NT))
         IF(NT.LE.NTERM) GOTO 150
         WRITE(LUN,11) ctmp(:ltmp)
         DO I=1,NTERM
            CTOK=CPARM(ICOMP,I,NTERM)
            WRITE(ctmp,*) PVAL(I),PLIM(1,I),PLIM(2,I),PLIM(3,I)
            ltmp = LENACT(ctmp)
            IF ( ltmp.LT.40 ) THEN
               ctmp(ltmp:40)=' '
               ltmp = 40
            END IF
            ctmp(ltmp+1:ltmp+2)=' !'
            ltmp = ltmp+2
            CALL CRAMIF(i,4,ctmp,ltmp)
            WRITE(LUN,11) ctmp(:ltmp)//' '//ctok(1:2)
         END DO
         IF(PVAL(NTERM+1).GE.0.) THEN
            ctmp='! WVAR='
            ltmp=8
            CALL CRAMF(PVAL(NTERM+1),ctmp,ltmp)
            WRITE(LUN,11) ctmp(:ltmp)
            ctmp='! NBIN='
            ltmp=8
            CALL CRAMF(PVAL(NTERM+2),ctmp,ltmp)
            WRITE(LUN,11) ctmp(:ltmp)
         END IF
         IF(ILOCAL.NE.0) THEN
            CLOSE(UNIT=LUN)
            CALL FRELUN(LUN)
         END IF
         RETURN
      ELSE IF ( LTOK.LE.0 ) THEN
         GOTO 890
      ELSE
         RETURN
      END IF
C---
C- Fall through to start reading a model
C- Reset all of COD's internal pointers.
      TMP=FNCLOA(+1,0,0,0.)
      NTERM=0
      NCOD=0
C- Get component token
  200 CONTINUE
      CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
      IF ( LTOK.LE.0 ) THEN
         PVAL(NTERM+1)=-1.
         GOTO 500
      END IF
      IF ( CTOK(1:1).EQ.'/' ) RETURN
      CTOKU = CTOK
      CALL UPC(CTOKU)
      LOCLAB=1
      IF(2.LE.LTOK .AND. LTOK.LE.4) THEN
C- Built-in components can only be specified with 2-4 characters.
         DO ICNUM=1,MXCNUM
            IF ( CLAB(ICNUM)(1:LTOK).EQ.CTOKU(1:LTOK) ) GOTO 250
            LOCLAB=LOCLAB+NTER(ICNUM)
         END DO
      END IF
C- Now try a COD file
      DO I=1,NCOD
         ctok = ccod(i)(:ltok)
         CALL UPC(ctok)
         IF ( ctok(1:LTOK).EQ.CTOKU(1:LTOK)) THEN
            ICNUM=MXCNUM+I
            GOTO 350
         END IF
      END DO
      KP1=0
      CALL CODFIL(CTOK,LTOK,KP1,NCTER,IER)
      IF ( IER.EQ.0 ) THEN
         IF ( NCOD.GE.MXCOD ) THEN
            WRITE(*,231) MXCOD
  231       FORMAT(' MODEL--Can only use',I6,' COD files.')
            NTERM=0
            RETURN
         END IF
         NCOD=NCOD+1
         ICNUM=MXCNUM+NCOD
         CCOD(NCOD)=CTOK
         NTER(ICNUM)=NCTER
         GOTO 350
      END IF
C---
C- Typo
      WRITE(*,211) CTOK(1:LTOK)
  211 FORMAT(' Illegal component: "',A,'"')
      GOTO 890
C---
C- Get number of terms for components with variable number of terms.
  250 CONTINUE
      IF ( ICNUM.EQ.MXCNUM-1 .OR. ICNUM.EQ.MXCNUM ) THEN
C- Spline or Akima
         LKP=KP
         CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
         IF(ISNUM(CTOK,LTOK).NE.0) THEN
            NKNOT=FPNUM(CTOK,LTOK,IER)
         ELSE
            KP=LKP
         END IF
         IF(NKNOT.LE.1) NKNOT=2
         NTER(ICNUM)=2*NKNOT
         IF ( ICNUM.EQ.MXCNUM-1 ) THEN
            CALL SPLIM(PVAL, PLIM, -1, NTER(ICNUM))
         ELSE
            CALL AKLIM(PVAL, PLIM, -1, NTER(ICNUM))
         ENDIF
         NKNOT=NTER(ICNUM)/2
      END IF
C---
C- Make sure we will not overflow the parameter arrays
  350 CONTINUE
      IF ( NTERM+NTER(ICNUM).GT.MXPAR ) THEN
         WRITE(*,351) MXPAR
  351    FORMAT(' MODEL--Can only contain',I6,' terms.')
         NTERM=0
         RETURN
      END IF
C---
C- Fill the ICOMP array
      DO I=1,NTER(ICNUM)
         ITMP=NTERM+I
C- If parameter is not the same as the old then unfreeze.
         IF ( ICOMP(ITMP).NE.ICNUM ) THEN
            PLIM(1,ITMP)=0.
         END IF
         ICOMP(ITMP)=ICNUM
      END DO
C---
C- Special initial conditions for Spline or Akima components
      IF ( ICNUM.EQ.MXCNUM-1 .OR. ICNUM.EQ.MXCNUM ) THEN
         XMIN(1) = Xmini(1)
         XMAX(1) = Xmaxi(1)
         XMIN(2) = Xmini(2)
         XMAX(2) = Xmaxi(2)
         IF ( XMIN(1).EQ.XMAX(1) ) XMAX(1) = XMIN(1)+1.
         DX=(XMAX(1)-XMIN(1))/(NKNOT-1)
         DO I=1,NKNOT
C- To start, evenly distribute X-locations of knots and freeze
            ITMP=NTERM+I
            PVAL(ITMP) =XMIN(1)+DX*(I-1)
            PLIM(1,ITMP)=-1.
            PLIM(1,ITMP+NKNOT)=0.
         END DO
      END IF
C---
C- Increment number of terms and loop
      NTERM=NTERM+NTER(ICNUM)
      GOTO 200
C---
C-
  500 IF(NTERM.LE.0) GOTO 890
C---
C- Now read initial parameter values.
      CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
      IF ( LTOK.GT.0 ) THEN
         IS=FPNUM(CTOK,LTOK,IER)
         IE=IS
      ELSE
         IS=1
         IE=NTERM
      END IF
      IS=MIN(MAX(IS,1),NTERM)
      IE=MIN(MAX(IE,1),NTERM)
      DO I=IS,IE
         IF ( KP.GE.ltmp ) THEN
            KP=0
            IF ( LUN.GT.0 ) THEN
C Reading a model file
  510          CONTINUE
               READ(LUN,11,ERR=930,END=930) ctmp
               ltmp=LENACT(ctmp)
C Skip lines starting with !
               IF ( ctmp(1:1).EQ.'!') GOTO 510
            ELSE
  520          CONTINUE
               IF ( icmd.GE.0 .AND. icmd.LT.Ncmd ) THEN
C Still be reading from the PLT CMD array
                  IF ( LENACT(Cmd(icmd+1)).LE.0 ) THEN
                     icmd = icmd+1
                     GOTO 520
                  END IF
                  CALL STWARN(1)
                  CALL LDBUF1(Cmd(icmd+1),Ier)
                  icmd = icmd+1-Ier
               ELSE
C No other place to read, must prompt the user directly.
                  CTOK=CPARM(ICOMP,I,NTERM)
                  WRITE(*,521) I,CTOK,PVAL(I),(PLIM(K,I),K=1,3)
  521             FORMAT(I3,' ',A2,': VAL(',1PG11.4,'), SIG(',G11.4,
     :      '), PLO(',G11.4,'), PHI(',G11.4,')?')
               END IF
               CALL GTBUF(' ',IER)
               IF ( IER.LT.0 ) GOTO 700
               CALL GTREST(ctmp,ltmp)
               CALL ALFSKS(ctmp, ltmp, kp)
               IF ( ctmp(kp+1:kp+1).EQ.'"' ) kp = kp + 1
            END IF
         END IF
C Get parameter value
         CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
         IF(LTOK.GT.0) THEN
            TMP=FPNUM(CTOK,LTOK,IER)
            IF(PVAL(I).NE.TMP) PVAL(NTERM+1)=-1.
            PVAL(I)=TMP
         END IF
C Get sigma or constraint
         CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
         IF(LTOK.GT.0) THEN
            IOLD=NINT(PLIM(1,I))
            PLIM(1,I)=FPNUM(CTOK,LTOK,IER)
            INEW=NINT(PLIM(1,I))
            IF(INEW.LT.-1) THEN
C Constrained freeze, ensure factor is correct.
               PLIM(2,I)=1.0
            ELSE IF(INEW.GE.0 .AND. IOLD.LT.-1) THEN
C Thawed a constrainded parameter.  Lower limit is meaningless, so reset.
               PLIM(2,I)=0.0
            END IF
         END IF
C Get lower limit or factor
         CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
         IF(LTOK.GT.0) PLIM(2,I)=FPNUM(CTOK,LTOK,IER)
C Get upper limit
         CALL ALF(ctmp,ltmp,KP,CTOK,LTOK)
         IF(LTOK.GT.0) PLIM(3,I)=FPNUM(CTOK,LTOK,IER)
         IF ( ctmp(kp+1:kp+1).EQ.'!' ) kp=ltmp
      END DO
C---
C- For constrained parameters, force parameter to be equal to another
C- valid parameter.
  700 CONTINUE
      DO i=1,NTERM
C Avoid integer overflows on numbers with BIG sigmas.
         IF(PLIM(1,i).LT.0.) THEN
            IF(NINT(-PLIM(1,i)).GT.1) THEN
               PLIM(1,i)=MIN(MAX(-FLOAT(NTERM),PLIM(1,i)),-2.)
C Parameter cannot be a constant times itself, just freeze it.
               IF(NINT(PLIM(1,i)).EQ.-i) THEN
                  PLIM(1,i)=-1.
                  PLIM(2,i)=0.
               END IF
            END IF
         END IF
      END DO
C-
      CALL FITLIM(ICOMP, PVAL, PLIM, NTERM, IHIT)
      IF(LUN.NE.0) THEN
         CLOSE(UNIT=LUN)
         CALL FRELUN(LUN)
      END IF
      RETURN
C---
  890 WRITE(*,891) (CLAB(K),K=1,MXCNUM)
  891 FORMAT(' Possible components are:'/(9(2X,A4)))
      RETURN
C-
  930 WRITE(*,931)
  931 FORMAT(' Unable to Open (or Read) full model file.')
      CLOSE(UNIT=LUN)
      CALL FRELUN(LUN)
      RETURN
C---
      END
C*********
      CHARACTER*(*) FUNCTION CPARM(ICOMP, IPAR, NTERM)
      INTEGER   ICOMP(*), IPAR, NTERM
C---
C Find the label associated with parameter IPAR.
C---
C ICOMP   I
C IPAR    I
C NTERM   I
C---
C 1990-Mar-02 - New routine [AFT]
C---
      INTEGER   MXTERM
      PARAMETER (MXTERM=68)
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
C
      CHARACTER CTLAB(MXTERM)*2
      INTEGER   I, ICNUM, IOFF, ITMP, LABPOS
C
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
C The (first three) term labels.
      DATA CTLAB/'CO', 'LI',  'QU',  'CU',  'X4',  'X5',  'IN','PN',
     1   'PE','PH','SN',  'GC','GW','GN',  'Gc', 'Gw', 'Gn',
     2   'EC','EW','EN',  'EC','EW','EN',  'ST','PT','DT','BN',
     3   'TS','RR','DT','BN',  'K ','X0','A1','M1','M2',
     4   'T1','T2','LE',  'RC','IN','S0',  'XC','NL',
     5   'LC','LW','LN', 'Xc','Yc','Gw','Gn', 'Xc','Yc','Gw','Gn',
     6   'Xc','Sx','Yc','Sy','TH','Gn',
     7   'Xc','Sx','Yc','Sy','TH','Gn', 'Ly'/
C---
      IF(IPAR.EQ.NTERM+1) THEN
         CPARM='WV'
         RETURN
      ELSE IF(IPAR.GT.NTERM) THEN
         CPARM=' N'
         RETURN
      END IF
C---
C Must first scan through to find out the parameter offset into the
C current component.
      IOFF=1
  120 IF(IOFF+NTER(ICOMP(IOFF)).LE.IPAR) THEN
         IOFF=IOFF+NTER(ICOMP(IOFF))
         GOTO 120
      END IF
      IOFF=IPAR-IOFF
C
      ICNUM=ICOMP(IPAR)
      IF(ICNUM.LE.MXCNUM-3) THEN
         LABPOS=1
         DO I=1,ICNUM-1
            LABPOS=LABPOS+NTER(I)
         END DO
         CPARM=CTLAB(LABPOS+IOFF)
      ELSE IF(ICNUM.EQ.MXCNUM-2) THEN
         CALL UINFO(IOFF+1, CPARM, ITMP)
      ELSE IF(ICNUM.EQ.MXCNUM-1) THEN
         IF(IOFF.LT.NTER(ICNUM)/2) THEN
            CPARM='SX'
         ELSE
            CPARM='SY'
         END IF
      ELSE IF(ICNUM.EQ.MXCNUM) THEN
         IF(IOFF.LT.NTER(ICNUM)/2) THEN
            CPARM='AX'
         ELSE
            CPARM='AY'
         END IF
      ELSE IF(ICNUM.GT.MXCNUM) THEN
         CPARM='P'//CHAR(ICHAR('0')+MOD(IOFF+1,10))
      END IF
C---
      RETURN
      END
C*********
      SUBROUTINE FRETHA(PARVAL, Ctmp, Ltmp, KP, PLIM, NTERM)
      REAL      PARVAL, PLIM(3,*)
      INTEGER   Ltmp, KP, NTERM
      CHARACTER Ctmp*(*)
C---
C Set PLIM(IPAR,1)=PARVAL for all IPAR values listed in Ctmp.
C---
C PARVAL  I    The value to set.
C Ctmp    I
C Ltmp    I
C KP      I/O
C PLIM    I/O
C NTERM   I
C---
C 1989-Oct-02 - [AFT]
C---
      CHARACTER CTOK*32
      INTEGER   I, IHI, ILO, LTOK, IER
C---
  110 CALL ALF(Ctmp, Ltmp, KP, CTOK, LTOK)
      IF(LTOK.LE.0) RETURN
      CALL IRANGE(CTOK, LTOK, 1, 0, ILO, IHI, IER)
      IF(ILO.LE.0 .OR. IHI.GT.NTERM) THEN
         WRITE(*,*) ILO,'--Illegal parameter'
         RETURN
      END IF
      DO I=ILO, IHI
         PLIM(1,I)=PARVAL
      END DO
      GOTO 110
      END
C*********
      SUBROUTINE FIT(CBUF, IFIT, Y, MXROW, Ngroup, ICWIN,
     :   Ipwin, IPYER, IWIN, IGRPOS, XYSCAL,
     :   ICOMP, PVAL, PLIM, NTERM)
      CHARACTER CBUF*(*)
      REAL      Y(*), XYSCAL(4,*), PVAL(*), PLIM(3,*)
      INTEGER   IFIT, MXROW, Ngroup, ICWIN
      INTEGER   Ipwin(*), IPYER(*), IWIN(*), IGRPOS(3,*)
      INTEGER   ICOMP(*), NTERM
C---
C This routine executes the PLT commands, FIT and FIT ERROR.
C---
C CBUF      I    The string containing possible sub-commands.
C IFIT      I/O  The number of the group fitted.
C Y         I
C MXROW     I
C Ngroup    I
C Icwin     I/O  The (first) window containing the plot group being fitted.
C Ipwin
C IPYER     I
C Iwin
C IGRPOS    I    Need to pass entire array since Ifit can change
C XYSCAL    I
C ICOMP     I
C PVAL      I/O
C PLIM      I
C NTERM     I
C---
C AFT
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      REAL      FPNUM
      INTEGER   ISNUM, LENACT
C
      CHARACTER CTOK*32
      REAL      FDEL
      SAVE      FDEL
      REAL      CHI, CHIM, PLO, PHI, xt(2)
      INTEGER   IEGOOD, IHIP, ILOP, ISGOOD, NITER
      SAVE      IEGOOD, IHIP, ILOP, ISGOOD, NITER
      INTEGER   I, ICNT, IDONE, IER
      INTEGER   IPAR, ITMP, IX, iy0
      INTEGER   KP, LB, LTOK, ndim
C
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
      DATA NITER/10/,ILOP,IHIP/1,1/
      DATA FDEL/2.7/
C---
      IF ( NTERM.LE.0 ) THEN
         WRITE(*,*) 'FIT--Error, No model defined.'
         RETURN
      END IF
C---
C Scan for sub-commands
      LB=LENACT(CBUF)
      KP=0
      IDONE=0
      CALL ALF(CBUF, LB, KP, CTOK, LTOK)
  100 CONTINUE
         CALL UPC(CTOK)
         IF(CTOK(1:1).EQ.'U') THEN
C 'UNcertainty' subcommand
            IF ( istat.NE.0 ) THEN
               WRITE(*,*) 'Using likelihood.'
            END IF
            GOTO 300
         ELSE IF(CTOK(1:1).EQ.'I') THEN
C 'Iteration' sub-command
            CALL ALF(CBUF, LB, KP, CTOK, LTOK)
            IF(LTOK.GT.0) THEN
               NITER=NINT(FPNUM(CTOK,LTOK,IER))
            END IF
         ELSE IF(CTOK(1:1).EQ.'S') THEN
C 'Statistic' sub-command
            CALL ALF(CBUF, LB, KP, CTOK, LTOK)
            CALL UPC(CTOK)
            IF(LTOK.EQ.0) THEN
               ISTAT=0
            ELSE IF(CTOK(1:1).EQ.'C') THEN
               ISTAT=0
               WRITE(*,*) 'Using chi^2.'
            ELSE IF(CTOK(1:1).EQ.'M') THEN
               ISTAT=1
            END IF
         ELSE IF(ISNUM(CTOK,LTOK).NE.0) THEN
C # sub-command
            ITMP=FPNUM(CTOK,LTOK,IER)
C%%%
C This allows user to ask to fit defined groups, but the fitting engine
C only fits the original data.
            IF(ITMP.GT.0 .AND. ITMP.LE.Ngroup .AND.
     :         igrpos(1,itmp).GE.0 ) THEN
C%%%
               IFIT=ITMP
            ELSE
               WRITE(*,*) 'FIT--',itmp,' is not a valid data group.'
            END IF
            ICWIN=IWIN(IFIT)
            CALL PLTXCC(Y, 1, ifit, xt, ndim, itmp)
            XMIN(1)=MIN(XYSCAL(1,ICWIN),XYSCAL(3,ICWIN))
            XMAX(1)=MAX(XYSCAL(1,ICWIN),XYSCAL(3,ICWIN))
            XMIN(2)=MIN(XYSCAL(2,ICWIN),XYSCAL(4,ICWIN))
            XMAX(2)=MAX(XYSCAL(2,ICWIN),XYSCAL(4,ICWIN))
            WRITE(*,231) IFIT,XMIN(1),XMAX(1)
            IF ( ndim.GT.1) WRITE(*,241) XMIN(2),XMAX(2)
            CALL FITIT(Y, IPYER(IFIT), MXROW, ifit,
     :       IGRPOS(1,IFIT), IGRPOS(2,IFIT), Xmin, Xmax, ISGOOD, IEGOOD,
     :       NITER, ISTAT, ICOMP, PVAL, PLIM, NTERM, CHI)
            IDONE=1
         END IF
         CALL ALF(CBUF, LB, KP, CTOK, LTOK)
      IF(LTOK.GT.0) GOTO 100
      IF ( istat.NE.0 ) THEN
         WRITE(*,*) 'Using likelihood.'
      END IF
C---
      IF ( IDONE.EQ.0 ) THEN
         CALL FITVIS(Ipwin, Ngroup, IFIT)
         ICWIN=IWIN(IFIT)
         CALL PLTXCC(Y, 1, ifit, xt, ndim, itmp)
         XMIN(1)=MIN(XYSCAL(1,ICWIN),XYSCAL(3,ICWIN))
         XMAX(1)=MAX(XYSCAL(1,ICWIN),XYSCAL(3,ICWIN))
         XMIN(2)=MIN(XYSCAL(2,ICWIN),XYSCAL(4,ICWIN))
         XMAX(2)=MAX(XYSCAL(2,ICWIN),XYSCAL(4,ICWIN))
         WRITE(*,231) IFIT,XMIN(1),XMAX(1)
  231    FORMAT(' Fitting group',I4,',  from',1PG11.3,' to',G11.3)
         IF ( ndim.GT.1) WRITE(*,241) XMIN(2),XMAX(2)
  241    FORMAT('      And bounded in Y by',1PG11.3,' to',G11.3)
         CALL FITIT(Y, IPYER(IFIT), MXROW, IFIT,
     :     IGRPOS(1,IFIT), IGRPOS(2,IFIT), Xmin, Xmax, ISGOOD, IEGOOD,
     :     NITER, ISTAT, ICOMP, PVAL, PLIM, NTERM, CHI)
      END IF
      RETURN
C---
C Come here to compute uncertainties.
C Read up to three optional numbers.
  300 CONTINUE
      ICNT=0
      DO I=1,3
         CALL ALF(CBUF,LB,KP,CTOK,LTOK)
         IF(LTOK.LE.0) GOTO 340
         IX=INDEX(CTOK(:LTOK),'.')
         IF(IX.GT.0) THEN
C Reset default delta CHI^2
            FDEL=FPNUM(CTOK,LTOK,IER)
         ELSE
C Reset parameter range over which to determine errors.
            ITMP=FPNUM(CTOK,LTOK,IER)
            ITMP=MIN( MAX(1,ITMP), NTERM)
            IF(ICNT.EQ.0) THEN
               ILOP=ITMP
            ELSE
               IF(ITMP.GT.ILOP) THEN
                  IHIP=ITMP
               ELSE
                  IHIP=ILOP
                  ILOP=ITMP
               END IF
            END IF
            ICNT=ICNT+1
         END IF
      END DO
  340 IF(ICNT.EQ.1) IHIP=ILOP
C---
C For each parameter in the given range.  Find the error.
      CHIM=PVAL(NTERM+1)
      NITER=10
      iy0=IGRPOS(1,IFIT)
      DO IPAR=ILOP,IHIP
         CALL UNCERT(Y, Ifit, iy0, IPYER(IFIT), MXROW, Xmin, Xmax,
     &    ISGOOD, IEGOOD, 1, ISTAT, ICOMP, PVAL, PLIM, NTERM, CHIM,
     :     IPAR, FDEL, PLO, PHI, IER)
         IF(IER.GT.0) THEN
            IF(IER.EQ.1) THEN
               WRITE(*,371) CHIM
  371          FORMAT(' New minimum found.  CHI^2=',1PG10.4)
            END IF
            RETURN
         END IF
         WRITE(*,411) IPAR,FDEL,PLO,PHI
  411    FORMAT(' Parameter ',I3,', Delta CHI^2=',1PG11.4,2G12.4)
      END DO
C---
      RETURN
      END
C*********
      SUBROUTINE FITVIS(Ipwin, Ngroup, IFIT)
      INTEGER   Ipwin(*), Ngroup, IFIT
C---
C Return the plot group to actually fit in Ifit.  This will be unchanged
C unless Ifit is 0 (not defined) or the group being fitted is no longer
C being plotted (i.e., was colored off).  In this case, find the smallest
C number corresponding to a plot group this is visible.
C---
C Ipwin   I    >0 group is being plotted, <=0 otherwise
C Ngroup  I
C IFIT    I/O
C---
C 1990-Mar-07 - [AFT]
C---
      INTEGER   ig
C---
      IF ( IFIT.EQ.0 .OR. Ipwin(IFIT).LE.0 ) THEN
         DO ig=1,Ngroup
            IF ( Ipwin(ig).GT.0 ) THEN
               IFIT=ig
               GOTO 230
            END IF
         END DO
C Everything is colored of.  Guess a random group.
         IFIT = 2
      END IF
C
  230 CONTINUE
      RETURN
      END
C*********
      SUBROUTINE UNCERT(Y, Igroup, iy0, LERY, MXROW, Xmin, Xmax,
     &   ISGOOD, IEGOOD, ICHAT, ISTAT, ICOMP, PVAL, PLIM, NTERM, CHIM,
     :   IPAR, VVREQ, PLO, PHI, IER)
      INTEGER   NTERM
      REAL      Y(*), Xmin(*), Xmax(*), PVAL(*), PLIM(3,*)
      REAL      CHIM, VVREQ, PLO, PHI
      INTEGER   Igroup, iy0, LERY, MXROW, ISGOOD, IEGOOD
      INTEGER   ICHAT, ISTAT, ICOMP(NTERM)
      INTEGER   IPAR, IER
C---
C Finds delta chi^2.
C---
C Y       I
C Igroup  I    The group number
C MXROW   I
C LERY    I
C ISGOOD  I
C IEGOOD  I
C ICHAT   I
C PVAL    I/O  Parameter values at minimum.
C PLIM    I/O  Bevington's errors at minimum.
C CHIM    I    Minimun chi square.
C IPAR    I    Index of parameter to find error.
C VVREQ   I    Requested value of delta chi squared.
C PLO       O  Low value of the parameter.
C PHI       O  Hi value for parameter
C IER       O  =0 Value found,
C              =1 New minimum found.
C              =2 Model insensitive to changes in some par.
C              =3 Parameter frozen.
C              <0 Gave up at least once.
C---
C AFT
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER    MXPAR
      PARAMETER (MXPAR=120)
C
      REAL      PTRY(MXPAR), SAVSIG(MXPAR)
      REAL      CHI, DELCO, DELP, FUDGE, P1, P2
      REAL      V1, V2, VNEW, VREQ
      INTEGER   I, ICNT, IFL, IHIT, NITER, NDOF, NFIT, NFPAR
C
      DATA NITER/20/
C---
      IF ( PVAL(NTERM+1).LT.0.0 ) THEN
C User has not done a Fit hence chi^2 is not defined.
         WRITE(*,*) 'UNCERT--Must do a Fit before UNcertainty.'
         IER=3
         RETURN
      END IF
C
      IF(PLIM(1,IPAR).LT.0.) THEN
         WRITE(*,*)
     :      'UNCERT--Not allowed to find error of frozen parameter.'
         IER=3
         RETURN
      END IF
      IER=0
      FUDGE=1.0
      IF ( LERY.EQ.0 .AND. Istat.EQ.0) THEN
C---
C No errors, must compute fudge factor that makes reduced CHI^2
C equal to 1.0.
         NFPAR=0
         DO I=1,NTERM
            IF(PLIM(1,I).GE.0) NFPAR=NFPAR+1
         END DO
         NFIT=PVAL(NTERM+2)
         NDOF=NFIT-NFPAR
         IF(CHIM.GT.0.) FUDGE=(NFIT-NFPAR)/CHIM
         WRITE(*,131) FUDGE
  131    FORMAT(' W-VAR is being multiplied by',1PG9.2,
     :      ' to convert to Chi^2')
      END IF
      VREQ=SQRT(VVREQ)
      DELP=-VREQ*PLIM(1,IPAR)
      IF(ICHAT.GT.0) WRITE(*,181)
  181 FORMAT(' Delta parm   Delta Chi^2')
C---
      DO I=1,NTERM
         PTRY(I)=PVAL(I)
         SAVSIG(I)=PLIM(1,I)
      END DO
      PLIM(1,IPAR)=-1.
      IFL=1
C---
  300 CONTINUE
      ICNT=0
      P1=0.
      V1=0.
C---
  350 CONTINUE
      PTRY(IPAR)=PVAL(IPAR)+DELP
      IF(PLIM(2,IPAR).LT.PLIM(3,IPAR)) THEN
C Hard limit
         PTRY(IPAR)=MIN( MAX(PLIM(2,IPAR),PTRY(IPAR)) ,PLIM(3,IPAR))
      END IF
      CALL FITLIM(ICOMP, PTRY, PLIM, NTERM, IHIT)
      CALL CURFIT(Y, Igroup, iy0, LERY, MXROW, ISGOOD, IEGOOD,
     :   Xmin, Xmax, -1, NITER, ISTAT, ICOMP, 0, NTERM,
     &   0.0, PTRY, PLIM, CHI)
      IF(CHI.LT.0.) THEN
C Model insensitive to changes in parameter value.
         DO I=1,NTERM
            PLIM(1,I)=SAVSIG(I)
         END DO
         IER=2
         RETURN
      END IF
C
      DELCO=(CHI-CHIM)*FUDGE
      IF(ICHAT.GT.0) WRITE(*,361) DELP,DELCO
  361 FORMAT(1X,1PG10.3,4X,G10.3)
      IF(DELCO.LT.0.) THEN
C Found a new lower value of Chi^2, return new parameters to user.
         DO I=1,NTERM
            PVAL(I)=PTRY(I)
         END DO
         PLIM(1,IPAR)=0.
         CHIM=CHI
         IER=1
         RETURN
      END IF
      IF(ABS(DELCO-VVREQ).LT..01) GOTO 400
      VNEW=SQRT(DELCO)
      IF(ICNT.EQ.0) THEN
         P2=DELP
         V2=VNEW
      ELSE
         IF(VNEW.LT.VREQ) THEN
            P1=DELP
            V1=VNEW
        ELSE
            P2=DELP
            V2=VNEW
         END IF
      END IF
      IF(ABS(V2-V1).GT.1.E-12 .AND. ICNT.LT.10) THEN
         DELP=P1+(VREQ-V1)*(P2-P1)/(V2-V1)
         IF(IFL.NE.0) THEN
            IF(DELP.GT.0.) GOTO 390
         ELSE
            IF(DELP.LT.0.) GOTO 390
         END IF
         IF(ICNT.GT.5) THEN
            IF(V1.EQ.0.  ) DELP= .9*DELP
            IF(V2.LT.VREQ) DELP=1.1*DELP
         END IF
         ICNT=ICNT+1
         GOTO 350
      END IF
  390 CONTINUE
      WRITE(*,391)
  391 FORMAT(' UNCERT--Give up.')
      IER=-1
C---
  400 CONTINUE
      IF(IFL.NE.0) THEN
         IFL=0
         PLO=PTRY(IPAR)
         DELP=-DELP
         GOTO 300
      END IF
      PHI=PTRY(IPAR)
C Restore the original sigmas.
      DO I=1,NTERM
         PLIM(1,I)=SAVSIG(I)
      END DO
C Note, splines keep internal parameters that need to be restored.
      CALL FITLIM(ICOMP, PVAL, PLIM, NTERM, IHIT)
      RETURN
      END
C*********
      SUBROUTINE FITLIM(ICOMP, PVAL, PLIM, NTERM, IHIT)
      INTEGER   NTERM
      INTEGER   ICOMP(NTERM)
      REAL      PVAL(NTERM), PLIM(3,NTERM)
C---
C This routine should be called just after parameters have been
C changed to make sure the parameters are in legal bounds.
C---
C ICOMP   I    Component defination.
C PVAL    I/O  Parameter values, can be altered to meet limits.
C PLIM    I    The limit array.
C IHIT      O  If non-zero, is the number of a parameter that
C              -was altered.
C---
C AFT
C---
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
C
      REAL      tmp
      INTEGER   I, IACOMP, IHIT, IX, NT
C
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
C---
      IHIT=0
      NT=1
C---
C Do the general parameter ajustments.
  100 IACOMP=ICOMP(NT)
      DO 110 I=NT,NT+NTER(IACOMP)-1
C Avoid integer overflows on numbers with BIG sigmas
         IF(PLIM(1,I).LT.0.) THEN
            IX=NINT(-PLIM(1,I))
            IF(IX.GT.1) THEN
               IF(PLIM(2,I).NE.0.) THEN
C Constrained to be a constant times another parameter.
                  PVAL(I)=PLIM(2,I)*PVAL(IX)
               ELSE
C Constrained to be a constant added to another parameter.
                  PVAL(I)=PLIM(3,I)+PVAL(IX)
               END IF
            END IF
         ELSE IF(PLIM(2,I).LT.PLIM(3,I)) THEN
C Hard limit
            IF(PVAL(I).LT.PLIM(2,I)) THEN
               IHIT=I
               PVAL(I)=PLIM(2,I)
            ELSE IF(PVAL(I).GT.PLIM(3,I)) THEN
               IHIT=I
               PVAL(I)=PLIM(3,I)
            END IF
         END IF
  110 CONTINUE
C Now check for special cases
      GOTO (890,890,890,890,890,890,890,890,320,320,890,890,890,890,
     :      890,890,890,890,890,650,650,660,660,890,
     :      800,820,860,890) IACOMP
C
C- Gaussian/Ngaus (force width to be positive)
  320 CONTINUE
      PVAL(NT+1)=ABS(PVAL(NT+1))
      IF ( Pval(nt+1).EQ.0.0 ) Pval(nt+1) = 1.0E-6
      GOTO 890
C
C- CGaus/NCgaus (force width to be positive)
  650 CONTINUE
      Pval(nt+2)=ABS(Pval(nt+2))
      IF ( Pval(nt+1).EQ.0.0 ) Pval(nt+1) = 1.0E-6
      GOTO 890
C
C- EGaus/NEgaus (force width to be positive)
  660 CONTINUE
C Sigma cannot be negative nor zero.
      Pval(nt+1)=ABS(Pval(nt+1))
      IF ( Pval(nt+1).EQ.0.0 ) Pval(nt+1) = 1.0E-6
      Pval(nt+3)=ABS(Pval(nt+3))
      IF ( Pval(nt+3).EQ.0.0 ) Pval(nt+3) = 1.0E-6
C Require sigx to be larger than sigy
      IF ( Pval(nt+3).GT.Pval(nt+1) ) THEN
C But only if sigx, sigy, and theta are all free
         IF ( Plim(1,nt+1).GE.0.0 .AND. Plim(1,nt+3).GE.0.0 .AND.
     &        Plim(1,nt+4).GE.0.0 ) THEN
            tmp = Pval(nt+3)
            Pval(nt+3) = Pval(nt+1)
            Pval(nt+1) = tmp
            Pval(nt+4) = Pval(nt+4)+90.
         END IF
      END IF
C Make sure angle is in range -90 +90
      tmp = MOD(Pval(nt+4)+90.,180.)
      IF ( tmp.LT.0.0 ) tmp=tmp+180.
      Pval(nt+4)=tmp-90.
      GOTO 890
C
C- User model.
  800 CALL ULIMIT(PVAL, PLIM, NT, NTER(IACOMP))
      GOTO 890
C
C- SPLN Spline
  820 CALL SPLIM(PVAL, PLIM, NT, NTER(IACOMP))
      GOTO 890
C
C- AKIMA model.
  860 CALL AKLIM(PVAL, PLIM, NT, NTER(IACOMP))
      GOTO 890
C---
  890 NT=NT+NTER(IACOMP)
      IF(NT.LE.NTERM) GOTO 100
C---
      RETURN
      END
C*********
      REAL FUNCTION FNFIT(Xt, ICOMP, PVAL, NTERM)
      INTEGER   ICOMP(*), NTERM
      REAL      Xt(2), PVAL(NTERM)
C---
C Compute the function value.
C---
C Xt         I  The (possibly 2 dimensional) independent variable
C ICOMP      I
C PVAL(*)    I
C PLIM(3,*)  I
C---
C AFT
C---
      REAL NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
      REAL      FNAKIM, FNCOD, FNSP, UFNY
C
      REAL      A2, ca, DEM, EX, FAC, P1, sa, T, T1LOG, T2LOG
      REAL      TC, TMP, TT, X, X1, X2, XC, xs, y, ys, z2
      INTEGER   IACOMP, IER, NT
C
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
      REAL      rtd
      DATA  rtd/57.2957795/
C---
      X=Xt(1)
      Y=Xt(2)
C
      FNFIT=0.
      NT=1
  100 CONTINUE
      IACOMP=ICOMP(NT)
      GOTO (200,210,220,230,240,250,260,300,320,320,360,360,400,450,
     :      500,550,600,620,640,650,650,660,660,670,
     :      800,820,860,880) IACOMP
C- CONS
  200 FNFIT=FNFIT+PVAL(NT)
      GOTO 890
C- LINR
  210 FNFIT=FNFIT+PVAL(NT)*X
      GOTO 890
C- QUAD
  220 FNFIT=FNFIT+PVAL(NT)*X*X
      GOTO 890
C- CUBI
  230 FNFIT=FNFIT+PVAL(NT)*X*X*X
      GOTO 890
C- X4
  240 X2=X*X
      FNFIT=FNFIT+PVAL(NT)*X2*X2
      GOTO 890
C- X5
  250 X2=X*X
      FNFIT=FNFIT+PVAL(NT)*X2*X2*X
      GOTO 890
C- POWR
  260 IF(X.GT.0.) THEN
         IF(PVAL(NT+1).GT.0.) THEN
            TMP=PVAL(NT)*LOG(X)+LOG( PVAL(NT+1))
            TMP=MIN(MAX(-70.,TMP),70.)
            FNFIT=FNFIT+EXP(TMP)
         ELSE IF(PVAL(NT+1).LT.0.) THEN
            TMP=PVAL(NT)*LOG(X)+LOG(-PVAL(NT+1))
            TMP=MIN(MAX(-70.,TMP),70.)
            FNFIT=FNFIT-EXP(TMP)
         END IF
      END IF
      GOTO 890
C- SIN
  300 IF(PVAL(NT).NE.0.) FNFIT=FNFIT+PVAL(NT+2)*SIN((X-PVAL(NT+1))*
     :      6.28318531/PVAL(NT))
      GOTO 890
C- GAUS/NGAU
  320 CONTINUE
      xs = (X-PVAL(NT))/PVAL(NT+1)
      ex = 0.
      IF ( ABS(xs).LT.12.0 ) ex = EXP(-xs*xs/2.)
      IF ( IACOMP.EQ.9 ) THEN
C GAus
         FNFIT=FNFIT+PVAL(NT+2)*ex
      ELSE
C NGaus
         FNFIT=FNFIT+PVAL(NT+2)*ex/(2.50662827*PVAL(NT+1))
      END IF
      GOTO 890
C- EXP and AEXP
  360 xs=0.
      IF ( PVAL(NT+1).NE.0. ) xs=(X-PVAL(NT))/PVAL(NT+1)
      IF ( IACOMP.EQ.12 ) xs=ABS(xs)
      EX=0.
      IF ( ABS(xs).LT.80. ) EX=EXP(-xs)
      FNFIT=FNFIT+PVAL(NT+2)*EX
      GOTO 890
C- BURS
  400 xs=0.
      IF ( X.GT.PVAL(NT) ) THEN
         IF ( X.LT.PVAL(NT+1) ) THEN
            FNFIT=FNFIT+PVAL(NT+3)*(X-PVAL(NT))/(PVAL(NT+1)-PVAL(NT))
         ELSE
            IF ( PVAL(NT+2).NE.0.) xs=(X-PVAL(NT+1))/PVAL(NT+2)
            EX=0.
            IF ( ABS(xs).LT.70. ) EX=EXP(-xs)
            FNFIT=FNFIT+PVAL(NT+3)*EX
         END IF
      END IF
      GOTO 890
C- SBUR
  450 T=X-PVAL(NT)
      IF(T.GT.0) THEN
         FAC=1.
         IF(PVAL(NT+1).NE.0.) FAC=2.718281828/ABS(PVAL(NT+1)*PVAL(NT+2))
         TC=LOG(FAC*T)
         P1=PVAL(NT+1)*TC-T/PVAL(NT+2)
         EX=0.
         IF(ABS(P1).LT.80.) EX=EXP(P1)
         FNFIT=FNFIT+PVAL(NT+3)*EX
      END IF
      GOTO 890
C- PEAR
  500 A2=PVAL(NT+2)*PVAL(NT+4)/PVAL(NT+3)
      X1=(X-PVAL(NT+1))/PVAL(NT+2)
      X2=(X-PVAL(NT+1))/A2
      IF ( -1.0.LT.X1 .AND. X2.LT.1.0 ) THEN
         IF(ABS(X1).GT.1.E-5) THEN
            T1LOG=LOG(1.+X1)
         ELSE
            T1LOG= X1-X1*X1/2.
         END IF
         IF(ABS(X2).GT.1.E-5) THEN
            T2LOG=LOG(1.-X2)
         ELSE
            T2LOG=-X2-X2*X2/2.
         END IF
         TT=PVAL(NT+3)*T1LOG+PVAL(NT+4)*T2LOG
         TT=MIN(MAX(-80.,TT),+80.)
         FNFIT=FNFIT+PVAL(NT)*EXP(TT)
      END IF
      GOTO 890
C- WIND
  550 IF(X.GE.PVAL(NT) .AND. X.LE.PVAL(NT+1)) FNFIT=FNFIT+PVAL(NT+2)
      GOTO 890
C- KING
  600 IF(PVAL(NT).GT.0.) THEN
         TMP=XT(1)/PVAL(NT)
         FNFIT=FNFIT+PVAL(NT+2)*(1.+TMP*TMP)**(-PVAL(NT+1))
      END IF
      GOTO 890
C- LN Natural LOG
  620 XC=X-PVAL(NT)
      IF(XC.GT.0.) FNFIT=FNFIT+PVAL(NT+1)*LOG(XC)
      GOTO 890
C- LORE Lorentz
  640 IF(PVAL(NT+1).EQ.0.) GOTO 890
      XC=2.*(X-PVAL(NT))/PVAL(NT+1)
      DEM=1.+XC*XC
      FNFIT=FNFIT+PVAL(NT+2)/DEM
      GOTO 890
C- CGaus/NCgaus
  650 CONTINUE
      xs = (X-PVAL(NT  ))/PVAL(NT+2)
      ys = (Y-PVAL(NT+1))/PVAL(NT+2)
      z2 = xs*xs + ys*ys
      ex = 0.
      IF ( z2.LT.144.0 ) ex = EXP(-z2/2.)
      IF ( IACOMP.EQ.20 ) THEN
C CGaus
         FNFIT=FNFIT+PVAL(NT+3)*ex
      ELSE
C NCgaus
         FNFIT=FNFIT+Pval(Nt+3)*ex/(6.283185307*Pval(Nt+2)*Pval(Nt+2))
      END IF
      GOTO 890
C- EGaus/NEgaus
  660 CONTINUE
      ca = COS(Pval(Nt+4)/rtd)
      sa = SIN(Pval(Nt+4)/rtd)
      xs = ca*(xt(1)-Pval(Nt  )) + sa*(xt(2)-Pval(Nt+2))
      ys =-sa*(xt(1)-Pval(Nt  )) + ca*(xt(2)-Pval(Nt+2))
      z2 = (xs/Pval(Nt+1))**2 + (ys/Pval(Nt+3))**2
      ex = 0.
      IF ( z2.LT.144.0 ) ex = EXP(-z2/2.)
      IF ( IACOMP.EQ.22 ) THEN
C Egaus
         FNFIT = FNFIT + Pval(nt+5)*ex
      ELSE
C NEgaus
         FNFIT = FNFIT+Pval(Nt+5)*ex/(6.283185307*Pval(Nt+1)*Pval(Nt+3))
      END IF
      GOTO 890
C- LY Linear in Y
  670 CONTINUE
      FNFIT=FNFIT+Pval(Nt)*Y
      GOTO 890
C- USER
  800 FNFIT=FNFIT+UFNY(XT,PVAL,NT,NTER(IACOMP))
      GOTO 890
C- SPLN Spline
  820 FNFIT=FNFIT+FNSP(XT,PVAL(NT))
      GOTO 890
C- AKIM Akima
  860 FNFIT=FNFIT+FNAKIM(XT,PVAL(NT))
      GOTO 890
C- COD
  880 FNFIT=FNFIT+FNCOD(0,0,XT,PVAL(NT),NTER(IACOMP),IER)
      GOTO 890
C---
  890 NT=NT+NTER(IACOMP)
      IF(NT.LE.NTERM) GOTO 100
      RETURN
      END
C*********
      SUBROUTINE MDERIV(Xt, Icomp, Pval, Plim, Nterm, Deriv)
      INTEGER   Nterm
      REAL      Xt(2), Pval(Nterm), Plim(3,*), Deriv(Nterm)
      INTEGER   Icomp(*)
C---
C Compute the Derivative with respect to the parameter values.
C---
C Xt         I  The (possibly 2 dimensional) independent variable
C Icomp      I
C Pval(*)    I
C Plim(3,*)  I
C Nterm      I
C Deriv(*)     O
C---
C AFT
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXCNUM, MXCOD
      PARAMETER (MXCNUM=27, MXCOD=1)
C
      REAL      A2, BT, ca, DEM, EX, FAC, fny0, P1, sa
      REAL      T, T1, T2, TC, TIM, TMP
      REAL      TT, T1LOG, T2LOG, X, X1, X2, XC, xs, XLOG, y, ys, z2
      INTEGER   I, IACOMP, IX, NT
C
      REAL      XMIN, XMAX
      INTEGER   ISTAT, NTER
      COMMON/FITCMN/XMIN(2),XMAX(2),ISTAT,NTER(MXCNUM+MXCOD)
      REAL      rtd
      DATA  rtd/57.2957795/
C---
      x = Xt(1)
      y = Xt(2)
      DO I=1,Nterm
         Deriv(I)=0.
      END DO
C
      NT=1
  100 CONTINUE
      IACOMP=Icomp(NT)
      GOTO (200,210,220,230,240,250,260,300,320,320,360,360,400,450,
     :    500,550,600,620,640,650,650,660,660,670,
     :    800,820,860,880) IACOMP
C- CONS
  200 Deriv(NT)=1.
      GOTO 890
C- LINR
  210 Deriv(NT)=X
      GOTO 890
C- QUAD
  220 Deriv(NT)=X*X
      GOTO 890
C- CUBI
  230 Deriv(NT)=X*X*X
      GOTO 890
C- X4
  240 Deriv(NT)=X*X*X*X
      GOTO 890
C- X5
  250 Deriv(NT)=X*X*X*X*X
      GOTO 890
C- POWR
  260 IF(X.GT.0.) THEN
         XLOG=LOG(X)
         TMP=Pval(NT)*XLOG
         TMP=MIN(MAX(-70.,TMP),70.)
         Deriv(NT+1)=EXP(TMP)
         Deriv(NT  )=Pval(NT+1)*Deriv(NT+1)*XLOG
      END IF
      GOTO 890
C- SIN
  300 IF(Pval(NT).NE.0) THEN
         TMP=6.28318531/Pval(NT)
         TIM=(X-Pval(NT+1))
         Deriv(NT+1)=-Pval(NT+2)*TMP*COS(TMP*TIM)
         Deriv(NT  )= Deriv(NT+1)*TIM/Pval(NT)
         Deriv(NT+2)=               SIN(TMP*TIM)
      END IF
      GOTO 890
C- GAUS/NGAU
  320 CONTINUE
      xs=(X-Pval(NT))/Pval(NT+1)
      EX=0.
      IF ( ABS(xs).LT.12.0 ) EX=EXP(-xs*xs/2.)
      IF ( IACOMP.EQ.9 ) THEN
C GAus
         Deriv(NT  )=Pval(NT+2)*EX*xs/Pval(NT+1)
         Deriv(NT+1)=xs*Deriv(NT)
         Deriv(NT+2)=EX
      ELSE
C NGaus
         EX  = EX/2.50662827
         tmp = Pval(NT+2)*EX/(Pval(NT+1)*Pval(NT+1))
         Deriv(NT  ) = tmp*xs
         Deriv(NT+1) = tmp*(xs*xs-1.0)
         Deriv(NT+2) = EX/Pval(NT+1)
      END IF
      GOTO 890
C- EXP and AEXP
  360 xs=0.
      IF(Pval(NT+1).NE.0.) xs=(X-Pval(NT))/Pval(NT+1)
      IF(IACOMP.EQ.12) xs=ABS(xs)
      EX=0.
      IF(ABS(xs).LT.80.) EX=EXP(-xs)
      Deriv(NT)=0.
      IF(Pval(NT+1).NE.0.) Deriv(NT)=Pval(NT+2)*EX/Pval(NT+1)
      Deriv(NT+1)=xs*Deriv(NT)
      Deriv(NT+2)=EX
      IF(IACOMP.EQ.12 .AND. X.LT.Pval(NT)) Deriv(NT)=-Deriv(NT)
      GOTO 890
C- BURS
  400 xs=0.
      IF ( X.GT.Pval(NT) ) THEN
         IF ( X.LT.Pval(NT+1) ) THEN
            DEM=Pval(NT+1)-Pval(NT)
            Deriv(NT)=Pval(NT+3)*(X-Pval(NT+1))/(DEM*DEM)
            Deriv(NT+1)=-Pval(NT+3)*(X-Pval(NT))/(DEM*DEM)
            Deriv(NT+3)=(X-Pval(NT))/DEM
         ELSE
            IF(Pval(NT+2).NE.0.) xs=(X-Pval(NT+1))/Pval(NT+2)
            EX=0.
            IF(ABS(xs).LT.80.) EX=EXP(-xs)
            Deriv(NT+1)=0.
            IF(Pval(NT+2).NE.0.) Deriv(NT+1)=Pval(NT+3)*EX/Pval(NT+2)
            Deriv(NT+2)=xs*Deriv(NT+1)
            Deriv(NT+3)=EX
         END IF
      END IF
      GOTO 890
C- SBUR
  450 T=X-Pval(NT)
      IF(T.GT.0) THEN
         Pval(NT+2)=ABS(Pval(NT+2))
         FAC=1.
         IF(Pval(NT+1).NE.0.) FAC=2.718281828/ABS(Pval(NT+1)*Pval(NT+2))
         TC=LOG(FAC*T)
         P1=Pval(NT+1)*TC-T/Pval(NT+2)
         EX=0.
         IF(ABS(P1).LT.80.) EX=EXP(P1)
         BT=Pval(NT+3)*EX
         Deriv(NT  )=BT*(1./Pval(NT+2)-Pval(NT+1)/T)
         Deriv(NT+1)=BT*(TC-1.)
         Deriv(NT+2)=BT*(T-Pval(NT+1)*Pval(NT+2))/
     :                  (Pval(NT+2)*Pval(NT+2))
         Deriv(NT+3)=   EX
      END IF
      GOTO 890
C- PEAR
  500 A2=Pval(NT+2)*Pval(NT+4)/Pval(NT+3)
      X1=(X-Pval(NT+1))/Pval(NT+2)
      X2=(X-Pval(NT+1))/A2
      IF ( -1.0.LT.X1 .AND. X2.LT.1.0 ) THEN
         T1=1.+X1
         T2=1.-X2
         IF(ABS(X1).GT.1.E-5) THEN
            T1LOG=LOG(1.+X1)
         ELSE
            T1LOG= X1-X1*X1/2.
         END IF
         IF(ABS(X2).GT.1.E-5) THEN
            T2LOG=LOG(1.-X2)
         ELSE
            T2LOG=-X2-X2*X2/2.
         END IF
         TT=Pval(NT+3)*T1LOG+Pval(NT+4)*T2LOG
         TT=MIN(MAX(-80.,TT),+80.)
         Deriv(NT)=EXP(TT)
         TMP=Pval(NT)*Deriv(NT)
         Deriv(NT+1)=TMP*Pval(NT+3)*(1./T2-1./T1)/Pval(NT+2)
         Deriv(NT+2)=X1*Deriv(NT+1)
         Deriv(NT+3)=TMP*(T1LOG-X1/T2)
         Deriv(NT+4)=TMP*(T2LOG+X2/T2)
      END IF
      GOTO 890
C- WIND
  550 IF(X.GE.Pval(NT) .AND. X.LE.Pval(NT+1)) THEN
         Deriv(NT+2)=1.
      END IF
      GOTO 890
C---
C- KIng
  600 IF(Pval(NT).GT.0.) THEN
         X1=X/Pval(NT)
         TMP=1.+X1*X1
         Deriv(NT+2)=TMP**(-Pval(NT+1))
         Deriv(NT  )=2.*Pval(NT+1)*Pval(NT+2)*Deriv(NT+2)*X1*X1/
     :         (Pval(NT)*TMP)
         Deriv(NT+1)=-Pval(NT+2)*LOG(TMP)*Deriv(NT+2)
      END IF
      GOTO 890
C- LN Natural LOG
  620 XC=X-Pval(NT)
      IF(XC.GT.0.) THEN
         Deriv(NT+1)=LOG(XC)
         Deriv(NT)  =-Pval(NT+1)/XC
      END IF
      GOTO 890
C- LORE Lorentz
  640 IF(Pval(NT+1).EQ.0.) GOTO 890
      XC=2.*(X-Pval(NT))/Pval(NT+1)
      DEM=1.+XC*XC
      Deriv(NT  )= Pval(NT+2)*4.*XC/(DEM*DEM*Pval(NT+1))
      Deriv(NT+1)= Pval(NT+2)*2.*XC*XC/(DEM*DEM*Pval(NT+1))
      Deriv(NT+2)=1./DEM
      GOTO 890
C- CGaus
  650 CONTINUE
      xs = (X-Pval(NT  ))/Pval(NT+2)
      ys = (Y-Pval(NT+1))/Pval(NT+2)
      z2 = xs*xs + ys*ys
      ex = 0.
      IF ( z2.LT.144.0 ) ex = EXP(-z2/2.)
      IF ( IACOMP.EQ.20 ) THEN
C CGgaus
         Deriv(NT  ) = ex*Pval(NT+3)*xs/Pval(NT+2)
         Deriv(NT+1) = ex*Pval(NT+3)*ys/Pval(NT+2)
         Deriv(NT+2) = ex*Pval(NT+3)*z2/Pval(NT+2)
         Deriv(NT+3) = ex
      ELSE
C NCgaus
         ex = ex/(6.283185307*Pval(NT+2)*Pval(NT+2))
         Deriv(NT  ) = ex*Pval(NT+3)*xs/Pval(NT+2)
         Deriv(NT+1) = ex*Pval(NT+3)*ys/Pval(NT+2)
         Deriv(NT+2) = ex*Pval(NT+3)*(z2-2.0)/Pval(NT+2)
         Deriv(NT+3) = ex
      END IF
      GOTO 890
C- EGaus/NEgaus
  660 CONTINUE
      ca = COS(Pval(nt+4)/rtd)
      sa = SIN(Pval(nt+4)/rtd)
      xs = ca*(xt(1)-Pval(nt  )) + sa*(xt(2)-Pval(nt+2))
      ys =-sa*(xt(1)-Pval(nt  )) + ca*(xt(2)-Pval(nt+2))
      z2 = (xs/Pval(nt+1))**2 + (ys/Pval(nt+3))**2
      ex = 0.
      IF ( z2.LT.144.0 ) ex = EXP(-z2/2.)
C
      IF ( IACOMP.EQ.22 ) THEN
C EGaus
         fny0 = ex*Pval(nt+5)
         Deriv(nt  )=fny0*( xs*ca/Pval(nt+1)**2-ys*sa/Pval(nt+3)**2)
         Deriv(nt+1)=fny0*((xs/Pval(nt+1))**2)/Pval(nt+1)
         Deriv(nt+2)=fny0*( xs*sa/Pval(nt+1)**2+ys*ca/Pval(nt+3)**2)
         Deriv(nt+3)=fny0*((ys/Pval(nt+3))**2)/Pval(nt+3)
         Deriv(nt+4)=(fny0/rtd)*(
     &  ( sa*(xt(1)-Pval(nt))-ca*(xt(2)-Pval(nt+2)) )*xs/Pval(nt+1)**2
     & +( ca*(xt(1)-Pval(nt))+sa*(xt(2)-Pval(nt+2)) )*ys/Pval(nt+3)**2)
         Deriv(nt+5)=ex
      ELSE
C NEgaus
         fny0 = ex*Pval(nt+5)/(6.283185307*Pval(nt+1)*Pval(nt+3))
         Deriv(nt  )=fny0*( xs*ca/Pval(nt+1)**2-ys*sa/Pval(nt+3)**2)
         Deriv(nt+1)=fny0*((xs/Pval(nt+1))**2-1)/Pval(nt+1)
         Deriv(nt+2)=fny0*( xs*sa/Pval(nt+1)**2+ys*ca/Pval(nt+3)**2)
         Deriv(nt+3)=fny0*((ys/Pval(nt+3))**2-1)/Pval(nt+3)
         Deriv(nt+4)=(fny0/rtd)*(
     &  ( sa*(xt(1)-Pval(nt))-ca*(xt(2)-Pval(nt+2)) )*xs/Pval(nt+1)**2
     & +( ca*(xt(1)-Pval(nt))+sa*(xt(2)-Pval(nt+2)) )*ys/Pval(nt+3)**2)
         Deriv(nt+5)=ex/(6.283185307*Pval(nt+1)*Pval(nt+3))
      END IF
      GOTO 890
C- LY Linear in Y
  670 CONTINUE
      Deriv(Nt)=Y
      GOTO 890
C- USER model
  800 CALL UDeriv(XT,Pval,Plim,Deriv,NT,NTER(IACOMP))
      GOTO 890
C- SPLN Spline fit
  820 CALL SPDERI(XT,Pval,Plim,Deriv,NT,NTER(IACOMP))
      GOTO 890
C- AKIM Akima model.
  860 CALL AKDERI(XT,Pval,Plim,Deriv,NT,NTER(IACOMP))
      GOTO 890
C- COD
  880 CALL CODDER(XT,Pval,Plim,Deriv,NT,NTER(IACOMP))
      GOTO 890
C---
  890 NT=NT+NTER(IACOMP)
      IF(NT.LE.Nterm) GOTO 100
      DO I=1,Nterm
C Avoid integer overflows on numbers with BIG sigmas.
         IF(Plim(1,I).LT.0.) THEN
            IX=NINT(-Plim(1,I))
            IF(IX.GT.1) THEN
               IF(Plim(2,I).NE.0.) THEN
C Parameter times Plim(2,I)
                  Deriv(IX)=Deriv(IX)+Plim(2,I)*Deriv(I)
               ELSE
C Parameter added to Plim(3,I)
                  Deriv(IX)=Deriv(IX)+Deriv(I)
               END IF
            END IF
         END IF
      END DO
      RETURN
      END
