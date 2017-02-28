C Contains entry points for:
C CODFIL
C CODLIS
C CODTOK
C FNCOD
C  FNCLOA
C CODADD   SUB  Add a word in the user dictionary
C  CODCTI  SUB  Convert word to integer token
C  CODITC  SUB  Convert integer token to word
C  CODWOR  SUB  Find a word in the dictionary
C CODDER
C---
      SUBROUTINE CODFIL(Cbuf, Lbuf, Kp, Nterm, Ier)
      CHARACTER Cbuf*(*)
      INTEGER   Lbuf, Kp, Nterm, Ier
C Entry codfqc/codfsc
      INTEGER   Ichata
C---
C COmponent DEFination FILe.
C Reads a disk COD file into program memory.
C---
C Cbuf      I
C Lbuf      I
C Kp        I/O
C Nterm       O
C Ier         O
C---
C 1989-Feb-13 - latest mod [AFT]
C---
      REAL  NO
      PARAMETER (NO=-1.2E-34)
C-
      REAL      FNCLOA, FNCOD
      INTEGER   LENACT
C-
      CHARACTER ctmp*256
      CHARACTER ctok*132
      CHARACTER clib*120
      CHARACTER cdisk*12, cdir*12
      REAL      tmp, X, PAR(1)
      INTEGER   ichat
      SAVE      ichat
      INTEGER   icode, idelpc, idelsp, ientry, itry, itmp, ios
      INTEGER   llib, ltmp, ltok, lun, newt
      DATA cdisk/'$XANADU'/, cdir/'xanlib/cod'/
      DATA ichat/100/
C---
   11 FORMAT(A)
C---
      clib=' '
      CALL PTEND(cdisk,cdir,clib)
      llib=LENACT(clib)
      CALL GETlun(lun)
      newt=0
      Ier=0
      IF ( Cbuf(Kp+1:Kp+1).EQ.'$' ) Kp=Kp+1
      ctok=Cbuf(Kp+1:)
C- First search the current directory for the file.
      itry=0
  100 CONTINUE
      CALL XTEND(ctok,'cod')
      CALL OPENWR(lun,ctok,'OLD',' ',' ',0,1,ios)
      IF ( ios.NE.0 ) THEN
         IF ( itry.EQ.0 ) THEN
C- Look in a standard library for file.
            itry=1
            ctok=clib(:llib)//Cbuf(Kp+1:)
            CALL CONC(ctok(llib+1:))
            GOTO 100
         END IF
         Ier=1
         GOTO 910
      END IF
C---
  120 CONTINUE
      READ(lun,11,ERR=500,END=500) ctmp
      ltmp=LENACT(ctmp)
      IF ( ichat.GT.0 ) WRITE(*,*) ctmp(:ltmp)
      Kp=0
      CALL ALF(ctmp,ltmp,Kp,ctok,ltok)
      IF ( ctok(1:1).EQ.'!' ) GOTO 120
C---
      Kp=0
  150 CONTINUE
      CALL ALF(ctmp,ltmp,Kp,ctok,ltok)
      IF ( ltok.EQ.0 ) GOTO 120
      CALL CODTOK(+1,ctok,ltok,icode,idelpc,idelsp,Ier)
      IF ( Ier.NE.0 ) GOTO 900
      IF ( idelpc.LE.0 ) GOTO 150
      tmp=FNCOD(-1,icode,X,PAR,-1,Ier)
      IF ( ctok(:ltok).EQ.':' ) THEN
         tmp=FNCLOA(-7,ientry,itmp,0.)
      END IF
      IF ( icode.GE.4000 .AND. icode.LT.6000 ) THEN
C- Count parameters
         newt=MAX(newt,icode-4000)
      END IF
      GOTO 150
C---
  500 CONTINUE
      Nterm=newt
      ientry=ientry+1
      tmp=FNCLOA(+4,ientry,0,0.)
      IF ( ichat.GE.50 ) WRITE(*,*) 'Nterm=',newt
      CLOSE(UNIT=lun)
      CALL FRElun(lun)
      RETURN
C---
C- Error return
  900 CONTINUE
      WRITE(*,*) 'CODFIL--Illegal token=',ctok(:ltok)
  910 Nterm=0
      CLOSE(UNIT=lun)
      CALL FRElun(lun)
      Ier=1
      RETURN
C*********
      ENTRY CODFSC(ichata)
      ichat = Ichata
      RETURN
C*********
      ENTRY CODFQC(ichata)
      Ichata = ichat
      RETURN
      END
C*********
      SUBROUTINE CODLIS
C---
C List the current COD program in a FORTRAN style.  Note this will
C only work for simple COD programs.
C---
C 1988-Nov-30 - [AFT]
C---
      INTEGER   MXSTAC
      PARAMETER (MXSTAC=10)
C-
      REAL      FNCLOA
      INTEGER   LENACT
C-
      CHARACTER cbuf*80, cstac(MXSTAC)*80
      CHARACTER ctok*80
      REAL      tmp
      INTEGER   lstac(MXSTAC), isum(MXSTAC)
      INTEGER   icnt, icode, idelpc, idelsp, ier, indent, ipnt
      INTEGER   lbuf, lev, lpc, ltok
C---
      icnt=0
      lpc=1
C
   90 CONTINUE
      cstac(1)='Stack1'
      lstac(1)=LENACT(cstac(1))
      ipnt=1
      indent=1
      lev=0
  100    CONTINUE
         tmp=FNCLOA(-1,lpc,icode,tmp)
         IF ( icode.EQ.0 ) THEN
C- END statement
            GOTO 900
         END IF
C- Get token form
         CALL CODTOK(-1,ctok,ltok,icode,idelpc,idelsp,ier)
         IF ( ctok(1:ltok).EQ.'IF' ) THEN
C- IF
            IF ( ipnt.LE.0 ) GOTO 800
            IF ( lstac(ipnt).GT.0 ) THEN
               ctok=' '
               WRITE(*,*) ctok(:indent),'IF ',cstac(ipnt)(:lstac(ipnt))
            END IF
            indent=indent+3
            ipnt=ipnt-1
         ELSE IF ( ctok(1:ltok).EQ.'ELSE' ) THEN
C- ELSE
            IF ( ipnt.LE.0 ) GOTO 800
            IF ( lstac(ipnt).GT.0 ) THEN
               ctok=' '
               WRITE(*,*) ctok(:indent),cstac(ipnt)(:lstac(ipnt))
            END IF
            WRITE(*,*) ctok(:indent-3),'ELSE'
         ELSE IF ( ctok(1:ltok).EQ.'THEN' ) THEN
C- THEN
            IF ( ipnt.LE.0 ) GOTO 800
            IF ( lstac(ipnt).GT.0 ) THEN
               ctok=' '
               WRITE(*,*) ctok(:indent),cstac(ipnt)(:lstac(ipnt))
            END IF
            indent=indent-3
            WRITE(*,*) ctok(:indent),'THEN'
         ELSE IF ( ctok(1:ltok).EQ. ':' ) THEN
C- :
            WRITE(*,*) ctok(:ltok)
         ELSE IF ( ctok(1:ltok).EQ.';' ) THEN
C- ;
            IF ( ipnt.EQ.0 ) THEN
               WRITE(*,111)
  111          FORMAT('; Return: ',A)
            ELSE
               WRITE(*,111) cstac(ipnt)(:lstac(ipnt))
            END IF
            WRITE(*,*)
            lpc=lpc+idelpc
            GOTO 90
         ELSE IF ( ctok(1:2).EQ.'DR' .OR. ctok(1:ltok).EQ.'.' ) THEN
C- DRop .
            IF ( ipnt.LE.0 ) GOTO 800
            ipnt=ipnt-1
         ELSE IF ( ctok(1:2).EQ.'DU' ) THEN
C- DUp
            IF ( ipnt.LE.0 ) GOTO 800
            cstac(ipnt+1)=cstac(ipnt)
            lstac(ipnt+1)=lstac(ipnt)
            isum(ipnt+1)=isum(ipnt)
            ipnt=ipnt+1
         ELSE IF ( ctok(1:2).EQ.'SW' ) THEN
C- SWap
            IF ( ipnt.LE.1 ) GOTO 800
            cbuf=cstac(ipnt)
            cstac(ipnt)=cstac(ipnt-1)
            cstac(ipnt-1)=cbuf
            lbuf=lstac(ipnt)
            lstac(ipnt)=lstac(ipnt-1)
            lstac(ipnt-1)=lbuf
         ELSE IF ( idelsp.EQ.1 ) THEN
C- Push number onto stack
            IF ( ipnt.GE.MXSTAC ) GOTO 800
            ipnt=ipnt+1
            cstac(ipnt)=ctok(:ltok)
            lstac(ipnt)=ltok
            isum(ipnt)=0
         ELSE IF ( idelsp.EQ.-1 ) THEN
C- Binary operation, first check that each of the two terms being
C- combined does not contain a + or - operator signaled by isum.NE.0.
C- If this is true then the term will need to be enclosed in '()'.
            IF ( ipnt.LE.1 ) GOTO 800
            IF ( isum(ipnt-1).GT.0 .OR.
     :       (ctok(1:1).EQ.'^' .AND. isum(ipnt-1).NE.0) ) THEN
               cbuf='('//cstac(ipnt-1)(:lstac(ipnt-1))//')'
               cstac(ipnt-1)=cbuf
               lstac(ipnt-1)=lstac(ipnt-1)+2
            END IF
            IF ( isum(ipnt).GT.0 .OR.
     :       (ctok(1:1).EQ.'^' .AND. isum(ipnt).NE.0) ) THEN
               cbuf='('//cstac(ipnt)(:lstac(ipnt))//')'
               cstac(ipnt)=cbuf
               lstac(ipnt)=lstac(ipnt)+2
            END IF
C-
            cbuf=cstac(ipnt-1)(:lstac(ipnt-1))//ctok(:ltok)//
     :       cstac(ipnt)(:lstac(ipnt))
            ipnt=ipnt-1
            lstac(ipnt)=LENACT(cbuf)
            cstac(ipnt)=cbuf(:lstac(ipnt))
            IF ( ctok(1:1).EQ.'+' .OR. ctok(1:1).EQ.'-' ) THEN
               isum(ipnt)= 1
            ELSE
               isum(ipnt)=-1
            END IF
         ELSE
C- Uniary operator
            IF ( ipnt.LE.0 ) GOTO 800
            IF ( ctok(2:2).EQ.'+' .OR. ctok(2:2).EQ.'-' ) THEN
C- Special treatment for 1+, 1-, 2+ and 2-
               cbuf='('//cstac(ipnt)(:lstac(ipnt))//')'//
     :          ctok(2:2)//ctok(1:1)
               isum(ipnt)=1
            ELSE
               cbuf=ctok(:ltok)//'('//cstac(ipnt)(:lstac(ipnt))//')'
               isum(ipnt)=0
            END IF
            cstac(ipnt)=cbuf
            lstac(ipnt)=LENACT(cbuf)
         END IF
C-
         IF ( ipnt.GT.0 .AND. lstac(ipnt).GE.40 ) THEN
            icnt=icnt+1
            cbuf='T'
            lbuf=1
            CALL CRAMI(icnt,cbuf,lbuf)
            WRITE(*,211) cbuf(:lbuf),cstac(ipnt)(:lstac(ipnt))
  211       FORMAT(1X,A4,'==',A)
            cstac(ipnt)=cbuf
            lstac(ipnt)=lbuf
            isum(ipnt)=0
         END IF
C         WRITE(*,*) I,ipnt,'  ',cstac(ipnt)(:lstac(ipnt))
      lpc=lpc+idelpc
      GOTO 100
C---
  800 CONTINUE
      WRITE(*,*) 'CODLIS--Stack error.'
      WRITE(*,*) cstac(ipnt)(:lstac(ipnt))
      RETURN
C---
  900 RETURN
      END
C*********
      SUBROUTINE CODTOK(Idir, Ctok, Ltok, Icode, Idelpc, Idelsp, Ier)
      INTEGER   Idir, Ltok, Icode, Idelsp, Idelpc, Ier
      CHARACTER Ctok*(*)
C---
C If Idir.LT.0 then
C  disassemble Icode into Ctok, Idelsp=change in stack
C Idir.GT.0
C  compile Ctok(:Ltok) to Icode, Idelpc=number of program steps used.
C---
C 1989-Jan-29 - [AFT]
C---
      REAL  NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXTOK, MXDEF
      PARAMETER (MXTOK=67, MXDEF=16)
C
      REAL      FPNUM, FNCLOA
      INTEGER   ISNUM, LENACT
C
      CHARACTER code(MXTOK)*4, cdef(MXDEF)*6
      SAVE      code,          cdef
      CHARACTER ctmp*132
      REAL      tmp, VALUE
      INTEGER   I, itmp, iend, LDEF, LOC
      INTEGER   INAME
      SAVE      INAME
      INTEGER   NDEL(MXTOK), IDEFS(MXDEF)
      SAVE      NDEL,        IDEFS
C
      DATA code/
     :   '+ ',    '- ',    '* ',    '/ ',    '^ ',    'DEPt',  'DRop',
     :   'DUP',   'OVer',  'PICK',  'ROLL',  'ROT',   'SWap',  '?Dup',
     :   'ABS',   'NEG',   '1/',    'PI',    'LN',    'EXP',   'LOG',
     :   'ALog',  'SQrt',  'COS',   'SIN',   'TAN',   'ACos',  'ASin',
     :   'ATan',  'A2tn',  'HCos',  'HSin',  'HTan',  'INT',   'NInt',
     :   'MOD',   '/MOD',  'TSIg',  'DTor',  'RTod',  'DMsd',  'DDms',
     :   'ERFC',   'ERF',  'GAMM',
     :   '<',     '=',     '>',     '0<',    '0=',    '0>',    'NOT',
     :   '.',     '1+',    '1-',    '2+',    '2-',    'MIN',   'MAX',
     :   'STO',   '+STO',  'RCL',   '?',     'VAR',   ':',     ';',
     :   'ABO'/
      DATA NDEL/
     :   -1,      -1,      -1,      -1,      -1,      1,       -1,
     :   0,       1,       0,       0,       0,       0,       1,
     :   0,       0,       0,       1,       0,       0,       0,
     :   0,       0,       0,       0,       0,       0,       0,
     :   0,       -1,      0,       0,       0,       0,       0,
     :   -1,      -1,      -1,      0,       0,       0,       0,
     :   0,       0,       0,
     :   -1,      -1,      -1,      0,       0,       0,       0,
     :   -1,      0,       0,       0,       0,       -1,      -1,
     :   -2,      -2,      1,       -1,      0,       0,       0,
     :   -999/
      DATA cdef/
     :   'X',   'IF',  'ELSE',  'THEN',  'FOR',   'LOOP', '+LOOP',
     :   'I',   'J',   'LEAVE', 'BEGIN', 'UNTIL', 'WHILE','REPEAT',
     :   'EXIT','Y'/
      DATA IDEFS/
     :    1,    -1,    0,       0,       -1,      0,      -1,
     :    1,    1,     0,       0,       -1,      -1,      0,
     :    0,     1/
      DATA INAME/0/
C---
      Idelsp=0
      Idelpc=1
      Ier=0
      IF ( Idir.LT.0 ) THEN
         IF ( Icode.GT.6000 ) THEN
C- VAR's and : name.
            CALL CODITC(Icode, Ctok, Ltok, Ier)
         ELSE IF ( Icode.GT.4000 ) THEN
            Ctok='P'
            Ltok=1
            itmp=Icode-4000
            CALL CRAMI(itmp,Ctok,Ltok)
         ELSE IF ( Icode.GT.2000 ) THEN
            LOC=Icode-2000
            tmp=FNCLOA(-2,LOC,0,VALUE)
            Ltok=0
            CALL CRAMF(VALUE,Ctok,Ltok)
         ELSE IF ( 0.LT.Icode .AND. Icode.LE.MXTOK ) THEN
            Ctok=code(Icode)
            Ltok=LENACT(Ctok)
         ELSE IF ( -MXDEF.LE.Icode .AND. Icode.LT.0 ) THEN
            Ctok=cdef(-Icode)
            Ltok=LENACT(Ctok)
         ELSE
            Ctok='END'
            Ltok=3
         END IF
      ELSE
C- Compile the token
         CALL UPC(Ctok)
         Icode=0
         DO I=1,MXTOK
            IF ( Ctok(1:2).EQ.code(I)(1:2) ) THEN
C- First two characters match.  Look for non-upper case to decide where
C- match should end.
               iend = 4
               itmp=ICHAR(code(I)(4:4))
               IF ( itmp.LT.65 .OR. 90.LT.itmp ) iend=3
               itmp=ICHAR(code(I)(3:3))
               IF ( itmp.LT.65 .OR. 90.LT.itmp ) iend=2
               itmp=ICHAR(code(I)(2:2))
               IF ( itmp.LT.65 .OR. 90.LT.itmp ) iend=1
               IF ( ltok.GE.iend ) THEN
C User entered at least minimum, match all the user typed.
                  ctmp = code(i)
                  CALL UPC(ctmp)
                  itmp = MIN(ltok,LENACT(ctmp))
                  IF ( Ctok(1:ltok).EQ.ctmp(:itmp) ) THEN
                     Icode=I
                     GOTO 300
                  END IF
               END IF
            END IF
         END DO
C---
C- Now consider words that can only appear in programs.
         IF ( Ltok.LT.LEN(Ctok) ) Ctok(Ltok+1:)=' '
         DO I=1,MXDEF
            LDEF=LENACT(cdef(I))
            IF ( LDEF.EQ.Ltok ) THEN
C- Must be exact length
               IF ( Ctok(:LDEF).EQ.cdef(I)(:LDEF) ) THEN
                  Icode=-I
                  GOTO 300
               END IF
            END IF
         END DO
C---
C- Check for special cases (parameter or constant)
         IF ( ISNUM(Ctok,Ltok).NE.0 ) THEN
C- A constant
            tmp=FPNUM(Ctok,Ltok,Ier)
            tmp=FNCLOA(2,0,Icode,tmp)
            GOTO 300
         END IF
         IF ( Ctok(1:1).EQ.'P' ) THEN
            IF ( ISNUM(Ctok(2:),Ltok-1).NE.0 ) THEN
C- Parameter number.
               Ltok=Ltok-1
               Icode=4000+MIN(NINT(FPNUM(Ctok(2:Ltok),Ltok,Ier)),100)
               GOTO 300
            END IF
         END IF
C- Last chance, check user-defined dictionary word.
         CALL CODCTI(Ctok, Ltok, Icode, Ier)
C---
C- Check for VARiable definations when compiling.
  300    CONTINUE
         IF ( Ier.EQ.0 ) THEN
C- Got a match, make sure not in VARiable defination.
            IF ( INAME.NE.0 ) THEN
               Idelpc=0
               Icode=0
               WRITE(*,*) 'CODTOK--Not allowed to redefine key word.'
               Ier=1
            END IF
C- Set flag to indicate that next token should go into dictionary.
            IF ( Ctok(1:Ltok).EQ.'VAR' ) THEN
               Idelpc=0
               INAME=1
            ELSE IF ( Ctok(1:Ltok).EQ.':' ) THEN
               INAME=2
            END IF
         ELSE
C- No match
            IF ( INAME.NE.0 ) THEN
C- We have a new dictionary word.
               Idelpc=0
               IF ( INAME.EQ.1 ) THEN
                  tmp=FNCLOA(+3,0,Icode,VALUE)
               ELSE
                  tmp=FNCLOA(-6,itmp,Icode,VALUE)
                  Icode=8000+itmp
               END IF
               CALL CODADD(Ctok, Ltok, Icode, Ier)
               INAME=0
            END IF
         END IF
      END IF
C---
  800 CONTINUE
      IF ( Ier.EQ.0 ) THEN
         IF ( Icode.EQ.-2 .OR. Icode.EQ.-3 .OR. Icode.EQ.-5 .OR.
     :      Icode.EQ.-6 .OR. Icode.EQ.-7 .OR. Icode.EQ.-13 ) Idelpc=2
         IF ( Icode.GT.2000 .AND. Icode.LT.8000 ) THEN
C- 2000, 4000, 6000 (const, parameter, variable)
            Idelsp=1
         ELSE IF ( 0.LT.Icode .AND. Icode.LE.MXTOK ) THEN
            Idelsp=NDEL(Icode)
         ELSE IF ( -MXDEF.LE.Icode .AND. Icode.LT.0 ) THEN
            Idelsp=IDEFS(-Icode)
         ELSE
            Idelsp=0
         END IF
      END IF
      END
C*********
      REAL FUNCTION FNCOD(ISTEP, INTER, X, PAR, NTERM, ier)
      REAL      X(2), PAR(*)
      INTEGER   ISTEP, INTER, NTERM, ier
C---
C On input:
C If ISTEP.LT.0 then execute the program step in INTER
C If ISTEP.EQ.0 then run program pointed at by IENTRY
C If ISTEP.GT.0 then take a single step
C On output
C ier=  0  for no error.
C ier= -1 if ISTEP<0 and INTER>0 and program is not compiling
C ier=100 when attempting to single step past final ;
C---
C ISTEP      I  Single step command
C X          I  The current value of X
C PAR(NTERM) I  The parameter array
C---
C 1989-Jan-29 - Latest mod [AFT]
C---
      REAL      NO, INF
      PARAMETER (NO=-1.2E-34, INF= 1.2E+34)
      INTEGER   MXPROG
      PARAMETER (MXPROG=1500)
      INTEGER   MXSTAC, MXMEM, MXPROC
      PARAMETER (MXSTAC=50, MXMEM=100, MXPROC=50)
C Note, ERF is an intinsic for g77.
      REAL      ERF, ERFC, GAMMA
C
      CHARACTER ctmp*8
      INTEGER*2 IPROG(MXPROG), IRET(MXSTAC), IPLOC(MXPROC)
      SAVE      IPROG,         IRET,         IPLOC
      INTEGER   IRPT, ISPT
      SAVE      IRPT, ISPT
      INTEGER   ltok
      REAL      STACK(MXSTAC), FMEM(MXMEM)
      SAVE      STACK,         FMEM
C
      REAL      tmp, RSEC
      INTEGER   I, icnt, IDEG, IMIN, INEW, INSTR, Itmp
      INTEGER   ICOMP, idelpc, idelsp, IENTRY, IPC
      SAVE      ICOMP, idelpc, idelsp, IENTRY, IPC
      INTEGER   IPCNT, LCLOC, lpc, LVAR
      SAVE      IPCNT, LCLOC, lpc, LVAR
      INTEGER   J, lev
C Used in ENTRY points
      REAL      FNCLOA, FNPMAT
      INTEGER   IFUNC, ILOC, Icode, K, LOC, IPNUM
      REAL      VALUE
C
      DATA  IPC/1/, ICOMP/0/, lpc/0/, IENTRY/2/
C---
      FNCOD=0.
      IF ( ICOMP.NE.0 ) THEN
         IF ( INTER.LT.0 ) THEN
            GOTO( 100,6020,6030,6040,6020,6060,6060, 100, 100, 100,
     :            100, 100, 100,6020, 100) -INTER
         END IF
  100    IF ( lpc.GE.MXPROG ) GOTO 950
         lpc=lpc+1
         IPROG(lpc)=INTER
C Number is MXTOK-1
         IF ( INTER.EQ.66 ) ICOMP=0
         RETURN
      ELSE
         IF ( ISTEP.LT.0 .AND. INTER.LT.0 ) THEN
C- Trying to interpret an instruction that can only be compiled.
            ier=-1
            RETURN
         END IF
      END IF
C---
      IF ( ISTEP.LT.0 ) THEN
C- Run code step passed in INTER
         IPC=IPC-1
         INSTR=INTER
      ELSE IF ( ISTEP.EQ.0 ) THEN
C- Reset and run entire program
         IRPT=0
         ISPT=0
         IPC=IENTRY
         INSTR=IPROG(IPC)
      ELSE
C- Single step from current position
         INSTR=IPROG(IPC)
      END IF
C---
    5 CONTINUE
      GOTO(1010,1020,1030,1040,1050,1060,1070,1080,1090,1100,
     :     1110,1120,1130,1140,1150,1160,1170,1180,1190,1200,
     :     1210,1220,1230,1240,1250,1260,1270,1280,1290,1300,
     :     1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,
     :     1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,
     :     1510,1520,1530,1540,1550,1560,1570,1580,1590,1600,
     :     1610,1620,1630,1640,1650,1660,1670) INSTR
C-
      IF ( INSTR.LT.0 ) THEN
         GOTO(5010,5020,5030,5040,5050,5060,5070,5080,5090,5100,
     :        5110,5120,5130,5140,5150,5160) -INSTR
         GOTO 900
      ELSE IF ( INSTR.GE.8000 ) THEN
C- GSB subroutine.
         IF ( IRPT.GE.MXSTAC ) GOTO 920
         IRPT=IRPT+1
         IRET(IRPT)=IPC
         Itmp=INSTR-8000
         IF ( Itmp.GT.IPCNT ) GOTO 930
         IPC=IPLOC(Itmp)
      ELSE IF ( INSTR.GT.6000 ) THEN
C- Push variable address onto stack
         IF ( ISPT.GE.MXSTAC ) GOTO 920
         Itmp=INSTR-6000
         ISPT=ISPT+1
         STACK(ISPT)=Itmp
      ELSE IF ( INSTR.GT.4000 ) THEN
C- Push a parameter onto the stack
         IF ( ISPT.GE.MXSTAC ) GOTO 920
         Itmp=INSTR-4000
         ISPT=ISPT+1
         STACK(ISPT)=PAR(Itmp)
      ELSE IF ( INSTR.GE.2000 ) THEN
C- Push a constant onto the stack
         IF ( ISPT.GE.MXSTAC ) GOTO 920
         ISPT=ISPT+1
         Itmp=INSTR-2000
         STACK(ISPT)=FMEM(Itmp)
      ELSE
         GOTO 900
      END IF
      GOTO 880
C---
C- +
 1010 IF ( ISPT.LE.1 ) GOTO 910
      STACK(ISPT-1)=STACK(ISPT-1)+STACK(ISPT)
      ISPT=ISPT-1
      GOTO 880
C- -
 1020 IF ( ISPT.LE.1 ) GOTO 910
      STACK(ISPT-1)=STACK(ISPT-1)-STACK(ISPT)
      ISPT=ISPT-1
      GOTO 880
C- *
 1030 IF ( ISPT.LE.1 ) GOTO 910
      STACK(ISPT-1)=STACK(ISPT-1)*STACK(ISPT)
      ISPT=ISPT-1
      GOTO 880
C- /
 1040 IF ( ISPT.LE.1 ) GOTO 910
      IF ( STACK(ISPT).NE.0. ) THEN
         STACK(ISPT-1)=STACK(ISPT-1)/STACK(ISPT)
      ELSE
         STACK(ISPT-1)=INF
      END IF
      ISPT=ISPT-1
      GOTO 880
C- ^
 1050 IF ( ISPT.LE.1 ) GOTO 910
      IF ( STACK(ISPT-1).EQ.0.0 ) THEN
         IF ( STACK(ISPT).EQ.0.0 ) GOTO 970
C zero to non-zero power.
         STACK(ISPT-1)=0.0
      ELSE
         IF ( STACK(ISPT-1).LT.0. ) GOTO 970
            tmp=STACK(ISPT)*LOG(STACK(ISPT-1))
            IF ( tmp.LE.78. ) THEN
            STACK(ISPT-1)=EXP(tmp)
         ELSE
            STACK(ISPT-1)=INF
         END IF
      END IF
      ISPT=ISPT-1
      GOTO 880
C- DEPth Return the current stack depth
 1060 IF ( ISPT.GE.MXSTAC ) GOTO 920
      STACK(ISPT+1)=ISPT
      ISPT=ISPT+1
      GOTO 880
C- DROP Lose top of stack
 1070 IF ( ISPT.GE.1 ) ISPT=ISPT-1
      GOTO 880
C- DUP Duplicate top of stack
 1080 IF ( ISPT.LE.0 ) GOTO 910
      IF ( ISPT.GE.MXSTAC ) GOTO 920
      STACK(ISPT+1)=STACK(ISPT)
      ISPT=ISPT+1
      GOTO 880
C- OVER Duplicate second item from top
 1090 IF ( ISPT.LT.2 ) GOTO 910
      IF ( ISPT.GE.MXSTAC ) GOTO 920
      STACK(ISPT+1)=STACK(ISPT-1)
      ISPT=ISPT+1
      GOTO 880
C- PICK Duplicate nth item on top of stack
 1100 IF ( ISPT.LE.0 ) GOTO 910
      Itmp=ISPT-NINT(STACK(ISPT))
      IF ( Itmp.LE.0 .OR. Itmp.GE.ISPT ) GOTO 910
      STACK(ISPT)=STACK(Itmp)
      GOTO 880
C- ROLL Rotate nth item to top
 1110 IF ( ISPT.LE.0 ) GOTO 910
      Itmp=ISPT-NINT(STACK(ISPT))
      ISPT=ISPT-1
      IF ( Itmp.LE.0 .OR. Itmp.GE.ISPT ) GOTO 910
      tmp=STACK(Itmp)
      DO 1115 J=Itmp,ISPT-1
         STACK(J)=STACK(J+1)
 1115 CONTINUE
      STACK(ISPT)=tmp
      GOTO 880
C- ROTRotate third item to top
 1120 IF ( ISPT.LT.3 ) GOTO 910
      tmp=STACK(ISPT-2)
      STACK(ISPT-2)=STACK(ISPT-1)
      STACK(ISPT-1)=STACK(ISPT)
      STACK(ISPT)=tmp
      GOTO 880
C- SWAP Reverse top two stack items
 1130 IF ( ISPT.LT.2 ) GOTO 910
      tmp=STACK(ISPT-1)
      STACK(ISPT-1)=STACK(ISPT)
      STACK(ISPT)=tmp
      GOTO 880
C- ?DUP Duplicate only if non-zero
 1140 IF ( ISPT.LE.0 ) GOTO 910
      IF ( STACK(ISPT).NE.0. ) THEN
         IF ( ISPT.GE.MXSTAC ) GOTO 920
         STACK(ISPT+1)=STACK(ISPT)
         ISPT=ISPT+1
      END IF
      GOTO 880
C- ABS Absolute value
 1150 STACK(ISPT)=ABS(STACK(ISPT))
      GOTO 880
C- NEG Negate
 1160 STACK(ISPT)=-STACK(ISPT)
      GOTO 880
C- 1/ (reciprocal)
 1170 IF (ABS(STACK(ISPT)) .LE. 1.0E-34) THEN
         STACK(ISPT)=INF
      ELSE
         STACK(ISPT)=1.0/STACK(ISPT)
      END IF
      GOTO 880
C- PI (Pi)
 1180 IF ( ISPT.GE.MXSTAC ) GOTO 920
      STACK(ISPT+1)=ATAN2(0.0,-1.0)
      ISPT=ISPT+1
      GOTO 880
C- LN (natural log)
 1190 IF ( STACK(ISPT).LE.0. ) GOTO 970
      STACK(ISPT)=LOG(STACK(ISPT))
      GOTO 880
C- EXP
 1200 tmp=MAX(-80.,MIN(STACK(ISPT),80.))
      STACK(ISPT)=EXP(tmp)
      GOTO 880
C- LOG (common log)
 1210 IF ( STACK(ISPT).LE.0. ) GOTO 970
      STACK(ISPT)=LOG10(STACK(ISPT))
      GOTO 880
C- ALOG (Anti common log)
 1220 tmp=MAX(-34.,MIN(STACK(ISPT),34.))
      STACK(ISPT)=10.0**tmp
      GOTO 880
C- SQRT (square root)
 1230 STACK(ISPT)=SQRT(ABS(STACK(ISPT)))
      GOTO 880
C- COS
 1240 STACK(ISPT)=COS(STACK(ISPT))
      GOTO 880
C- SIN
 1250 STACK(ISPT)=SIN(STACK(ISPT))
      GOTO 880
C- TAN
 1260 STACK(ISPT)=TAN(STACK(ISPT))
      GOTO 880
C- ACOS (arc cosine)
 1270 IF (ABS(STACK(ISPT)) .LE. 1.0) THEN
         STACK(ISPT)=ACOS(STACK(ISPT))
      ELSE
         STACK(ISPT)=INF
      END IF
      GOTO 880
C- ASIN (arc sin)
 1280 IF (ABS(STACK(ISPT)) .LE. 1.0) THEN
         STACK(ISPT)=ASIN(STACK(ISPT))
      ELSE
         STACK(ISPT)=INF
      END IF
      GOTO 880
C- ATAN (arc tan)
 1290 STACK(ISPT)=ATAN(STACK(ISPT))
      GOTO 880
C- A2TN (arc tan 2 arguments)
 1300 IF ( ISPT.LE.1 ) GOTO 910
      IF ( STACK(ISPT).EQ.0.0 .AND. STACK(ISPT-1).EQ.0.0 ) THEN
         STACK(ISPT-1)=0.0
      ELSE
         STACK(ISPT-1)=ATAN2(STACK(ISPT-1),STACK(ISPT))
      END IF
      ISPT=ISPT-1
      GOTO 880
C- HCOS (Hyperbolic cosine)
 1310 STACK(ISPT)=COSH(STACK(ISPT))
      GOTO 880
C- HSIN (Hyperbolic sine)
 1320 STACK(ISPT)=SINH(STACK(ISPT))
      GOTO 880
C- HTAN (Hyperbolic tangent)
 1330 STACK(ISPT)=TANH(STACK(ISPT))
      GOTO 880
C- INT (Integer truncation)
 1340 STACK(ISPT)=INT(STACK(ISPT))
      GOTO 880
C- NINT (Nearest integer)
 1350 STACK(ISPT)=NINT(STACK(ISPT))
      GOTO 880
C- MOD (Remainder a1-a2*[a1/a2])
 1360 IF ( ISPT.LE.1 ) GOTO 910
      STACK(ISPT-1)=MOD(STACK(ISPT-1),STACK(ISPT))
      ISPT=ISPT-1
      GOTO 880
C- /MOD (Remainder a1-a2*[a1/a2], INT(a1/a2)
 1370 IF ( ISPT.LE.1 ) GOTO 910
      tmp=STACK(ISPT-1)
      STACK(ISPT-1)=MOD(STACK(ISPT-1),STACK(ISPT))
      STACK(ISPT)=INT(tmp/STACK(ISPT))
      GOTO 880
C- TSig (Transfer of sign |a1| Sign a2)
 1380 IF ( ISPT.LE.1 ) GOTO 910
      STACK(ISPT-1)=SIGN(STACK(ISPT-1),STACK(ISPT))
      ISPT=ISPT-1
      GOTO 880
C- DTor (Convert degrees to radians)
 1390 STACK(ISPT)=STACK(ISPT)*ATAN2(0.0,-1.0)/180.0
      GOTO 880
C- RTod (Convert radians to degrees)
 1400 STACK(ISPT)=STACK(ISPT)*180.0/ATAN2(0.0,-1.0)
      GOTO 880
C- DMsd (Convert a number from DDDMMSS.S format to decimal degrees)
 1410 IDEG=INT(STACK(ISPT)/1.0E4)
      IMIN=INT((STACK(ISPT) - IDEG*1.0E4)/1.0E2)
      RSEC=STACK(ISPT) - IDEG*1.0E4 - IMIN*1.0E2
      STACK(ISPT)=IDEG + IMIN/60.0 + RSEC/3600.0
      GOTO 880
C- DDms (Convert a number from decimal degrees to DDDMMSS.S format)
 1420 IDEG=INT(STACK(ISPT))
      IMIN=INT((STACK(ISPT) - IDEG)*60.0)
      RSEC=STACK(ISPT) - IDEG - IMIN/60.0
      STACK(ISPT)=IDEG*1.0E4 + IMIN*1.0E2 + RSEC
      GOTO 880
C- ERFC
 1430 CONTINUE
      STACK(ISPT)=ERFC(STACK(ISPT))
      GOTO 880
C- ERF
 1440 CONTINUE
      STACK(ISPT)=ERF(STACK(ISPT))
      GOTO 880
C- Gamma
 1450 CONTINUE
      STACK(ISPT)=GAMMA(STACK(ISPT))
      GOTO 880
C- <
 1460 IF ( ISPT.LE.1 ) GOTO 910
         IF ( STACK(ISPT-1).LT.STACK(ISPT) ) THEN
         STACK(ISPT-1)=1.0
      ELSE
         STACK(ISPT-1)=0.0
      END IF
      ISPT=ISPT-1
      GOTO 880
C- =
 1470 IF ( ISPT.LE.1 ) GOTO 910
      IF ( STACK(ISPT-1).EQ.STACK(ISPT) ) THEN
         STACK(ISPT-1)=1.0
      ELSE
         STACK(ISPT-1)=0.0
      END IF
      ISPT=ISPT-1
      GOTO 880
C- >
 1480 IF ( ISPT.LE.1 ) GOTO 910
      IF ( STACK(ISPT-1).GT.STACK(ISPT) ) THEN
         STACK(ISPT-1)=1.0
      ELSE
         STACK(ISPT-1)=0.0
      END IF
      ISPT=ISPT-1
      GOTO 880
C- 0<
 1490 IF ( ISPT.LE.0 ) GOTO 910
      IF ( STACK(ISPT).LT.0.0 ) THEN
         STACK(ISPT)=1.0
      ELSE
         STACK(ISPT)=0.0
      END IF
      GOTO 880
C- 0=
 1500 IF ( ISPT.LE.0 ) GOTO 910
      IF ( STACK(ISPT).EQ.0.0 ) THEN
         STACK(ISPT)=1.0
      ELSE
         STACK(ISPT)=0.0
      END IF
      GOTO 880
C- 0>
 1510 IF ( ISPT.LE.0 ) GOTO 910
      IF ( STACK(ISPT).GT.0.0 ) THEN
         STACK(ISPT)=1.0
      ELSE
         STACK(ISPT)=0.0
      END IF
      GOTO 880
C- NOT
 1520 IF ( ISPT.LE.0 ) GOTO 910
      IF ( STACK(ISPT).EQ.0.0 ) THEN
         STACK(ISPT)=1.0
      ELSE
         STACK(ISPT)=0.0
      END IF
      GOTO 880
C- . (Print the number at the top of the stack).
 1530 IF ( ISPT.LE.0 ) GOTO 910
      WRITE(*,*) STACK(ISPT)
      ISPT=ISPT-1
      GOTO 880
C- 1+
 1540 IF ( ISPT.LE.0 ) GOTO 910
      STACK(ISPT)=STACK(ISPT)+1.
      GOTO 880
C- 1-
 1550 IF ( ISPT.LE.0 ) GOTO 910
      STACK(ISPT)=STACK(ISPT)-1.
      GOTO 880
C- 2+
 1560 IF ( ISPT.LE.0 ) GOTO 910
      STACK(ISPT)=STACK(ISPT)+2.
      GOTO 880
C- 2-
 1570 IF ( ISPT.LE.0 ) GOTO 910
      STACK(ISPT)=STACK(ISPT)-2.
      GOTO 880
C- MIN
 1580 IF ( ISPT.LE.1 ) GOTO 910
      ISPT=ISPT-1
      STACK(ISPT)=MIN(STACK(ISPT),STACK(ISPT+1))
      GOTO 880
C- MAX
 1590 IF ( ISPT.LE.1 ) GOTO 910
      ISPT=ISPT-1
      STACK(ISPT)=MAX(STACK(ISPT),STACK(ISPT+1))
      GOTO 880
C- STO
 1600 IF ( ISPT.LT.2 ) GOTO 910
      Itmp=STACK(ISPT)
      IF ( Itmp.LE.0 .OR. Itmp.GT.LVAR ) GOTO 940
      FMEM(Itmp)=STACK(ISPT-1)
      ISPT=ISPT-2
      GOTO 880
C- +STO
 1610 IF ( ISPT.LT.2 ) GOTO 910
      Itmp=STACK(ISPT)
      IF ( Itmp.LE.0 .OR. Itmp.GT.LVAR ) GOTO 940
      FMEM(Itmp)=FMEM(Itmp)+STACK(ISPT-1)
      ISPT=ISPT-2
      GOTO 880
C- RCL
 1620 IF ( ISPT.LE.0 ) GOTO 910
      Itmp=STACK(ISPT)
      IF ( Itmp.LE.0 .OR. Itmp.GT.LVAR ) GOTO 940
      STACK(ISPT)=FMEM(Itmp)
      GOTO 880
C- ?
 1630 IF ( ISPT.LE.0 ) GOTO 910
      Itmp=STACK(ISPT)
      IF ( Itmp.LE.0 .OR. Itmp.GT.LVAR ) GOTO 940
      WRITE(*,*) FMEM(Itmp)
      ISPT=ISPT-1
      GOTO 880
C- VAR (NOP)
 1640 GOTO 880
C- : (Begin colon defination).  Cannot execute when compiling or
C-   running a program.
 1650 IF ( ISTEP.GE.0 .OR. ICOMP.NE.0 ) GOTO 930
      ICOMP=1
      IF ( lpc.GE.MXPROG ) GOTO 950
      lpc=lpc+1
      IPROG(lpc)=INSTR
      IF ( IPCNT.GE.MXPROC ) GOTO 920
      IPCNT=IPCNT+1
      IPLOC(IPCNT)=lpc
      RETURN
C- ;
 1660 IF ( ICOMP.NE.0 ) THEN
         ICOMP=0
         RETURN
      ELSE
         IF ( IRPT.LE.0 ) THEN
            IF ( ISTEP.GT.0 ) ier=100
            GOTO 900
         END IF
         IRPT=IRPT-1
         IPC=IRET(IRPT+1)
      END IF
      GOTO 880
C- ABOrt
 1670 IPC=1
      IRPT=0
      ISPT=0
      FNCOD=NO
      RETURN
C---
C- X  (place current X into stack).
 5010 IF ( ISPT.GE.MXSTAC ) GOTO 920
      ISPT=ISPT+1
      STACK(ISPT)=X(1)
      GOTO 880
C- IF
 5020 IF ( ISPT.LE.0 ) GOTO 910
      ISPT=ISPT-1
      IPC=IPC+1
      IF ( STACK(ISPT+1).EQ.0.0 ) THEN
C- Not true, search for ELSE or THEN statement
         IF ( IPROG(IPC).GT.0 ) THEN
            IPC=IPROG(IPC)
C- Special treatment for ELSE.
            IF ( IPROG(IPC).EQ.-3 ) IPC=IPC+1
            GOTO 880
         ELSE
            GOTO 950
         END IF
      END IF
      GOTO 880
 6020 IF ( lpc.GE.MXPROG-1 ) GOTO 950
      lpc=lpc+1
      IPROG(lpc)=INTER
      lpc=lpc+1
      IPROG(lpc)=0
      IRPT=IRPT+1
      IRET(IRPT)=lpc
      RETURN
C- ELSE, Assume correct nesting, search for closing THEN statement
 5030 lev=0
      IPC=IPC+1
      IF ( IPROG(IPC).GT.0 ) THEN
         IPC=IPROG(IPC)
         GOTO 880
      ELSE
         GOTO 950
      END IF
      GOTO 880
 6030 IF ( lpc.GE.MXPROG-1 ) GOTO 950
      lpc=lpc+1
      IPROG(lpc)=INTER
      IF ( IRPT.LE.0 ) GOTO 950
      IF ( IPROG(IRET(IRPT)).NE.0 ) GOTO 950
      IPROG(IRET(IRPT))=lpc
      lpc=lpc+1
      IRET(IRPT)=lpc
      RETURN
C- THEN, Assume correctly stuctured program.
 5040 GOTO 880
 6040 IF ( lpc.GE.MXPROG-1 ) GOTO 950
      lpc=lpc+1
      IPROG(lpc)=INTER
      IF ( IRPT.LE.0 ) GOTO 950
      IF ( IPROG(IRET(IRPT)).NE.0 ) GOTO 950
      IPROG(IRET(IRPT))=lpc
      IRPT=IRPT-1
      RETURN
C- FOR
 5050 IF ( IRPT+3.GT.MXSTAC ) GOTO 920
      IF ( ISPT.LE.1 ) GOTO 910
      IPC=IPC+1
      IF ( IPROG(IPC).LE.0 ) GOTO 930
      IRPT=IRPT+3
      IRET(IRPT)=IPROG(IPC)
      IRET(IRPT-1)=NINT(STACK(ISPT))
      IRET(IRPT-2)=NINT(STACK(ISPT-1))
      ISPT=ISPT-2
      GOTO 880
C- LOOP
 5060 IF ( IRPT.LT.3 ) GOTO 910
      IRET(IRPT-1)=IRET(IRPT-1)+1
      IF ( IRET(IRPT-1).LE.IRET(IRPT-2) ) THEN
         IPC=IPROG(IPC+1)
      ELSE
         IRPT=IRPT-3
         IPC=IPC+1
      END IF
      GOTO 880
C---
 6060 IF ( lpc.GE.MXPROG-1 ) GOTO 950
      lpc=lpc+1
      IPROG(lpc)=INTER
      lpc=lpc+1
      IF ( IRPT.LE.0 ) GOTO 950
      IF ( IPROG(IRET(IRPT)).NE.0 ) GOTO 950
      IPROG(IRET(IRPT))=lpc
      IPROG(lpc)=IRET(IRPT)
      IRPT=IRPT-1
      RETURN
C- +LOOP
 5070 IF ( IRPT.LT.3 ) GOTO 910
      IF ( ISPT.LE.0 ) GOTO 920
      Itmp=IRET(IRPT-1)-IRET(IRPT-2)
      IRET(IRPT-1)=IRET(IRPT-1)+NINT(STACK(ISPT))
      INEW=IRET(IRPT-1)-IRET(IRPT-2)
      ISPT=ISPT-1
      IF ( (Itmp.LE.0 .AND. INEW.GT.0) .OR.
     :    (Itmp.GE.0 .AND. INEW.LT.0) ) THEN
C- terminate
         IRPT=IRPT-3
         IPC=IPC+1
      ELSE
C- loop
         IPC=IPROG(IPC+1)
      END IF
      GOTO 880
C- I
 5080 IF ( IRPT.LT.3 ) GOTO 930
      IF ( ISPT.GE.MXSTAC ) GOTO 920
      ISPT=ISPT+1
      Itmp=IRPT
 5085 IF ( IPROG(IRET(Itmp)-1).EQ.-6 ) THEN
         STACK(ISPT)=IRET(Itmp-1)
         GOTO 880
      END IF
      Itmp=Itmp-1
      IF ( Itmp.GT.0 ) GOTO 5085
      GOTO 930
C- J
 5090 IF ( IRPT.LT.6 ) GOTO 930
      IF ( ISPT.GE.MXSTAC ) GOTO 920
      ISPT=ISPT+1
      Itmp=IRPT
      icnt=0
 5095 IF ( IPROG(IRET(Itmp)-1).EQ.-6 ) THEN
         STACK(ISPT)=IRET(Itmp-1)
         IF ( icnt.EQ.1 ) GOTO 880
         icnt=icnt+1
         Itmp=Itmp-2
      END IF
      Itmp=Itmp-1
      IF ( Itmp.GT.0 ) GOTO 5095
      GOTO 930
C- LEAVE
 5100 IF ( IRPT.LT.3 ) GOTO 910
      IPC=IRET(IRPT)
      IRPT=IRPT-3
      GOTO 880
C- BEGIN
 5110 IF ( IRPT.GE.MXSTAC ) GOTO 920
      IRPT=IRPT+1
      IRET(IRPT)=IPC
      GOTO 880
C- UNTIL
 5120 IF ( ISPT.LE.0 ) GOTO 910
      ISPT=ISPT-1
      IF ( STACK(ISPT+1).NE.0.0 ) THEN
C- TRUE
         IRPT=IRPT-1
      ELSE
C- FALSE
         IPC=IRET(IRPT)
      END IF
      GOTO 880
C- WHILE
 5130 IF ( ISPT.LE.0 ) GOTO 910
      ISPT=ISPT-1
      IPC=IPC+1
      IF ( STACK(ISPT+1).NE.0.0 ) THEN
C- TRUE
         GOTO 880
      ELSE
C- FALSE
         IRPT=IRPT-1
         IF ( IPROG(IPC).GT.0 ) THEN
            IPC=IPROG(IPC)
            GOTO 880
         ELSE
            lev=0
            J=IPC+1
 5135       CONTINUE
            IF ( IPROG(J).EQ.0 ) GOTO 930
            Itmp=IPROG(J)
            CALL CODTOK(-1,ctmp,ltok,Itmp,idelpc,idelsp,ier)
            IF ( IPROG(J).LT.0 ) THEN
               IF ( ctmp(1:5).EQ.'BEGIN' ) THEN
C- Nested BEGIN
                  lev=lev+1
               ELSE IF ( ctmp(1:5).EQ.'UNTIL' ) THEN
C- Nested UNTIL
                  lev=lev-1
               ELSE IF ( ctmp(1:6).EQ.'REPEAT' ) THEN
C- REPEAT
                  IF ( lev.LE.0 ) THEN
                     IPROG(IPC)=J
                     IPC=J
                     GOTO 880
                  END IF
                  lev=lev-1
               END IF
            END IF
            J=J+idelpc
            IF ( J.LT.MXPROG ) GOTO 5135
         END IF
      END IF
      GOTO 930
C- REPEAT
 5140 IF ( IRPT.LE.0 ) GOTO 910
      IPC=IRET(IRPT)
      GOTO 880
C- EXIT
 5150 DO 5155 I=IRPT,1,-1
         IF ( IPROG(IRET(I)).GT.8000 ) THEN
            IPC=IRET(I)
            IRPT=I-1
            GOTO 880
         END IF
 5155 CONTINUE
      GOTO 930
C- Y  (place current Y onto stack).
 5160 IF ( ISPT.GE.MXSTAC ) GOTO 920
      ISPT=ISPT+1
      STACK(ISPT)=X(2)
      GOTO 880
C---
  880 CONTINUE
      IPC=IPC+1
      IF ( IPC.GT.0 ) INSTR=IPROG(IPC)
      IF ( ISTEP.EQ.0 ) GOTO 5
C- Interactive, and a colon def
      IF ( ISTEP.LT.0 .AND. IRPT.GT.0 ) GOTO 5
C---
  900 IF ( ISPT.GT.0 ) THEN
         FNCOD=STACK(ISPT)
      ELSE
         FNCOD=NO
      END IF
      RETURN
C---
  910 WRITE(*,*) 'FNCOD--Stack empty'
      ISPT=0
      ier=1
      GOTO 980
C---
  920 WRITE(*,*) 'FNCOD--Stack overflow'
      ier=2
      GOTO 980
C---
  930 WRITE(*,*) 'FNCOD--Error in structure or nesting'
      ier=3
      GOTO 980
C--
  940 WRITE(*,*) 'FNCOD--Illegal variable address'
      ier=4
      GOTO 980
C---
  950 WRITE(*,*) 'FNCOD--Out of program memory'
      ier=5
      GOTO 980
C---
C- Undefined operation
  970 FNCOD=NO
      STACK(ISPT)=NO
      IF ( ISTEP.NE.0 ) WRITE(*,*) 'FNCOD--Math error.'
      RETURN
C---
  980 IF ( ISTEP.GE.0 ) THEN
         WRITE(*,*) 'At line',IPC
      END IF
      FNCOD=NO
      RETURN
C---
      ENTRY FNCLOA(IFUNC, ILOC, Icode, VALUE)
      FNCLOA = 0.
      IF ( IFUNC.EQ.1 ) THEN
C- Complete reset.
         DO 996 I=1,lpc
            IPROG(I)=0
  996    CONTINUE
         ICOMP=0
         IPCNT=0
         LCLOC=MXMEM+1
         LVAR=0
         lpc=0
C- reset the dictionary.
         CALL CODADD(' ',-1,0,Itmp)
      ELSE IF ( IFUNC.EQ.2 ) THEN
C- Return the code value for a constant term VALUE.
         Icode=0
         DO 998 I=LCLOC,MXMEM
            IF ( FMEM(I).EQ.VALUE ) THEN
               Icode=2000+I
               RETURN
            END IF
  998    CONTINUE
         IF ( LCLOC.LE.LVAR ) STOP 'FNCOD--OUT OF FLOATING MEMORY.'
         Itmp=LCLOC-1
C- Only allocate memory, if code is being compiled.
         IF ( ICOMP.NE.0 ) LCLOC=LCLOC-1
         FMEM(Itmp)=VALUE
         Icode=2000+Itmp
      ELSE IF ( IFUNC.EQ.3 ) THEN
C- Allocate memory to a new variable.
         IF ( LVAR.GE.LCLOC ) STOP 'FNCOD--OUT OF FLOATING MEMORY.'
         LVAR=LVAR+1
         Icode=6000+LVAR
      ELSE IF ( IFUNC.EQ.4 ) THEN
         IENTRY=ILOC
         IPC=ILOC
      ELSE IF ( IFUNC.EQ.-1 ) THEN
C- Program at specified location
         Icode=IPROG(ILOC)
      ELSE IF ( IFUNC.EQ.-2 ) THEN
C- Read constant
         VALUE=FMEM(ILOC)
      ELSE IF ( IFUNC.EQ.-3 ) THEN
C- Prepare to run program
         IF ( ISPT.GT.0 ) THEN
            VALUE=STACK(ISPT)
         ELSE
            VALUE=NO
         END IF
         IPC=IENTRY
         IRPT=0
         ISPT=0
      ELSE IF ( IFUNC.EQ.-4 ) THEN
C- Print stack
         IF ( ICOMP.EQ.0 ) THEN
            WRITE(*,*) (STACK(K),K=1,ISPT)
         END IF
      ELSE IF ( IFUNC.EQ.-5 ) THEN
C- Read code at current PC
         ILOC=IPC
         Icode=IPROG(IPC)
      ELSE IF ( IFUNC.EQ.-6 ) THEN
         ILOC=IPCNT
      ELSE IF ( IFUNC.EQ.-7 ) THEN
C- Read last valid program location
         ILOC=lpc
      END IF
      RETURN
C---
      ENTRY FNPMAT(LOC, IPNUM, ier)
      FNPMAT = 0.
      DO I=1,IPCNT
         IF ( LOC.EQ.IPLOC(I) ) THEN
            IPNUM=I
            ier=0
            RETURN
         END IF
      END DO
      IPNUM=0
      ier=1
      RETURN
      END
C*********
      SUBROUTINE CODADD(CWORD, LWORD, IVAL, ier)
      CHARACTER CWORD*(*)
      INTEGER   LWORD, IVAL, ier
C Entry CODCTI
      CHARACTER CWORD1*(*)
      INTEGER   LWORD1, IVAL1, ier1
C Entry CODITC
      CHARACTER CWORD2*(*)
      INTEGER   Icode2, LWORD2, ier2
C Entry CODWOR
      CHARACTER CWORD3*(*)
      INTEGER   IWNUM, LWORD3, ier3
C---
C Add a word to the dictionary.  The structure of a dictionary
C entry is:
C length  : contains
C ------    --------
C 1         LWORD, length of the current word in bytes
C 1         LPAR, length of the parameter field in bytes
C LWORD     CWORD(:LWORD) the word
C IPAR      the parameter field (currently is only the I*2 code).
C---
C 1989-Feb-13 - Latest mod [AFT]
C---
      INTEGER   MXDIC
      PARAMETER (MXDIC=1000)
      CHARACTER CDIC*(MXDIC)
      SAVE      CDIC
      CHARACTER C2tmp*2
      INTEGER   icnt, IPAR, IPOS, Itmp, ltok, LPAR
      INTEGER   LDIC
      SAVE      LDIC
      DATA  LDIC/0/
C---
      IF ( LWORD.LT.0 ) THEN
         LDIC=0
      ELSE IF ( LDIC+LWORD+4.GT.MXDIC ) THEN
         ier=1
      ELSE
         ier=0
         CDIC(LDIC+1:LDIC+1)=CHAR(LWORD)
         CDIC(LDIC+2:LDIC+2)=CHAR(2)
         CDIC(LDIC+3:LDIC+2+LWORD)=CWORD(:LWORD)
         CALL I4TOC2(IVAL, C2tmp)
         CDIC(LDIC+3+LWORD:LDIC+4+LWORD)=C2tmp
         LDIC=LDIC+4+LWORD
      END IF
      RETURN
C---
      ENTRY CODCTI(CWORD1, LWORD1, IVAL1, ier1)
C---
C Find the character CWORD in the dictionary.
C---
      IPOS=0
  200 IF ( IPOS.LT.LDIC ) THEN
         ltok=ICHAR(CDIC(IPOS+1:IPOS+1))
         LPAR=ICHAR(CDIC(IPOS+2:IPOS+2))
         Itmp=IPOS+2+ltok+LPAR
         IF ( LWORD1.EQ.ltok ) THEN
            IF ( CWORD1(:LWORD1).EQ.CDIC(IPOS+3:IPOS+2+ltok) ) THEN
               ier1=0
               CALL C2TOI4(CDIC(Itmp-1:Itmp), IVAL1)
               RETURN
            END IF
         END IF
         IPOS=Itmp
         GOTO 200
      END IF
      ier1=1
      RETURN
C---
      ENTRY CODITC(Icode2, CWORD2, LWORD2, ier2)
C---
C Find the code, Icode2, in the dictionary.
C---
      IPOS=0
  300 IF ( IPOS.LT.LDIC ) THEN
         ltok=ICHAR(CDIC(IPOS+1:IPOS+1))
         LPAR=ICHAR(CDIC(IPOS+2:IPOS+2))
         Itmp=IPOS+2+ltok+LPAR
         C2tmp=CDIC(Itmp-1:Itmp)
         CALL C2TOI4(C2tmp, IPAR)
         IF ( IPAR.EQ.Icode2 ) THEN
            LWORD2=ltok
            CWORD2(:LWORD2)=CDIC(IPOS+3:IPOS+2+LWORD2)
            ier2=0
            RETURN
         END IF
         IPOS=Itmp
         GOTO 300
      END IF
      LWORD2=0
      CWORD2(1:2)=' '
      ier2=1
      RETURN
C---
      ENTRY CODWOR(IWNUM, CWORD3, LWORD3, ier3)
C---
C Return the token corresponding to word number IWNUM.
C---
      icnt=0
      IPOS=0
  400 IF ( IPOS.LT.LDIC ) THEN
         icnt=icnt+1
         ltok=ICHAR(CDIC(IPOS+1:IPOS+1))
         LPAR=ICHAR(CDIC(IPOS+2:IPOS+2))
         IF ( icnt.EQ.IWNUM ) THEN
            LWORD3=ltok
            CWORD3(:LWORD3)=CDIC(IPOS+3:IPOS+2+LWORD3)
            ier3=0
            RETURN
         END IF
         IPOS=IPOS+2+ltok+LPAR
         GOTO 400
      END IF
      ier3=1
      RETURN
      END
C*********
      SUBROUTINE CODDER(X, PAR, PLIM, DERIV, NT, NTERM)
      REAL      X(2), PAR(*), PLIM(3,*), DERIV(*)
      INTEGER   NT, NTERM
C---
C This routine takes the derivative of the FNCOD function with
C respect to the various parameters.  This is done numerically.
C---
C 1988-Nov-30 - [AFT]
C---
      REAL      FNCOD
      REAL      DX, FNY0, Ptmp, tmp
      INTEGER   I, ier
C---
      FNY0 = FNCOD(0,0,X,PAR(NT),NTERM,ier)
      DO I=NT,NT+NTERM-1
         IF ( PLIM(1,I).GE.0. ) THEN
            Ptmp = PAR(I)
            tmp  = ABS(Ptmp)
            IF ( tmp.GT.0.01 .AND. tmp.LT.100. ) THEN
C- For scaled parameters, use a fixed step size.  This is more
C- numerically stable.
               DX = .001
            ELSE IF ( tmp.LT.1.E-15 ) THEN
C- Near zero, avoid divide by zero problems.
               DX = .001
            ELSE
               DX = .001*tmp
            END IF
            PAR(I) = PAR(I)+DX
            DERIV(I) = (FNCOD(0,0,X,PAR(NT),NTERM,ier)-FNY0)/DX
            PAR(I) = Ptmp
         END IF
      END DO
      RETURN
      END
C*********
      SUBROUTINE C2TOI4(cbuf, I4BUF)
      CHARACTER cbuf*2
      INTEGER   I4BUF
C---
C Convert a CHARACTER*2 into an INTEGER.
C---
      CHARACTER c2tmp*2
      INTEGER*2 i2tmp
      EQUIVALENCE (c2tmp,i2tmp)
C---
      c2tmp = cbuf
      I4BUF = i2tmp
      RETURN
      END
C*********
      SUBROUTINE I4TOC2(I4BUF, cbuf)
      INTEGER   I4BUF
      CHARACTER cbuf*2
C---
C Convert an INTEGER*4 into a CHARACTER*2
C---
      CHARACTER c2tmp*2
      INTEGER*2 i2tmp
      EQUIVALENCE (c2tmp,i2tmp)
C---
      i2tmp = I4BUF
      cbuf = c2tmp
      RETURN
      END
