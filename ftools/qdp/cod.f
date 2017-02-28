C Program COD
C---
C This is the interactive version of COD.  This program allows
C the user to read and test COD programs.
C---
C 1989-Feb-13 - Latest mod [AFT]
C---
      INTEGER   MXPAR
      PARAMETER (MXPAR=120)
      REAL   NO
      PARAMETER (NO=-1.2E-34)
      REAL      FNCLOA, FNCOD, FNPMAT, FPNUM
      INTEGER   ISNUM
C-
      CHARACTER cbuf*128
      CHARACTER ctok*128
      CHARACTER chlib*256
      REAL      pval(MXPAR)
      REAL      tmp, value, xval
      INTEGER   I, icode, idelpc, idelsp, iend, ier, ifirst
      INTEGER   ihi, ilo, iloc, ipc, ipnum, itmp
      INTEGER   kp1, lbuf, loc, ltok, nterm
      DATA ifirst/1/, xval/0.0/
C---
      chlib='cod'
      CALL PTEND('$LHEASOFT','help',chlib)
      WRITE(*,*) 'Type HELP for help.'
C- Reset
      tmp=FNCLOA(+1,0,0,0.)
C---
C- Print stack
  100 CONTINUE
      tmp=FNCLOA(-4,iloc,icode,value)
C-
  110 CONTINUE
      CALL GTBUF('COD>',ier)
      IF ( ier.LT.0 ) GOTO 900
C---
  150 CONTINUE
      CALL GTCHAR(ctok,ltok)
      IF ( ltok.LE.0 ) GOTO 100
C---
      CALL UPC(ctok)
      icode=-1
C- Check built-in function list
      CALL CODTOK(+1,ctok,ltok,icode,idelpc,idelsp,ier)
      IF ( ier.NE.0 ) GOTO 200
      IF ( idelpc.EQ.0 ) GOTO 150
      tmp=FNCOD(-1,icode,xval,pval,nterm,ier)
      IF ( ier.LT.0 ) THEN
         WRITE(*,*) 'COD--',ctok(:ltok),' can only be compiled.'
         GOTO 100
      END IF
      GOTO 150
C---
C- Check list of COD commands
  200 CONTINUE
      IF ( ctok(1:1).EQ.'A' ) THEN
C- ADD a COD file to existing COD functions
         CALL GTCHAR(ctok,ltok)
         kp1=0
         CALL CODFIL(ctok,ltok,kp1,nterm,ier)
         ipc=1
         GOTO 100
      ELSE IF ( ctok(1:1).EQ.'G' ) THEN
C- GEt a COD file
C- Reset all internal pointers for COD models.
         tmp=FNCLOA(+1,0,0,0.)
         CALL GTCHAR(ctok,ltok)
         kp1=0
         CALL CODFIL(ctok, ltok, kp1, nterm, ier)
         IF ( ier.NE.0 ) THEN
            WRITE(*,*) 'COD--Unable to open: ',ctok(:ltok)
            GOTO 100
         END IF
         ipc=1
         GOTO 100
      ELSE IF ( ctok(1:2).EQ.'HE' ) THEN
C- HElp
         ctok=' '
C         CALL GTHELP(chlib,ctok)
         CALL GETLUN(itmp)
         CALL GTXHLP(itmp, chlib, ctok)
         CALL FRELUN(itmp)
         GOTO 100
      ELSE IF ( ctok(1:1).EQ.'L' ) THEN
C- List
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF ( ctok(1:1).EQ.'C' ) THEN
C- LIst [ilo,ihi]
            ilo=1
            ihi=500
            CALL GTINT(ilo,ier)
            CALL GTINT(ihi,ier)
            ilo=MIN(MAX(1,ilo),500)
            ihi=MIN(MAX(1,ihi),500)
            loc=ilo
  230          tmp=FNCLOA(-1,loc,icode,tmp)
               CALL CODTOK(-1,ctok,ltok,icode,idelpc,idelsp,ier)
               IF ( ctok(:ltok).EQ.':' ) THEN
                  tmp=FNPMAT(loc,ipnum,ier)
                  IF ( ier.EQ.0 ) THEN
                     itmp=8000+ipnum
                     CALL CODITC(itmp,cbuf,lbuf,ier)
                     ctok=': '//cbuf(:lbuf)
                     ltok=lbuf+2
                  END IF
               END IF
               IF ( idelpc.LE.1 ) THEN
                  WRITE(*,231) loc,icode,ctok(:ltok)
  231             FORMAT(1X,I4,I7,A10)
               ELSE
                  tmp=FNCLOA(-1,loc+1,itmp,tmp)
                  WRITE(*,241) loc,icode,ctok(:ltok),itmp
  241             FORMAT(1X,I4,I7,A10,'   (',I6,')')
               END IF
            IF ( icode.EQ.0 ) GOTO 110
            loc=loc+idelpc
            IF ( loc.LE.ihi ) GOTO 230
         ELSE IF ( ctok(1:1).EQ.'D' ) THEN
C- LIst Dictionary (of built-in functions)
            WRITE(*,*) 'These words are available all the time:'
            lbuf=0
            cbuf=' '
            iend=0
            DO I=1, 200
               CALL CODTOK(-1,ctok,ltok,I,idelpc,idelsp,ier)
               IF ( ctok(1:3).EQ.'END' ) iend=1
               IF ( iend.EQ.0 ) THEN
                  cbuf(lbuf+1:lbuf+ltok)=ctok
                  lbuf=lbuf+MAX(ltok,6)+2
               END IF
               IF ( lbuf.GT.48 .OR. iend.NE.0 ) THEN
                  WRITE(*,*) cbuf(:lbuf)
                  lbuf=0
                  cbuf=' '
                  IF ( iend.NE.0 ) GOTO 300
               END IF
            END DO
C- Now list the functions that can only be used inside programs:
  300       CONTINUE
            WRITE(*,*)
            WRITE(*,*) 'These can only be used in colon definitions:'
            iend=0
            DO I=-1, -200, -1
               CALL CODTOK(-1,ctok,ltok,I,idelpc,idelsp,ier)
               IF ( ctok(1:3).EQ.'END' ) iend=1
               IF ( iend.EQ.0 ) THEN
                  cbuf(lbuf+1:lbuf+ltok)=ctok
                  lbuf=lbuf+MAX(ltok,6)+2
               END IF
               IF ( lbuf.GT.48 .OR. iend.NE.0 ) THEN
                  WRITE(*,*) cbuf(:lbuf)
                  lbuf=0
                  cbuf=' '
                  IF ( iend.NE.0 ) GOTO 320
               END IF
            END DO
C- List user-defined words (if any exist)
  320       CALL CODWOR(1, ctok, ltok, ier)
            IF ( ier.NE.0 ) GOTO 110
            WRITE(*,*)
            WRITE(*,*) 'The following user-defined words exist:'
            DO 330 I=1,200
               CALL CODWOR(I, ctok, ltok, ier)
               IF ( ier.EQ.0 ) THEN
                  cbuf(lbuf+1:lbuf+ltok)=ctok
                  lbuf=lbuf+MAX(ltok,16)+2
               END IF
               IF ( lbuf.GT.60 .OR. ier.NE.0 ) THEN
                  WRITE(*,*) cbuf(:lbuf)
                  IF ( ier.NE.0 ) GOTO 110
                  lbuf=0
                  cbuf=' '
               END IF
  330       CONTINUE
            GOTO 110
         ELSE IF ( ctok(1:1).EQ.'E' ) THEN
            CALL CODLIS
         ELSE IF ( ctok(1:1).EQ.'P' ) THEN
            IF ( nterm.LE.0 ) THEN
               WRITE(*,*) 'Current model contains no parameters.'
            ELSE
               DO 350 I=1,nterm
                  WRITE(*,341) I,pval(I)
  341             FORMAT(I3,': VAL(',1PG11.4,')')
  350          CONTINUE
            END IF
            WRITE(*,351) xval
  351       FORMAT(' X=',1PG11.4)
         ELSE
            WRITE(*,*) 'COD--Must say what you want to List.'
            GOTO 100
         END IF
         GOTO 110
      ELSE IF ( ctok(1:1).EQ.'N' ) THEN
C- Newpar
         CALL GTCHAR(ctok,ltok)
         IF ( ctok(1:1).EQ.'X' ) THEN
            CALL GTREAL(xval,ier)
         ELSE
            IF ( ltok.GT.0 ) itmp=FPNUM(ctok,ltok,ier)
            CALL GTREAL(tmp,ier)
            IF ( 0.LT.itmp .AND. itmp.LE.nterm ) THEN
               pval(itmp)=tmp
            ELSE
               WRITE(*,*) 'Parameter is out of range.'
            END IF
         END IF
         GOTO 100
      ELSE IF ( ctok(1:2).EQ.'EX' .OR. ctok(1:1).EQ.'Q' ) THEN
C- Quit
         GOTO 900
      ELSE IF ( ctok(1:2).EQ.'RU' ) THEN
C- RUn the program, get X and then RUN with it.
         tmp=FNCLOA(-3,iloc,icode,xval)
         tmp=FNCOD(0,0,xval,pval,nterm,ier)
         WRITE(*,*) ' FNCOD=',tmp
         GOTO 100
      ELSE IF ( ctok(1:1).EQ.'S' ) THEN
C- Step
         CALL GTCHAR(ctok,ltok)
         CALL UPC(ctok)
         IF ( ctok(1:1).EQ.'I' ) THEN
C- Step Init, reset internal parameters, and get next token
            ifirst=0
            tmp=FNCLOA(-3,iloc,icode,xval)
            CALL GTCHAR(ctok,ltok)
         END IF
         IF ( ifirst.EQ.1 ) THEN
C- Reset internal parameters
            ifirst=0
            tmp=FNCLOA(-3,iloc,icode,xval)
         END IF
         IF ( ISNUM(ctok,ltok).NE.0 ) THEN
C- Step [Init] #
            ihi=FPNUM(ctok,ltok,ier)
         ELSE
            ihi=1
         END IF
         DO 380 I=1,ihi
C- Get current ipc and icode
            tmp=FNCLOA(-5,ipc,icode,tmp)
            CALL CODTOK(-1,ctok,ltok,icode,idelpc,idelsp,ier)
            WRITE(*,231) ipc,icode,ctok(:ltok)
            IF ( icode.EQ.0 ) GOTO 100
            tmp=FNCOD(1,0,xval,pval,nterm,ier)
            IF ( ier.EQ.100 ) WRITE(*,*) ' FNCOD=',tmp
            IF ( ier.NE.0 ) GOTO 100
  380    CONTINUE
      ELSE
         WRITE(*,*) 'COD--Illegal token=',ctok(:ltok)
         GOTO 100
      END IF
      GOTO 150
C---
  900 CONTINUE
      CALL EDICOM('OFF',3)
      END
