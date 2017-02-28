      SUBROUTINE RDQDP(Ichat, Lunin, Cnam, Yray, Mxpts, Iery, Mxvec,
     :   Nrow, Npts, Nvec, Cmd, Mxcmd, Ncmd, Ier)
      INTEGER   Mxpts, Mxvec, Mxcmd
      CHARACTER Cnam*(*), Cmd(Mxcmd)*(*)
      REAL      Yray(Mxpts)
      INTEGER   Iery(Mxvec)
      INTEGER   Ichat, Lunin, Nrow, Npts, Nvec, Ncmd, Ier
C---
C Opens and reads a QDP file.
C---
C Ichat     I    >0 print row/col info, >10 means print comment lines,
C                =-1 do an HTML decode
C Lunin     I    <>0 means file already open on lun.
C Cnam      I/O  File name.
C Yray        O  The data array
C Mxpts     I    The actual size of the Y array.
C Iery        O  The PLT error flag array
C Mxvec     I    The actual size of the Iery array
C Nrow        O  Maximum number of rows that the file could contain.
C Npts,Nvec   O  Needed by PLT
C Cmd         O  Command array (Mxcmd input dimension).
C Ncmd        O  Number of commands read
C Ier         O  =-1 if user entered EOF, =0 file read, =1 no file read.
C---
C 1989-Jul-31 - Changed calling sequence to allow RDQDP to optimize storage.
C 1989-Feb-13 - Latest mod [AFT]
C---
      REAL      FPNUM
      INTEGER   ISNUM, LENACT
C
      CHARACTER cbuf*1024
      CHARACTER ctok*128, ctmp
      REAL      value
      INTEGER   i, icon, ierf, icol, ios, itmp, ix
      INTEGER   kp, lbuf, Lnam, ltok, lun, ncol
C---
   11 FORMAT(A)
C---
      Nvec=0
C- Open file if needed.
      lun=Lunin
      IF ( lun.EQ.0 ) THEN
  100    IF ( Cnam.EQ.' ' ) THEN
            CALL GTBUF('QDP file name:',Ier)
            IF ( Ier.LT.0 ) GOTO 900
            CALL GTCHAR(Cnam,Lnam)
         END IF
         IF ( Cnam.EQ.' ' ) THEN
            Ier=1
            GOTO 950
         END IF
  130    CALL XTEND(Cnam,'qdp')
         IF ( lun.EQ.0 ) THEN
            CALL GETlun(lun)
         ELSE
            CLOSE(UNIT=lun)
         END IF
         CALL OPENWR(lun,Cnam,'OLD',' ',' ',0,1,ios)
         IF ( ios.NE.0 ) THEN
            Cnam=' '
            GOTO 100
         END IF
      END IF
      INQUIRE(UNIT=lun,NAME=Cnam)
C---
      Ier=0
      DO i=1,Mxvec
         Iery(i)=0
      END DO
      Ncmd=0
      IF ( Ichat.GE.0 .AND. Ncmd.LT.Mxcmd ) THEN
         Ncmd=Ncmd+1
         Cmd(Ncmd)='LA F '//Cnam
      END IF
      Npts=0
C---
  200 CONTINUE
      READ(lun,11,END=800) cbuf
      lbuf=LENACT(cbuf)
C Skip blank lines.
      IF ( lbuf.EQ.0 ) GOTO 200
      IF ( ichat.EQ.-1 ) CALL HDECOD(cbuf, lbuf)
C---
      kp=0
      CALL ALFSKS(cbuf, lbuf, kp)
      IF ( cbuf(kp+1:kp+1).EQ.'!' ) THEN
C Echo all comment lines (if chatter on).
         IF ( Ichat.GT.10 .AND. lbuf.GT.0) WRITE(*,221) cbuf(:lbuf )
  221    FORMAT(1X,A)
         GOTO 200
      END IF
      ctmp = cbuf(kp+1:kp+1)
      CALL ALF(cbuf,lbuf,kp,ctok,ltok)
C Special treatment for a single number and a continuation mark as
C the trailing minus would causes ISNUM to return not a number.
      IF ( ctok(ltok:ltok).EQ.'-' ) THEN
         itmp=ltok-1
      ELSE
         itmp=ltok
      END IF
      IF ( ISNUM(ctok,ltok).NE.0 .AND. ctmp.NE.'"' ) GOTO 300
      CALL UPC(ctok)
      IF ( ctok(1:4).EQ.'READ' ) THEN
         CALL ALF(cbuf,lbuf,kp,ctok,ltok)
         CALL UPC(ctok)
         ierf=0
         IF ( ctok(1:1).EQ.'S' ) ierf=1
         IF ( ctok(1:1).EQ.'T' ) ierf=2
         IF ( ierf.EQ.0 ) GOTO 200
C---
  230    CALL ALF(cbuf,lbuf,kp,ctok,ltok)
         IF ( ltok.LE.0 ) GOTO 200
         ix=FPNUM(ctok,ltok,Ier)
         IF ( ix.LT.0 .OR. ix.GT.Mxvec ) GOTO 230
         Iery(ix)=ierf
         GOTO 230
      ELSE
         IF ( Ncmd.LT.Mxcmd ) Ncmd=Ncmd+1
         IF ( lbuf.GT.0) Cmd(Ncmd)=cbuf(:lbuf )
      END IF
      GOTO 200
C---
C Read a data line.
  300 CONTINUE
      IF ( lbuf.LE.0 ) THEN
         itmp=0
      ELSE
         itmp=INDEX(cbuf(:lbuf),'!')
      END IF
      IF ( itmp.GT.1) lbuf=LENACT(cbuf(:itmp-1) )
      IF ( lbuf.GT.0 .AND. cbuf(lbuf:lbuf).EQ.'-' ) THEN
         icon=1
         lbuf=lbuf-1
      ELSE
         icon=0
      END IF
C If this is the first data point then calculate Nvec, Nrow, and ncol.
C Ensure that neither Y nor Iery arrays will overflow.
      IF ( Npts.EQ.0 ) THEN
C First we calculate the number of tokens on the line
         kp=0
         icol=0
  310    CONTINUE
            CALL RDCONR(lun,Ichat,icon,cbuf,lbuf,kp,ctok,ltok,value)
            icol=icol+1
            Yray(icol)=value
         IF ( kp.LT.lbuf ) GOTO 310
C
C Next we match tokens to vectors.  This is done here because user
C may not have forgotten to include an error on a vector in which
C an error is expected from an 'READ xERR' command.
         ncol=0
  330    CONTINUE
            Nvec=Nvec+1
            ncol=ncol+1
            IF ( Iery(Nvec).EQ.1 ) THEN
C Skip one column for symmetric errors
               ncol=ncol+1
            ELSE IF ( Iery(Nvec).EQ.2 ) THEN
C or two columns for two-sided errors
               ncol=ncol+2
            END IF
         IF ( ncol.LT.icol .AND. Nvec.LT.Mxvec ) GOTO 330
C
         Nrow=Mxpts/ncol
         IF ( Ichat.GT.0 ) THEN
            WRITE(*,351) ncol, Nvec, Nrow
  351       FORMAT(/,' Reading',I5,' columns,',I5,' vectors.',
     :         '  Maximum number of rows is',I8,'.',/)
         END IF
C
C We can now move the first row to the proper location in the Y array.
C This is done from the top down to avoid the possibility of trashing
C a number before moving it.
         IF ( ncol.GT.1 ) THEN
            itmp=Nrow*(ncol-1)+1
            DO i=ncol,2,-1
               Yray(itmp)=Yray(i)
               Yray(i)=0.
               itmp=itmp-Nrow
            END DO
         END IF
         Npts=1
      ELSE
C
         IF ( Npts.GE.Nrow ) THEN
            WRITE(*,*) 'RDQDP--Too many lines in file.'
            GOTO 800
         END IF
         kp=0
         Npts=Npts+1
         itmp=Npts
         DO icol=1, ncol
            CALL RDCONR(lun,Ichat,icon,cbuf,lbuf,kp,ctok,ltok,value)
            Yray(itmp)=value
            itmp=itmp+Nrow
         END DO
      END IF
      GOTO 200
C---
C No error
  800 CONTINUE
      Ier=0
      GOTO 950
C---
C Error
  900 CONTINUE
      Ier=-1
C Common exit
  950 CONTINUE
      IF ( lun.GT.0 ) THEN
         CLOSE(UNIT=lun)
         CALL FRElun(lun)
      END IF
      RETURN
      END
C*********
      SUBROUTINE RDCONR(Lun,Ichat,Icon,Cbuf,Lbuf,Kp,Ctok,Ltok,Value)
      CHARACTER Cbuf*(*), Ctok*(*)
      INTEGER   Lun, Ichat, Icon, Lbuf, Kp, Ltok
      REAL      Value
C---
C Read with continuation a real number.
C---
C Lun     I    The open unit being read
C Ichat   I    =-1 to hdecod lines as they are read
C Icon    I/O
C Cbuf    I/O
C Lbuf    I/O
C Kp      I/O
C Ctok    I/O
C Ltok    I/O
C Value     O
C---
      REAL      FPNUM
      INTEGER   LENACT
C
      INTEGER   ier, itmp
C---
   11 FORMAT(A)
C---
      CALL ALF(Cbuf, Lbuf, Kp, Ctok, Ltok)
      Value = FPNUM(Ctok, Ltok, ier)
C
  100 CONTINUE
      IF ( Kp.GE.Lbuf .AND. Icon.GT.0 ) THEN
         READ(Lun,11,END=800) Cbuf
         Lbuf=LENACT(Cbuf)
         IF ( ichat.EQ.-1 ) CALL HDECOD(Cbuf, Lbuf)
         IF ( Lbuf.LE.0 ) THEN
            itmp=0
         ELSE
            itmp=INDEX(Cbuf(:Lbuf),'!')
         END IF
         IF ( itmp.GT.1) Lbuf=LENACT(Cbuf(:itmp-1) )
         IF ( Lbuf.GT.0 .AND. Cbuf(Lbuf:Lbuf).EQ.'-' ) THEN
            Icon=1
            Lbuf=Lbuf-1
         ELSE
            Icon=0
         END IF
         Kp=0
         CALL ALFSKS(Cbuf, Lbuf, Kp)
         IF ( Kp.GE.Lbuf ) GOTO 100
      END IF
C---
  800 RETURN
      END
