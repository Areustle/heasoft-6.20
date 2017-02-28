**==gtbuf.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE GTBUF(Cprom,Ier)
      CHARACTER Cprom*(*)
      INTEGER Ier
C     Entry GTCHAR, GTPEEK, GTREST
      CHARACTER Ctoken*(*)
      INTEGER Ltoken
C     Entry GTDBLE
      DOUBLE PRECISION Dnum
C     Entry GTINT
      INTEGER Inum
C     Entry GTLEV
      INTEGER Lev
C     Entry GTMODE
      INTEGER Inmode
C     Entry GTREAL
      REAL Rnum
C     Entry LDBUF
      CHARACTER Cmd(*)*(*)
      INTEGER Ncmd , Ierld
C     Entry LDBUF1
      CHARACTER Cmd1*(*)
C     Entry STWARN
      INTEGER Ldwarn
C     Entry XGETIN
      CHARACTER Cext1*(*)
C     Entry XSETIN
      CHARACTER Cext2*(*)
C     entry gtbufsavecmdline
      CHARACTER Cmdline*(*)
C     entry gtbufinlen
      INTEGER Inlen
C---
C     When first called this routine tries to read a string from the
C     command line.  If no string was given or on future calls, this
C     routine will prompt the user for input.
C---
C     Special first characters (must be first character on line)
C     !     Ignore the entire line (comment line)
C     $     Spawn to VMS
C     @file Open next level of indirect file and read buffer from there.
C     @     Force buffer to be read from terminal (even when reading a file).
C     Special character can appear anywhere in line.
C     !     (not enclosed in ") means rest of line is a comment.
C---
C     CPROM     I    Used to prompt the user.
C     IER         O  =0 no error, =-1 if user entered an EOF.
C---
C     Requires:         for:
C     ALF.FOR           ALF, FPNUM, ISNUM, ALFSKS
C     LENACT.FOR        LENACT
C     LOGGER.FOR        LOGGER
C     SCRIPT.FOR        SCRIPT
C     XTEND.FOR         XTEND
C     XWRCMD            XWRCMD
C     .XANLIB]SYS.xxx   RDFORN, SPAWN, PROMPT
C---
C     1991-Feb-11 - Add LDBUF1 entry [AFT]
C     1990-Dec-04 - Remove GTPROM, add STWARN [AFT]
C     1990-Feb-02 - Added parameters to indirect files [AFT]
C     1989-Dec-13 - Added command recall [AFT]
C     1989-Jul-06 - System independent indirect command files [AFT]
C---
C     Implementation of indirect file parameters:  MXARGS determines the
C     maximum total number of parameters used at all indirect levels.  The
C     total number of characters in all parameter strings is determined
C     by LEN(CARGS).  Since some levels may use no parameters and others
C     many, a two level pointer system is used.  ISTACK(1,LEVEL) determines
C     the offset into IPOS when reading commands at depth of LEVEL.
C     ISTACK(2,LEVEL) is the number of valid parameters at that level.
C     Let IOFF=ISTACK(1,LEVEL) then IPOS(1,IOFF+IPAR) gives the start
C     position in CARGS for parameter number IPAR.  IPOS(2,IOFF+IPAR) is
C     the end position.
C---
C
      INTEGER MXLEV
      PARAMETER (MXLEV=15)
      INTEGER MXPREV
      PARAMETER (MXPREV=20)
      INTEGER MXARGS
      PARAMETER (MXARGS=32)
      REAL FPNUM
      INTEGER LENACT , ISNUM
C
      CHARACTER(1000) cprev(MXPREV)
      CHARACTER(500) cargs
      SAVE cprev , cargs
      CHARACTER(1000) cbuf, cload
      SAVE cbuf , cload
      CHARACTER(1000) cnam, ctmp, ctmp1
      CHARACTER(32) ctok
      CHARACTER(10) cdef
      SAVE cdef
      REAL rbuf(2)
      REAL tmp
      INTEGER lunsav(MXLEV) , istack(2,0:MXLEV) , ipos(2,0:MXARGS)
      SAVE lunsav , istack , ipos
      INTEGER i , ibas , ic , ie , ifnb
      INTEGER ios , ipar , iper , iq , is , itmp , itmprd , iwrlog
      INTEGER lnam , lprom , ltok , ltmp , luntry
      INTEGER idspawn , icnt , iecho , ifirst
      SAVE idspawn , icnt , iecho , ifirst
      INTEGER iload , irdcom , istopl , itop
      SAVE iload , irdcom , istopl , itop
      INTEGER kp , lbuf , laspos , ldlev , level , mode , nargs , nload
      SAVE kp , lbuf , laspos , ldlev , level , mode , nargs , nload
      CHARACTER(4) c4tmp
C     Entry GTBUFSTAND
      LOGICAL standalone
      SAVE standalone
C
C     entry GTBUFSAVECMDLINE
      character(1000) savedcmdline
      LOGICAL savedcmd
      SAVE savedcmd
      SAVE savedcmdline
      LOGICAL expandat
      SAVE expandat
 
C     entry GTBUFINLEN
      INTEGER savedlbuf
      SAVE savedlbuf
 
 
      DATA cdef/'XCO'/
      DATA idspawn/0/ , iecho/1/ , ifirst/1/
      DATA iload/0/ , irdcom/1/ , istopl/ - 1/ , itop/1/
      DATA ldlev/0/ , level/0/
      DATA mode/0/ , nload/0/
      DATA standalone/.FALSE./
      DATA savedcmd/.FALSE./
      DATA expandat/.FALSE./
      DATA savedlbuf/0/
C---
C-    First time through, search for foreign arguments.
      lprom = LENACT(Cprom)
      IF ( ifirst.NE.0 ) THEN
C     Fill recall buffer with blanks, ignoring any commands the user
C     has previously loaded.
         DO 50 i = nload + 1 , MXPREV
            cprev(i) = ' '
 50      CONTINUE
C     Start at level zero
         nargs = 0
C     No arguments at any level
         DO 100 i = 0 , MXLEV
            istack(1,i) = 0
            istack(2,i) = 0
 100     CONTINUE
C     No character data in argument stack
         ipos(1,0) = 0
         ipos(2,0) = 0
         laspos = ipos(2,0)
C     User has not entered any commands (yet)
         icnt = 0
C     Archive centers like to disable people
         CALL TRLOG('SPAWN_DISABLE',13,cbuf,lbuf)
         IF ( lbuf.GT.0 ) idspawn = 1
C     Has user setup a private command line editing string?
C         CALL TRLOG('GTBUF_EDIT',10,cbuf,lbuf)
C         CALL EDICOM(cbuf,lbuf)
      ENDIF
      itmprd = 0

      DO WHILE ( .TRUE. )
C---
         ios = 0
         Ier = 0
         kp = 0

         IF ( ldlev.GT.0 ) THEN
            IF ( level.LT.ldlev ) ldlev = 0
            IF ( level.LE.istopl ) THEN
C     Stop flag is set and we have returned to the original level.  Stop
C     and report condition to user.
               istopl = -1
               Ier = 1
               GOTO 400
            ENDIF
         ENDIF
C---
C     This is the code to do the primary read.
         iwrlog = 0
         IF ( level.LE.0 ) THEN
C     Read an interactive response
            IF ( irdcom.NE.0 ) THEN
C     First time try the command line
               irdcom = 0
               IF ( savedcmd ) THEN
                  cbuf = savedcmdline
                  lbuf = LENACT(savedcmdline)
                  IF ( lbuf.GT.0 ) expandat = .TRUE.
               ELSE
                  CALL RDFORN(cbuf,lbuf)
                  IF ( lbuf.GT.0 ) expandat = .TRUE.
               ENDIF
               IF ( lbuf.GT.0 ) THEN
                  ios = 0
                  GOTO 120
C     If nothing on the command line, and there is no prompt, then exit quietly.
C     (This allows the main program to flush the command line read.)
               ELSEIF ( lprom.EQ.0 ) THEN
                  GOTO 400
               ENDIF
            ENDIF
C     User must actually type something
C            IF ( ICEdit.NE.0 ) THEN
C     either with the super-duper single character IO
C Now the gnu readline
C               CALL QLINE(Cprom,lprom,cprev,icnt,itop,MXPREV,cbuf,lbuf,
C     &                    ios)
            CALL FREADLINE(Cprom,lprom,cbuf,lbuf,ios)
            savedlbuf = lbuf
C            ELSE
C     or with standard Fortran IO.
C               CALL PROMPT(Cprom,0)
C               READ (*,'(A)',IOSTAT=ios) cbuf
C               lbuf = LENACT(cbuf)
C            ENDIF
C     Write LOG file
 120        ctmp = Cprom(:lprom) // ' ' // cbuf(:lbuf)
            ltmp = 0
            CALL LOGGER(5,rbuf,0,ctmp,ltmp)
C---
C     Load the command recall stack.  (Saving commands with load flag set
C     could mess up the counting.)
            IF ( lbuf.GT.0 .AND. nload.LE.0 ) THEN
C     Only push non-blank commands onto the stack
               icnt = icnt + 1
               itop = MOD(itop,MXPREV) + 1
               cprev(itop) = cbuf
            ENDIF
         ELSEIF ( lunsav(level).GT.0 ) THEN
C     Read from indirect command file
            READ (lunsav(level),'(A)',IOSTAT=ios) cbuf
            lbuf = LENACT(cbuf)
            IF ( ios.EQ.0 .AND. iecho.NE.0 .AND. ldlev.EQ.0 ) iwrlog = 1
         ELSEIF ( lunsav(level).LT.0 ) THEN
C     Read the LDBUF1 command.
            level = level - 1
            cbuf = cload
            lbuf = LENACT(cbuf)
            iload = 0
         ELSE
C     Read a LDBUF command.
            IF ( itop.EQ.nload ) THEN
               level = level - 1
C     If no other levels load commands, then turn off NLOAD flag.
               itmp = nload
               nload = 0
               DO 130 i = 1 , level
                  IF ( lunsav(i).EQ.0 ) nload = itmp
 130           CONTINUE
               GOTO 300
            ENDIF
            icnt = icnt + 1
            itop = MOD(itop,MXPREV) + 1
            cbuf = cprev(itop)
            lbuf = LENACT(cbuf)
         ENDIF
C---
C-    Use /* to denote EOF
         IF ( cbuf(1:2).EQ.'/*' ) ios = -1
         IF ( ios.NE.0 ) THEN
            lbuf = 0
            IF ( level.GT.0 ) THEN
               IF ( lunsav(level).GT.0 ) THEN
                  CLOSE (UNIT=lunsav(level))
                  CALL FRELUN(lunsav(level))
                  nargs = nargs - istack(2,level)
                  laspos = ipos(2,nargs)
               ENDIF
               istack(2,level) = 0
               level = level - 1
               GOTO 300
            ELSE
               Ier = -1
               IF ( level.LT.0 ) THEN
C     User has entered an EOF and hence is unable to correct an error that
C     occurred while reading an indirect file.  Log this response and
C     unwind the indirect file stack.
                  CALL XWRCMD(level,Cprom,lprom,'/*',2)
                  level = ABS(level)
                  itmp = level
                  DO 135 i = 1 , itmp
                     IF ( lunsav(level).GT.0 ) THEN
                        CLOSE (UNIT=lunsav(level))
                        CALL FRELUN(lunsav(level))
                        nargs = nargs - istack(2,level)
                     ENDIF
                     istack(2,level) = 0
                     level = level - 1
 135              CONTINUE
                  laspos = ipos(2,nargs)
               ENDIF
            ENDIF
         ELSE
C---
C     If the first character is '!' then ignore the complete line.
            IF ( mode.EQ.0 .AND. cbuf(1:1).EQ.'!' .AND. 
     &           .NOT.standalone ) GOTO 300
C Otherwise, strip off comments, this is done by scaning the input
C line looking for a ! not enclosed in ".
 140        iq = 0
            iper = 0
            ic = lbuf
            DO 160 i = 1 , ic
               IF ( iq.NE.0 ) THEN
C- Odd number of " marks.
                  IF ( cbuf(i:i).EQ.'"' ) iq = 0
C- Even number of " marks.
               ELSEIF ( cbuf(i:i).EQ.'"' ) THEN
                  iq = 1
               ELSEIF ( cbuf(i:i).EQ.'!' .AND. .NOT.standalone ) THEN
                  lbuf = i - 1
                  lbuf = LENACT(cbuf(:lbuf))
                  GOTO 180
               ELSEIF ( cbuf(i:i).EQ.'%' ) THEN
                  IF ( iper.EQ.0 ) iper = i
               ENDIF
 160        CONTINUE
C---
C Got an GTBUF command, deal with it
 180        IF ( iper.NE.0 ) THEN
               ltok = INDEX(cbuf(iper+1:),'%') + 1
               IF ( ltok.LE.0 ) GOTO 200
               ctok = cbuf(iper:iper+ltok-1)
               kp = iper + ltok - 1
               CALL UPC(ctok)
C %EDit% for command line editing
C                  CALL EDICOM(cbuf(kp+1:),lbuf-kp)
               IF ( ctok(1:3).EQ.'%ED' ) GOTO 300
               IF ( ctok(1:2).EQ.'%E' ) THEN
C %Echo% to control the printing of command read from indirect files.
                  CALL ALF(cbuf,lbuf,kp,ctok,ltok)
                  IF ( ltok.GT.0 ) THEN
                     CALL UPC(ctok)
                     IF ( ctok(1:2).EQ.'OF' ) THEN
                        iecho = 0
                     ELSE
                        iecho = 1
                     ENDIF
                  ENDIF
                  GOTO 300
               ELSEIF ( ctok(1:2).EQ.'%L' ) THEN
C %Log%
                  CALL ALF(cbuf,lbuf,kp,ctok,ltok)
                  ctmp = ctok(1:3)
                  CALL UPC(ctmp(1:3))
                  IF ( ltok.GE.3 .AND. ctmp(1:3).EQ.'%OF' ) THEN
                     CALL LOGGER(4,rbuf,itmp,'KEEP',4)
                  ELSE
                     rbuf(1) = 0.0
                     itmp = 1
                     CALL LOGGER(3,rbuf,itmp,ctok,ltok)
                  ENDIF
                  GOTO 300
               ELSEIF ( ctok(1:2).EQ.'%R' ) THEN
C %Recall%ALL
                  CALL ALF(cbuf,lbuf,kp,ctok,ltok)
                  IF ( ltok.LE.0 ) GOTO 300
                  CALL UPC(ctok)
                  IF ( ctok(1:1).EQ.'A' ) THEN
                     IF ( icnt.LE.MXPREV ) THEN
                        itmp = 0
                        ibas = 0
                     ELSE
                        itmp = itop
                        ibas = icnt - MXPREV
                     ENDIF
                     DO 182 i = 1 , MXPREV
                        itmp = itmp + 1
                        IF ( itmp.GT.MXPREV ) itmp = 1
                        ibas = ibas + 1
                        WRITE (*,99002) ibas , cprev(itmp)
     &                                  (:LENACT(cprev(itmp)))
                        IF ( itmp.EQ.itop ) GOTO 300
 182                 CONTINUE
                     GOTO 300
                  ENDIF
               ELSEIF ( ctok(1:2).EQ.'%S' ) THEN
C %Script%
                  CALL ALF(cbuf,lbuf,kp,ctok,ltok)
                  ctmp = ctok(1:3)
                  CALL UPC(ctmp(1:3))
                  IF ( ltok.GE.3 .AND. ctmp(1:3).EQ.'%OF' ) THEN
                     CALL SCRIPT(4,tmp,itmp,'KEEP',4)
                  ELSE
                     rbuf(1) = 0.0
                     itmp = 1
                     CALL SCRIPT(3,tmp,itmp,ctok,ltok)
                  ENDIF
                  GOTO 300
               ELSEIF ( ctok(1:2).EQ.'%V' ) THEN
C %Version%
                  WRITE (*,99003) 'GTBUF version: 2006-Aug-30'
                  GOTO 300
               ELSEIF ( ISNUM(ctok(2:),ltok-2).NE.0 ) THEN
C %N% argument substitution.
                  ipar = INT(FPNUM(ctok(2:),ltok-2,itmp))
                  IF ( ipar.GT.0 .AND. ipar.LE.istack(2,level) ) THEN
                     itmp = istack(1,level) + ipar
                     is = ipos(1,itmp)
                     ie = ipos(2,itmp)
                     ctmp = cargs(is:ie)//cbuf(iper+ltok:)
                  ELSE
                     ctmp = cbuf(iper+ltok:)
                  ENDIF
                  cbuf(iper:) = ctmp
                  iper = INDEX(cbuf(iper:),'%')
                  lbuf = LENACT(cbuf)
                  IF ( iper.GT.0 ) GOTO 140
               ENDIF
            ENDIF
C---
C If not generated by a load, write response to LOG and SCRIPT files.
            IF ( nload.LE.0 ) CALL XWRCMD(level,Cprom,lprom,cbuf,lbuf)
            IF ( iwrlog.NE.0 ) THEN
C Echo response to screen and LOG file.
               ctmp = '!' // Cprom(:lprom) // ' ' // cbuf(:lbuf)
               ltmp = LENACT(ctmp)
               CALL LOGGER(5,rbuf,0,ctmp,ltmp)
               WRITE (*,99001) ctmp(:ltmp)
            ENDIF
C---
C- Check for special characters in first column.
 200        IF ( mode.EQ.0 ) THEN
               IF ( cbuf(1:1).EQ.'$' .AND. .NOT.standalone ) THEN
C $ to spawn
                  IF ( idspawn.EQ.0 ) THEN
                     lbuf = lbuf - 1
                     WRITE (*,99003) 'Spawning...'
                     c4tmp = 'SAV'
C                     CALL EDICOM(c4tmp,3)
                     CALL SPAWN(cbuf(2:),lbuf,Ier)
                     c4tmp = 'RES'
C                     CALL EDICOM(c4tmp,3)
                  ELSE
                     WRITE (*,99003) 'Spawning has been disabled.'
                  ENDIF
                  GOTO 300
               ELSEIF ( cbuf(1:1).EQ.'@' ) THEN
                  IF ( .NOT.(standalone) ) THEN
C @ open indirect command file.
                     IF ( lbuf.EQ.1 ) THEN
C (no file name means read next line from terminal)
                        level = -level
                        itmprd = 1
                        GOTO 300
                     ENDIF
                     IF ( level.GE.MXLEV ) THEN
                        WRITE (*,99005) MXLEV
                        Ier = 1
                        GOTO 400
                     ENDIF
                     kp = 1
                     CALL ALF(cbuf,lbuf,kp,cnam,lnam)
                     CALL GETLUN(luntry)
                     IF ( LENACT(cdef).GT.0 ) CALL XTEND(cnam,cdef)
C Search current directory
                     CALL OPENWR(luntry,cnam,'OLD',' ',' ',0,1,ios)
 202                 DO WHILE ( .TRUE. )
                        IF ( ios.EQ.0 ) THEN
                           level = ABS(level) + 1
                           lunsav(level) = luntry
                           istack(1,level) = istack(1,level-1)
     &                        + istack(2,level-1)
                           istack(2,level) = 0
                           IF ( kp.LT.lbuf ) THEN
                              DO WHILE ( .TRUE. )
                                 CALL ALF(cbuf,lbuf,kp,ctmp,ltmp)
                                 IF ( nargs.GE.MXARGS ) THEN
                                    WRITE (*,99006) MXARGS , ctmp(:ltmp)
                                    GOTO 204
                                 ENDIF
                                 IF ( laspos+ltmp.GT.LEN(cargs) ) THEN
                                    WRITE (*,99007) ctmp(:ltmp)
                                    GOTO 204
                                 ENDIF
                                 istack(2,level) = istack(2,level) + 1
                                 nargs = nargs + 1
                                 ipos(1,nargs) = laspos + 1
                                 ipos(2,nargs) = laspos + ltmp
                                 cargs(laspos+1:laspos+ltmp) = ctmp
                                 laspos = ipos(2,nargs)
                                 IF ( kp.GE.lbuf ) GOTO 204
                              ENDDO
                           ENDIF
C Don't start reading indirect file on first call.
 204                       IF ( ifirst.EQ.0 .OR. lprom.GT.0 ) GOTO 300
                           lbuf = 0
                           GOTO 400
                        ENDIF
C Search user defined directory (if it exists)
                        CALL TRLOG('MY_XCOMS',8,ctmp,ltmp)
                        IF ( ltmp.GT.0 ) THEN
                           ctmp(ltmp+1:) = cnam
                           CALL OPENWR(luntry,ctmp,'OLD',' ',' ',0,1,
     &                                 ios)
                           IF ( ios.EQ.0 ) GOTO 206
                        ENDIF
                        GOTO 208
 206                 CONTINUE
                  ENDDO
C Search system directory
 208                 ctmp = cnam
                     CALL PTEND('$HEADAS','/../ftools/xanlib/xcoms',ctmp
     &                                )
                     CALL OPENWR(luntry,ctmp,'OLD',' ',' ',0,1,ios)
                     IF ( ios.EQ.0 ) GOTO 202
C Nowhere to be found
                     CALL FRELUN(luntry)
                     WRITE (*,99008) cnam(:LENACT(cnam)) , ios
                     IF ( ifirst.NE.0 .AND. lprom.EQ.0 ) GOTO 400
                     GOTO 300
                  ELSEIF ( expandat ) THEN
                     expandat = .FALSE.
                     ctmp1 = cbuf(2:)
                     CALL GETLUN(luntry)
                     CALL OPENWR(luntry,ctmp1,'old',' ',' ',0,1,ios)
                     cbuf = ' '
                     READ (luntry,'(a)',IOSTAT=ios) ctmp1
                     DO WHILE ( ios.EQ.0 )
                        cbuf = cbuf(1:LENACT(cbuf))//' '//ctmp1
                        READ (luntry,'(a)',IOSTAT=ios) ctmp1
                     ENDDO
                     lbuf = LENACT(cbuf)
                     CLOSE (luntry)
                     CALL FRELUN(luntry)
                     GOTO 400
* do nothing
                  ENDIF
               ENDIF
            ENDIF
C---
C Skip leading blanks.
            CALL ALFSKS(cbuf,lbuf,kp)
            ifnb = kp
         ENDIF
         GOTO 400
 300     CONTINUE
      ENDDO
C---
C GTREST should return preceeding blanks (to make SELECT work)
C hence, we must reset KP=0.
 400  ifirst = 0
      kp = 0
      IF ( itmprd.NE.0 ) level = ABS(level)
      RETURN
C---
C*********
      ENTRY GTCHAR(Ctoken,Ltoken)
C---
C Return the next token as a character string.
C---
      CALL ALF(cbuf,lbuf,kp,Ctoken,Ltoken)
      RETURN
C*********
      ENTRY GTDBLE(Dnum,Ier)
C---
C Return the next token as a DOUBLE PRECISION number.  Note, the
C parser is not used to decode the number, hence things like NO or
C embedded arithmetic operators are not allowed.
C---
      CALL ALF(cbuf,lbuf,kp,ctok,ltok)
      IF ( ISNUM(ctok,ltok).NE.0 ) THEN
C---
C      READ(CTOK(:LTOK),911,ERR=920) DNUM
C---
         IF ( ltok.LT.LEN(ctok) ) ctok(ltok+1:) = ','
         READ (ctok,99009,ERR=500) Dnum
         Ier = 0
         RETURN
      ENDIF
C---
 500  Ier = 1
      RETURN
C*********
      ENTRY GTINT(Inum,Ier)
C---
C Return the next token as an integer number.
C---
      CALL ALF(cbuf,lbuf,kp,ctok,ltok)
      Ier = 0
      IF ( ltok.GT.0 ) THEN
         tmp = FPNUM(ctok,ltok,Ier)
         IF ( Ier.EQ.0 ) THEN
            Inum = NINT(tmp)
         ELSE
            Ier = 1
         ENDIF
      ENDIF
      RETURN
C*********
      ENTRY GTLEV(Lev)
C---
C Return the current level of indirectness.  This assumes that the
C GTBUF parser has already been called.  In order to allow PLT to
C be called by routines that do not use the GTBUF parser, set the
C 'already been called' flag now.
C---
      Lev = level
      ifirst = 0
      RETURN
C*********
      ENTRY GTMODE(Inmode)
C---
C Toggle the mode to INMODE.  Note, original mode is returned in
C INMODE.
C---
      itmp = mode
      mode = Inmode
      Inmode = itmp
      RETURN
C*********
      ENTRY GTREAL(Rnum,Ier)
C---
C Return the next token as a real number.
C---
      CALL ALF(cbuf,lbuf,kp,ctok,ltok)
      Ier = 0
      IF ( ltok.GT.0 ) THEN
         tmp = FPNUM(ctok,ltok,Ier)
         IF ( Ier.EQ.0 ) THEN
            Rnum = tmp
         ELSE
            Ier = 1
         ENDIF
      ENDIF
      RETURN
C*********
      ENTRY GTPEEK(Ctoken,Ltoken)
C---
C Peek at the next token, i.e., return the token but do not advance
C the character pointer.
C---
      CALL ALFSKS(cbuf,lbuf,kp)
      IF ( kp.GE.lbuf ) THEN
         Ctoken = ' '
         Ltoken = 0
      ELSE
         Ctoken(1:1) = cbuf(kp+1:kp+1)
         Ltoken = 1
         IF ( cbuf(kp+2:kp+2).NE.' ' .AND. cbuf(kp+2:kp+2).NE.',' ) THEN
            Ctoken(2:2) = cbuf(kp+2:kp+2)
            Ltoken = 2
         ENDIF
      ENDIF
      RETURN
C*********
      ENTRY GTREST(Ctoken,Ltoken)
C---
C Get rest of string, excluding comments.
C---
      IF ( kp.LT.lbuf ) THEN
         Ctoken = cbuf(kp+1:lbuf)
         Ltoken = MAX(lbuf-kp,0)
      ELSE
         Ctoken = ' '
         Ltoken = 0
      ENDIF
      kp = lbuf
      RETURN
C*********
      ENTRY GTSECO
C---
C Someone called XINIRD.  This will keep GTBUF from also reading the
C command line.
C---
      ifirst = 0
      RETURN
C*********
      ENTRY LDBUF(Cmd,Ncmd,Ierld)
C---
C Load commands into the queue without prompting the user.
C---
C CMD(*)  I    The commands to be loaded
C NCMD    I    The number of commands to load
C IERLD     O  =0 all commands loaded, otherwise the number of
C              -unloaded commands
C---
      IF ( level.GE.MXLEV ) THEN
         WRITE (*,99005) MXLEV
         Ierld = Ncmd
      ELSE
         IF ( nload.EQ.0 ) nload = itop
         level = level + 1
         lunsav(level) = 0
         IF ( ldlev.EQ.0 ) ldlev = level
         Ierld = 0
         DO 550 i = 1 , Ncmd
            itmp = nload + 1
            IF ( itmp.GT.MXPREV ) itmp = 1
            IF ( itmp.EQ.itop ) THEN
               Ierld = Ierld + 1
            ELSE
               nload = itmp
               cprev(nload) = Cmd(i)
            ENDIF
 550     CONTINUE
      ENDIF
      RETURN
C*********
      ENTRY LDBUF1(Cmd1,Ierld)
C---
C Load a single command without touching the command recall stack.
C---
C CMD1    I
C IERLD     O
C---
      IF ( level.GE.MXLEV ) THEN
         WRITE (*,99005) MXLEV
         Ierld = 1
      ELSEIF ( iload.NE.0 ) THEN
         WRITE (*,99010)
      ELSE
C Push command stack
         level = level + 1
C Flag to read from the CLOAD array
         lunsav(level) = -1
C Save command
         cload = Cmd1
C Note that CLOAD is in use
         iload = 1
C Note the current stack level (so caller can be notified when command
C is completed
         IF ( ldlev.EQ.0 ) ldlev = level
C No (detected) errors
         Ierld = 0
      ENDIF
      RETURN
C*********
      ENTRY STWARN(Ldwarn)
C---
C This entry point should be used just before LDBUF loads any commands.
C If LDWARN>0 then will return an error when it is unable to read from
C the LDBUF command array, that has been loaded after STWARN has been
C called.  Using LDWARN=0 returns GTBUF to its normal state where it
C will automatically prompt the terminal for input.
C---
C LDWARN  I    Warning flag value
C---
      IF ( Ldwarn.GT.0 ) THEN
         istopl = level
      ELSE
         istopl = -1
      ENDIF
      RETURN
C*********
      ENTRY XGETIN(Cext1)
C---
C Return the default extension for indirect files.
C---
      Cext1 = cdef
      RETURN
C*********
      ENTRY XNEWCM
C---
C This routine should be used when the software wishes to force
C the next few lines to be read from the terminal.
C---
      level = -ABS(level)
      RETURN
C*********
      ENTRY XRMVCM
C---
C Return control to the current indirect file (if it exists).
C---
      level = ABS(level)
      RETURN
C*********
      ENTRY XSETIN(Cext2)
C---
C Set the default extension for indirect files.
C---
      cdef = Cext2
      RETURN
 
      ENTRY GTBUFSTAND
C---
C Sets standalone mode
C---
      standalone = .TRUE.
      RETURN
 

 
      ENTRY GTBUFNOTSTAND
C---
C Sets standalone mode
C---
      standalone = .FALSE.
      RETURN
 
      ENTRY GTBUFEXPANDAT
C---
C Turns on expandaing @
C---
      expandat = .TRUE.
      RETURN
 

      ENTRY GTBUFNOEXPANDAT
C---
C Turns off expanding @
C---
      expandat = .FALSE.
      RETURN
 

      ENTRY GTBUFSTOPSCRIPT
C---
C Stops scripts
C---
 
      DO 600 i = 1 , level
         IF ( lunsav(level).GT.0 ) THEN
            CLOSE (UNIT=lunsav(level))
            CALL FRELUN(lunsav(level))
            nargs = nargs - istack(2,level)
         ENDIF
         istack(2,level) = 0
         level = level - 1
 600  CONTINUE
      RETURN
 
      ENTRY GTBUFSETCMDLINE(Cmdline)
 
      savedcmdline = Cmdline
      savedcmd = .TRUE.
      RETURN
 
      ENTRY GTBUFINLEN(Inlen)
      Inlen = savedlbuf
      RETURN
 

      ENTRY GTDEST()
C---
C "Destroys" the GTBUF object.  This is useful if you are going to use
C some non-GTBUF IO or want to super cleanly exit a program.
C---
      level = ABS(level)
      itmp = level
      DO i = 1, itmp
         IF ( lunsav(level).GT.0 ) THEN
            CLOSE (UNIT=lunsav(level))
            CALL FRELUN(lunsav(level))
            nargs = nargs - istack(2,level)
         ENDIF
         istack(2,level) = 0
         level = level - 1
      END DO
      laspos = ipos(2,nargs)
      RETURN
C*********


 
 
C---
99001 FORMAT (A)
99002 FORMAT (1X,I5,': ',A)
99003 FORMAT (1X,A)
99005 FORMAT (1X,'GTBUF--Too many indirect levels.  ','Max depth is',I6,
     &        '.')
99006 FORMAT (' GTBUF--Maximum number of arguments is',I5,
     &        '.'/' GTBUF--No room for "',A,'".')
99007 FORMAT (' GTBUF--Arguments too long.  No room for "',A,'".')
99008 FORMAT (' GTBUF--Unable to open ',A,', IOS=',I5)
99009 FORMAT (D20.0)
99010 FORMAT ('LDBUF1--A command has already been loaded.')
      END
