      SUBROUTINE QLINE (CPROM, LPROM, CPREV, Icnt,
     :         MXPREV, CBUF, LBUF, IOS)
      INTEGER   LPROM, Icnt, MXPREV, LBUF, IOS
      CHARACTER CPROM*(*), CPREV(MXPREV)*(*), CBUF*(*)
C---
C This routine reads the terminal stream and interprets the special
C control characters used for command line editing.
C---
C Cprom   I
C Lprom   I
C Cprev   I/O
C Icnt    I    Total number of commands that have been stored so far.
C Mxprev  I
C Cbuf      O
C Lbuf      O
C Ios       O
C---
      INCLUDE   'edicmn.inc'
      INTEGER   IBELL
      PARAMETER (IBELL =  7)
      INTEGER   IBS
      PARAMETER (IBS   =  8)
      INTEGER   ILF
      PARAMETER (ILF   = 10)
      INTEGER   ICR
      PARAMETER (ICR   = 13)
      INTEGER   IESC
      PARAMETER (IESC  = 27)
      INTEGER   IDEL
      PARAMETER (IDEL  =127)
C
C The following contain the ANSI escape sequences to clear the current
C line and to move the cursor right.
C
      character(2) CLLINE
      PARAMETER (CLLINE = '[K')
      character(3) CURRIG
      PARAMETER (CURRIG = '[1C')
C
      CHARACTER CGET*1
      INTEGER   LENACT
C
      CHARACTER CHR*1
      INTEGER   I, IPOS, IPT, ITMP, LPREV, NTOP
C
C Ntop points to where the new command will be written.
      NTOP = MOD(Icnt, MXPREV) + 1
      IPT = NTOP
      CPREV(IPT) = ' '
      LPREV=LEN(CPREV(1))
      CBUF = ' '
      LBUF = 0
      IPOS = 0
C
      IF(IFTYPE.LT.0) CALL PUTSTR(CHAR(ICR)//CHAR(ILF), 2)
      CALL PUTSTR (CPROM,LPROM)
      IF(LPROM.GT.0) CALL PUTSTR (' ', 1)
      CALL FLUSH(6)
C
  150 IF(CGET(CHR).NE.CHAR(ICR)) THEN
C
         IF(CHR.EQ.CHAR(IEOF)) THEN
C EOF character
            IOS=-1
            LBUF =0
            GOTO 910
C
         ELSE IF(CHR.EQ.CHAR(IUP)) THEN
C Recall the previous command
            ITMP= IPT - 1
            IF(ITMP.LE.0 .AND. Icnt.GE.MXPREV) ITMP = MXPREV
            IF(ITMP.EQ.NTOP .OR. ITMP.LE.0) THEN
C Beep so user knows there are no more commands that can be recalled.
               CALL PUTSTR(CHAR(IBELL), 1)
            ELSE
               IPT=ITMP
               CBUF = CPREV (IPT)
               LBUF = LENACT(CBUF)
               IPOS = LBUF
               CALL PUTSTR (CHAR(ICR)//CHAR(IESC)//CLLINE,2+LEN(CLLINE))
               CALL PUTSTR (CPROM, LPROM)
               CALL PUTSTR (' ', 1)
               CALL PUTSTR (CBUF, LBUF)
            END IF
            CALL FLUSH(6)
C
         ELSE IF(CHR.EQ.CHAR(IDOWN)) THEN
C Go to next command
            IF(IPT.EQ.NTOP) THEN
C Don't jump to the end of the list from the current command
               CALL PUTSTR(CHAR(IBELL), 1)
            ELSE
               IPT= IPT + 1
               IF(IPT.GT.MXPREV) IPT = 1
               CBUF = CPREV (IPT)
               IPOS = LENACT(CBUF)
               LBUF = IPOS
               CALL PUTSTR (CHAR(ICR)//CHAR(IESC)//CLLINE,2+LEN(CLLINE))
               CALL PUTSTR (CPROM, LPROM)
               CALL PUTSTR (' ', 1)
               CALL PUTSTR (CBUF, IPOS)
            END IF
            CALL FLUSH(6)
C
         ELSE IF(CHR.EQ.CHAR(ILEFT)) THEN
C Move left one character
            IF(IPOS.GT.0) THEN
               IPOS = IPOS - 1
               CALL PUTSTR (CHAR(IBS), 1)
               CALL FLUSH(6)
            ENDIF
C
         ELSE IF(CHR.EQ.CHAR(IRIGHT) ) THEN
C Move right one character
            IF(IPOS.LT.LBUF) THEN
               IPOS = IPOS + 1
               CALL PUTSTR (CHAR(IESC)//CURRIG,1+LEN(CURRIG))
               CALL FLUSH(6)
            ENDIF
C
         ELSE IF(CHR.EQ.CHAR(IDEL)) THEN
C Delete one character
            IF(IPOS.GT.0) THEN
               IPOS = IPOS -1
C               CBUF(IPOS+1:LBUF-1) = CBUF(IPOS+2:LBUF)
               DO 190 I=IPOS+1,LBUF-1
                  CBUF(I:I)=CBUF(I+1:I+1)
  190          CONTINUE
               CBUF(LBUF:LBUF)=' '
               LBUF = LBUF -1
               CALL PUTSTR (CHAR(IBS), 1)
               CALL PUTSTR (CBUF(IPOS+1:),LBUF-IPOS+1)
               DO 210 I=1,LBUF-IPOS+1
                  CALL PUTSTR(CHAR(IBS), 1)
  210          CONTINUE
               CALL FLUSH(6)
            ENDIF
C
         ELSE IF( CHR.EQ.CHAR(IBEG)) THEN
C Move to beginning of line
            DO 220 I=1, IPOS
               CALL PUTSTR (CHAR(IBS), 1)
  220       CONTINUE
            IPOS=0
            CALL FLUSH(6)
C
         ELSE IF( CHR.EQ.CHAR(IEND)) THEN
C Move to end of line
            DO 230 I=IPOS+1, LBUF
               CALL PUTSTR (CHAR(IESC)//CURRIG,1+LEN(CURRIG))
  230       CONTINUE
            IPOS=LBUF
            CALL FLUSH(6)
C
         ELSE IF(CHR.EQ.CHAR(IERASE)) THEN
C Erase entire line
            LBUF = 0
            IPOS = 0
            CBUF = ' '
            CALL PUTSTR (CHAR(ICR)//CHAR(IESC)//CLLINE, 2+LEN(CLLINE))
            CALL PUTSTR (CPROM, LPROM)
            CALL PUTSTR (' ', 1)
            CALL FLUSH(6)
C
         ELSE IF( CHR.EQ.CHAR(IWRITE)) THEN
C Rewrite entire line
            CALL PUTSTR(CHAR(ICR)//CHAR(ILF), 2)
            CALL PUTSTR (CPROM,LPROM)
            CALL PUTSTR (' ', 1)
            CALL PUTSTR (CBUF,LBUF)
            CALL FLUSH(6)
            IPOS=LBUF
C
         ELSE
C Add character to buffer
            IF(LBUF.EQ.0) THEN
C First character in buffer
               CBUF(1:1) = CHR
               LBUF = 1
               IPOS = 1
               CALL PUTSTR(CHR, 1)
            ELSE IF( LBUF.GE.LPREV ) THEN
C Buffer is at maximum possible length, be careful
               IF( IPOS.EQ.1 ) THEN
                  CBUF = CHR//CBUF(1:LPREV-1)
                  IPOS = IPOS +1
               ELSE IF( IPOS.GE.LPREV ) THEN
                  CBUF(LPREV:LPREV)= CHR
                  CALL PUTSTR(CHAR(IBS), 1)
               ELSE
                  CBUF = CBUF(1:IPOS-1)//CHR//CBUF(IPOS:LPREV-1)
                  IPOS = IPOS +1
               ENDIF
               CALL PUTSTR(CHR, 1)
            ELSE
C Character is to be inserted
               IPOS = IPOS + 1
               LBUF = LBUF + 1
               IF(IPOS.EQ.LBUF) THEN
                  CBUF(LBUF:LBUF) = CHR
                  CALL PUTSTR(CHR,1)
               ELSE
                  DO 240 I=LBUF,IPOS+1,-1
                     CBUF(I:I)=CBUF(I-1:I-1)
  240             CONTINUE
                  CBUF(IPOS:IPOS) = CHR
                  CALL PUTSTR(CBUF(IPOS:LBUF), LBUF-IPOS+1)
                  DO 250 I=1, LBUF-IPOS
                     CALL PUTSTR (CHAR(IBS), 1)
  250             CONTINUE
               END IF
            ENDIF
            CALL FLUSH(6)
         ENDIF
         GOTO 150
      END IF
C
      IOS=0
C
  910 CALL PUTSTR(CHAR(ICR), 1)
      IF(IFTYPE.GT.0) CALL PUTSTR(CHAR(ILF),1)
      CALL FLUSH(6)
      RETURN
      END
