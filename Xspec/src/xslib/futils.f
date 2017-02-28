C- Contains entry points for
C ALF
C ALFSKS
C FPNUM
C ISNUM
C IRANGE
C---
      SUBROUTINE ALF(Cbuf,Lbuf,Kp,Ctok,Ltok)
      CHARACTER Cbuf*(*), Ctok*(*)
      INTEGER   Lbuf, Kp, Ltok
C---
C Strip preceeding spaces, copy token from CBUF to CTOK, and
C then leave KP pointing one character before the start of the
C next token.  NOTE: if LTOK=0 then CTOK is not modified.
C---
C Cbuf      I    Character array to be parsed
C Lbuf      I    The last valid character in CBUF
C Kp+1      I/O  Points at next character to be parsed
C Ctok        O  The parsed token
C Ltok        O  Number of valid characters in CTOK
C---
C [AFT]
C---
      INTEGER ITAB
      PARAMETER (ITAB=9)
      CHARACTER chr
      INTEGER   iquote
C---
      Ltok = 0
      CALL ALFSKS(Cbuf,Lbuf,Kp)
      IF ( Kp.GE.Lbuf ) GOTO 900
      Kp = Kp + 1
      iquote = 0
      IF ( Cbuf(Kp:Kp).EQ.'"' ) THEN
         iquote = 1
         IF ( Cbuf(Kp+1:Kp+1).EQ.'"' ) THEN
            Kp = Kp + 2
            Ltok = Ltok + 1
            Ctok(Ltok:Ltok) = '"'
         ELSE
            Kp = Kp + 1
         END IF
      END IF
C---
 150  chr = Cbuf(Kp:Kp)
      IF ( iquote.EQ.0 ) THEN
         IF ( chr.EQ.' ' ) GOTO 200
         IF ( chr.EQ.CHAR(ITAB) ) GOTO 200
         IF ( chr.EQ.',' ) GOTO 900
      ELSE
         IF ( chr.EQ.'"' ) THEN
            IF ( Cbuf(Kp+1:Kp+1).NE.'"' ) GOTO 200
            Kp = Kp + 1
         END IF
      END IF
      IF ( Ltok.LT.LEN(Ctok) ) THEN
         Ltok = Ltok + 1
         Ctok(Ltok:Ltok) = chr
      END IF
      IF ( Kp.GE.Lbuf ) GOTO 200
      Kp = Kp + 1
      GOTO 150
C---
C- Strip trailing spaces and tabs and up to one comma.
 200  CALL ALFSKS(Cbuf,Lbuf,Kp)
      IF ( Kp.LT.Lbuf .AND. Cbuf(Kp+1:Kp+1).EQ.',' ) Kp = Kp + 1
C---
 900  IF ( Ltok.LT.LEN(Ctok) ) Ctok(Ltok+1:) = ' '
      RETURN
      END
C*********
      SUBROUTINE ALFSKS(Cbuf,Lbuf,Kp)
      CHARACTER Cbuf*(*)
      INTEGER   Lbuf, Kp
C---
C Skip spaces and tabs.  Upon return either CBUF(KP+1:KP+1).NE.' '
C or KP.GE.LBUF
C---
C CBUF    I
C LBUF    I
C KP      I/O
C---
C [AFT]
C---
      INTEGER ITAB
      PARAMETER (ITAB=9)
      CHARACTER chr
C---
 100  IF ( Kp.GE.Lbuf ) RETURN
      chr = Cbuf(Kp+1:Kp+1)
      IF ( chr.EQ.' ' .OR. chr.EQ.CHAR(ITAB) ) THEN
         Kp = Kp + 1
         GOTO 100
      END IF
      RETURN
      END
C*********
      REAL FUNCTION FPNUM(Ctok,Ltok,Ier)
      CHARACTER Ctok*(*)
      INTEGER Ltok, Ier
C---
C Decodes number found starting at CTOK(1:1).  Anything which cannot
C be read as part of a number terminates the decoding.  For maximum
C portability, the string is copied to an internal buffer checking that
C the string forms a valid number.  Then the internal buffer is read
C via an internal Fortran READ.
C---
C Ctok    I    The character buffer to be decoded.
C Ltok    I    The number of valid characters in Cbuf.
C Ier       O  =1 if an error occurs, =0 otherwise.
C---
C [AFT]
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXLEV
      PARAMETER (MXLEV=5)
C
      CHARACTER(48) cdum
      CHARACTER(2) cno
      CHARACTER chr
      REAL      stack(MXLEV)
      REAL      factor
      INTEGER   iop(MXLEV), ipri(0:5)
      SAVE      ipri
      INTEGER   ios, kp, ldum, level
      DATA  ipri/10000,3,3,2,2,1/
      DATA  cdum/' '/
C---
C- Check for NO-DATA flag
      IF ( Ltok.EQ.2 ) THEN
         cno = Ctok(1:2)
         CALL UPC(cno)
         IF ( cno.EQ.'NO' ) THEN
            FPNUM = NO
            Ier = 0
            RETURN
         END IF
      END IF
C---
      FPNUM = 0.
      level = 0
      kp = 1
      Ier = 1
C---
C If number starts with ( then push this onto the operator stack.
 120  CONTINUE
      IF ( Ctok(kp:kp).EQ.'(' ) THEN
         IF ( level.EQ.0 ) THEN
            level = 1
            iop(level) = 0
         END IF
         iop(level) = iop(level) + 100
         kp = kp + 1
         GOTO 120
      END IF
      factor = 0.0
C At the end we need a 24 character string that is *right* justified
C with leading blanks.  We ensure this by leaving room for 23 blanks
C at the front i.e., the number is in cbuf(ldum-23:ldum).
      ldum = 23
C---
C Initialize cdum string with the sign if one exists. Note, a sign
C (+/-) with no trailing digits, is illegal.
      IF ( Ctok(kp:kp).EQ.'-' .OR. Ctok(kp:kp).EQ.'+' ) THEN
         IF ( kp.GE.Ltok ) GOTO 900
         ldum = ldum + 1
         cdum(ldum:ldum) = Ctok(kp:kp)
         kp = kp + 1
      END IF
C---
C Deal with digits left of decimal point.
 130  IF ( Ctok(kp:kp).GE.'0' .AND. Ctok(kp:kp).LE.'9' ) THEN
C If there is enough room for exponent, then
         IF ( ldum.LT.LEN(cdum)-5 ) THEN
C copy digit
            ldum = ldum + 1
            cdum(ldum:ldum) = Ctok(kp:kp)
         ELSE
C otherwise we ill need to apply a correct factor.
            IF ( factor.EQ.0. ) THEN
               factor = 10.
            ELSE
               factor = factor * 10.
            END IF
         END IF
         IF ( kp.GE.Ltok ) GOTO 190
         kp = kp + 1
         GOTO 130
      END IF
C---
C Allow an (optional) single decimal point.
      IF ( Ctok(kp:kp).EQ.'.' ) THEN
         IF ( ldum.LT.LEN(cdum)-5 ) THEN
            ldum = ldum + 1
            cdum(ldum:ldum) = Ctok(kp:kp)
         END IF
 150     kp = kp + 1
         IF ( Ctok(kp:kp).LT.'0' .OR. Ctok(kp:kp).GT.'9' ) GOTO 160
         IF ( ldum.LT.LEN(cdum)-5 ) THEN
            ldum = ldum + 1
            cdum(ldum:ldum) = Ctok(kp:kp)
         END IF
         IF ( kp.LT.Ltok ) GOTO 150
      END IF
C---
C Check for exponent.
 160  IF ( kp.GT.Ltok ) GOTO 190
      chr = Ctok(kp:kp)
      CALL UPC(chr)
      IF ( chr.NE.'E' .AND. chr.NE.'D' ) GOTO 190
C Change 'Ex' into '1Ex'
      IF ( ldum.EQ.0 ) THEN
         ldum = ldum + 1
         cdum(ldum:ldum) = '1'
      END IF
      ldum = ldum + 1
      cdum(ldum:ldum) = chr
      kp = kp + 1
      IF ( Ctok(kp:kp).EQ.'+' .OR. Ctok(kp:kp).EQ.'-' ) THEN
         ldum = ldum + 1
         cdum(ldum:ldum) = Ctok(kp:kp)
         kp = kp + 1
      END IF
 180  IF ( Ctok(kp:kp).GE.'0' .AND. Ctok(kp:kp).LE.'9' ) THEN
         ldum = ldum + 1
         cdum(ldum:ldum) = Ctok(kp:kp)
         kp = kp + 1
         GOTO 180
      END IF
C---
 190  IF ( ldum.EQ.0 ) GOTO 900
C
      IF ( level.GE.MXLEV ) GOTO 900
      level = level + 1
      READ (cdum(ldum-23:ldum),191,IOSTAT=ios) stack(level)
 191  FORMAT (D24.0)
C
      IF ( ios.NE.0 ) GOTO 900
      IF ( factor.NE.0. ) stack(level) = stack(level) * factor
C---
C If two or more characters remain on the line, then check for an operator.
  210 IF ( kp.GE.Ltok ) THEN
         iop(level) = 0
      ELSE
         iop(level) = INDEX( '+-*/^', Ctok(kp:kp) )
         IF ( iop(level).GT.0 ) kp = kp + 1
      END IF
C---
C If more than one number on stack, see if we can apply operator.
      IF ( level.GT.1 ) THEN
 240     CONTINUE
         IF ( iop(level-1).GE.100 ) THEN
            IF ( Ctok(kp:kp).NE.')' ) THEN
               GOTO 120
            ELSE
               iop(level-1) = iop(level-1) - 100
               IF ( iop(level-1).EQ.0 ) THEN
C This case occurs if we have a leading ( which has now been matched with a ).
                  stack(level-1) = stack(level)
                  level = level - 1
               END IF
               kp = kp + 1
               GOTO 210
            END IF
         END IF
         IF ( ipri(iop(level)).GE.ipri(iop(level-1)) ) THEN
            IF ( iop(level-1).EQ.1 ) THEN
               stack(level-1) = stack(level-1) + stack(level)
            ELSE IF ( iop(level-1).EQ.2 ) THEN
               stack(level-1) = stack(level-1) - stack(level)
            ELSE IF ( iop(level-1).EQ.3 ) THEN
               stack(level-1) = stack(level-1) * stack(level)
            ELSE IF ( iop(level-1).EQ.4 ) THEN
               stack(level-1) = stack(level-1) / stack(level)
            ELSE IF ( iop(level-1).EQ.5 ) THEN
               stack(level-1) = stack(level-1)**stack(level)
            ELSE
               WRITE(*,*) 'FPNUM--Program error: ',level,iop(level-1)
            END IF
            iop(level-1) = iop(level)
            level = level - 1
            IF ( level.GT.1 ) GOTO 240
         END IF
      END IF
C
      IF ( iop(level).NE.0 ) GOTO 120
C
C No more operators, then pop top number in stack and return.
      FPNUM = stack(level)
      Ier = 0
C---
 900  CONTINUE
      RETURN
      END
C*********
      INTEGER FUNCTION ISNUM(Ctok,Ltok)
      CHARACTER Ctok*(*)
      INTEGER   Ltok
C---
C Return -1 if CTOK is a number, 0 otherwise.
C---
C Ctok    I
C Ltok    I
C---
C [AFT]
C---
      CHARACTER chr
C---
      ISNUM = 0
      IF ( Ltok.LE.0 ) RETURN
      chr = Ctok(1:1)
      IF ( chr.EQ.'+' .OR. chr.EQ.'-' .OR. chr.EQ.'.' .OR.
     &     (chr.GE.'0' .AND. chr.LE.'9') ) THEN
C- First digit OK, check last digit
         chr = Ctok(Ltok:Ltok)
         IF ( chr.GE.'0' .AND. chr.LE.'9' ) GOTO 900
         IF ( Ltok.GT.1 .AND. chr.EQ.'.' ) GOTO 900
      END IF
C- Now check for NO keyword.
      IF ( Ltok.NE.2 ) RETURN
      IF ( chr.NE.'N' .AND. chr.NE.'n' ) RETURN
      chr = Ctok(2:2)
      IF ( chr.NE.'O' .AND. chr.NE.'o' ) RETURN
C---
 900  ISNUM = -1
      RETURN
      END
C*********
      SUBROUTINE IRANGE(Ctok,Ltok,Ilodef,Ihidef,Ilo,Ihi,Ier)
      CHARACTER Ctok*(*)
      INTEGER   Ltok, Ilodef, Ihidef, Ilo, Ihi, Ier
C---
C Parse a range from the token.
C---
C Ctok    I
C Ltok    I
C Ilodef  I
C Ihidef  I
C Ilo       O
C Ihi       O
C Ier       O
C---
C [AFT]
C---
      REAL      FPNUM
      INTEGER   itmp
C---
      Ilo = Ilodef
      Ihi = Ihidef
      itmp = INDEX(Ctok,'..')
      IF ( itmp.GT.0 ) THEN
         IF ( itmp.GT.1 ) THEN
            Ilo = INT(FPNUM(Ctok,itmp-1,Ier))
         END IF
         IF ( itmp.LT.Ltok-1 ) THEN
            Ihi = INT(FPNUM(Ctok(itmp+2:),Ltok-itmp-1,Ier))
         END IF
      ELSE IF ( Ltok.GT.0 ) THEN
         Ilo = INT(FPNUM(Ctok,Ltok,Ier))
         Ihi = Ilo
      END IF
      RETURN
      END


C---
      INTEGER FUNCTION LENACT(CBUF)
      CHARACTER CBUF*(*)
C---
C Function to return the active length of a character string, not
C counting any trailing blanks.  N.B. an all blank string will
C return ZERO as the length.
C---
C CBUF    I    String whose length is to be measured.
C---
C 1988-Jun-13 - Standard Fortran version [AFT]
C---
      INTEGER   I
C---
      DO 190 I=LEN(CBUF),1,-1
         IF(CBUF(I:I).NE.' ') THEN
            LENACT=I
            RETURN
         END IF
  190 CONTINUE
      LENACT=0
      RETURN
      END
      
C*********


      FUNCTION trfnam(inname)

      CHARACTER*(*) trfnam, inname

c  Function to translate a filename from the system-independent format 
c  to the system-dependent format. The system-dependency is wrapped-up
c  in the PTEND call.

c  kaa   1/27/93

      INTEGER lname, ltmp, kp
      INTEGER lenact
      CHARACTER(255) ctmp, cdisk, cdir, cfile

      EXTERNAL lenact

      kp = 0
      lname = lenact(inname)
      CALL alf(inname, lname, kp, ctmp, ltmp)

      IF ( ltmp .LT. lname ) then
         cdisk = ctmp
         CALL alf(inname, lname, kp, ctmp, ltmp)
         cdir = ctmp
         CALL alf(inname, lname, kp, ctmp, ltmp)
         cfile = ctmp
         CALL ptend(cdisk, cdir, cfile)
         trfnam = cfile
      ELSE
         trfnam = inname
      ENDIF

      END
**==XERROR.spg  processed by SPAG 3.09I  at 09:58 on 23 Apr 1993
      SUBROUTINE XAERROR(Cstr,Nonimp)
      CHARACTER Cstr*(*)
      INTEGER Nonimp
C---
C XPARSE subroutine to write an error string on the terminal and/or the log
C file, depending on the values of the chattyness flags
C---
C CSTR    I    String to be written.  N.B. the first character is
C              assumed to contain FORTRAN carriage control infor-
C              mation
C NONIMP  I    The unimportance of the string.  If =0, the string
C              is never written, else if 1 < NONIMP < TRMCHT (terminal
C              chattyness) the string is written to the terminal
C              OR appended to the error log file if currently open.
C---
C 1991-Nov-21 - Andy Pollock's adaptation of xwrite
C 1992-Sep-30 - Bruce O'Neel modified xwrite
C 1993-Apr-23 - Bruce O'Neel, Added code to write to stderr or sys$error
c 1994-Nov-28 - EAG call fxwrite instead
C---
      INCLUDE 'xparinc.inc'
C
      INTEGER destination
 
c      character(200) tstr , tstr1
C---
C      tstr = 'ERROR: ' // Cstr
c      tstr = Cstr
      IF ( Nonimp.EQ.0 ) RETURN

      destination = 0
      IF ( ABS(Nonimp).LE.Trmcht ) destination = destination + 2
      if ( ABS(Nonimp) .le. Logcht) destination = destination + 4
      call fxwrite (Cstr, destination)

      RETURN
C---
      END
C*********
      SUBROUTINE FORTYP(IFTYPE)
      INTEGER   IFTYPE
C---
C IFTYPE should return
C  0  if single character IO (RDCHR, PUTSTR) has NOT been implemented.
C -1  if Fortran I/O preceeds write statements with a CR//LF.
C +1  if CR//LF follow Fortran write operations.
C---
      IFTYPE=+1
      RETURN
      END
C*********
      SUBROUTINE PUTSTR (CBUF, LBUF)
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C Write CBUF(:LBUF) characters to the terminal.  If LBUF=0 then
C CBUF(:LENAC(LBUF)) characters are written.  It is very important
C that no other characters such as CR, LF, etc. are written.
C---
      INTEGER   LENACT
C
      INTEGER   I, ITMP
C---
      ITMP=LBUF
      IF(ITMP.EQ.0) ITMP=LENACT(CBUF)
      DO I=1,LBUF
         CALL PUTC(CBUF(I:I))
      END DO
      RETURN
      END
C*********
