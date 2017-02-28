      SUBROUTINE XGTARG(STRING, IPARSE, IBEG, IEND, QSKIP, IFLAG,
     :   IDELIM, *, *, *)
      CHARACTER STRING*(*)
      INTEGER   IPARSE, IBEG, IEND, IFLAG, IDELIM
      LOGICAL   QSKIP
C---
C Version 2.0:  Allows for the request_prompt string (default ??
C      to be checked for.  Also, if the stirng is in prompt mode
C      then instead of the falling off the end condition being
C      returned, a ? is returned (inquiry character)
C---
C STRING  i    The string to be parsed
C IPARSE  i/r  The parse position, at the call the value
C              should be at the position just before where parsing
C              is to begin.  Therefore, for a string just begun it
C              should be 0.  On return it will be set at the position
C              appropriate to obtain the next argument.  When
C              IPARSE is > len(STRING), then there was no
C              argument found before the end of the string reached
C              (See the IFLAG = 1 condition below) while if
C              IPARSE = len(STRING) the NEXT call to XGTARG
C              will run off the end.  (Not all run-off-the-ends
C              can be predicted this way when continuation chars
C              are used).
C              N.B. as
C              each argument is parsed, its form may be modified,
C              particularly arguments in character strings, so it
C              is difficult to backup over previously parsed strings.
C              Thus, except for new strings (where IPARSE must
C              be initially set to zero) the value should not
C              be adjusted by the calling program.
C IBEG      r  The first character of the returned argument
C IEND      r  The last character of the returned argument
C QSKIP     r  If true, the argument field has been 'skipped over'
C              (i.e., it contains an empty field).
C IFLAG     r  The condition flag:
C              -1 - An EOF was generated while processing a
C                   continuation.  IPARSE is set > LEN(STRING).
C               0 - Nothing unusual
C               1 - The parse fell off the end (also indicated
C                   by IPARSE > LEN(STRING)).
C               2 - The field reached the terminal skip character.
C                   This and subsequent calls will raise the
C                   QSKIP=.TRUE. condition.
C IDELIM    r  The delimiter flag:
C               0 - The argument was normally delimited by a comma,
C                   end of line, or the infinite skip char.
C               1 - The argument was delimited by the
C                   first special delimiter
C               2 - The argument was delimited by the
C                   second special delimiter
C Alternate returns:
C      Alternate returns are based on the value of ABS(IFLAG)
C---
C 1985-Oct-20 - rashafer 
C 31 July 1991 : allow for an argument of the form .."...." [AMTP]
C 24 June 1992 : allow for ignoring delimiters in parentheses [AMTP]
C---
      INCLUDE 'xparinc.inc'
      LOGICAL XCHKDL,XCHKBL,XCHKOP,XCHKCL,QDEL,QDONE,QCOPY,QQUOTE
      INTEGER NL
      INTEGER LENSTR,ICOPY,IVAL,JVAL

C needed to ensure XPRSBD common block is initialized under gfortran

      CALL XPARSE(' ')

C---
      LENSTR=LEN(STRING)
      IFLAG=0
      IDELIM=0
      IPARSE=IPARSE+1
      IBEG=1
      IEND=0

C** Come from for case of continuation char processing OR the
C** REQUEST_INQUIRY string has tobggled off RETURN_INQUIRY
  100 CONTINUE
C** Skip to first non-blank character

      QDONE = .FALSE.
      DO WHILE(.NOT.QDONE)
          IF (IPARSE .LE. LENSTR) THEN
             IF ( XCHKBL(STRING(IPARSE:IPARSE)) ) THEN
                IPARSE=IPARSE+1
             ELSE
                QDONE = .TRUE.
             ENDIF
          ELSE
             QDONE = .TRUE.
          ENDIF
      END DO

      IF(IPARSE.GT.LENSTR) THEN
         QSKIP=.FALSE.
         IF(RETURN_INQUIRY) THEN
            IPARSE=LENSTR-1
            IBEG=LENSTR-1
            IEND=LENSTR-1
            STRING(IBEG:LENSTR)=INQUIRY
            RETURN
         ELSE
            IPARSE=LENSTR+1
            IFLAG=1
            RETURN 1
         END IF
      END IF

      QQUOTE=.FALSE.
      NL=0
      IF(STRING(IPARSE:IPARSE).EQ.OPNSTR) THEN
         QQUOTE=.TRUE.
         QSKIP=.FALSE.
         IBEG=IPARSE+1
         QDONE=.FALSE.
         QCOPY=.FALSE.
         DO WHILE((IPARSE.LT.LENSTR).AND.(.NOT.QDONE))
            IPARSE=IPARSE+1
            IF(STRING(IPARSE:IPARSE).EQ.CLSSTR) THEN
               IF(IPARSE.EQ.LENSTR) THEN
                  QDONE=.TRUE.
               ELSE
                  QDONE=STRING(IPARSE+1:IPARSE+1).NE.CLSSTR
                  IF(.NOT.QDONE) THEN
                     IF(.NOT.QCOPY) THEN
                        QCOPY=.TRUE.
                        ICOPY=IPARSE
                     END IF
                     IPARSE=IPARSE+1
                  END IF
               END IF
            END IF
            IF(QCOPY) THEN
               STRING(ICOPY:ICOPY)=STRING(IPARSE:IPARSE)
               ICOPY=ICOPY+1
            END IF
         END DO
         IF(QDONE) THEN
            IF(QCOPY) THEN
               IEND=ICOPY-2
            ELSE
               IEND=MAX(IPARSE-1,IBEG)
            END IF
            IPARSE=IPARSE+1
         ELSE
            IF(QCOPY) THEN
               IEND=ICOPY-1
            ELSE
               IEND=LENSTR
            END IF
         END IF
C** as a quoted string ends on the end-of-string character
C** then a false QDEL means that the end was not found by
C** a delimiter (such as comma or the specials), so the
C** existence of the specials must still be checked
         QDEL=.FALSE.
      ELSEIF(XCHKDL(STRING(IPARSE:),IVAL)) THEN
C** the first argument of the string was a delimiter, indicating
C** that either there was an empty field (i.e. just to a comment)
C** or a skipped field to a comma, infinite-skip, or some special
C** delimiter
         IF(IVAL.EQ.1) THEN
            IF(RETURN_INQUIRY) THEN
               IPARSE=LENSTR-1
               IBEG=LENSTR-1
               IEND=LENSTR-1
               STRING(IBEG:LENSTR)=INQUIRY
               RETURN
            ELSE
               IPARSE=LENSTR+1
               IFLAG=1
               RETURN 1
            END IF
         ELSE
            QSKIP=.TRUE.
            IF(IVAL.GT.0) THEN
               IF(IVAL.EQ.2) THEN
                  IPARSE=IPARSE-1
                  IFLAG=2
                  RETURN 2
               ELSE
C** a special delimiter
                  IDELIM=IVAL-2
               END IF
            END IF
            RETURN
         END IF
      ELSE
C** some ordinary character
         QSKIP=.FALSE.
         IBEG=IPARSE
         QDEL=.FALSE.
         QDONE=.FALSE.
         DO WHILE((.NOT.QDONE).AND.(IPARSE.LT.LENSTR))
            IPARSE=IPARSE+1
            IF(XCHKDL(STRING(IPARSE:),IVAL)) THEN
               QDONE=.TRUE.
               QDEL=.TRUE.
            ELSE IF(STRING(IPARSE:IPARSE).EQ.OPNSTR) THEN
               QQUOTE=.TRUE.
               DO WHILE((IPARSE.LT.LENSTR).AND.(QQUOTE))
                  IPARSE=IPARSE+1
                  QQUOTE=(STRING(IPARSE:IPARSE).NE.CLSSTR)
               END DO
            ELSE IF(XCHKOP(STRING(IPARSE:IPARSE))) THEN
               NL=1
               DO WHILE((IPARSE.LT.LENSTR).AND.(NL.GT.0))
                  IPARSE=IPARSE+1
                  IF(XCHKOP(STRING(IPARSE:IPARSE)))THEN
                     NL=NL+1
                  ELSE IF(XCHKCL(STRING(IPARSE:IPARSE)))THEN
                     NL=NL-1
                  ENDIF
               END DO
            ELSE
               QDONE=XCHKBL(STRING(IPARSE:IPARSE))
            END IF
         END DO
         IF(QDONE) THEN
            IEND=IPARSE-1
         ELSE
C** IPARSE must be the end of the string
            IEND=IPARSE
C** Set IPARSE to point past the end of the string (no more
C** processing will then probably be necessary)
            IPARSE=IPARSE+1
         END IF
      END IF
C** If not already at a delimiter, then advance until a non-blank
C** character is found
      IF(.NOT.QDEL) THEN
         IF(STRING(IPARSE:).EQ.' ')THEN
C** reset IPARSE to point to the last char in the string, so
C** that IPARSE > LENSTR is reserved for the condition that
C** this call to XGTARG caused a run-off-the-end, while
C** IPARSE = LENSTR means that the NEXT call to XGTARG will
C** generate that condition
            IPARSE = LENSTR
         ELSE
            DO WHILE(XCHKBL(STRING(IPARSE:IPARSE)).AND.
     &               (IPARSE.LE.LENSTR))
               IPARSE=IPARSE+1
            END DO
            IF(XCHKDL(STRING(IPARSE:),JVAL)) THEN
               IF(JVAL.EQ.1) THEN
                  IPARSE=LENSTR
               ELSEIF(JVAL.EQ.2) THEN
                  IPARSE=IPARSE-1
               ELSEIF(JVAL.GE.3) THEN
C** as a special delimiter ends the argument, QDEL is
C** set to allow for the appropriate special return
                  QDEL=.TRUE.
                  IVAL=JVAL
               END IF
            ELSE
               IPARSE=IPARSE-1
            END IF
         END IF
      END IF
      IF(QDEL) THEN
C** the argument ended on a delimiter
         IF(IVAL.EQ.1) THEN
            IPARSE=LENSTR
         ELSEIF(IVAL.EQ.2) THEN
            IPARSE=IPARSE-1
C** here the infinite-skip condition char was found.  The
C** special condition will not be raised until the NEXT
C** argument is parsed.
            RETURN
         ELSEIF(IVAL.GE.2) THEN
            IDELIM=IVAL-2
            RETURN
         END IF
      END IF
C** now check for continuation condition
      IF((IPARSE.EQ.LENSTR).AND.(.NOT.QQUOTE).AND.(NL.EQ.0)
     &                     .AND.(STRING(IBEG:IEND).EQ.CONTIN)) THEN
         CALL XCREAD('->',STRING,IVAL)
         IF(IVAL.EQ.0) THEN
            IPARSE=1
            GOTO 100
         END IF
         IFLAG=-1
         RETURN 1
      END IF
C** check for inquiry request
      IF((.NOT.QQUOTE).AND.(NL.EQ.0).AND.
     &   (STRING(IBEG:IEND).EQ.REQUEST_INQUIRY)) THEN
         RETURN_INQUIRY=.NOT. RETURN_INQUIRY
         IF(.NOT. RETURN_INQUIRY) THEN
            IPARSE=IPARSE+1
            GOTO 100
         END IF
         IBEG=IEND
         STRING(IBEG:IEND)=INQUIRY
      END IF
      RETURN
      END
