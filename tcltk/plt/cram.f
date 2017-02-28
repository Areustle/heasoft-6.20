C--- CRAM.FOR contains entry points for:
C CRAMDF
C CRAMFF
C CRAMF
C CRAMI
C CRAMI8
C CRAMIF
C CRAM
C*********
      SUBROUTINE CRAMDF(Dnum, Nspace, Ndig, Cbuf, Lbuf)
      DOUBLE PRECISION Dnum
      INTEGER   Nspace, Ndig, Lbuf
      CHARACTER Cbuf*(*)
C---
C Do a formatted conversion of Dnum into ASCII characters.  If NDIG>0
C then the NDIG right of decimal point will be diplayed using an F format.
C If this would cause more than MXDIG digits to be displayed, then E
C format with maximum accuracy will be used instead.  If NDIG<0 then
C only ABS(NDIG) of the number will be displayed, in F format it possible,
C G format otherwise.  Using NDIG=0 is equivalent to calling CRAMF.
C In all cases a minimum of NSPACE characters are used in CBUF and leading
C blanks are used to fill in the extra space.  If the number cannot
C be expressed in NSPACE characters, then the additional space is
C allocated to the buffer to write the number.  This messes up a nice
C neat column format, I like this better than not seeing the number
C at all.
C---
C Dnum      I    Number to convert
C NSPACE    I    Number of spaces to use in CBUF
C NDIG      I    Number of digits to output.  NDIG=0 outputs all digits.
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C 2003-Jan-24 - Extracted from CRAMFF [AFT]
C---
      DOUBLE PRECISION NO
      PARAMETER (NO=-1.2D-34)
C MXDIG is the maximum number of digits that can be represented in a
C single precision floating point number.
      INTEGER   MXDIG
      PARAMETER (MXDIG=14)
      INTEGER   LENACT
C
      CHARACTER cdum*32, cform*32
      INTEGER   i, iexp, itmp, kp, ldum, nf, ng
C---
      IF ( Dnum.EQ.NO ) THEN
C No data magic number
         cdum = 'NO'
         ldum = 2
      ELSE IF ( Dnum.NE.Dnum ) THEN
C Must be a IEEE NaN, output as No data
         cdum = 'NO'
         ldum = 2
      ELSE IF ( Dnum.EQ.0. ) THEN
C Zero, special case
         cdum = ' 0'
         ldum = 2
      ELSE
         IF ( Ndig.EQ.0 ) THEN
C Write all digits of number.
            WRITE (cdum,*) Dnum
C  171       FORMAT(1PG14.7)
            CALL CRAM(cdum,ldum)
         ELSE
C IEXP is the exponent when number is expressed in scientific notation.
            iexp = INT(500.+LOG10(ABS(Dnum))) - 500
C
            IF ( Ndig.GT.0 ) THEN
C Display NDIG right of decimal point
               nf = MIN(Ndig,MXDIG)
               ng = MXDIG - 1
            ELSE
C Display ABS(NDIG) total
               ng = MIN(ABS(Ndig),MXDIG) - 1
               nf = ng - iexp
            END IF
C---
C If only displaying 0 to MXDIG digits right of decimal point AND if
C fewer than MXDIG total digits are to be displayed,
            IF ( 0.LE.nf .AND. nf.LE.Nspace .AND. (nf+iexp+1).LE.MXDIG )
     &           THEN
C then use F format,
               IF ( nf.LE.9 ) THEN
                  WRITE (cform,121) nf
 121              FORMAT ('(F14.',I1,')')
               ELSE
                  WRITE (cform,131) nf
 131              FORMAT ('(F14.',I2,')')
               END IF
            ELSE
C else use G Format.  Note, there is a bug in that if the number is displayed
C in an F FORMAT, then the scale factor is not applied, and one too few
C digits is displayed.
               IF ( ng.LE.9 ) THEN
                  WRITE (cform,141) ng
 141              FORMAT ('(1PE20.',I1,')')
               ELSE
                  WRITE (cform,151) ng
 151              FORMAT ('(1PE20.',I2,')')
               END IF
C---
            END IF
            WRITE (cdum,cform) Dnum
            ldum = LENACT(cdum)
C---
C Now strip leading blanks (if any).
            kp = 0
            CALL ALFSKS(cdum,ldum,kp)
            IF ( kp.GT.0 ) THEN
               DO 100 i = 1, ldum - kp
                  cdum(i:i) = cdum(i+kp:i+kp)
 100           CONTINUE
               ldum = ldum - kp
            END IF
         END IF
      END IF
C---
C Copy to output buffer, right justified.
      IF ( ldum.LE.Nspace ) THEN
         itmp = Nspace - ldum
         Cbuf(Lbuf+1:Lbuf+itmp) = ' '
         Cbuf(Lbuf+itmp+1:Lbuf+Nspace) = cdum(:ldum)
         Lbuf = Lbuf + Nspace
      ELSE
         Cbuf(Lbuf+1:Lbuf+ldum) = cdum(:ldum)
         Lbuf = Lbuf + ldum
      END IF
      RETURN
      END
C*********
      SUBROUTINE CRAMFF(Fnum,Nspace,Ndig,Cbuf,Lbuf)
      REAL      Fnum
      INTEGER   Nspace, Ndig, Lbuf
      CHARACTER Cbuf*(*)
C---
C Do a formatted conversion of FNUM into ASCII characters.  If NDIG>0
C then the NDIG right of decimal point will be diplayed using an F format.
C If this would cause more than MXDIG digits to be displayed, then E
C format with maximum accuracy will be used instead.  If NDIG<0 then
C only ABS(NDIG) of the number will be displayed, in F format it possible,
C G format otherwise.  Using NDIG=0 is equivalent to calling CRAMF.
C In all cases a minimum of NSPACE characters are used in CBUF and leading
C blanks are used to fill in the extra space.  If the number cannot
C be expressed in NSPACE characters, then the additional space is
C allocated to the buffer to write the number.  This messes up a nice
C neat column format, I like this better than not seeing the number
C at all.
C---
C FNUM      I    Number to convert
C NSPACE    I    Number of spaces to use in CBUF
C NDIG      I    Number of digits to output.  NDIG=0 outputs all digits.
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C 1992-Sep-01 - rewrite [AFT]
C 1994-May-12 - Add test for NaN [AFT]
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
C MXDIG is the maximum number of digits that can be represented in a
C single precision floating point number.
      INTEGER   MXDIG
      PARAMETER (MXDIG=7)
      INTEGER   LENACT
C
      CHARACTER cdum*32, cform*32
      INTEGER   i, iexp, itmp, kp, ldum, nf, ng
C---
      IF ( Fnum.EQ.NO ) THEN
C No data magic number
         cdum = 'NO'
         ldum = 2
      ELSE IF ( Fnum.NE.Fnum ) THEN
C Must be a IEEE NaN, output as No data
         cdum = 'NO'
         ldum = 2
      ELSE IF ( Fnum.EQ.0. ) THEN
C Zero, special case
         cdum = ' 0'
         ldum = 2
      ELSE
         IF ( Ndig.EQ.0 ) THEN
C Write all digits of number.
            WRITE (cdum,*) Fnum
C  171       FORMAT(1PG14.7)
            CALL CRAM(cdum,ldum)
         ELSE
C IEXP is the exponent when number is expressed in scientific notation.
            iexp = INT(500.+LOG10(ABS(Fnum))) - 500
C
            IF ( Ndig.GT.0 ) THEN
C Display NDIG right of decimal point
               nf = MIN(Ndig,MXDIG)
               ng = MXDIG - 1
            ELSE
C Display ABS(NDIG) total
               ng = MIN(ABS(Ndig),MXDIG) - 1
               nf = ng - iexp
            END IF
C---
C If only displaying 0 to MXDIG digits right of decimal point AND if
C fewer than MXDIG total digits are to be displayed,
            IF ( 0.LE.nf .AND. nf.LE.MXDIG .AND. (nf+iexp+1).LE.MXDIG )
     &           THEN
C then use F format,
               WRITE (cform,121) nf
 121           FORMAT ('(F14.',I1,')')
            ELSE
C else use G Format.  Note, there is a bug in that if the number is displayed
C in an F FORMAT, then the scale factor is not applied, and one too few
C digits is displayed.
               WRITE (cform,141) ng
 141           FORMAT ('(1PE14.',I1,')')
C---
            END IF
            WRITE (cdum,cform) Fnum
            ldum = LENACT(cdum)
C---
C Now strip leading blanks (if any).
            kp = 0
            CALL ALFSKS(cdum,ldum,kp)
            IF ( kp.GT.0 ) THEN
               DO 100 i = 1, ldum - kp
                  cdum(i:i) = cdum(i+kp:i+kp)
 100           CONTINUE
               ldum = ldum - kp
            END IF
         END IF
      END IF
C---
C Copy to output buffer, right justified.
      IF ( ldum.LE.Nspace ) THEN
         itmp = Nspace - ldum
         Cbuf(Lbuf+1:Lbuf+itmp) = ' '
         Cbuf(Lbuf+itmp+1:Lbuf+Nspace) = cdum(:ldum)
         Lbuf = Lbuf + Nspace
      ELSE
         Cbuf(Lbuf+1:Lbuf+ldum) = cdum(:ldum)
         Lbuf = Lbuf + ldum
      END IF
      RETURN
      END
C*********
      SUBROUTINE CRAMF(Fnum,Cbuf,Lbuf)
      REAL      Fnum
      CHARACTER Cbuf*(*)
      INTEGER   Lbuf
C---
C Convert floating point number into ASCII characters and
C append to CBUF.
C---
C FNUM      I    Number to convert
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C AFT
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      CHARACTER cdum*32
      INTEGER   ldum
C---
      IF ( Fnum.NE.NO ) THEN
         WRITE (cdum,*) Fnum
C  121    FORMAT(1PG14.7)
         CALL CRAM(cdum,ldum)
      ELSE
         cdum = 'NO'
         ldum = 2
      END IF
      Cbuf(Lbuf+1:Lbuf+ldum) = cdum
      Lbuf = Lbuf + ldum
      RETURN
      END
C*********
      SUBROUTINE CRAMI(Inum,Cbuf,Lbuf)
      INTEGER   Inum, Lbuf
      CHARACTER Cbuf*(*)
C---
C Convert integer number into ASCII characters and
C append to CBUF.
C---
C INUM      I    Number to convert
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C AFT
C---
      CHARACTER cdum*32
      INTEGER   ldum
C---
      WRITE (cdum,101) Inum
 101  FORMAT (I32)
      CALL CRAM(cdum,ldum)
      Cbuf(Lbuf+1:Lbuf+ldum) = cdum
      Lbuf = Lbuf + ldum
      RETURN
      END
C*********
      SUBROUTINE CRAMI8(I8num,Cbuf,Lbuf)
      INTEGER*8 I8num
      INTEGER   Lbuf
      CHARACTER Cbuf*(*)
C---
C Convert integer number into ASCII characters and
C append to CBUF.
C---
C I8NUM     I    Number to convert
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C 2008-May-15 - Extract from CRAMI - AFT
C---
      CHARACTER cdum*32
      INTEGER   ldum
C---
      WRITE (cdum,101) I8num
 101  FORMAT (I32)
      CALL CRAM(cdum,ldum)
      Cbuf(Lbuf+1:Lbuf+ldum) = cdum
      Lbuf = Lbuf + ldum
      RETURN
      END
C*********
      SUBROUTINE CRAMIF(Inum,Nspace,Cbuf,Lbuf)
      INTEGER   Inum, Nspace, Lbuf
      CHARACTER Cbuf*(*)
C---
C Convert integer number into ASCII characters and append to CBUF.
C---
C INUM      I    Number to convert
C NSPACE    I    Number of characters to pack number into
C CBUF      I/O  Character buffer
C LBUF      I/O  Number of valid characters in CBUF
C---
C AFT
C---
      CHARACTER cdum*32
      INTEGER ldum, il, nc
C---
      WRITE (cdum,101) Inum
 101  FORMAT (I32)
      CALL CRAM(cdum,ldum)
      nc = MAX(Nspace,ldum)
      il = 1 + nc - ldum
      Cbuf(Lbuf+il:Lbuf+nc) = cdum
      Lbuf = Lbuf + nc
      RETURN
      END
C*********
      SUBROUTINE CRAM(Cdum,Ldum)
      CHARACTER Cdum*(*)
      INTEGER   Ldum
C---
C Remove redundant information.
C---
C CBUF      I/O  Character buffer
C LBUF        O  Number of valid characters in CBUF
C---
C AFT
C---
      INTEGER   LENACT
      INTEGER   i, id, ie
C---
C- Remove leading blanks.
      i = 1
 100  IF ( Cdum(i:i).EQ.' ' ) THEN
         i = i + 1
         GOTO 100
      END IF
      Cdum = Cdum(i:)
C- Remove 0 in E+0x or E-0x
      ie = INDEX(Cdum,'E+0')
      IF ( ie.LE.0 ) ie = INDEX(Cdum,'E-0')
      IF ( ie.GT.0 ) Cdum(ie+2:) = Cdum(ie+3:) // ' '
C- Remove trailing E+0
      Ldum = LENACT(Cdum)
      IF ( Ldum.GT.2 ) THEN
         IF ( Cdum(Ldum-2:).EQ.'E+0' ) THEN
            Cdum(Ldum-2:Ldum) = '   '
            Ldum = Ldum - 3
         END IF
      END IF
C- Remove trailing zeros.
      id = INDEX(Cdum,'.')
      IF ( id.NE.0 ) THEN
         ie = INDEX(Cdum,'E') - 1
         IF ( ie.LE.0 ) ie = Ldum
         DO 130 i = ie, id + 1, -1
            IF ( Cdum(i:i).NE.'0' ) GOTO 190
            Cdum(i:) = Cdum(i+1:) // ' '
 130     CONTINUE
      END IF
C
 190  Ldum = LENACT(Cdum)
C- Remove trailing .
      IF ( Cdum(Ldum:Ldum).EQ.'.' ) Ldum = Ldum - 1
      IF ( Ldum.EQ.0 ) THEN
C- CRAM will transform .000000 into a blank string, which really should
C- be zero.
         Ldum = 1
         Cdum(1:1) = '0'
      ELSE
C- Blank fill.
         DO 200 i = Ldum + 1, LEN(Cdum)
            Cdum(i:i) = ' '
 200     CONTINUE
      END IF
      RETURN
      END
