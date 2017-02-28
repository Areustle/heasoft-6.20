      SUBROUTINE hdecod(Cbuf, Lbuf)
      CHARACTER Cbuf*(*)
      INTEGER   Lbuf
C---
C Web browsers encode data sending back to the server.  This routine
C effectively decodes by converting
C
C 1) "+" symbols back to spaces,
C 2) and hex codes (%xx) back to the original characters.
C---
C Cbuf    I/O  Buffer
C Lbuf    I/O  Active length
C---
C 1999-Apr-20 - [AFT]
C---
      INTEGER   i, ihex, itmp, lout
C---
      lout = 0
      i = 1
  110 CONTINUE
         IF ( Cbuf(i:i).EQ.'+' ) THEN
C Turn + signs into spaces
            lout = lout + 1
            Cbuf(lout:lout) = ' '
         ELSE IF ( Cbuf(i:i).EQ.'%' ) THEN
C Decode the HEX strings
            i = i + 1
            ihex = ICHAR(Cbuf(i:i)) - ICHAR('0')
            IF ( ihex.GT.9 ) ihex = ihex - 7
            i = i + 1
            itmp = ICHAR(Cbuf(i:i)) - ICHAR('0')
            IF ( itmp.GT.9 ) itmp = itmp - 7
            lout = lout + 1
            Cbuf(lout:lout) = CHAR(ihex*16+itmp)
         ELSE
C Just copy the original character
            lout = lout + 1
            Cbuf(lout:lout) = Cbuf(i:i)
         END IF
         i = i + 1
      IF ( i.LE.Lbuf ) GOTO 110
C---
      Lbuf = lout
      RETURN
      END
