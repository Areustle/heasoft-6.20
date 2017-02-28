      SUBROUTINE PUTSTR (CBUF, LBUF)
      CHARACTER CBUF*(*)
      INTEGER   LBUF
C---
C Write CBUF(:LBUF) characters to the terminal.  If LBUF=0 then
C CBUF(:LENACT(LBUF)) characters are written.  It is very important
C that no other characters such as CR, LF, etc. are written.
C---
      INTEGER   LENACT
C
      INTEGER   ITMP
C---
      ITMP=LBUF
      IF(ITMP.EQ.0) ITMP=LENACT(CBUF)
      CALL cwrite(CBUF(:ITMP))
      RETURN
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
      SUBROUTINE PAGSZE(LINES)
      INTEGER   LINES
C---
C Returns the number of lines on a page
C---
      INTEGER ROW, COL

      CALL cpgsze(row, col)
      lines = row
      RETURN
      END
