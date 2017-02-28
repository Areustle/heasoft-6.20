**==XWARN.spg  processed by SPAG 3.09I  at 15:17 on 30 Sep 1992
      SUBROUTINE XWARN(Cstr,Nonimp)
      CHARACTER Cstr*(*)
      INTEGER Nonimp
C---
C XPARSE subroutine to write a warning string on the terminal and/or the log
C file, depending on the values of the chattyness flags
C---
C CSTR    I    String to be written.  N.B. the first character is
C              assumed to contain FORTRAN carriage control infor-
C              mation
C NONIMP  I    The unimportance of the string.  If =0, the string
C              is never written, else if 1 < NONIMP < TRMCHT (terminal
C              chattyness) the string is written to the terminal
C              OR appended to the warning log file if currently open.
C---
C 1991-Nov-21 - Andy Pollock's adaptation of xwrite
C 1992-Sep-30 - Bruce O'Neel modified xwrite
C---
      INCLUDE 'xparinc.inc'
      INTEGER LENACT
C
      REAL rbuf
      INTEGER lstr , nbuf
 
      character(200) tstr
C---
      tstr = 'WARNING: ' // Cstr
      IF ( Nonimp.EQ.0 ) RETURN
      IF ( ABS(Nonimp).LE.Trmcht ) THEN
	call xwrite(tstr,nonimp)
      ENDIF
      RETURN
C---
99001 FORMAT (A)
      END
