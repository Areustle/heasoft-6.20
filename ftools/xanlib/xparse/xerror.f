**==XERROR.spg  processed by SPAG 3.09I  at 09:58 on 23 Apr 1993
      SUBROUTINE XERROR(Cstr,Nonimp)
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
      INTEGER LENACT
C
      REAL rbuf
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
99001 FORMAT (A)
      END
