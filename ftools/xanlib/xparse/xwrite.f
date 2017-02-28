**==XWRITE.spg  processed by SPAG 3.09I  at 15:32 on  9 Sep 1992
      SUBROUTINE XWRITE(Cstr,Nonimp)
      CHARACTER Cstr*(*)
      INTEGER   Nonimp
C Entry XWRTPR
      CHARACTER Cout*(*)
C---
C XPARSE subroutine to write a string on the terminal and/or the log
C file, depending on the values of the chattyness flags
C---
C CSTR    I    String to be written.  N.B. the first character is
C              assumed to contain FORTRAN carriage control infor-
C              mation
C NONIMP  I    The unimportance of the string.  If =0, the string
C              is never written, else if 1 < NONIMP < TRMCHT (terminal
C              chattyness) the string is written to the terminal
C              and if 1 < ABS(NONIMP) < LOGCHT the string is
C              appended to the log file (if currently open).
C---
C 1986-Mar-08 - rashafer
C---
      INCLUDE 'xparinc.inc'
C
      INTEGER   destination
      logical iflog

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')
C---
      IF ( Nonimp.EQ.0 ) RETURN

      destination = 0
      IF ( (Nonimp.LE.Trmcht) .AND. (Nonimp.GT.0) ) destination = 1
      IF ( ABS(Nonimp).LE.Logcht ) destination = destination + 4
      call fxwrite (Cstr, destination)

      RETURN
C*********
      ENTRY XWRTPR(Cout)
C---
C XWRTPR is used to write a string to the terminal/log file in
C exactly the same way as for a prompt, but without requesting
C any input.  Useful for creating multiple line prompts.
C---
      destination = 5
      call fxwrite (Cout, destination)

      RETURN
C---
99001 FORMAT (A)
      END
