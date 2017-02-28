      SUBROUTINE XCREAD(CPROM, CBUF, IER)
      CHARACTER CPROM*(*), CBUF*(*)
      INTEGER   IER
C---
C XPARSE subroutine to read in a command line.
C---
C CPROM   I    The prompt string, if blank then there is no prompt.
C CBUF      O  The returned string after input (blank only if there
C              is an eof or an error, or if an empty string input)
C IER       O  =0 then something was read otherwise the IO error flag
C              (if <0 then an EOF was raised)
C---
C 1989-Aug-08 - New GTBUF version - [AFT]
C---
      INTEGER   LBUF
      CALL GTBUF(CPROM, IER)
      IF(IER.NE.0) RETURN
      CALL GTREST(CBUF, LBUF)
      RETURN
      END

C---
C XCREAD_FROM_C: identical to XCREAD but also returns LBUF, needed
C by xanlib/xpi/xpigettext.c such that we may preserve a space ' '
C entered for a parameter value.
C---
      SUBROUTINE XCREAD_FROM_C(CPROM,CBUF,LBUF,IER)
      CHARACTER CPROM*(*), CBUF*(*)
      INTEGER   IER
C---
C CPROM   I    The prompt string, if blank then there is no prompt.
C CBUF      O  The returned string after input (blank only if there
C              is an eof or an error, or if an empty string input)
C IER       O  =0 then something was read otherwise the IO error flag
C              (if <0 then an EOF was raised)
      INTEGER   LBUF
      CALL GTBUF(CPROM, IER)
      IF(IER.NE.0) RETURN
      CALL GTREST(CBUF, LBUF)
      RETURN
      END
