      LOGICAL   FUNCTION XQWRTE (nonimp)
      INTEGER   nonimp
C---
C XPARSE logical function to return true if a string with the given
C nonimportance would be written currently EITHER to the
C terminal OR to the Log file
C---
C NONIMP  I    The unimportance of the string.  If =0, the string
C              is never writen, else if 1 < NONIMP < TRMCHT (terminal
C              chattyness) the string is written to the terminal
C              and if 1 < ABS(NONIMP) < LOGCHT the string is
C              appended to the log file (if currently open).
C---
      INCLUDE 'xparinc.inc'
      CHARACTER CTMP
      REAL      RBUF
      INTEGER   LSTR, NBUF
C---
      IF(NONIMP.EQ.0) then
	xqwrte= .FALSE.
	RETURN
      ENDIF
      CALL LOGGER(2, RBUF, NBUF, CTMP, LSTR)
      XQWRTE =
     &    ((nonimp.gt.0).and.(nonimp.le.trmcht)).or.
     &    ((nonimp.ne.0).and.(RBUF.ne.0.).and.(abs(nonimp).le.logcht))
      return
      end
