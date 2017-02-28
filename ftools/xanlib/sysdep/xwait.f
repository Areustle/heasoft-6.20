      SUBROUTINE XWAIT(SECS)
      REAL SECS
C---
C Waits the specified no of seconds before returning
C---
C SECS   I    The number of seconds to wait
C---
      CALL sleep(nint(SECS))
      RETURN
      END
