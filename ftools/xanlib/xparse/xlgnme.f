      SUBROUTINE XLGNME(CLNAM)
      CHARACTER CLNAM*(*)
C---
C XPARSE subroutine to return the name of the log file.
C---
C CLNAM     O    The log file name
C---
C 1990-Sep-14 - Rewrite to support log file driver [AFT]
C---
      REAL      RBUF
      INTEGER   LTMP, NBUF
C---
      CALL LOGGER(2, RBUF, NBUF, CLNAM, LTMP)
      IF(RBUF.EQ.0.0) CLNAM='none'
      RETURN
      END
