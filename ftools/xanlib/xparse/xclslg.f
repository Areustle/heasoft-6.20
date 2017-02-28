      SUBROUTINE XCLSLG(CDISP)
      CHARACTER CDISP*(*)
C---
C XPARSE subroutine to close (and dispose) of the logfile.
C---
C CDISP  I    The disposition string 'SAVE'/'DELETE'/' '
C---
C 1990-Sep-12 - New version to use LOGGER driver - [AFT]
C---
      REAL     RBUF
      INTEGER  LTMP, NBUF
C---
      CALL LOGGER(4, RBUF, NBUF, CDISP, LTMP)
      RETURN
      END
