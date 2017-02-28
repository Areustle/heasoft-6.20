      SUBROUTINE XCLSSC(CDISP)
      CHARACTER CDISP*(*)
C---
C XPARSE subroutine to close (and dispose) of the logfile.
C---
C CDISP  I    The disposition string 'SAVE'/'DELETE'/' '
C---
C 1990-Nov-01 - Created from XCLSLG [AFT]
C---
      REAL     RBUF
      INTEGER  LTMP, NBUF
C---
      CALL SCRIPT(4, RBUF, NBUF, CDISP, LTMP)
      RETURN
      END
