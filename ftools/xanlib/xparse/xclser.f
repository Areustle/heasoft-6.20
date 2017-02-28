      SUBROUTINE XCLSER(CDISP)
      CHARACTER CDISP*(*)
C---
C XPARSE subroutine to close (and dispose) of the error log file.
C---
C CDISP  I    The disposition string 'SAVE'/'DELETE'/' '
C---
C 1991-Nov-21 - [AMTP] adaptation of xclslg
C---
      REAL     RBUF
      INTEGER  LTMP, NBUF
C---
      CALL ERRLOG(4, RBUF, NBUF, CDISP, LTMP)
      RETURN
      END
