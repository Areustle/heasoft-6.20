      SUBROUTINE fndtop(CTOK, LTOK, NTOP, IEQTOP, CPICS, ICHSTR, ICHSTP)
      CHARACTER CTOK*(*), CPICS*(*)
      INTEGER   LTOK, NTOP, IEQTOP, ICHSTR(*), ICHSTP(*)
C---
C SHF subroutine to find the topic corresponding to a given topic
C CTOK, as input on unit one.  See SHFTOD for argument details.
C---
C CTOK(:LTOK) I:  The topic to match
C IEQTOP      R:  the topic number of the indicated topic CTOK
C                  if zero, then no match was found.
C---
C 21-Dec-1988 - Cleaned up [AFT]
C  1-Jun-1985 - rashafer
C---
      INTEGER   I
      LOGICAL   QMATCH
C---
      DO 190 I = 1, NTOP
         CALL SMATCH(CTOK(1:LTOK), CPICS(ICHSTR(I):ICHSTP(I)), .FALSE.,
     &               .TRUE., QMATCH)
         IF (QMATCH) THEN
            IEQTOP = I
            RETURN
         ENDIF
 190  CONTINUE
      IEQTOP = 0
      RETURN
      END
