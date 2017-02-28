      INTEGER FUNCTION ISACT(IWIN, NGROUP, ICONT, ICWIN)
      INTEGER   IWIN(*), NGROUP, ICONT, ICWIN
C---
C Runs throught the IWIN array and returns 1 if ICWIN is currently
C active, and 0 otherwise
C---
C IWIN    I    Contains window that each group is to be plotted in
C NGROUP  I    The number of groups
C ICWIN   I    The current window
C---
C 1990-Jun-12
C---
      INTEGER   I
C---
C Activate windows that contain data
      DO 150 I=1,NGROUP
         IF(IWIN(I).EQ.ICWIN) THEN
            ISACT=1
            RETURN
         END IF
  150 CONTINUE
      IF(ICONT.NE.0 .AND. ICWIN.EQ.1) THEN
         ISACT=1
      ELSE
         ISACT=0
      END IF
      RETURN
      END
