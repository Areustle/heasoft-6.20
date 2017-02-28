      SUBROUTINE XWRCMD(LEVEL, CPROM, LPROM, CBUF, LBUF)
      INTEGER   LEVEL, LPROM, LBUF
      CHARACTER CPROM*(*), CBUF*(*)
C---
C Write the command prompt and response (if logging).
C---
C 1994-Aug-19 - Prevent overflow if LEN(CBUF)>LEN(CTMP) [AFT]
C 1991-Feb-07 - Only write SCRIPT file [AFT]
C 1990-Sep-25 - Use LOG and SCRIPT file device drivers [AFT]
C---
      CHARACTER(1000) CTMP
      CHARACTER(16) CHRIEK
      REAL      RBUF
      INTEGER   LTMP, NBUF, NEWL
      DATA CHRIEK/'!!!!!!!!!!!!!!!!'/
C---
C Create script string.
      CTMP = ' '
      LTMP = 0
      IF(LEVEL.GT.0) THEN
         LTMP = MIN(MAX(1,LEVEL),LEN(CHRIEK))
         CTMP(:LTMP) = CHRIEK(:LTMP)
      END IF
C First add what user typed to string
      IF(LBUF.GT.0) THEN
         NEWL = MIN(LEN(CTMP),LTMP+LBUF)
         CTMP(LTMP+1:NEWL) = CBUF(:LBUF)
         LTMP = NEWL
      END IF
C Leave a min of 30 spaces for neat output.
      IF(LTMP.LT.30) THEN
         LTMP = 30
      ELSE
         IF ( LTMP.LT.LEN(CTMP) ) LTMP = LTMP+1
      END IF
C Next add the comment character
      IF ( LTMP+2.LE.LEN(CTMP) ) THEN
         CTMP(LTMP+1:LTMP+2) = '! '
         LTMP = LTMP+2
      END IF
C Add the prompt that the user reponded to.
      IF(LPROM.GT.0) THEN
         NEWL = MIN(LEN(CTMP),LTMP+LPROM)
         CTMP(LTMP+1:NEWL) = CPROM(:LPROM)
         LTMP = NEWL
      END IF
C Write to script file.
      CALL SCRIPT(5, RBUF, NBUF, CTMP, LTMP)
      RETURN
      END
