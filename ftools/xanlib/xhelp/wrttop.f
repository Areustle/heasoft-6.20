      SUBROUTINE wrttop(NTOP, CHEAD, ICHSTR, ICHSTP, COUT)
      CHARACTER CHEAD*(*), COUT*(*)
      INTEGER NTOP, ICHSTR(*), ICHSTP(*)
C---
C DACHE facitility subroutine to write out the current topic chain
C---
C NTOP      I    No of topic levels currently active
C CHEAD     I    Contains topic names
C ICHSTR    I    Start character for the topic name
C ICHSTP    I    Stop character for the topic name
C COUT        O  Work space for output
C---
C 21-Jul-1985 - rashafer
C---
      INTEGER LENACT
C
      INTEGER ITOP, NCHAR, LENC
C---
      COUT = CHEAD(ICHSTR(1):ICHSTP(1))
      NCHAR = ICHSTP(1) + 1
      WRITE (*, *)
      DO 130 ITOP = 2, NTOP
         LENC = ICHSTP(ITOP) - ICHSTR(ITOP) + 1
         IF (LENC+NCHAR.GT.78) THEN
            NCHAR = LENACT(COUT(:NCHAR-1))
            WRITE (*, 111) COUT(:NCHAR)
 111        FORMAT (1X, A)
            NCHAR = 0
c           COUT(:NCHAR-1) = ' '
         ENDIF
         COUT(NCHAR+1:NCHAR+LENC) = CHEAD(ICHSTR(ITOP):ICHSTP(ITOP))
         NCHAR = NCHAR + LENC + 1
 130  CONTINUE
      IF (NCHAR.GT.0) THEN
         NCHAR = LENACT(COUT(:NCHAR-1))
         WRITE (*, 111) COUT(:NCHAR)
      ENDIF
      WRITE (*, *)
      RETURN
      END
