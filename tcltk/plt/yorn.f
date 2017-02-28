      SUBROUTINE YORN(CPROM, IDEF, IANS)
      CHARACTER CPROM*(*)
      INTEGER   IDEF, IANS
C---
C Simple routine to ask the user a Yes/No question.
C---
C Cprom   I    The prompting question
C Idef    I/O  The default answer.  Idef=0 means user cannot default.
C Ians      O  =1 for yes, =-1 for no, =0 for EOF.
C---
C 1992-Jul-22 - [AFT]
C---
      INTEGER   LENACT
C
      CHARACTER CTMP*80
      CHARACTER CDUM*3
      INTEGER   IER, ITMP, LDUM
C---
      ITMP=LENACT(CPROM)
      IF( IDEF.LT.0 ) THEN
         CTMP=CPROM(:ITMP)//' (N)?'
      ELSE IF ( IDEF.GT.0 ) THEN
         CTMP=CPROM(:ITMP)//' (Y)?'
      ELSE
         CTMP=CPROM(:ITMP)//'?'
      END IF
C---
  100 CALL GTBUF(CTMP, IER)
      IF(IER.LT.0) THEN
         IANS=0
      ELSE
         CALL GTREST(CDUM, LDUM)
         IANS = 0
         IF ( LDUM.EQ.0 ) THEN
            IANS = IDEF
         ELSE IF(CDUM(1:1).EQ.'Y' .OR. CDUM(1:1).EQ.'y') THEN
            IANS = 1
         ELSE IF(CDUM(1:1).EQ.'N' .OR. CDUM(1:1).EQ.'n') THEN
            IANS = -1
         END IF
         IF ( IANS.EQ.0 ) THEN
            WRITE(*,121)
  121       FORMAT(' Please answer Y or N')
            GOTO 100
         END IF
         IF ( IDEF.NE.IANS ) IDEF = IANS
      END IF
      RETURN
      END
