      LOGICAL FUNCTION XQMTCH(TEST, BASE, QPART)
      CHARACTER*(*) TEST,BASE
      LOGICAL   QPART
C---
C XPARSE function to see if a string is the same as a base string
C---
C XQMTCH       Function result: if true, then a match exists
C TEST    I    The string to be tested (this must have already been
C              -LENACT-ed so that its length has removed padding
C BASE    I    The string to be compared against
C QPART     O  If true, then only a partial match was made (only
C              -allowed if the flag QXPART is true), otherwise
C              -a complete match.
C---
C 1985-Oct-18 - rashafer
C---
      INCLUDE 'xparinc.inc'
      INTEGER   LENACT
      INTEGER   LTEST, LBASE, ITEST, IBASE, I
      character(1) CTEST, CBASE
C---
      LTEST=LEN(TEST)
      LBASE=LENACT(BASE)
      QPART=.FALSE.
      IF(LBASE.LT.LTEST) THEN
C** the base string was too short
         XQMTCH=.FALSE.
         RETURN
      ELSEIF(LBASE.GT.LTEST) THEN
C** the base string was longer than the test string
         IF(.NOT.QXPART) THEN
C** partial matches are not allowed
            XQMTCH=.FALSE.
            RETURN
         ELSE
            QPART=.TRUE.
         END IF
      END IF
C---
      IF(QXCASE)THEN
C** case is not significant so do the conversion
         DO I = 1,LTEST
            ITEST=ICHAR(TEST(I:I))
            IBASE=ICHAR(BASE(I:I))
            IF(ITEST.NE.IBASE) THEN
               CTEST = TEST(I:I)
               CALL LOCASE(CTEST)
               ITEST=ICHAR(CTEST)
               CBASE = BASE(I:I)
               CALL LOCASE(CBASE)
               IBASE=ICHAR(CBASE)
               IF(ITEST.NE.IBASE) THEN
                  XQMTCH=.FALSE.
                  RETURN
               END IF
            END IF
         END DO
         XQMTCH=.TRUE.
      ELSE
         XQMTCH=TEST.EQ.BASE(:LTEST)
      END IF
      RETURN
      END
