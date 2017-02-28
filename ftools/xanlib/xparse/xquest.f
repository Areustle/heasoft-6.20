      SUBROUTINE XQUEST(CPROM, CDEF, QANS, QEOF)
      CHARACTER CPROM*(*), CDEF*(*)
      LOGICAL   QANS, QEOF
C---
C XPARSE subroutine to return a logical true or false depending on the
C answer to a prompted question
C---
C CPROM   I    The prompting question
C CDEF    I    The default character answer (if blank, then the
C               default depends on the input value of qans
C               i.e. the default will be to leave qans uchanged
C QANS    I/O  On return, true if yes, false if no (input
C               value used only if CDEF is a blank character)
C QEOF      O  TRUE if EOF condition raised.
C---
C 1985-Oct-11 - rashafer
C---
      INTEGER    IDEF
      character(1) CHOICE(3)
      character(72) CHOSE
      DATA CHOICE/'Y','N','*'/
C---
      IF(CDEF.EQ.' ') THEN
         IF(QANS) THEN
            IDEF=1
         ELSE
            IDEF=2
         END IF
      ELSE
         CHOICE(3)=CDEF
         IDEF=3
      END IF
      CALL XCHOSE(CPROM,CHOICE,2,CHOSE,IDEF,QEOF)
      QANS=IDEF.EQ.1
      RETURN
      END
