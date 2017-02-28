      SUBROUTINE OPENDA(LUN, CDEF, CEXT, LRECL, IOPN, IER)
      CHARACTER CDEF*(*), CEXT*(*)
      INTEGER   LUN, LRECL, IOPN, IER
C---
C This subroutine opens a direct access file on unit LUN.
C---
C LUN     I    Free logical unit number.
C CDEF    I    Default file name.
C CEXT    I    Default extension.
C LRECL   I    Record length in bytes.
C IOPN      O  =1 found default file, =2 User entered filename.
C IER       O  <0 ^Z pressed, =0 file opened, >0 file not opened.
C---
      INTEGER   LENACT
C
      CHARACTER CPROM*64
      CHARACTER CNAM*64
      INTEGER   LNAM
C---
      IER = 0
c
c  Ziqin Pan, June 20,2005
c  Move 100 CONTINUE out of if-then-else-endif block.
c
100   CONTINUE
      IF(CDEF.NE.' '.AND.IER.EQ.0) THEN
         CNAM=CDEF
         IOPN=1
      ELSE
c  100    CPROM = 'Please enter ' // CEXT(:LENACT(CEXT)) // ' file name:'
         CPROM = 'Please enter ' // CEXT(:LENACT(CEXT)) // ' file name:'
         CALL GTBUF(CPROM,IER)
         IF(IER.LT.0) GOTO 900
         CALL GTCHAR(CNAM,LNAM)
         IF(LNAM.EQ.0) THEN
            IOPN=0
            IER=1
            RETURN
         END IF
         IOPN=2
      END IF
C---
      CALL XTEND(CNAM,CEXT)
      CLOSE(UNIT=LUN)
      CALL OPENWR(LUN,CNAM,'OLD','D',' ',LRECL,1,IER)
      IF(IER.NE.0) GOTO 100
      RETURN
C---
  900 IER=-1
      IOPN=0
      RETURN
      END
