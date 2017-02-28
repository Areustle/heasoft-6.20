      SUBROUTINE XSQUEZ(STRING, RMCHAR, NSAVE, LENGTH)
      CHARACTER STRING*(*)
      CHARACTER RMCHAR*1
      INTEGER   NSAVE, LENGTH
C---
C Subroutine to take a string and xsquez out any blanks
C** This is pretty grungy code.  Surely we can do better than this
C---
C STRING  I/O  String to have chars removed removed
C RMCHAR  I    The character to be removed
C NSAVE   I    All strings of char of length nsave are to be
C              retained.  To remove all occurances nsave <=0
C LENGTH    O  Actual length of xsquezed string
C---
C 1984-Jul-29 - rashafer
C 1985-Mar-08 - to allow arbitrary characters to be removed, and
C               to allow an arbitrary no. of them to be retained.
C---
      INTEGER   IRM, INITL, LENACT, INITB, IVAL, ICOUNT
      LOGICAL   QON
C---
      IRM=ICHAR(RMCHAR(1:1))
      INITL=LENACT(STRING)
      INITB=INDEX(STRING(:INITL),RMCHAR(1:1))
      IF(INITB.EQ.0) THEN
         LENGTH=INITL
      ELSE
         QON=.FALSE.
         LENGTH=INITB-1
         INITB=INITB-1
  100    IF(INITB.LT.INITL) THEN
            INITB=INITB+1
            IVAL=ICHAR(STRING(INITB:INITB))
            IF(IVAL.NE.IRM) THEN
               QON=.FALSE.
               LENGTH=LENGTH+1
               STRING(LENGTH:LENGTH)=CHAR(IVAL)
            ELSE
               IF(.NOT.QON) THEN
                  QON=.TRUE.
                  ICOUNT=1
               ELSE
                  ICOUNT=ICOUNT+1
               END IF
               IF(ICOUNT.LE.NSAVE) THEN
                  LENGTH=LENGTH+1
                  STRING(LENGTH:LENGTH)=RMCHAR(1:1)
               END IF
            END IF
            GOTO 100
         END IF
         IF(LENGTH.LT.INITL) THEN
             STRING(LENGTH+1:INITL)= ' '
         END IF
      END IF
      RETURN
      END
