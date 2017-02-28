      LOGICAL FUNCTION XCHKDL(STRING, IVAL)
      CHARACTER*(*) STRING
      INTEGER   IVAL
C---
C Function to check if a character is one of the xparse delimeters.
C IVAL
C 0    = comma
C 1    = commnt - the comment begin char
C 2    = skpall - the terminal skip character
C 3    = spcbr1 - the first 'special break' char
C 4    = spcbr2 - the second 'special break' char
C---
C XCHKDL    O  If true, then char is a delimeter
C STRING  I    The first character is the one to be checked
C IVAL      O  If xchkdl is true, this flag indicates which
C              delimeter was matched.
C---
C 1985-Oct-22 - rashafer
C---
      INCLUDE 'xparinc.inc'
      character(1) CHAR
      INTEGER   IPOINT
C---
      IVAL=-1
      CHAR=STRING(1:1)
      IF(CHAR.EQ.COMMA) THEN
         IVAL=0
      ELSEIF(CHAR.EQ.COMMNT) THEN
         IVAL=1
      ELSEIF(CHAR.EQ.SKPALL) THEN
C** Check if there are no other non-blank characters before the end
C** of line (or the comment char)
         IPOINT=2
         DO WHILE((IPOINT.LE.LEN(STRING)).AND.
     :            (STRING(IPOINT:IPOINT).EQ.BLANK))
            IPOINT=IPOINT+1
         END DO
         IF((IPOINT.GT.LEN(STRING)).OR.
     :      (STRING(IPOINT:IPOINT).EQ.COMMNT)) THEN
             IVAL=2
         END IF
      ELSEIF(CHAR.EQ.SPCBR1) THEN
         IVAL=3
      ELSEIF(CHAR.EQ.SPCBR2) THEN
         IVAL=4
      END IF
      XCHKDL=IVAL.GE.0
      RETURN
      END
