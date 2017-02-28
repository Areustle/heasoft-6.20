      LOGICAL FUNCTION XCHKBL(STRING)
      CHARACTER*(*) STRING
C---
C Function to check if a character is one of the xparse blank
C characters (blank or horizontal tab).
C---
C XCHKBL    O  If true, then char is a delimeter
C STRING  I    The first character is the one to be checked
C---
C 1985-Oct-22 - rashafer
C---
      INCLUDE 'xparinc.inc'
C---
      XCHKBL=(STRING(1:1).EQ.BLANK).OR.(STRING(1:1).EQ.TAB)
      RETURN
      END
