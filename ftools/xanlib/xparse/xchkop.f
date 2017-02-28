      LOGICAL FUNCTION XCHKOP(STRING)
      CHARACTER*(*) STRING
C---
C Function to check if a character is an opening parenthesis
C---
C XCHKOP  O    If true, then char is an opening parenthesis
C STRING  I    The first character is the one to be checked
C---
C 1992-Jun-24 - Andy Pollock
C---
      INCLUDE 'xparinc.inc'
C---
      XCHKOP=(TESTPR.AND.(STRING(1:1).EQ.OPNPR))
      RETURN
      END
