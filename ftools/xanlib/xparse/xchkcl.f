      LOGICAL FUNCTION XCHKCL(STRING)
      CHARACTER*(*) STRING
C---
C Function to check if a character is an closing parenthesis
C---
C XCHKCL  O    If true, then char is a closing parenthesis
C STRING  I    The first character is the one to be checked
C---
C 1992-June-24 - Andy Pollock
C---
      INCLUDE 'xparinc.inc'
C---
      XCHKCL=(TESTPR.AND.(STRING(1:1).EQ.CLSPR))
      RETURN
      END
