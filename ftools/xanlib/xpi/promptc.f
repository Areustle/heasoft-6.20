**==promptc.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
* fortran <=> c interface between prompt and lex/yacc for gtcom
*
      SUBROUTINE PROMPTC(Line)
      IMPLICIT NONE
 
 
      character(1000) Line
 
 
 
      INCLUDE 'yaccfor.inc'
 
      INTEGER*4 LENACT
 
 
 
      CALL PROMPT(GTPrompt,LENACT(GTPrompt))
 
      RETURN
      END
