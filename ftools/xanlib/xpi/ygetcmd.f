**==ygetcmd.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* returns the command captured by the yyacc/lex code
*
*
      SUBROUTINE YGETCMD(Str)
 
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(*) Str
 
      Str = SCOm
      RETURN
      END
