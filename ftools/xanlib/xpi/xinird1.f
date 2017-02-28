**==xinird.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Xparse replacement using yacc/lex
*
 
      SUBROUTINE XINIRD1(Prom,Tstring,Ierr)
 
      IMPLICIT NONE
*
 
 
      CHARACTER*(*) Prom , Tstring
      INTEGER*4 Ierr
      INTEGER*4 zparse
 
      CALL XNXTRD1(Prom,Tstring,zparse,Ierr,*100)
 
 100  RETURN
      END
