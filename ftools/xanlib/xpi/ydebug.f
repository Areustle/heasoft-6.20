**==ydebug.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
* ydebug turns on or off debug
*
      SUBROUTINE YDEBUG(Deb)
 
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
      LOGICAL*4 Deb
 
      DEBug = Deb
*      IF ( Sval(1).NE.' ' ) CALL TOGDEB()
 
      RETURN
      END
