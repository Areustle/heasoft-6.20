**==ycapline.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Captures the input line
*
      SUBROUTINE YCAPLINE(Line)
*
*
      IMPLICIT NONE
      CHARACTER*(*) Line
      INTEGER*4 LENACT
 
      INCLUDE 'yaccfor.inc'
 
 
      SLIne = Line(1:LENACT(Line)-1)
 
      RETURN
      END
 
