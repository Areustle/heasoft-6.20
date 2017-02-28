**==ISPRINT.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- isprint - identify printable character
* 12 April 1992 : AMTP
      LOGICAL*4 FUNCTION ISPRINT(C)
      character(1) C
*-
      ISPRINT = (C.GE.' ')
      RETURN
      END
