**==ISDIGIT.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- isdigit - identify decimal digit character
* 5 February 1992 : AMTP
      LOGICAL*4 FUNCTION ISDIGIT(C)
      character(1) C
*-
      ISDIGIT = ((C.GE.'0') .AND. (C.LE.'9'))
      RETURN
      END
