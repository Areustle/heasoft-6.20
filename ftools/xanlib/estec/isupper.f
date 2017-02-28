**==ISUPPER.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
*- isupper - identify upper case character
* 5 February 1992 : AMTP
      LOGICAL*4 FUNCTION ISUPPER(C)
      character(1) C
*-
      ISUPPER = ((C.GE.'A') .AND. (C.LE.'Z'))
      RETURN
      END
