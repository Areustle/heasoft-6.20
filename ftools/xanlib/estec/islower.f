**==ISLOWER.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- islower - identify lower case character
* 5 February 1992 : AMTP
      LOGICAL*4 FUNCTION ISLOWER(C)
      character(1) C
*-
      ISLOWER = ((C.GE.'a') .AND. (C.LE.'z'))
      RETURN
      END
