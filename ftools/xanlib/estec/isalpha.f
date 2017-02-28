**==ISALPHA.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- isalpha - identify lower or upper case character
* 5 February 1992 : AMTP
      LOGICAL*4 FUNCTION ISALPHA(C)
      character(1) C
* External references :
      LOGICAL*4 ISLOWER
      LOGICAL*4 ISUPPER
*-
      ISALPHA = (ISLOWER(C) .OR. ISUPPER(C))
      RETURN
      END
