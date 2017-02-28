**==ISALNUM.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- isalnum - identify alphanumeric character
* 5 February 1992 : AMTP
      LOGICAL*4 FUNCTION ISALNUM(C)
      character(1) C
* External references :
      LOGICAL*4 ISALPHA
      LOGICAL*4 ISDIGIT
*-
      ISALNUM = (ISALPHA(C) .OR. ISDIGIT(C))
      RETURN
      END
