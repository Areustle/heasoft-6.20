**==NOSTAR.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
c  Character function to remove any stars from within the string
 
      FUNCTION NOSTAR(String)
      CHARACTER*(*) NOSTAR , String
 
      INTEGER i , j , len , LENACT
 
      len = LENACT(String)
      j = 1
      NOSTAR = ' '
      DO 100 i = 1 , len
         IF ( String(i:i).NE.'*' ) THEN
            NOSTAR(j:j) = String(i:i)
            j = j + 1
         ENDIF
 100  CONTINUE
 
      END
