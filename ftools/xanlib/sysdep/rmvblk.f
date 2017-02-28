      SUBROUTINE RMVBLK(Name)
c
*     author linda osborne
*     12/4/89
* modified 8/6/92 to remove c*400 dummy array - Nick White
*
      CHARACTER*(*) Name
      INTEGER*4 jj , i , k , LENACT , out , newlen
*
      jj = LENACT(Name)
      i = 1
      out = 0
      DO 100 k = 1 , jj
         IF ( Name(k:k).NE.' ' ) THEN
            Name(i:i) = Name(k:k)
            i = i + 1
         ELSE
            out = out + 1
         ENDIF
 100  CONTINUE
c
      newlen = jj - out
      Name(newlen+1:jj) = ' '
      RETURN
      END
