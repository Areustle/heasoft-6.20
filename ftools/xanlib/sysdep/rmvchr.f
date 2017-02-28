      SUBROUTINE RMVCHR(Name,char)
c remove a character or set of characters specified by char 
c and do not pad with blanks
c based on rmvblk
c
*     author Nick White
*     9/15/92 
*
      CHARACTER*(*) Name, char
      INTEGER*4 jj , i , k , LENACT , out , newlen
*
      jj = LENACT(Name)
      i = 1
      out = 0
      IF ( Name.EQ.char) RETURN
*
      DO 100 k = 1 , jj
         IF ( Name(k:k).NE.char ) THEN
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
