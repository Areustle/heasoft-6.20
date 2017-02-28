      SUBROUTINE RMVXBK(String)
c
c Subroutine to remove extra blanks from strings.  Wherever two or more blanks
c are in succession, only one blank will remain.  The end of the string is
c padded with blanks.  Any leading blanks at the beginning of the string or
c null characters within the string will also be removed.  Null characters are
c replaced with blanks.
c
      CHARACTER*(*) String
      INTEGER*2 LENACT , i , j , lstring , newlen , out
c
      lstring = LENACT(String)
      j = 1
      out = 0
      DO 100 i = 1 , lstring
         IF ( String(i:i).EQ.CHAR(0) ) THEN
            String(j:j) = ' '
            j = j + 1
         ELSEIF ( String(i:i).NE.' ' ) THEN
            String(j:j) = String(i:i)
            j = j + 1
         ELSEIF ( (String(i:i).EQ.' ') .AND. (String(i+1:i+1).NE.' ') )
     &            THEN
            String(j:j) = String(i:i)
            j = j + 1
         ELSE
            out = out + 1
         ENDIF
 100  CONTINUE
c
      newlen = lstring - out
      IF ( String(1:1).EQ.' ' ) THEN
         String(1:newlen-1) = String(2:newlen)
         String(newlen:newlen) = ' '
      ENDIF
c
      IF ( out.GT.0 ) THEN
         DO 110 i = newlen+1, lstring
           String(i:i) = ' '
 110     CONTINUE
      ENDIF
c
      RETURN
      END
