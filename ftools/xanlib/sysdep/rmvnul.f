      SUBROUTINE RMVNUL(Name)
c
*     author linda osborne
*     12/4/89
*     9/8/91 renamed from remove_nulls  Nick
c
      CHARACTER*(*) Name
      INTEGER*4 LENACT , lname , i , k
*
      lname = LENACT(Name)
      i = 1
      DO 100 k = 1 , lname
         IF ( Name(k:k).NE.CHAR(0) ) THEN
            Name(i:i) = Name(k:k)
            i = i + 1
         ELSE
            Name(i:i) = ' '
            i = i + 1
         ENDIF
 100  CONTINUE
      RETURN
      END
