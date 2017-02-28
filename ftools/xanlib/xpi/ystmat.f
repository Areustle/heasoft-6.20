**==ystmat.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* match of first part of string
      FUNCTION YSTMAT(Str1,Str2)
 
      IMPLICIT NONE
 
      LOGICAL YSTMAT
      CHARACTER*(*) Str1 , Str2
      INTEGER LENACT
 
      YSTMAT = .FALSE.
 
      IF ( LENACT(Str1).LE.LENACT(Str2) ) THEN
         IF ( Str2(1:LENACT(Str1)).EQ.Str1 ) YSTMAT = .TRUE.
      ELSE
         IF ( Str1(1:LENACT(Str2)).EQ.Str2 ) YSTMAT = .TRUE.
      ENDIF
 
      RETURN
      END
