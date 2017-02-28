**==ycapval.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Captures the command
*
      SUBROUTINE YCAPVAL(Value)
*
*
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(*) Value
 
 
      INTEGER i , ilen
 
 
 
      IF ( NPArs.EQ.0 ) THEN
         NPArs = NPArs + 1
      ELSEIF ( SVAl(NPArs).NE.' ' ) THEN
         NPArs = NPArs + 1
      ENDIF
 
      SVAl(NPArs) = ' '
 
      ilen = LEN(Value)
 
      IF ( Value(1:1).NE.'"' ) THEN
         i = 1
         DO WHILE ( i.LE.LEN(Value) .AND. Value(i:i).NE.' ' )
            i = i + 1
         ENDDO
         ilen = i
      ENDIF
 
      DO 100 i = 1 , ilen
         IF ( ICHAR(Value(i:i)).GE.32 .AND. ICHAR(Value(i:i)).LE.127 )
     &        SVAl(NPArs)(i:i) = Value(i:i)
 100  CONTINUE
 
 
      CALL YSTCLQ1(SVAl(NPArs))
      RETURN
 
      END
