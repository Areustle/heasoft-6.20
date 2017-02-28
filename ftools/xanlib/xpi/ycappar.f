**==ycappar.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* Captures a parameter
*
      SUBROUTINE YCAPPAR(Param)
*
*
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
 
      CHARACTER*(*) Param
      INTEGER*4 LENACT
 
      INTEGER i
 
      NPArs = NPArs + 1
      SPArs(NPArs) = ' '
 
      DO 100 i = 1 , LEN(Param)
         IF ( ICHAR(Param(i:i)).GE.32 .AND. ICHAR(Param(i:i)).LE.127 )
     &        SPArs(NPArs)(i:i) = Param(i:i)
 100  CONTINUE
 
 
      i = LENACT(SPArs(NPArs))
      IF ( SPArs(NPArs)(i:i).EQ.'=' ) SPArs(NPArs)(i:i) = ' '
 
 
      RETURN
 
      END
