**==tblfpr.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      FUNCTION TBLFPR(Parname)
 
* this finds the index for a parameter name
 
      IMPLICIT NONE
 
      INTEGER*4 TBLFPR
      CHARACTER*(*) Parname
 
      INCLUDE 'tbl.inc'
 
      character(80) p1
      character(80) p2
      INTEGER*4 i
 
      p1 = Parname
      DO 100 i = 1 , LEN(p1)
         IF ( ICHAR(p1(i:i)).EQ.0 ) p1(i:i) = ' '
 100  CONTINUE
      CALL UPC(p1)
 
      DO 200 i = 1 , TBLpcnt
         p2 = TBLpname(i)
         CALL UPC(p2)
         IF ( p1.EQ.p2 ) THEN
            TBLFPR = i
            RETURN
         ENDIF
 200  CONTINUE
      TBLFPR = 0
      RETURN
      END
