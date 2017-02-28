**==XCT0K.spg  processed by SPAG 3.09I  at 09:48 on 20 Aug 1992
*- xct0k - exact binary-coded string match
      LOGICAL*4 FUNCTION XCT0K(Size,Value,N,List)
* Description :
*  checks if a value appears in a list
* History :
*  09 July 1991 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
 
* Import :
      INTEGER*4 Size
      character(1) Value(*)
      INTEGER*4 N
      character(1) List(*)
* Local variables :
      LOGICAL*4 there
      INTEGER*4 i , j , k
*-
      there = .FALSE.
      j = 0
      DO WHILE ( .NOT.there .AND. (j.LT.N) )
         j = j + 1
         i = 1
         k = (j-1)*Size + 1
         there = (Value(i).EQ.List(k))
         DO WHILE ( there .AND. (i.LT.Size) )
            i = i + 1
            k = k + 1
            there = (Value(i).EQ.List(k))
         ENDDO
      ENDDO
 
      XCT0K = there
 
      RETURN
 
      END
