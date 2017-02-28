**==XCT0J.spg  processed by SPAG 3.09I  at 09:48 on 20 Aug 1992
*- xct0j - exact integer*2 match
      LOGICAL*4 FUNCTION XCT0J(Value,N,List)
* Description :
*  checks if a value appears in a list
* History :
*  26 November 1990 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
 
* Import :
      INTEGER*2 Value
      INTEGER*4 N
      INTEGER*2 List(*)
* Local variables :
      LOGICAL*4 there
      INTEGER*4 j
*-
      there = .FALSE.
      j = 0
      DO WHILE ( .NOT.there .AND. (j.LT.N) )
         j = j + 1
         there = (Value.EQ.List(j))
      ENDDO
 
      XCT0J = there
 
      RETURN
 
      END
