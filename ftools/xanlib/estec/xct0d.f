**==XCT0D.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
*- xct0d - exact real*8 match
      LOGICAL*4 FUNCTION XCT0D(Value,N,List)
* Description :
*  checks if a value appears in a list
* History :
*  26 November 1990 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
 
* Import :
      REAL*8 Value
      INTEGER*4 N
      REAL*8 List(*)
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
 
      XCT0D = there
 
      RETURN
 
      END
