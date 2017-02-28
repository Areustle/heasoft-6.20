**==XCT0C.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
*- xct0c - exact string match
      LOGICAL FUNCTION XCT0C(Value,N,List)
* Description :
*  checks if a string appears in a list
* History :
*  15 February 1990 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)
 
* Import :
*  value - ..to be checked
      CHARACTER*(*) Value
*  n - length of the list
      INTEGER N
*  list(*) - ..of answers to be checked
      CHARACTER*(*) List(*)
 
* Local variables :
      LOGICAL there
      INTEGER j
*-
      there = .FALSE.
      j = 0
      DO WHILE ( .NOT.there .AND. (j.LT.N) )
         j = j + 1
         there = (Value.EQ.List(j))
      ENDDO
 
      XCT0C = there
 
      RETURN
 
      END
