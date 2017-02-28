**==ystcls1.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Removes leading spaces in a string
 
      SUBROUTINE YSTCLS1(Instr)
 
      IMPLICIT NONE
 
      CHARACTER*(*) Instr
 
      INTEGER*4 i
 
      i = 1
 
      IF ( LEN(Instr).EQ.1 ) RETURN
      IF ( Instr.EQ.' ' ) RETURN
 
      DO WHILE ( Instr(i:i).EQ.' ' .AND. i.LT.LEN(Instr) )
         i = i + 1
      ENDDO
 
      Instr = Instr(i:LEN(Instr))
 
      RETURN
      END
