**==YSTCLS.spg  processed by SPAG 3.09I  at 10:58 on  6 Aug 1992
* Removes leading spaces in a string
 
      SUBROUTINE YSTCLS(Instr)
 
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
