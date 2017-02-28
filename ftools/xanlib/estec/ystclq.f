**==YSTCLQ.spg  processed by SPAG 3.09I  at 10:58 on  6 Aug 1992
* Removes leading and trailing " in a string
 
      SUBROUTINE YSTCLQ(Instr)
 
      IMPLICIT NONE
 
      CHARACTER*(*) Instr
 
      INTEGER*4 i
 
      i = 1
 
      IF ( LEN(Instr).EQ.1 ) RETURN
      IF ( Instr.EQ.' ' ) RETURN
 
      CALL YSTCLS(Instr)
      IF ( Instr(1:1).EQ.'"' ) Instr = Instr(2:LEN(Instr))
      i = LEN(Instr)
      DO WHILE ( Instr(i:i).EQ.' ' .AND. i.GT.1 )
         i = i - 1
      ENDDO
 
      IF ( Instr(i:i).EQ.'"' ) Instr(i:i) = ' '
 
      RETURN
      END
