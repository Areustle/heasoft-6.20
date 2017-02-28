**==ystclq1.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Removes leading and trailing " in a string
 
      SUBROUTINE YSTCLQ1(Instr)
 
      IMPLICIT NONE
 
      CHARACTER*(*) Instr
 
      INTEGER*4 i
 
      i = 1
 
      IF ( LEN(Instr).EQ.1 ) RETURN
      IF ( Instr.EQ.' ' ) RETURN
 
      CALL YSTCLS1(Instr)
      IF ( Instr(1:1).EQ.'"' ) Instr = Instr(2:LEN(Instr))
      i = LEN(Instr)
      DO WHILE ( Instr(i:i).EQ.' ' .AND. i.GT.1 )
         i = i - 1
      ENDDO
 
      IF ( Instr(i:i).EQ.'"' ) Instr(i:i) = ' '
 
      RETURN
      END
**==ystclsq1.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Removes leading and trailing ' in a string
 
      SUBROUTINE YSTCLSQ1(Instr)
 
      IMPLICIT NONE
 
      CHARACTER*(*) Instr
 
      INTEGER*4 i
 
      i = 1
 
      IF ( LEN(Instr).EQ.1 ) RETURN
      IF ( Instr.EQ.' ' ) RETURN
 
      CALL YSTCLS1(Instr)
      IF ( Instr(1:1).EQ.'''' ) Instr = Instr(2:LEN(Instr))
      i = LEN(Instr)
      DO WHILE ( Instr(i:i).EQ.' ' .AND. i.GT.1 )
         i = i - 1
      ENDDO
 
      IF ( Instr(i:i).EQ.'''' ) Instr(i:i) = ' '
 
      RETURN
      END
**==ystcln.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
      SUBROUTINE YSTCLN(Instr)
 
* remove the nulls from a string
 
      CHARACTER*(*) Instr
      INTEGER i
 
      DO 100 i = 1 , LEN(Instr)
         IF ( ICHAR(Instr(i:i)).EQ.0 ) Instr(i:i) = ' '
 100  CONTINUE
 
      RETURN
      END
