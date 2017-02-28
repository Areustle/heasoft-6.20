C ***********************************************************
C SUBROUTINE:
C	grab_name
C
C DESCRIPTION:
C	extracts the meaningful name from a CHARACTER string (i.e. strips
C        out blanks)
C
C AUTHOR:
C	from "grab_meaningful_name" in pcarf.f 
C
C MODIFICATION HISTORY:
C
C NOTES:
C 
C USEAGE:
C      CALL grab_name(filfil,ifs,ife)
C
C ARGUMENTS:
C	infile	- name of file from which a_values extracted
C       ifs     - location of initial CHARACTER
C       ife     - location of final non-blank CHARACTER
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C **************************************************************

      subroutine grab_name(filfil,ifs,ife)

      IMPLICIT NONE
      CHARACTER*(*) filfil
      INTEGER ifs,ife,i
      DO i=160,1,-1
         if(filfil(i:i).NE.' ') GOTO 10
      ENDDO
 10   ife=i
      DO i=ife,1,-1
         if(filfil(i:i).eq.'/') GOTO 20
      ENDDO
      ifs=1
      RETURN
 20   ifs=i+1
      RETURN
      END
