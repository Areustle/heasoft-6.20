C******************************************************************************
C FUNCTION:
C      fzext
C
C DESCRIPTION:
C      return 1 if logical is .TRUE.
C      return 0 if logical is .FALSE.
C
C AUTHOR/DATE:
C      Mike Tripicco 05 July 1996
C
C MODIFICATION HISTORY:
C
C USAGE:
C      
C ARGUMENTS:
C      lval - logical value
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
        integer function fzext(lval)
        implicit none
        logical lval

        if ( lval .eqv. .TRUE.) then
           fzext=1
        else
           fzext=0
        endif
        end
