


C******************************************************************************
C SUBROUTINE:
C      fcerrm
C
C DESCRIPTION:
C      This subroutine provides an echo of the error status to
C      terminal environment in use.
C
C AUTHOR/DATE:
C      Kent Blackburn  12/5/91
C
C MODIFICATION HISTORY:
C      Peter Wilson    12/11/97: Up'd bounds for error messages
C      Ning Gan        00/04/12: Added Global status.
C
C NOTES:
C
C USAGE:
C      call fcerrm(stat)
C
C ARGUMENTS:
C      stat - fitsio returned error status code
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      message - error message and code
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************
      subroutine fcerrm(stat)

      integer stat
      character(40) context,message

      if ((stat .lt. 1).or.(stat .gt.505)) goto 99
      context = 'Error Status Returned : '
      write(message,1000) context,stat
 1000 format(A24,I4)
      call fcerr(message)
      message = ' '
      call ftgerr(stat,message)
      call fcerr(message)

C     Set the global status
      call setheastatus(stat)

      call fitsdstk()
 99   return
      end
