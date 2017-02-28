C******************************************************************************
C SUBROUTINE:
C      fcecho
C
C DESCRIPTION:
C      This subroutine provides a single point to send text to the
C      terminal. This routine should be modified when a new host
C      environment is used.
C
C AUTHOR/DATE:
C      Kent Blackburn  11/5/91
C
C MODIFICATION HISTORY:
C
C  11/28/94 EAG call fxwrite, which in turn calls umsput
C
C NOTES:
C      fcecho uses F77/VOS like calls for terminal I/O
C
C USAGE:
C      call fcecho(string)
C
C ARGUMENTS:
C      string - text string sent to terminal
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine umsput - put message
C
C******************************************************************************
      subroutine fcecho(string)

      character*(*) string
      integer dest,prio,irafsts

      dest = 1
      prio = 0
      irafsts = 0

c write to STDOUT and logfile
      call fxwrite (string, 5)
c      call logstr(string)
c      call umsput(string,dest,prio,irafsts)
      return
      end
