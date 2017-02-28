

C******************************************************************************
C SUBROUTINE:
C      fcerr
C
C DESCRIPTION:
C      This subroutine provides a single point to send text to the
C      stderr. This routine should be modified when a new host
C      environment is used.
C
C AUTHOR/DATE:
C      Kent Blackburn  2/18/93
C
C MODIFICATION HISTORY:
C
C 11/28/94 EAG changed to call fxwrite (which calls umsput)
C
C NOTES:
C      fcerr uses F77/VOS like calls for terminal I/O
C
C USAGE:
C      call fcerr(string)
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
      subroutine fcerr(string)

      character*(*) string
      integer dest,prio,irafsts
      character(40) taskname
c      common /task/ taskname
      character(1024) buffer1,buffer2
      integer i, fcstln

      dest = 2
      prio = 0
      irafsts = 0
      call gtaskn(taskname)
      buffer1 = taskname
      i = fcstln(buffer1)
      buffer2 = buffer1(1:i)//' : '//string

c write to STDERR and logfile
      call fxwrite (buffer2, 6)
c      call logstr(buffer2)
c      call umsput(buffer2,dest,prio,irafsts)
      return
      end
