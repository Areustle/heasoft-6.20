
C******************************************************************************
C SUBROUTINE:
C      fitsdstk
C
C DESCRIPTION:
C      This subroutine dumps the FITSIO Error Stack
C
C AUTHOR/DATE:
C      Kent Blackburn  6/21/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call fitsdstk()
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C      msgstk - error message string
C
C CALLED ROUTINES:
C      subroutine umsput - echo message to terminal
C
C******************************************************************************
      subroutine fitsdstk()

      integer dest,prio,irafsts
      character(80) msgstk

C     Report contents of the FITSIO error message stack
      msgstk = ' '
      dest = 2
      prio = 0
      irafsts = 0
c      call umsput(msgstk,dest,prio,irafsts)
c write to logfile and STDERR
      call fxwrite (msgstk, 6)
      msgstk = ' ***** FITSIO Error Stack Dump ***** '
      call fcerr(msgstk)
      msgstk = ' '

 10   call ftgmsg(msgstk)
c      call umsput(msgstk,dest,prio,irafsts)
c write to logfile and STDERR
      if (msgstk .eq. ' ') goto 99
      call fxwrite (msgstk, 6)
      goto 10

 99   return
      end
