

C******************************************************************************
C SUBROUTINE:
C      frmblk
C
C DESCRIPTION:
C      Remove blanks from a string
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       October, 1992
C
C MODIFICATION HISTORY:
C
C NOTES:
C       currently in SEC2TIME.F
C
C USAGE:
C      call frmblk (string)
C
C ARGUMENTS:
C       string (char, i/o) - string from which to remove blanks
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcstln - return index of last non-blank character
C
C******************************************************************************
      subroutine frmblk (string)

      character*(*) string

      integer length, i
      integer fcstln

      length = fcstln(string)
      if (length .le. 0) return

      i = 1
 10   if (string(i:i) .eq. ' ') then
         string(i:length-1) = string(i+1:length)
         string(length:length) = ' '
         length = length - 1
      else
         i = i + 1
      endif
      if (i .ge. length) return
      goto 10

      end
