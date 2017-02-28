
C******************************************************************************
C SUBROUTINE:
C      fprint
C
C DESCRIPTION:
C       This routine send a message either to a file or to the terminal
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C MODIFICATION HISTORY:
C
C NOTES:
C       currently included in the file FSTRUCT.F
C
C USAGE:
C      call fprint (ounit, ifout, context)
C
C ARGUMENTS:
C      ounit    - output unit number
C      ifout    - true if output is to a file
C      context  - message
C
C PRIMARY LOCAL VARIABLES:
C      ounit    - output unit number
C      ifout    - true if output is to a file
C      context  - message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      function fcstln   - index of last non-blank character
C
C******************************************************************************

      subroutine fprint (ounit, ifout, context)

      integer ounit
      logical ifout
      character*(*) context

      integer fcstln

      if ( ifout ) then
         write (ounit, 1001) context(1:fcstln(context))
      else
         call fcecho (context)
      endif

      return
 1001 format (A)
      end
