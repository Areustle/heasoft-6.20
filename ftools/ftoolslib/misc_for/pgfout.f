
C******************************************************************************
C SUBROUTINE:
C       pgfout
C
C DESCRIPTION:
C       This routine send a message either to a file or to the terminal
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C MODIFICATION HISTORY:
C       Renamed from fprintemp EAG 10/3/94
C       12/9/94 EAG added pgfcount common so count can be set from outside
C
C NOTES:
C
C
C USAGE:
C      call pgfout (ounit, ifout, context, status)
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

      subroutine pgfout (ounit, ifout, context, status)

      integer ounit, status
      logical ifout
      character*(*) context

      integer fcstln, count
      character(1) more

      common /pgfcount/ count
C     The pgfcount common block is initialized to 0 by the pgfini
C     block data program unit

      if ( ifout ) then
         write (ounit, 1001) context(1:fcstln(context))
      else
         call fcecho (context)
         count = count + 1
         if ((ounit .lt. 0) .and. (count .ge. 23)) then
            status = 0
            call uclgst ('more', more, status)
            call ftupch (more)
            if ((more .eq. 'N') .or. (more .eq. 'Q') .or.
     &          (more .eq. 'F')) then
               call uclpst ('more', 'Yes', status)
               status = 1
            endif
            count = 0
         endif

      endif

      return
 1001 format (A)
      end
