C****************************************************************************
C SUBROUTINE:
C      timestamp
C
C DESCRIPTION:
C      Write a history date/time stamp in the current extension
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       August, 1994
C
C MODIFICATION HISTORY:
C       Jeff Guerber, RSTX/GSFC, June 1998.  Write timestamp in new Fits style
C                     (yyyy-mm-ddThh:mm:ss).  Get it straight from new
C                     (c)fitsio routine ftgstm, instead of calling gtdati,
C                      gttime, and jnt2str.  Also get task name from gtaskn().
C
C NOTES:
C     If the task common block is populated, the name of the task is
C     included in the history record.  After June 1998, timestamp will be
C     in UTC if available; before, it was whatever system gave, usu. local.
C
C USAGE:
C      call timestamp (unit)
C
C ARGUMENTS:
C      unit - integer - input - unit number of FITS file to write to
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      subroutine timestamp (unit)

      integer unit

      integer fcstln, status, timeref
      character(50) date
      character(80) context
      character(40) taskname

      status = 0
      context = ' '
      call gtaskn( taskname )

C get the current date/time string.
      call ftgstm( date, timeref, status )

C If the taskname common is initialized
      if ((taskname(1:1) .ne. char(0)) .and.
     &     (taskname(1:1) .ne. ' ')) then
         context = taskname(1:fcstln(taskname)) // ' at'
      endif

      context = context(1:fcstln(context)) // ' ' //
     &     date(1:fcstln(date))

      call ftphis (unit, context, status)

 999  return
      end


