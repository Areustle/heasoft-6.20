

C*****************************************************************************
C FTOOLS TASK:
C      jnt2str
C
C FILE:
C      jnt2str.f
C
C DESCRIPTION:
C      Convert integer times to strings
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       November 1, 1993
C
C MODIFICATION HISTORY:
C      Jeff Guerber, Raytheon STX, 1998-06-27.  Return date in yyyy-mm-dd
C          format.  Why call fttm2s instead of just writing it this way?
C          `Second' was written f14.8, but new standard requires leading
C          0s for values less than 10.
C
C NOTES:
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      fttm2s (cfitsio)
C
C******************************************************************************
      subroutine jnt2str (year, month, day, hour, minute, second,
     &     date, time, status)

      implicit none
      character*(*) date, time
      integer status
      integer day, month, year, hour, minute
      double precision second

      character(70)  dt

C     Convert to string of form "yyyy-mm-ddThh:mm:ss.dddddddd"
C     then take appropriate pieces

      call fttm2s( year, month, day, hour, minute, second, 8,
     &    dt, status )
      date = dt(1:10)
      time = dt(12:)

C  remove blanks from strings (prob. not necessary anymore)
      call frmblk (time)
      call frmblk (date)

 999  return
      end
