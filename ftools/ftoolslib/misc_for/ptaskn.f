
C******************************************************************************
C FUNCTION:
C      ptaskn
C
C DESCRIPTION:
C      Method to set the name of the current task
C
C AUTHOR/DATE:
C      James Peachey, HEASARC/GSFC/NASA, Hughes STX, 6 October, 1997
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C ARGUMENTS:
C      name - string - input - name of current task
C
C CALLED ROUTINES:
C      function fcstln(name) - returns position of last non-space in name
C
C******************************************************************************

      block data ptasknblock
      character(40) taskname
      common /task/ taskname
      end

      subroutine ptaskn(name)
      implicit none

      character*(*) name
      character(40) taskname
      common /task/ taskname
      integer fcstln
      integer truelen

      truelen = fcstln(name)
      taskname = name(1:truelen)

      return
      end
