
C******************************************************************************
C FUNCTION:
C      gtaskn
C
C DESCRIPTION:
C      Method to get the name of the current task
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
C      name - string - output - name of current task
C
C CALLED ROUTINES:
C      function fcstln(name) - returns position of last non-space in name
C
C******************************************************************************

      subroutine gtaskn(name)
      implicit none

      character(40) name
      character(40) tmpname
      character(40) taskname
      common /task/ taskname
      integer fcstln
      integer truelen

      tmpname = taskname
      truelen = fcstln(tmpname)
      name = tmpname(1:truelen)

      return
      end
C******CVS Log info***********************************************************
C
C     $Log: gtaskn.f,v $
C     Revision 3.3  2013/05/21 19:08:18  irby
C     Change character*n to character(n) to silence warnings: "Obsolescent
C     feature: Old-style character length".
C
C     Revision 3.2  1999/11/03 19:11:33  elwin
C     Don't pass taskname to function to avoid wierd Common/Argument variable
C     conflicts.  Do little dance with fcstln, probably,to avoid problems with
C     "C string" behavior in some compilers where if taskname wasn't initialized,
C     there was no \0 in the first 41 bytes of taskname, causing segfaults
C     "upstream" (or so we infer).
C
