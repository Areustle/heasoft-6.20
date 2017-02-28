*+DIREX
	subroutine direx(dir,exist)

	implicit none
	character*(*) dir
	logical exist

C-----------------------------------------------------------------------
C Description: If the directory specified by the dir argument exists
C              the argument exist is returned as true.
C
C Arguments:   dir    (i): the name of the directory to check
C              exist  (r): true if dir exists, false otherwise
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar Sep 14 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	inquire(file=dir,exist=exist)
	return
	end
