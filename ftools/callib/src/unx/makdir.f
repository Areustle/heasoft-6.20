*+MAKDIR
	subroutine makdir(dir)

	implicit none
	character*(*) dir

C-----------------------------------------------------------------------
C Description: Creates the directory specified by the argument dir. All
C              intermediate directories will be created if necessary.
C
C Arguments:   dir  (i): the directory to be created
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar Sep 14 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	character(160) cmd
	integer fcstln,errstat
	logical exist

	call direx(dir,exist)

	if (exist) return

	cmd = 'mkdir -p '//dir(:fcstln(dir))

	call cspawn(cmd,fcstln(cmd),errstat)

	end
