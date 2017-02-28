C******************************************************************************
C SUBROUTINE:
C      fgfcal
C
C DESCRIPTION:
C	Takes as input calfil, the name of the desired calibration file in the
C	refdata directory.  Returns infile as the system dependent path to
C	the calibration file in the refdata directory.
C
C AUTHOR/DATE:
C       Ron Zellar  2/17/94 
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call fgfcal(infile,calfil,status)
C
C ARGUMENTS:
C	infile - The system dependent path to the refdata cal file
C	calfil - The name of the requested cal file in the refdata directory
C	status - The success status of this routine
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C	fgrfdr - gets the path to the refdata directory
C
C******************************************************************************
	subroutine fgfcal(infile,calfil,status)

	implicit none
	character*(*) infile,calfil
	integer status, fcstln

C	get the refdata directory
	call fgrfdr(infile,status)
	if (status .ne. 0) return

C	append the requested calibration file to infile
	infile = infile(:fcstln(infile))//calfil(:fcstln(calfil))

	return
	end
