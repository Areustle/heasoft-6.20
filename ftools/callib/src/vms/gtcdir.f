*+GTCDIR
	subroutine gtcdir(dir)

	character*(*) dir

C-----------------------------------------------------------------------
C Description: Returns the current directory in the form "/caldb/..." or
C              "CALDB:[...]".  Valid only for processes running within 
C              the calibration database.
C
C Arguments:   dir (r) : The curdir within the Caldb
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C              Ron Zellar Sept 29, 1993 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

        call gtdir(dir)

	return
	end
