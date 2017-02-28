*+CBDNV
	subroutine cbdnv(str,nvals,status)

	implicit none
	character(80) str
	integer nvals,status

C-----------------------------------------------------------------------
C Description: Returns the number of value descriptors contained in an
C              input CBD string.
C
C Arguments:   str     (i): the CBD string value
C              nvals   (i): the number of value descriptors in str
C              status  (i): the success status for this routine
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 27, 1994 -- Original version
C-----------------------------------------------------------------------
*- Version 1.0

	integer index,comma,nxtcma
	logical done

C	initialize the comma variable to the beginning of the value 
C       descriptor string
	comma = index(str,'(')

	if (comma .eq. 0) then
		status = 1
		return
	endif

C	initialize the counter variable
	nvals = 0

C	initialize the loop flag
	done = .false.

100	continue

	nxtcma = index(str(comma+1:),',') + comma

	if (nxtcma .eq. comma) then
		nxtcma = index(str(comma+1:),')') + comma
		if (nxtcma .eq. comma) then
			status = 1
			return
		endif
		done = .true.
	endif

	nvals = nvals + 1

	if (done) then
		return
	else
		comma = nxtcma
		goto 100
	endif

	end
