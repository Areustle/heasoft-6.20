*+CBDMP
	subroutine cbdmp(str,n,status)

	implicit none
	character(80) str
	integer n,status

C-----------------------------------------------------------------------
C Description: Increments the pointer n so that it is pointing at the 
C              next value descriptor.  If the pointer is initially set
C              to zero, then the pointer is moved to the first value
C              descriptor.  If the pointer is initially pointing at the 
C              last value descriptor, then it will not be incremented
C              and a non-zero status will be returned.
C
C Arguments:   str     (i):   The CBD string 
C              n       (i/r): pointer incremented to point at the next
C                             value descriptor
C              status  (r):   The success status for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 28, 1994 -- Original version
C-----------------------------------------------------------------------
*- Version 1.0

	integer m

C	initialize the status variable
	status = 0

C	See if I need to look for the first value descriptor
	if (n .eq. 0) then
		m = index(str,'(')
		if (m .eq. 0) then
			status = 1
			return
		endif
C	Otherwise, look for the next value descriptor
	else
		m = index(str(n:),',') + n - 1
		if (m .eq. (n-1)) then
			status = 2
			return
		endif
	endif

	n = m + 1

	return

	end
