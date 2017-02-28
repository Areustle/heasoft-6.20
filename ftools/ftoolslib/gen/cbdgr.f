*+CBDGR
	subroutine cbdgr(str,n,value,status)

	implicit none
	character(80) str
	integer n,status
	real value

C-----------------------------------------------------------------------
C Description: Returns the value descriptor in str beginning at the 
C              position n to the variable value.  NOTE this routine
C              should be used after the value descriptor has been 
C              identified using cbdtyp as type = 3 (single real value).
C
C Arguments:   str     (i): the CBD string
C              n       (i): the pointer indicating where the value
C                           descriptor begins
C              value   (r): the value contained in the value descriptor
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 28, 1994 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	integer m,index

C	initilalize the status flag to 0
	status = 0

C	find the end of the value descriptor
	m = index(str(n+1:),',') + n

	if (m .eq. n) m = index(str(n+1:),')') + n
	if (m .eq. n) then
		status = 1
		return
	endif

	m = m - 1

C	convert the string value into a real value
	call ftc2rr(str(n:m),value,status)

	return
	end
