*+CBDGV
	subroutine cbdgv(str,n,type,minval,maxval,lpos,rpos,status)

	implicit none
	character(80) str
	integer n,type,lpos,rpos,status
	real minval,maxval

C-----------------------------------------------------------------------
C Description: Returns the value descriptor in str beginning at the 
C              position n.  
C
C              If the value descriptor is a string value then 
C
C              type = 1 
C              lpos = left most character position of the descriptor 
C              rpos = right most character position of the descriptor 
C
C              If the value descriptor is a range of values then
C
C              type = 2 
C              minval = minimum value in the range 
C              maxval = maximum value in the range
C              lpos = left most character position of the descriptor 
C              rpos = right most character position of the descriptor
C
C              If the value descriptor is a single real value then 
C
C              type will = 3 
C              minval = the value
C              maxval = the value
C              lpos = left most character position of the descriptor 
C              rpos = right most character position of the descriptor
C
C Arguments:   str     (i): the CBD string
C              n       (i): the pointer indicating where the value
C                           descriptor begins
C              type    (r): the type of value contained in the value
C                           descriptor
C              minval  (r): the minimum value in the range of values 
C                           specified by the value descriptor
C              maxval  (r): the maximum value in the range of values
C                           specified by the value descriptor
C              lpos    (r): the left most character position of the 
C                           value descriptor not including any double
C                           quotes.
C              rpos    (r): the right most character position of the 
C                           value descriptor not including any double
C                           quotes.
C              status  (r): the success status for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 28, 1994 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	integer errstat

C	initilalize the status flag to 0
	status = 0

C	Find out what kind of value we're going to get
	call cbdtyp(str,n,type,errstat)
	if (errstat .ne. 0) then
		status = 1
		return
	endif

C	Get lpos and rpos no matter what type it is
	call cbdgs(str,n,lpos,rpos,errstat)
	if (errstat .ne. 0) then
		status = 2
		return
	endif

C	if it's a range of values, retrieve them
	if (type .eq. 2) then
		call cbdgrg(str,n,minval,maxval,errstat)
		if (errstat .ne. 0) then
			status = 3
			return
		endif
	endif

	if (type .eq. 3) then
		call cbdgr(str,n,minval,errstat)
		if (errstat .ne. 0) then
			status = 4
			return
		endif
		maxval = minval
	endif

	return

	end
