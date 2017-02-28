*+CBDTYP
	subroutine cbdtyp(str,n,type,status)

	implicit none

	character*(*) str
	integer n,type,status

C-----------------------------------------------------------------------
C Description: Returns the variable type which describes whether the 
C              value indexed by the pointer n is a string value (=1), a 
C              range of values (=2), or a real value (=3).
C
C Arguments:   str     (i): string containing values in the CBDnXXXX
C                           format
C              n       (i): indicates the character number in str where 
C                           the value descriptor begins (e.g the second 
C                           descriptor begins at position 15 in 
C                           'SILLY(1.0-2.0,"HELLO")'. ) 
C              type    (r): the type of value for the descriptor pointed
C                           at by n
C              status  (r): the success value for this routine. Not = 0
C                           indicates unable to parse descriptor
C
C Origin:      written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 27, 1994 -- Original version
C              P. Wilson  May 17, 1999 -- Check for ':' as range separator
C-----------------------------------------------------------------------
*- Version 1.1

	integer index,m,sep

C	initialize varaiables
	status = 0
	type = 0
	m = 0

C	find the end of the value descriptor
	m = index(str(n+1:),',') + n

	if (m .eq. n) then
		m = index(str(n+1:),')') + n
		if (m .eq. n) then
			status = 1
			return
		endif
	endif

	m = m - 1


C	See if the value descriptor is a string
	if (str(n:n) .eq. '"') then
		if (str(m:m) .ne. '"') then
			status = 1
			return
		endif
		type = 1
		return
	endif

	if( (str(n:n).ne.'1') .and. (str(n:n).ne.'2') .and. 
     &	    (str(n:n).ne.'3') .and. (str(n:n).ne.'4') .and. 
     &	    (str(n:n).ne.'5') .and. (str(n:n).ne.'6') .and.
     &	    (str(n:n).ne.'7') .and. (str(n:n).ne.'8') .and. 
     &	    (str(n:n).ne.'9') .and. (str(n:n).ne.'0') .and.
     &	    (str(n:n).ne.'-') .and. (str(n:n).ne.'.') ) then
		type = 1
		return
	endif


C	See if the value descriptor is a range of values
C       Check first for a colon within the string

	sep = index(str(n:m),':')

	if (sep .eq. 0 .and. n .lt. m) then

C          Failed. Now check for the older syntax of a dash separator
C          Skip first character which, if a dash, indicates a negative number
	   sep = index(str(n+1:m),'-')

	endif

	if (sep .ne. 0) then
		type = 2
		return
	endif

C	The value descriptor must be a real value

	type = 3
	return

	end
