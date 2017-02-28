*+CKCBD
	subroutine ckcbd(cbdval,result)

	implicit none
	character*(*) cbdval(9)
	logical result

C-----------------------------------------------------------------------
C Description: Attempts to read the CBD strings given in cbdval.  If the
C              string formats are unreadable, result is returned as
C              false.
C
C Arguments:   cbdval  (i) : the array containg the CBD strings
C              result  (r) : indicates whether the strings are readable
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar May 25, 1994 -- Original Version
C              Mike Corcoran Apr 17 2006 -- changed call to cbdcom in 
C                               line 40
C-----------------------------------------------------------------------
*- Version 1.0

	integer errstat, i
	logical pres,vres,ures

        errstat = 0

C	Initialize the result flag
	result = .false.

	if (cbdval(1) .eq. 'NONE') then
	     result = .true.
	     return
	endif

	Do 500 i=1,9
	     if (cbdval(i) .eq. 'NONE') goto 501
	     call cbdcom(cbdval(i),cbdval(i),pres,vres,ures,errstat)
	     if (errstat .ne. 0) return
500	continue
501	continue

	result = .true.

	return
	end
