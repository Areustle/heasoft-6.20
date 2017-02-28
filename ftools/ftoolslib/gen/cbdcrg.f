*+CBDCRG
	subroutine cbdcrg(str,minval,maxval,result,status)

	implicit none
	character(80) str
	real minval,maxval
	logical result
	integer status

C-----------------------------------------------------------------------
C Description: Compares a range of values with the value descriptors in 
C              the CBD string.  A result of .true. is returned if any
C              value in the range is contained in the CBD string.
C              Otherwise a result of .false. is reutrned.  If the CBD 
C              string is unable to be parsed, then a non-zero status is 
C              returned.
C
C Arguments:   str     (i): The CBD string
C              minval  (i): The smallest value in the range of values to
C                           be compared to the value descriptors in the
C                           CBD string
C              maxval  (i): The largeest value in the range of values to
C                           be compared to the value descriptors in the
C                           CBD string
C              result  (r): Flag describing if any value in the range is
C                           contained in the CBD string
C              status  (r): Success flag for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 31, 1994 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

        integer n,type,errstat
	real smval,lgval,value

C       initialize the pointer, status flag, and result flag
        status = 0
        n = 0
        result = .false.

C       Top of the comparison loop
100     continue

C       Move the pointer to the next value descriptor
        call cbdmp(str,n,errstat)
        if ( (errstat .ne. 0) .and. (errstat .ne. 2) ) then
                status = 1
                return
        endif

C       if errstat = 2 then finished examining the string
        if (errstat .eq. 2) return

C       Determine the type of value descriptor
        call cbdtyp(str,n,type,errstat)
        if (errstat .ne. 0) then
                status = 2
                return
        endif

C	if string, then ignore.  if range of values or if single
C       real value then compare

	if (type .eq. 2) then
		call cbdgrg(str,n,smval,lgval,errstat)

		if (errstat .ne. 0) then
			status = 3
			return
		endif

		if( ((smval .ge. minval) .or. (lgval .ge. minval)) .and.
     &              ((smval .le. maxval) .or. (lgval .le. maxval)) )then
			result = .true.
			return
		endif

	else if (type .eq. 3) then
		call cbdgr(str,n,value,errstat)

		if (errstat .ne. 0) then
			status = 3
			return
		endif

		if((value .ge. minval) .and. (value .le. maxval)) then
			result = .true.
			return
		endif
	endif

C	Go back to top of search loop
	goto 100

	end
