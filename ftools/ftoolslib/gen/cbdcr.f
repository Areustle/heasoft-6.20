*+CBDCR
	subroutine CBDCR(str,op,value,result,status)

	implicit none
	character*(*) str
	character(2) op
	real value
	logical result
	integer status

C-----------------------------------------------------------------------
C Description: Compares a real value with the value descriptors in the 
C              CBD string as directed by the operator value, op.  The 
C              comparison is accomplished by evaluating the expression
C
C                    cbd_value_desc op value
C
C              where op equals one of 'eq', 'ne', 'gt', 'lt', 'ge', and
C              'le'.  (e.g. passing 'gt' will ask "Is the cbd value 
C              desciptor greater than <value>.")
C
C              A result of .true. is returned if any of the value 
C              descriptors in the cbd string meet the criteria described
C              by the op string.  Otherwise a result of .false. is 
C              reutrned.  If the CBD string is unable to be parsed, then 
C              a non-zero status is returned.
C
C Arguments:   str     (i): The CBD string
C              op      (i): The expression operator
C              value   (i): The value to be compared to the value 
C                           descriptors in the CBD string
C              result  (r): Flag describing if the cbd value descriptors
C                           meet the criteria established by the op value
C              status  (r): Success flag for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 31, 1994 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	integer n,type,errstat
	real strval,minval,maxval

C	initialize the pointer, status flag, and result flag
	status = 0
	n = 0
	result = .false.

C	Top of the comparison loop
100	continue

C	Move the pointer to the next value descriptor
	call cbdmp(str,n,errstat)
	if ( (errstat .ne. 0) .and. (errstat .ne. 2) ) then
	     status = 1
	     return
	endif

C	if errstat = 2 then finished examining the string
	if (errstat .eq. 2) return

C	Determine the type of value descriptor
	call cbdtyp(str,n,type,errstat)
	if (errstat .ne. 0) then
	     status = 2
	     return
	endif

C	If string -> ignore, if range or real -> get
C       values and compare
	if (type .eq. 2) then
	     call cbdgrg(str,n,minval,maxval,errstat)

	     if (errstat .ne. 0) then
	          status = 3
	          return
	     endif

	     if (op .eq. 'eq') then
	          if ((value .ge. minval).and.(value .le. maxval)) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'ne') then
	          if ((value .lt. minval).or.(value .gt. maxval)) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'gt') then
	          if (maxval .gt. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'ge') then
	          if (maxval .ge. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'lt') then
	          if (minval .lt. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'le') then
	          if (minval .le. value) then
	               result = .true.
	               return
	          endif
	     endif

	else if (type .eq. 3) then
	     call cbdgr(str,n,strval,errstat)

	     if (errstat .ne. 0) then
	          status = 3
	          return
	     endif

	     if (op .eq. 'eq') then
	          if (value .eq. strval) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'ne') then
	          if (value .ne. strval) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'gt') then
	          if (strval .gt. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'ge') then
	          if (strval .ge. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'lt') then
	          if (strval .lt. value) then
	               result = .true.
	               return
	          endif
	     else if (op .eq. 'le') then
	          if (strval .le. value) then
	               result = .true.
	               return
	          endif
	     endif
	endif

C	Go back to top of searching loop
	goto 100

	end
