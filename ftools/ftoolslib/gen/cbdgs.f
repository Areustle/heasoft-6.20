*+CBDGS
	subroutine cbdgs(str,n,lpos,rpos,status)

	implicit none
	character(80) str
	integer n,lpos,rpos,status
	
C-----------------------------------------------------------------------
C Description: Returns the left most character position value to lpos 
C              and the right most character position value to rpos for 
C              the value descriptor indicated by the pointer n.
C              NOTE: this routine should only be used after the cbdtyp
C              routine has identified the value descriptor as a string
C              (type = 1).
C
C Arguments:   str     (i): The CBD string
C              n       (i): the pointer indicating where the value
C                           descriptor begins
C              lpos    (r): the left most character position of the 
C                           value descriptor
C              rpos    (r): the right most character position of the 
C                           value descriptor
C              status  (r): The success status for this routine
C-----------------------------------------------------------------------
*- Version 1.0

	integer m,index

C       initialize the status flag
        status = 0

C       find the end of the value descriptor
        m = index(str(n+1:),',') + n

        if (m .eq. n) m = index(str(n+1:),')') + n
        if (m .eq. n) then
                status = 1
                return
        endif

        m = m - 1

C	Don't include the double quotes in the string if they're there
	if (str(n:n) .eq. '"') then
		lpos = n + 1
	else
		lpos = n
	endif

	if (str(m:m) .eq. '"') then
		rpos = m - 1
	else
		rpos = m
	endif


	return
	end
