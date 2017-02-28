*+CBDGRG
	subroutine cbdgrg(str,n,minval,maxval,status)

	implicit none
	character(80) str
	integer n, status
	real maxval,minval

C-----------------------------------------------------------------------
C Description: Returns the value descriptor in str beginning at the 
C              position n to the variables minval and maxval.  NOTE this
C              routine should be used after the value descriptor has 
C              been identified using cbdtyp as type = 2 
C              (range of values).
C
C Arguments:   str     (i): The CBD string
C              n       (i): the pointer indicating where the value
C                           descriptor begins
C              minval  (r): The minimum value in the range of values as
C                           specified by the value descriptor
C              maxval  (r): The maximum value in the range of values as
C                           specified by the value descriptor
C              status  (r): The success status for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 28, 1994 -- Original Version
C              P. Wilson  May 17, 1999 -- Check for ':' as range separator
C-----------------------------------------------------------------------
*- Version 1.1

	integer m,index,sep

C	initialize the status flag
	status = 0

C       find the end of the value descriptor
        m = index(str(n+1:),',') + n

        if (m .eq. n) m = index(str(n+1:),')') + n
        if (m .eq. n) then
                status = 1
                return
        endif

        m = m - 1

C	find the character separating the values
	sep = index(str(n+1:m),':')
	
	if (sep .eq. 0) then
C          Couldn't find a colon.  Try older syntax which used '-'.
	   sep = index(str(n+1:m),'-')
	   if (sep .eq. 0) then
	      status = 2
	      return
	   endif
	endif

	sep = sep + n

C	convert the minimum and maximum range values to real numbers
	call ftc2rr(str(n:sep-1),minval,status)
	call ftc2rr(str(sep+1:m),maxval,status)

	return
	end

