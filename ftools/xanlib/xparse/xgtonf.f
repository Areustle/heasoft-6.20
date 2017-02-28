	logical*4 function xgtonf(string, iparse, type, default, ierr)
c		rashafer 30 April 1987
c	XPARSE subroutine to look for the string ON or OFF on the parse string
c
c	XGTONF	returns TRUE if the next string is "On"
c		returns FALSE if the next string is "OFF"
	character*(*) string
	integer*4 iparse
	character*(*) type	
c  i: A descriptor string
	logical*1 default	
c  i: the value returned if skipped over or
				
c 	run off the end
	integer*4 ierr		
c  r: status -- 0 = success
				
c 		-1 = EOF
c
	character(3) opt(2)
	integer*4 iopt, idelim
	data opt/'on','off'/
	if(default)then
	    iopt=1
	else
	    iopt=2
	    end if
	call xgtmch(string,iparse,opt,2,type,iopt,ierr,idelim)
	xgtonf = iopt.eq.1
	return
	end
