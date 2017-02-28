	subroutine xcheck(string, iparse, ierr)
c
c		rashafer 29 April 1987
c	XPARSE subroutine to check a line for any of the special characters
c	and then act on them if they are:
c		COMMAND (default #) Pass on to the XPARSE routine
c		OPSYS (default $) Pass on to the XOPSYS routine
c
	character*(*) string	
c  i/r: The string to be parsed
	integer*4 iparse	
c  i/r: The parse position (moved to point to the position before the next 
c        non-blank char if not one of the special charcters: 
	integer*4 ierr		
c  r: Status flag: 0 - No special char found
c		   1 = A special char found and	successfully handled (should
c 		       probably initiate a re-read
c 		  -1 = An EOF was generated somewhere along the way
c 	       Other = Some error condition
c
	include 'xparinc.inc'
c
	integer*4 lenstr
	logical*4 xchkbl, qdone

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')

	lenstr = len(string)
        qdone = (iparse.ge.lenstr)
	do while(.not.qdone)
           if(xchkbl(string(iparse+1:iparse+1)).and.
     &                       (iparse.lt.lenstr))then
              iparse=iparse+1
           else
              qdone  = .true.
           endif
           if(iparse.ge.lenstr)qdone  = .true.
        end do
	ierr=0
	if(iparse.lt.lenstr)then
	    if(string(iparse+1:iparse+1) .eq. command) then
		iparse=iparse+1
		call xcomnd(string,iparse,ierr)
		if(ierr.eq.1)then
		    ierr=2
		elseif(ierr.eq.0)then
		    ierr=1
		    end if
	    elseif(string(iparse+1:iparse+1) .eq. opsys) then
		iparse=iparse+1
		call xopsys(string,iparse)
		ierr=1
		end if
	    end if
	return
	end
