	subroutine xgenrd(prompt,string,iparse,ierr,*,*)
c		rashafer 31 Oct 1986
c	An XPARSE general read routine, that on its FIRST invocation will
c	do an XINIRD and on all others an XNXTRD.
	character*(*)	prompt	
c I: Prompt string.
	character*(*)	string	
c I/R: Read string.
	integer*4	iparse	
c I/R: Parse position.
	integer*4	ierr	
c I/R: Error flag (0= success)
c
c	Returns:
c		*1:	EOF
c		*2:	Other I/O error
	logical*4 qfirst
	data qfirst/.true./
	if(qfirst)then
	    call xinird(prompt,string,iparse)
	    if(iparse.lt.0)then
		ierr=-1
		end if
	    qfirst=.false.
	else
	    call xnxtrd(prompt,string,iparse,ierr)
	    end if
	if(ierr.lt.0)then
	    return 1
	elseif(ierr.eq.0)then
	    return
	else
	    return 2
	    end if
	end
