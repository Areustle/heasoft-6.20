	subroutine xnxtrd(prompt, string, iparse, ierr, *, *)
c		XNXTRD - rashafer 27 Feb 1986
c	XPARSE subroutine to flush out the current parse string and
c	read in a new one
c	prompt	c*	i: prompt string
c	string	c*	i/r: parse string
c	iparse	i4	i/r: parse position (on return zero, indicating
c			a fresh position)
c	ierr	i4	r: error flag. 0 - success
c				<0	- EOF
c				>0	- I/O error of some sort on read
c	Alternate returns:
c		*1 - EOF
c		*2 - I/O error
	character*(*) prompt,string
	integer*4 iparse,ierr
c
	include 'xparinc.inc'

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')

c
c **	** come from when XCHECK detected a special input line and thus
c **	** the ordinary command line had to be read in
100	continue
	return_inquiry = .false.  
c  Turn off inquiry mode for remainder
	call xpflsh(string,iparse,ierr)
	if(ierr.ne.0) return 1
	call xcread(prompt,string,ierr)
	return_inquiry = require_inquiry 
c  Re-instate inquiry mode, if selected
       if(ierr.eq.0)then
          iparse=0
          call xcheck(string,iparse,ierr)
          if(ierr.ne.0) goto 100 
c  Try another read
       elseif(ierr.gt.1)then
          return 2
       end if
       if(ierr.lt.0)then
          return 1
       end if
       return
       end
