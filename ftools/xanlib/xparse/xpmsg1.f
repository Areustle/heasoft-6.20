	subroutine xpmsg1 (descr, value)
c		rashafer 14 april 1986
c	XPARSE message subroutine, to place an information message in
c	the XPRMSG string (in the common block)
	character*(*) descr, value
	include 'xparinc.inc'
	integer*4 lenact
	xprmsg = descr(:lenact(descr)) // ': (' //
     &	 value(:lenact(value)) // ')'
	return
	end
