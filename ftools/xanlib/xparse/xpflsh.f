	subroutine xpflsh (string, iparse, ierr, *)
c		XPFLSH    rashafer Feb 27 86
c	XPARSE subroutine to `flush' out the parse string (skipping over
c	any continuation cards developed)
c	string	c*	i/r: parse string
c	iparse	i4	i/r: parse posistion (on return equal to the
c			length of the string)
c	ierr	i4	r: error flag.  0 - success
c			-1 = EOF during the reading of a continuation.
c	Alternate returns:
c		*1	EOF (ierr = -1)
	character*(*) string
	integer*4 iparse,ierr
c
	integer*4 ibeg,iend,iflag,idelim,lenstr
	logical*4 qskip
c
	lenstr = len(string)
	do while(iparse.lt.lenstr)
		call xgtarg(string,iparse,ibeg,iend,qskip,iflag,idelim)
c		** the end has come if an infinite skip or an EOF under
c		** a continuation processing is reached
		if(iflag.eq.2)iparse=lenstr
		end do
	if(iflag.eq.-1)then
		ierr=-1
	else
		ierr= 0
		return
		end if
	return
	end
