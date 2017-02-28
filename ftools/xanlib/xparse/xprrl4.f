	subroutine xprrl4(string,iparse,nreq,descr,ndesc,bkreal,
     &	 nret,iflag,idelim)
c
c	fwj haberl 26 Aug 1986
c	XPARSE subroutine to prompt for a requested number of arguements as
c	real*4 numbers.
c
c	iparse	c*	i/r: parse position
c	nreq	i4	i: no. of strings requested
c	desc	c*(ndesc)	i: description of the string requested
c	ndesc	i4	i: no. of descriptions passed
c	bkreal	r4	i: the real values to pass back
c	nret	i4	r: no. of strings actually processed (where skips
c				and infinite skips are included as processed)
c				if nret not = NREQ then there was a fall off
c				the end of the string, or a special delimeter
c				met (see input value of IDELIM)
c	iflag	i4	r: value of condition flag at last call to XGTARG
c	idelim	i4	i/r: if <= -1  there is no checking for special
c			delimeters, else on return IDELIM contains the value
c			of the delimeter that triggered the return (if 0
c			then a comma, if 1 then special del 1, etc.)
c
	character*(*) string,descr(*)
	character(30) tmpstr
	integer*4 iparse,iflag,idelim,ierr,nreq,ndesc,nret
	integer*4 i,lent,lenact,lentmp
	real*4 bkreal(*)
	include 'xparinc.inc'
	xprmsw=' Enter '
	lent=lenact(xprmsw)
	do i=1,ndesc
	    xprmsw(lent+2:)=descr(i)
	    lent=lent+lenact(descr(i))+1
	    write(tmpstr,'(1pg14.6)')bkreal(i)
	    call xsquez(tmpstr,' ',0,lentmp)
	    xprmsw(lent+1:)='('//tmpstr(:lentmp)//'),'
	    lent=lenact(xprmsw)
	    end do
	xprmsw(lent:lent)=':'
	call xnxtrd(xprmsw(:lent),string,iparse,ierr)
	call xgtrl4(string,iparse,nreq,descr,ndesc,bkreal,nret,
     &	 iflag,idelim)
	return
	end
