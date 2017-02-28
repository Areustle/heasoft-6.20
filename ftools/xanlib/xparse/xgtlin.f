	subroutine xgtlin(string, iparse, nreq, descr, ndesc,
     &	 lowlim, hilim, nlim, retval, nret,
     &	 iflag, idelim, *, *, *)
c		rashafer 14 April 1986
c	XPARSE subroutine to return a list of I*4 (using XGTNUM)
	character*(*) string,descr(*)
	integer*4 retval(*),iparse,nreq,ndesc,nret,iflag,idelim,
     &	 lowlim(*),hilim(*),nlim
	call xgtnum(string,iparse,nreq,descr,ndesc,
     &		lowlim,hilim,nlim,3,
     &		retval, nret,
     &		iflag, idelim, *910, *920, *930)
	return
910	continue
	return 1
920	continue
	return 2
930	continue
	return 3
	end
