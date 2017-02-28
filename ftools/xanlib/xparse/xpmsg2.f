	subroutine xpmsg2 (type, string, descr, value)
c		rashafer 14 april 1986
c	XPARSE message subroutine, to place an information message in
c	the XPRMSG string (in the common block)
	character*(*) descr, value, string, type
	include 'xparinc.inc'
	integer*4 lenact
	xprmsg='Unable to parse '//type//' from "'//
     &         string(:lenact(string))//'"'
	if(lenact(descr).gt.0)then
		xprmsg(lenact(xprmsg)+2:)='for '//descr
		end if
	xprmsg(lenact(xprmsg)+1:)= ': ('//value(:lenact(value))//')'
	return
	end
