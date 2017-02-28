	logical*4 function xisint(string, ivalue)

c	XISINT	- Returned TRUE if the string is an integer*4
	character*(*) string	
c  I: string to be checked
	integer*4 ivalue	
c  R:    If XISINT is true, it is the value parsed.

	real*8 dval
	logical*4 qint,xisnum
	xisint = xisnum(string, dval, qint, ivalue)
	xisint = xisint.and.qint
	return
	end
