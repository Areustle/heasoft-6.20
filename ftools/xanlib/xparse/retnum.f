c	retnum	rashafer 6 April 1984
c		subroutine to return the numerical value of a string using
c		the fortran translation routines
	subroutine retnum(string,qnum,qint,dval,ival)
c	string	c*	i: string to be translated
c	qnum	l4	r: if true, the string was a number
c	qint	l4	r: if true, the string was an integer
c	dval	r8	r: the returned value
c	ival	i4	r: the returned integer value
	character*(*) string
	logical*4 qnum,qint
	real*8 dval
	integer*4 len, lenact, ios, ival
	len=lenact(string)
	if(len.le.0)then
		qnum=.false.
		qint=.false.
		return
		end if
	read(string,'(f16.0)',iostat=ios)dval
	qnum=ios.eq.0
	if(qnum)then
		read(string,'(i10)',iostat=ios)ival
		qint=ios.eq.0
	else
		qint=.false.
		end if
	return
	end
