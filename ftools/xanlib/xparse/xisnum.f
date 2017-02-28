	logical*4 function xisnum(string, dval, qint, ival)
c	xisnum	rashafer 6 April 1984
c	modified from RETNUM (Rick Utility) 8 March 1986 for XPARSE inclusion
c		subroutine to return the numerical value of a string using
c		the fortran translation routines
c	** Returned value
c	XISNUM	L4	-: if true, a real number was parsed
c
c	string	c*	i: string to be translated
c	dval	r8	r: the returned value (modified only if XISNUM is true)
c	qint	l4	r: if true, the string was an integer
c	ival	i4	r: the returned integer value (modified if QINT is true)
	character*(*) string
	logical*4 qint
	real*8 dval
	integer*4 ival
c
	integer*4 lenact,len,ios
	integer*4 ivaltm
	real*8 dvaltm
	len=lenact(string)
	if(len.le.0)then
		xisnum=.false.
		qint=.false.
		return
		end if
	read(string,'(f15.0)',iostat=ios)dvaltm
	xisnum=ios.eq.0
	if(xisnum)then
		dval=dvaltm
		read(string,'(i8)',iostat=ios)ivaltm
		qint=ios.eq.0
		if(qint)then
		    ival=ivaltm
		    end if
	else
		qint=.false.
		end if
	return
	end
