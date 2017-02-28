c	prsint	rashafer	8 Feb 1984
c		subroutine to parse a string for an integer
	subroutine prsint(string,num,ier)
c	arguments:
c	string	c*	i: string to be parsed
c	num	i4	r: integer value of string
c	ier	i4	r: error flag...  If ier=0 then an integer was
c			detected and parsed correctly.
c	n.b.:  the character string is assumed to contain no non-numeric
c	characters other than an optional prefix plus or minus
	character*(*) string
        integer*4 num, ier
        integer*4 n0val, n9val, nmval, npval
        integer*4 ls, is, isign, icchar, ival

C	parameter(n0val=ichar('0'),n9val=ichar('9'),nmval=ichar('-'),
C     &            npval=ichar('+'))
	n0val=ichar('0')
	n9val=ichar('9')
	nmval=ichar('-')
	npval=ichar('+')
	ls=len(string)
	ier=0
	is=1
	isign=1
	icchar=ichar(string(is:is))
	if(icchar.eq.npval)then
		is=is+1
	elseif(icchar.eq.nmval)then
		is=is+1
		isign=-1
		end if
	ival=0
	do while((is.le.ls).and.(ier.eq.0))
		icchar=ichar(string(is:is))
		if((icchar.ge.n0val).and.(icchar.le.n9val))then
			ival=10*ival+(icchar-n0val)
			is=is+1
		else
			ier=1
			end if
		end do
	num=isign*ival
	return
	end
