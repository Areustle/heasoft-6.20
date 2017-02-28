c	retstr	rashafer	4 Sept 1983
c		subroutine for making character string matches between
c		a test string and an input array of strings
	subroutine retstr(testin,array,nval,qall,ival)
c	testin	c*	input character string
c	array	c*(nval)	input array of strings to be matched with
c			test (assumed to be canonicalized)
c	nval	i4	input dimension of array
c	qall	l4	input flag: if true then there must be a complete
c				match between test and the array value,
c				but if false, then the array value may
c				have some unmatched characters, in which case
c				the first array value is deemed to be the
c				match value.
c	ival	i4	returned index of the array which matches test.
c				if 0, then no match found.
	logical*4 qall,qmatch
	integer*4 nval, ltest, lenact, ival
	character*(*) testin,array(nval)
c	convert the string to all lower case
	ltest=lenact(testin)
	ival=0
	if((ltest.eq.0).or.(len(array(1)).lt.ltest))return
	do ival=1,nval
		if(qmatch(testin,array(ival),.not.qall,.true.))return
		end do
	ival=0
	return
	end
