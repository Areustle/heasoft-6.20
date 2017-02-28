	logical*4 function xqxmch(test,base)
c		rashafer 18 october 1985
c	XPARSE function to see if a string is the same as a base string.
c		The match must be EXACT in length (no partial lengths allowed)
c		although case need not be significant.
c	xqxmch	L4	function result: if true, then a match exists
c	test	c*	I: the string to be tested
c	base	c*	I: the string to be compared against
	character*(*) test,base
	logical*4 qpart,xqmtch
	integer*4 lenact
	xqxmch = xqmtch(test(:lenact(test)),base,qpart)
	if (xqxmch.and.qpart) then
	    xqxmch =.false.
	    end if
	return
	end
