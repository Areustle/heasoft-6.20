	logical*4 function xparr4( string, idescr, descr, retval)
c		rashafer 14 April 1986
c	XPARSE routine to parse out an integer*4
c	xparr4	L4	Function: If true, an integer*4 was parsed and
c			returned in retval
c	string	c*	i: string to hold value
c	idescr	i4	i: if > 0 a descriptor is passed
c	descr	c*	i: a descriptor of the variable
c	retval	r4	i/r: the current value
	character*(*) string, descr
	integer*4 idescr
	real*4  retval
	include 'xparinc.inc'
	logical*4 xisint,xisnum,isnum
	integer*4 sqzlen
	character(15) tmpstr
	real*8 dval

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')

	isnum=xisnum(string,dval,xisint,sqzlen)
	if(isnum)then
	    xparr4=.true.
	    retval=dval
	else
	    xparr4=.false.
	    write(tmpstr,'(1pg15.7)')retval
	    call xsquez(tmpstr,' ',0,sqzlen)
	    if(string.eq.inquiry)then
		    if(idescr.gt.0)then
			call xpmsg1(descr,tmpstr(:sqzlen))
		    else
			call xpmsg1('number',tmpstr(:sqzlen))
			end if
	    else
		    if(idescr.gt.0)then
			call xpmsg2('a number',string,descr,tmpstr(:sqzlen))
		    else
			call xpmsg2('a number',string,' ',tmpstr(:sqzlen))
			end if
		    end if
	    end if
	return
	end
