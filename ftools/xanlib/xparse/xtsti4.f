	logical*4 function xtsti4(value,min,max,idescr,descr)
c
c		rashafer 28 May 1986
c	XPARSE subroutine to test if a value is in range
c	Integer*4 version
	integer*4 value,min,max
	integer*4 idescr
	character*(*) descr
	character(12) valstr(3)
	include 'xparinc.inc'
	xtsti4 = (value.ge.min).and.(value.le.max)
	if(xtsti4)return
c	** now create the error message
	write(valstr(1),'(i12)')value
	write(valstr(2),'(i12)')min
	write(valstr(3),'(i12)')max
	if(idescr.gt.0)then
	    call xpmsg3(descr,valstr)
	else
	    call xpmsg3('an integer',valstr)
	    end if
	if(value.lt.min)then
	    value=min
	else
	    value=max
	    end if
	return
	end
