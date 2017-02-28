	subroutine xpmsg3(descr,valstr)
c		subroutine to write a message that a number is out of range
c		in the XPRMSG string in the common block
	character*(*) descr, valstr(3)
	include 'xparinc.inc'
	integer*4 i,retlen(3),lenact
	do i=1,3
	    call xsquez(valstr(i),' ',0,retlen(i))
	    end do
	xprmsg = 'The value, '//valstr(1)(:retlen(1))//
     &	 ', of '//descr(:lenact(descr))//
     &	 ' is outside the allowed range ('
     &	//valstr(2)(:retlen(2))//','//valstr(3)(:retlen(3))//'):'
	return
	end
