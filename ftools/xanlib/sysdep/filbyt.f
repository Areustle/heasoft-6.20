	SUBROUTINE FILBYT (n,a,b)
c----
c  simple routine to copy bytes back and forth
c---

c--- n is size, a and b are arrays, copies a to b

	character(1) a,b(*)
	integer n
        integer i

	do i=1,n
          b(i) = a
	end do

	return
	
	end
