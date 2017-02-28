	subroutine xfrlun(iunit,ierr)
c		rashafer 9 Feb 1986
c	XPARSE subroutine to free a logical unit no
c	iunit	i4	r: the logical unit returned
c	ierr	i4	r: an error flag
	integer*4 iunit,ierr

	call frelun(iunit)
	ierr = 0
	return
	end
