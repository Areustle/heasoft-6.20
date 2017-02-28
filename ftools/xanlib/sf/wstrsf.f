c	wstrsf		rashafer 16 March 85
c		SF subroutine to write an array of character strings as
c		auxilary records to the current SF package.  N.B.  If
c		the package header shows an indeterminate no. of records
c		then good practice is for the user to add a terminating record.
c		(one with length 0).
	subroutine wstrsf(iunit,strar,nstr,ierrsf)
c
c	iunit	i4		i: Write unit
c	strar	c*(nstr)	i: Array of character strings to be written.
c	nstr	i4		i: No. of strings
c	ierrsf	i4		i/r: SF error flag
c			16 -	io write error
	character*(*)strar(*)
	integer*4 istr, nstr, iunit, ierrsf, lc, lenact, ios
	do istr=1,nstr
		lc=max(lenact(strar(istr)),1)
		write(iunit,iostat=ios)lc,strar(istr)(:lc)
		if(ios.ne.0)then
			if(ierrsf.eq.0)write(*,*)'WSTRSF: Write i/o error ',
     &			  ios,' while porcessing string ',istr
			ierrsf=16
			return
			end if
		end do
	return
	end
