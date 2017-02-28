	subroutine termsf (unit,ierrsf)
c		rashafer 14 April 1986
c	SF subroutine to terminate a set of subsidary records for a package
c	with a record with a 0 value length indicator.
c	unit	i4	i: unit
c	ierrsf	i4	i/r: error flag
c			Errors: 1: i/o error during write
	integer*4 unit,ierrsf
	integer*4 zero,ierr
	parameter (zero=0)
	write(unit,iostat=ierr)zero
	if(ierr.ne.0)then
	    if(ierrsf.eq.0)then
		write(*,*)'TERMSF: I/O error during package termination.'
		end if
	    ierrsf=1
	    end if
	return
	end
