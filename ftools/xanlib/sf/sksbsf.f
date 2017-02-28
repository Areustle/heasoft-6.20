	subroutine sksbsf( unit, ierrsf)
c		rashafer 16 april 1986
c	SF routine to skip a single subsidiary record to the current
c	package

c	unit	i4	i: unit no.
c	ierrsf	i4	i/r: the error flag
c			7  - eof (silent)
c			17 - I/O read error
c			20 - Unable to reposition after a badly terminated
c			package.
c			27 - At end of the package (silent)
	integer*4 unit,ierrsf
	integer*4 tmplen,ios
	read(unit,iostat=ios)tmplen
	if(ios.lt.0)then
	    ierrsf=7
	elseif(ios.gt.0)then
	    if(ierrsf.eq.0)write(*,*)'SKSBSF: Read I/O error:',ios
	    ierrsf=17
	elseif(tmplen.lt.0)then
	    backspace(unit,iostat=ios)
	    if(ierrsf.eq.0)write(*,*)'SKSBSF: Badly terminated package'
	    if(ios.ne.0)then
		if(ierrsf.eq.0)write(*,*)
     &		 'SKSBSF: Unable to backspace to start of next package'
		ierrsf=20
		end if
	elseif(tmplen.eq.0)then
	    ierrsf=27
	    end if
	return
	end


