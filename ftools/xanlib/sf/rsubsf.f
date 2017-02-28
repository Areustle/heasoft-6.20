	subroutine rsubsf( unit, buffer, length, ierrsf)
c		rashafer 16 april 1986
c	SF routine to read a single subsidiary record to the current
c	package
c	unit	i4	i: unit no.
c	buffer	b(length)	i: the buffer
c	length	i4	i: the length of the buffer, on return the
c			actual number of bytes read.  N.B, that when
c			a package has been explicitly terminated, the
c			terminating record returns a length of zero. (And
c			the silent error #27 is raised).
c	ierrsf	i4	i/r: the error flag
c			7  - eof (silent)
c			17 - I/O read error
c			20 - Unable to reposition after a badly terminated
c			package.
c			21 - Buffer too small
c			27 - At end of the package (silent)
	integer*4 unit,length,ierrsf
	character(1) buffer(*)
	integer*4 tmplen,i,ios
	read(unit,iostat=ios)tmplen,(buffer(i),i=1,min(tmplen,length))
	if(ios.lt.0)then
	    ierrsf=7
	    length=0
	elseif(ios.gt.0)then
	    if(ierrsf.eq.0)write(*,*)'RSUBSF: Read I/O error:',ios
	    ierrsf=17
	    length=0
	elseif(tmplen.gt.length)then
	    if(ierrsf.eq.0)write(*,*)
     &	     'RSUBSF: Buffer too small, actual length:',tmplen,
     &	     ' buffer length:',length
	    ierrsf=21
	elseif(tmplen.lt.0)then
	    backspace(unit,iostat=ios)
	    length=0
	    if(ierrsf.eq.0)write(*,*)'RSUBSF: Badly terminated package'
	    if(ios.ne.0)then
		if(ierrsf.eq.0)write(*,*)
     &		 'RSUBSF: Unable to backspace to start of next package'
		ierrsf=20
		end if
	    end if
	length=tmplen
	if(length.eq.0)then
	    ierrsf=27
	    end if
	return
	end
