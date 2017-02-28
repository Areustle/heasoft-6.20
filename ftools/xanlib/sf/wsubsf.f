	subroutine wsubsf( unit, buffer, length, ierrsf)
c		rashafer 16 april 1986
c	SF routine to write a single subsidiary record to the current
c	package
c	unit	i4	i: unit no.
c	buffer	b(length)	i: the buffer
c	length	i4	i: the length of the buffer to be written
c	ierrsf	i4	i/r: the error flag
c			16 - i/o write error
c			1 - 0 length record for subsidiary (a 1 byte record
c			is actually written.  Use TERMSF to terminate the
c			package if part of an indeterminate no. of records
c			package.
	integer*4 unit,length,ierrsf
	character(1) buffer(*)
	integer*4 tmplen,i,ios
	if(length.le.0)then
	    if(ierrsf.eq.0)
     &         write(*,*)'WSUBSF: Attempt to write 0 length record'
	    tmplen=1
	    ierrsf=1
	else
	    tmplen=length
	    end if
	write(unit,iostat=ios)tmplen,(buffer(i),i=1,tmplen)
	if(ios.ne.0)then
	    if(ierrsf.eq.0)write(*,*)'WSUBSF: Write I/O error:',ios
	    ierrsf=16
	    end if
	return
	end
