*+ CRSUBSF
	subroutine crsubsf( unit, buffer, length, ierrsf)

	IMPLICIT NONE
	integer	unit,length,ierrsf
	character(1) buffer(*)
c
c Description
c  SF routine to read a single subsidiary record to the current package
c  Nicked from XANLIB (but FTOOL-ized)
c 
c Passed Parameters
c  UNIT	i4	i: unit no.
c  BUFFER	b(length)	i: the buffer
c  LENGTH	i4	i: the length of the buffer, on return the
c			actual number of bytes read.  N.B, that when
c			a package has been explicitly terminated, the
c			terminating record returns a length of zero. (And
c			the silent error #27 is raised).
c IERRSF	i4	i/r: the error flag
c			7  - eof (silent)
c			17 - I/O read error
c			20 - Unable to reposition after a badly terminated
c			package.
c			21 - Buffer too small
c			27 - At end of the package (silent)
c
c Author & Modification History
c  rashafer (1986 april 16) , XANADU original
c  Ian M George (1.0.1:1993 Apr 22), made FTOOLS/CALTOOLS compatible
c  Ian M George (1.0.2:1993 Jul 19), better error diagnostics
	character(7) version
	parameter (version = '1.0.2')
*-
c Internals & Initialization
	character(80) message
	character(80) errstr
	integer tmplen,i,ios, fcstln
	errstr = '** CRSUBSF '//version// ' ERROR:'

	read(unit,iostat=ios)tmplen,(buffer(i),i=1,min(tmplen,length))
	if(ios.lt.0)then
	    ierrsf=7
	    length=0
	elseif(ios.gt.0)then
	    if(ierrsf.eq.0) then 
		write(message, '(2a,i12)') errstr, 'Read I/O status:',ios
		call fcecho(message)
	    endif
	    ierrsf=17
	    length=0
	elseif(tmplen.gt.length)then
	    if(ierrsf.eq.0) then 
		call fcecho(errstr)
		write(message, '(a,i12,a,i12)') 
     &	     '... Buffer too small, actual length:',tmplen,
     &	     ' buffer length:',length
		call fcecho(message)
	    endif
	    ierrsf=21
	elseif(tmplen.lt.0)then
	    backspace(unit,iostat=ios)
	    length=0
	    if(ierrsf.eq.0) then 
		message = errstr(:fcstln(errstr))// 'Badly terminated package'
		call fcecho(message)
	    endif
	    if(ios.ne.0)then
	        if(ierrsf.eq.0) then 
		    call fcecho(errstr)
		    message = 
     &          '... Unable to backspace to start of next package' 
		    call fcecho(message)
	        endif
		ierrsf=20
		end if
	    end if
	length=tmplen
	if(length.eq.0)then
	    ierrsf=27
	    end if
	return
	end
