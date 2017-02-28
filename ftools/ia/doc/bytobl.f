c	bytobl		rashafer 27 June 85
c		SHF subroutine the accumulates bytes into a buffer array and
c		then writes out sequential blocks into the indicated
c		direct access file
	subroutine bytobl(bytear,lenbyt,buf,clen,cpage,lenbuf,iunit)
c		modified 25 Feb 1987 to do a record write when lenbyt is
c			a negative number.
c		modified 10 mar 1987 to properly handle multiple buffer length
c		records
c	bytear	b(lenbyt)	i: string of bytes to be accumulated
c	lenbyt	i4		i: no. of bytes to so accumulate, when < 0
				
c   then dump the buffer in any case
c	buf	b(lenbuf)	w/r: accumulation buffer
c	clen	i4		i/r: current location in buffer
c	cpage	i4		i/r: current d.a. record in file
c	lenbuf	i4		i: size of buffer (BYTYES
c !!!)
c	iunit	i4		i: Fortran unit no.  File must have been
c				previously opend as a Fortran Unformatted
c				direct access file.  Be sure that the record
c				length in the open statement was a
c				number of full-words (4 byte quantitites).
c				(Thus lenbuf must be a multiple of 4)
	integer*4 lenbyt,clen,cpage,lenbuf,iunit
	character(1) bytear(*), buf(lenbuf)
	integer*4 i
	if(lenbyt.lt.0)then
	    if(clen.gt.0)then
c **		** non empty buffer so dump it
		cpage=cpage+1   
		write(iunit,rec=cpage)buf
		clen=0
		end if
	    return
	    end if
	i = 0
	do while (i .lt. lenbyt)
	    do while((clen.lt.lenbuf).and.(i.lt.lenbyt))
		i=i+1
		clen=clen+1
		buf(clen)=bytear(i)
		end do
	    if(clen.eq.lenbuf)then
		cpage=cpage+1
		write(iunit,rec=cpage)buf
		clen=0
		end if
	    end do
	return
	end
