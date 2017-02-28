*+ CRSTRSF
	subroutine crstrsf(iunit,strar,nstr,nstrot,ierrsf)

	IMPLICIT NONE
	integer iunit, nstr, nstrot, ierrsf
	character*(*) strar(*)
c
c Description
c   SF subroutine to write an array of character strings as
c   auxilary records to the current SF package.  
c   N.B.  If the package header shows an indeterminate no. of records
c	  then good practice is for the user to add a terminating record.
c	  (one with length 0).
c  Hacked version of XANADU routine RSTRSF
c
c Passed Parameters
c	iunit	i4		i: read unit
c	strar	c*(nstr)	i/r: Array of character strings to be read.
c	nstr	i4		i: dimension of array.
c	nstrot	i4		i/r: No. of string. to be read in array.  
c				A negative value
c				indicates that the 'indeterminate' size
c				auxilary region condition holds.
c				Returns the actual no of strings read.
c				If an error condition arises such that too few
c				records can be found, then nstr is returned
c				with the actual no. of records read.
c	ierrsf	i4		i/r: SF error flag
c			17 -	io read error
c			18 -	EOF condition before required records read.
c			19 -	Too few input records.
c			20 - 	Unable to reposition after a badly terminated
c				indefinite no. of auxilary records.
c			21 -    At least one has insufficient info
c Called Routines
c
c Origin
c  IMG Hacked version of XANADU routine RSTRSF, written by Rick Shafer,
c  which itself was hacked from the XANADU routine WSTRSF
c
c Author/Modification History
c  rashafer 	(1995 Mar 16), original RSTRSF 
c  Ian M George (1993 Jun 21), FTOOL-ized it
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals 
	logical qwerr
	integer istr, lens, nreqs, ios, lc
	character(30) errstr, wrnstr
	character(70) message
c Initialize
	errstr = '** CRSTRSF ERROR :'
	wrnstr = '** CRSTRSF WARNING :'
c Go
	qwerr=ierrsf.eq.0
	ierrsf=0
	istr=0
	lens=len(strar(1))
	nreqs=min(nstrot,nstr)
	nstrot=-1
	do while((istr.ne.nreqs).and.(istr.lt.nstr))
	   istr=istr+1
	   read(iunit,iostat=ios)lc,strar(istr)(:min(lc,lens))
	   if(ios.ne.0)then
		if(ios.lt.0)then
c... EOF condition
		    if(nstr.gt.0)then
			if(qwerr) then
			  message = errstr // 'EOF before all records read'
		          call fcecho(message)
		         endif
			 ierrsf=18
			 nstrot=istr-1
			 istr=nstr
		     endif
		 else
		  if(qwerr) then
		   write(message,'(2a,i12)') errstr, 'Read i/o error',ios
 		   call fcecho(message)
		   write(message,'(a,i12)') '... processing string', istr
 		   call fcecho(message)
		  endif	
   	  ierrsf=17
		  nstrot=istr-1
		  istr=nstr
		 endif
	   endif
c Trap other errors
	   if(lc.lt.0)then
		if(qwerr) then
		   message = errstr // 'Badly terminated auxilary package'
 		   call fcecho(message)
		endif
		nstrot=istr-1
		if(nstr.ge.0)then
		   if(qwerr)then
		   message = errstr // 'End of package found'
 		   call fcecho(message)
		   write(message,'(a,i12)') '... processing string', istr
 		   call fcecho(message)
		  endif	
		  ierrsf=19
		endif
		istr=nstr
		backspace(iunit,iostat=ios)
		if(ios.ne.0)then
		   if(qwerr)then
		   message = errstr // 
     &			'Unable to reposition to package header'
 		   call fcecho(message)
		  endif	
		  ierrsf=20
		endif
	   elseif((lc.eq.0).and.(nstr.ge.0))then
		nstrot=istr-1
		ierrsf=19
		if(qwerr)then
		   message = errstr // 'End of package found'
 		   call fcecho(message)
		   write(message,'(a,i12)') '... processing string', istr
 		   call fcecho(message)
		 endif	
		istr=nstr
	    elseif(lens.lt.lc)then
		if(qwerr)then
		   message = errstr // 'Buffer too small'
 		   call fcecho(message)
		   message = ' ... some information lost'
 		   call fcecho(message)
		 endif	
		 ierrsf=21
	     else
		strar(istr)(lc+1:)=' '
	     endif
	end do
	if(nstrot.lt.0)nstrot=istr

	return
	end
