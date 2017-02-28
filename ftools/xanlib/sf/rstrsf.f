c	rstrsf		rashafer 16 March 85
c		SF subroutine to write an array of character strings as
c		auxilary records to the current SF package.  N.B.  If
c		the package header shows an indeterminate no. of records
c		then good practice is for the user to add a terminating record.
c		(one with length 0).
c	modified from WSTRSF - Now READs a set of character strings.
	subroutine rstrsf(iunit,strar,nstr,nstrot,ierrsf)
c
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
	character*(*)strar(*)
	logical*4 qwerr
	integer*4 iunit, nstr, nstrot, ierrsf, istr, lens, nreqs, ios, lc
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
c				**EOF condition
			    if(nstr.gt.0)then
				if(qwerr)write(*,*)'RSTRSF: EOF before all ',
     &	  'records read in:',istr
				ierrsf=18
				nstrot=istr-1
				istr=nstr
				end if
			else
			    if(qwerr)write(*,*)'RSTRSF: Read i/o error ',ios,
     &	  ' while processing string ',istr
			    ierrsf=17
			    nstrot=istr-1
			    istr=nstr
			    end if
			end if
		if(lc.lt.0)then
			if(qwerr)write(*,*)
     &	  'RSTRSF: Badly terminated auxilary package'
			nstrot=istr-1
			if(nstr.ge.0)then
			    if(qwerr)write(*,*)
     &	  'RSTRSF: End of package while processing ',istr
			    ierrsf=19
			    end if
			istr=nstr
			backspace(iunit,iostat=ios)
			if(ios.ne.0)then
			    if(qwerr)write(*,*)
     &  'RSTRSF: Unable to reposition to package header'
			    ierrsf=20
			    end if
		elseif((lc.eq.0).and.(nstr.ge.0))then
			nstrot=istr-1
			ierrsf=19
			if(qwerr)write(*,*)
     &  ' RSTRSF: End of package while reading in string ',istr
			istr=nstr
		elseif(lens.lt.lc)then
			if(qwerr)write(*,*)
     &	'RSTRSF: Buffer too small,',lens,' to contain record ',nstr,
     &	' some information lost'
			ierrsf=21
		else
			strar(istr)(lc+1:)=' '
			end if
		end do
	if(nstrot.lt.0)nstrot=istr
	return
	end
