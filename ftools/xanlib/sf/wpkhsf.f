c	wpkhsf	rashafer	8 March 1985
c		SF subroutine to write a package header record
	subroutine wpkhsf(iunit,pkgtyp,index,nsubs,infoar,buffer,
     &                    len,ierrsf)
c	iunit	i4	i: output unit
c	pkgtyp	c*	i: package type
c	index	i4	i: index
c	nsubs	i4	i: no. of subsidiary records
c	infoar	i4(4)	i: spare
c	buffer	b*	i: buffer
c	len	i4	i: len of buffer
c	ierrsf	i4	i/r: SF error flag
c			15 - Write error
	character(1) buffer(*)
	character*(*) pkgtyp
	integer*4 idrec(7),infoar(4)
	character(1) idrecb(28)
	character(12) pkgout
	equivalence(idrec(1),idrecb(1),pkgout)
	integer*4 iunit, index, nsubs, len, ierrsf, ios, lent, i
	pkgout=pkgtyp
	idrec(4)=index
	idrec(5)=nsubs
	idrec(6)=infoar(1)
	idrec(7)=infoar(2)
	lent=28+len
	write(iunit,iostat=ios)-lent,idrec,(buffer(i),i=1,len)
	if(ios.ne.0)then
		if(ierrsf.eq.0)write(*,*)'WPKHSF:Write error:',ios
		ierrsf=15
	else
		ierrsf=0
		end if
	return
	end
