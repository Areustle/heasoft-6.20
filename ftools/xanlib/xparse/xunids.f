	subroutine xunids(chose,choice,nch,jch,type)
c		rashafer 19 oct 1985
c	    XPARSE subroutine to print a message when a string was unidentified
c	chose	c*	i: the string tested (special case if '?')
c	choice	c*(nch)	i: the possible values
c	nch	i4	i: the no. of possible values
c	jch	i4	i: the error indicator: if 0 then ther had been
c			no (allowed) match found.  If < 0 then
c			multiple matches were indicated starting with
c			choice (-hcg(
c	type	c*	i: the type of value being looked for
	integer*4 nch,jch
	character*(*) chose,choice(nch),type
	include 'xparinc.inc'

	integer*4 linew
	parameter (linew=72)
	logical*4 qpart,xqmtch,qpart2
	integer*4 lenact
	integer*4 lc,ncol,icol,ic,kch,ich,lentyp

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')

	qpart=jch.lt.0
	lentyp=lenact(type)
	if(qpart)then
	    write(*,*)' A unique example of ',type(:lentyp),
     &	     ' must be chosen:'
	    ich= -jch
	else
	    ich = 1
	    if(chose.ne.inquiry)then
c **		** Does not match the enquire mode
		write(*,*)'''',chose(:lenact(chose)),''' does not match.'
		end if
	    write(*,*)' Choose from the following ',type(:lentyp),':'
	    end if
	lc= len(choice(1))
	ncol=linew/(lc+1)
	icol=0
	ic=0
	xprmsw(:linew)= ' '
	do kch = ich,nch
	    if((.not.qpart).or.(xqmtch(chose,choice(kch),qpart2)))then
		icol=icol+1
		xprmsw(ic+1:ic+lc) = choice(kch)
		if(icol.eq.ncol)then
		    write(*,'(1x,a)')xprmsw(:linew)
		    xprmsw(:linew)= ' '
		    icol=0
		    ic=0
		else
		    ic=ic+lc+1
		    end if
		end if
	    end do
	if(icol.gt.0)then
	    write(*,'(1x,a)')xprmsw(:linew)
	    end if
	return
	end
