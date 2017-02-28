	subroutine xptarg(string, iparse, newarg, idelim, ierr, *)
c		rashafer 12 April 1986
c	XPARSE subroutine to put a string back on the iparse line.  To
c	include the effects of special characters, the string is
c	automatically enclosed in open and close quotes.  Therefore trailing
c	blanks MAY be inadvertently be included unless NEWARG is LENACTed first.
c	string	c*	i/r: the parse string
c	iparse	i4	i/r: the parse position
c	newarg	c*	i: the argument to be placed on the string
c	idelim	i4	i: the delimiter to be placed there  (N.B. that
c			IDELIM = 0 will result in a blank, rather than a comma
c			to avoid an extra blank field at the end of the line,
c			unless if len(newarg) <=0, in which case only a comma
c			is put in and no argument)
c	ierr	i4	r: if 0 then all OK,
c			if = 1 then an overflow in the iparse string
c	Alternate returns:
c		*1	- overflow error condition
	character*(*) string,newarg
	integer*4 iparse,idelim,ierr
	include 'xparinc.inc'
	integer*4 oldlen,i,lenact,lendel,lentot,lenstr
	if(len(newarg).le.0)then
	    if(idelim.eq.0)then
		if(iparse.le.0)then
		    xprmsw(1:1)=comma
		    oldlen=1
		else
		    oldlen=0
		endif
	    else
		oldlen=0
	    end if
	else	
	    oldlen=1
	    xprmsw(1:1)=opnstr
	    do i=1,len(newarg)
		oldlen=oldlen+1
		xprmsw(oldlen:oldlen)=newarg(i:i)
		if(newarg(i:i).eq.clsstr)then
		    oldlen=oldlen+1
		    xprmsw(oldlen:oldlen)=clsstr
		end if
	    end do
	    oldlen=oldlen+1
	    xprmsw(oldlen:oldlen)=clsstr
	end if
	oldlen=oldlen+1
	if(idelim.ne.0)then
	    if(idelim.eq.1)then
		xprmsw(oldlen:oldlen)=spcbr1
	    elseif(idelim.eq.2)then
		xprmsw(oldlen:oldlen)=spcbr2
	    elseif(idelim.eq.-1)then
		xprmsw(oldlen:oldlen)=commnt
	    end if
	else
	    if(string(iparse:iparse).eq.comma)then
		xprmsw(oldlen:oldlen)=comma
	    else
		xprmsw(oldlen:oldlen)=blank
	    end if
	endif
	lenstr=lenact(string)
	lendel=lenstr-iparse
	lendel=min(0,lendel)
	lentot=len(string)
	if(lendel+oldlen.gt.lentot)then
         write(*,*)'*ERROR*:XPARSE:XPTARG: Possible loss of string (''',
     &	   string(lenstr+lentot-lendel-oldlen:lenstr),''')'
	end if
	xprmsw(oldlen+1:)=string(iparse+1:)
	string=xprmsw
	iparse=0
	return
	end
