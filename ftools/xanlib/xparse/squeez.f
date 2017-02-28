c	squeez	rashafer 29 July 1984
c		Subroutine to take a string and squeez out any blanks
c	modified 8 March 85 to allow arbitrary characters to be removed,
c		and to allow an arbitrary no. of them to be retained.
	subroutine squeez(string,rmchar,nsave,length)
c	string	c*	i/r: string to have blanks removed
c	char	c*	i: the character to be removed
c	nsave	i4	i: all strings of char of length nsave are to be
c			retained.  To remove all occurances nsave <=0
c	length	i4	r: actual length of squeezed string
	character*(*) string
	character(1) rmchar
	logical*4 qon
	integer*4 irm, initl, initb, nsave, length, lenact, ival, icount
	irm=ichar(rmchar(1:1))
	initl=lenact(string)
	initb=index(string(:initl),rmchar(1:1))
	if(initb.eq.0)then
		length=initl
	else
		qon=.false.
		length=initb-1
		do while(initb.lt.initl)
			initb=initb+1
			ival=ichar(string(initb:initb))
			if(ival.ne.irm)then
				qon=.false.
				length=length+1
				string(length:length)=char(ival)
			else
				if(.not.qon)then
					qon=.true.
					icount=1
				else
					icount=icount+1
					end if
				if(icount.le.nsave)then
					length=length+1
					string(length:length)=rmchar(1:1)
					end if
				end if
			end do
		end if
	return
	end
