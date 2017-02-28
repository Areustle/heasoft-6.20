c	fndtop		rashafer 1 June 1985
c renamed gettop Nick 30.8.91
c		SHF subroutine to find the topic corresponding to a given
c		topic string, as input on unit one.  See SHFTOD for
c		argument details.
	subroutine gettop(string,lenn,ieqtop,topics,ichstr,ichstp,inisub,
     &		inxtop)
c	ieqtop	i4	r: the topic number of the indicated topic string
c			if zero, then no match was found.
        integer*4 lenn, lenb, lene, iflag, idelim, jtop, ktop, ieqtop
        integer*4 itop
	character*(*) string,topics
	integer*4 ichstr(*),ichstp(*),inisub(*),inxtop(*)
	logical*4 qskip,qpart,xqmtch
	itop=1
100	continue
	call xgtarg(string,lenn,lenb,lene,qskip,iflag,idelim)
	if(qskip.or.(iflag.ne.0))goto 900
	jtop=0
	ktop=inisub(itop)
	do while((jtop.eq.0).and.(ktop.ne.0))
		if(xqmtch(topics(ichstr(ktop):ichstp(ktop)),string(lenb:lene),
     &		    qpart))then
			jtop=ktop
			itop=jtop
		else
			ktop=inxtop(ktop)
			end if
		end do
	if(jtop.eq.0)then
		write(*,*)' Unable to make an equivalence match, ',
     &		  string(:lene)
		ieqtop=0
		return
		end if
	goto 100
c **	** come from when the end of all the arguments has been reached
900	continue
	ieqtop=itop
	return
c **	** come from for the continuation line
	end
