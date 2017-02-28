	subroutine xgtrrg(string,iparse,nrange,rngnam,descr,rnglw,
     &		rnghi,rngmn,rngmx,idefrg,qsing,iflag,idelim,*,*,*)
c		rashafer   8 June 1986
c			XPARSE subroutine to get a general real range from
c			the prompt string (see the XPRRRG subroutine
c			for the details of what constitutes a general
c			string
	character*(*)	string
	integer*4	iparse
	integer*4	nrange
	character*(*)	rngnam(*),descr
	real*4		rnglw(*),rnghi(*),rngmn(*),rngmx(*)
	integer*4	idefrg(*)
	logical*4	qsing
	integer*4	iflag	
c  Error Conditions 0 - A range parsed (or
c 			 the field was skipped)
c		   -1 EOF on corrections
c		    1 End of string reached
c 		    2 Infinite skip
	integer*4	idelim
c	** Alternate returns
c			*1	EOF
c			*2	end of string
c			*3	Infinite skip
c
	logical*4 qskip,qdone
	integer*4 ibeg,iend
	call xgtarg(string,iparse,ibeg,iend,qskip,iflag,idelim)
	if(iflag.ne.0)then
	    if(iflag.eq.1)then
		return 2
	    elseif(iflag.lt.0)then
		return 1
	    elseif(iflag.eq.2)then
		return 3
	    else
		return
		end if
	    end if
	if(qskip)then
	   return
	   end if
	qdone=.false.
	do while(.not.qdone)
	    call xprrrg(string(ibeg:iend),nrange,rngnam,descr,rnglw,rnghi,
     &	     rngmn,rngmx,idefrg,.true.,qsing,iflag)
	    qdone=iflag.eq.0
	    if(.not.qdone)then
		call xinfix('Replace entire argument:',string,iparse,iflag)
		if(iflag.ne.0)then
		    iflag=-1
		    return 1
		    end if
		end if
	    end do
	return
	end
