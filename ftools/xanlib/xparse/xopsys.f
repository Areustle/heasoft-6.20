	subroutine xopsys(instrg, lenn)
	character*(*) instrg
	integer*4 lenn
C---
C Spawn a process to execute operating system commands
C---
C instrg  i/r  parse string
C lenn	  i/r  current parse position
C---
C rashafer 4 Dec 1984
C---
	integer*4 lcopy
	integer*4 lenact, iend, ibeg, iflag, iret, idelim
C
	include 'xparinc.inc'
	character(100) copy
	logical*4 qskip, request_inquire

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')


	if(lenn.lt.len(instrg))then
	    copy = instrg(lenn+1:)
	    if(lenact(copy).gt.0)then
		request_inquire = .false.
C  Turn off inquiry mode for blank lines
		call xgtarg(instrg,lenn,ibeg,iend,qskip,iflag,idelim,*200,*200
     &		 ,*200)
		if((.not.qskip).and.instrg(ibeg:iend).eq.inquiry)then
		    call xinfix(' Input an empty line OR a single command:'
     &		     ,instrg,lenn,iflag)
		    if(iflag.ne.0)then
			lenn=len(instrg)
			return
			end if
		    copy=instrg(lenn+1:)
		    if(lenact(copy).le.0)goto 200
		    end if
		lcopy = lenact(copy)
		WRITE(*,*) 'Spawning...'
		call spawn(copy, lcopy, iret)
		lenn=len(instrg)
		return
		end if
	    end if
C **	** come from for an empty line on the parse string
200	continue
	write(*,*)' Type ''lo'' to return to program'
	call spawn(' ',0,iret)
	lenn=len(instrg)
	return
	end
