c -----------------------------------------------------------
*+ FCRHKY
	subroutine fcrhky(unit,maxhist,nhist,hist,status)

	IMPLICIT NONE
	integer unit,maxhist,nhist,status
	character*(*) hist(*)

c Description
c  Reads all the history keywords in the current HDU
c
c Author/Modification history
c  Peter D Wilson (1.0.0: 1998 Feb 20) original
c  Mike Tripicco (2000 Oct 31) CFITSIO mod starts 
c    HISTORY kwds in column 9 instead of 11
*-
c Internals 
	integer stat
	character(80) card

	stat = status
	call ftgrec(unit,0,card,stat)

 10	call ftgnxk(unit,'HISTORY ',1,' ',0,card,stat)
	if( stat.eq.0 ) then
	   if( nhist.lt.maxhist ) then
	      nhist = nhist+1
c	      hist(nhist) = card(11:)
	      hist(nhist) = card(9:)
	      goto 10
	   else
	      hist(nhist) = '  [...more... See input files]'
	   endif
	endif
	if( stat.ne.202 .and. stat.ne.0 ) status = stat

	return
	end
c -----------------------------------------------------------
