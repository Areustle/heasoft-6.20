	integer*4 function newunit ()
c
c	Spin through all possible Fortran units and return the 
c	first one not in use.  This avoids the pain of having
c	to preallocate units. Fortran is most cool.
c       $Id: newunit.f,v 3.1 2002/04/16 20:32:12 irby Exp $
c       $Log: newunit.f,v $
c       Revision 3.1  2002/04/16 20:32:12  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/02/20  20:35:51  programs
c Initial revision
c
c

	implicit none

	integer*4 i
	logical unit_open

	do i = 99, 7, -1
	  inquire ( unit=i, opened=unit_open )
	  if ( .not. unit_open ) then
	    newunit = i
	    return
	  endif
	enddo

c
c	Ooops! Couldn't find one. This should never happen.....
c
	write(0,'("Error: unable to find free FORTRAN unit")')
	call exit(1)

	return
	end
