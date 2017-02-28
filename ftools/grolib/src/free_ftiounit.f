	subroutine free_ftiounit ( ftio_unit,
     *                             ftio_status )
c       $Id: free_ftiounit.f,v 3.1 2002/04/16 20:32:08 irby Exp $
c       $Log: free_ftiounit.f,v $
c       Revision 3.1  2002/04/16 20:32:08  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:49:39  wang
c Initial revision
c

	implicit none
	
	include 'ftio.h'
	include 'ftiosys.h'

	integer*4 ftio_unit, ftio_status, i

	do i=1,ftio_max_unit
	  if ( .not. isopen(i) )then	
	    isopen(i) = .true.
	    ftio_unit = i
	    ftio_status = ftio_ok
	    return
	  endif
	enddo

	ftio_unit = -1
	ftio_status = ftio_nounits
	
	return
	end
