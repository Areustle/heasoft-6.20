        subroutine ftio_rewind ( ftio_unit,
     *                           ftio_status )
c
c
c	FORTRAN Wrapper to ftio frewind routine
c       $Id: ftio_rewind.f,v 3.1 2002/04/16 20:32:10 irby Exp $
c       $Log: ftio_rewind.f,v $
c       Revision 3.1  2002/04/16 20:32:10  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:51:00  wang
c Initial revision
c
c
	implicit none

	include 'ftio.h'
	include 'ftiosys.h'
	
	integer*4 ftio_unit
	integer*4 ftio_status

	integer*4 rewind

	if ( .not. isopen(ftio_unit) ) then
	  ftio_status = ftio_notopen
	  return
	endif

	ftio_status = rewind ( ftio_unit )

	return
	end

