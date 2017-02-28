        subroutine ftio_close ( ftio_unit,
     *                          ftio_status )
c
c
c	FORTRAN Wrapper to ftio fclose routine
c       $Id: ftio_close.f,v 3.1 2002/04/16 20:32:09 irby Exp $
c       $Log: ftio_close.f,v $
c       Revision 3.1  2002/04/16 20:32:09  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.2  1996/08/13  19:23:37  wang
c Change the code that set isopen(ftio_unit) to false when the file is close.
c
c Revision 1.1  1996/03/27  18:50:01  wang
c Initial revision
c
c
	implicit none

	include 'ftio.h'
	include 'ftiosys.h'

	integer*4 ftio_unit
	integer*4 ftio_status

	integer*4 fclose

	if ( .not. isopen(ftio_unit) ) then
	  ftio_status = ftio_notopen
	  return
	endif
        isopen(ftio_unit) = .false.
	ftio_status = fclose ( ftio_unit )

	return
	end

