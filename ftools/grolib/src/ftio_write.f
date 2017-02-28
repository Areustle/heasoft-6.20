        subroutine ftio_write ( ftio_unit,
     *                          ftio_buflen,
     *                          ftio_buffer,
     *                          ftio_status )
c
c
c	FORTRAN Wrapper to ftio fwrite routine
c       $Id: ftio_write.f,v 3.1 2002/04/16 20:32:11 irby Exp $
c       $Log: ftio_write.f,v $
c       Revision 3.1  2002/04/16 20:32:11  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:51:14  wang
c Initial revision
c
c
	implicit none

	include 'ftio.h'
	include 'ftiosys.h'

	integer*4 ftio_unit
	integer*4 ftio_buflen
	integer*4 ftio_status

	character*(*) ftio_buffer

	integer*4 fwrite

	if ( .not. isopen(ftio_unit) ) then
	  ftio_status = ftio_notopen
	  return
	endif

	ftio_status = fwrite ( ftio_buffer,
     *                         ftio_unit,
     *                         ftio_buflen )

	return
	end

