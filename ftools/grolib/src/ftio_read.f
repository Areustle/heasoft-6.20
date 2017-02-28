        subroutine ftio_read ( ftio_unit,
     *                         ftio_buflen,
     *                         ftio_buffer,
     *                         ftio_reclen,
     *                         ftio_status )
c
c
c	FORTRAN Wrapper to ftio fread routine
c       $Id: ftio_read.f,v 3.2 2013/05/21 19:08:27 irby Exp $
c       $Log: ftio_read.f,v $
c       Revision 3.2  2013/05/21 19:08:27  irby
c       Change character*n to character(n) to silence warnings: "Obsolescent
c       feature: Old-style character length".
c
c       Revision 3.1  2002/04/16 20:32:10  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:50:46  wang
c Initial revision
c
c
	implicit none

	include 'ftio.h'
	include 'ftiosys.h'

	integer*4 ftio_unit
	integer*4 ftio_buflen
	integer*4 ftio_reclen
	integer*4 ftio_status

	integer*4 fread, length

	character(1) ftio_buffer(ftio_buflen)

	if ( .not. isopen(ftio_unit) ) then
	  ftio_status = ftio_notopen
	  return
	endif

	length = ftio_buflen

	ftio_status = fread ( ftio_buffer,
     *                        ftio_unit,
     *                        length )

	ftio_reclen = length

	return
	end

