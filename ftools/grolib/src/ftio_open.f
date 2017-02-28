        subroutine ftio_open ( ftio_access,
     *                         ftio_fname,
     *                         ftio_device,
     *                         ftio_recfm,
     *                         ftio_blksize,
     *                         ftio_lrecl,
     *                         ftio_unit,
     *                         ftio_status )
c
c
c	FORTRAN Wrapper to ftio fopen routine
c       $Id: ftio_open.f,v 3.2 2013/05/21 19:08:27 irby Exp $
c       $Log: ftio_open.f,v $
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
c Revision 1.1  1996/03/27  18:50:34  wang
c Initial revision
c
c
	implicit none

	include 'ftio.h'
	include 'ftiosys.h'

	data isopen /ftio_max_unit*.false./

	integer*4 ftio_unit
	integer*4 ftio_access
	integer*4 ftio_device
	integer*4 ftio_blksize
	integer*4 ftio_lrecl
	integer*4 ftio_status

	character*(*) ftio_fname
	character(2)   ftio_recfm

	integer*4 fopen

	call free_ftiounit ( ftio_unit, ftio_status )

	if ( ftio_status .ne. ftio_ok ) then
	  return
	endif

	ftio_status = fopen ( ftio_access,
     *                        ftio_unit,
     *                        ftio_fname,
     *                        ftio_device,
     *                        ftio_recfm,
     *                        ftio_blksize,
     *                        ftio_lrecl )

	return
	end

