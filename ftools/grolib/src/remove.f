	subroutine remove ( file, status )
c
c	Remove or delete file
c       $Id: remove.f,v 3.4 2013/05/21 19:08:27 irby Exp $
c       $Log: remove.f,v $
c       Revision 3.4  2013/05/21 19:08:27  irby
c       Change character*n to character(n) to silence warnings: "Obsolescent
c       feature: Old-style character length".
c
c       Revision 3.3  2006/04/17 20:54:27  irby
c       Updates for compliance with g95 and gfortran:
c
c       - Replace call to lnblnk with equivalent call to len_trim.
c         lnblnk (a routine which used to live in libg2c) is not currently
c         available with g95 or gfortran.
c
c       - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
c
c       - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
c
c       - Fix non-integer loop variables.
c
c       Revision 3.2  2003/08/19 20:42:14  irby
c       Renamed unlink.c to gro_unlink.c to avoid confusion with the system
c       unlink routine.
c
c       Revision 3.1  2002/04/16 20:32:13  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/02/20  20:36:00  programs
c Initial revision
c
c
	implicit none

	character*(*) file
	integer*4 status, n, m, len_trim
	character(129) nfile
	character(256) context

c
c	Make sure that trailing blanks are not passed to
c	unlink system call
c
	
	n = len_trim(file)
	m = n+1
	nfile(1:n) = file(1:n)
	nfile(m:m) = char(0)

	call gro_unlink ( nfile(1:m), status )

	if ( status .ne. 0 ) then
          context = 'Cannot remove ' // nfile(1:n) // ' '
	  call fcerr (context) 
	endif
c
c	status values are same as UNIX system <errno.h>
c	relevent values are:
c
c	0 - ok
c	1 - not owner
c	2 - no such file or directory
c	13 - perrmission denied
c	17 - unlink zero length filename
c	

	return
	end
	
