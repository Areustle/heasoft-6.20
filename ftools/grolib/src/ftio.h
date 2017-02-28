c
c
c	FORTRAN include for IBM FTIO Utilities
c
c	Revision 1.1  1991/10/08  21:31:57  esp
c       $Id: ftio.h,v 3.2 2013/05/21 19:08:27 irby Exp $
c       $Log: ftio.h,v $
c       Revision 3.2  2013/05/21 19:08:27  irby
c       Change character*n to character(n) to silence warnings: "Obsolescent
c       feature: Old-style character length".
c
c       Revision 3.1  2002/04/16 20:32:09  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.2  1996/09/11  11:25:17  wang
c Change 'ftio_max_unit' from 50 to 150.
c
c Revision 1.1  1996/03/27  18:51:29  wang
c Initial revision
c
c
c
c	FTIO System parameters
c
c	integer*4 ftio_unit
	integer*4 ftio_max_unit

	parameter ( ftio_max_unit =150 )

c	integer*4 ftio_blksize
	integer*4 ftio_max_blksize

	parameter ( ftio_max_blksize = 32760 )
c
c	FTIO Error status codes
c
c	integer*4 ftio_status

	integer*4 ftio_ok
	integer*4 ftio_bad_length
	integer*4 ftio_incstnt_len
	integer*4 ftio_bad_unit
	integer*4 ftio_no_memory
	integer*4 ftio_open_failed
	integer*4 ftio_bad_blksize
	integer*4 ftio_bad_recfm
	integer*4 ftio_no_buffers
	integer*4 ftio_write_error
	integer*4 ftio_eof
	integer*4 ftio_nounits
	integer*4 ftio_notopen
	integer*4 ftio_syserr

	parameter ( ftio_ok          =  0 ) 
	parameter ( ftio_bad_length  =  1 )
	parameter ( ftio_incstnt_len =  2 )
	parameter ( ftio_bad_unit    =  3 )
	parameter ( ftio_no_memory   =  4 )
	parameter ( ftio_open_failed =  5 )
	parameter ( ftio_bad_blksize =  6 )
	parameter ( ftio_bad_recfm   =  7 )
	parameter ( ftio_no_buffers  =  8 )
	parameter ( ftio_write_error =  9 )
	parameter ( ftio_eof         = 10 )
	parameter ( ftio_nounits     = 11 )
	parameter ( ftio_notopen     = 12 )
	parameter ( ftio_syserr      = 13 )

c
c	FTIO Device Types
c
c 	integer*4 ftio_device

	integer*4 ftio_label_tape
	integer*4 ftio_nonlabel_tape
	integer*4 ftio_disk_file

	parameter ( ftio_disk_file = 0 )
	parameter ( ftio_label_tape = -1 )
	parameter ( ftio_nonlabel_tape = 1 )

c
c	FTIO Device Access 
c
c	integer*4 ftio_access
c
c	NOTE: read/write access does not work
c
	integer*4 ftio_readonly
	integer*4 ftio_writeonly

	parameter ( ftio_readonly  = 1 )
	parameter ( ftio_writeonly = 2 )

c
c	FTIO file name
c
c 	character(256) ftio_fname
c
c	
c	FTIO Record Format
c
c	character(2) ftio_recfm

	character(2) ftio_recfm_v
	character(2) ftio_recfm_vb
	character(2) ftio_recfm_f
	character(2) ftio_recfm_fb

	parameter ( ftio_recfm_v  = 'V' )
	parameter ( ftio_recfm_vb = 'VB' )
	parameter ( ftio_recfm_f  = 'F' )
	parameter ( ftio_recfm_fb = 'FB' )
c
c
c	FTIO Logical Record Length for recfm = F
c
c 	integer*4 ftio_lrecl
c
c
c	FTIO Buffer Length - size in bytes of user buffer
c
c	integer*4 ftio_buflen
c
c	FTIO Buffer - user's buffer space
c
c	character(1)(ftio_buflen) ftio_buffer
c
c	FTIO Record Length - length of record in bytes returned on read
c
c	integer*4 ftio_reclen
c
c
c
c	******************************************************************
c
c			FTIO Routine Calling Sequences
c
c	******************************************************************
c
c
c	subroutine ftio_open ( ftio_access, ! Input - file access read or write
c    *                         ftio_fname,  ! Input - file name
c    *                         ftio_device, ! Input - tape or disk
c    *                         ftio_recfm,  ! Input - IBM record format
c    *                         ftio_blksize,! Input - block size
c    *                         ftio_lrecl,  ! Input - logical record length
c    *                         ftio_unit,   ! Output - I/O unit
c    *                         ftio_status )! Output - status
c
c
c	subroutine ftio_close ( ftio_unit,    ! Input - I/O unit
c    *                          ftio_status ) ! Output - status
c
c
c	subroutine ftio_read ( ftio_unit,   ! Input - I/O unit
c    *                         ftio_buflen, ! Input - buffer length 
c    *                         ftio_buffer, ! Input - user buffer
c    *                         ftio_reclen, ! Output - record length in bytes
c    *                         ftio_status )! Output - status
c    *                         
c
c
c	subroutine ftio_write ( ftio_unit,   ! Input - I/O unit
c    *                          ftio_buflen, ! Input - buffer length
c    *                          ftio_buffer, ! Input - user buffer
c    *                          ftio_status )! Output - status
c
c
c
c	subroutine ftio_rewind ( ftio_unit,    ! Input - I/O unit
c    *                           ftio_status ) ! Output - status
c
c
c
c	subroutine ftio_errmsg ( ftio_status ) ! Input - Error status code
c
c

