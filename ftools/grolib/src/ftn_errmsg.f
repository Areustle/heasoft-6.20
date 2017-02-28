	subroutine ftn_errmsg ( ftn_status )
c
c	Print error message relating to specified error
c	to stderr.
c       $Id: ftn_errmsg.f,v 3.3 2013/05/21 19:08:27 irby Exp $
c       $Log: ftn_errmsg.f,v $
c       Revision 3.3  2013/05/21 19:08:27  irby
c       Change character*n to character(n) to silence warnings: "Obsolescent
c       feature: Old-style character length".
c
c       Revision 3.2  2005/08/26 19:36:38  irby
c       Purely cosmetic changes to allow compilation with gfortran/g95, mostly
c       involving fixes to lines longer than 72 chars that were being truncated,
c       but in the case of the CGRO code, also moving misplaced (tabbed) line
c       continuation characters to their appropriate position in column 6.
c
c       Revision 3.1  2002/04/16 20:32:12  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/02/20  20:35:34  programs
c Initial revision
c
c
	implicit none

	include 'ftnerr.h'

	integer*4 minerr, maxerr, last_char, msg_number, ftn_status

	parameter ( minerr = ftn_format_error )
	parameter ( maxerr = ftn_bad_unit_opr )

	character(80) msgtext (minerr:maxerr)
	character(2)  count
	character(8)  format 

        data msgtext ( ftn_format_error ) 
     &           /' FTN - error in format'/
        data msgtext ( ftn_illegal_unit ) 
     &           /' FTN - illegal unit number'/
        data msgtext ( ftn_no_format_io ) 
     &           /' FTN - formatted io not allowed'/
        data msgtext ( ftn_no_unform_io ) 
     &           /' FTN - unformatted io not allowed'/
        data msgtext ( ftn_no_direct_io ) 
     &           /' FTN - direct io not allowed'/
        data msgtext ( ftn_no_seqntl_io ) 
     &           /' FTN - sequential io not allowed'/
        data msgtext ( ftn_no_backspace ) 
     &           /' FTN - cannot backspace file'/
        data msgtext ( ftn_off_beg_recd ) 
     &           /' FTN - off beginning of record'/
        data msgtext ( ftn_no_file_stat ) 
     &           /' FTN - cannot status file'/
        data msgtext ( ftn_repeat_count ) 
     &           /' FTN - no * after repeat count'/
        data msgtext ( ftn_off_end_recd ) 
     &           /' FTN - off end of record'/
        data msgtext ( ftn_trunc_failed ) 
     &           /' FTN - truncation failed'/
        data msgtext ( ftn_bad_list_inp ) 
     &           /' FTN - incomprehensible list input'/
        data msgtext ( ftn_no_free_spce ) 
     &           /' FTN - out of free space'/
        data msgtext ( ftn_not_connectd ) 
     &           /' FTN - unit not connected'/
        data msgtext ( ftn_unexpct_char ) 
     &           /' FTN - read unexpected character'/
        data msgtext ( ftn_blnk_logical ) 
     &           /' FTN - blank logical input field'/
        data msgtext ( ftn_newfile_exst ) 
     &           /' FTN - new file already exists'/
        data msgtext ( ftn_oldfile_gone ) 
     &           /' FTN - cannot find old file'/
        data msgtext ( ftn_system_error ) 
     &           /' FTN - unknown system error or too many open files'/
        data msgtext ( ftn_require_seek ) 
     &           /' FTN - requires seek ability'/
        data msgtext ( ftn_bad_argument ) 
     &           /' FTN - illegal argument'/
        data msgtext ( ftn_negtv_repeat ) 
     &           /' FTN - negative repeat count'/
        data msgtext ( ftn_bad_unit_opr ) 
     &           /' FTN - illegal operation for unit'/


	msg_number = ftn_status

	if ( msg_number .eq. ftn_eof ) then
c
c	   Typically this is and End-of-File on READ
c
	   write(0,'("FTN - End of File")') 

	elseif ( msg_number .eq. ftn_ok ) then
	   write(0,'("FTN - OK")') 

	elseif ( msg_number .ge. minerr .and. msg_number 
     &                                        .le. maxerr ) then
c
c	  Find last non-blank character in string
c	
	  last_char = index ( msgtext(msg_number), '   ' ) - 1 

	  if ( last_char .lt. 10 ) then
	    write (count,'(i1)') last_char
	  else
	    write (count,'(i2)') last_char
	  endif

	  format = '(a' // count // ')'  
	  write(0,format) msgtext(msg_number)

	else
c
c	  Unknown or Vendor dependent error
c
	  write(0,'("FTN Error - ",i4)') ftn_status

	endif

	return
	end
