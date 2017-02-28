	subroutine ftio_errmsg  ( ftio_status )
c
c       Routine to print ftio error message to standard error
c       $Id: ftio_errmsg.f,v 3.3 2013/05/21 19:08:27 irby Exp $
c       $Log: ftio_errmsg.f,v $
c       Revision 3.3  2013/05/21 19:08:27  irby
c       Change character*n to character(n) to silence warnings: "Obsolescent
c       feature: Old-style character length".
c
c       Revision 3.2  2006/04/17 20:54:27  irby
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
c       Revision 3.1  2002/04/16 20:32:10  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/03/27  18:50:14  wang
c Initial revision
c
c
        implicit none

        include 'ftio.h'

        integer*4 ftio_status, len_trim, len

        character(64) errmsg(ftio_ok:ftio_syserr)

        data errmsg(ftio_ok)          /' FTIO OK'/
        data errmsg(ftio_bad_length)  
     &               /' FTIO Bad record length'/
	data errmsg(ftio_incstnt_len) 
     &              /' FTIO Inconsistent length in BDW or SDW'/
	data errmsg(ftio_bad_unit)    
     &              /' FTIO Bad or unknown I/O Unit number'/
	data errmsg(ftio_no_memory)   
     &              /' FTIO Insufficient memory'/
	data errmsg(ftio_open_failed) 
     &              /' FTIO Open failed'/ 
	data errmsg(ftio_bad_blksize) 
     &              /' FTIO Illegal blocksize'/
	data errmsg(ftio_bad_recfm)   
     &              /' FTIO Illegal record format'/
	data errmsg(ftio_no_buffers)  
     &              /' FTIO Unable to get buffer memory'/
	data errmsg(ftio_write_error) 
     &              /' FTIO Write error'/
	data errmsg(ftio_eof)         
     &              /' FTIO End Of File'/
	data errmsg(ftio_nounits)     
     &              /' FTIO No free I/O units'/
	data errmsg(ftio_notopen)     
     &              /' FTIO File not open'/
	data errmsg(ftio_syserr)      
     &              /' FTIO System Error - see errmsg'/


        if ( ftio_status .lt. ftio_ok .and. ftio_status 
     &                   .gt. ftio_syserr ) then
          write(0,'(" Error status is out of range for FTIO")')
        else
          len = len_trim ( errmsg(ftio_status) )
          write(0,10) errmsg(ftio_status)
10        format(' ',a)
        endif

        return
        end


