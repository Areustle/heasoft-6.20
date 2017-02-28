*+CFRELUN
 	subroutine cfrelun(lun)

	implicit none
 	integer lun

C-----------------------------------------------------------------------
C Description: Releases a Logical Unit Number
C
C Arguments:   lun  (i): the logical unit number to be released
C
C Origin:      Swiped from XPI libraries to support IRAF build of ftools
C
C Authors/Modification History:
C AFT Feb 13 1989 -- Original version?
C Ron Zellar Aub 29, 1994 -- renamed cfrelun so that it does not
C                            conflict with frelun in XPI. removed print
C                            statements and other non-IRAF stuff.
C-----------------------------------------------------------------------

      integer status, fcstln
      logical opened
      character(255) fname
	character(4) cval
	character(160) contxt

      status = 0
      inquire (unit=lun,opened=opened)
      if (opened) then
         inquire (unit=lun,name=fname)
         call fcerr(' ')
	 write(cval,'(I4)')lun
	 contxt='frelun called with unit '//cval
         call fcerr(contxt)
	 contxt='on a file which is still open named '
	 call fcerr(contxt)
	 call fcerr(fname(1:fcstln(fname)))
      end if
      call ftfiou(lun,status)
      RETURN
      END
