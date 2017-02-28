*+OPFITS
c     ------------------------------------------------------
      subroutine opfits(unit,filename,killit,chatter,status)
c     ------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------------
c Open a new FITS file, deleting an old one of the same name if requested
c --- VARIABLES ----------------------------------------------------------
c
      IMPLICIT NONE
      integer unit, status,chatter
      character*(*) filename
      logical killit
c
c --- ARGUMENTS ---
c
c      unit     - input  - integer - unit number to open
c      filename - input  - string  - name of file to open ! to overwrite
c      status   - output - integer - status of operation
c      chatter  - input  - integer - user info printed at hight chatter
c      killit   - input  - logical - if true then filename is clobbered
c
c --- MODIFICATION HISTORY ----------------------------------------------
c
c Rehana Yusaf (1.0.0: Sep 12 1994), Emily Greene's FFINIT used as a
c                                    basis, but with additional killit 
c                                    argument
c Rehana Yusaf (1.0.1; Dec 14 1995) add wtinfo and friends
      character(5) version
      parameter (version='1.0.1')
*-
c -----------------------------------------------------------------------
c
c --- INTERNALS ---
c
      character(6) subname
      parameter (subname='opfits')
      character(70) subinfo

      if (status .ne. 0) return

      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,20,2,subinfo)

      if (filename(1:1) .eq. '!') then
         filename = filename(2:)
         killit = .true.
      endif

      if (killit) call clobber (filename, status)
      if (status .ne. 0) then
         subinfo = ' clobbering existing file'
         call wterrm(subname,version,subinfo)
         goto 999
      endif

      call ftinit (unit, filename, 2880, status)

 999  return
      end
c -------------------------------------------------------------------
c     END OF OPFITS
c -------------------------------------------------------------------

