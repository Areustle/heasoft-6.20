C****************************************************************************
C SUBROUTINE:
C      ffinit
C
C DESCRIPTION:
C      Open a new FITS file, deleting an old one of the same name if requested
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       August, 1994
C
C MODIFICATION HISTORY:
C
C   26May98: (MJT)
C      If 'clobber' is true (either because of a clobber parameter set
C      to "true" or because the filename starts with "!") pass "!filename"
C      to ffinit (which is now simply the wrapped CFITSIO ffinit) and then
C      strip it back off again.
C
C NOTES:
C      FITS blocksize is created to be 2880 bytes
C
C USAGE:
C      call ffinit (unit, filename, status)
C
C ARGUMENTS:
C      unit     - input  - integer - unit number to open
C      filename - input  - string  - name of file to open ! to overwrite
C      status   - output - integer - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      subroutine ffinit (unit, filename, status)

      integer unit, status
      character*(*) filename
      character(512) tmpfnam

      logical killit

      if (status .ne. 0) return

C check for clobber parameter in parameter file
      call uclgsb ('clobber', killit, status)
      if (status .ne. 0) then
         killit = .false.
         status = 0
      endif

      if (filename(1:1) .eq. '!') then
         filename = filename(2:)
         killit = .true.
      endif

      if (killit) then
         tmpfnam = '!'//filename
      else
         tmpfnam = filename
      endif

      call ftinit (unit, tmpfnam, 2880, status)

 999  return
      end
