C****************************************************************************
C SUBROUTINE:
C      faopen
C
C DESCRIPTION:
C      Open an ascii file with system dependent options
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       August, 1994
C
C MODIFICATION HISTORY:
C       Ron Zellar -- moved open statements to faopnw
C
C NOTES:
C
C USAGE:
C      call faopen (unit, filename, iomode, recl, status)
C
C ARGUMENTS:
C      unit     - input  - integer - unit number to open
C      filename - input  - string  - name of file to open ! to overwrite
C      iomode   - input  - integer - I/O mode to open file
C                                    1 - readonly
C                                    2 - new file (! overwrites if exists)
C                                    3 - read/write
C                                    4 - append
C      recl     - input  - integer - maximum record length <0 system default
C                                    (133 for VMS, ignored for unix)
C      status   - output - integer - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      subroutine faopen (unit, filename, iomode, recl, status)

      integer unit, iomode, recl, status
      character*(*) filename

      logical killit

      if (status .ne. 0) return

      if (filename(1:1) .eq. '!') then
         filename = filename(2:)
         killit = .true.
      else if (iomode .ne. 1) then
         call uclgsb ('clobber', killit, status)
         if (status .ne. 0) then
            killit = .false.
            status = 0
         endif
      endif

      if ((iomode .ne. 1) .and. (killit))
     &     call clobber (filename, status)
      if (status .ne. 0) goto 999

C     Open the file
      call faopnw (unit, filename, iomode, recl, status)

 999  return
      end


