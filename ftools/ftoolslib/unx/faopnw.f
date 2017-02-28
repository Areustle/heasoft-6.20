C****************************************************************************
C SUBROUTINE:
C      faopnw
C
C DESCRIPTION:
C      Open an ascii file with system dependent options on unix platforms
C
C AUTHOR:  
C       Ron Zellar
C       Hughes STX
C       Oct, 1994
C
C MODIFICATION HISTORY:
C	Original code came from the faopen routine written by Emily Greene
C
C NOTES:
C
C USAGE:
C      call faopnw (unit, filename, iomode, recl, status)
C
C ARGUMENTS:
C      unit     - input  - integer - unit number to open
C      filename - input  - string  - name of file to open ! to overwrite
C      iomode   - input  - integer - I/O mode to open file
C                                    1 - readonly
C                                    2 - new file
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
      subroutine faopnw (unit, filename, iomode, recl, status)

      integer unit, iomode, recl, status
      character*(*) filename

      if (status .ne. 0) return

C and open the file!

      if (iomode .eq. 1) then
         open (unit, file=filename, status='old', iostat=status)
      else if (iomode .eq. 2) then
         open (unit, file=filename, status='new', iostat=status)
      else if (iomode .eq. 3) then
         open (unit, file=filename, status='unknown', iostat=status)
      else if (iomode .eq. 4) then
         open (unit, file=filename, status='unknown', iostat=status)

C append access is not supported on all unix systems

 10      read (unit, '(a)', end=999)
         goto 10
      endif

 999  return
      end


