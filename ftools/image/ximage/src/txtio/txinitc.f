      subroutine txinitc(filename, status)
      implicit none
c
c    Initializes all comments/commands in text i/o buffer 
c
c  I  filename  (s)  Text file location
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer status

      include 'txtio.inc'
      include '../include/io.inc'

      if ( status.ne.0 ) return

      if ( TXTqfile.ne.filename ) then
         call XWRITE(' txinitc: Mismatched filename', 10)
         status = -1
         return
      endif

      NQCom = 0

      return
      end
