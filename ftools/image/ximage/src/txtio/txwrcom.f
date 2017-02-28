      subroutine txwrcom(filename, comstring, status)
      implicit none
c
c    Appends a comment/command to text i/o buffer 
c
c  I  filename  (s)  Text file location
c  I  comstring (s)  Comment/command string
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename, comstring
      integer status

      include 'txtio.inc'
      include '../include/io.inc'

      if ( status.ne.0 ) return

      if ( TXTqfile.ne.filename ) then
         call txinit(status)
         TXTqfile = filename
      endif

      if ( NQCom+1.gt.MXQcom ) then
         call XWRITE(' txwrcom: Too many comments/commands', 10)
         status = -1
         return
      endif

      NQCom = NQCom + 1

      QCOm(NQCom) = comstring

      return
      end
