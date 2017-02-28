      subroutine txinit(status)
      implicit none
c
c  Initialize text i/o buffer
c
c  O  status   (i)  Error flag (Initialized to 0)
c
      integer status

      include 'txtio.inc'
c
c  Local variables
c
      integer i

      txtqfile = ' '
      txtqmode = -1
      nqrows = -1
      nqcol = 0
      nqpts = 0
      nqcom = 0
      do i = 1, mxqcol
         qerr(i) = -1
      enddo
      status = 0
 
      return
      end
