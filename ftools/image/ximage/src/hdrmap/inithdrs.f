      subroutine inithdrs
      implicit none
c
c  Initialize all headers defined in header.inc
c
      include '../include/maxvals.inc'
      include '../include/mapdef.inc'

      integer i

      do i = 1, MAX_MAPS
         call cphead(' ', MAPids(i))
      enddo

      return
      end
