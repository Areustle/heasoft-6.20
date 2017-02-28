      subroutine flipwork (Map, Work, Szx, Szy, Mode, Status)
      implicit none
c
c  Core routine for flipping image horizontally or vertically
c
c I/O Map    (r)  Image map
c I/O Work   (r)  Work map
c  I  Szx/y  (i)  Size of map
c  I  Mode   (i)  Flip mode (1=horiz 2=vert)
c
      integer*4 Szx, Szy, Mode, Status
      real*4 Map(Szx,Szy), Work(Szx,Szy)
c
c  Local variables
c
      integer*4 i, j

      if ( mode.eq.1 ) then

        do i = 1, Szx
           do j = 1, Szy
              Work(Szx-i+1,j) = Map(i,j)
           enddo
        enddo

      elseif ( mode.eq.2 ) then

        do i = 1, Szx
           do j = 1, Szy
              Work(i,Szy-j+1) = Map(i,j)
           enddo
        enddo

      else
         call XWRITE(' Invalid flip mode', 5)
         Status = -1
      endif

      return
      end
