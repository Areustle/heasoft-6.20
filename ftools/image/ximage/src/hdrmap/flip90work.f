      subroutine flip90work (Map, Work, Szx, Szy, Mode, Status)
      implicit none
c
c  Core routine for rotating by +/-90
c
c I/O Map    (r)  Image map
c I/O Work   (r)  Result map
c  I  Szx/y  (i)  Size of map
c  I  Mode   (i)  Flip mode (3=+90 4=-90)
c
      integer*4 Szx, Szy, Mode, Status
      real*4 Map(Szx,Szy), Work(Szy,Szx)
c
c  Local variables
c
      integer*4 i, j

      if ( mode.eq.3 ) then

         do i = 1, Szx
            do j = 1, Szy
               Work(j,Szx-i+1) = Map(i,j)
            enddo
         enddo

      elseif ( mode.eq.4 ) then

         do i = 1, Szx
            do j = 1, Szy
               Work(Szy-j+1,i) = Map(i,j)
            enddo
         enddo

      else

         call XWRITE(' Invalid flip mode', 5)
         Status = -1

      endif

      return
      end
