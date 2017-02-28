      SUBROUTINE HISTOMAX(Map, Szx, Szy, Bxcen, Bycen, Boxrad, 
     &                    Xout, Yout)
      IMPLICIT NONE
c
c  Determine new center for box by histogram of selected region
c
c  I  map      (r)  Image map
c  I  szx/y    (i)  Size of image
c  I  bgx/ycen (r)  Center of box in image coordinates
c  I  boxrad   (r)  Half size of box in image coordinates
c  O  x/yout   (r)  Point of maximum intensity in image coor.
c
      integer*4 Szx, Szy
      real*4 Map(Szx,Szy)
      REAL*4 Bxcen, Bycen, Boxrad, Xout, Yout

      include '../include/io.inc'

      REAL*4 binsum, max_binsum, rnull
      INTEGER i, j, ilef, irit, itop, ibot
      LOGICAL isrnull
C
      Xout = Bxcen
      Yout = Bycen

      irit = MIN(int(Bxcen + Boxrad - 0.5), Szx)
      itop = MIN(int(Bycen + Boxrad - 0.5), Szy)
      ilef = MAX(int(Bxcen - Boxrad + 0.5), 1)
      ibot = MAX(int(Bycen - Boxrad + 0.5), 1)
      
     
c Find maximum  x bin
      
      max_binsum = rnull()
      do i = ilef, irit
         binsum = rnull()
         do j = ibot, itop
            if ( .not.isrnull(Map(i,j)) ) then
               if ( isrnull(binsum) ) then
                  binsum = Map(i,j)
               else
                  binsum = binsum + Map(i,j)
               endif
            endif
         enddo
         write(ZWRite,*) ' Column ', i, ' : ', binsum
         call xwrite(ZWRite, 30)
         if ( .not.isrnull(binsum) ) then
            if ( isrnull(max_binsum) .or. binsum.gt.max_binsum ) then
               max_binsum = binsum
               Xout = real(i)
            endif
         endif
      enddo
 
c Find maximum  y bin

      max_binsum = rnull()
      do j = ibot, itop
         binsum = rnull()
         do i = ilef, irit
            if ( .not.isrnull(Map(i,j)) ) then
               if ( isrnull(binsum) ) then
                  binsum = Map(i,j)
               else
                  binsum = binsum + Map(i,j)
               endif
            endif
         enddo
         write(ZWRite,*) ' Row ', j, ' : ', binsum
         call xwrite(ZWRite, 30)
         if ( .not.isrnull(binsum) ) then
            if ( isrnull(max_binsum) .or. binsum.gt.max_binsum ) then
               max_binsum = binsum
               Yout = real(j)
            endif
         endif
      enddo

      return
      end
