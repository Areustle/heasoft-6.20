      subroutine set_vpcbnds(Maxvps, Vpconfig, Vpconnum, Vpset)
      implicit none
c
c  Oct. 28, 1998
c  Sets viewport to bounds of viewports defined in the configuration
c
c  I  maxvps   (i) Maximum number of viewports in configuration
c  I  vpconfig (r) Viewport configuration
c  I  vpconnum (i) Number of viewports in configuration
c  O  vpset    (r) Viewport
c
      integer Maxvps, Vpconnum
      real*4 Vpconfig(Maxvps,4), Vpset(4)
c
c  Local variables
c
      integer i

      if ( Vpconnum.eq.0 ) return
      Vpset(1) = 1.0
      do i = 1,Vpconnum
         if ( vpconfig(i,1).lt.Vpset(1) ) then
            Vpset(1) = Vpconfig(i,1)
         endif
      enddo
      Vpset(2) = 0.0
      do i = 1,Vpconnum
         if ( Vpconfig(i,2).gt.Vpset(2) ) then
            Vpset(2) = Vpconfig(i,2)
         endif
      enddo
      Vpset(3) = 1.0
      do i = 1,Vpconnum
         if ( Vpconfig(i,3).lt.Vpset(3) ) then
            Vpset(3) = Vpconfig(i,3)
         endif
      enddo
      Vpset(4) = 0.0
      do i = 1,Vpconnum
         if ( Vpconfig(i,4).gt.Vpset(4) ) then
            Vpset(4) = Vpconfig(i,4)
         endif
      enddo

      return
      end
