      subroutine bgcntreg(Ireg, Map, Szx, Szy, Zmx, Zmy, Xcen, Ycen, 
     &                    Bgmap, Bgszx, Bgszy, Bgzmx, Bgzmy, Bgxcen,
     &                    Bgycen, Wgt, Sum, Area, Status)
      implicit none
c
c  Sum counts in an area specified by region (loaded with xinitreg or
c     added with xboxreg - ximreg.c) and return 
c     sum of values Map(i,j)/Bgmap(i,j) for all i,j in region
c     Note: This result must be multiplied by the absolute weight,
c           usually the Bgmap value corresponding to the center of
c           the area.  
c
c     To get raw counts with no background correction Bgmap should
c     contain values of 1.0 
c
c  I  Ireg      (i)  Region number
c  I  Map       (r)  Image map
c  I  Szx/y     (i)  Size of map
c  I  Zmx/y     (d)  Rebin of map
c  I  X/Ycen    (d)  Center of map in detector coords
c  I  Bgmap     (r)  Image map
c  I  Bgszx/y   (i)  Size of map
c  I  Bgzmx/y   (d)  Rebin of map
c  I  Bgx/ycen  (d)  Center of map in detector coords
c  I  Wgt       (r)  Weight to rescale background to
c  O  Sum       (r)  Sum of values Map(i,j)*Wgt/Bgmap(i,j) for all i,j 
c                    in region
c  O  Area      (r)  Number of pixels in counted region
c  O  Status    (i)  Error flag (0 = OK)
c
      integer*4 Ireg, Szx, Szy
      integer*4 Bgszx, Bgszy, Status
      real*4 Map(Szx,Szy), Bgmap(Bgszx,Bgszy), Wgt, Sum, Area
      real*8 Xcen, Ycen, Zmx, Zmy, Bgxcen, Bgycen, Bgzmx, Bgzmy

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer i, j, ixmin, ixmax, iymin, iymax, bgi, bgj
      real*4 xmin, xmax, ymin, ymax, rx, ry, ri, rj
      real*8 dxmin, dxmax, dymin, dymax
      real*8 dx, dy
      logical good, ISINREG, isrnull, isdnull
c
c  Determine bounding box for speed optimization
c  Only check pixels in bounding box to see if in region 
c  If region is unbounded, check all pixels in image
c
      call bboxreg(Ireg, dxmin, dxmax, dymin, dymax, Status)
      if ( Status.ne.0 ) return

      if ( isdnull(dxmin) .or. isdnull(dxmax) .or.
     &     isdnull(dymin) .or. isdnull(dymax) ) then
         ixmin = 1
         ixmax = Szx
         iymin = 1
         iymax = Szy
      else 
         xmin = dxmin
         xmax = dxmax
         ymin = dymin
         ymax = dymax

         call calimgpix(Szx,Szy,Zmx,Zmy,Xcen,Ycen,xmin,ymin,
     &                  ri,rj,2)
         ixmin = MIN(Szx,MAX(1,nint(ri)))
         iymin = MIN(Szy,MAX(1,nint(rj)))
         call calimgpix(Szx,Szy,Zmx,Zmy,Xcen,Ycen,xmax,ymax,
     &                  ri,rj,2)
         ixmax = MAX(1,MIN(Szx,nint(ri)))
         iymax = MAX(1,MIN(Szy,nint(rj)))
      endif
c
c  Calculate sum and area
c
      Sum = 0.
      Area = 0.
      do i = ixmin, ixmax
         do j = iymin, iymax
            ri = i
            rj = j
            call calimgpix(Szx,Szy,Zmx,Zmy,Xcen,Ycen,rx,ry,
     &                     ri,rj,1)
            dx = rx
            dy = ry
            good = isinreg(ireg,dx,dy)
            if ( good ) then
               call calimgpix(Bgszx,Bgszy,Bgzmx,Bgzmy,Bgxcen,Bgycen,
     &                        rx,ry,ri,rj,2)
               bgi = NINT(ri)
               bgj = NINT(rj)
               if ( Bgi.ge.1 .and. Bgi.le.Bgszx .and.
     &              Bgj.ge.1 .and. Bgj.le.Bgszy ) then
                  if ( .not.isrnull(Map(i,j)) ) then
                     Sum = Sum + Map(i,j)*Wgt/Bgmap(bgi,bgj)
                     Area = Area + 1.
                  endif
               else
                  call xwarn(' Out of BGMAP bounds', 10)
               endif
            endif
         enddo
      enddo

      return
      end
