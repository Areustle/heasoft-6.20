      SUBROUTINE GET_EXPOSURE(Exmap,Szx,Szy,Exmapid,Xpix,Ypix,
     &                        Hbox,Exposure)
      IMPLICIT NONE
c
c return the correct exposure, from the exposure map
c
c  I  exmap    (r)  Exposure map
c  I  szx/y    (i)  Size of map
c  I  exmapid  (s)  Exposure map id string
c  I  x/ypix   (r)  Center of box
c  I  hbox     (r)  Half box size
c  O  exposure (d)  Exposure time
c
      integer*4 Szx, Szy
      real*4 Exmap(Szx,Szy)
      character*(*) Exmapid
      real*4 Xpix, Ypix, Hbox
      real*8 Exposure
c
c  Local variables
c
      integer*4 ixexp , iyexp , ipix, status
      integer*4 di
      real*8 xcen, ycen, zmx, zmy
      real*4 ximg, yimg
      integer*4 x, y, hbox_exp
      logical isrnull
c
      call get_refram(exmapid, di, di, zmx, zmy, xcen, ycen, status)
c
c When the exposure map is calculate from sum imges
c so the exposure map one to one the image 
c X/Y undefined... commenting out
c
c       IF ( Exp_map_header(60).EQ.4 ) THEN
c        ixexp = X
c        iyexp = Y
c        hbox_exp = hbox
c       ELSE
c
c otherwise get the zoom of the exposure header
c
      call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,Xpix,Ypix,ximg,yimg,2)
c     ixexp = ximg
c     iyexp = yimg
      ixexp = NINT(ximg)
      iyexp = NINT(yimg)
      hbox_exp = hbox/zmx
      ipix = 0
      x = ixexp - hbox_exp
      Exposure = 0.0
c
c loop around and average the exposure over the box
c
      do while ( x .le. ixexp+hbox_exp )
         y = iyexp - hbox_exp
         do while( y .le. iyexp+hbox_exp )
c
c if the exposure map is rebinned compare with the image
c

            if (x.gt.0 .and. y.gt.0 .and. x.le.Szx .and. y.le.Szy ) then
c              Exposure = Exmap(x,y)*60.0+Exposure
               if ( .not.isrnull(Exmap(x,y)) ) then
                  Exposure = Exmap(x,y)+Exposure
                  ipix = ipix + 1
               endif
            endif
            y = y + 1
         enddo
         x = x + 1
      enddo
      if (ipix.gt.0) Exposure = Exposure/float(ipix)
      RETURN
      END
