      SUBROUTINE imgpix(mapid, xpix, ypix, ximg, yimg, mode, status)
      implicit none
c
c subrotine to convert pixel coordintes from the image array to the 
c original detector array and viceversa
c  I    Mapid    s   map type
c  I/O  xpix     r   x detector pixel return in mode 1
c  I/O  ypix     r   y detectot pixel
c  I/O  ximg     r   x image pixels  return in mode 2
c  I/O  yimg     r   y image pixels
c  I    mode     i   mode 1=img to detector 2=det 2img
c  O    status   i   error return
c
c Input variable
      INTEGER*4 mode, status
      REAL*4 xpix, ypix, ximg, yimg
      REAL*8 dximg, dyimg, dxpix, dypix
      CHARACTER*(*) Mapid
c
c Local variable
      INTEGER*4 szx, szy
      REAL*8 xcen, ycen, zmx, zmy 

      include '../include/maxvals.inc'
      character*(MAX_IDSTR) wcsid
c
      call gheads(Mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         call get_refram(Mapid, szx,szy,zmx,zmy, xcen,ycen, status)
         call calimgpix(szx,szy,zmx,zmy,xcen,ycen,xpix,ypix,ximg,yimg,
     &                  mode)
      else
         if ( mode.eq.1 ) then
            dximg = ximg
            dyimg = yimg
            call wcsimgpix(wcsid, dximg, dyimg, dxpix, dypix, 1, status)
            xpix = dxpix
            ypix = dypix
         else if ( mode.eq.2 ) then
            dxpix = xpix
            dypix = ypix
            call wcsimgpix(wcsid, dximg, dyimg, dxpix, dypix, 0, status)
            ximg = dximg
            yimg = dyimg
         else
            status = -1
            return
         endif
         
      endif

      return 
      end
