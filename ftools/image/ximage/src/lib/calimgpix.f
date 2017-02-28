      SUBROUTINE calimgpix(szx, szy, zmx, zmy, xcen, ycen, xpix, ypix, 
     &                     ximg, yimg, mode)
      implicit none
c
c subrotine to convert pixel coordintes from the image array to the 
c original detector array and viceversa
c   I   szx      i   image size in x
c   I   szy      i   image size in y
c   I   zmx      d   image zoom in x
c   I   zmy      d   image zoom in y
c   I   xcen     d   image center in x
c   I   ycen     d   image center in y
c  I/O  xpix     r   x detector pixel return in mode 1
c  I/O  ypix     r   y detectot pixel
c  I/O  ximg     r   x image pixels  return in mode 2
c  I/O  yimg     r   y image pixels
c  I    mode     i   mode 1=img to detector 2=det 2img
c
c Input variable
      INTEGER*4 szx, szy
      REAL*8 xcen, ycen, zmx, zmy 
      REAL*4 xpix, ypix, ximg, yimg
      INTEGER*4 mode
c
c if error occurs check the variable mixture
c
c image 2 pixel
      If (mode.eq.1 ) then
c
c UNDO -- My opinion: The prior code had a 0.5 off error
c         because of inaccuracy of the integer*2 header
c         in holding the center.  This int(x/ycen) code
c         maintains that innaccuracy for comparison purposes,
c         and should eventually be removed.
c
c Matches old version (from integer edge to integer edge)
c        xpix = (ximg - szx/2.)*zmx + int(xcen)
c        ypix = (yimg - szy/2.)*zmy + int(ycen)

c Mixed version (from integer edge to integer center)
c        xpix = (ximg - szx/2.)*zmx + xcen
c        ypix = (yimg - szy/2.)*zmy + ycen

c Correct version (from integer center to integer center)
         xpix = (ximg - (float(szx)/2.+0.5))*zmx + xcen
         ypix = (yimg - (float(szy)/2.+0.5))*zmy + ycen

      elseif(mode.eq.2) then 
c
c pixel 2 image 
c Matches old version (from integer edge to integer edge)
c        ximg = (xpix - int(xcen))/zmx + szx/2.
c        yimg = (ypix - int(ycen))/zmy + szy/2.

c Mixed version (from integer center to integer edge)
c        ximg = (xpix - xcen)/zmx + szx/2.
c        yimg = (ypix - ycen)/zmy + szy/2.

c Correct version (from integer center to integer center)
c
c  Avoid divide by zero... just assume rebin of 1
c
         if ( zmx.gt.0.d0 ) then
            ximg = (xpix - xcen)/zmx + szx/2. + 0.5
         else
            ximg = (xpix - xcen) + szx/2. + 0.5
         endif
         if ( zmy.gt.0 ) then
            yimg = (ypix - ycen)/zmy + szy/2. + 0.5
         else
            yimg = (ypix - ycen) + szy/2. + 0.5
         endif

      endif 
      return 
      end
