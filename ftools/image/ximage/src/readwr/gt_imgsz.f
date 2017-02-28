      subroutine gt_imgsz (Naxes, Usrzm, Ctype, Cdelt, 
     &                     Szxstr, Szystr, Maxdefsz, Szx, Szy)
      implicit none
c
c   Determine size of final image
c
c  I  Naxes    (i) Size of original image
c  I  Usrzm(2) (i) Rebin factor
c  I  Ctype    (c) Coordinate system
c  I  Cdelt    (d) Pixel size in pixel coords
c  I  Szxstr   (c) User-entered size (preserves decimal in arcmin case)
c  I  Szystr   (c) User-entered size (preserves decimal in arcmin case)
c  I  Maxdefsz (i) Maximum default size of image
c I/O Szx      (i) Size of image in x (if < 0, arcmin)
c I/O Szy      (i) Size of image in y (if < 0, arcmin)
c
      integer*4 Naxes(3)
      character*(*) Szxstr, Szystr, Ctype(2)
      real*8 Usrzm(2), Cdelt(2)
      integer*4 Maxdefsz, Szx, Szy

      include '../include/io.inc'
c
c  Local variables
c
      integer*4 i, status, LENACT
      real*4 dr
      real*8 tmpreb, dd

      call XWRITE(' gt_imgsz: ', 30)
      if ( Szx.lt.0 .or. Szy.lt.0 ) then
         call xwrite(' Negative size interpreted as arcmin', 10)
         write(ZWRite,'(4a)') ' Coordinate type: ', Ctype(1)(1:8),
     &                        ' ', Ctype(2)(1:8)
         call xwrite(ZWRite, 15)
      endif

      if ( Szx.lt.0 ) then
         call strnum(Szxstr, 4, dd, status)
         tmpreb = MAX(1.d0,Usrzm(1))
         dr = -(dd/60.)/abs(Cdelt(1)*tmpreb)
         Szx = int(float(nint(dr))/2.0)*2
         if ( Szx.lt.2 ) Szx = 2
      endif
      if ( Szy.lt.0 ) then
         call strnum(Szystr, 4, dd, status)
         tmpreb = MAX(1.d0,Usrzm(2))
         dr = -(dd/60.)/abs(Cdelt(2)*tmpreb)
         Szy = int(float(nint(dr))/2.0)*2
         if ( Szy.lt.2 ) Szy = 2
      endif

      if ( Szx.eq.0 .and. Szy.eq.0 ) then
         if ( Usrzm(1).gt.0.d0 .and. Usrzm(2).gt.0.d0 ) then
            Szx = NINT(dble(Naxes(1))/Usrzm(1))
            Szy = NINT(dble(Naxes(2))/Usrzm(2))
         else
            if ( Maxdefsz.gt.0 .and. 
     &           (Naxes(1).gt.Maxdefsz .or. Naxes(2).gt.Maxdefsz) ) then
               write(ZWRite, *) 'Image size is larger than maximum ',
     &                          ' default of ', Maxdefsz
               call rmvxbk(ZWRite(2:))
               call xwarn(ZWRite, 10)
               call xwarn('Specify the image size or rebin for '//
     &                     'full image', 10)
            endif
            if ( Maxdefsz.eq.0 ) then
               Szx = Naxes(1)
               Szy = Naxes(2)
            else
               Szx = MIN(Naxes(1), Maxdefsz)
               Szy = MIN(Naxes(2), Maxdefsz)
            endif
         endif
      endif
c
c  Failsafe for Naxes = 0 (Can happen with event files)
c
      if ( Usrzm(1).le.0.d0 ) Usrzm(1) = 1.d0
      if ( Usrzm(2).le.0.d0 ) Usrzm(2) = 1.d0
      if ( Szx.eq.0 ) Szx = 256
      if ( Szy.eq.0 ) Szy = 256
c
c  Force even-sized image
c
      Szx = MAX(2,int(Szx/2)*2)
      Szy = MAX(2,int(Szy/2)*2)

      return
      end
