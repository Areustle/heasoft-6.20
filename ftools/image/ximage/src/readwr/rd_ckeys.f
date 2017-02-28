      subroutine rd_ckeys (Lun, Xi, Yi, Cenpix, Ctype, Cunit, Crval,
     &                     Crpix, Cdelt, Crota2)
      implicit none
c
c  Get coordinate information from C-kewords:
c  CTYPE1/2, CUNIT1/2, CRVAL1/2 CRPIX1/2, CDELT1/2, CROTA2
c
c  Let fitsio routines take care of CD matrix translation, etc.
c
c  I  Lun     (i)  Logical unit of open FITS file
c  I  Xi      (i)  Index for X coordinate (image = 1, event = X column)
c  I  Yi      (i)  Index for Y coordinate (image = 2, event = Y column)
c  I  Cenpix  (d)  Image center
c  O  Ctype   (c)  Coordinate system
c  O  Cunit   (c)  Units of pixel
c  O  Crval   (d)  Sky coordinates of reference pixel
c  O  Crpix   (d)  Reference pixel
c  O  Cdelt   (d)  Pixel size
c  O  Crota2  (d)  Rotation
c
      integer*4 Lun, Xi, Yi
      character*(*) Ctype(2), Cunit(2)
      real*8 Cenpix(2), Crval(2), Crpix(2), Cdelt(2), Crota2
      
      real*8 dnull
      logical isdnull
c      
c Local variables
c
      integer*4 status, tstatus, warnmsg, nfound
      character(4) type
      character(80) keyname, comment
      
      status = 0
      warnmsg = 20
c
      Ctype(1) = ' '
      Ctype(2) = ' '
      Cunit(1) = ' '
      Cunit(2) = ' '
      Crpix(1) = dnull()
      Crpix(2) = dnull()
c
c  Try table keywords first
c
      call ftkeyn('TCRVL',Xi,keyname,status)
      call ftgkyd(Lun, keyname, Crval(1), comment, status)
      call ftkeyn('TCRVL',Yi,keyname,status)
      call ftgkyd(Lun, keyname, Crval(2), comment, status)

      call ftkeyn('TCRPX',Xi,keyname,status)
      call ftgkyd(Lun, keyname, Crpix(1), comment, status)
      call ftkeyn('TCRPX',Yi,keyname,status)
      call ftgkyd(Lun, keyname, Crpix(2), comment, status)

      call ftkeyn('TCDLT',Xi,keyname,status)
      call ftgkyd(Lun, keyname, Cdelt(1), comment, status)
      call ftkeyn('TCDLT',Yi,keyname,status)
      call ftgkyd(Lun, keyname, Cdelt(2), comment, status)

      call ftkeyn('TCTYP',Xi,keyname,status)
      call ftgkys(Lun, keyname, Ctype(1), comment, status)
      call ftkeyn('TCTYP',Yi,keyname,status)
      call ftgkys(Lun, keyname, Ctype(2), comment, status)
c
c  TCROT key is optional
c
      Crota2 = 0.
      tstatus = 0
      call ftkeyn('TCROT',Yi,keyname,tstatus)
      call ftgkyd(Lun, keyname, Crota2, comment, tstatus)

c  Otherwise, try image keywords
c
      if ( status.eq.0 ) then
         call XWRITE (' Using coordinate keys from table', warnmsg)
         call ftgkns(Lun, 'TCUNI', Xi, 1, Cunit(1), nfound, status)
         status = 0
         call ftgkns(Lun, 'TCUNI', Yi, 1, Cunit(2), nfound, status)
         status = 0
      else
         status = 0
c
c  Use cfitsio to calculate CDELT, CROTA2 (but overwrite other C-keys)
c
         call ftgics(Lun, Crval(1), Crval(2), Crpix(1), Crpix(2),
     &               Cdelt(1), Cdelt(2), Crota2, type, status)
         if ( status.ne.0 ) then
            call xwarn('Skew exists, CD matrix conversion is '
     &                 //'poorly approximated', 5)
            status = 0
         endif

         Crpix(1) = dnull()
         Crpix(2) = dnull()
         call ftgkyd(Lun, 'CRPIX1', Crpix(1), comment, status)
         call ftgkyd(Lun, 'CRPIX2', Crpix(2), comment, status)
         status = 0

         call ftgkyd(Lun, 'CRVAL1', Crval(1), comment, status)
         call ftgkyd(Lun, 'CRVAL2', Crval(2), comment, status)
         status = 0

         call ftgkys(Lun, 'CTYPE1', Ctype(1), comment, status)
         call ftgkys(Lun, 'CTYPE2', Ctype(2), comment, status)
         status = 0

         call ftgkys(Lun, 'CUNIT1', Cunit(1), comment, status)
         call ftgkys(Lun, 'CUNIT2', Cunit(2), comment, status)
         status = 0

      endif
c
c Set reference pixel to image pixel center if not found
c
      if ( isdnull(Crpix(1)) .or. isdnull(Crpix(2)) ) then
         Crpix(1) = Cenpix(1)
         Crpix(2) = Cenpix(2)
      endif
c
c  Check squareness of pixels
c
      if ( abs(Cdelt(1)).ne.abs(Cdelt(2)) ) then
         call XWARN(' Pixels are not square: do not rotate image',
     &              warnmsg)
      endif
c
c  Check that cdelt is not zero to avoid div by zero later
c
      if ( Cdelt(1).eq.0.d0 .or. Cdelt(2).eq.0.d0 ) then
         Cdelt(1) = 1.d0
         Cdelt(2) = 1.d0
      endif

      return
      end
