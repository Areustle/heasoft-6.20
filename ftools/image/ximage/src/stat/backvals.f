      subroutine backvals(mapid, imgpix, detpix, arcmin, detpixsec,
     &                    arcminsec, status)
      implicit none
c
c  Print calculated background and box size used
c  NOTE: arcmin, detpixsec, arcminsec may be returned as RNULL()
c        if pixel size or exposure is invalid, use ISRNULL() to check
c
c  I  mapid     (s)  Map id string
c  O  imgpix    (r)  Background in cnts/image-pixel
c  O  detpix    (r)  Background in cnts/detector-pixel
c  O  arcmin    (r)  Background in cnts/square-arcmin
c  O  detpixsec (r)  Background in cnts/detector-pixel/sec
c  O  arcminsec (r)  Background in cnts/square-arcmin/sec
c  O  status    (i)  Error flag(0=OK)
c
      CHARACTER*(*) Mapid
      REAL*4 imgpix, detpix, arcmin, arcminsec, detpixsec
      INTEGER*4 Status

      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'
c
c  Local variables
c
      REAL*8 exposure, pixsiz1, pixsiz2, zmx, zmy
      REAL*4 RNULL
c
      Status = 0

      imgpix = BNEw
      call gheadd(mapid, 'ZMX', zmx, 0, status)
      call gheadd(mapid, 'ZMY', zmy, 0, status)
      call gheadd(mapid, 'CDELT1', pixsiz1, 0, status)
      call gheadd(mapid, 'CDELT2', pixsiz2, 0, status)
      pixsiz1 = abs(pixsiz1)*60.
      pixsiz2 = abs(pixsiz2)*60.
      call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
      detpix = imgpix/(zmx*zmy)
      if ( pixsiz1.eq.0.d0 .or. pixsiz2.eq.0.d0 ) then
         arcmin = RNULL()
      else
         arcmin = imgpix/(pixsiz1*pixsiz2)
      endif
      if ( exposure.le.0.d0 ) then
         detpixsec = RNULL()
         arcminsec = RNULL()
      else
         detpixsec = detpix/exposure
         arcminsec = imgpix/(pixsiz1*pixsiz2)/exposure
      endif
c
      RETURN
      END
