      subroutine ghdwcs(mapid, crval, drpix, ddelt, crota2, imgequ,
     &                  wcstyp, status)
      implicit none
c
c  Retrieves keyword values from header which are needed by CFITSIO
c  routines, ftwldp and ftxypx, for transformations to and from a
c  world coordinate system.
c
c  I  mapid   (s)  Map identifier
c  O  crval   (d)  Reference value keys
c  O  drpix   (d)  Reference pixel keys
c  O  ddelt   (d)  Pixel size keys
c  O  crota2  (d)  Rotation key
c  O  imgequ  (i)  Image equinox
c  O  wcstyp  (s)  Coordinate system id - last 4 chars of CTYPE1
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) mapid, wcstyp
      real*8 crval(2), drpix(2), ddelt(2), crota2
      integer imgequ, status
c
c  Local variables
c
      integer*4 LENACT
      real*8 cdelt(2), zmx, zmy, dd
      character(80) ctype
      logical ISDNULL

      status = 0

      call gheadd(mapid, 'CRVAL1', crval(1), 0, status)
      call gheadd(mapid, 'CRVAL2', crval(2), 0, status)
      call gheadd(mapid, 'DRPIX1', drpix(1), 0, status)
      call gheadd(mapid, 'DRPIX2', drpix(2), 0, status)
      call gheadd(mapid, 'CDELT1', cdelt(1), 0, status)
      call gheadd(mapid, 'CDELT2', cdelt(2), 0, status)
      call gheadd(mapid, 'ZMX', zmx, 0, status)
      call gheadd(mapid, 'ZMY', zmy, 0, status)
      ddelt(1) = cdelt(1)/zmx
      ddelt(2) = cdelt(2)/zmy
      call gheadd(mapid, 'CROTA2', crota2, 0, status)
      if ( ISDNULL(crota2) ) crota2 = 0.
      call gheadd(mapid, 'EQUINOX', dd, 0, status)
      if ( ISDNULL(dd) ) then
         imgequ = 2000
      else
         imgequ = NINT(dd)
      endif
      call gheads(mapid, 'CTYPE1', ctype, 0, status)
      wcstyp = ctype(lenact(ctype)-3:lenact(ctype))

      return
      end
