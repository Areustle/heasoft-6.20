      subroutine getexohdr (Mapid, Header, Status)
      implicit none
c
c  Set integer*2 header for writing to exosat file
c
c  I  Mapid    (c)  Map to write
c  O  Header  (i*2) Exosat header
c  O  Status   (i)  Error flag (0 = OK)
c
      character*(*) Mapid
      integer*2 Header(64)
      integer*4 Status

      integer LENACT, DETIDX
c
c  Local variables
c
      character(2) hdrstr
      integer*4 i, szx, szy
      character(30) ctype1
      real*8 cdelt1, cdelt2, pixsize(2), zmx, zmy
      integer iddeg, idmin, irhr, irmin
      real*4 dsec, rsec
      character(30) telescop, instrume, detnam, filter
      integer year, daynum, mon, day, hr, imin
      real*8 sec

      integer*4 di
      real*8 dd
      character(100) ds
      logical ISDNULL
c
c  Blank Header
c
      do i = 1, 64
         Header(i) = 0
      enddo

      Status = 0

c
c  Set Header
c
c  Record specs
c     Header(1) = 'e$' or '$$'

      if ( mapid(1:2).eq.'EX' ) then
         hdrstr = 'e$'
      else
         hdrstr = '$$'
      endif
      call xmove(2, hdrstr, Header(1))

      call gheadi(Mapid, 'SZX', szx, 0, status)
      call gheadi(Mapid, 'SZY', szy, 0, status)

c  ?? setting header uses szx*2, while writer sets to szx
c     Header(2) = 2*szx
      Header(2) = szx

c  First data record
      Header(3) = 2

c  ??
c     Header(4) = ?
c     Header(5) = ?

c  Image scaling
c     Header(6) = BZERO (first 2 bytes)
c     Header(7) = BZERO (last 2 bytes)

      call gheadd(Mapid, 'BZERO', dd, 0, status)
      di = nint(dd)
      call xmove(4, di, Header(6))

c     Header(8) = BSCALE (first 2 bytes)
c     Header(9) = BSCALE (last 2 bytes)

      call gheadd(Mapid, 'BSCALE', dd, 0, status)
      di = nint(dd)
      call xmove(4, di, Header(8))

c  Bits per pixel (Defined, as bitpix, but never set)
c     Header(10) = bitpix

c  Image min and max
      call gheadd(Mapid, 'DATAMIN', dd, 0, status)
      Header(11) = MAX(MIN(nint(dd),32768), 0)
      call gheadd(Mapid, 'DATAMAX', dd, 0, status)
      Header(12) = MAX(MIN(nint(dd),32768), 0)

c  Image size (x,y)
      Header(13) = szx
      Header(14) = szy

c  Pixel size in arcsec (x,y)
      call gheads(Mapid, 'CTYPE1', ctype1, 0, status)
      call gheadd(Mapid, 'ZMX', zmx, 0, status)
      call gheadd(Mapid, 'ZMY', zmy, 0, status)
      call gheadd(Mapid, 'CDELT1', cdelt1, 0, status)
      call gheadd(Mapid, 'CDELT2', cdelt2, 0, status)
      i = LENACT(ctype1)
      if ( ctype1(i-2:i).ne.'TAN' ) then
         pixsize(1) = abs(cdelt1)
         pixsize(2) = abs(cdelt2)
      else
         pixsize(1) = abs(cdelt1)*3600.
         pixsize(2) = abs(cdelt2)*3600.
      endif

      IF ( int(pixsize(1)).NE.pixsize(1) .AND. pixsize(1).LE.300. ) 
     &         pixsize(1) = -pixsize(1)*100.
      IF ( int(pixsize(2)).NE.pixsize(2) .AND. pixsize(2).LE.300. ) 
     &         pixsize(2) = -pixsize(2)*100.
      Header(15) = nint(pixsize(1))
      Header(16) = nint(pixsize(2))

c  Image zoom (x,y)
      Header(17) = int(zmx)
      Header(18) = int(zmy)

c  Image center (x,y)
      call gheadd(Mapid, 'DRPIX1', dd, 0, status)
      Header(19) = nint(dd)
      call gheadd(Mapid, 'DRPIX2', dd, 0, status)
      Header(20) = nint(dd)

c  Reference pixel sky coordinates
      call gheadd(Mapid, 'CRVAL1', dd, 0, status)
      call chra(dd, irhr, irmin, rsec, 1)
      Header(21) = irhr
      Header(22) = irmin
      Header(23) = int(rsec)
      Header(24) = int((rsec - real(Header(23)))*100)
c Rounding disabled for sake of comparison with past values
c     Header(24) = nint((rsec - real(Header(23)))*100)

      call gheadd(Mapid, 'CRVAL2', dd, 0, status)
      call chdec(dd, iddeg, idmin, dsec, 1)
      Header(25) = iddeg
      Header(26) = idmin
      Header(27) = int(dsec*100.)
c Rounding disabled for sake of comparison with past values
c     Header(27) = nint(dsec*100.)

c  Roll angle
      call gheadd(Mapid, 'CROTA2', dd, 0, status)
      if ( ISDNULL(dd) ) dd = 0.
      dd = dd - 90.
      call CHDEC(dd, iddeg, idmin, dsec, 1)
      Header(28) = iddeg
      Header(29) = idmin
      Header(30) = nint(dsec)

c  Object name
c     Header(31-38) = OBJECT
      call gheads(Mapid, 'OBJECT', ds, 0, status)
      read (ds(1:16), '(8(a2))') (Header(i), i=31,38)

c  Energy limits
      call gheadi(Mapid, 'EMIN', di, 0, status)
      Header(39) = di
      call gheadi(Mapid, 'EMAX', di, 0, status)
      Header(40) = di

c  Detector/Filter Index
      call gheads(Mapid, 'TELESCOP', telescop, 0, status)
      call gheads(Mapid, 'INSTRUME', instrume, 0, status)
      call gheads(Mapid, 'DETNAM', detnam, 0, status)
      Header(41) = DETIDX(telescop, instrume, detnam)
      Header(42) = -2
      if ( telescop(1:6).eq.'EXOSAT' .and. instrume(1:3).eq.'CMA' ) then
         call gheads(Mapid, 'FILTER', filter, 0, status)
         call exoifil(filter, di, 2)
         Header(42) = di
      endif

c  Exposure
c     Header(43-44) = Exposure
      call gheadd(Mapid, 'EXPOSURE', dd, 0, status)
      call XMOVE(4,nint(dd),Header(43))

c  Observation times
      call gheads(Mapid, 'DATE-OBS', ds, 0, status)
      call fts2tm(ds,year,mon,day,hr,imin,sec,status)
      call getdaynum(mon, day, year, daynum)
      Header(45) = year
      Header(46) = daynum
      Header(47) = hr
      Header(48) = imin
      Header(49) = sec

      call gheads(Mapid, 'DATE-END', ds, 0, status)
      call fts2tm(ds,year,mon,day,hr,imin,sec,status)
      call getdaynum(mon, day, year, daynum)
      Header(50) = year
      Header(51) = daynum
      Header(52) = hr
      Header(53) = imin
      Header(54) = sec

c  North angle
      call gheadd(Mapid, 'XIMNORTH', dd, 0, status)
      call CHDEC(dd, iddeg,idmin,dsec,1)
      Header(55) = iddeg
      Header(56) = idmin
      Header(57) = nint(dsec)

c  Scaled sample rate dead time
      call gheadd(Mapid, 'DTIME', dd, 0, status)
      Header(58) = dd*real(10000)

c  Equinox
      call gheadd(Mapid, 'EQUINOX', dd, 0, status)
      if ( ISDNULL(dd) ) then
         Header(59) = 0
      else 
         Header(59) = dd
      endif

c  ??
c     Header(60) = ?

c  Image type (1=RB, 2=LC)
c  Event reader, set to 2
      Header(61) = 2

c  ??
c     Header(62) = ?
c     Header(63) = ?

c  In event reader, set to 1 if energy limits are set, 0 otherwise
c     Header(64) = ?

      return
      end
