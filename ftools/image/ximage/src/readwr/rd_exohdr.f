      subroutine rd_exohdr(Filename, Mapid, Nbyte, Lun, Status)
      implicit none
c
c  Open an image file of type exosat with direct access
c  without knowing the record length in advance
c  Then, reads header and sets internal XIMAGE header
c  (From RDHEA routine)
c
c  I  Filename    (c) Name of exosat file
c  I  Mapid       (c) Map to set header 
c  I  Nbyte       (i) Number of total bytes in image
c  O  Lun         (i) Logical unit of opened file
c  O  Status      (i) Error flag  ( 0 = OK )
c
      character*(*) Filename, Mapid
      integer*4 Nbyte, Lun, Status

      include '../include/startup.inc'
c
c  Local variables
c
      integer*4 i, j, LENACT
      character(2) id
      integer*4 ire, k, ri, ilr, inr, ireh
      integer*2 header(64)

      integer*4 ideg, ihr, imin
      real*4 rsec
      integer*4 iyr, idaynum, imon, iday
      real*8 dsec
      character(16) object
      integer*4 szx, szy
      real*8 cdelt1, cdelt2, zmx, zmy
      integer numfilts
      parameter (numfilts = 8)

      integer*4 di, di1, di2
      real*8 dd
      character(100) ds
      real*8 DNULL

      Status = 0

      do i = 1, 64
         header(i) = 0
      enddo

      call getlun(Lun)

      CALL OPENWR(Lun,Filename,'OLD','D',' ',Nbyte,1,Status)
      IF ( Status.NE.0 ) THEN
         CALL XWRITE('Error opening exosat file with record length',5)
         CALL XWRITE(Filename,5)
         RETURN
      ENDIF
c
      Status = 0

      READ (Lun,REC=1,ERR=100) id
      IF ( id.NE.'$$' .and. id.NE.'e$') THEN
         CALL XWRITE(' Invalid exosat file: no header', 5)
         CALL XWRITE(Filename, 5)
         Status = -2
         RETURN
      ENDIF
c
c   Because of the length of the record in the case of exosat image
c   is given by the size of the image this can be smaller than the lenght
c   of the header record (which need 64 integer*2 to be defined). In the
c   following bit of code first is checking if the length is smaller than 64
c   and if it is, then the number of records to reach 64 i*2 is calculated and
c   then read..
c
      ire = Nbyte/2
c
      k = 0
      ri = ire/64. - 1
      IF ( ri.LT.0 ) THEN
         inr = 64/ire
         ilr = ire
      ELSE
         inr = 1
         ilr = 64
      ENDIF

      DO 50 i = 1 , inr
         READ (Lun,REC=i,ERR=100) (header(j),j=k+1,k+ilr)
         k = j - 1
 50   CONTINUE
c
      ireh = header(2)
      IF ( ireh.NE.ire ) header(2) = ire
c
c  Save header data into internal XIMAGE header
c

c    Header(1) = '$$' or 'e$'
c    Header(2) = Record length
c    Header(3) = First record
c    Header(4) = ?
c    Header(5) = ?
c
c Image scaling  (ignore)
c    Header(6,7) = BZERO
c    Header(8,9) = BSCALE

c     call XMOVE(4, header(6), di)
c     dd = di
      dd = 0.
      call gheadd(Mapid, 'BZERO', dd, 1, status)
c     call XMOVE(4, header(8), di)
c     dd = di
      dd = 1.
      call gheadd(Mapid, 'BSCALE', dd, 1, status)

c Bits per pixel
c    Header(10) = BITPIX

c Image min and max
c    Header(11) = DATAMIN
c    Header(12) = DATAMIN

      dd = header(11)
      call gheadd(Mapid, 'DATAMIN', dd, 1, status)
      dd = header(12)
      call gheadd(Mapid, 'DATAMAX', dd, 1, status)

c Image size (Already set in header by mapalloc)
c    Header(13) = SZX
c    Header(14) = SZY

      szx = header(13)
      szy = header(14)

c Pixel size (arcsec)
c    Header(15) = CDELT1/3600
c    Header(16) = CDELT2/3600

      di = header(15)
      if ( di.lt.0 ) di = -di/100
      cdelt1 = -float(di)/3600.
      call gheadd(Mapid, 'CDELT1', cdelt1, 1, status)
      di = header(16)
      if ( di.lt.0 ) di = -di/100
      cdelt2 = float(di)/3600.
      call gheadd(Mapid, 'CDELT2', cdelt2, 1, status)

c Image zoom
c    Header(17) = ZMX
c    Header(18) = ZMY

      zmx = header(17)
      call gheadd(Mapid, 'ZMX', zmx, 1, status)
      zmy = header(18)
      call gheadd(Mapid, 'ZMY', zmy, 1, status)

c Image center (Reference pixel)
c    Header(19) = DRPIX1
c    Header(20) = DRPIX2

      dd = header(19)
      call gheadd(Mapid, 'DRPIX1', dd, 1, status)
      dd = header(20)
      call gheadd(Mapid, 'DRPIX2', dd, 1, status)
      
c Reference pixel in sky coordinates
c    Header(21-24) = CRVAL1
c    Header(25-27) = CRVAL2

      ihr = header(21)
      imin = header(22)
      rsec = real(header(23)) + real(header(24))/100.
      call chra(dd, ihr, imin, rsec, 0)
      call gheadd(Mapid, 'CRVAL1', dd, 1, status)
      ideg = header(25)
      imin = header(26)
      rsec = real(header(27))/100.
      call chdec(dd, ideg, imin, rsec, 0)
      call gheadd(Mapid, 'CRVAL2', dd, 1, status)

c Roll angle
c    Header(28-30) = CROTA2

      ideg = header(28)
      imin = header(29)
      rsec = header(30)
      call chdec(dd, ideg, imin, rsec, 0)
c     call gheadd(Mapid, 'XIMROLL', dd, 1, status)
      dd = dd + 90.
      call gheadd(Mapid, 'CROTA2', dd, 1, status)

c Object name
c    Header(31-38) = OBJECT

      call XMOVE(16, header(31), object)
      call gheads(Mapid, 'OBJECT', object, 1, status)

c Energy limits
c    Header(39) = EMIN
c    Header(40) = EMAX

      di = header(39)
      call gheadi(Mapid, 'EMIN', di, 1, status)
      di = header(40)
      call gheadi(Mapid, 'EMAX', di, 1, status)

c Telescope/Instrument
c    Header(41) = TELESCOPE/INSTRUME code
c    Header(42) = FILTER code

      di = header(41)
      if ( di.gt.0 .and. di.le.ZEXpnum ) then
         ds = ZTElescop(di)
         call UPC(ds)
         if ( ds.ne.'EXOSAT' ) header(42) = -2
         call gheads(Mapid, 'TELESCOP', ds, 1, status)
         ds = ZINstrume(di)
         call UPC(ds)
         call gheads(Mapid, 'INSTRUME', ds, 1, status)
      else
         call gheads(Mapid, 'TELESCOP', 'UNKNOWN', 1, status)
         call gheads(Mapid, 'INSTRUME', 'UNKNOWN', 1, status)
      endif
      di = header(42)
      if ( di.gt.0 ) then
         call exoifil(ds, di, 1)
         call gheads(Mapid, 'FILTER', ds, 1, status)
      endif

c Exposure time
c    Header(43-44) = EXPOSURE

      call XMOVE(4, header(43), di)
      dd = di
      call gheadd(Mapid, 'EXPOSURE', dd, 1, status)

c Observation times
c    Header(45-49) = DATE-OBS
c    Header(50-54) = DATE-END

      iyr = header(45)
      idaynum = header(46)
      call xydmd(iyr, idaynum, imon, iday)
      ihr = header(47)
      imin = header(48)
      dsec = header(49)
      status = 0
      call fttm2s(iyr, imon, iday, ihr, imin, dsec, 0, ds, status)
      call gheads(Mapid, 'DATE-OBS', ds, 1, status)
      
      iyr = header(50)
      idaynum = header(51)
      call xydmd(iyr, idaynum, imon, iday)
      ihr = header(52)
      imin = header(53)
      dsec = header(54)
      status = 0
      call fttm2s(iyr, imon, iday, ihr, imin, dsec, 0, ds, status)
      call gheads(Mapid, 'DATE-END', ds, 1, status)

c North angle
c    Header(55-57) = NORTH

      ideg = header(55)
      imin = header(56)
      rsec = header(57)
      call chdec(dd, ideg, imin, rsec, 0)
      call gheadd(Mapid, 'XIMNORTH', dd, 1, status)

c Dead time correction
c    Header(58) = DTIME

      dd = real(header(58))/10000.
      call gheadd(Mapid, 'DTIME', dd, 1, status)

c Equinox
c    Header(59) = EQUINOX

      dd = header(59)
      if ( dd .le. 0.d0 ) dd = DNULL()
      call gheadd(Mapid, 'EQUINOX', dd, 1, status)

c    Header(60) = ?
c    Header(61) = Image type (1=RB, 2=LC) {Event reader sets to 2}
c    Header(62) = ?
c    Header(63) = ?
c    Header(64) = ? {Event rdr sets to 1 if energy limits, 0 otherwise}

c
c  Derived values
c
      call DIRPOS(Filename, di1, di2)
      ds = Filename(di2+1:LENACT(Filename))
      call gheads(Mapid, 'FILE', ds, 1, status)
      di = INDEX(ds, '.')
      if ( di.eq.0 ) then
         di = LENACT(ds)
      else
         di = di - 1
      endif
      call gheads(Mapid, 'ROOT', ds(1:di), 1, status)
      call gheads(Mapid, 'CTYPE1', 'RA---TAN', 1, status)
      call gheads(Mapid, 'CTYPE2', 'DEC--TAN', 1, status)
      dd = real(szx)/2.0
      call gheadd(Mapid, 'CRPIX1', dd, 1, status)
      dd = real(szy)/2.0
      call gheadd(Mapid, 'CRPIX2', dd, 1, status)
      dd = cdelt1/zmx
      call gheadd(Mapid, 'DDELT1', dd, 1, status)
      dd = cdelt2/zmy
      call gheadd(Mapid, 'DDELT2', dd, 1, status)
c
c  Set MAPTYPE to integer
c
      call gheads(Mapid, 'MAPTYPE', 'I', 1, status)

      Status = 0

      RETURN
 
c  error conditions detected
c
 100  CONTINUE
      Status = -1
      CALL XWRITE(' Error reading exosat header',10)
      RETURN
      END
