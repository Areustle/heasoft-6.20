      subroutine getbgmap (Mapid, Bgmap, Bgsz, Status)
      implicit none
c
c  Read in FITS background map corresponding to current map's
c  detector.  If non-existent generate flat map internally
c
c  I  Mapid   (s)  Map id string
c  O  Bgmap   (r)  Background map
c  I  Bgsz    (i)  Size of map
c  O  Status  (i)  Error flag (0 = OK)
c
      character*(*) Mapid
      integer*4 Bgsz, Status
      real*4 Bgmap(Bgsz,Bgsz)

      include '../include/maxvals.inc'
      include '../include/sitedef.inc'
      include '../include/startup.inc'
c
c  Local variables
c
      character*(MAX_IDSTR) bgmapid
      integer i, j, itel
      character*(MAX_FILELEN) mapfil
      integer*4 szx, szy, di
      real*8 imgcen(2), detcen(2), crpix(2), drpix(2), datamin, datamax
      real*8 imgref(2), detref(2), dzm(2), bgzmx, bgzmy, zmx, zmy
      logical there

      bgmapid = 'BGMAP'
      status = 0

      call get_itel(mapid, itel)
      call lkupcal(itel, '*_background.fits', mapfil, status)
      there = status.eq.0

      if ( there ) then
         call XWRITE(' Reading background map', 15)
         call rd_fitsmap(mapfil, Bgmap, Bgsz, Bgsz, imgref, detref, dzm, 
     &                   datamin, datamax, status)
         if ( status.ne.0 ) return
         bgzmx = dzm(1)
         bgzmy = dzm(2)
      else
         call XWRITE (' Generating flat background map...',15)
         call gheadd(mapid, 'CRPIX1', crpix(1), 0, status)
         call gheadd(mapid, 'CRPIX2', crpix(2), 0, status)
         call gheadd(mapid, 'DRPIX1', drpix(1), 0, status)
         call gheadd(mapid, 'DRPIX2', drpix(2), 0, status)
         call gheadd(mapid, 'ZMX', zmx, 0, status)
         call gheadd(mapid, 'ZMY', zmy, 0, status)
         call gheadi(mapid, 'SZX', szx, 0, status)
         call gheadi(mapid, 'SZY', szy, 0, status)
         imgcen(1) = dble(szx)/2.0 + 0.5
         imgcen(2) = dble(szy)/2.0 + 0.5
         detcen(1) = (imgcen(1) - crpix(1))*zmx + drpix(1)
         detcen(2) = (imgcen(2) - crpix(2))*zmy + drpix(2)

         call gheadi(mapid, 'SZX', di, 0, status)
c        zmx = MAX(1,zmx*di/Bgsz)
c        zmx = zmx*di/Bgsz + 1
         bgzmx = zmx*float(di)/float(Bgsz)
         if ( bgzmx*Bgsz.lt.zmx*di ) bgzmx = bgzmx + 1.
         call gheadi(mapid, 'SZY', di, 0, status)
c        zmy = MAX(1,zmy*di/Bgsz)
c        zmy = zmy*di/Bgsz + 1
         bgzmy = zmy*float(di)/float(Bgsz)
         if ( bgzmy*Bgsz.lt.zmy*di ) bgzmy = bgzmy + 1.

         imgref(1) = float(Bgsz)/2. + 0.5
         imgref(2) = float(Bgsz)/2. + 0.5
         detref(1) = detcen(1)
         detref(2) = detcen(2)

         do i = 1, Bgsz
            do j = 1, Bgsz
               Bgmap(i,j) = 1.
            enddo
         enddo
         datamin = 1.
         datamax = 1.
      endif

      call gheadi(bgmapid, 'LOADED', 1, 1, status)
      call gheadi(bgmapid, 'SZX', Bgsz, 1, status)
      call gheadi(bgmapid, 'SZY', Bgsz, 1, status)
      call gheadd(bgmapid, 'ZMX', bgzmx, 1, status)
      call gheadd(bgmapid, 'ZMY', bgzmy, 1, status)
      call gheadd(bgmapid, 'CRPIX1', imgref(1), 1, status)
      call gheadd(bgmapid, 'CRPIX2', imgref(2), 1, status)
      call gheadd(bgmapid, 'DRPIX1', detref(1), 1, status)
      call gheadd(bgmapid, 'DRPIX2', detref(2), 1, status)
      call gheadd(bgmapid, 'DATAMIN', datamin, 1, status)
      call gheadd(bgmapid, 'DATAMAX', datamax, 1, status)

      return
      end
