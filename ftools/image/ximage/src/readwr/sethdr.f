      subroutine sethdr(Mapid,Wcsid,Filename,Ximscale,Ximzero,Unit,
     &                  Datamin,Datamax,Szx,Szy,Totzm,Crpix,Crval,
     &                  Object,Emin,Emax,Telescop,Instrume,Detnam,
     &                  Filter,Exposure,Dtcor,Vignapp,Begobs,Endobs,
     &                  Maptype)
     
      implicit none

      include '../include/startup.inc'
c
c  Set double, integer, and string headers
c
c  I  Mapid     (c)  Which header is to be set
c  I  Wcsid     (c)  Coordinate data structure id
c  I  Filename  (c)  Filename
c  I  Ximscale  (d)  Image scaling applied
c  I  Ximzero   (d)  Image offset applied
c  I  Unit      (c)  Unit of image values
c  I  Datamin   (d)  Minimum data value
c  I  Datamax   (d)  Maximum data value
c  I  Szx       (i)  Image size in X
c  I  Szy       (i)  Image size in Y
c  I  Totzm     (d)  Total zoom
c  I  Crpix     (d)  Original image Crpix
c  I  Crval     (d)  Original reference pixel (e.g. ra,dec)
c  I  Object    (c)  Object name
c  I  Emin      (i)  Minimum energy channel
c  I  Emax      (i)  Maximum energy channel
c  I  Telescop  (i)  Telescope
c  I  Instrume  (i)  Instrument
c  I  Detnam    (i)  Detector name
c  I  Filter    (c)  Filter
c  I  Exposure  (d)  Exposure time (sec)
c  I  Dtcor     (d)  Dead time correction
c  I  Vignapp   (l)  Whether vignetting has been applied (exposure map)
c  I  Begobs    (c)  Date for observation begin (yyyy-mm-ddThh:mm:ss)
c  I  Endobs    (c)  Date for observation end   (yyyy-mm-ddThh:mm:ss)
c  I  Maptype   (s)  Image equinox
c
      character*(*) Mapid, Wcsid, Filename, Unit
      real*8 Ximzero, Ximscale, Datamin, Datamax
      integer*4 Szx, Szy
      real*8 Totzm(2), Crpix(2), Crval(2)
      character*(*) Object, Telescop, Instrume, Detnam, Filter 
      integer Emin, Emax
      real*8 Exposure, Dtcor
      logical Vignapp
      character*(*) Begobs, Endobs, Maptype
c
c  Local variables
c
      integer*4 mode, status, di1, di2, LENACT
      character(80) ds
      real*8 dd

      mode = 1
c
c  Set coordinate keywords from wcs data structure
c
      call wcstohdr(wcsid, mapid, status)
c
c  Set Double Header
c
*     call cphead(' ', Mapid)
c
c  Record applied scaling as XIMSCALE, XIMZERO
c  Set scaling to be applied as BSCALE=1.0, BZERO=0.0
c
c    Currently, BSCALE/BZERO don't really do anything
c    but they have been left in as they may be needed
c    if we decide to use values scaled from integer
c    map in calculations. SCALE and DISPLAY have been
c    modified in anticipation of this possibility.
c    However, LEVELS, POLYGON, CONTOUR, DETECT, SOSTA,
c    etc. will need modifications to use BSCALE and BZERO
c    in calculations
c
c     dd = 1.0/Ximscale
      dd = 1.0
      call gheadd(Mapid, 'BSCALE', dd, mode, status)
c     dd = -Ximzero
      dd = 0.0
      call gheadd(Mapid, 'BZERO', dd, mode, status)
      call gheads(Mapid, 'BUNIT', Unit, mode, status)
      call gheadd(Mapid, 'XIMSCALE', Ximscale, mode, status)
      call gheadd(Mapid, 'XIMZERO', Ximzero, mode, status)
      call gheadd(Mapid, 'DATAMIN', Datamin, mode, status)
      call gheadd(Mapid, 'DATAMAX', Datamax, mode, status)
      call gheadd(Mapid, 'OCRPIX1', Crpix(1), mode, status)
      call gheadd(Mapid, 'OCRPIX2', Crpix(2), mode, status)
      call gheadd(Mapid, 'OCRVAL1', Crval(1), mode, status)
      call gheadd(Mapid, 'OCRVAL2', Crval(2), mode, status)
      call gheadd(Mapid, 'EXPOSURE', Exposure, mode, status)
      call gheadd(Mapid, 'DTIME', Dtcor, mode, status)
c
c  Set Integer Header
c
      call gheadi(Mapid, 'LOADED', 1, mode, status)
      call gheadi(Mapid, 'SZX', Szx, mode, status)
      call gheadi(Mapid, 'SZY', Szy, mode, status)
      call gheadd(Mapid, 'ZMX', Totzm(1), mode, status)
      call gheadd(Mapid, 'ZMY', Totzm(2), mode, status)
      call gheadi(Mapid, 'EMIN', Emin, mode, status)
      call gheadi(Mapid, 'EMAX', Emax, mode, status)
      if ( Vignapp ) then
         call gheadi(Mapid, 'VIGNAPP', 1, mode, status)
      else
         call gheadi(Mapid, 'VIGNAPP', 0, mode, status)
      endif
c
c  Set String Header
c
      call DIRPOS(Filename, di1, di2)
      ds = Filename(di2+1:LENACT(Filename))
      call gheads(Mapid, 'FILE', ds, mode, status)
      di1 = INDEX(ds, '.')
      if ( di1.eq.0 ) then
         di1 = LENACT(ds)
      else
         di1 = di1 - 1
      endif
      call gheads(Mapid, 'ROOT', ds(1:di1), 1, status)
      call gheads(Mapid, 'OBJECT', Object, mode, status)
      call gheads(Mapid, 'TELESCOP', Telescop, mode, status)
      call gheads(Mapid, 'INSTRUME', Instrume, mode, status)
      call gheads(Mapid, 'DETNAM', Detnam, mode, status)
      call gheads(Mapid, 'FILTER', Filter, mode, status)
      call gheads(Mapid, 'DATE-OBS', Begobs, mode, status)
      call gheads(Mapid, 'DATE-END', Endobs, mode, status)
      call gheads(Mapid, 'MAPTYPE', Maptype, mode, status)
      
      return
      end
