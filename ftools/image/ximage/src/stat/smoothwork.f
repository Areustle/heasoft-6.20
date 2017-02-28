      SUBROUTINE SMOOTHWORK(Map,Szx,Szy,Mapid,Sigpix,
     &                      Scaling_factor,Smooth_x,Smooth_y,
     &                      Wavesmth,Round,Status)
      IMPLICIT NONE
c
c      IMAGE SMOOTHING
C
C      L.CHIAPPETTI - ESOC - JUN 84 <850319.1118>
C      VERSION 1 MOD 1
C Vx   ORIGINAL ADAPTMENT FROM RB ROUTINE (FAULTY IF N .NE. 256)
C VxM1 ABOVE BUG CORRECTED, ALSO BUG IN LOWER CORNER CORRECTED !
C VxM2 RTE 6 CHANGES
C VxM3 INTERACTIVE SYSTEM CHANGES - P.G. -
C      FINALISED FOR LE IA ON -841214.1826-
C V1M1 NOW SCALING FACTOR HAS PHYSICAL MEANING
C      ALSO SUITABLE DEFAULT IS PROVIDED
C
c   VAX/VMS version  12/2/90 GTh
c
c   adapted to work from BROWSE and XIMAGE by P.G. on 13/2/90
c   added exposure maps NEW 1 sep 1993
c
c  I  map            (r)  Image map
c  I  szx/y          (i)  Size of maps
c  I  mapid          (s)  Map id string
c  I  sigpix         (r)  Sigma of the gaussian in image pixels
c  I  scaling_factor (r)  Scale factor used to increase precision
c  I  smooth_x       (l)  Whether to smooth x direction
c  I  smooth_y       (l)  Whether to smooth y direction
c  I  wavesmth       (l)  Whether to use wavelet smoothing
c  I  round          (l)  Whether to round map values (i.e. NINT(x))
c  I  status         (i)  Error flag (0 = OK)
c
      integer*4 Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid
      real*4 Sigpix, Scaling_factor
      logical Smooth_x, Smooth_y, Wavesmth, Round

      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER*4 i , j, p_mapbuf, p_sbuf, p_dbuf, bufsz
      REAL*4 rmin, rmax, anrm, rnull
      REAL*8 dmin, dmax, mpscal
      LOGICAL isrnull

      write(ZWRite,*) ' Scaling factor : ', Scaling_factor
      call XWRITE(ZWRite, 10)
C
C  smoothing part
C
c  When calling SMTH the original image will be passed to the
c  subroutine. If SMTH returns to the calling program the
c  smoothed image is stored in MAP.
c
      bufsz = MAX(Szx,Szy)
      call ralloc(1, bufsz, 1, p_sbuf, status)
      if ( status.eq.0 ) call ralloc(1, bufsz, 1, p_dbuf, status)
      if ( status.eq.0 ) call ralloc(1, Szx, Szy, p_mapbuf, status)
      if ( status.ne.0 ) then
         call XWRITE(' Unable to allocate temporary buffers for smooth',
     &               10)
         return
      endif

      CALL SMTH(Map,memr(p_mapbuf),Szx,Szy,memr(p_sbuf),memr(p_dbuf),
     &          bufsz,Sigpix,Scaling_factor,anrm,Smooth_x,Smooth_y,
     &          Wavesmth)

      call ralloc(0, bufsz, 1, p_sbuf, status)
      call ralloc(0, bufsz, 1, p_dbuf, status)
      call ralloc(0, Szx, Szy, p_mapbuf, status)

      write(ZWRite,'(a,1x,f12.5)') ' Internal normalisation factor : ',
     &                             anrm
      call XWRITE(ZWRite, 15)
c
C  get max and min of smoothed image
C
      call gheadd(mapid, 'DATAMAX', dmax, 0, status)
c
c  Find min/max (NOTE: smooth command loses nulls, so don't
c                      bother checking for them)
c
      rmin = rnull()
      rmax = rnull()

      do i = 1, Szx
         do j = 1, Szy
            if ( Round ) Map(i,j) = NINT(Map(i,j))
            if ( isrnull(rmin) .or. Map(i,j).lt.rmin )
     &         rmin = Map(i,j)
            if ( isrnull(rmax) .or. Map(i,j).gt.rmax )
     &         rmax = Map(i,j)
         enddo
      enddo

      write(ZWRite,*) ' Min and max : ', rmin, rmax
      call XWRITE(ZWRite, 10)

      if ( rmax.ne.0. ) then
         mpscal = Scaling_factor*dmax/rmax
         write(ZWRite,'(a,1x,f12.7)') 
     &        ' Max-preserving scaling factor : ', mpscal
         call XWRITE(ZWRite, 10)
      else
         call xwrite(' Map all zero, No max-preserving '//
     &                'scaling factor', 10)
      endif
      dmin = rmin
      dmax = rmax

      call gheadd(mapid, 'DATAMIN', dmin, 1, status)
      call gheadd(mapid, 'DATAMAX', dmax, 1, status)
c
c  After smoothing, map type depends on Round value
c
      if ( Round ) then
         call gheads(mapid, 'MAPTYPE', 'I', 1, status)
      else
         call gheads(mapid, 'MAPTYPE', 'R', 1, status)
      endif

      RETURN
      END
