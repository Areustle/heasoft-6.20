      SUBROUTINE VIGNETTING(Cmdid,Mapid, Status)
      IMPLICIT NONE
c
c  Correct exposure map for vignetting
c
c  I  cmdid   (i)  Command id
c  I  mapid   (s)  Map id string
c  O  status  (i)  Status
c
      character*(*) Mapid
      integer*4 Cmdid, Status

      include '../include/io.inc'
      include '../include/maxvals.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER*4 argc, i , j
      REAL*4 vcor , large , small , xpix , ypix, dr
      REAL*8 zmx, zmy, xcen, ycen, exposure, dd
      character(100) telstr
      integer*4 ivign, itel, p_vign
      integer*4 exmapptr, szx, szy
      real*4 val
      character*(MAX_IDSTR) exmapid, wcsid, cpwcsid
      logical isloaded, isrnull, first

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      if ( status.ne.0 ) return
c
c multiplication exposure map vignetting
c
      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         return
      endif

      exmapid = ' '
      call gheads(mapid, 'EXMAPID', exmapid, 0, status)
      if ( exmapid.eq.' ' ) then
          call xwrite(' Generating flat exposure map to be corrected',
     &                10)
          call getexmap(mapid, exmapid, status)
          if ( status.ne.0 ) return
          call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
          call gheadi(mapid, 'SZX', szx, 0, status)
          call gheadi(mapid, 'SZY', szy, 0, status)
          call cphead(mapid, exmapid)
          call gheadd(exmapid, 'DATAMIN', exposure, 1, status)
          call gheadd(exmapid, 'DATAMAX', exposure, 1, status)
          call gheadi(exmapid, 'MAPPTR', -1, 1, status)
          call gheads(exmapid, 'MAPTYPE', 'R', 1, status)
          call gheads(exmapid, 'MAPCOPY', ' ', 1, status)
          call gheads(exmapid, 'FILE', 'vignetting', 1, status)
          call gheads(exmapid, 'WCSID', wcsid, 0, status)
          call gheads(exmapid, 'WCSID', ' ', 1, status)
          call mapalloc(szx, szy, exmapid, exmapptr, status)

          call copywcs(wcsid, MAX_IDSTR, cpwcsid, status)
          call gheads(exmapid, 'WCSID', cpwcsid, 1, status)

          do i = 1, szx
             do j = 1, szy
                dr = exposure
                call mapvalset(memr(exmapptr),szx,szy,i,j,dr)
             enddo
          enddo
          call gheads(mapid, 'EXMAPID', exmapid, 1, status)
      endif

      call gheadi(exmapid, 'VIGNAPP', ivign, 0, status)
      IF ( ivign.eq.1 ) THEN
         WRITE (ZWRite,
     &          '(''Exposure map has been corrected for vignetting'')')
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,'(''The correction will not be applied '')')
         CALL XWRITE(ZWRite,10)
         RETURN
      ENDIF

      call gheadi(exmapid, 'MAPPTR', exmapptr, 0, status)
      call get_itel(exmapid, itel)
      call get_telstr(exmapid, telstr)
      call get_refram(exmapid, szx, szy, zmx, zmy, xcen, ycen, status)
      large = -99999999.0
      small = 99999999.0

      IF ( itel.GT.0 ) THEN
         WRITE (ZWRite,'(''Exposure map from : '',a)') telstr
         CALL XWRITE(ZWRite,10)
      ELSE
         CALL XWRITE('Detector Unknown',10)
         CALL XWRITE('No vignetting applied',10)
         RETURN
      ENDIF
c
c  Initializations for get_vign
c
      first = .TRUE.
      p_vign = -1

      DO 100 i = 1 , Szx
         xpix = (i-Szx/2)*zmx + xcen
         DO 50 j = 1 , Szy
            ypix = (j-Szy/2)*zmy + ycen
            CALL GET_VIGN(Mapid,xpix,ypix,itel,vcor,first,p_vign,Status)
            IF ( vcor.GT.0 ) THEN
               call mapvalget(memr(exmapptr),szx,szy,i,j,val)
               if ( .not.isrnull(val) ) then
                  val = val/vcor
                  call mapvalset(memr(exmapptr),szx,szy,i,j,val)
               endif
            ELSE
               val = 0.0
               call mapvalset(memr(exmapptr),szx,szy,i,j,val)
            ENDIF
            if ( val.gt.large ) large = val
            if ( val.lt.small ) small = val
 50      CONTINUE
 100  CONTINUE
c
c  Free vignetting map from get_vign
c
      call ralloc(0, -1, -1, p_vign, status)
c
c update header
c
      dd = small
      call gheadd(exmapid, 'DATAMIN', dd, 1, status)
      dd = large
      call gheadd(exmapid, 'DATAMAX', dd, 1, status)
      call gheadi(exmapid, 'VIGNAPP', 1, 1, status)
      call gheadi(exmapid, 'EXPMAP', 1, 1, status)
      ZWRite = 'Exposure map corrected for the vignetting'
      CALL XWRITE(ZWRite,10)
      RETURN
      END
