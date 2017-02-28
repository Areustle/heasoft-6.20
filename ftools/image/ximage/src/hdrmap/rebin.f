      subroutine rebin(Cmdid,Mapid,Status)
      implicit none
c
c  Rebin image
c
c  I  Cmdid   (i)  Command id
c  I  Mapid   (s)  Map id string
c  O  Status  (i)  Error return
c
      INTEGER*4 Cmdid, Status
      CHARACTER*(*) Mapid

      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
c
c  Local variables
c
      INTEGER*4 mode, rebinfac
      REAL*8 usrreb, cenpix(2), newzm(2)

      INTEGER*4 argc
      LOGICAL isloaded
      INTEGER*4 newszx, newszy, lenact
      integer szx, szy, mapptr, exmapptr, newptr
      character*(MAX_IDSTR) wcsid, exmapid, exwcsid
      real*8 datamin, datamax
      logical expmap
      character(100) ds
      character(80) mapcodes
c
c defaults and inits.
c
      usrreb = 2.d0
      mode = 1

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      CALL GPARI(Cmdid,'MODE',mode,Status)
      CALL GPARD(Cmdid,'REBINNING_FACT',usrreb,Status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         status = -1
         return
      endif

      if ( MOD(usrreb,1.d0).ne.0.d0 ) then
         call xwrite(' Specified rebin must be an integer',10)
         call xwrite(' Use remap command to rebin by non-integer', 10)
         status = -1
         return
      endif
      rebinfac = int(usrreb)
c
c mode (0 1 2) =0 smooth the image for the specified rebin
c                 (the image will end with the same size, same pixels size too)
c                 the count rate in each pixels is obtained averaging
c                 the counts found in the piuxels involved in the rebinning
c                 (es rebin=2 4pixels ..)
c              =1 this is a "normal" rebin. the pixels and the image size
c                 change by the rebinning factor
c              =2 the pixels size change but the image size stay the original
c                 (the image scrink in the frame). use for mosaic
c
      if ( mode.lt.0 .or. mode.gt.2 ) then
         call XWRITE(' Valid modes are: ', 10)
         call XWRITE('  0 - Smooth for rebinning factor '//
     &               '(image size unchanged)', 10)
         call XWRITE('  1 - Image and pixel size change by factor', 10)
         call XWRITE('  2 - Pixel size changes by factor (image '//
     &               'shrinks in frame)', 10)
         Status = -1
         return
      endif

      IF ( mode.EQ.0 ) THEN
         CALL XWRITE(' Please note that rebinning in this way ',10)
         CALL XWRITE(' the poissonian statistics will be destroyed',10)
         CALL XWRITE(' Do not run DETECT on this image ',10)
      ENDIF

      IF ( rebinfac.LE.0 ) THEN
         CALL XWRITE('>>>>> Enter "rebin n" or "rebin/rebinning=n',10)
         CALL XWRITE('             where n > 0',10)
         Status = 1
         return
      ENDIF

c
c  Check map size
c
      call gheadi(mapid, 'SZX', szx, 0, status)
      call gheadi(mapid, 'SZY', szy, 0, status)
      if ( mode.eq.1 ) then
         newszx = INT(dble(szx/rebinfac)/2.d0)*2
         newszy = INT(dble(szy/rebinfac)/2.d0)*2
         if ( newszx.lt.2 .or. newszy.lt.2 ) then
            call XWRITE(' Size after rebin is too small', 10)
            status = -1
            return
         endif
      endif
c
      call gheadi(mapid, 'MAPPTR', mapptr, 0, status)
      call gheads(mapid, 'WCSID', wcsid, 0, status)

      expmap = .FALSE.
      call rebinwork(mode, rebinfac, memr(mapptr), szx, szy,
     &               expmap, datamin, datamax)
      call gheadd(mapid, 'DATAMIN', datamin, 1, status)
      call gheadd(mapid, 'DATAMAX', datamax, 1, status)

      call gheads(mapid, 'EXMAPID', exmapid, 0, status)
      if ( exmapid.ne.' ' ) then
         call gheadi(exmapid, 'MAPPTR', exmapptr, 0, status)
         call gheads(exmapid, 'WCSID', exwcsid, 0, status)
         call gheads(exmapid, 'WCSID', ' ', 1, status)
         expmap = .TRUE.
         call rebinwork(mode, rebinfac, memr(exmapptr),szx, szy,
     &                  expmap, datamin, datamax)
         call gheadd(exmapid, 'DATAMIN', datamin, 1, status)
         call gheadd(exmapid, 'DATAMAX', datamax, 1, status)
      endif

      newzm(1) = rebinfac
      newzm(2) = rebinfac
      cenpix(1) = dble(newszx/2)*newzm(1) + 0.5
      cenpix(2) = dble(newszy/2)*newzm(2) + 0.5

      if ( mode.eq.0 ) then
c
c  Map size and pixel size doesn't change
c
         call gheads(mapid, 'MAPTYPE', 'R', 1, status)
         if ( exmapid.ne.' ' ) then
            call gheads(exmapid, 'MAPTYPE', 'R', 1, status)
         endif
      else if ( mode.eq.1 ) then
c
c  Move into new sized maps
c
         call gheadi(mapid, 'MAPPTR', -1, 1, status)
         call gheads(mapid, 'WCSID', ' ', 1, status)
         call mapalloc(newszx, newszy, mapid, newptr, status)
         if ( status.ne.0 ) then
            call gheadi(mapid, 'MAPPTR', mapptr, 1, status)
            return
         endif
         call cprrmap(memr(mapptr),szx,szy,1,newszx,1,newszy,
     &                memr(newptr),newszx,newszy,1,newszx,1,newszy,
     &                status)
         call wcslintrf(wcsid, cenpix, newszx, newszy, newzm, status)
         call wcstohdr(wcsid, mapid, status)
         call gheads(mapid, 'WCSID', wcsid, 1, status)
         call pmapfree(mapptr, status)

         if ( exmapid.ne.' ' ) then
            call gheadi(exmapid, 'MAPPTR', -1, 1, status)
            call gheads(exmapid, 'WCSID', ' ', 1, status)
            call mapalloc(newszx, newszy, exmapid, newptr, status)
            if ( status.ne.0 ) then
               call gheadi(exmapid, 'MAPPTR', exmapptr, 1, status)
               return
            endif
            call cprrmap(memr(exmapptr),szx,szy,1,newszx,1,newszy,
     &                   memr(newptr),newszx,newszy,1,newszx,1,newszy,
     &                   status)
            call wcslintrf(exwcsid, cenpix, newszx, newszy, newzm, 
     &                     status)
            call wcstohdr(exwcsid, exmapid, status)
            call gheads(exmapid, 'WCSID', exwcsid, 1, status)
            call pmapfree(exmapptr, status)
         endif
      else
c
c  Pixel size changes, map size does not (mode=2)
c
         call wcslintrf(wcsid, cenpix, szx, szy, newzm, status)
         call wcstohdr(wcsid, mapid, status)

         if ( exmapid.ne.' ' ) then
            call wcslintrf(exwcsid, cenpix, szx, szy, newzm, status)
            call wcstohdr(exwcsid, exmapid, status)
         endif
      endif
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'Rb'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      return
      end
