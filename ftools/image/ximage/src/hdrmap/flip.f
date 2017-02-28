      subroutine flip (Cmdid, Mapid, Status)
      implicit none
c
c  Flips image (horiz/vert) or Rotates by 90 (+/-)
c
c  I  Cmdid  (i)  Command id
c  I  Mapid  (i)  Map id string
c  O  Status (i)  Error flag (0 = OK)
c
      integer*4 Cmdid, Status
      character*(*) Mapid

      include '../include/dynmem.inc'
      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c   Local variables
c
      integer*4 argc, oldptr, newptr, idx, szx, szy, mode, di, lenact
      logical horiz, vert, plus90, minus90, isloaded
      character(100) ds
      character(80) mapcodes
      character*(MAX_IDSTR) wcsid

      horiz = .FALSE.
      vert = .FALSE.
      plus90 = .FALSE.
      minus90 = .FALSE.

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'HORIZONTAL',horiz,Status)
      CALL GPARL(Cmdid,'VERTICAL',vert,Status)
      CALL GPARL(Cmdid,'+90',plus90,Status)
      CALL GPARL(Cmdid,'-90',minus90,Status) 
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         Status = -1
         return
      endif

      if ( horiz ) then
         call XWRITE(' Flip horizontally', 10)
         mode = 1
      elseif ( vert ) then
         call XWRITE(' Flip vertically', 10)
         mode = 2
      elseif ( plus90 ) then
         call XWRITE(' Rotate 90 degrees clockwise', 10)
         mode = 3
      elseif ( minus90 ) then
         call XWRITE(' Rotate 90 degrees counterclockwise', 10)
         mode = 4
      else
         call XWRITE(' Flip method not specified', 5)
         Status = -1
         return
      endif
c
c  Get pointer and zap it in header, so mapalloc doesn't free it
c
      call gheads(mapid, 'WCSID', wcsid, 0, status)
      call gheads(mapid, 'WCSID', ' ', 1, status)
      call gheadi(mapid, 'MAPPTR', oldptr, 0, status)
      call gheadi(mapid, 'MAPPTR', -1, 1, status)
      call gheadi(mapid, 'SZX', szx, 0, status)
      call gheadi(mapid, 'SZY', szy, 0, status)
      call mapalloc(szx, szy, mapid, newptr, status)
      if ( status.ne.0 ) then
         call gheadi(mapid, 'MAPPTR', oldptr, 1, status)
         call gheads(mapid, 'WCSID', wcsid, 1, status)
         return
      endif
         
      call gheads(mapid, 'WCSID', wcsid, 1, status)
      if ( mode.eq.1 .or. mode.eq.2 ) then
         call flipwork (memr(oldptr), memr(newptr), Szx, Szy, mode, 
     &                  Status)
      else
         call flip90work (memr(oldptr), memr(newptr), Szx, Szy, mode, 
     &                  Status)
         di = Szx
         Szx = Szy
         Szy = di
         call gheadi(mapid, 'SZX', Szx, 1, status)
         call gheadi(mapid, 'SZY', Szy, 1, status)
         call mapidx(mapid, idx, status)
         MSZx(idx) = Szx
         MSZy(idx) = Szy
      endif
c
c  Free old map
c
      call pmapfree(oldptr, status)
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'F'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)

      return
      end
