      subroutine mapcopy (fromid, toid, status)
      implicit none
c
c  Copy map to new slot
c
c  I  fromid  (s)  Id of map to copy from
c  I  toid    (s)  Id of map to copy to
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) fromid, toid
      integer status

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
      include '../include/dynmem.inc'
      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 szx, szy, fromptr, toptr, ifrom, ito
      LOGICAL isloaded, iseqmapid
      character*(MAX_IDSTR) exfromid, extoid, wcsid, cpwcsid

      Status = 0

      if ( .not.isloaded(fromid) ) then
         call XWRITE (' Image not loaded', 10)
         Status = -1
         return
      endif
      if ( iseqmapid(fromid, toid) ) then
         call xwrite(' Cannot copy map to itself', 10)
         Status = -1
         return
      endif
      if ( isloaded(toid) ) call mapfree(toid, status)
c
c  To header will be same as from except for MAPPTR and EXMAPID
c  Init MAPPTR, so mapalloc can set to new location
c
      call cphead (fromid,toid)
c
c  Make deep copy of wcs data
c
      call gheads(fromid, 'WCSID', wcsid, 0, status)
      call copywcs(wcsid, MAX_IDSTR, cpwcsid, status)
      call gheads(toid, 'WCSID', cpwcsid, 1, status)
c
c  Record copy dependency
c
      call mapidx(fromid, ifrom, status)
      call mapidx(toid, ito, status)
      write(ZWRite,'(2a,1x,a)') 'pgtk::upcopy ', MAPids(ifrom), 
     &                                           MAPids(ito)
      call tclrun(ZWRite, status)

      call gheadi(toid, 'LOADED', 0, 1, status)
      call gheadi(toid, 'MAPPTR', -1, 1, status)
c
c  Allocate necessary space
c
      call gheadi(fromid, 'SZX', szx, 0, status)
      call gheadi(fromid, 'SZY', szy, 0, status)
      call gheadi(fromid, 'MAPPTR', fromptr, 0, status)
      call mapalloc(szx, szy, toid, toptr, status)
      call cprrmap (memr(fromptr), szx, szy, 1, szx, 1, szy,
     &              memr(toptr), szx, szy, 1, szx, 1, szy, 
     &              status)
      if ( status.ne.0 ) return
      call gheadi(toid, 'LOADED', 1, 1, status)
c
c  Check for associated exposure map
c
      call gheads(fromid, 'EXMAPID', exfromid, 0, status)

      if ( exfromid.ne.' ' ) then
         call getexmap(toid, extoid, status)
         if ( status.ne.0 ) then
            extoid = ' '
            status = 0
         else
            call cphead (exfromid,extoid)
c
c         Make deep copy of wcs data
c
            call gheads(exfromid, 'WCSID', wcsid, 0, status)
            call copywcs(wcsid, MAX_IDSTR, cpwcsid, status)
            call gheads(extoid, 'WCSID', cpwcsid, 1, status)
            call gheadi(extoid, 'LOADED', 0, 1, status)
            call gheadi(extoid, 'MAPPTR', -1, 1, status)
            call gheadi(exfromid, 'SZX', szx, 0, status)
            call gheadi(exfromid, 'SZY', szy, 0, status)
            call gheadi(exfromid, 'MAPPTR', fromptr, 0, status)
            call mapalloc(szx, szy, extoid, toptr, status)
            call cprrmap (memr(fromptr), szx, szy, 1, szx, 1, szy,
     &                    memr(toptr), szx, szy, 1, szx, 1, szy, 
     &                    status)
            call gheadi(extoid, 'LOADED', 1, 1, status)
        endif
        call gheads(toid, 'EXMAPID', extoid, 1, status)
      endif

      return
      end
