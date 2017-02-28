      subroutine mapfree(mapid, status)
      implicit none
c
c  Frees pointers associated with maps and blanks headers
c
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) mapid
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      character*(MAX_IDSTR) exmapid, wcsid
      integer mapptr, exmapptr, ptridx
      logical isloaded

      status = 0

       
      if ( isloaded(mapid) ) then
         call gheads(mapid, 'WCSID', wcsid, 0, status)
         if ( wcsid.ne.' ' ) call wcsdecref(wcsid)
         call gheads(mapid, 'WCSID', ' ', 1, status)
      endif

      call gheadi(mapid, 'MAPPTR', mapptr, 0, status)
      if ( status.ne.0 ) return

      if ( isloaded(mapid) .and. mapptr.ne.-1 ) then

         write(ZWRite,'(2a)') ' Freeing: ', mapid
         call xwrite(ZWRite, 20)

c
c  Check for associated exposure map
c
         call gheads(mapid, 'EXMAPID', exmapid, 0, status)
         if ( status.ne.0 ) return

         if ( exmapid.ne.' ' ) then
            call gheads(exmapid, 'WCSID', wcsid, 0, status)
            if ( wcsid.ne.' ' ) call wcsdecref(wcsid)
            call expiremap(exmapid, status)
            call gheadi(exmapid, 'MAPPTR', exmapptr, 0, status)
            if ( status.ne.0 ) return
c
c  Free exposure map first
c
            call pmapfree(exmapptr, status)
            if ( status.eq.0 ) then
               call mapidx(exmapid, ptridx, status)
               P_Map(ptridx) = -1
               MSZx(ptridx) = 0
               MSZy(ptridx) = 0
               if ( MAPlock(ptridx) ) MAPlock(ptridx) = .FALSE.
               if ( IDIsmap.eq.ptridx ) call setdismap(' ', status)
               call cphead(' ', exmapid)
            endif
         endif
c
c  Then, free map itself
c
         call expiremap(mapid, status)
         call pmapfree(mapptr, status)
         if ( status.eq.0 ) then
            call mapidx(mapid, ptridx, status)
            P_Map(ptridx) = -1
            MSZx(ptridx) = 0
            MSZy(ptridx) = 0
            if ( MAPlock(ptridx) ) MAPlock(ptridx) = .FALSE.
            if ( IDIsmap.eq.ptridx ) call setdismap(' ', status)
            call cphead(' ', mapid)
         endif

      endif

      return
      end
