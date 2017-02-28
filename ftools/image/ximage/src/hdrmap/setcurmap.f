      subroutine setcurmap(mapid, status)
      implicit none
c
c  Set current map
c
c  I  mapid  (s)  Map id string
c  O  status (i)  Error flag (0=OK)
c
      character*(*) mapid

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer ptridx, status
      character*(MAX_IDSTR) curmapid
      logical readonly, global

      status = 0

      call mapidx(mapid, ptridx, status)
      if ( status.eq.0 ) then
         ICUrmap = ptridx
         curmapid = mapids(ICUrmap)
         readonly = .TRUE.
         global = .TRUE.
         call tclvars('curmap', curmapid, readonly, global, status)
      endif

      return
      end
