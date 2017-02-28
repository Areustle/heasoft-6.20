      subroutine setdismap(mapid, status)
      implicit none
c
c  Set display map
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
      character*(MAX_IDSTR) dismapid
      logical readonly, global

      status = 0
      readonly = .TRUE.
      global = .TRUE.
c
c  If mapid blank, set to no displayed map
c
      if ( mapid.eq.' ' ) then
         IDIsmap = 0
         call tclvars('dismap', ' ', readonly, global, status)
         return
      endif

      call mapidx(mapid, ptridx, status)
      if ( status.eq.0 ) then
         IDIsmap = ptridx
         dismapid = mapids(IDIsmap)
         call tclvars('dismap', dismapid, readonly, global, status)
      endif

      return
      end
