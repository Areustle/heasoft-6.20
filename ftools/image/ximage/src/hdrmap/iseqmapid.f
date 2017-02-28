      function iseqmapid (amapid, bmapid)
      implicit none

      logical iseqmapid
c
c  Returns whether amapid is same as bmapid
c  Not just simple string comparison, as there are many aliases
c  If either arg is not an actual mapid, false is returned
c
c  I  amapid  (s)  Map identifier
c  I  bmapid  (s)  Map identifier
c
      character*(*) amapid, bmapid
c
c  Local variables
c
      integer ia, ib, status

      if ( amapid.eq.' ' .or. bmapid.eq.' ' ) then 
         iseqmapid = .FALSE.
         return
      endif

      call mapidx(amapid, ia, status)
      if ( status.ne.0 ) then
         iseqmapid = .FALSE.
         return
      endif

      call mapidx(bmapid, ib, status)
      if ( status.ne.0 ) then
         iseqmapid = .FALSE.
         return
      endif

      iseqmapid = ia.eq.ib

      return
      end
