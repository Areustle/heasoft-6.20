      function iscpmapid (amapid, bmapid)
      implicit none

      logical iscpmapid
c
c  Returns whether amapid is a copy of bmapid
c  Not just simple string comparison, as there are many aliases
c
c  I  amapid  (s)  Map identifier
c  I  bmapid  (s)  Map identifier
c
      character*(*) amapid, bmapid

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer ia, ib, idx, status
      character(100) cmd
      character(10) afull, bfull

      call mapidx(amapid, ia, status)
      afull = MAPids(ia)
      call mapidx(bmapid, ib, status)
      bfull = MAPids(ib)

      if ( ia.eq.ib ) then
         iscpmapid = .TRUE.
         return
      endif
      
      write(cmd,'(4a)') 'lsearch [chheader map=', afull,' key=mapcopy] '
     &                  , bfull
      call tclresi(cmd, idx, status)
      if ( idx.eq.-1 ) then
         iscpmapid = .FALSE.
      else
         iscpmapid = .TRUE.
      endif

      return
      end
