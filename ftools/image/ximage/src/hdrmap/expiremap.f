      subroutine expiremap(mapid, status)
      implicit none
c
c  Invalidate a mapid (i.e. it has change, so any state knowledge
c   is no longer accurate)
c
c  I  mapid  (s)  Map id string
c  O  status (i)  Error flag (0=OK)
c
      character*(*) mapid
      integer status

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
      include '../stat/backgd.inc'
c
c  Local variables
c
      integer imap, ptridx, LENACT
      character*(MAX_IDSTR) tmpmapid
      character(100) cmd

      status = 0
c
c  Invalidate plot states 
c
      call mapidx(mapid, ptridx, status)
      if ( status.eq.0 ) then
         imap = ptridx
         tmpmapid = mapids(imap)
         write(cmd,'(2a)') 'pgtk::expire ', tmpmapid(1:LENACT(tmpmapid))
         call tclrun(cmd, status)
c
c  Invalidate statistic calculation if it was calculated for the
c  expiring map (background, detect)
c
         if ( STAtid.ne.' ' ) then
            call mapidx(STAtid, ptridx, status)
            if ( status.eq.0 .and. ptridx.eq.imap ) call initstat
         endif
      endif

      return
      end
