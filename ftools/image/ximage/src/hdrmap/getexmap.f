      subroutine getexmap(mapid, exmapid, status)
      implicit none
c
c  Return exposure mapid from map id
c  e.g.  mapid = MAP1, exmapid = EXMAP1
c
c  I   mapid    (s)  Map id string
c  O   exmapid  (s)  Exposure map id string
c  O   status   (i)  Error flag (0=OK)
c
      character*(*) mapid, exmapid
      integer status

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      character*(MAX_IDSTR) tmpstr
      integer ptridx

      status = 0

      call mapidx(mapid, ptridx, status)
      if ( status.ne.0 ) return
c
c  Strictly depends on following layout:
c
c  1 -> MAP1
c  2 -> EXMAP1
c  ...
c  MAX_SLOTS*2-1 -> MAP[MAX_SLOTS]
c  MAX_SLOTS*2   -> EXMAP[MAX_SLOTS]
c  MAX_SLOTS*2+1 -> BGMAP
c  MAX_SLOTS*2+2 -> TMPMAP1
c  ...
c  MAX_SLOTS*2+1+MAX_TMPMAPS -> TMPMAP[MAX_TMPMAPS]
c
      if ( ptridx.gt.MAX_SLOTS*2+1 ) then
         call gettmpmap(tmpstr, status)
         if ( status.ne.0 ) return
         exmapid = tmpstr
      else if ( ptridx.gt.MAX_SLOTS*2 ) then
         call xaerror(' Background map cannot have exposure map', 5)
         status = -1
         return
      else if ( MOD(ptridx,2).eq.0 ) then
         call xaerror(' Exposure maps cannot have exposure maps', 5)
         status = -1
         return
      else
         exmapid = MAPids(ptridx+1)
      endif

      return
      end
