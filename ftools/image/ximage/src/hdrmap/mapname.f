      subroutine mapname (mapid, name)
      implicit none
c
c  Returns true map name
c
c  I  mapid  (s)  Map identifier (can be alias, 1, dis, etc.)
c  O  name   (s)  True map name
c  O  status (i)  Error flag (0=OK)
c
      character*(*) mapid, name

      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer idx, status

      status = 0

      call mapidx(mapid, idx, status)
      if ( status.eq.0 ) then
         name = MAPids(idx)
      else
         name = ' '
      endif

      return
      end
