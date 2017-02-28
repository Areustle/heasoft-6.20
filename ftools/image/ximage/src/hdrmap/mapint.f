      subroutine mapint(mapid, status)
      implicit none
c
c  Converts real map into integer map
c
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) mapid
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      logical isloaded
      integer i, mapptr, szx, szy, totsz
      character(1) maptype
      real*8 dd

      status = 0

      if ( .not.isloaded(mapid) ) then
         call xwrite(' Image not loaded', 10)
         status = -1
         return
      endif

      maptype = ' '
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      if ( maptype.ne.'R' ) then
         call xwrite(' Map type not real', 10)
         status = -1 
         return
      endif

      call gheadi(mapid, 'MAPPTR', mapptr, 0, status)
      call gheadi(mapid, 'SZX', szx, 0, status)
      call gheadi(mapid, 'SZY', szy, 0, status)
      if ( status.ne.0 ) return

      totsz = szx*szy
      
      do i = 1, totsz
         memr(mapptr+i-1) = INT(memr(mapptr+i-1))
      enddo
c
c  Update header with type and new min/max
c
      maptype = 'I'
      call gheads(mapid, 'MAPTYPE', maptype, 1, status)
      call gheadd(mapid, 'DATAMIN', dd, 0, status)
      dd = INT(dd)
      call gheadd(mapid, 'DATAMIN', dd, 1, status)
      call gheadd(mapid, 'DATAMAX', dd, 0, status)
      dd = INT(dd)
      call gheadd(mapid, 'DATAMAX', dd, 1, status)

      return
      end
