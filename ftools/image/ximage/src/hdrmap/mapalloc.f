      subroutine mapalloc(szx, szy, mapid, mapptr, status)
      implicit none
c
c  Allocates/deallocates memory for a two-dimensional real*4 map
c
c  I  szx     (i)  Size in the x direction
c  I  szy     (i)  Size in the y direction
c  I  mapid   (i)  Map id string
c  O  mapptr  (i)  Map pointer
c  O  status  (i)  Error flag (0=OK)
c
      integer szx, szy, mapptr, status
      character*(*) mapid

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer ptridx, totsz, LENACT
      real*8 dszx, dszy, dtotsz, dmaxint

      status = 0
      dszx = szx
      dszy = szy
      dtotsz = dszx*dszy
      dmaxint = MAX_INT
      if ( dtotsz.gt.dmaxint ) then
         call xwarn(' Total map size greater than integer can hold', 5)
         status = -1
         return
      endif
      totsz = INT(dtotsz)

      call mapfree(mapid, status)

      write(ZWRite,'(2a)') ' Allocating: ', mapid
      call xwrite(ZWRite, 20)
      write(ZWRite,*) szx,'x',szy
      call RMVBLK(ZWRite)
      call XWRITE(' Allocating '//ZWRite(:LENACT(ZWRite))//
     &            ' real map',25)
      mapptr = -1
      call udmget(totsz, 6, mapptr, status)
      if ( status.ne.0 ) then
         write(ZWRite,*) szx,'x',szy
         call RMVBLK(ZWRite)
         ZWRite = 'Could not allocate '//ZWRite(:LENACT(ZWRite))//
     &            ' real map'
         call XAERROR(ZWRite,5)
      else
         call gheadi(mapid, 'MAPPTR', mapptr, 1, status)
         call gheadi(mapid, 'SZX', szx, 1, status)
         call gheadi(mapid, 'SZY', szy, 1, status)
         call mapidx(mapid, ptridx, status)
         P_Map(ptridx) = mapptr
         MSZx(ptridx) = szx
         MSZy(ptridx) = szy
         write(ZWRite,*) ' Real ptr: ', mapptr
         call xwrite(ZWRite, 30)
      endif

      return
      end
