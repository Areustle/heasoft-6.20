      subroutine dalloc(mode, szx, szy, p_dmap, status)
      implicit none
c
c  Allocates memory for a temporary two-dimensional real*8 map
c
c  I  mode    (i)  1=Allocate, 0=Deallocate
c  I  szx     (i)  Size in the x direction
c  I  szy     (i)  Size in the y direction
c  O  p_dmap  (i)  Pointer for double precision map
c  O  status  (i)  Error flag (0=OK)
c
      integer mode, szx, szy, p_dmap, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer totsz, LENACT
      real*8 dszx, dszy, dtotsz, dmaxint

      status = 0
c
c     If freeing pointer and pointer has no associated memory,
c       nothing to do
c
      if ( mode.eq.0 .and. p_dmap.eq.-1 ) return

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

      if ( mode.eq.1 ) then
         write(ZWRite,*) szx,'x',szy
         call RMVBLK(ZWRite)
         call XWRITE(' Allocating '//ZWRite(:LENACT(ZWRite))//
     &               ' double precision map',25)
         p_dmap = -1
         call udmget(totsz, 7, p_dmap, status)
         if ( status.ne.0 ) then
            write(ZWRite,*) szx,'x',szy
            call RMVBLK(ZWRite)
            ZWRite = 'Could not allocate '//ZWRite(:LENACT(ZWRite))//
     &           ' double precision map'
            call XAERROR(ZWRite,5)
         else
            write(ZWRite,*) ' Double ptr: ', p_dmap
            call xwrite(ZWRite, 30)
         endif
      else if ( mode.eq.0 ) then
         call XWRITE(' Deallocating double precision map',25)
         write(ZWRite,*) ' Double ptr: ', p_dmap
         call xwrite(ZWRite, 30)
         call udmfre(p_dmap, 7, status)
         if ( status.ne.0 ) then
            call XAERROR('Could not free double precision map',5)
         endif
      endif

      return
      end
