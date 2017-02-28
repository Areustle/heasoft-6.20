      subroutine mkcralloc(mode, szx, szy, p_rmap, status)
c
c  Allocates memory for a temporary two-dimensional real*4 map
c
c  I  mode    (i)  1=Allocate, 0=Deallocate
c  I  szx     (i)  Size in the x direction
c  I  szy     (i)  Size in the y direction
c  O  p_rmap  (i)  Pointer for real map
c  O  status  (i)  Error flag (0=OK)
c
      integer mode, szx, szy, p_rmap, status
c
c  Local variables
c
      integer totsz, LENACT
      character(80) zwrite

      totsz = szx*szy
      status = 0

      if ( mode.eq.1 ) then
         write(zwrite,*) szx,'x',szy
         call RMVBLK(zwrite)
         call XWRITE(' Allocating '//zwrite(:LENACT(zwrite))//
     &               ' real map',25)
         p_rmap = -1
         call udmget(totsz, 6, p_rmap, status)
         if ( status.ne.0 ) then
            write(zwrite,*) szx,'x',szy
            call RMVBLK(zwrite)
            zwrite = 'Could not allocate '//zwrite(:LENACT(zwrite))//
     &           ' real map'
            call XAERROR(zwrite,5)
         else
            write(zwrite,*) ' Real ptr: ', p_rmap
            call xwrite(zwrite, 30)
         endif
      else if ( mode.eq.0 ) then
         call XWRITE(' Deallocating real map',25)
         write(zwrite,*) ' Real ptr: ', p_rmap
         call xwrite(zwrite, 30)
         if ( p_rmap.gt.0 ) call udmfre(p_rmap, 6, status)
         if ( status.ne.0 ) then
            call XAERROR('Could not free real map',5)
         endif
      endif

      return
      end
