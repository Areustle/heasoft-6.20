      subroutine mkhalloc(mode, szx, szy, p_work, status)
      implicit none
c
c  Allocates memory for a two-dimensional integer map
c
c  I  mode   (i)  1=Allocate, 0=Deallocate
c  I  szx    (i)  Size in the x direction
c  I  szy    (i)  Size in the y direction
c  O  p_work (i)  Pointer for work map
c  O  status (i)  Error flag (0=OK)
c
      integer mode, szx, szy, p_work, status
c
c  Local variables
c
      character(100) zwrite
      integer totsz, LENACT

      totsz = szx*szy
      status = 0

      if ( mode.eq.1 ) then
         write(zwrite,*) szx,'x',szy
         call RMVBLK(zwrite)
         call XWRITE(' Allocating '//zwrite(:LENACT(zwrite))//
     &               ' integer work map',25)
         p_work = -1
         call udmget(totsz, 4, p_work, status)
         if ( status.ne.0 ) then
            call XAERROR(' Could not allocate '//zwrite(:LENACT(zwrite))
     &                   //' integer work map', 5)
         else
            write(zwrite,*) ' Integer (work) ptr: ', p_work
            call xwrite(zwrite, 30)
         endif
      else if ( mode.eq.0 ) then
         call XWRITE(' Deallocating work map',25)
         write(zwrite,*) ' Integer (work) ptr: ', p_work
         call xwrite(zwrite, 30)
         if ( p_work.gt.0 ) call udmfre(p_work, 4, status)
         if ( status.ne.0 ) then
            call XAERROR('Could not free integer work map',5)
         endif
      endif

      return
      end
