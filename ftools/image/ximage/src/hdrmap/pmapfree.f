      subroutine pmapfree(mapptr, status)
      implicit none
c
c  Frees specific map pointer
c
c  I  mapptr  (i)  Map pointer
c  O  status  (i)  Error flag (0=OK)
c
      integer mapptr, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Local variables
c
      status = 0

      if ( mapptr.ne.-1 ) then

         write(ZWRite,*) ' Deallocating map ptr: ', mapptr
         call xwrite(ZWRite, 30)
         call udmfre(mapptr, 6, status)
         if ( status.ne.0 ) then
            call XAERROR('Could not free real map',5)
         endif

      endif

      return
      end
