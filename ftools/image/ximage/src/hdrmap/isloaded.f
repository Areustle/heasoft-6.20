      function isloaded (mapid)
      implicit none

      logical isloaded
c
c  Returns whether the specified map has been loaded
c
c  I  mapid  (c)  Map identifier
c
      character*(*) mapid
c
c  Local variables
c
      integer*4 iload, status

      call gheadi(mapid, 'LOADED', iload, 0, status)
      if ( iload.eq.0 .or. status.ne.0 ) then
         isloaded = .FALSE.
      else
         isloaded = .TRUE.
      endif

      return
      end
