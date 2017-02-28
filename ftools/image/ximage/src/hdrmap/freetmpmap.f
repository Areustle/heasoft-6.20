      subroutine freetmpmap(Mapid,Status)

      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Release a temporary map for later usage
c  
c  I  mapid        (s) Map id 
c  O  status       (i) Error flag (0=OK)
c
      character*(*) Mapid
      integer Status
c
c  Local variables

      integer idx, LENACT

      call mapidx(mapid, idx, status)
      if ( MAPlock(idx) ) then
         MAPlock(idx) = .FALSE.
      else
         write(ZWRite,'(2a)') ' Not loaded: ', mapid(:LENACT(mapid))
         call xwrite(ZWRite, 10)
      endif

      return
      end
