      subroutine gettmpmap(Mapid,Status)

      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Return a temporary map
c  
c  O  mapid        (s) Map id 
c  O  status       (i) Error flag (0=OK)
c
      character*(*) Mapid
      integer Status
c
c  Local variables

      logical found
      integer idx, maxidx

      call mapidx('TMPMAP1', idx, status)
      maxidx = idx + MAX_TMPMAPS - 1
      found = .FALSE.
      do while ( idx .le. maxidx .and. .not.found ) 
         if ( .not.MAPlock(idx) ) then
            found = .TRUE.
            MAPlock(idx) = .TRUE.
         else
            idx = idx + 1
         endif
      enddo
      if ( .not.found ) then
         call xwrite(' No temporary maps available', 10)
         status = -1
         return
      endif
      Mapid = MAPids(idx)

      return
      end
