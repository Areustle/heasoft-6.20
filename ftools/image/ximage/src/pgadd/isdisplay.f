      function isdisplay()
      implicit none

      logical isdisplay
c
c If PGQINF reports question mark for device query, image
c is not displayed, otherwise it is.
c
c  Local variables
c
      integer len
      character(10) value

      call PGQINF ('STATE', value, len)
      if ( value(1:4).eq.'OPEN' ) then
         isdisplay = .TRUE.
      else
         isdisplay = .FALSE.
      endif

      return
      end
         
