      function ismouse()
      implicit none

      logical ismouse
c
c If PGQINF reports YES for CURSOR parameter, device is interactive.
c
c  Local variables
c
      integer len
      character(10) value

      call PGQINF ('CURSOR', value, len)
      if ( value.eq.'YES' ) then
         ismouse = .TRUE.
      else
         ismouse = .FALSE.
      endif

      return
      end
         
