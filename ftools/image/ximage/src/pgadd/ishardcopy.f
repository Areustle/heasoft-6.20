      function ishardcopy()
      implicit none

      logical ishardcopy
c
c  Returns whether device is hardcopy, meaning its color table can't
c  be manipulated, once displayed.
c
c  Local variables
c
      integer len
      character(10) value
      integer c1, c2

      call PGQINF ('TYPE', value, len)
      if ( value.eq.'GIF' ) then
         ishardcopy = .FALSE.
      else
         call PGQINF ('HARDCOPY', value, len)
         if ( value.eq.'YES' ) then
            ishardcopy = .TRUE.
         else
            ishardcopy = .FALSE.
         endif
      endif

      return
      end
         
