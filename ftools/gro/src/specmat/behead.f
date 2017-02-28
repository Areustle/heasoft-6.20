*This routine chops off the front part of a string.  It removes 
*everything up until a plus, minus, period, or number.

* @(#) behead.f 1.2@(#)

      subroutine behead(lstring,string)

      integer lstring
      character*(*) string

      i = 1
      do while (i.le.lstring)
         if (string(i:i).eq.'.'
     >        .or.string(i:i).eq.'+'
     >        .or.string(i:i).eq.'-'
     >        .or.string(i:i).ge.'0'.and.string(i:i).le.'9') then
            string = string(i:)
            RETURN
         endif
         i = i + 1
      enddo
      
      write (*,*) 'Failure to find number in string.'

      return
      end
