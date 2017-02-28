      subroutine xrslash (string,islash)
      implicit none
      integer lenact
      integer islash, idum
      character(160) string
c---
c  Return the position of last slash '/'
c  in the string containing the file name
c  unless there's a ']' in which case we're in VMS so
C  that's actually what we want
c---
       islash=index(string,']')
       if(islash.eq.0) then
          do idum=lenact(string),1,-1
             if(string(idum:idum).eq.'/') then
                islash=idum
                return
             endif
          enddo
       endif
       return
       end



