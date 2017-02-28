**==EXDELTMP.spg  processed by SPAG 3.09I  at 10:43 on 12 Jan 1993
      SUBROUTINE EXDELTMP(ctype)

      IMPLICIT NONE

      CHARACTER*(*) ctype
      character(100) ctmp

      INTEGER LENACT
      EXTERNAL LENACT
 
      ctmp = ctype(:lenact(ctype))//'.tmp'
      call makefname(ctmp)
      call unlink(ctmp)
 
      RETURN
      END

      subroutine makefname(fname)

      implicit none

      character*(*) fname

      character(10)  str
      integer random, getpid, lenact

      logical first

      save first,random
      data first /.true./

      if (first) then
         random = getpid()
         first = .false.
      end if

      write(str,'(i6.6)')random
      fname = fname(1:lenact(fname))//str(1:6)
      return
      end

