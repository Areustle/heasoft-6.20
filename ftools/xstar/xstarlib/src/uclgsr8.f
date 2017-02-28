      subroutine uclgsr8(kdum,result,ierr)
      implicit none
      integer ierr
      real*8 result
      real result4
      character*(*) kdum
c
      ierr=0
c
      call uclgsr(kdum,result4,ierr)
      result=result4
c
      return
      end
