      function istk()
      implicit none

      logical istk
c
c  Function that checks whether currently using tk device
c
      integer isup, status

      istk = .FALSE.
      call tclresi('pgtk::tkisup', isup, status)
      if ( status.eq.0 .and. isup.eq.1 ) istk = .TRUE.

      return
      end
