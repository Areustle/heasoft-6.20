
*+GT_FCHAN
c     -----------------------------------------------------------
      subroutine gt_fchan(channel,fchan,lchan,nchan,ierr,chatter)
c     -----------------------------------------------------------
c --- DETERMINE FCHAN VALUE FROM DYNAMICALY ALLOCATED ARRAY ---
c
c --- VARIABLES ---
c
      IMPLICIT NONE
      integer channel(*),fchan,ierr,chatter,lchan,nchan
c
      character(5) version
      parameter (version = '1.0.0')
c
c --- AUTHORS ---
c
c Rehana Yusaf 1.0.0; 1994 March 10
c Rehana Yusaf 1.0.1; 1995 Aug 3
*-
c -------------------------------------------------------------
c
      fchan = channel(1)
      lchan = channel(nchan)
      return
      end
c -------------------------------------------------------------
c     END OF GT_FCHAN
c -------------------------------------------------------------

