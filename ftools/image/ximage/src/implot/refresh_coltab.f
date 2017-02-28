      subroutine refresh_coltab
      implicit none
c
c  Refresh current color table
c
      include 'colortab.inc'
c
c  Local variables
c
      logical isdisplay
      integer i
      real ramp(maxcols)

      if ( .not.isdisplay() ) return

c  Determine ramp-intensity levels
c  Evenly space colors starting from ends

      do i = 1, curnumcols
         ramp(i) = (i-1)*real(1.0/(curnumcols-1))
      enddo

      CALL PGCTAB(ramp, currvals, curgvals, curbvals, curnumcols, 
     &            curcontra, curbright)

      RETURN
      END
