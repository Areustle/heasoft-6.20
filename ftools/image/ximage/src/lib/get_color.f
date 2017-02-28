      subroutine get_color(color)

      implicit none
c
c  When XIMAGE was freed from its 16 color standard, a new
c  problem cropped up.  Throughout the code, there are places
c  where certain plotted elements are given default colors.
c  With varying color ranges, these values no longer correspond
c  to the same color.  The current interpretation is that the 
c  the color value is normalized to 16.
c
c I/O color        (i) Input color value (0-16)
c                      Output color value (scaled over PGQCIR values)
c
      integer color
c
c  Local variables
c
      integer minci, maxci
      real*4 frac

      if ( color.lt.0 ) return
c
c  Reserved color indices (0-15)
c
      frac = MIN(float(color)/16., 1.0)
      call PGQCIR(minci, maxci)
      color = NINT((maxci - minci)*frac) + minci

      return
      end
