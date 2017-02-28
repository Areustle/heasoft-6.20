      subroutine line_pgstate (Color, Lwidth, Lstyle)
      implicit none
c
c  Sets line properties based on qualifier values
c
c  I  Color  (i)  Color index
c  I  Lwidth (i)  Line width
c  I  Lstyle (c)  Line style

      integer Color, Lwidth, Lstyle
c
c Local variables
c
      integer status
      
      status = 0

      if ( Color.ge.0 ) call PGSCI(Color)
      if ( Lwidth.gt.0 .and. Lwidth.le.201 ) call PGSLW(Lwidth)
      if ( Lstyle.gt.0 .and. Lstyle.le.5 ) call PGSLS(Lstyle)

      return
      end
