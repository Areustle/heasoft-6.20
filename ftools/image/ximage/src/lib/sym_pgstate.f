      subroutine sym_pgstate (Color, Csize, Lwidth)
      implicit none
c
c  Sets symbol properties based on qualifier values
c
c  I  Color  (i)  Color index
c  I  Csize  (r)  Character size
c  I  Lwidth (i)  Line width

      integer Color, Lwidth
      real Csize
c
c Local variables
c
      integer status
      
      status = 0

      if ( Color.ge.0 ) call PGSCI(Color)
      if ( Csize.gt.0.0 ) call PGSCH(Csize)
      if ( Lwidth.gt.0 .and. Lwidth.le.201 ) call PGSLW(Lwidth)

      return
      end
