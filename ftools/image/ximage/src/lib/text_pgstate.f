      subroutine text_pgstate (Color, Csize, Lwidth, Font)
      implicit none
c
c  Sets text properties based on qualifier values
c
c  I  Color  (i)  Color index
c  I  Csize  (r)  Character size
c  I  Lwidth (i)  Line width
c  I  Font   (c)  Font

      integer Color, Lwidth
      real Csize
      character*(*) Font
c
c Local variables
c
      integer fontval
      integer status
      real*8 dd
      logical ISDIGIT
      
      INTEGER nfont
      PARAMETER (nfont=4)
      character(6) fontopts(nfont)
c
      DATA fontopts /'NORMAL', 'ROMAN', 'ITALIC', 'SCRIPT'/

      status = 0

      if ( Color.ge.0 ) call PGSCI(Color)
      if ( Csize.gt.0.0 ) call PGSCH(Csize)
      if ( Lwidth.gt.0 .and. Lwidth.le.201 ) call PGSLW(Lwidth)

      if ( Font.ne.' ' ) then
         if ( isdigit(font(1:1)) ) then
            call strnum(font, -4, dd, status)
            fontval = int(dd)
         else
            call matchopts (Font, fontopts, nfont, fontval, status)
         endif
         if ( status.eq.0 ) then
            call PGSCF(fontval)
         else
            call PGQCF(fontval)
            font = fontopts(fontval)
         endif
      endif

      return
      end
