      subroutine read_coltab (icoltab, filename)
      implicit none
c
c  Read color table file into color table
c
c  I  icoltab  (i) Color table to read into
c  I  filename (c) Location of color table file
c
      integer icoltab
      character*(*) filename

      include '../include/colordef.inc'
      include 'colortab.inc'
c
c  Local variables
c
      integer numcols, status
      real inrvals(maxcols), ingvals(maxcols), inbvals(maxcols)
      integer i

      if ( filename.eq.' ' ) then
         if ( icoltab.eq.CTAB_DEFAULT ) then
            defcoltab = ' '
            defnumcols = 0
            defcontra = 1.0
            defbright = 0.5
         elseif ( icoltab.eq.CTAB_CURRENT ) then
            curcoltab = ' '
            curnumcols = 0
            curcontra = 1.0
            curbright = 0.5
         endif
         return
      endif

      status = 0
      call txinit(status)
      call txrdcol(filename, 1, maxcols, inrvals, numcols, status)
      call txrdcol(filename, 2, maxcols, ingvals, numcols, status)
      call txrdcol(filename, 3, maxcols, inbvals, numcols, status)
      if ( status.ne.0 ) then
         call XWRITE (' Could not read color table',10)
         call XWRITE (filename,15)
         return
      end if
 
      if ( numcols.gt.0 ) then
         if ( icoltab.eq.CTAB_DEFAULT ) then
            do i = 1, numcols
               defrvals(i) = inrvals(i)
               defgvals(i) = ingvals(i)
               defbvals(i) = inbvals(i)
            enddo
            defcoltab = filename
            defnumcols = numcols
            defcontra = 1.0
            defbright = 0.5
         elseif ( icoltab.eq.CTAB_CURRENT ) then
            do i = 1, numcols
               currvals(i) = inrvals(i)
               curgvals(i) = ingvals(i)
               curbvals(i) = inbvals(i)
            enddo
            curcoltab = filename
            curnumcols = numcols
            curcontra = 1.0
            curbright = 0.5
         endif
      else
         call xwarn('No data in colour table',10)
      endif

      return
      END
