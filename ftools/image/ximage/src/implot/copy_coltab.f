      subroutine copy_coltab (icoltab)
      implicit none
c
c  Copy indicated color table into other table
c
c  I  icoltab (i)  Color table to copy
c
      integer icoltab

      include '../include/colordef.inc'
      include 'colortab.inc'
c
c  Local variables
c
      integer i

      if ( icoltab.eq.CTAB_DEFAULT ) then

         curcoltab = defcoltab
         curnumcols = defnumcols
         curcontra = defcontra
         curbright = defbright

         do i = 1, curnumcols
            currvals(i) = defrvals(i)
            curgvals(i) = defgvals(i)
            curbvals(i) = defbvals(i)
         enddo

      else if ( icoltab.eq.CTAB_CURRENT ) then

         defcoltab = curcoltab
         defnumcols = curnumcols
         defcontra = curcontra
         defbright = curbright

         do i = 1, curnumcols
            defrvals(i) = currvals(i)
            defgvals(i) = curgvals(i)
            defbvals(i) = curbvals(i)
         enddo

      endif

      return
      end
