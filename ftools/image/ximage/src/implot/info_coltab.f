      subroutine info_coltab (icoltab, name, contrast, bright)
      implicit none
c
c  Copy indicated color table into other table
c
c  I  icoltab  (i)  Color table to get info on
c  O  name     (i)  Name of color table
c  O  contrast (i)  Contrast of color table
c  O  bright   (i)  Brightness of color table
c
      integer icoltab
      character*(*) name
      real*4 contrast, bright
      
      include '../include/colordef.inc'
      include 'colortab.inc'

      if ( icoltab.eq.CTAB_DEFAULT ) then

         name = defcoltab
         contrast = defcontra
         bright = defbright

      else if ( icoltab.eq.CTAB_CURRENT ) then

         name = curcoltab
         contrast = curcontra
         bright = curbright

      endif

      return
      end
