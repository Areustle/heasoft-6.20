      subroutine mod_coltab (icoltab, contra, bright)
      implicit none
c
c  Set contrast and brightness for color table
c
c  I  icoltab  (i)  Color table to modify
c  I  contra   (r)  Contrast
c  I  bright   (r)  Brightness
c
      integer icoltab
      real contra, bright

      include '../include/colordef.inc'
      include 'colortab.inc'

      if ( icoltab.eq.CTAB_DEFAULT ) then
         defcontra = contra
         if ( bright.ge.0.0 .or. bright.le.1.0) defbright = bright
      elseif ( icoltab.eq.CTAB_CURRENT ) then
         curcontra = contra
         if ( bright.ge.0.0 .or. bright.le.1.0) curbright = bright
      endif

      return
      end
