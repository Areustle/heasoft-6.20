c
c Color tables
c
c  cur[rgb]vals = Colors for current image color table
c  def[rgb]vals = Colors for default image color table
c
      integer maxcols
      parameter (maxcols=256)
      character(255) curcoltab, defcoltab
      integer curnumcols, defnumcols
      real currvals, curgvals, curbvals
      real defrvals, defgvals, defbvals
      real curcontra, curbright, defcontra, defbright
c
      COMMON /colnm / curcoltab, defcoltab
      COMMON /coltab/ currvals(maxcols), curgvals(maxcols), 
     &                curbvals(maxcols), curnumcols, curcontra, 
     &                curbright, defrvals(maxcols), defgvals(maxcols), 
     &                defbvals(maxcols), defnumcols, defcontra, 
     &                defbright
