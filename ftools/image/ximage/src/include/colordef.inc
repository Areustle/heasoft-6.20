c
c  Color table constants
c
      integer CTAB_CURRENT, CTAB_DEFAULT
      parameter (CTAB_CURRENT=1)
      parameter (CTAB_DEFAULT=0)
c
c  Available color tables
c
c ztabnum= number of color tables
c ztab_name= color table names
c
      INTEGER*4 ztabmax, ztabnum
      PARAMETER (ztabmax=100)
      character(20) ztab_name(ztabmax)
c
c  Colors for PGPLOT default color indexes (0-15)
c
      integer numpgcols
      parameter (numpgcols = 16)
      real pgrvals(numpgcols), pggvals(numpgcols), pgbvals(numpgcols)
      character(20) pgdefnam(numpgcols)
c
c  Color index to display for null pixels
c
      integer nullcol

      data pgdefnam/'Background', 'Foreground', 'Red', 'Green',
     &              'Blue', 'Light blue', 'Magenta', 'Yellow',
     &              'Orange', 'Yel.+Green', 'Green+Cyan',
     &              'Blue+Cyan', 'Blue+Mag', 'Red+Mag',
     &              'Dark Gray', 'Light Gray'/

      common /coldefn/ ztabnum, nullcol
      common /colpg/ pgrvals, pggvals, pgbvals
      common /coldefs/ ztab_name
