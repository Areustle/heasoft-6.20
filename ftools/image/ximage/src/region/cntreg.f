      subroutine cntreg(Ireg, Map, Szx, Szy, Zmx, Zmy, Xcen, Ycen, 
     &                  Sum, Area, Status)
      implicit none
c
c  Sum counts in an area specified by region (loaded with xinitreg or
c     added with xboxreg - ximreg.c)
c
c  I  Ireg      (i)  Region number
c  I  Map       (r)  Image map
c  I  Szx/y     (i)  Size of map
c  I  Zmx/y     (d)  Rebin of map
c  I  X/Ycen    (d)  Center of map in detector coords
c  O  Sum       (r)  Sum of pixels in counted region
c  O  Area      (r)  Number of pixels in counted region
c  O  Status    (i)  Error flag (0 = OK)
c
      integer*4 Ireg, Szx, Szy, Status
      real*4 Map(Szx,Szy), Sum, Area
      real*8 Zmx, Zmy, Xcen, Ycen

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c  Local variables
c
      real*4 dumbgmap, wgt
      integer bgszx, bgszy
      real*8 bgzmx, bgzmy, bgxcen, bgycen
c
c  Call count region routine which accounts for background,
c  except provide a dummy flat background
c
      dumbgmap = 1.
      bgszx = 1
      bgszy = 1
      bgzmx = dble(Szx)*Zmx
      bgzmy = dble(Szy)*Zmy
      bgxcen = Xcen
      bgycen = Ycen
      wgt = 1.
      
      call bgcntreg(Ireg, Map, Szx, Szy, Zmx, Zmy, Xcen, Ycen, 
     &              dumbgmap, bgszx, bgszy, bgzmx, bgzmy, bgxcen,
     &              bgycen, wgt, Sum, Area, Status)
c
c  No need to do anything with Sum as it was divided by 1.
c  No weighting need be applied.
c
      return
      end
