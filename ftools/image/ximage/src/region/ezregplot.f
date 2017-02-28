      subroutine ezregplot(mapid, regionfile, status)
      implicit none
c
c  Simple wrapper which plots a regionfile to the image
c
c  I  mapid       (s)  Map id string
c  I  regionfile  (s)  Regionfile to plot
c  O  status      (i)  Error flag
c
      character*(*) mapid, regionfile
      integer status

      include '../include/maxvals.inc'
c
c  Local variables
c
      character*(MAX_IDSTR) wcsid
      integer color, lwidth, lstyle, excolor, exlwidth, exlstyle
      integer numreg, ireg

c
c  Include = green, Exclude = red
c
      color = 3
      lwidth = 1
      lstyle = 1
      excolor = 2
      exlwidth = 1
      exlstyle = 1

      status = 0

      CALL PGSAVE
c
c  Set WCS coordinates for degrees/hms formats
c
      call gheads(mapid, 'WCSID', wcsid, 0, status)
      call setregwcs(wcsid, status)

      call xinitreg(Regionfile, numreg, status)
      do ireg = 1, numreg
         call plotreg(ireg, color, lwidth, lstyle, excolor,
     &                exlwidth, exlstyle, status)
      enddo
      call xfreereg(status)
      CALL PGUNSA

      return
      end
