      subroutine crosssect(Cutmode, Log, Map, Szx, Szy, Xmin, Xmax,
     &                     Ymin, Ymax, Cross, Crx, Cry, 
     &                     Datamin, Datamax)
      implicit none
c
c  Create evenly spaced cross-sections from Map
c
c   I  Cutmode         (i)  0=straight, 1=average, 2=non-empty avg
c   I  Log             (l)  Whether to scale logarithmically
c   I  Map             (r)  Map
c   I  Szx/y           (i)  Map size
c   I  X/Ymin/max      (i)  Region of map to cross-section
c   I  Cross           (r)  Cross-section buffer
c   I  Crx/y           (i)  Size of cross-section buffer
c   O  Datamin         (r)  Minimum value in cross-section
c   O  Datamax         (r)  Maximum value in cross-section
c
      integer Cutmode, Szx, Szy, Xmin, Xmax, Ymin, Ymax, Crx, Cry
      logical Log
      real*4 Map(Szx,Szy), Cross(Crx,Cry)
      real*4 Datamin, Datamax

      include '../include/io.inc'
c
c  Local variables
c
      real*4 chunksz, chtot, rmin, rmax, rnull
      integer i, im, j, k, n, row, xwid, ywid
      logical isrnull

c  Initialize to avoid warning
      k = 0
c  --

c
c  Copy cross-sections into Cross map
c
      rmin = rnull()
      rmax = rnull()

      xwid = Xmax - Xmin + 1
      ywid = Ymax - Ymin + 1
c
c  Standard cut (ignore nearby cross-sections)
c
      if ( Cutmode.eq.0 ) then

         do j = 1, Cry
            k = nint(float(ywid)/float(Cry)*float(j)) + Ymin - 1
            do i = 1, Crx
               im = i + Xmin - 1
               if ( isrnull(Map(im,k)) ) then
                  Cross(i,j) = 0.
               elseif ( Log ) then
                  if ( Map(im,k).le.0 ) then
                     Cross(i,j) = 0.
                  else
                     Cross(i,j) = log10(Map(im,k))
                  endif
               else
                  Cross(i,j) = Map(im,k)
               endif
               if ( isrnull(rmin) .or. Cross(i,j).lt.rmin ) 
     &            rmin = Cross(i,j)
               if ( isrnull(rmax) .or. Cross(i,j).gt.rmax ) 
     &            rmax = Cross(i,j)
            enddo
         enddo

      else
c
c  Average nearby cross-sections
c
         chunksz = float(ywid)/float(Cry)
         chtot = 0.
         j = 1
         row = Ymin
         do while ( chtot+chunksz.le.float(ywid) ) 
            chtot = chtot + chunksz
            do i = 1, Crx
               im = i + Xmin - 1
               Cross(i,j) = 0.
               n = 0
               k = row
               do while ( float(k-Ymin+1).le.chtot ) 
                  if ( .not.isrnull(Map(im,k)) ) then
                     Cross(i,j) = Cross(i,j) + Map(im,k)
                  endif
                  if ( Cutmode.eq.1 .or. 
     &                (Cutmode.eq.2 .and. .not.isrnull(Map(im,k))
     &                              .and. Map(im,k).gt.0)  ) n = n + 1
                  k = k + 1
               enddo
               if ( n.gt.0 ) Cross(i,j) = Cross(i,j)/float(n)
               if ( Log ) then
                  if ( Cross(i,j).lt.1. ) then
                     Cross(i,j) = 0.
                  else
                     Cross(i,j) = log10(Cross(i,j))
                  endif
               endif
               if ( isrnull(rmin) .or. Cross(i,j).lt.rmin ) 
     &            rmin = Cross(i,j)
               if ( isrnull(rmax) .or. Cross(i,j).gt.rmax ) 
     &            rmax = Cross(i,j)
            enddo
            row = k
            j = j + 1
         enddo
      endif

      Datamin = rmin
      Datamax = rmax
c
c  Work around for PGPLOT drawing bug.  If last bin shoots up, the line
c  that comes down extends to the bottom of the viewport.  Zeroing
c  the last bin prevents this, but we're losing a bin of data.
c
      do j = 1, Cry
         Cross(Crx,j) = rmin
      enddo

      return
      end
