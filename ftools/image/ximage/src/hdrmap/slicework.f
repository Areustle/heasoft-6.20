      subroutine slicework(Map, Szx, Szy, Mapid, X_slice, Istart, Iend, 
     &                     Log, Sdim, Sum, Sumtot, Npix, Xy,
     &                     Summin, Summax, Status)
      implicit none
c
c  Do slice calculation
c
c  I  map         (r)  Display map
c  I  szx/y       (i)  Size of image map
c  I  x_slice     (l)  Direction of slice
c  I  istart      (i)  Start pixel to sum (image coords)
c  I  iend        (i)  End pixel to sum (image coords)
c  I  log         (l)  Whether to consider log of Sum
c  I  sdim        (i)  Dimension of slice arrays
c  O  sum         (r)  Average (nonzero) pixel value across image
c  O  sumtot      (r)  Sum total across image
c  O  npix        (i)  Number of nonzero pixels across image
c  O  xy          (r)  Location of summed row or column
c  O  summin      (r)  Minimum sum
c  O  summax      (r)  Maximum sum
c  O  status      (i)  Error flag (0=OK)
c
      integer Szx, Szy, Sdim, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid
      logical X_slice, Log
      integer*4 Istart, Iend
      real*4 Sum(Sdim), Sumtot(Sdim), Xy(Sdim)
      integer Npix(Sdim)
      real*4 Summin, Summax

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      integer di
      real*8 xcen, ycen, zmx, zmy
      real*4 ximg, yimg, xpix, ypix
      integer i, j, numsum
      logical isrnull

      Summin = 0.
      Summax = 0.
      numsum = 0

      Status = 0

      call get_refram(mapid, di, di, zmx, zmy, xcen, ycen, Status)
      if ( Status.ne.0 ) return
c
c add the map value beetwem startpix and endpix, taking into account the 
c non empty number of points. the value is stored in sum(i) the xy(i) 
c has the pixels location in the detector frame. If log is specified 
c then the log value of sum(i) is calculated
c
      if ( X_slice ) then
         do 250 i = 1, Szx
            Sumtot(i) = 0.
            Sum(i) = 0.
            Npix(i) = 0
            do j = Istart, Iend
               if ( .not.isrnull(Map(i,j)) .and.
     &              Map(i,j).gt.0. ) then
                  Sumtot(i) = Sumtot(i) + Map(i,j)
                  Npix(i) = Npix(i) + 1
               endif
            enddo
            if ( Npix(i).gt.0 ) then
               Sum(i) = Sumtot(i)/float(Npix(i))
               numsum = numsum + 1
            endif
            IF ( Log .AND. Sum(i).GT.0. ) Sum(i) = ALOG(Sum(i))
            ximg = i
            yimg = 1
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                     ximg, yimg, 1)
            xy(i) = xpix
            Summin = MIN(Sum(i),Summin)
            Summax = MAX(Sum(i),Summax)
 250     continue
c
c add the map value beetwem y1 and y2, taking into account the non empty
c number of points. the value is stored in sum(i) the xy(i) has the pixels
c value in the image frame. If log is specified then the log value of sum(i)
c is calculated
c
      else
         do 300 j = 1, Szy
            Sumtot(j) = 0.
            Sum(j) = 0.
            Npix(j) = 0
            do 260 i = Istart, Iend
               if ( .not.isrnull(Map(i,j)) .and.
     &              Map(i,j).gt.0. ) then
                  Sumtot(j) = Sumtot(j) + Map(i,j)
                  Npix(j) = Npix(j) + 1
               endif
 260        CONTINUE
            if ( Npix(j).gt.0 ) then
               Sum(j) = Sumtot(j)/float(Npix(j))
               numsum = numsum + 1
            endif
            IF ( Log .AND. Sum(j).GT.0. ) Sum(j) = ALOG(Sum(j))
            ximg = 1
            yimg = j
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                     ximg, yimg, 1)
            Xy(j) = ypix
            Summin = MIN(Sum(j),Summin)
            Summax = MAX(Sum(j),Summax)
 300     CONTINUE
      ENDIF

      if ( numsum.eq.0 ) then
         call xwrite(' Error: no positive data in part of image chosen '
     &               , 5)
         status = -1
      endif

      return
      end
