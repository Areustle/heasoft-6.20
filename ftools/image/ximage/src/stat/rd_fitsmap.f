      subroutine rd_fitsmap (Filename, Map, Szx, Szy, Imgref,
     &                       Detref, Begzm, Datamin, Datamax, Status)
      implicit none
c
c  Read simple map (i.e. Szx=Naxes(1) and Szy=Naxes(2))
c  Intended for use reading background and vignetting correction maps
c
c  I  Filename  (s)  Filename of FITS map
c  O  Map       (r)  Where FITS map is read into
c  I  Szx,Szy   (i)  Size of map
c  O  Imgref    (d)  Reference point in image coords
c  O  Detref    (d)  Reference point in detector coords
c  O  Begzm(2)  (d)  FITS map zoom
c  O  Datamin   (d)  Minimum map value
c  O  Datamax   (d)  Maximum map value
c  O  Status    (i)  Error flag (0 = OK)
c
      character*(*) Filename
      integer*4 Szx, Szy, Status
      real*8 Imgref(2), Detref(2), Begzm(2), Datamin, Datamax
      real*4 Map(Szx,Szy)

      include '../include/io.inc'
c
c Local variables
c
      integer*4 lun, maxstat
      integer*4 block, bitpix, gcount, pcount
      integer*4 i, j, group, nullval, idim
      logical simple, extend, anyf
      integer*4 naxis, naxes(2)
      real*8 crpix(2), cdelt(2), crval(2), crota2
      character(80) ctype(2), cunit(2), comment
      real*8 cenpix(2)
      real*4 rmin, rmax, rnull
      logical isrnull
      
      status = 0
c
c Open FITS file 
c
      block = 1
      call GETLUN(lun)
      call FTOPEN(lun, Filename, 0, block, status)
      call FTGHPR(lun, 2, simple, bitpix, naxis, naxes, pcount, gcount,
     &            extend, status)
      if ( status.ne.0 ) then
         call XWRITE(' Error reading integer map: ', 10)
         call XWRITE(Filename, 10)
         goto 500
      endif

      if ( naxes(1).ne.Szx .or. naxes(2).ne.Szy ) then
         call XWRITE(' Integer map buffer and map file are '//
     &               'different sizes', 10)
         status = -1
         goto 500
      endif
c
c Read needed keywords
c
      cenpix(1) = float(naxes(1))/2. + 0.5
      cenpix(2) = float(naxes(2))/2. + 0.5
      call rd_ckeys (lun, 1, 2, cenpix, ctype, cunit, crval, crpix,
     &               cdelt, crota2)
      call rd_detcoor(lun, .FALSE., 1, 2, -2, naxes, cdelt, Imgref,
     &                Detref, Begzm)
c
c   Data min/max
c
      maxstat = 0
      call FTGKYD (lun,'DATAMIN',Datamin,comment,maxstat)
      call FTGKYD (lun,'DATAMAX',Datamax,comment,maxstat)
c
c Read in whole image
c
c NOTE: Since this routine is used only to read in background
c       and vignetting maps, NULLs are not expected, so set to 1
c
      group = 0
      nullval = 1.
      idim = Szx
      call FTG2DE (lun, group, nullval, idim, Szx, Szy, 
     &             Map, anyf, Status)
      if ( anyf ) call xwarn(' Unexpected NULL values present', 10)
c
c Close FITS file
c
 500  continue
      call FTCLOS(lun, status)
      call FRELUN(lun)
c
c  If no DATAMIN DATAMAX keywords, look through image
c
      if ( maxstat.eq.0 ) then
         call xwrite(' Determine fits image min/max from keywords', 15)
      else
         rmin = rnull()
         rmax = rnull()

         do i = 1, Szx
            do j = 1, Szy
               if ( isrnull(rmin) .or. Map(i,j).lt.rmin )
     &            rmin = Map(i,j)
               if ( isrnull(rmax) .or. Map(i,j).gt.rmax )
     &            rmax = Map(i,j)
            enddo
         enddo
c
c   Set min/max to zero for all-null image
c
         if ( isrnull(rmin) .and. isrnull(rmax) ) then
            rmin = 0
            rmax = 0
         endif

         Datamin = rmin
         Datamax = rmax
         call xwrite(' Determine fits image min/max from image', 15)
      endif

      return
      end
