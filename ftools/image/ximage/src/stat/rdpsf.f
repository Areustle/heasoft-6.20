      subroutine rdpsf(filename, offset, pixsize, first, tmaxpsf, 
     &                 tmpfrac, sfa, status)
      implicit none
c
c  Make PSF correction based on multi-column file from PSF
c  command.
c
c  I  filename   (s)  PSF correction file
c  I  offset     (r)  Distance for optical axis in arcmin
c  I  pixsize    (d)  Pixel size in arcsec
c I/O first      (l)  True on first call from loop,
c                     False for subsequent calls
c  I  tmaxpsf    (i)  Maximum index of tfrac
c  O  tmpfrac    (r)  PSF output
c  O  sfa        (r)  ?area (Carries zoom info)
c  O  status     (i)  Error flag (0=OK)
c
      character*(*) filename
      integer*4 tmaxpsf, status
      real*4 offset, tmpfrac(0:tmaxpsf), sfa
      real*8 pixsize
      logical first

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include 'ximpsf.inc'
c
c  Local variables
c
      integer*4 i, ncols, nrows, cola, colb, icol
      integer*4 location, unittype
      real*4 offaxis(MXQCOL), tmpoff
      integer*4 order(MXQCOL)
      real*4 psfbufa(0:MAXPSF), psfbufb(0:MAXPSF), psfbufr(0:MAXPSF)
      integer irad1, irad2
      real*8 pixscal
      real*4 rad, a1, b1, lga, lgb
      logical found

      character*(MAX_FILELEN) savfile

      DATA savfile/' '/
      SAVE savfile, offaxis, unittype, order

      pixscal = 0.d0

      if ( first ) then
         if ( tmaxpsf.gt.MAXPSF ) then
            call xwarn(' TMAXPSF argument in rdpsf > MAXPSF', 5)
            call xwarn(' Should never happen', 5)
         endif
         call XWRITE (' Reading psf file:', 10)
         call XWRITE (filename, 10)
         savfile = ' '
      endif

      call txinfo(filename, ncols, nrows, status)
      if ( status.ne.0 ) return

      if ( savfile.ne.filename ) then
c       UNITTYPE of 1 = detector pix, 2 = arcmin, 3 = arcsec
c                   (assume 1 if not there)
         call txrdikey(filename, 'UNITTYPE', 0, unittype, status)
         if ( status.ne.0 ) unittype = 1
         status = 0
         offaxis(1) = -1.0
         order(1) = 1
         found = .FALSE.
         do i = 2, ncols
            offaxis(i) = -1.0
            order(i) = i
            call txrdkey(filename, 'OFFAXIS', i, offaxis(i), status)
            if ( status.eq.0 ) found = .TRUE.
            status = 0
         enddo
c
c  If OFFAXIS keyword missing, -1.0 is recorded, column ignored. 
c  If no OFFAXIS keywords, always use second column
c
         if ( .not.found ) offaxis(2) = 0.0

         call qsort_detect(offaxis, order, MXQCOL, ncols, Status)
         if ( status.ne.0 ) return
      endif

      if ( unittype.eq.1 ) then
         tmpoff = offset/(pixsize/60.)
      elseif ( unittype.eq.2 ) then
         tmpoff = offset
         pixscal = pixsize/60.
      elseif ( unittype.eq.3 ) then
         tmpoff = offset/60.
         pixscal = pixsize
      else
         call xwarn(' Bad UNITTYPE in PSF file', 5)
         status = -1
         savfile = ' '
         return
      endif
      icol = location(offaxis, ncols, tmpoff)

      if ( icol.lt.2 ) then
         cola = order(2)
         colb = order(2)
      elseif ( icol.ge.ncols ) then
         cola = order(ncols)
         colb = order(ncols)
      else
         cola = order(icol)
         colb = order(icol+1)
      endif

      write(ZWRite, *) ' rdpsf: offset = ', tmpoff,
     &                 ' columns = ', cola, colb
      call XWRITE(ZWRite, 25)

c the first value in the value is distance in pixels number from the
c center of source. the value of the psf is refereed to a box
c 2n+1. if the first value is 0 this means that the psf value is foer a box
c of 1 pixel (if the pixels are the original pixels that means the rebin=1)
c rescale the psf read from the file taking into account the zoom used for
c calcolating teh psf

      call txrdcol(filename, 1, MAXPSF+1, psfbufr, nrows, status)
      if ( unittype.eq.1 ) then
         sfa = (2.*psfbufr(0)+1)**2.
      else
         sfa = 1.0
      endif

      call txrdcol(filename, cola, MAXPSF+1, psfbufa, nrows, status)
      call txrdcol(filename, colb, MAXPSF+1, psfbufb, nrows, status)
      i = 0
      do while ( i.le.tmaxpsf )

         if ( unittype.eq.1 ) then
            rad = float(i)*SQRT(sfa) + (SQRT(sfa)-1)/2.
            irad1 = MIN(i,nrows-1)
            irad2 = irad1
         else
            rad = (float(i)+0.5)*pixscal
            if ( rad.le.psfbufr(0) ) then
               irad1 = 0
               irad2 = 0
            elseif ( rad.ge.psfbufr(nrows-1) ) then
               irad1 = nrows - 1
               irad2 = nrows - 1
            else
               irad2 = 0
               do while ( irad2.le.nrows-1 .and. rad.gt.psfbufr(irad2) ) 
                  irad2 = irad2 + 1
               enddo
               irad1 = irad2 - 1
            endif
         endif
c
c  Interpolation from SAX version
c
         if ( irad1.eq.irad2 ) then
            a1 = psfbufa(irad1)
            b1 = psfbufb(irad1)
         else
            a1 = psfbufa(irad1) + (psfbufa(irad2) - psfbufa(irad1))/
     &           (psfbufr(irad2) - psfbufr(irad1))*
     &           (rad-psfbufr(irad1))
            b1 = psfbufb(irad1) + (psfbufb(irad2) - psfbufb(irad1))/
     &           (psfbufr(irad2) - psfbufr(irad1))*
     &           (rad-psfbufr(irad1))
         endif
         lga = alog10(a1)
         lgb = alog10(b1)
         if ( cola.eq.colb ) then
            tmpfrac(i) = 10**lga
         else
            tmpfrac(i) = 10**( lga + (lgb-lga)
     &               /(offaxis(icol+1) - offaxis(icol))
     &               *(tmpoff - offaxis(icol)) )
         endif
c
c  Linear interpolation
c
c        tmpfrac(i-1) = ((tmpoff - offaxis(icol))
c    &               (offaxis(icol+1) - offaxis(icol))
c    &               *(psfbufb(i-1)-psfbufa(i-1))) + psfbufa(i-1)
c
         i = i + 1
      enddo

      savfile = filename

      return
      end
