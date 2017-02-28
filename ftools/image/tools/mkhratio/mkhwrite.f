      subroutine mkhwrite(hdrfile, outfile, rmap, szx, szy, datamin,
     &                    datamax, clobber, status)
      implicit none
c
c  Write output image
c
c  I  hdrfile   (s)  file to copy header from
c  I  outfile   (s)  output image file
c  O  rmap      (i)  real image array
c  I  szx/y     (i)  size of image
c  I  datamin   (d)  Minimum data value
c  I  datamax   (d)  Maximum data value
c  I  clobber   (l)  clobber existing file?
c  O  status    (i)  error flag (0=OK)
c
      character*(*) hdrfile, outfile 
      integer szx, szy, status
      real rmap(szx,szy)
      real*8 datamin, datamax
      logical clobber
c
c  Local variables
c
      integer hlun, olun, blocksize, bitpix, naxis, naxes(2)
      integer group, i, j, nkeys, nelements, di
      logical exists, found
      character(80) card
      real*4 rnull
      integer dec
      parameter(dec=8)

      status = 0

      call ftgiou(hlun,status)
      call ftgiou(olun,status)
c
c  Check for existing file
c
      inquire(file=outfile , exist=exists)
      if ( exists ) then
         if ( clobber ) then
            open(olun, file=outfile , status='old', err=900)
            close(olun, status='delete', err=900)
         else
            call xwrite(' File already exists', 5)
            goto 900
         endif
      endif
c
c    create the new empty FITS file
      blocksize=1
      call ftinit(olun,outfile ,blocksize,status)
c
c     initialize parameters about the FITS image
      group = 1
      bitpix = -32
      naxis = 2
      naxes(1) = szx
      naxes(2) = szy
c
c     write the required header keywords
c     call ftphps(olun,bitpix,naxis,naxes,status)
c
c copy header 
c
      call ftnopn(hlun, hdrfile, 0, status)
      call ftghps(hlun, nkeys, di, status)
      do i = 1, nkeys
         call ftgrec(hlun, i, card, status)
         call ftprec(olun, card, status)
      enddo
      call ftclos(hlun,status)
      call ftfiou(hlun, status)

      call ftukyj(olun,'BITPIX',bitpix,'&',status)
      call ftukyd(olun,'DATAMIN',datamin,dec,'Minimum in array',status)
      call ftukyd(olun,'DATAMAX',datamax,dec,'Maximum in array',status)
      call ftpdat(olun,status)

      nelements = szx*szy
      call ftppne(olun,group,1,nelements,rmap,rnull(),status)
c
cc    close the file and free the unit number
      call ftclos(olun, status)
  900 continue
      call ftfiou(olun, status)

      return 
      end

