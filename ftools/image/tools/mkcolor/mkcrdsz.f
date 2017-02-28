      subroutine mkcrdsz(rfile, gfile, bfile, szx, szy, status)
      implicit none
c
c  Look up image size
c
c  I  rfile     (s)  red image file
c  I  gfile     (s)  green image file
c  I  bfile     (s)  blue image file
c  O  szx       (i)  image size in x
c  O  szy       (i)  image size in y
c  O  status    (l)  error flag (0 = OK)
c  
      character*(*) rfile, gfile, bfile
      integer szx, szy, status
c
c  Local variables
c
      integer lun, readwrite, blocksize, naxes(2), nfound
      integer rszx, rszy, gszx, gszy, bszx, bszy

      readwrite = 0
      status = 0
c
c     Get an unused Logical Unit Number to use to open the FITS files
      call ftgiou(lun,status)
      if ( status.ne.0 ) goto 900
c
c  Read size for red image
c
      call ftopen(lun,rfile,readwrite,blocksize,status)
      call ftgknj(lun,'NAXIS',1,2,naxes,nfound,status)
      if ( status.ne.0 .or. nfound.ne.2 ) then
         status = -1
         call xwrite(' Failed to read NAXISn keywords for red image', 5)
      endif
      rszx = naxes(1)
      rszy = naxes(2)
      call ftclos(lun, status)
c
c  Read size for green image
c
      call ftopen(lun,gfile,readwrite,blocksize,status)
      call ftgknj(lun,'NAXIS',1,2,naxes,nfound,status)
      if ( status.ne.0 .or. nfound.ne.2 ) then
         status = -1
         call xwrite(' Failed to read NAXISn keywords for green image',
     &                5)
      endif
      gszx = naxes(1)
      gszy = naxes(2)
      call ftclos(lun, status)
c
c  Read size for blue image
c
      call ftopen(lun,bfile,readwrite,blocksize,status)
      call ftgknj(lun,'NAXIS',1,2,naxes,nfound,status)
      if ( status.ne.0 .or. nfound.ne.2 ) then
         status = -1
         call xwrite(' Failed to read NAXISn keywords for blue image',
     &                5)
      endif
      bszx = naxes(1)
      bszy = naxes(2)
      call ftclos(lun, status)

      call ftfiou(lun,status)
      if ( status.ne.0 ) goto 900

      if ( rszx.ne.gszx .or. rszx.ne.bszx .or.
     &     rszy.ne.gszy .or. rszy.ne.bszy ) then
         call xwrite(' Image size mismatch', 5)
         status = -1
      else
         szx = rszx
         szy = rszy
      endif

 900  continue
      return
      end
