      subroutine mkhrdsz(afile, bfile, szx, szy, status)
      implicit none
c
c  Look up image size
c
c  I  afile     (s)  image file A
c  I  bfile     (s)  image file B
c  O  szx       (i)  image size in x
c  O  szy       (i)  image size in y
c  O  status    (l)  error flag (0 = OK)
c  
      character*(*) afile, bfile
      integer szx, szy, status
c
c  Local variables
c
      integer lun, readwrite, blocksize, naxes(2), nfound
      integer aszx, aszy, bszx, bszy

      readwrite = 0
      status = 0
c
c     Get an unused Logical Unit Number to use to open the FITS files
      call ftgiou(lun,status)
      if ( status.ne.0 ) goto 900
c
c  Read size for image A
c
      call ftopen(lun,afile,readwrite,blocksize,status)
      call ftgknj(lun,'NAXIS',1,2,naxes,nfound,status)
      if ( status.ne.0 .or. nfound.ne.2 ) then
         status = -1
         call xwrite(' Failed to read NAXISn keywords for image A', 5)
      endif
      aszx = naxes(1)
      aszy = naxes(2)
      call ftclos(lun, status)
c
c  Read size for image B
c
      call ftopen(lun,bfile,readwrite,blocksize,status)
      call ftgknj(lun,'NAXIS',1,2,naxes,nfound,status)
      if ( status.ne.0 .or. nfound.ne.2 ) then
         status = -1
         call xwrite(' Failed to read NAXISn keywords for image B',
     &                5)
      endif
      bszx = naxes(1)
      bszy = naxes(2)
      call ftclos(lun, status)

      call ftfiou(lun,status)
      if ( status.ne.0 ) goto 900

      if ( aszx.ne.bszx .or. aszx.ne.bszx ) then
         call xwrite(' Image size mismatch', 5)
         status = -1
      else
         szx = aszx
         szy = aszy
      endif

 900  continue
      return
      end
