      subroutine rd_gti (lun, gtinum, igtistart, igtistop, 
     &                   gtiuni, status)

      implicit none
      include '../include/dynmem.inc'
c
c Reads and returns GTIs for open fits file
c  Assume current HDU is set to the GTI unless currently at
c   primary array (HDU=1) and then search for a GTI extension
c
c  I  lun       (i)   Logical unit of open FITS file
c  O  gtinum    (i)   Number of values into GTI arrays
c  O  igtistart (i)   Array pointer of start times for GTIs
c  O  igtistop  (i)   Array pointer of stop times for GTIs
c  O  gtiuni    (c)   Unit of times in GTI arrays
c  O  status    (i)   Error flag (0=OK)
c
      integer lun, gtinum, igtistart, igtistop
      character*(*) gtiuni
      integer status
c
c Local variables
c
      integer numkeys,numvals 
      parameter (numkeys = 2, numvals = 1)
      character(8) keylist(numkeys)
      character(8) vallist(numvals)
      data keylist /'EXTNAME','HDUCLAS1'/
      data vallist /'*GTI'/
      
      integer nrows, svhdu
      character(80) comment
      integer starti, stopi, numfnd, ihdu, hdutype
      logical anyf

      gtiuni = ' '
      status = 0
      nrows = 0

      call FTGHDN(lun, svhdu)
c
c Find the good time interval (GTI) table
c
      if ( svhdu.le.1 ) then
         call matchext(lun,keylist,numkeys,vallist,numvals,ihdu,status)
         if ( status.ne.0 ) then
            gtinum = 0
            return
         endif
      endif

      call FTGKYJ(lun,'NAXIS2',nrows,comment,status)
c
      if ( nrows.gt.0 ) then

         call gtialloc(1, nrows, igtistart, igtistop, status)
         if ( status.ne.0 ) return
c
c read start column
         call FTGCNO(lun,.FALSE.,'start',starti,status)
         call FTGCVD(lun,starti,1,1,nrows,0.d0,memd(igtistart),anyf,
     &               status)
c
c read stop column
         call FTGCNO(lun,.FALSE.,'stop',stopi,status)
         call FTGCVD(lun,stopi,1,1,nrows,0.d0,memd(igtistop),anyf,
     &               status)

c determine unit of start and stop 
c
         call FTGKNS(lun,'TUNIT',starti,1,gtiuni,numfnd,status)
         if ( status.eq.0 ) then
            call UPC(gtiuni)
            call RMVBLK(gtiuni)
         else
            nrows = 0
         endif

      endif

      gtinum = nrows

      status = 0
      call FTMAHD(lun, svhdu, hdutype, status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not move back to HDU from before GTI read',
     &               5)
      endif

      return
      end
