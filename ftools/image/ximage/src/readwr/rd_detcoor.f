      subroutine rd_detcoor (Lun, Deqc, Xi, Yi, Idet, Naxes, Cdelt,
     &                       Imgref, Detref, Begzm)
      implicit none
c
c  Read detector coordinates
c
c  I  Lun      (i)  Logical unit of open FITS file
c I/O Deqc     (l)  Whether to set detector coords to same as image
c  I  Xi       (i)  X column or 1 for image
c  I  Yi       (i)  Y column or 2 for image
c  I  Idet     (i)  Satellite/detector index
c  I  Naxes(2) (d)  Image size
c  I  Cdelt(2) (d)  Pixel size
c  O  Imgref(2)(d)  Reference pixel in image coordinates
c  O  Detref(2)(d)  Reference pixel in detector coordinates
c  O  Begzm(2) (d)  Pixel scaling img/det
c
      integer*4 Lun, Xi, Yi, Idet, Naxes(*)
      logical Deqc
      real*8 Cdelt(2), Imgref(2), Detref(2), Begzm(2)
c
      include '../include/maxvals.inc'
c      
c Local variables
c
      integer*4 i, status, warnmsg
      character(80) comment
      real*8 drpix(2), ddelt(2), rebx, reby, imgbin
      real*8 dd

      logical ISDNULL
      
      status = 0
      warnmsg = 15

      Imgref(1) = 0.d0
      Imgref(2) = 0.d0
      Detref(1) = 0.d0
      Detref(2) = 0.d0
      Begzm(1) = 1.d0
      Begzm(2) = 1.d0

      if ( Deqc ) return
c
c  get reference pixel in detector coords 
c
      call FTGKND(Lun,'DRPIX',Xi,1,Detref(1),i,status)
      if ( i.le.0 ) status = -1
      call FTGKND(Lun,'DRPIX',Yi,1,Detref(2),i,status)
      if ( i.le.0 ) status = -1
      if ( status.eq.0 ) then
         call FTGKND(Lun,'CRPIX',Xi,1,Imgref(1),i,status)
         if ( i.le.0 ) status = -1
         call FTGKND(Lun,'CRPIX',Yi,1,Imgref(2),i,status)
         if ( i.le.0 ) status = -1
      endif

      if ( status.eq.0 ) then
         call XWRITE(' Using DRPIX keywords', warnmsg)
      else
         status = 0
         call XWARN('DRPIX keywords not found', warnmsg)
         call gmdbd(Idet, 'DRPIX1', drpix(1), 0, status)
         call gmdbd(Idet, 'DRPIX2', drpix(2), 0, status)
         if ( ISDNULL(Drpix(1)) .or. ISDNULL(Drpix(2)) )
     &        status = -1
         if ( status.eq.0 ) then
            call XWARN(' assuming DRPIX based on mission info', 10)
c
c  Legacy convention in mdb has drpix as whole numbers, assume
c    as center of pixel immediately lower-left of center
c
            Detref(1) = drpix(1) + 0.5
            Detref(2) = drpix(2) + 0.5
            Imgref(1) = dble(Naxes(1))/2. + 0.5
            Imgref(2) = dble(Naxes(2))/2. + 0.5
         else
            call XWARN(' no mission detector info available', warnmsg)
            Deqc = .TRUE.
            return
         endif
      endif
      status = 0
c
c  get pixel size in detector coords
c
      ddelt(1) = 0.d0
      ddelt(2) = 0.d0 

      CALL FTGKND(lun,'DDELT',Xi,1,ddelt(1),i,status)
      if ( i.le.0 ) status = -1
      CALL FTGKND(lun,'DDELT',Yi,1,ddelt(2),i,status)
      if ( i.le.0 ) status = -1

      if ( status.eq.0 ) then
         call XWRITE(' Using DDELT keywords', warnmsg)
      else
         status = 0
         CALL FTGKND(lun,'DRDELT',Xi,1,Ddelt(1),i,status)
         if ( i.le.0 ) status = -1
         CALL FTGKND(lun,'DRDELT',Yi,1,Ddelt(2),i,status)
         if ( i.le.0 ) status = -1

         if ( status.eq.0 ) then
            call XWRITE(' Using DRDELT keywords', warnmsg)
         endif 
      endif

      if ( ddelt(1).ne.0.d0 .and. ddelt(2).ne.0.d0 ) then
         Begzm(1) = abs(Cdelt(1)/ddelt(1))
         Begzm(2) = abs(Cdelt(2)/ddelt(2))
         return
      endif
      
      status = 0
      CALL FTGKYD(lun,'REBIN_X',rebx,comment,status)
      CALL FTGKYD(lun,'REBIN_Y',reby,comment,status)
      if ( status.eq.0 ) then
         call XWRITE(' Using REBIN_X/Y', warnmsg)
         Begzm(1) = rebx
         Begzm(2) = reby
         return
      endif

      status = 0
      CALL FTGKYD(lun,'IMGBIN',imgbin,comment,status)
      if ( status.eq.0 ) then
         call XWRITE(' Using IMGBIN', warnmsg)
         Begzm(1) = imgbin
         Begzm(2) = imgbin
      endif

      status = 0
      call XWARN('DDELT keywords not found', warnmsg)
      if ( Idet.gt.0 ) then
         call XWARN(' using mission info', warnmsg)
         call gmdbd(Idet, 'PIXSIZE', dd, 0, status)
         if ( dd.eq.0.0 .or. status.ne.0 ) then
            call XWARN(' mission info unset, zero DDELT', warnmsg)
            status = -1
         else
            status = 0
            ddelt(1) = dd/3600.
            ddelt(2) = dd/3600.
         endif
      else
         status = -1
      endif
            
      if ( status.eq.0 ) then
         Begzm(1) = abs(Cdelt(1)/ddelt(1))
         Begzm(2) = abs(Cdelt(2)/ddelt(2))
      else
         call XWARN(' Assuming rebin of 1', warnmsg)
         Begzm(1) = 1.d0
         Begzm(2) = 1.d0
      endif

      status = 0

      return
      end
