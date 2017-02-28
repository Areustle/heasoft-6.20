      subroutine rd_img_xim(Lun,Imgnum,Type,Map,Szx,Szy,
     &                      Usrzm,Naxes,Cenpix,Exposure,
     &                      Ximscale,Ximzero,Unit,Datamin,Datamax,
     &                      Maptype,Status)
      implicit none
c
c  Core image reader
c  
c  I  Lun      (i) Logical unit of open FITS file
c  I  Imgnum   (i) Image number
c  I  Type     (c) Type of image ('IMG' or 'EXP')
c  O  Map      (r) Image map
c  I  Szx      (i) Size in x direction (to be read)
c  I  Szy      (i) Size in y direction (to be read)
c  I  Usrzm(2) (d) User-entered rebin factor
c  I  Naxes    (i) Size of FITS image (in file)
c  I  Cenpix   (d) Center in pixel coordinates
c  I  Exposure (d) Exposure time
c I/O Ximscale (d) Image scaling to apply
c I/O Ximzero  (d) Image offset to apply
c  O  Unit     (c) Unit of image values
c I/O Datamin  (d) Minimum data value (filter)
c I/O Datamax  (d) Maximum data value (filter)
c  O  Maptype  (c) Data type of map ('I' or 'R')
c  I  Status   (i) Error flag (0 = OK)
c
      integer*4 Lun, Imgnum, Szx, Szy
      character*(*) Type, Maptype, Unit
      real*4 Map(Szx,Szy)
      integer*4 Naxes(3)
      real*8 Usrzm(2), Cenpix(2)
      real*8 Exposure, Datamin, Datamax, Ximscale, Ximzero
      integer*4 Status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      integer*4 i, j, starow, endrow, ir, p_row, bitpix
      integer*4 xtmp, ytmp, xref, yref
      integer*4 group, bufsize, bufaxes(2), istart, nelements
      integer*4 imgoffset
      character(30) errmsg
      real*4 nullval, rnull, rmin, rmax
      logical anyf, isrnull
      character(80) comment
      real*8 bscale, bzero

      CALL XWRITE(' Reading an image ',10)
c
c  Initialize map
c
      Status = 0
      do i = 1, Szx
         do j = 1, Szy
            Map(i,j) = rnull()
         enddo
      enddo
c
c  Reference center
c    Cenpix assumes integer values in pixel center (Must be between
c                                                   pixels)
c    xref/yref must assume integer values at pixel edge
c
      xref = INT(Cenpix(1) - 0.5)
      yref = INT(Cenpix(2) - 0.5)
c
c  Read data type
c
      call ftgidt(Lun, bitpix, status)
      if ( status.ne.0 ) return
      call ftgkys(lun, 'BUNIT', unit, comment, status)
      if ( status.ne.0 ) then
         unit = 'count'
         status = 0
      endif
      if ( bitpix.gt.0 ) then
         call ftgkyd(lun, 'BSCALE', bscale, comment, status)
         call ftgkyd(lun, 'BZERO', bzero, comment, status)
         if ( status.ne.0 ) then
            Maptype = 'I'
            status = 0
         else
c
c  If BSCALE = 1.0 and BZERO = 0.0, assume int, otherwise float
c
            if ( bscale.eq.1.d0 .and. bzero.eq.0.d0 ) then
               Maptype = 'I'
            else
               Maptype = 'R'
            endif
         endif
      else
         Maptype = 'R'
      endif
c
c  Read image
c
      bufaxes(1) = Naxes(1)
      bufsize = MAX(Naxes(1),Szx*Szy)
      bufaxes(2) = bufsize/Naxes(1)
      call ralloc(1, bufsize, 1, p_row, status)
      if ( status.ne.0 ) return
      starow = MAX(1, yref - Szy/2*INT(Usrzm(2)) + 1)
      endrow = MIN(Naxes(2), yref + Szy/2*INT(Usrzm(2)), 
     &                       starow + bufaxes(2) - 1)
      nullval = rnull()
      group = 0
      imgoffset = (imgnum - 1)*Naxes(1)*Naxes(2)

      write(ZWRite,*) ' Reading into map buffer '
      call XWRITE(ZWRite, 20)

      do while ( starow.le.endrow ) 

         istart = Naxes(1)*(starow - 1) + 1 + imgoffset
         nelements = Naxes(1)*(endrow - starow + 1)

         write(ZWRite,*) ' Read rows: ',starow, endrow
         call XWRITE(ZWRite, 25)
         write(ZWRite,*) ' First element: ',istart
         call XWRITE(ZWRite, 25)
         write(ZWRite,*) ' Number of elements: ', nelements
         call XWRITE(ZWRite, 25)

         call ftgpve(Lun,group,istart,nelements,nullval,memr(p_row),
     &               anyf,status)

         if ( Status.ne.0 ) then
            call XWRITE(' Failed to read image', 10)
            call FTGERR(status, errmsg)
            call XWRITE(errmsg, 10)
            return
         endif

         write(ZWRite,*) ' Transferring ',bufaxes(1),' x ',
     &                   endrow-starow+1, ' buffer into MAP'
         call XWRITE(ZWRite, 25)

         do j = starow, endrow
            do i = 1, bufaxes(1)
               xtmp = float(i - xref - 1)/Usrzm(1) + Szx/2. + 1.
               ytmp = float(j - yref - 1)/Usrzm(2) + Szy/2. + 1.
               ir = i + (j-starow)*bufaxes(1)
               if ( xtmp.le.Szx .and. xtmp.ge.1 .and.
     &              ytmp.le.Szy .and. ytmp.ge.1 ) then
                  if ( isrnull(Map(xtmp,ytmp)) ) then
                     Map(xtmp,ytmp) = memr(p_row+ir-1)
                  else
                     Map(xtmp,ytmp) = memr(p_row+ir-1) + Map(xtmp,ytmp)
                  endif
               endif
            enddo
         enddo

         starow = endrow + 1
         endrow = MIN(Naxes(2), yref + Szy/2*INT(Usrzm(2)), 
     &                starow + bufaxes(2) - 1)

      enddo

      call ralloc(0, Szx, 1, p_row, status)
      status = 0
c
c  Find min/max
c
      rmin = rnull()
      rmax = rnull()

      do i = 1, Szx
         do j = 1, Szy
            if ( .not.isrnull(Map(i,j)) ) then
               Map(i,j) = Map(i,j)*Ximscale+Ximzero
               if ( isrnull(rmin) .or. Map(i,j).lt.rmin ) 
     &            rmin = Map(i,j)
               if ( isrnull(rmax) .or. Map(i,j).gt.rmax ) 
     &            rmax = Map(i,j)
            endif
         enddo
      enddo
c
c   Set min/max to zero for all-null image
c
      if ( isrnull(rmin) .and. isrnull(rmax) ) then
         rmin = 0
         rmax = 0
      endif

      if ( rmax.gt.float(MAX_INT) ) then
         Maptype = 'R'
      endif

      Datamin = rmin
      Datamax = rmax
c
      return
      end
