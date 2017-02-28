      subroutine wr_fits( filename, template, map, szx, szy, mapid,
     &                    maptype, nonull, status)
      implicit none
c
c  Write map to fits
c
c  I  filename   (c) Filename for fits file
c  I  template   (c) Header template file
c  I  map        (r) Image map
c  I  szx,szy    (i) Size of map
c  I  mapid      (c) Header id
c  I  maptype    (c) Map data type (I=integer, R=real)
c  I  nonull     (l) If true, exclude null values (set to 0)
c  O  status     (i) Error flag
c
      CHARACTER*(*) filename, template, mapid, maptype
      INTEGER*4 szx, szy, status
      REAL*4 map(szx,szy)
      LOGICAL nonull

      include '../include/dynmem.inc'

      INTEGER*4 LENACT
c
c Local variables
c
      integer*4 i, j, lun, blank, inull, p_tmp
      integer*4 block, bitpix, naxis, naxes(2)
      real*4 rnull
      character(30) errtext
      real*8 datamin, datamax
      logical isrnull

      include '../include/io.inc'
    
      ZWRite = ' Writing FITS file: '//filename(:LENACT(filename))
      CALL XWRITE(ZWRite,5)
      CALL GETLUN(lun)
      block = 1
      CALL FTINIT(lun,filename,block,status)
      IF ( status.NE.0 ) THEN
         call XWRITE (' error initializing file', 10)
         return
      ENDIF

      call gheadd(mapid, 'DATAMAX', datamax, 0, status)
      if ( status.eq.0 ) call gheadd(mapid, 'DATAMIN', datamin, 0, 
     &                               status)
      IF ( status.ne.0 ) THEN
         call XWRITE (' error getting file min/max', 10)
         status = -1
         goto 500
      ENDIF

      if ( maptype.eq.'I' ) then
         if ( datamax.gt.32768. .or. datamin.lt.-32767. ) then
            bitpix = 32
            blank = inull()
         else
            bitpix = 16
            blank = -32768
         endif
      else
         bitpix = -32
      endif
      naxis = 2
      naxes(1) = szx
      naxes(2) = szy
      call ftcrim(lun, bitpix, naxis, naxes, status)
      call wr_fithdr (lun, mapid, template)
      call ftpdat (lun, status)

      if ( maptype.eq.'I' ) then
         if ( nonull ) then
            call ftpnul(lun, 0, status)
         else
            call ftpnul(lun, blank, status)
            call ftpkyj(lun, 'BLANK', blank, 'Value of null pixels',
     &                  status)
         endif
         call ftppne(lun, 0, 1, szx*szy, map, rnull(), status)
      else
         if ( nonull ) then
            call ralloc(1, szx, szy, p_tmp, status)
            if ( status.ne.0 ) then
               call xwrite(' Failed to allocate buffer to filter'//
     &                     ' out NULLs', 5)
               return
            endif
            do i = 1, szx
               do j = 1, szy
                  if ( isrnull(map(i,j)) ) then
                     call mapvalset(memr(p_tmp),szx,szy,i,j,0.)
                  else
                     call mapvalset(memr(p_tmp),szx,szy,i,j,map(i,j))
                  endif
               enddo
            enddo
            call ftp2de(lun,0,szx,naxes(1),naxes(2),memr(p_tmp),status)
            call ralloc(0, szx, szy, p_tmp, status)
         else
            call ftppne(lun, 0, 1, szx*szy, map, rnull(), status)
         endif
      endif
      call ftpcks(lun, status)

 500  continue
      call ftclos(lun, status)
      call frelun(lun)
      if ( status.eq.0 ) then
         write (ZWRite,'(1x,i15,2a)') Szx*Szy, ' records written',
     &                                     ' to file'
         call RMVXBK(ZWRite(2:))
         call XWRITE(ZWRite,5)
      else
         call XWRITE(' Failed to write fits image', 5)
         call ftgerr(status, errtext)
         call XWRITE(errtext, 15)
      endif
            
      RETURN
      END
