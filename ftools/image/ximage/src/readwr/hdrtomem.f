      subroutine hdrtomem(Mapid, Filename, Status)
      implicit none
c
c  Write map header to memory FITS (mem://) for reading 
c  into AST FitsChan
c
c  I  mapid    (s)  Map id string
c  I  filename (s)  FITS memory spec (mem://string)
c  O  status   (i)  Error flag (0=OK)
c
      INTEGER*4 Status
      CHARACTER*(*) Mapid, Filename

      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
c
c Local variables
c
      INTEGER*4 LENACT
      CHARACTER*(MAX_FILELEN) template
      INTEGER*4 lun, block, bitpix, naxis, naxes(2)
      INTEGER*4 svlun

      DATA svlun /-1/
c
      Status = 0
      template = ' '
c
c  Cleanup temporary mem file from last call to this routine
c
      if ( svlun.ne.-1 ) then
         call ftclos(svlun, status)
         call frelun(svlun)
         if ( status.ne.0 ) then
            call xwrite("Failed to close last mem: file", 5)
            return
         endif
      endif

      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      ZWRite = ' Writing FITS file: '//filename(:LENACT(filename))
      CALL XWRITE(ZWRite,15)

      CALL GETLUN(lun)
      block = 1
      CALL FTINIT(lun,filename,block,status)
      IF ( status.NE.0 ) THEN
         call XWRITE (' error initializing internal file', 10)
         return
      ENDIF
c
c  Dummy 1x1 image
c
      bitpix = 16
      naxis = 2
      naxes(1) = 0
      naxes(2) = 0
      call ftcrim(lun, bitpix, naxis, naxes, status)
c
c  Write header
c
      call wr_fithdr(lun, mapid, template)
c
c  Leave open so memory is persistent
c
c     call ftclos(lun, status)
c     call frelun(lun)
      svlun = lun

      RETURN
      END
