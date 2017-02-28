      SUBROUTINE WRDETECT(Mapid, Equinox, Filedet, Fitsdet, Csz, 
     &                    Cbufcnt, Cbuferr, Cbufx, Cbufy, Cbufvcor,
     &                    Cbufra, Cbufdec, Cbuferad, Cbufhbox, Cbufprob,
     &                    Cbufsnr, Status)
      IMPLICIT NONE
c
c    Writes detect output to screen and file(s)
c
c  I  mapid    (s)   Map id string
c  I  equinox  (i)   XIMAGE equinox
c  I  filedet  (s)   Plain text output file
c  I  fitsdet  (s)   FITS output file (source list)
c  I  csz      (i)   Size of column buffers
c  O  cbufcnt  (r)   Column buffer for count rate
c  O  cbuferr  (r)   Column buffer for count rate error
c  O  cbufx    (r)   Column buffer for source x position
c  O  cbufy    (r)   Column buffer for source y position
c  O  cbufvcor (r)   Column buffer for vignetting correction
c  O  cbufra   (d)   Column buffer for source RA position
c  O  cbufdec  (d)   Column buffer for source Dec position
c  O  cbuferad (d)   Column buffer for error radius
c  O  cbufhbox (d)   Column buffer for half-box size
c  O  cbufprob (d)   Column buffer for probability
c  O  cbufsnr  (d)   Column buffer for signal to noise ratio
c  O  status   (i)   Error flag (0=OK)
c
      INTEGER*4 Equinox, Csz, Status
      CHARACTER*(*) Mapid, Filedet, Fitsdet
      REAL Cbufcnt(Csz), Cbuferr(Csz), Cbufx(Csz), Cbufy(Csz) 
      REAL Cbufvcor(Csz), Cbuferad(Csz), Cbufhbox(Csz)
      REAL Cbufprob(Csz), Cbufsnr(Csz)
      REAL*8 Cbufra(Csz), Cbufdec(Csz)
 
      INCLUDE '../include/io.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
      INCLUDE 'detect.inc'
c
c  Local variables
c
      integer*4 found, i, LENACT
      INTEGER*4 ierror_rad , psize, ngood
      INTEGER*4 lun, ier
c
      REAL*4 error_rad , ERROR_RADIUS , csec , cerr , vcor
      REAL*4 sisterr , snrat , dist
      REAL*4 hbox, snr
      real*8 xoff, yoff, dd
c
      character*(MAX_IDSTR) exmapid, wcsid
      logical cma, first, badexpo, vignapp
      real*4 xpos, ypos, xpix, ypix
      character(80) object, filein, ctype1, ds
      character(80) dateobs, dateend
      character(100) telstr
      real*8 exposure, dtime, mapexp, xcen, ycen, ximg, yimg
      real*8 ra, dec, pixsize, dtra, dtdec, epoch, zmx, zmy
      character(30) xpstr, ypstr, corstr, rastr, decstr, buf1, buf2
      character(40) system, proj, xlab, ylab, unit, bunit
      integer*4 itel, szx, szy, p_vign, ivign, slen1, slen2
      real*4 imgpix, detpix, arcmin, arcminsec, detpixsec
      real*4 dr3(3)
      integer exmapptr, exszx, exszy
 
      integer block
      character*(MAX_FILELEN) template
 
      integer tfields
      parameter ( tfields=11 )
      character(12) ttype(tfields)
      character(2) tform(tfields)
      character(8) tunit(tfields)
      logical there, ISRNULL
c
      IF ( NUMdet.EQ.0 ) THEN
         CALL XWRITE(' No sources detected', 10)
         RETURN
      ENDIF
c
c  Retrieve header info
c
      exmapid = ' '
      CALL gheads(mapid, 'EXMAPID', exmapid, 0, status)
      vignapp = .FALSE.
      IF ( exmapid.NE.' ' ) THEN
         CALL gheadi(exmapid, 'MAPPTR', exmapptr, 0, status)
         CALL gheadi(exmapid, 'SZX', exszx, 0, status)
         CALL gheadi(exmapid, 'SZY', exszy, 0, status)
         CALL gheadi(exmapid, 'VIGNAPP', ivign, 0, status)
         if ( ivign.gt.0 ) vignapp = .TRUE.
      ENDIF
      CALL gheads(mapid, 'OBJECT', object, 0, status)
      CALL get_itel(mapid, itel)
      CALL get_telstr(mapid, telstr)
      CALL gheadd(mapid, 'EXPOSURE', exposure, 0, status)
      CALL gheadd(mapid, 'DTIME', dtime, 0, status)
      CALL gheads(mapid, 'FILE', filein, 0, status)
      wcsid = ' '
      CALL gheads(mapid, 'WCSID', wcsid, 0, status)
      IF ( wcsid.EQ.' ' ) THEN
         CALL xwrite(' WCSID not found', 5)
         status = -1
         RETURN
      ENDIF
      CALL gheadd(mapid, 'CDELT1', pixsize, 0, status)
      CALL get_refram(mapid, szx, szy, zmx, zmy, xcen, ycen, status)
      ximg = float(szx)/2. + 0.5
      yimg = float(szy)/2. + 0.5
      CALL wcsimgsky(wcsid, ximg, yimg, ra, dec, Equinox, 1, status)
      CALL wcsfrminfo(wcsid, system, proj, xlab, ylab, unit, epoch)
      CALL gheads(mapid, 'DATE-OBS', dateobs, 0, status)
      CALL gheads(mapid, 'DATE-END', dateend, 0, status)
      CALL gheads(mapid, 'BUNIT', bunit, 0, status)
      CALL gheads(mapid, 'CTYPE1', ctype1, 0, status)
c
c  Retrieve mdb info
c
      cma = .FALSE.
      IF ( itel.GT.0 ) THEN
         IF ( ZTElescop(itel).EQ.'EXOSAT'.AND.ZINstrume(itel)(1:3)
     &        .EQ.'CMA' ) THEN
            cma = .TRUE.
            CALL get_optax(itel, xcen, ycen, xoff, yoff)
         ENDIF
      ENDIF
c
c open output file. the output file willhave the same name of
c the input file, otherwise (simulate image) detect.out
c
      IF ( object(1:16).EQ.'  simulation    ' ) Filedet = ' '
      badexpo = .FALSE.
      IF ( exposure.LE.0. ) THEN
         exposure = 1.
         badexpo = .TRUE.
      ENDIF
      IF ( exmapid.ne.' ' .or. .not.badexpo ) THEN
         bunit(MIN(LENACT(bunit)+1,6):) = '/s'
      ENDIF
 
      IF ( Filedet(1:1).EQ.' ' ) THEN
         Filedet = ' '
         CALL gheads(mapid, 'ROOT', Filedet, 0, status)
         IF ( Filedet.EQ.' ' ) Filedet = 'detout'
      ENDIF
c
c  Output detect file
c
      CALL XTEND(Filedet, 'det')
      CALL GETLUN(lun)
      CALL OPENWR(lun, Filedet, 'unknown', ' ', ' ', 0, 0, ier)
      IF ( ier.NE.0 ) THEN
         WRITE (ZWRite, '(3a,i4)') ' Error opening output file ',
     &                            Filedet(:LENACT(Filedet)), ' no ', ier
         CALL XWRITE(ZWRite, 5)
         CALL FRELUN(lun)
         RETURN
      ENDIF
      found = 0

      WRITE (lun, '(2a)') '! Field Name     : ', object(:LENACT(object))
      WRITE (lun, '(2a)') '! Instrument     : ', telstr(:LENACT(telstr))

      CALL xistr(NUMdet, buf1, slen1)
      WRITE (lun, '(2a)') '! No of sources  : ', buf1(:slen1)

      dd = exposure
      if ( badexpo ) dd = 0.
      call xdstr(dd, -1, buf1, slen1)
      WRITE (lun, '(2a)') '! Exposure (sec) : ', buf1(:slen1)
      WRITE (lun, '(2a)') '! Input file     : ', filein(:LENACT(filein))

      CALL backvals(Mapid, imgpix, detpix, arcmin, detpixsec, arcminsec, 
     &              status)
      CALL xdstr(zmx, 5, buf1, slen1)
      CALL xdstr(zmy, 5, buf2, slen2)
      IF ( buf1.EQ.buf2 ) THEN
         buf2 = ' '
         slen2 = 1
      ENDIF
      WRITE (lun, '(2a,1x,a)') '! Image zoom     : ', 
     &                          buf1(1:slen1), buf2(1:slen2)

      IF ( ISRNULL(detpixsec) ) THEN
         dd = detpix
         buf1 = '! Back/orig-pix:'
      ELSE
         dd = detpixsec
         buf1 = '! Back/orig-pix/s:'
      ENDIF
      slen1 = LENACT(buf1)
      call xdstr(dd, -1, buf2, slen2)
      WRITE (lun, '(a,1x,a)') buf1(:slen1), buf2(:slen2)

      if ( xlab.eq.'RA' ) then
         WRITE (lun, '(a,i4)') '! Equinox        : ', Equinox
      endif

      slen1 = MAX(LENACT(xlab),LENACT(ylab))
      dd = ra
      call xdstr(dd, -1, buf2, slen2)
      WRITE (lun, '(4a)') '! ', xlab(:slen1), ' Image Center: ', 
     &                                        buf2(:slen2)
      dd = dec
      call xdstr(dd, -1, buf2, slen2)
      WRITE (lun, '(4a)') '! ', ylab(:slen1), ' Image Center: ', 
     &                                        buf2(:slen2)

      WRITE (lun, '(2a)') '! Start Time : ', dateobs(:LENACT(dateobs))
      WRITE (lun, '(2a)') '! End Time   : ', dateend(:LENACT(dateend))

c
c  Set up pointer and first logical for use with get_vign
c
      p_vign = -1
      first = .TRUE.
      ngood = 0
 
      DO 100 i = 1, NUMdet
 
         IF ( HOT(i).EQ.1 ) THEN
            ngood = ngood + 1
 
            xpix = DTSox(i)
            ypix = DTSoy(i)
            Cbufx(ngood) = xpix
            Cbufy(ngood) = ypix
            CALL calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix, 
     &                     xpos, ypos, 2)
c
            dist = 0.
            IF ( cma ) dist = SQRT((xpix-xoff)**2+(ypix-yoff)**2)
 
            IF ( ERR(i).GT.0.0 ) THEN
               snrat = SINt(i)/ERR(i)
            ELSE
               snrat = 0.0
            ENDIF
            IF ( .NOT.cma ) THEN
c
c no sistematic error
               sisterr = 0.
            ELSEIF ( dist.LT.300. ) THEN
c systematic errors
               sisterr = 0.03*SINt(i)
            ELSEIF ( dist.LT.600. ) THEN
               sisterr = 0.06*SINt(i)
            ELSE
               sisterr = 0.1*SINt(i)
            ENDIF
            ERR(i) = SQRT(ERR(i)**2.+sisterr*sisterr)
            IF ( SINt(i).LE.0. ) GOOdflag(i) = 0
c
c assume vignetting is applied if exposure map exists
c
            IF ( vignapp ) THEN
               vcor = 1.
            ELSE
               CALL GET_VIGN(Mapid, xpix, ypix, itel, vcor, first, 
     &                       p_vign, status)
               IF ( status.NE.0 ) THEN
                  vcor = 1.
                  status = 0
               ENDIF
               VCO(i) = vcor
            ENDIF
            IF ( dtime.NE.0 ) THEN
               SINt(i) = SINt(i)*dtime*vcor
               ERR(i) = ERR(i)*dtime*vcor
            ELSE
               SINt(i) = SINt(i)*vcor
               ERR(i) = ERR(i)*vcor
            ENDIF
c
c get half box size in arc sec
c
            hbox = BXHa(i)*abs(pixsize)/zmx
            if ( unit.eq.'deg' ) hbox = hbox*3600.
            Cbufhbox(ngood) = hbox
            psize = nint(hbox)
c
            IF ( vcor.GT.99.0 ) vcor = 99.0
            IF ( exmapid.NE.' ' ) THEN
               CALL GET_EXPOSURE(memr(exmapptr), exszx, exszy, exmapid, 
     &                           xpix, ypix, bxha(i), mapexp)
               IF ( mapexp.LE.0 ) THEN
                  mapexp = exposure
                  CALL XWARN(' Exposure from map is zero', 5)
               ENDIF
               VCO(i) = mapexp
               Cbufvcor(ngood) = VCO(i)
               csec = SINt(i)/mapexp
               cerr = ERR(i)/mapexp
            ELSE
               Cbufvcor(ngood) = vcor
               csec = SINt(i)/exposure
               cerr = ERR(i)/exposure
            ENDIF
            Cbufcnt(ngood) = csec
            Cbuferr(ngood) = cerr
 
            ximg = xpos
            yimg = ypos
            CALL wcsimgsky(wcsid, ximg, yimg, dtra, dtdec, Equinox, 1, 
     &                     status)
            Cbufra(ngood) = dtra
            Cbufdec(ngood) = dtdec
 
            IF ( xlab.EQ.'RA' ) THEN
               CALL cnv_radec(rastr, decstr, dtra, dtdec, dr3, dr3, 2, 
     &                        3, status)
            ELSE
               CALL wcsformat(system, dtra, dtdec, rastr, decstr)
            ENDIF
 
            found = found + 1
 
            IF ( found.EQ.1 ) THEN
               if ( xlab.eq.'RA' ) then
                  write(buf1, '(a,i4,a)') 'RA(', Equinox, ')'
                  write(buf2, '(a,i4,a)') 'Dec(', Equinox, ')'
               else 
                  buf1 = xlab
                  buf2 = ylab
               endif
               IF ( exmapid.NE.' ' ) THEN
                  corstr = 'Exp'
               ELSE
                  corstr = 'Vig'
               ENDIF
               write(ZWRite, '(4a,2x,a,1x,2a)') 
     &          '   #   ', bunit(1:10), ' err         pixel      ', 
     &          corstr(:3),buf1(:10), buf2(:11),' Err  H-Box'
               CALL XWRITE(ZWRite, 10)
               ZWRite(1:1) = '!'
               WRITE (lun, '(a)') ZWRite(:LENACT(ZWRite))

               buf1 = unit
               if ( unit.eq.'deg' ) buf1 = 'sec'
               slen1 = LENACT(buf1)
               if ( slen1.gt.3 ) slen1 = 3
               write(ZWRite, '(4a)') 
     &          '                             x     y     corr  ', 
     &          '                      rad  (', buf1(:slen1), ')'
               CALL XWRITE(ZWRite, 10)
               ZWRite(1:1) = '!'
               WRITE (lun, '(2a)') ZWRite(:LENACT(ZWRite)), 
     &                             '    prob    snr'
            ENDIF
c
            error_rad = ERROR_RADIUS(xpix, ypix, snrat, itel)
            ierror_rad = error_rad
            Cbuferad(ngood) = error_rad
 
            IF ( snrat.GT.99.9 ) snrat = 99.9
            IF ( ERR(i).NE.0 ) THEN
               snr = SINt(i)/ERR(i)
            ELSE
               snr = 0.0
            ENDIF
            Cbufprob(ngood) = PROb(i)
            Cbufsnr(ngood) = snr

            WRITE(xpstr, '(f7.1)') xpix
            if ( xpstr(1:1).eq.'*' ) then
               dd = xpix
               call xdstr(dd, 6, xpstr, slen1)
            endif
 
            WRITE(ypstr, '(f7.1)') ypix
            if ( ypstr(1:1).eq.'*' ) then
               dd = ypix
               call xdstr(dd, 6, ypstr, slen2)
            endif

            IF ( exmapid.NE.' ' ) THEN
               dd = VCO(i)
               call xdstr(dd, 6, corstr, slen1)
            ELSE
               write(corstr, '(f4.2)') vcor
               if ( corstr(1:1).eq.'*' ) then
                  dd = vcor
                  call xdstr(dd, 4, corstr, slen1)
               endif
            ENDIF

            WRITE(buf1, '(i6)') psize
            if ( buf1(1:1).eq.'*' .or. buf1.eq.'     0' ) then
               dd = hbox
               call xdstr(dd, 5, buf1, slen1)
            endif

            WRITE (ZWRite, 99000) found, csec, cerr,
     &          xpstr(:LENACT(xpstr)), ypstr(:LENACT(ypstr)), 
     &          corstr(:LENACT(corstr)), rastr(:10), decstr(:11),
     &          ierror_rad, buf1(:LENACT(buf1))
            CALL XWRITE(ZWRite, 10)

            dd = xpix
            call xdstr(dd, -1, xpstr, slen1)
            dd = ypix
            call xdstr(dd, -1, ypstr, slen2)

            WRITE (lun, 99100) found, csec, cerr,
     &          xpstr(:LENACT(xpstr)), ypstr(:LENACT(ypstr)), 
     &          corstr(:LENACT(corstr)), rastr(:LENACT(rastr)), 
     &          decstr(:LENACT(decstr)), ierror_rad, 
     &          buf1(:LENACT(buf1)), PROb(i), snr
         ENDIF

 100  CONTINUE
c
c  Free get_vign pointer
c
      CALL ralloc(0, -1, -1, p_vign, status)
 
      CLOSE (lun)
      CALL FRELUN(lun)
      IF ( found.EQ.0 ) THEN
         CALL XWRITE('No source detected', 10)
         RETURN
      ENDIF
c
c  Write out column buffers to FITS file
c
      DATA ttype/'SRCRATE', 'SRCRATE_ERR', 'X', 'Y', 'VIGNET', 'RA',
     &     'DEC', 'ERRRAD', 'HBOXSIZE', 'PROB', 'SNR'/
      DATA tform/'1E', '1E', '1E', '1E', '1E', '1D', '1D', '1E', '1E', 
     &     '1E', '1E'/
      DATA tunit/' ', ' ', 'pixel', 'pixel', ' ', 'deg', 
     &     'deg', 'arcsec', 'arcsec', ' ', ' '/
c
c  Adjust VIGNET column name in exposure map case
c
      if ( exmapid.ne.' ' ) then
         ttype(5) = 'EXPOSURE'
         tunit(5) = 's'
      endif
c
c   Fixup column names and units if not RA/Dec system
c
      tunit(1) = bunit(1:8)
      tunit(2) = bunit(1:8)
      if ( xlab.ne.'RA' ) then
         if ( xlab.eq.'X' ) then
            ttype(6) = 'X2'
            ttype(7) = 'Y2'
         else
            ttype(6) = xlab(1:12)
            ttype(7) = ylab(1:12)
         endif
         tunit(6) = unit(1:8)
         tunit(7) = unit(1:8)
         if ( unit.ne.'deg' ) then
            tunit(8) = unit(1:8)
            tunit(9) = unit(1:8)
         endif
      endif
 
      IF ( Fitsdet.NE.' ' ) THEN
 
         WRITE (ZWRite, '(2a)') ' Writing FITS source list: ', 
     &                          Fitsdet(:LENACT(Fitsdet))
         CALL xwrite(ZWRite, 10)
 
         if ( ctype1(1:2) .eq. 'RA' ) then
            template = 'detect'
         else
            template = 'detnosky'
         endif
         CALL lkupfile(template, 'hdr', 'header template', status)
         IF ( status.NE.0 ) RETURN
 
c     Create file
         CALL getlun(lun)
         block = 1
         CALL ftinit(lun, Fitsdet, block, status)
         IF ( status.NE.0 ) THEN
            INQUIRE (FILE=Fitsdet, EXIST=there)
            IF ( there ) THEN
               ZWRite = ' File exists: '//Fitsdet(:LENACT(Fitsdet))
               CALL xwrite(ZWRite, 5)
               CALL xwrite(' Remove existing file or use different name'
     &                     , 10)
            ELSE
               CALL ftgerr(status, ds)
               ZWRite = ' ERROR: '//ds(:LENACT(ds))
               CALL xwrite(ZWRite, 5)
            ENDIF
            RETURN
         ENDIF
 
c     Write long string message to primary
c        call ftplsw(lun, status)
 
c     Write binary table
         CALL ftibin(lun, ngood, tfields, ttype, tform, tunit, 
     &               'SRCLIST', 0, status)
         CALL ftpcle(lun, 1, 1, 1, ngood, Cbufcnt, status)
         CALL ftpcle(lun, 2, 1, 1, ngood, Cbuferr, status)
         CALL ftpcle(lun, 3, 1, 1, ngood, Cbufx, status)
         CALL ftpcle(lun, 4, 1, 1, ngood, Cbufy, status)
         CALL ftpcle(lun, 5, 1, 1, ngood, Cbufvcor, status)
         CALL ftpcld(lun, 6, 1, 1, ngood, Cbufra, status)
         CALL ftpcld(lun, 7, 1, 1, ngood, Cbufdec, status)
         CALL ftpcne(lun, 8, 1, 1, ngood, Cbuferad, -1.0, status)
         CALL ftpcle(lun, 9, 1, 1, ngood, Cbufhbox, status)
         CALL ftpcle(lun, 10, 1, 1, ngood, Cbufprob, status)
         CALL ftpcle(lun, 11, 1, 1, ngood, Cbufsnr, status)
 
c     Write header keywords
         CALL wr_fithdr(lun, Mapid, template)
         CALL ftpdat(lun, status)
 
         CALL ftclos(lun, status)
         CALL frelun(lun)
      ENDIF
 
      RETURN

99000 FORMAT (1X, I4, 1X, 1PE8.2, '+/-', E7.1, 1X, A, 1X, A, 
     &        1X, A, 1X, A, 1X, A, 1X, i3, 1X, A)
99100 FORMAT (1X, I4, 1X, 1PE8.2, '+/-', E7.1, 1X, A, 1X, A, 
     &        1X, A, 1X, A, 1X, A, 1X, i3, 1X, A, 1X, 1pe9.3, 
     &        1X, 1pe9.3)

      END
