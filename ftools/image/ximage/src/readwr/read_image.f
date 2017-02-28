      SUBROUTINE READ_IMAGE(Cmdid, Mapid, Imgcnt, Filename, 
     &                      Rdmapid, Status)
      IMPLICIT NONE
c
c  Read image interface
c
c  I  Cmdid       (s)  Command id
c  I  Mapid       (s)  Map id string (current)
c  O  Imgcnt      (i)  Image counter
c  O  Filename    (s)  Location of read file
c  O  Rdmapid     (s)  Map id string (that was read in)
c  O  Status      (i)  Error flag (0 = OK) 
c
      INTEGER*4 Cmdid, Imgcnt, Status 
      character*(*) Mapid, Filename, Rdmapid
c
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
 
      INTEGER*4 ieof, argc
      integer itel

      integer szx, szy, tmpszx, tmpszy
      real*8 rebin, begzm(2), usrzm(2), totzm(2)
      integer imgnum, extnum
c
      INTEGER*4 LENACT
c
      integer*4 equinox, radec_equinox
      logical global, readonly
c
      real*8 tmin , tmax
      real*4 dumra(3), dumdec(3)
c
      character(30) string_ra , string_dec, sizestr, szxstr, szystr
      character(8) filetype, maptype
      character(8) type
      character*(MAX_FILELEN) filetmp, gtiext, gtifile, regionfile
      character(200) expr
      character(10) gtimode
c
      LOGICAL exosat, fits, rosata3d
      LOGICAL detros, unknown, exp_map, vignapp
      LOGICAL there, deqc, regcenoff
      logical time, color, isdnull

      INTEGER*4 ierr, rec1, nbyte, numreg, i
      real*8 xmin, xmax, ymin, ymax
      real*8 txmin, txmax, tymin, tymax

      integer gtinum, p_gtistart, p_gtistop
      real*8 tontime

      integer*4 emin, emax

      integer*4 lun
      character(30) xcol, ycol, zcol, ecol, tcol
      integer*4 maxdefsz, nrows, xi, yi, zi, ei, ti

      integer*4 rwmode, block, hdutype
      character(80) ctype(2), cunit(2)
      character(80) object, begobs, endobs
      character(80) telescop, instrume, detnam, filter, unit
      real*8 ximscale, ximzero, exposure, dtcor
      integer*4 equimg

      real*8 crval(2), crpix(2), cdelt(2), crota2, dnull
      real*8 uspix(2), usval(2), cenpix(2), cendpix(2)
      integer*4 naxes(3)

      real*8 imgref(2), detref(2)

      integer di1, di2
      character*(MAX_IDSTR) exmapid, setid, wcsid
      integer*4 mapptr, p_work, p_twork, p_i2map
      integer*4 wszx, wszy, tszx, tszy
      integer*4 wstat, tstat

      real*4 avgfact, rmin, rmax
      real*8 datamin, datamax
      character(30) str1, str2, errmsg
      integer slen1, slen2

      logical isloaded
      real*8 dd

      Filename = ' '
      exosat = .FALSE.
      rosata3d = .FALSE.
      detros = .FALSE.
      unknown = .FALSE.
      exp_map = .FALSE.
      fits = .FALSE.
      color = .FALSE.
      time = .FALSE.
      gtiext = ' '
      gtifile = ' '
      gtimode = ' '
      ximscale = 1.0
      ximzero = 0.0
      uspix(1) = dnull()
      uspix(2) = dnull()
      rebin = 0.d0
      sizestr = ' '
      szxstr = ' '
      szystr = ' '
      tmpszx = 0
      tmpszy = 0
      imgnum = 1
      tmin = 0.
      tmax = 0.
      usval(1) = -10.
      usval(2) = 100.
      filetype = ' '
      type = ' '
      xcol = ' '
      ycol = ' '
      zcol = ' '
      string_ra = ' '
      string_dec = ' '
      regionfile = ' '
      numreg = 0
      expr = ' '
      radec_equinox = -10
      ierr = 0

      ecol = ' '
      emin = 0
      emax = 0
      tcol = ' '
      Rdmapid = Mapid
      regcenoff = .FALSE.
      vignapp = .FALSE.

      txmin = dnull()
      txmax = dnull()
      tymin = dnull()
      tymax = dnull()
c
c  Tcl variable read_image_regcenoff can be used to turn off 
c  region centering/resizing behavior
c
      Status = 0

      call tclresl("if [info exists read_image_regcenoff] "//
     &             "{set read_image_regcenoff} else {set result 0}", 
     &              regcenoff, status)

      call tclresi("if [info exists read_image_limitsize] "//
     &             "{set read_image_limitsize} else {set result -1}", 
     &              maxdefsz, status)
      if ( maxdefsz.lt.0 ) maxdefsz = 1024

      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'MAPID',Rdmapid,Status)
      CALL GPARL(Cmdid,'EXOSAT',exosat,Status)
      CALL GPARL(Cmdid,'RFITS',rosata3d,Status)
      CALL GPARL(Cmdid,'FITS',fits,Status)
      CALL GPARL(Cmdid,'UNKNOWN',unknown,Status)
      CALL GPARL(Cmdid,'COLOR_IMAGE',color,status)
      CALL GPARL(Cmdid,'TIME_IMAGE',time,status)
      CALL GPARL(Cmdid,'EXPOSURE_MAP',exp_map,Status)
      CALL GPARS(Cmdid,'FILE',Filename,Status)
      CALL GPARS(Cmdid,'GTIEXT',gtiext,Status)
      CALL GPARS(Cmdid,'GTIFILE',gtifile,Status)
      CALL GPARS(Cmdid,'GTIMODE',gtimode,Status)
      CALL GPARS(Cmdid,'REGIONFILE',regionfile,Status)
      CALL GPARS(Cmdid,'EXPR',expr,Status)
      CALL GPARD(Cmdid,'XPIX',uspix(1),Status)
      CALL GPARD(Cmdid,'YPIX',uspix(2),Status)
      CALL GPARD(Cmdid,'REBIN',rebin,Status)
      CALL GPARS(Cmdid,'SIZE',sizestr,Status)
      CALL GPARS(Cmdid,'SZX',szxstr,Status)
      CALL GPARS(Cmdid,'SZY',szystr,Status)
      CALL GPARD(Cmdid,'TMIN',tmin,Status)
      CALL GPARD(Cmdid,'TMAX',tmax,Status)
      CALL GPARI(Cmdid,'EMIN',emin,Status)
      CALL GPARI(Cmdid,'EMAX',emax,Status)
      CALL GPARL(Cmdid,'DETECTOR',detros,Status)
      CALL GPARS(Cmdid,'XCOL',xcol,Status)
      CALL GPARS(Cmdid,'YCOL',ycol,Status)
      CALL GPARS(Cmdid,'ZCOL',zcol,Status)
      CALL GPARS(Cmdid,'ECOL',ecol,Status)
      CALL GPARS(Cmdid,'TCOL',tcol,Status)
      CALL GPARS(Cmdid,'RA',string_ra,Status)
      CALL GPARS(Cmdid,'DEC',string_dec,Status)
      CALL GPARI(Cmdid,'EQUINOX',radec_equinox,Status)
      CALL GPARI(Cmdid,'IMAGE_NUMBER',imgnum,Status)
      CALL GPARD(Cmdid,'BSCALE',ximscale,Status)
      CALL GPARD(Cmdid,'BZERO',ximzero,Status)
      if ( status.ne.0 ) return
c
      setid = Rdmapid
      exmapid = ' '
      wcsid = ' '
c
c  Strip quotes from string qualifiers that may contain spaces
c
      call qustrip(Filename)
      call qustrip(regionfile)
      call qustrip(gtiext)
      call qustrip(gtifile)
      call qustrip(expr)
c
c  Help messages
c
      if ( gtimode.eq.'?' ) then
         call XWRITE(' GTIMODE options: AND, OR, SUB, NONE', 10)
         return
      endif
c
c Determine filename
c
      IF ( Filename.EQ.' ' ) THEN
         DO WHILE ( .TRUE. )
            CALL XCREAD(' Enter file name: ',Filename,ieof)
            IF ( Filename.EQ.' ' ) THEN
               CALL XWRITE(' Please type a filename or EXIT',10)
               GOTO 200
            ENDIF
            IF ( ieof.NE.0 .OR. Filename.EQ.'EXIT' .OR. 
     &           Filename.EQ.'QUIT' .OR. Filename.EQ.'HELP' .OR. 
     &           Filename.EQ.'exit' .OR. Filename.EQ.'quit' ) THEN
               Status = 1
               RETURN
            ENDIF
            GOTO 300
 200     ENDDO
      ENDIF
c     Adds extension number and removes spaces
 300  call xrstrext(Filename,filetmp,extnum,ierr)
      Filename = filetmp
c
c  Region file
c
      if ( regionfile.NE.' ' ) then
         if ( regionfile(1:1).eq.'@' ) then
            inquire(FILE=regionfile(2:), EXIST=there)
         else
            inquire(FILE=regionfile, EXIST=there)
         endif
         if ( .not.there ) then
            call XTEND(regionfile,'reg')
            inquire(FILE=regionfile, EXIST=there)
         endif
         if ( .not.there ) then
            WRITE (ZWRite,99001) regionfile(:LENACT(regionfile))
            call xwrite(ZWRite, 10)
            regionfile = ' '
         else
            WRITE (ZWRite,99002) regionfile(:LENACT(regionfile))
            CALL XWRITE(ZWRite,10)
         endif
      endif
c
c  Ra/Dec of image center
c

c   get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      IF ( string_ra.NE.' ' .and. string_dec.ne.' ' ) THEN
         call cnv_radec(string_ra, string_dec, usval(1), usval(2), 
     &                  dumra, dumdec, 1, 0, status)
         IF ( Status.NE.0 ) RETURN
         WRITE (ZWRite,*) ' Input Ra and Dec of' , usval(1) , usval(2)
         call XWRITE(ZWRite,10)
c
c     if no equinox provided, use XIMAGE equinox
c     otherwise, change XIMAGE equinox to entered value
c
         if ( radec_equinox.eq.-10 ) then
            write(ZWRite,'(a,i4,a)') ' Equinox of ',equinox,' assumed'
            CALL xwarn(ZWRite,10)
         elseif ( radec_equinox.ne.equinox ) then
            write(ZWRite,'(a,i4)') 'Changing XIMAGE equinox to ',
     &                             radec_equinox
            call XWRITE(ZWRite,10)
            equinox = radec_equinox
            readonly = .FALSE.
            global = .TRUE.
            call tclvari('default(equinox)', equinox, readonly, global,
     &                   status)
         endif
      ENDIF
c
c Determine file type
c
      IF ( .NOT.exosat .AND. .NOT.fits .AND. .NOT.rosata3d ) THEN
         fits = .TRUE.
         CALL XWRITE(' Assuming its a FITS file...',15)
      ENDIF

      if ( fits ) then
         call gt_fitstype(Filename,filetype,extnum,Status)
         if ( Status.ne.0 ) return
      elseif ( exosat ) then
         filetype = 'EXO'
      elseif ( rosata3d ) then
         filetype = 'EVT'
      endif

c
c Qualifier warnings
c
      if ( filetype.ne.'EVT' ) then
         Status = 1
         IF ( (Emin.NE.0.OR.Emax.NE.0.OR.Ecol.ne.' ') ) THEN
            CALL XERROR(' EMIN, EMAX, ECOL only for EVENT fits files',
     &                  5)
            RETURN
         ELSEIF ( (tmin.NE.0.OR.tmax.NE.0 ) ) THEN
            CALL XERROR(' TMIN or TMAX only for EVENT fits files',5)
            RETURN
         ELSEIF ( Detros ) THEN
            CALL XERROR(
     &            ' DETECTOR is only used with EVENT fits files',5)
            RETURN
         elseif ( Xcol.ne.' ' .or. Ycol.ne.' ' .or. Zcol.ne.' ') then
            call XERROR(
     &         ' XCOL,YCOL,ZCOL only used with EVENT fits files',5)
            return
         elseif ( Gtifile.ne.' '.OR.Gtimode.ne.' ' ) then
            call XERROR(' GTIFILE or GTIMODE only for EVENT fits files',
     &                  5)
            return
         elseif ( regionfile.ne.' ' ) then
            call XERROR(' REGIONFILE only for EVENT fits files',5)
            return
         elseif ( expr.ne.' ' ) then
            call XERROR(' Column filtering EXPRession only for EVENT'//
     &                  ' fits files',5)
            return
         elseif ( time.OR.color) then
            call XERROR(' TIME_IMAGE and COLOR_IMAGE only for EVENT'//
     &                  ' fits files',5)
            return
         ENDIF
         Status = 0
      else
c
c  Starting the qualifiers checking and default setting for events
c  Determine image type valid only for event file
         if ( time ) then
            type = 'TIME'
         elseif ( color ) then
            type = 'COLOR'
         else if ( zcol.ne.' ') then
            type = 'ZCOL'
            call UPC(zcol)
         else
            type = 'EVT'
         endif
c
c  Determine gtimode and gtifile
c
         call upc(gtimode)
         if ( gtimode.eq.'AND' .or. gtimode.eq.'OR' .or.
     &        gtimode.eq.'SUB' ) then
            if ( gtifile.eq.' ' ) then
               call XWARN(' GTIMODE requires GTIFILE to be set',5)
               gtimode = ' '
            endif
         elseif ( gtimode.eq.'NONE' ) then
            if ( gtifile.ne.' ' ) then
               call XWARN(' GTIMODE of NONE ignores GTIFILE value',5)
               gtifile = ' '
            endif
         elseif ( gtimode.eq.' ' ) then
            if ( gtifile.ne.' ' ) then
               gtimode = 'SUB'
               call XWRITE(' Assuming GTIMODE = SUB', 10)
            endif
         else
            call XERROR(' Invalid GTIMODE: Use AND, OR, SUB, NONE', 5)
            Status = -1
            return
         endif
c
c Determine columns for image coordinates
c
         if ( tcol.eq.' ' ) tcol = 'TIME'
         if ( detros ) then
            xcol = 'D*X'
            ycol = 'D*Y'
         endif

      endif

      if ( filetype.ne.'IMG' ) then
         IF ( imgnum.NE.1 ) THEN
            CALL XERROR(' IMAGE_NUMBER is only for primary fits files',
     &                  5)
            RETURN
         ELSEIF ( exp_map ) THEN
            CALL XERROR(' Exposure maps are only for image files',5)
            RETURN
         ENDIF
      else
         if ( exp_map ) then

            type = 'EXP'
            call gheads(Rdmapid, 'EXMAPID', exmapid, 0, status)
            if ( exmapid.ne.' ' ) call mapfree(exmapid, status)
            call getexmap(Rdmapid, exmapid, status)
            setid = exmapid

c         Assume vignetting has been applied to exposure map by default
            vignapp = .TRUE.

            if ( status.ne.0 ) return
         else
            type = 'IMG'
         endif
      endif
c
c  Image size
c
      if ( sizestr.ne.' ' ) then
         call strnum(sizestr, 8, dd, status)
         if ( status.ne.0 ) then
            call xwrite(' Failed to parse size, using default', 10)
         else
            tmpszx = nint(dd)
            tmpszy = nint(dd)
c        ** Catch cases where -1 < dd < 0
c        ** szx/ystr gets parsed again later for -ve size
            if ( dd.lt.0. .and. tmpszx.ge.0 ) then
               tmpszx = -1
               tmpszy = -1
            endif
            szxstr = sizestr
            szystr = sizestr
         endif
      else
         if ( szxstr.ne.' ' .and. szystr.eq.' ' ) szystr = szxstr
         if ( szxstr.eq.' ' .and. szystr.ne.' ' ) szxstr = szystr
         if ( szxstr.ne.' ' .and. szystr.ne.' ' ) then
            call strnum(szxstr, 8, dd, status)
            tmpszx = nint(dd)
            if ( dd.lt.0. .and. tmpszx.ge.0 ) tmpszx = -1
            call strnum(szystr, 8, dd, status)
            tmpszy = nint(dd)
            if ( dd.lt.0. .and. tmpszy.ge.0 ) tmpszy = -1
         endif
      endif

      if ( filetype.eq.'IMG' .and. 
     &     rebin - dble(int(rebin)) .ne. 0.d0 ) then
         call xwarn(' Rebin must be positive integer for FITS image', 
     &               10)
         call xwrite(' Use remap command to rebin image by real value ', 
     &               10)
         rebin = int(rebin)
      endif

      usrzm(1) = rebin
      usrzm(2) = rebin
c
c exosat image reader
c NOTE for exosat images is not possible to set size or zoom
c the entire array is read with the size given in the header  
c
      IF ( filetype.eq.'EXO' ) THEN

         CALL XWRITE(' Reading an image in EXOSAT format',10)

         call rd_exosz(Filename, tmpszx, tmpszy, rec1, nbyte, Status)
         if ( status.ne.0 ) return

         szx = tmpszx
         szy = tmpszy

         if ( exp_map ) then

            if ( .not.isloaded(Rdmapid) ) then
               call XWRITE(' Must load image before exposure map', 10)
               goto 400
            endif
            di1 = 0
            di2 = 0
            call gheadi(Rdmapid, 'SZX', di1, 0, Status)
            call gheadi(Rdmapid, 'SZY', di2, 0, Status)
            if ( di1.ne.szx .or. di2.ne.szy ) then
               call XWRITE(
     &              ' Exposure map must be same size as image', 10)
               Status = -1
               goto 400
            endif
            call mapalloc(szx, szy, exmapid, mapptr, status)
            call gheads(Rdmapid,'EXMAPID', exmapid, 1, status)
            call gheadi(exmapid,'EXPMAP', 1, 1, status)

         else

            call mapalloc(szx, szy, Rdmapid, mapptr, Status)
            if ( Status.ne.0 ) goto 400

         endif

         call i2alloc(1, szx, szy, p_i2map, Status)
         if ( Status.ne.0 ) goto 400

         call rd_exohdr(Filename,setid,nbyte,lun,status)
         call rd_exoimg(lun,rec1,memr(mapptr),mems(p_i2map),szx,szy,
     &                  status)
         if ( Status.eq.0 ) call gheadi(Rdmapid, 'LOADED', 1, 1, status)

         call i2alloc(0, Szx, Szy, p_i2map, Status)
c
c  Construct wcsid frameset from internal header
c
         call genhdrwcs(setid, MAX_IDSTR, wcsid, status)

         deqc = .FALSE.
         call gheadd(setid, 'CRPIX1', imgref(1), 0, Status)
         call gheadd(setid, 'CRPIX2', imgref(2), 0, Status)
         call gheadd(setid, 'DRPIX1', detref(1), 0, Status)
         call gheadd(setid, 'DRPIX2', detref(2), 0, Status)
         call gheadd(setid, 'ZMX', begzm(1), 0, Status)
         call gheadd(setid, 'ZMY', begzm(2), 0, Status)
         call wcssetdet(wcsid, deqc, imgref, detref, begzm, status)
         call gheads(setid, 'WCSID', wcsid, 1, Status)

 400     continue
         close(lun)
         call FRELUN(lun)

c
c rosat photon file reader
c  aka fits event reader
c
      ELSEIF ( filetype.eq.'EVT' .or. filetype.eq.'IMG' ) THEN
c
c  Open FITS file
c
         rwmode = 0
         block = 1
         CALL GETLUN(lun)
         CALL FTOPEN(lun,Filename,rwmode,block,Status)
         IF ( Status.NE.0 ) THEN
            CALL XERROR(' Failed to open fits file',10)
            call ftgerr(Status, errmsg)
            call upc(errmsg(1:1))
            write(ZWRite,'(2a)') ' ', errmsg
            call xwrite(ZWRite, 15)
            call FTCLOS(lun, Status)
            call FRELUN(lun)
            RETURN
         ENDIF
c
c  Determine mission
c
         call rd_detkeys(lun, unknown, telescop, instrume, detnam,
     &                   filter, itel, Status)
         Status = 0
c
c  Move to appropriate extension
c
         if ( filetype.eq.'EVT' ) then

            call gt_missinfo (itel, xcol, ycol, ecol, emin, emax,
     &                        tmpszx, tmpszy, usrzm, gtiext, Status)
c
c          Find the events table
c
            call go_evttbl(Lun, extnum, nrows, Status)
            if ( Status.eq.0 ) then
               call XWRITE(' got events table ',15)
            else
               call XWARN(' Failed to get event table',10)
            endif
c 
c          Get column indices
c
            call getcol(lun, xcol, xi, Status)
            call getcol(lun, ycol, yi, Status)
c
c          If ZCOL specified, then it's required
c
            if ( Status.eq.0 ) then
               call getcol(lun, zcol, zi, Status)
               if ( zi.le.0 .and. zcol.ne.' ' ) then
                  call xwrite(' Specified ZCOL not found', 10)
                  Status = -1
               else
                  Status = 0
               endif
            endif
c
c          Energy column may or may not be necessary depending on 
c           whether emin/max constraint is applied
c
            if ( Status.eq.0 ) then
               call getcol(lun, ecol, ei, Status)
               call rd_elimits(lun, ei, emin, emax, Status)
               if ( ei.le.0 .and. emin+emax.ne.0 ) then
                  call xwrite(' Energy column for min/max energy '//
     &                        'constraint not found', 10)
                  Status = -1
               endif
               if ( ei.le.0 .and. color ) then
                  call xwrite(' Energy column for color image '//
     &                        'not found', 10)
                  Status = -1
               endif
            endif
c
c          Time column may or may not be necessary depending on whether
c           the GTIs are being used or a timing image is created.
c
            if ( Status.eq.0 ) then
               call getcol(lun, tcol, ti, Status)
               Status = 0
               if ( ti.le.0 ) then
                  if ( time ) then
                     call xwrite(
     &                   ' Time column for timing image not found', 10)
                     Status = -1
                  else
                     call xwrite(' Lack of time column not fatal'//
     &                           ' unless GTIs are present', 10)
                  endif
               endif
            endif

         elseif ( filetype.eq.'IMG' ) then

            call FTMAHD(Lun, extnum+1, hdutype, Status)

         endif
         if ( Status.ne.0 ) goto 500
c
c  Look up common keywords
c
         call rd_eqxkey (lun, equinox, equimg, Status)
         Status = 0
         call rd_objkey(lun, object, Status)
         Status = 0
         call rd_obskeys (lun, begobs, endobs, Status)
         Status = 0
         call rd_expkeys(lun, exposure, dtcor, vignapp, Status)
         Status = 0
c
c  Read image size
c
         call rd_imgsz(Lun, filetype, imgnum, xi, yi, naxes, cenpix,
     &                 Status)
         if ( Status.ne.0 ) goto 500
c
c  Read in coordinate system
c
         call rd_ckeys (lun, xi, yi, cenpix, ctype, cunit, crval, crpix,
     &                  cdelt, crota2)
c
c Determine initial wcs coordinate frame before rebin/recenter
c
         if ( filetype.eq.'IMG' ) then
            call genimgwcs(Filename, extnum, MAX_IDSTR, wcsid, Status)
         endif
         if ( filetype.ne.'IMG' .or. Status.ne.0 ) then
            if ( Status.ne.0 ) then
               call xwrite(' Original image WCS is invalid, generating '
     &                     //'failsafe values', 10)
            endif
            call genckeywcs(naxes, ctype(1), ctype(2), cunit(1),
     &                      cunit(2), crval, crpix, cdelt, crota2,
     &                      equimg, MAX_IDSTR, wcsid, Status)
         endif
         if ( Status.ne.0 ) goto 500
c
c  Determine final image size
c
         call gt_imgsz (naxes, usrzm, ctype, cdelt, szxstr,
     &                  szystr, maxdefsz, tmpszx, tmpszy)

         if ( filetype.eq.'EVT' .or. unknown ) then
            deqc = .TRUE.
         else
            deqc = .FALSE.
         endif

         call rd_detcoor (lun, deqc, xi, yi, itel, naxes, cdelt,
     &                    imgref, detref, begzm)
c
c Setup detector coordinate frame
c
         call wcssetdet(wcsid, deqc, imgref, detref, begzm, status)
         if ( status.ne.0 ) goto 500
c
c  Determine final image center
c
c        Use reference pixel if image size unknown and no user entry
c
         if ( naxes(1).eq.0 .or. naxes(2).eq.0 ) then
            cenpix(1) = imgref(1)
            cenpix(2) = imgref(2)
         endif
c
c  Load region(s) into memory
c
         if ( regionfile.ne.' ' ) then
            call setregwcs(wcsid, status)
            call xinitreg(regionfile, numreg, status)
            if ( status.ne.0 ) then
               call XWRITE ('Failed to read region:', 10) 
               call XWRITE (regionfile, 10) 
               regionfile = ' '
               numreg = 0
               Status = 0
            elseif (( isdnull(uspix(1)) .or. isdnull(uspix(2)) )
     &              .and. .not.regcenoff ) then
               do i = 1, numreg
                  call bboxreg(i, xmin, xmax, ymin, ymax, status)
                  if ( status .ne. 0 ) then
                     call XWRITE('Failed to get region bounds', 10)
                     goto 500 
                  endif
c
c                 Do not center and resize if unbounded region
c
                  if ( .not.isdnull(xmin) .and. .not.isdnull(xmax) .and.
     &                .not.isdnull(ymin) .and. .not.isdnull(ymax) ) then
                     if ( isdnull(txmin) .or. xmin.lt.txmin ) 
     &                 txmin = xmin
                     if ( isdnull(txmax) .or. xmax.gt.txmax )
     &                 txmax = xmax
                     if ( isdnull(tymin) .or. ymin.gt.tymin )
     &                 tymin = ymin
                     if ( isdnull(tymax) .or. ymax.gt.tymax )
     &                 tymax = ymax
                  endif
               enddo
               if ( .not.isdnull(txmin) .and. .not.isdnull(txmax) .and.
     &              .not.isdnull(tymin) .and. .not.isdnull(tymax) ) then
                  call xwrite(' Centering and sizing on region...', 10)
                  dd = (txmax-txmin)/MAX(1.d0,usrzm(1))+2.d0
                  if ( szxstr.eq.' ' .and. szystr.eq.' ' ) then
                     if ( maxdefsz.eq.0 ) then
                        tmpszx = MAX(
     &                       NINT((txmax-txmin)/MAX(1.d0,usrzm(1)))+2,
     &                       NINT((tymax-tymin)/MAX(1.d0,usrzm(2)))+2)
                     else 
                        tmpszx = MAX(
     &                    MIN(maxdefsz,
     &                       NINT((txmax-txmin)/MAX(1.d0,usrzm(1)))+2),
     &                    MIN(maxdefsz,
     &                       NINT((tymax-tymin)/MAX(1.d0,usrzm(2)))+2))
                     endif
                     tmpszy = tmpszx
                  endif
                  uspix(1) = (txmin + txmax)/2.d0
                  uspix(2) = (tymin + tymax)/2.d0
               endif
            endif
         endif
c
c        Convert user-entered center
c
         if ( usval(1).ge.0. ) then
            call wcsimgsky(wcsid, cenpix(1), cenpix(2), usval(1),
     &                     usval(2), equinox, 0, status)
         else if ( .not.isdnull(uspix(1)) .and. 
     &             .not.isdnull(uspix(2)) ) then
            call wcsimgpix(wcsid, cenpix(1), cenpix(2), uspix(1),
     &                     uspix(2), 0, status)
         endif
c
c  Force to a pixel boundary
c
         cenpix(1) = int(cenpix(1)) + 0.5
         cenpix(2) = int(cenpix(2)) + 0.5

         call wcsimgpix(wcsid, cenpix(1), cenpix(2), cendpix(1),
     &                  cendpix(2), 1, status)
c
c  Remap WCS frames to account for rebin/recenter 
c
         call wcslintrf(wcsid, cenpix, tmpszx, tmpszy, usrzm, status)
         if ( status.ne.0 ) goto 500
c
c  Determine final rebin
c
         totzm(1) = abs(usrzm(1)*begzm(1))
         totzm(2) = abs(usrzm(2)*begzm(2))
         if ( totzm(1).le.0.d0 .or. totzm(2).le.0.d0 ) then
            call xwarn(' Bad image rebin, defaulting to 1', 10)
            totzm(1) = 1.d0
            totzm(2) = 1.d0
         endif
c
c  Write out image info
c
         write(ZWRite,*) ' Image size = ',tmpszx, ' x ', tmpszy, 
     &                   ' pixels'
         call RMVXBK(ZWRite(2:))
         call xwrite(ZWRite, 10)
         call xdstr(totzm(1), 4, str1, slen1)
         if ( totzm(1).ne.totzm(2) ) then
            call xdstr(totzm(2), 4, str2, slen2)
            write(ZWRite,'(4a)') ' Image rebin = ', str1(1:slen1),
     &                           ' x ', str2(1:slen2)
         else
            write(ZWRite,'(2a)') ' Image rebin = ', str1(1:slen1)
         endif
         call RMVXBK(ZWRite(2:))
         call xwrite(ZWRite, 10)
         write(ZWRite,'(a,f10.1,a,f10.1)') ' Image center = ', 
     &                 cendpix(1), ', ', cendpix(2)
         call RMVXBK(ZWRite(2:))
         call xwrite(ZWRite, 10)
         if ( ecol.ne.' ' ) then
            write(ZWRite, *) ' Energy column = ', 
     &                       ecol(:LENACT(ecol)), ' Min = ', emin,
     &                       ' Max = ', emax
            call RMVXBK(ZWRite(2:))
            call xwrite(ZWRite, 10)
         endif
c
c  Allocate maps
c
         szx = tmpszx
         szy = tmpszy

         if ( type.eq.'EXP' ) then

            if ( .not.isloaded(Rdmapid) ) then
               call XWRITE(' Must load image before exposure map', 10)
               Status = -1
               return
            endif
            di1 = 0
            di2 = 0
            call gheadi(Rdmapid, 'SZX', di1, 0, Status)
            call gheadi(Rdmapid, 'SZY', di2, 0, Status)
            if ( di1.ne.szx .or. di2.ne.szy ) then
               call XWRITE(
     &              ' Exposure map must be same size as image', 10)
               Status = -1
               return
            endif
            call mapalloc(szx, szy, exmapid, mapptr, status)
            call gheads(Rdmapid,'EXMAPID', exmapid, 1, status)
            call gheadi(exmapid,'EXPMAP', 1, 1, status)

         else

            call mapalloc(szx, szy, Rdmapid, mapptr, status)
            if ( Status.ne.0 ) goto 500

         endif

         if ( filetype.eq.'EVT' ) then
c
c    Determine composite GTIs
c
            call gt_gtis (lun, gtiext, gtifile, gtimode, tmin, tmax, 
     &                    gtinum, p_gtistart, p_gtistop, tontime,
     &                    Status)
            if ( ti.le.0 .and. gtinum.gt.0 ) then
               call xwrite(' Time column required for GTI usage', 10)
               status = -1
            endif
            if ( Status.ne.0 ) goto 500

            if ( tontime.GT.0 ) then
               exposure = tontime
               call xdstr(exposure, 12, str1, slen1)
               write(ZWRite,'(3a)') ' Using gti for exposure ',
     &                              str1(1:slen1), ' s'
               call RMVXBK(ZWRite(2:))
               call XWRITE(ZWRite,10)
            endif
c
c     Warn if no exposure 
c
            if ( exposure.le.0. ) then
               call XWRITE(' Exposure not defined. Use chheader to set'
     &                   //' EXPOSURE keyword, if necessary.', 10)
            endif
c
c     Accumulate image
c
c  Minimize memory requirements - only allocate (large) work maps
c   when needed
c
            if ( type.eq.'TIME' ) then
               wszx = Szx
               wszy = Szy
               tszx = Szx
               tszy = Szy
               maptype = 'R'
            elseif ( type.eq.'COLOR' ) then
               wszx = Szx
               wszy = Szy
               tszx = 1
               tszy = 1
               maptype = 'R'
            else
               wszx = 1
               wszy = 1
               tszx = 1
               tszy = 1
               maptype = 'I'
            endif

            call ralloc(1, wszx, wszy, p_work, wstat)
            call ralloc(1, tszx, tszy, p_twork, tstat)
c           Only good status (i.e. zero) if both are zero
            Status = wstat + tstat

            call rd_evt_xim(lun,type,memr(mapptr),Szx,Szy,memr(p_work),
     &                      wszx,wszy,memr(p_twork),tszx,tszy,
     &                      usrzm,xi,yi,zi,ei,ti,emin,emax,
     &                      gtinum,memd(p_gtistart),memd(p_gtistop),
     &                      cenpix,numreg,expr,exposure,ximscale,
     &                      ximzero,unit,datamin,datamax,Status)
            if ( Status.ne.0 ) goto 500
c
c     Deallocate temporary arrays
c
            call gtialloc(0, gtinum, p_gtistart, p_gtistop, Status)
            call ralloc(0, wszx, wszy, p_work, Status)
            call ralloc(0, tszx, tszy, p_twork, Status)

         elseif ( filetype.eq.'IMG' ) then

            call rd_img_xim(lun,imgnum,type,memr(mapptr),
     &                      szx,szy,usrzm,naxes,
     &                      cenpix,exposure,ximscale,ximzero,unit,
     &                      datamin,datamax,maptype,status)
            if ( Status.ne.0 ) goto 500
            if ( exp_map ) then

               maptype = 'R'
c
c            Apply averaging after read/rebin for exposure maps
c              (i.e. value/(rebin*rebin) )
c
               avgfact = usrzm(1)*usrzm(2)
               if ( avgfact .gt. 1.0 ) then
                  call mopwork(memr(mapptr),szx,szy,avgfact,1,1,
     &                         memr(mapptr),szx,szy,4,' ',rmin,rmax,
     &                         Status)
                  if ( Status.ne.0 ) goto 500
                  datamin = rmin
                  datamax = rmax
               endif

            endif

         endif

         call xdstr(datamin, -1, str1, slen1)
         call xdstr(datamax, -1, str2, slen2)
         write(ZWRite,*) ' Image level, min = ', str1(:slen1),
     &                                ' max = ', str2(:slen2)
         call xwrite(ZWRite, 10)

         call sethdr (setid,wcsid,Filename,ximscale,ximzero,unit,
     &                datamin,datamax,Szx,Szy,totzm,crpix,crval,
     &                object,emin,emax,telescop,instrume,detnam,filter,
     &                exposure,dtcor,vignapp,begobs,endobs,maptype)

         call gheads(setid,'WCSID', wcsid, 1, status)
c
c  Close FITS file
c
         Status = 0
 500     CALL XWRITE(' Done, closing file ',15)
         CALL FTCLOS(lun,Status)
         CALL FRELUN(lun)
c
c  Clean up after regions
c
         if ( numreg.gt.0 ) then
            call xfreereg(status)
            if ( status.ne.0 ) then
               call xwrite(' Failed to free regions', 10)
            endif
         endif

      else

            write(ZWRite,'(2a)') ' Invalid file type: ',
     &                       filetype(:LENACT(filetype))
            call XWRITE(ZWRite,5)

      endif

      if ( status.ne.0 ) Filename = ' '
      Imgcnt = Imgcnt + 1

      RETURN
99001 FORMAT (' Region file not found: ', a)
99002 FORMAT (' Using region file: ',a)
      END
