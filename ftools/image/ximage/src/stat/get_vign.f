      SUBROUTINE GET_VIGN(Mapid,Xpix,Ypix,Itel,Vcor,First,P_vign,Status)
      IMPLICIT NONE
c
c  I  mapid  (s)  Map id string
c  I  x/ypix (r)  Location of detection
c  I  itel   (i)  Telescope index
c  O  vcor   (r)  Vignetting correction
c I/O p_vign (i)  Pointer to vignetting map
c  O  status (i)  Error flag  (0=OK)
c
      character*(*) Mapid
      real*4 Xpix, Ypix, Vcor
      integer*4 Itel, P_vign, Status
      logical First

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/startup.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER*4 LENACT
      REAL*8 xoff , yoff, pixsize
      character(80) telescop , instrume, detnam
      CHARACTER*(MAX_FILELEN) engfile, vignfile
      LOGICAL*4 there

      integer*4 lun, block, imgnum, naxes(3), di
      integer*4 szx, szy, ix, iy, ip
      real*8 drpix(2), cenpix(2), xcen, ycen, datamin, datamax
      real*8 imgref(2), detref(2), dzm(2)

      integer i, j
      real*4 pscale, mapval
      integer MAXCHAN, npha, nbin, binning, begchan, endchan
      parameter(MAXCHAN = 2048)
      real*4 pha(MAXCHAN), eng(MAXCHAN), vfrac

      real*4 offaxis, ximg, yimg
      REAL*4 x1 , y1 , phi, tmpoff
      real*4 estep, eoffset, energy
      integer emin, emax, ncols, nrows, chstat
      integer sav_itel
      logical vigncalc, isrnull

      data sav_itel /-999/
      
      save sav_itel, emin, emax
      save xoff, yoff, pixsize, datamax, energy
      save cenpix, imgref, detref, dzm, szx, szy
      save engfile, vignfile

      Status = 0
c  Initialize to avoid warning
      vigncalc = .FALSE.
c  --
c
c  Get telescope id strings from id number
c
      if ( Itel.gt.0 ) then
         telescop = ZTElescop(itel)
         instrume = ZINstrume(itel)
         detnam = ZDEtnam(itel)
         call locase(telescop)
         call locase(instrume)
         call locase(detnam)
      else
         telescop = 'unknown'
         instrume = 'unknown'
         detnam = ' '
      endif
c
c  Look up header/mdb information for new mission
c
      if ( First ) sav_itel = -999
      if ( sav_itel.ne.Itel ) then
         vigncalc = .FALSE.
         datamax = 0.
         energy = -1.
         call get_refram(mapid, di, di, dzm(1), dzm(2), xcen, ycen, 
     &                   status)
         call get_optax(itel, xcen, ycen, xoff, yoff)
         call gheadi(mapid, 'EMIN', emin, 0, status)
         call gheadi(mapid, 'EMAX', emax, 0, status)
         call gmdbs(Itel, 'ENGFILE', engfile, 0, status)
         call gmdbs(Itel, 'VIGNFILE', vignfile, 0, status)
         call gmdbd(Itel, 'PIXSIZE', pixsize, 0, status)
c
c  Check for vignetting image specified in mdb
c
         if ( vignfile.ne.' ' ) then
            inquire(FILE=vignfile, EXIST=there)
            if ( there ) then
               write (ZWRite,*) ' Using vignetting map from mdb : ',
     &                          vignfile(:LENACT(vignfile))
               call xwrite(ZWRite, 15)
c              Override vignetting calculation with mdb file
               engfile = ' '
            else
               write (ZWRite,*) ' Vignetting map from mdb not found: ',
     &                          vignfile(:LENACT(vignfile))
               call xwrite(ZWRite, 10)
               vignfile = ' '
            endif
         endif
c
c    Find energy file (PI distribution file)
c
         if ( engfile.ne.' ' ) then
            inquire(FILE=engfile, EXIST=there)
            if ( there ) then
               write (ZWRite,*) ' Using energy file from mdb : ',
     &                          engfile(:LENACT(engfile))
               call xwrite(ZWRite, 15)
            else
               write (ZWRite,*) ' Energy file from mdb not found: ',
     &                          engfile(:LENACT(engfile))
               call xwrite(ZWRite, 10)
               engfile = ' '
            endif
         endif
c
c  If no energy file in mdb or input as Enginfile and no vignetting file
c  in mdb look in calibration directory under mission for energy file
c
         if ( engfile.eq.' ' .and. energy.lt.0. .and.
     &        vignfile.eq.' ' ) then
            call lkupcal(Itel,'eng_*.vign', engfile, status)
            if ( status.ne.0 ) then
               call lkupcal(Itel,'eng_*.dat', engfile, status)
            endif
            if ( status.eq.0 ) then
               write (ZWRite,*) ' Using energy file from cal : ',
     &                          engfile(:LENACT(engfile))
               call xwrite(ZWRite, 15)
            endif
         endif

      endif
c
c     Off axis in arcmin
c
      offaxis = SQRT((Xpix-xoff)**2.+(Ypix-yoff)**2.)*pixsize/60.
c
c     Vignetting function
c
c  If no vignetting file in mdb, assume function to calculate exists
c  (If function not found, calibration directory is searched later)
c
      if ( vignfile.eq.' ' ) vigncalc = .TRUE.
      if ( engfile.ne.' ' .or. energy.ge.0. .or. vigncalc ) then
c
c     Read energy distribution file
c
         if ( engfile.ne.' ' ) then
            if ( first ) call txinit(status)
            call txrdfile(engfile, status)
            call txrdikey(engfile, 'BINNING', 0, binning, status)
            call txrdkey(engfile, 'ESTEP', 0, estep, status)
            call txrdkey(engfile, 'EOFFSET', 0, eoffset, status)
            chstat = 0
            call txrdikey(engfile, 'BEGCHAN', 0, begchan, chstat)
            call txrdikey(engfile, 'ENDCHAN', 0, endchan, chstat)
            if ( chstat.ne.0 ) then
               begchan = emin
               endchan = emax
            endif
            call txinfo(engfile, ncols, nrows, status)
            if ( status.ne.0 ) then
               write (ZWRite,*) ' Energy file incomplete: ',
     &                            engfile(:LENACT(engfile))
               call xwrite(ZWRite, 10)
               engfile = ' '
               vigncalc = .FALSE.
               goto 400
            endif
            if ( ncols.gt.0 ) then
               call txrdcol(engfile, 1, MAXCHAN, pha, npha, status)
            else
               if ( first ) then
                  energy = eoffset
                  engfile = ' '
                  write(ZWRite, *) 
     &              ' Using average energy for vignetting: ', eoffset
                  call xwrite(ZWRite, 10)
               endif
               npha = 1
            endif
c
c  Translate channels into energies based on EOFFSET, ESTEP keywords
c
            do i = 1, npha
               eng(i) = estep*float(i-1) + eoffset
            enddo
c
c  Bin channels based on BINNING, BEGCHAN, ENDCHAN keywords
c
            i = MAX(emin, begchan)
            j = MIN(emax, endchan)
            if ( j.lt.i ) then
               i = 1
               j = 1
            endif
            call binchan(i,j,binning,npha,eng,pha,nbin)
         else
c
c   If no energy file, use input energy (Energyin)
c   If energy not given (i.e. <=0) then vign function probably isn't 
c   energy-dependent, otherwise an energy file should have been present
c
            if ( first .and. energy.gt.0. ) then
               write(ZWRite, *) ' Using average energy for vignetting: '
     &                          , energy
               call xwrite(ZWRite, 10)
            endif
            nbin = 1
            pha(nbin) = 1.
            eng(nbin) = energy
         endif
c
c retrive the vignetting map or function
c
         Vcor = 0.
c
c  Loop on energy bins
c
         DO i = 1, nbin
c
c  Mission-specific vignetting functions:
c    * Energy units of eng (from engfile) should be same as in function
c    * Offaxis is in arcmin, convert if function requires different
c    * Function sets vfrac to the correction at that offaxis/energy
c
            IF ( telescop.EQ.'rosat' ) THEN

               CALL ROSAT_VIGN(offaxis,eng(i),vfrac,Status)
               if ( status.ne.0 ) then
                  call XWRITE(' ROSAT vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif


            ELSEIF ( telescop.EQ.'einstein' ) THEN

               CALL EINSTEIN_VIGN(offaxis,eng(i),vfrac,Status)
               if ( status.ne.0 ) then
                  call XWRITE(' EINSTEIN vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif

            ELSEIF ( telescop.EQ.'asca' ) THEN

               x1 = (Xpix-xoff)
               y1 = (Ypix-yoff)
               phi = ATAN2(x1,y1)
               CALL ASCA_VIGN(offaxis,phi,eng(i),vfrac,Status)
               if ( status.ne.0 ) then
                  call XWRITE(' ASCA vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif

            ELSEIF (telescop.EQ.'sax') then
c
c distance from the centre in mm
c
c plate scale 1 arcsec = 8.969E-3 mm
c
               pscale = 8.969E-3
               tmpoff = offaxis*60.*pscale
               CALL SAX_VIGN(tmpoff,eng(i),vfrac,status)
               if ( status.ne.0 ) then
                  call XWRITE(' SAX vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif

            ELSEIF ( telescop.EQ.'jetx' ) then

               CALL JETX_VIGN(offaxis,eng(i),vfrac,status)
               if ( status.ne.0 ) then
                  call XWRITE(' JETX vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif

            ELSEIF ( telescop.EQ.'swift' ) then

               CALL SWIFT_VIGN(offaxis,eng(i),vfrac,status)
               if ( status.ne.0 ) then
                  call XWRITE(' Swift XRT vignetting failed', 10)
                  vigncalc = .FALSE.
                  goto 400
               endif

            ELSE

               if ( energy.gt.0 .or. engfile.ne.' ' ) then
                  call xwarn(' Energy unused, no vignetting function', 
     &                       10)
               endif
               energy = -1.
               engfile = ' '
               vigncalc = .FALSE.
               goto 400

            ENDIF
c
c  Scale contribution of calculated correction based on energy
c  distribution before adding to final vignetting correction
c
            if ( vfrac.gt.0. ) then
               Vcor = Vcor + (1./vfrac)*pha(i)
            else
               Vcor = 0.
            endif

         ENDDO
      ENDIF
c
c  Vignetting image file
c
  400 CONTINUE
      status = 0

      if ( first .and. vigncalc ) then
         call xwrite(' Internal vignetting function', 15)
      endif

      if ( .not.vigncalc ) then
c
c  If no internal vignetting function look in calibration directory
c  for vignetting map
c
         if ( first ) then
            if ( vignfile.eq.' ' ) then
               call lkupcal(Itel,'*_vignetting_correction.fits',
     &                      vignfile, Status)
c
c  If no vignetting map, use flat vignetting (i.e. no correction)
c
               if ( Status.eq.0 ) then
                  write (ZWRite,*) ' Using vignetting map from cal : ',
     &                             vignfile(:LENACT(vignfile))
                  call xwrite(ZWRite, 15)
               else
                  Status = 0
                  IF ( Itel.le.0 ) THEN
                     CALL XWRITE(' ** Warning unknown telescope ',10)
                  ELSE
                     CALL XWRITE(' ** Warning using a flat vignetting',
     &                           10)
                  ENDIF
                  goto 500
               endif
            endif
c
c  Get vignetting correction from file
c
            block = 1

            call GETLUN(lun)
            call FTOPEN(lun, vignfile, 0, block, status)
            if ( status.ne.0 ) then
               call XWRITE(' Could not open vignetting correction map',
     &                     10)
               call XWRITE(vignfile, 10)
               call FTCLOS(lun, status)
               call FRELUN(lun)
               goto 500
            endif
            call rd_imgsz(lun, 'IMG', imgnum, di, di, naxes, cenpix,
     &                    status)
            call FTCLOS(lun, status)
            call FRELUN(lun)
            if ( status.ne.0 ) goto 500

            szx = naxes(1)
            szy = naxes(2)
            call ralloc(1, szx, szy, P_vign, status)
            if ( status.ne.0 ) goto 500

            call rd_fitsmap(vignfile, memr(P_vign), szx, szy,
     &                      imgref, detref, dzm, datamin, datamax,
     &                      status)
            if ( status.ne.0 ) then
               call ralloc(0, szx, szy, P_vign, status)
               P_vign = -1
            endif
         endif

  500    CONTINUE

         if ( P_vign.ne.-1 ) then
            drpix(1) = (cenpix(1) - imgref(1))*dzm(1) + detref(1)
            drpix(2) = (cenpix(2) - imgref(2))*dzm(2) + detref(2)
            call calimgpix(szx,szy,dzm(1),dzm(1),drpix(1),drpix(2),
     &                     Xpix,Ypix,ximg,yimg,2)
            ix = NINT(ximg)
            iy = NINT(yimg)
            if ( ix.ge.1 .and. ix.le.szx .and.
     &           iy.ge.1 .and. iy.le.szy ) then
               ip = ix + (iy - 1)*szx
               mapval = memr(P_vign + ip - 1)
            else
               mapval = 0.
            endif
            if ( isrnull(mapval) ) then
               call xwarn(' Null vignetting map value', 15)
               Vcor = 0.0
            elseif ( mapval.eq.0 ) then
               call xwarn(' Zero vignetting map value', 15)
               Vcor = 0.0
            else
               Vcor = datamax/mapval
            endif
         else
            Vcor = 1.0
         endif

      endif
c
c  Note: P_vign must be freed outside get_vign
c
      First = .FALSE.
      sav_itel = Itel
      return
      end
