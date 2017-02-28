      SUBROUTINE SMOOTH(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c  Smooth image interface
c
c  I  cmdid   (i)  Command id
c  I  map     (r)  Image map
c  I  szx/y   (i)  Size of maps
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0 = OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
      include 'backgd.inc'
c
c  Local variables
c
      INTEGER*4 ibbace
      REAL*4 sigpix, sigmin, scaling_factor, max_scaling, back_bright
      REAL*4 image_max, max_smooth, scaling
      LOGICAL optimize, isloaded, skycor
      character(80) ctype
      REAL*8 pixsiz, dd
c
      INTEGER*4 argc, i, LENACT
      REAL*8 to_exposure, exposure
      LOGICAL smooth_x , smooth_y , x_only , y_only , exp_map
      LOGICAL wavelet, keepreal, round
      character*(MAX_IDSTR) exmapid
      integer exmapptr, exszx, exszy
      character(1) maptype
      character(100) ds
      character(80) mapcodes

      to_exposure = 0.d0
      exposure = 0.d0
      back_bright = 0.
      scaling = 0.
      sigpix = 1.5
      smooth_x = .TRUE.
      smooth_y = .TRUE.
      x_only = .FALSE.
      y_only = .FALSE.
      exp_map = .FALSE.
      wavelet= .false.
      keepreal = .false.
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARR(Cmdid,'SIGMA',sigpix,Status)
      CALL GPARR(Cmdid,'SCALING_FACTOR',scaling,Status)
      CALL GPARR(Cmdid,'BACK_BRIGHTNESS',back_bright,Status)
      CALL GPARD(Cmdid,'TO_EXPOSURE',to_exposure,Status)
      CALL GPARL(Cmdid,'X_ONLY',x_only,Status)
      CALL GPARL(Cmdid,'Y_ONLY',y_only,Status)
      CALL GPARL(Cmdid,'EXPOSURE_MAP',exp_map,Status)
      CALL GPARL(Cmdid,'WAVELET',wavelet,status)
      CALL GPARL(Cmdid,'REAL',keepreal,status)
      if ( status.ne.0 ) return

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         status = -1
         return
      endif
      if ( sigpix.le.0 ) then
         call xwrite(' SIGMA must be larger than 0', 10) 
         status = -1
         return
      endif
      
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      if ( maptype(1:1).eq.'I' ) then
         round = .not.keepreal
      else 
         round = .false.
      endif
c
c  Calculate sigma in arcmin from entered sigma in pixels
c
      call gheadd(mapid, 'CDELT1', pixsiz, 0, status)
      pixsiz = abs(pixsiz)*60.
      call gheads(mapid, 'CTYPE1', ctype, 0, status)
      i = LENACT(ctype)
      call UPC(ctype)
      skycor = ctype(i-2:i).eq.'TAN'

      if ( skycor ) then
         sigmin = sigpix*pixsiz
         write(ZWRite,'(a,f10.5,a)') ' Sigma (arcmin) : ', sigmin
         call XWRITE(ZWRite, 10)
      endif
c
c the background scaling as the normal scaling
c This include the background smooth
c and the standard smoth with a cut at 50
c
      call gheadd(mapid, 'DATAMAX', dd, 0, status)
      image_max = dd
      if ( image_max.eq.0 ) image_max = 1.0
      max_smooth = 50.
      max_scaling = float(MAX_INT)/image_max

      if ( back_bright.gt.0. ) then
         if ( NBOxes.gt.0 ) then
            call xwrite(' Using existing background calculation', 10)
            call prback(mapid, status)
            scaling_factor=1.0/BNEw
         else
            ibbace = 0
            optimize = .FALSE.
            DRAw_boxes = .FALSE.
            SIGmult = DEFsigm
            BARylim = DEFbarl
            BXHprob = DEFhprob
            call getbgmap(Mapid, BGMap, BGSz, status)
            if ( status.ne.0 ) return
            call do_back(Map,Szx,Szy,Mapid,optimize,ibbace,status)
            if ( status.ne.0 ) return
            scaling_factor=1.0/BNEw
            NBOxes = 0
         endif
      else
         IF (image_max.GT.max_smooth) max_smooth=image_max
         scaling_factor= max_smooth/image_max
      endif
c
c Test for the different option
c 1- bright-background, 2-scaling from a qualifier, 3-scaling is 
c from best1 with the cut intensity at 50 
c
      if (back_bright.gt.0.) then
         call XWRITE(' 1- bright-background', 15)
         scaling_factor=back_bright*scaling_factor
      elseif(scaling.gt.0)then 
         call XWRITE(' 2- scaling from qualifier', 15)
         scaling_factor=scaling
      else
         call XWRITE(' 3- cut intensity at 50', 15)
      endif 
c 
c If to_exposure is specified the scaling_factor is overwritten
c
      if ( to_exposure.gt.0.d0 ) then
         call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
         if ( exposure.le.0.d0 ) then
            call xwrite(' No valid exposure time, aborting smooth', 10)
            return
         endif
         scaling_factor= to_exposure/exposure
      endif
c 
      IF ( x_only .AND. y_only ) THEN
         CALL XWRITE(' Smoothing will be performed in x AND y ',10)
      ELSE
         IF ( x_only ) smooth_y = .FALSE.
         IF ( y_only ) smooth_x = .FALSE.
      ENDIF
c
      IF ( scaling_factor.GT.max_scaling ) THEN
         scaling_factor = max_scaling
         CALL XWRITE(' Max scaling factor exceeded ',10)
         WRITE (ZWRite,'('' scaling factor reset to '',f8.3)')
     &          max_scaling
         CALL XWRITE(ZWRite,10)
      ENDIF

      if ( exp_map ) then
         call gheads(mapid, 'EXMAPID', exmapid, 0, status)
         if ( exmapid.eq.' ' ) then
             call xwrite(' No exposure map for current map', 10)
             status = -1
             return
         endif
         call gheadi(exmapid, 'MAPPTR', exmapptr, 0, status)
         call gheadi(exmapid, 'SZX', exszx, 0, status)
         call gheadi(exmapid, 'SZY', exszy, 0, status)
         call smoothwork(memr(exmapptr),exszx,exszy,exmapid,sigpix,
     &                   scaling_factor,smooth_x,smooth_y,wavelet,
     &                   .FALSE.,status)
      else
         call smoothwork(Map,Szx,Szy,Mapid,sigpix,scaling_factor,
     &                   smooth_x,smooth_y,wavelet,round,status)
      endif
c
c  Append operation to MAPCODES
c
      call gheads(mapid, 'MAPCODES', mapcodes, 0, status)
      write(ds,'(2a)') mapcodes(:LENACT(mapcodes)), 'S'
      call gheads(mapid, 'MAPCODES', ds, 1, status)
      call expiremap(mapid, status)
c
      RETURN
      END
