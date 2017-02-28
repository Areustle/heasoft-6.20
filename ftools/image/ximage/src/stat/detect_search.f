      SUBROUTINE DETECT_SEARCH(Map, Szx, Szy, Mapid, Snr_thr, Prob_lim,
     &                         Detlab, Status)
      IMPLICIT NONE
c
c  Detection algorithm
c
c  I  map                 (i)  Image map
c  I  szx/y               (i)  Size of map
c  I  mapid               (s)  Map id string
c  I  snr_thr             (r)  Signal to noise ratio threshold
c  I  prob_lim            (r)  Probability that src is a bg fluctuation
c  I  detlab              (l)  Whether to label detections
c  O  status              (i)  Error flag (0 = OK)
c
      INTEGER*4 Szx, Szy
      real*4 Map(Szx,Szy)
      character*(*) Mapid
      REAL*4 Snr_thr, Prob_lim
      LOGICAL Detlab
      INTEGER*4 Status

      INCLUDE '../include/io.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE 'backgd.inc'
      INCLUDE 'excess.inc'
      INCLUDE 'detect.inc'
      INCLUDE 'ximpsf.inc'
c
c  Local variables
c
      logical isdisplay
      character(10) bgmapid
      integer*4 i, j, ii, jj, di, itel
      real*4 xpix, ypix, bgxpix, bgypix, bgxpos, bgypos
      real*8 xoff, yoff, xcen, ycen, bgxcen, bgycen
      real*8 zmx, zmy, bgzmx, bgzmy
      integer*4 ibgx, ibgy

      logical good, first, unknown
      real*4 xpos, ypos
      real*4 roff(MAXsou), rmin, rmax, rdel, rpre
      integer*4 rord(MAXsou), opixbox, optbox
      REAL*4 sfa
      real*4 scts, opixback, optbxh, stonr
      real*4 srccnts, srcpix, baccnts, bacpix
      real*8 exposure
      REAL*4 scrit, plim, dist

      integer slen
      character(20) ds, text
      REAL*4 boxh, xtxt, ytxt
      integer*4 srcnum, numdup
      character(1) bs

      bs = CHAR(92)
c
c set energy for energy dependent psf to 1.0 kev
c
      bgmapid = 'BGMAP'
c
c  Retrieve detector details
c
      call get_itel(mapid, itel)
      call get_refram(mapid, di, di, zmx, zmy, xcen, ycen, status)
      call get_refram(bgmapid, di, di, bgzmx, bgzmy, bgxcen, bgycen,
     &                status)
      call get_optax(itel, xcen, ycen, xoff, yoff)
c     xoff = xoff + 0.5
c     yoff = yoff + 0.5
      call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
c
c loop on candidate sources
c
      IF ( NUMexs.LE.0 ) RETURN
      NUMdet = 0
c
c sort by radius
c
      CALL XWRITE(' >>>>> Sort by radius',10)
      do i = 1 , NUMexs
c        call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
c    &                  ASOux(i)-0.5, ASOuy(i)-0.5, 1)
         call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                  ASOux(i)+0.5, ASOuy(i)+0.5, 1)
         roff(i) = SQRT((xpix-xoff)**2.+(ypix-yoff)**2.)
         rord(i) = i
      enddo
      CALL QSORT_DETECT(roff, rord, MAXsou, NUMexs, Status)
      if ( status.ne.0 ) return
 
      rmin = roff(1)
      rmax = roff(NUMexs)
      rdel = (rmax-rmin)/30.0
      IF ( rdel.LE.0.0 ) rdel = 1.0
      rpre = rmin - rdel*2.0
c
c  Init first for NPSF routine outside loop
c
      first = .TRUE.
c
      CALL XWRITE(' >>>>> Applying thresholds',10)
c
      DO 300 jj = 1, NUMexs
         i = rord(jj)
         good = .TRUE.
         xpos = ASOux(i)
         ypos = ASOuy(i)
         IF ( xpos.NE.0. .OR. ypos.NE.0. ) THEN
c
c get proper psf for source
c
c           call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
c    &                     xpos-0.5, ypos-0.5, 1)
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                     xpos+0.5, ypos+0.5, 1)
            IF ( roff(jj)-rpre.GT.rdel ) THEN
               CALL NPSF(Mapid, xpix,ypix,itel,sfa,-1.0,' ',first,
     &                   .FALSE., unknown,status)
               if ( status.ne.0 ) then
                  call XWRITE(' Aborting: NPSF failed', 10)
                  return
               endif
               ii = (roff(jj)-rmin)/rdel
               rpre = ii*rdel + rmin
            ENDIF
c
c get optimum box size for intensity estimation
c
            scts = INTm(i)
            opixback = BACk/(zmx*zmy)
            CALL NOBOX(sfa,opixback,scts,opixbox)
            if ( itel.gt.0 ) then
               if ( ZTElescop(itel).eq.'EXOSAT' .and.
     &              ZINstrume(itel)(1:3).eq.'CMA' ) then
                  IF ( roff(jj).LT.400. ) THEN
                     IF ( opixbox.GT.30 ) opixbox = 30
                  ELSE
                     IF ( opixbox.GT.50 ) opixbox = 50
                  ENDIF
               endif
            endif
c
c first estimate of source controid
c
            optbox = MAX(1,NINT(float(opixbox)/zmx))
            optbxh = float(optbox)/2.
            CALL BARYCENTER(Map,Szx,Szy,xpos,ypos,optbox,good)
c
c  Note: trybary was written assuming a coordinate convention
c  where the center of the pixel is an integer value
c                           |_|_|_|
c                            1 2 3
c
c  Detect assumes the left edge of the pixel is 
c  its integer value.  i.e. |_|_|_|
c                           1 2 3 
c
c  There is a 0.5 adjustment to translate between the conventions
c
c           xpos = xpos - 0.5
c           ypos = ypos - 0.5
c           CALL TRYBARY(Map,Szx,Szy,xpos,ypos,optbox,good)
c           xpos = xpos + 0.5
c           ypos = ypos + 0.5
c
c calculate total number of counts in optimum size box
c
            CALL RAW_COUNTS(Map,Szx,Szy,xpos,ypos,srccnts,srcpix,
     &                      optbox,optbxh,good)
c
c estimate local background from background boxes near source position
c
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, bgxpix,
     &                     bgypix, xpos+0.5, ypos+0.5, 1)
            call calimgpix(BGSz, BGSz, bgzmx, bgzmy, bgxcen, bgycen, 
     &                     bgxpix, bgypix, bgxpos, bgypos, 2)
c
c  To match prior version... (maybe incorrect)
c  Under "leftmost side of first pixel is 0" system, array index
c    formula is int(imgpos)+1, however original code used int(imgpos)
c    Current convention for bgx/ypos is "center of first pixel is 1"
c    Array index formula should be nint(imgpos).  int(imgpos-0.5)
c    mimics original behavior.
c
c           ibgx = nint(bgxpos)
c           ibgy = nint(bgypos)
            ibgx = int(bgxpos-0.5)
            ibgy = int(bgypos-0.5)

            NUMdet = NUMdet + 1
            if ( NUMdet.gt.MAXdet ) then
               call xwarn(' Maximum number of sources reached', 10)
               NUMdet = NUMdet - 1
               goto 400
            endif
            CALL LOCAL_BG(xpos,ypos,ibgx,ibgy,zmx,baccnts,bacpix,
     &                    status)
            if ( status.ne.0 ) return
            IF ( bacpix.EQ.0 ) THEN
               baccnts = BNEw
               bacpix = 1
            ENDIF
c
c estimate source intensity
c
            write(ZWRite,*) NUMdet, ' - x/ypix ', xpix, ypix
            call XWRITE(ZWRite, 20)

            CALL INTENSITY(srccnts,srcpix,baccnts,bacpix,opixbox,
     &                     unknown,stonr)
c           print*,srccnts,srcpix,baccnts,bacpix,opixbox,stonr
c
c     set dynamical thresholds
c                        scrit (min signal to noise)
c                        plim  (prob that source is a back fluctuation)
c
c     thresholds are increased dynamically for large exposure times
c                                          and large off_axis angles
c
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                     xpos, ypos, 1)
            dist = SQRT((xpix-xoff)**2.+(ypix-yoff)**2.)
            CALL DYNM_THRESH(itel,exposure,dist,scrit,plim)
            IF ( Snr_thr.GT.0.0 ) scrit = Snr_thr
            IF ( Prob_lim.GT.0.0 ) plim = Prob_lim
c
c  check if source intensity is above dynamical thresholds
c
            CALL FINAL_CHECKS(itel,xpix,ypix,stonr,scrit,plim,dist)
            DTSox(NUMdet) = xpos
            DTSoy(NUMdet) = ypos
            BXHa(NUMdet) = optbxh
            if ( good ) then
               GOOdflag(NUMdet) = 1
            else
               GOOdflag(NUMdet) = 0
            endif

         ENDIF
 300  CONTINUE
 400  CONTINUE
c
c end of main loop
c
c check for duplicate entries and remove them
c
c     do i = 1, NUMdet
c        print*,i,DTSox(i),DTSoy(i),PSFco(i),VCO(i)
c        print*,i,BASo(i),SINt(i),ERR(i),INTm(i)
c        print*,i,PROb(i),HOT(i),BXHa(i),GOOdflag(i)
c     enddo

      numdup = 0
      write(ZWRite,*) ' Detections before dup removal ', NUMdet
      call XWRITE(ZWRite,15)
      CALL XWRITE(' >>>>> removing duplicates',10)
      DO 500 i = 1 , NUMdet
         DO 450 j = 1 , NUMdet
            IF ( i.NE.j .AND. HOT(j).NE.0 .AND. HOT(i).NE.0 ) THEN
c
c the condition is changed /2
c               boxh = (BXHa(i)+BXHa(j))
               boxh = (BXHa(i)+BXHa(j))/1.5
               IF ( ABS(DTSox(i)-DTSox(j)).LE.boxh .AND. 
     &              ABS(DTSoy(i)-DTSoy(j)).LE.boxh ) THEN
                  numdup = numdup + 1
                  IF ( SINt(i).LT.SINt(j) ) THEN
                     HOT(i) = 0
                  ELSE
                     HOT(j) = 0
                  ENDIF
               ENDIF
            ENDIF
 450     CONTINUE
 500  CONTINUE
      write(ZWRite,*) ' Detections after dup removal ', NUMdet-numdup
      call XWRITE(ZWRite,15)
c
c draw boxes on image, if ok
c
      srcnum = 0
      do i = 1, NUMdet

C Convert DTSox/y and BXHa to original pixel coordinates

         BXHa(i) = BXHa(i)*zmx
c        call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
c    &                  DTSox(i)-1., DTSoy(i)-1., 1)
         call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix, ypix,
     &                  DTSox(i)-0.5, DTSoy(i)-0.5, 1)
c        IMSox(i) = DTSox(i)-0.5
c        IMSoy(i) = DTSoy(i)-0.5
         IMSox(i) = DTSox(i)
         IMSoy(i) = DTSoy(i)
         DTSox(i) = xpix
         DTSoy(i) = ypix

         IF ( isdisplay() .AND. HOT(i).GE.1 ) THEN
c
            xtxt = (xpix - BXHa(i))
            ytxt = (ypix - BXHa(i))

            IF ( HOT(i).NE.2 ) THEN
               srcnum = srcnum + 1
               call xistr(srcnum, ds, slen)
c              Character 0199 is a space
               write(text,'(3a)') ds(1:slen),bs,'(0199)'
            ELSE
               text = 'Hot Spot'
            ENDIF

            call jrnbox(xpix,ypix,BXHa(i)*2.,BXHa(i)*2.,0.,-1,-1,-1)
            if ( Detlab ) then
               call jrnlab(xtxt,ytxt,text,' ',-1,-1.0,-1,-1,
     &                     -1,-1.0,-1,'right',0.0)
            endif
         ENDIF
      enddo
c
c clean up
c
      IF ( NUMdet.GT.0 ) THEN
         j = 0
         DO 550 i = 1 , NUMdet
            IF ( HOT(i).EQ.1 ) THEN
               j = j + 1
               IMSox(j) = IMSox(i)
               IMSoy(j) = IMSoy(i)
               DTSox(j) = DTSox(i)
               DTSoy(j) = DTSoy(i)
               PSFco(j) = PSFco(i)
               VCO(j) = VCO(i)
               BASo(j) = BASo(i)
               SINt(j) = SINt(i)
               ERR(j) = ERR(i)
               PROb(j) = PROb(i)
               HOT(j) = 1 
               BXHa(j) = BXHa(i)
               GOOdflag(j) = GOOdflag(i)
            ENDIF
 550     CONTINUE
         NUMdet = j
      ENDIF
      write(ZWRite,*) ' Number of detections after cleanup ', NUMdet
      call XWRITE(ZWRite, 15)
      RETURN
      END
