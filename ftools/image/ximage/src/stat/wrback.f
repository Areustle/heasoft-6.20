      SUBROUTINE WRBACK(Mapid, Filename, Ipd, Plot, Status)
      IMPLICIT NONE
c
c  Writes background file
c
c  I  mapid         (s)  Map id string
c  I  filename      (s)  Name of file
c  I  ipd           (i)  Number of count rate intervals 
c  I  plot          (l)  Whether to plot background distribution
c  O  status        (i)  Error flag (0 = OK)
c
      CHARACTER*(*) Mapid, Filename
      INTEGER*4 Ipd, Status
      LOGICAL Plot

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'
c
c  Local variables
c
      INTEGER*4 lun , no_of_rejects
      INTEGER*4 LENACT , i, inb , ir , iz , len
      INTEGER*4 MAXPD , lboxes
      PARAMETER (MAXPD=100)
      REAL*4 ctmin , ctmax , del , xpd(MAXPD+1) , pd_err , cmean
      INTEGER*4 iu , npd(MAXPD), slen1, slen2
      character(50) reason, buf1, buf2
      CHARACTER*(MAX_FILELEN+10) cmd

      character(80) telescop, instrume, detnam, ds
      integer*4 itel, detidx, szx, szy, imgequ
      real*8 zmx, zmy, xcen, ycen, exposure
      REAL*8 pixsize(2) , pix , piy
      real*8 ra_centre , dec_centre , dd , rollit
      character(20) strra, strdec
      real*4 dar1(3), dar2(3)
      real*4 imgpix, detpix, arcmin, arcminsec, detpixsec
      logical ISRNULL


      CALL GETLUN(lun)
      CALL OPENWR(lun,Filename,'unknown',' ',' ',0,0,Status)
      IF ( Status.NE.0 ) THEN
         WRITE (ZWRite,99001) Filename , Status
         CALL XWRITE(ZWRite,10)
         RETURN
      ENDIF

      call gheads(mapid, 'TELESCOP', telescop, 0, status)
      call gheads(mapid, 'INSTRUME', instrume, 0, status)
      call gheads(mapid, 'DETNAM', detnam, 0, status)
      itel = detidx(telescop, instrume, detnam)
      call get_refram(mapid, szx, szy, zmx, zmy, xcen, ycen, status)
     
c
      WRITE (lun,'(''! Results from XIMAGE background routine'')')
c telescope number
      WRITE (lun,'(''! telescope '',a)') telescop(:LENACT(telescop))
      WRITE (lun,'(''! instrument '',a)') instrume(:LENACT(instrume))
      WRITE (lun,'(''! detector '',a)') detnam(:LENACT(detnam))
      WRITE (lun,'(''! telescope number '',i4)') itel
c  image size in pixels
      WRITE (lun,'(''! image size in pixels '',        2(2x,i6))')
     &       szx , szy
c  x,y pixels of image center
      WRITE (lun,'(''! x,y pixels of image center '', 2(2x,f8.1))')
     &       xcen, ycen
c zoom factor
      call xdstr(zmx, 5, buf1, slen1)
      call xdstr(zmy, 5, buf2, slen2)
      IF ( buf1.eq.buf2 ) then
         WRITE (lun,'(''! zoom factor'',2x,a)') buf1(1:slen1)
      ELSE
         WRITE (lun,'(''! zoom factor'',2(2x,a))') buf1(1:slen1),
     &                                             buf2(1:slen2)
      ENDIF

      call get_skyhea(mapid,ra_centre,dec_centre,dd,rollit,
     &                pixsize,imgequ,ds,status)
      call cnv_radec(strra, strdec, ra_centre, dec_centre, dar1, dar2,
     &               2, 2, status)

c pixel size in arcseconds
      pix = abs(pixsize(1))*3600.
      piy = abs(pixsize(2))*3600.

      WRITE (lun,
     &       '(''! pixel size in arcseconds'',         2(2x,f10.4))')
     &       pix , piy
c
c ra, dec of image center in degrees
      WRITE (lun,'(''! equinox      '',i4)') imgequ
      WRITE (lun,'(''! ra  (center) '',g16.8)') ra_centre
      WRITE (lun,'(''! dec (center) '',g16.8)') dec_centre
c ra, dec of image center in h m s
      WRITE (lun,'(''! ra  (center) '',a)') strra(:LENACT(strra))
      WRITE (lun,'(''! dec (center) '',a)') strdec(:LENACT(strdec))
c roll angle in degrees
      WRITE (lun,'(''! roll angle '',g16.8)') rollit
c exposure time in seconds
      call gheadd(mapid, 'EXPOSURE', exposure, 0, status)
      WRITE (lun,'(''! exposure time in seconds'',        2x,f16.3)')
     &       exposure
c no of background boxes along x axis
      WRITE (lun,
     &    '(''! no of background boxes along x axis ''          ,2x,i4)'
     &    ) NBX
      WRITE (lun,
     &    '(''! no of background boxes along y axis ''          ,2x,i4)'
     &    ) NBY
c total no of background boxes
      WRITE (lun,
     &       '(''! total no of background boxes ''           ,2x,i4)')
     &       NBOxes
c size of background boxes in image pixels
      WRITE (lun,
     &'(''! size of background boxes in image pixels  ''     ,2x,       
     &i3)') IBBac
 
      ctmax = 0.0
      ctmin = 0.0
      cmean = 0.0
      no_of_rejects = 0
 
      DO 100 i = 1 , NBOxes
         IF ( BB_flags(i).NE.0 ) THEN
            no_of_rejects = no_of_rejects + 1
         ELSE
            IF ( BB(i).LT.ctmin .OR. ctmin.EQ.0.0 ) ctmin = BB(i)
            IF ( BB(i).GT.ctmax .OR. ctmax.EQ.0.0 ) ctmax = BB(i)
            cmean = BB(i) + cmean
         ENDIF
 100  CONTINUE
      lboxes = NBOxes - no_of_rejects
      IF ( lboxes.GT.6 ) THEN
         ctmax = ctmax
         ctmin = ctmin
         IF ( Ipd.GT.MAXPD ) Ipd = MAXPD
         del = ctmax - ctmin
         del = del/Ipd
 
         DO 150 iu = 1 , Ipd
            xpd(iu) = ctmin + del*(iu-1)
            npd(iu) = 0
            BB(iu) = (BB(iu)-cmean)
 150     CONTINUE
         xpd(Ipd+1) = xpd(Ipd) + del
c
         DO 200 i = 1 , NBOxes
            IF ( BB_flags(i).NE.0 ) THEN
               no_of_rejects = no_of_rejects + 1
            ELSE
               DO 160 iu = 1 , Ipd
                  IF ( BB(i).GE.xpd(iu) .AND. BB(i).LT.xpd(iu+1) )
     &                 npd(iu) = npd(iu) + 1
 160           CONTINUE
            ENDIF
 200     CONTINUE
         DO 250 iu = 1 , Ipd
            xpd(iu) = ctmin + del*(iu-1) + del*0.5
 250     CONTINUE
 
      ELSE
 
         Plot = .FALSE.
         CALL XWRITE(' Too few background boxes to plot',10)
 
      ENDIF
 
c no of rejected background boxes
      WRITE (lun,
     &       '(''! no of rejected background boxes  ''          ,2x,i4)'
     &       ) no_of_rejects
c rejected box numbers and codes
      WRITE (lun,'(''! rejected box numbers and codes '')')
      DO 300 i = 1 , NBOxes
         IF ( BB_flags(i).NE.0 ) THEN
            IF ( BB_flags(i).EQ.1 ) THEN
               reason = ' barycenter too far from center'
            ELSEIF ( BB_flags(i).EQ.2 ) THEN
               reason = ' non poissonian statistics '
            ELSEIF ( BB_flags(i).EQ.3 ) THEN
               reason = ' back value too far from average '
            ELSEIF ( BB_flags(i).EQ.4 ) THEN
               reason = ' box halves differ too much in dist.  '
            ENDIF
            WRITE (lun,
     &  '(''! box number '',i4,'' rejection code '',i2,              a)'
     &  ) i , BB_flags(i) , reason(1:LENACT(reason))
         ENDIF
 300  CONTINUE
c write out image background
      call backvals(Mapid, imgpix, detpix, arcmin, detpixsec, arcminsec,
     &              status)
      WRITE (lun,
     &'(''! Mean count rate =''1pe10.4,0p,'' cts/s'',           ''      
     &                                '')') cmean
      WRITE (lun,
     &'(''! background =''1pe10.4,0p,'' cts/pixel'',           ''       
     &                                '')') detpix
      if ( .not.ISRNULL(arcminsec) ) then
         WRITE (lun,
     &   '(''!            =''1pe10.4,0p,             '' cts/sqarcmin/s''   
     &   )') arcminsec
      else if ( .not.ISRNULL(arcmin) ) then
         WRITE (lun,
     &   '(''!            =''1pe10.4,0p,             '' cts/sqarcmin''   
     &   )') arcmin
      endif
      if ( .not.ISRNULL(detpixsec) ) then
         WRITE (lun,
     &     '(''!            =''1pe10.4,0p,            '' cts/pixel/s'')'
     &     ) detpixsec
      endif
c
c write out box values and weights
c
      WRITE (lun,'(''! Box values, error, npix and weights '')')
      DO 400 inb = 1 , NBOxes
         IF ( BB(inb).GE.0 ) WRITE (lun,99002) inb , BB(inb) , 
     &                              BB_sig(inb) , BB_npix(inb) , WW(inb)
 400  CONTINUE
c
c write out distribution of background
c
      ir = Ipd
      DO 500 inb = 1 , Ipd - 1
         iz = Ipd - inb
         IF ( npd(iz).NE.0 .AND. ir.EQ.Ipd ) ir = iz + 1
 500  CONTINUE
c
      WRITE (lun,'(''! Bgnd distribution'')')
      WRITE (lun,'(''read serr 1 2'')')
      WRITE (lun,'(''line step'')')
      WRITE (lun,'(''la x Counts per pixel'')')
      WRITE (lun,'(''la y No of occurences'')')
      del = del/2.0
      DO 600 inb = 1 , ir
         IF ( npd(inb).GT.0 ) THEN
            pd_err = SQRT(FLOAT(npd(inb)))
         ELSE
            pd_err = 0.0
         ENDIF
         WRITE (lun,99003) xpd(inb) , del , npd(inb) , pd_err
 600  CONTINUE
c
      CLOSE (lun)
      CALL FRELUN(lun)
c make a plot
 
      IF ( Plot ) THEN
         cmd = 'qdp '//Filename(:LENACT(Filename))
         len = LENACT(cmd)
         CALL SPAWN(cmd,len,status)
      ENDIF
c
      RETURN
99001 FORMAT ('Error opening file',a,'no',i4)
99002 FORMAT ('!',i6,1x,e10.4,1x,e10.4,1x,i10,1x,e10.4)
99003 FORMAT (' ',e11.5,1x,e11.5,1x,i10,1x,f9.4)
      END
