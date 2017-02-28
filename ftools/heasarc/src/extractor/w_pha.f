
C*==w_pha.spg  processed by SPAG 4.50J  at 15:13 on  8 Mar 1995
c*******************************************************************
c     SUBROUTINE:
c     W_PHA
c
c     DESCRIPTION:
c     This subroutine writes a fits PHA file
c
c*******************************************************************
 
 
      SUBROUTINE W_PHA(Phasze, Pha, Chan, Gtis, Imaxgtis, Imaxccd, 
     &                 Gtihdunms, Exposs, Area, Qinreg,
     &                 Imszh1, Imszh2, Imageh, Iwmsz1, Iwmsz2, Wmap, 
     &                 Bbox, Totpixh, Onefile)

      IMPLICIT NONE
 
      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
 
      INTEGER Imszh1, Imszh2, Iwmsz1, Iwmsz2, Phasze
      INTEGER Chan(Phasze), Bbox(4)
      REAL Pha(Phasze)
      REAL Imageh(Imszh1,Imszh2), Wmap(Iwmsz1, Iwmsz2)
      DOUBLE PRECISION Area
      INTEGER Imaxgtis(MAXCCD), Imaxccd, Totpixh
      DOUBLE PRECISION Gtis(3,MAXGTI,MAXCCD), Exposs(MAXCCD)
      CHARACTER*(*) Onefile
      character(30) Gtihdunms(MAXCCD)
      LOGICAL Qinreg(0:MAXCCD-1)
 

      DOUBLE PRECISION dtmp

      REAL backscal

      INTEGER lun , inlun , blocksize, status
      INTEGER i, j, hdutype
      INTEGER bitpix , naxis , naxes(100) , pcount , gcount
      INTEGER group , photons
 
      LOGICAL simple , extend, qexist

      character(30) extname
      CHARACTER(255) comment
      CHARACTER(72) contxt
      CHARACTER(70) histkey
 
      character(40) taskname
      COMMON / task / taskname

      INTEGER LENACT
      EXTERNAL LENACT


      lun = 0
      inlun = 0
      blocksize = 0
      j = 0
      hdutype = 0
      status = 0
      DO 100 i = 1 , 100
         naxes(i) = 0
 100  CONTINUE
      bitpix = 0
      naxis = 0
      group = 0
      comment = ' '
      dtmp = 0.0
      i = 0
 
      IF ( Phafile.EQ.' ' ) RETURN
 
      photons = 0
      DO i = 1 , Phasze
         photons = NINT(photons + Pha(i))
      ENDDO

      IF ( Expos .GT. 0. ) THEN
         WRITE (comment,'(a,i8,a,1pg10.4,a)') ' Spectrum         has ',
     &       photons, ' counts for ', photons/Expos, ' counts/sec'
      ELSE
         comment = ' Spectrum has no exposure time'
      ENDIF
      CALL fcecho(comment) 
 
      status = 0
 
 
C     ********************************************************
 
C     *********************************************************

c Set up the wmap array to write out the WMAP.

      Buildwmap = Buildwmap .AND. (Bbox(3).GE.Bbox(1)) 
     &                      .AND. (Bbox(4).GE.Bbox(2))

      IF ( Buildwmap ) THEN
         DO j = Bbox(2), Bbox(4)
            DO i = Bbox(1), Bbox(3)
               wmap(i-Bbox(1)+1,j-Bbox(2)+1) = Imageh(i,j)
            ENDDO
         ENDDO
      ENDIF

c Open the output file

      INQUIRE(file=Phafile(:lenact(Phafile)), exist=qexist)
      IF ( qexist .AND. clobber ) 
     &           CALL delfil(Phafile(:lenact(Phafile)))
      CALL GETLUN(lun)
      CALL FTINIT(lun,Phafile,1,status)
 
      simple = .TRUE.
      bitpix = -32
      pcount = 0
      gcount = 1
      extend = .TRUE.

      IF ( Buildwmap ) THEN
         naxis = 2
         naxes(1) = Iwmsz1
         naxes(2) = Iwmsz2
      ELSE
         naxis = 0
      ENDIF

C     Now do the weighted map
C     write the required primary array keywords:
 
      CALL FTPHPR(lun,simple,bitpix,naxis,naxes,pcount,gcount,extend,
     &            status)
      CALL FTRDEF(lun,status)

C     define primary array structure:
 
      CALL FTPDEF(lun,bitpix,naxis,naxes,pcount,gcount,status)
 
C Copy the other keywords from the input file
 
      CALL GETLUN(inlun)
      CALL FTOPEN(inlun,Onefile,0,blocksize,status)
 
C Copy the primary header keywords

      CALL XCOPYNOSCALE(inlun,lun,status)

C Find the event extension in the event file

      i = 2
      CALL UPC(Eventname)
      extname = ' '
      DO WHILE ( status.EQ.0 .AND. extname.NE.Eventname )
         CALL FTMAHD(inlun,i,hdutype,status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGKYS(inlun,'EXTNAME',extname,comment,status)
            CALL UPC(extname)
         ENDIF
         i = i + 1
      ENDDO

C copy the event extension keywords

      CALL XCOPYNOSCALE(inlun,lun,status)
      contxt = 'Error copying header'
      IF ( status.NE.0 ) GOTO 999

c Get rid of any duplicate keywords and the NEVENTS and EXTNAME keywords

      CALL rmvdky(lun, status)

      CALL FTDKEY(lun,'NEVENTS',status)
      IF ( status .NE. 0 ) status = 0
      CALL FTDKEY(lun,'EXTNAME',status)
      IF ( status .NE. 0 ) status = 0

c Set the descriptor keywords
 
      CALL FTDKEY(lun,'hduname',status)
      IF ( status .NE. 0 ) status = 0
      CALL FTUKYS(lun,'hduname','WMAP',' ',status)
      CALL FTUKYS(lun,'hduclass','ogip',
     &              'Format conforms to OGIP/GSFC conventions',status)
      CALL FTUKYS(lun,'hduclas1','IMAGE',
     &              'Extension contains an image',status)
      CALL FTUKYS(lun,'hduclas2','WMAP',
     &              'Extension contains a weighted map',status)
 
      IF ( Buildwmap ) THEN
         CALL FTUKYJ(lun,'axlen1',naxes(1),'Dim of axis 1',status)
         CALL FTUKYJ(lun,'axlen2',naxes(2),'Dim of axis 2',status)
      ENDIF
 
      CALL FTUKYS(lun,'CONTENT','SPECTRUM',
     &              'spectrum file contains time intervals and events',
     &              status)

      contxt = 'error writing keyword'
      IF ( status.NE.0 ) GOTO 999

      CALL wstdky(lun, status)

      IF ( Buildwmap ) THEN
 
         CALL FTUKYJ(lun,'WMREBIN',Binh,'Weighted Map rebinning',
     &                 status)
 
         CALL FTUKYJ(lun,'OBS-MODE',-10,
     &                 'observing mode 1=point,2=slew,3=calibration',
     &                 status)
 
         call FTUKYE(lun,'NPIXSOU',SNGL(Area),10,
     &               'Number of image bin in region',status)
         call FTUKYJ(lun,'SKYBIN',binf, 'Image binning factor',status)
         call FTUKYD(lun,'PIXSIZE',skypix*binf,15,
     &               'Image binsize (degrees)',status)
         call FTUKYL(lun,'WMAPFIX',wtmapfix,
     &           'If true pixels outside selected region set to -1',
     &               status)

c Write the DataModel keywords

         IF ( hmtype(1) .NE. ' ' ) THEN
            CALL FTPKYS(lun,'mtype1',hmtype(1),
     &               'DM Keyword: Descriptor name.', status)
            CALL FTPKYS(lun,'mform1',hmform(1),
     &               'DM Keyword: Descriptor value.', status)
         ENDIF
         IF ( hmtype(2) .NE. ' ' ) THEN
            CALL FTPKYS(lun,'mtype2',hmtype(2),
     &               'DM Keyword: Descriptor name.', status)
            CALL FTPKYS(lun,'mform2',hmform(2),
     &               'DM Keyword: Descriptor value.', status)
         ENDIF

c Write the data subspace keywords

         CALL W_DSKEYS(lun, status)

c Write the WCS coordinates

         IF ( wmapver .EQ. 1 ) THEN

            CALL FTUKYS(lun,'HDUVERS','1.0.0',
     &                  'Version of format (OGIP memo OGIP-96-001)',
     &                  status)
            CALL FTUKYS(lun,'HDUVERS1','1.0.0','Obsolete',status)
            CALL FTUKYS(lun,'HDUVERS2','1.0.0','Obsolete',status)

            CALL FTUKYD(lun,'crpix1',1.0d0,15,'X axis reference pixel',
     &                  status)
            CALL FTUKYJ(lun,'crval1',Bbox(1)-1+(hbbox(1)+binh-1)/binh,
     &                  'coord of X ref pixel', status)
            dtmp = hcrdelt(1)*binh
            CALL FTUKYD(lun,'cdelt1',dtmp,15,'X axis increment',
     &                  status)

            CALL FTUKYD(lun,'crpix2',1.0d0,15,'Y axis reference pixel',
     &                  status)
            CALL FTUKYJ(lun,'crval2',Bbox(2)-1+(hbbox(2)+binh-1)/binh,
     &                  'coord of Y ref pixel', status)
            dtmp = hcrdelt(2)*binh
            CALL FTUKYD(lun,'cdelt2',dtmp,15,'Y axis increment',
     &                  status)

         ELSE

            CALL FTUKYS(lun,'ctype1',hctype(1),' ',status)
            IF ( hcname(1).NE.' ' ) CALL FTUKYS(lun,'cname1',hcname(1),
     &                                          ' ',status)
            dtmp = (hcrpix(1)-(hbbox(1)-1))/DBLE(Binh) 
     &            + DBLE(Binh-1)/DBLE(2*Binh) - BBox(1) + 1
            CALL FTUKYD(lun,'crpix1',dtmp,15,'X axis reference pixel',
     &                  status)
            CALL FTUKYD(lun,'drpix1',hcrpix(1),15,
     &                  'original X axis reference pixel', status)
            CALL FTUKYD(lun,'crval1',hcrval(1),15,
     &                  'coord of X ref pixel', status)
            dtmp = hcrdelt(1)*binh
            CALL FTUKYD(lun,'cdelt1',dtmp,15,'X axis increment',
     &                  status)
            CALL FTUKYD(lun,'ddelt1',hcrdelt(1),15,
     &                  'original X axis increment', status)

            CALL FTUKYS(lun,'ctype2',hctype(2),' ',status)
            IF ( hcname(2).NE.' ' ) CALL FTUKYS(lun,'cname2',hcname(2),
     &                                          ' ',status)
            dtmp = (hcrpix(2)-(hbbox(2)-1))/DBLE(Binh) 
     &            + DBLE(Binh-1)/DBLE(2*Binh) - BBox(2) + 1
            CALL FTUKYD(lun,'crpix2',dtmp,15,'Y axis reference pixel',
     &                  status)
            CALL FTUKYD(lun,'drpix2',hcrpix(2),15,
     &                  'original Y axis reference pixel', status)
            CALL FTUKYD(lun,'crval2',hcrval(2),15,
     &                  'coord of Y ref pixel', status)
            dtmp = hcrdelt(2)*binh
            CALL FTUKYD(lun,'cdelt2',dtmp,15,'Y axis increment',
     &                  status)
            CALL FTUKYD(lun,'ddelt2',hcrdelt(2),15,
     &                  'original Y axis increment', status)

c Set physical WCS coordinates. Includes the LTV and LTM keywords which
c dmextract writes out although I don't think they are standard WCS.

            CALL FTUKYS(lun,'wcsnamep','PHYSICAL',' ',status)

            CALL FTUKYS(lun,'wcsty1p','PHYSICAL',' ',status)
            dtmp = (1-(hbbox(1)-1))/DBLE(Binh) 
     &         + DBLE(Binh-1)/DBLE(2*Binh) - BBox(1) + 1 - 1.0d0/binh
            CALL FTUKYD(lun,'ltv1',dtmp,15,
     &                  'offset for logical to physical pixel',status)
            dtmp = 1.0d0/binh
            CALL FTUKYD(lun,'ltm1_1',dtmp,15,
     &                  'slope for logical to physical pixel',status)
            CALL FTUKYS(lun,'ctype1p',xcolh,'Source of X-axis',status)
            CALL FTUKYD(lun,'crpix1p',1.0d0,15,'X axis reference pixel',
     &                  status)
            dtmp = (hbbox(1)-1) + (BBox(1)-1)*Binh + (Binh+1)/2.
            CALL FTUKYD(lun,'crval1p',dtmp,15,
     &                  'coord of X ref pixel in original image', 
     &                  status)
            dtmp = binh
            CALL FTUKYD(lun,'cdelt1p',dtmp,15,'X axis increment',
     &                  status)

            CALL FTUKYS(lun,'wcsty2p','PHYSICAL',' ',status)
            dtmp = (1-(hbbox(2)-1))/DBLE(Binh) 
     &         + DBLE(Binh-1)/DBLE(2*Binh) - BBox(2) + 1 - 1.0d0/binh
            CALL FTUKYD(lun,'ltv2',dtmp,15,
     &                  'offset for logical to physical pixel',status)
            dtmp = 1.0d0/binh
            CALL FTUKYD(lun,'ltm2_2',dtmp,15,
     &                  'slope for logical to physical pixel',status)
            CALL FTUKYS(lun,'ctype2p',ycolh,'Source of Y-axis',status)
            CALL FTUKYD(lun,'crpix2p',1.0d0,15,'Y axis reference pixel',
     &                  status)
            dtmp = (hbbox(2)-1) + (BBox(2)-1)*Binh + (Binh+1)/2.
            CALL FTUKYD(lun,'crval2p',dtmp,15,
     &                  'coord of Y ref pixel in original image', 
     &                  status)
            dtmp = binh
            CALL FTUKYD(lun,'cdelt2p',dtmp,15,'Y axis increment',
     &                  status)

            CALL FTUKYS(lun,'HDUVERS','2.0.0',
     &                  'Version of format (OGIP memo OGIP-96-001)',
     &                  status)

         ENDIF

         CALL FTUKYD(lun,'optic1',hoptic(1),15,'X Optical axis',
     &               status)
         CALL FTUKYD(lun,'optic2',hoptic(2),15,'Y Optical axis',
     &               status)

c Not standard but useful for diagnostic purposes

         CALL FTUKYJ(lun,'BBOX1',Bbox(1),'extractor bbox(1)',status)
         CALL FTUKYJ(lun,'BBOX2',Bbox(2),'extractor bbox(2)',status)
         CALL FTUKYJ(lun,'HBBOX1',hbbox(1),'extractor hbbox(1)',status)
         CALL FTUKYJ(lun,'HBBOX2',hbbox(2),'extractor hbbox(2)',status)

         CALL FTUKYJ(lun,'X-OFFSET',Bbox(1)-1+(hbbox(1)+binh-1)/binh,
     &               'first original X pixel in map',status)
         CALL FTUKYJ(lun,'Y-OFFSET',Bbox(2)-1+(hbbox(2)+binh-1)/binh,
     &               'first original Y pixel in map',status)
 
         CALL FTUKYJ(lun,'totcts',Totpixh,'Total pixel count',status)

c If the WMAP fix was used then record the relation between image and detector
c coordinates

         IF ( wtmapfix .AND. 
     &        .NOT.( (Xcolf.EQ.Xcolh).AND.(Ycolf.EQ.Ycolh) ) ) THEN

            CALL FTUKYD(lun,'EXTXF0',wfxf0,15,'extractor X image zero',
     &                  status)
            CALL FTUKYD(lun,'EXTYF0',wfyf0,15,'extractor Y image zero',
     &                  status)
            CALL FTUKYD(lun,'EXTXH0',wfxh0,15,
     &                  'extractor X detector zero',status)
            CALL FTUKYD(lun,'EXTYH0',wfyh0,15,
     &                  'extractor Y detector zero',status)
            CALL FTUKYD(lun,'EXTXSIGN',xsign,15,
     &                  'extractor X orientation',status)
            CALL FTUKYD(lun,'EXTYSIGN',ysign,15,
     &                  'extractor Y orientation',status)
            CALL FTUKYD(lun,'EXTTHETA',wftheta,15,
     &                  'extractor rotation angle (radians)',status)
         ENDIF

c Write the REF* keywords which record the relation between image coordinates and
c the sky. This is potentially useful if the image is in detector coordinates.

         CALL w_refkeys(lun, status)
 
c Write history keywords specifying the columns used to create the WMAP

         histkey = 'WMAP X axis is '//xcolh
         CALL FTPHIS(lun, histkey, status)
         histkey = 'WMAP Y axis is '//ycolh
         CALL FTPHIS(lun, histkey, status)
 
      ENDIF
 
C     write the primary array of data:
 
      IF ( Buildwmap ) THEN
         CALL FTP2DE(lun,group,Iwmsz1,naxes(1),naxes(2),Wmap,status)
         contxt = 'Failed to write the WMAP image'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

      CALL XWARN1FITS(lun)
 
C Set the backscal factor. If no region selection then assume we
C are using the entire detector else backscal is set to the fraction
C of the detector selected.

      IF ( regionfile .EQ. ' ' ) THEN
         backscal = 1.0
      ELSE
         IF ( ((fbound(3)-fbound(1)+1)/binf)*
     &        ((fbound(4)-fbound(2)+1)/binf).EQ.0.0 ) THEN
            backscal = 0.0
         ELSE
            backscal = SNGL(Area/(((fbound(3)-fbound(1)+1)/binf)*
     &                        ((fbound(4)-fbound(2)+1)/binf)))
         ENDIF
      ENDIF

C Write the spectrum

      call wtpha2(lun, 10, 0, ' ', 0, ' ', telescope, instrument, ' ', 
     &            filter, '1.2.0', 'total', Chan(1), SNGL(Expos),
     &            .FALSE., 1.0, 'none', .FALSE., backscal, 'none', 
     &            1., 'none', 'none', Phasze, Ecol, Chan, Pha, 1, 
     &            .FALSE., 0., .FALSE., 0., .FALSE., 0, .FALSE., 0, 
     &            Phasze, status)
      contxt = 'Failure while writing the spectrum'
      IF ( status .NE. 0 ) GOTO 999

C Delete the EXPOSURE, HDUCLAS1, and HDUCLAS2 keywords since otherwise
C we will end up with two different copies of these after the XCOPYNOSCALE
C call. We will later update them to be correct. This is a hack - better
C would be to modify XCOPYNOSCALE so that these keywords are not copied.

      CALL FTDKEY(lun,'EXPOSURE',status)
      CALL FTDKEY(lun,'HDUCLAS1',status)
      CALL FTDKEY(lun,'HDUCLAS2',status)

C Copy over keywords from the input event extension

      CALL XCOPYNOSCALE(inlun,lun,status)
      CALL FTCLOS(inlun,status)
      CALL FRELUN(inlun)

C Get rid of any duplicate keywords

      CALL RMVDKY(lun, status)

C The standard keywords from the event files - this will overwrite
C values copied over above for duplicate keyword names

      CALL WSTDKY(lun, status)

C now reset the HDUCLAS1 and HDUCLAS2 keywords correctly

      CALL FTUKYS(lun,'HDUCLAS1','SPECTRUM',
     &            'PHA dataset (OGIP memo OGIP-92-007)',status)
      CALL FTUKYS(lun,'HDUCLAS2','TOTAL',
     &            'Gross PHA Spectrum (source + bkgd)',status)

C and set up some other useful keywords

      CALL FTUKYJ(lun,'totcts',Totpixh,'Total counts in spectrum',
     &            status)
 
      IF ( nompnt(1) .NE. -999.d0 )
     &      CALL FTUKYD(lun,'ra_pnt',nompnt(1),15,
     &                  'File average of RA(degrees)',status)
      IF ( nompnt(2) .NE. -999.d0 )
     &      CALL FTUKYD(lun,'dec_pnt',nompnt(2),15,
     &                  'File average of DEC(degrees)',status)
      IF ( nompnt(3) .NE. -999.d0 )
     &      CALL FTUKYD(lun,'pa_pnt',nompnt(3),15,
     &                  'File average of ROLL(degrees)', status)
 
c Keywords describing the spectral binning (used by SAS)

      CALL FTUKYJ(lun,'specdelt',specbin,'Binning factor for spectrum',
     &            status)
      CALL FTUKYJ(lun,'specpix',Chan(1),
     &            'The rebinned channel corresponding to SPECVAL',
     &            status)
      CALL FTUKYD(lun,'specval',(Chan(1)+(specbin-1)/2.0d0),
     &            15, 'Original channel value at center of SPECPIX',
     &            status)

c Write the data subspace keywords

      CALL W_DSKEYS(lun, status)


      CALL XWARN1FITS(lun)
 

c Write the GTI extensions
 
      IF ( Imaxccd .GT. 1 ) THEN
         DO i = 1, Imaxccd
            IF ( Qinreg(NINT(Gtis(3,1,i))) ) THEN
               CALL w_gti(Lun, Imaxgtis(i), MAXGTI, Gtis(1,1,i), 
     &                    Gtihdunms(i), Exposs(i), NINT(Gtis(3,1,i)), 
     &                    'GTI', Status)
            ENDIF
         ENDDO
      ELSE
         CALL w_gti(Lun, Imaxgtis(1), MAXGTI, Gtis(1,1,1), 
     &              Gtihdunms(1), Exposs(1), NINT(Gtis(3,1,1)), 
     &              'GTI', Status)
      ENDIF

c If a region file was defined then write a region extension

      CALL w_regions(Lun, Status)

c Close the file

      CALL xclsfl(lun,status)

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF
 
      RETURN
 
      END
