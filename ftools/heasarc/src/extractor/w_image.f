
C*==w_image.spg  processed by SPAG 4.50J  at 15:13 on  8 Mar 1995
 
c*******************************************************************
c     SUBROUTINE:
c     W_IMAGE
c
c     DESCRIPTION:
c     This subroutine writes a fits image file
c
c*******************************************************************
 
 
      SUBROUTINE W_IMAGE(Gtis, Imaxgtis, Imaxccd, Gtihdunms, Exposs, 
     &                   Qinreg, Imszf1, Imszf2, Imagef, Totpixf, 
     &                   Onefile)

      IMPLICIT NONE

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc' 
      INCLUDE 'keys.inc'
 
      INTEGER Imszf1, Imszf2
      DOUBLE PRECISION Gtis(3,MAXGTI,MAXCCD), Exposs(MAXCCD)
      REAL Imagef(Imszf1,Imszf2)
      INTEGER Imaxgtis(MAXCCD), Imaxccd, Totpixf
      CHARACTER Onefile*(*), Gtihdunms(MAXCCD)*(*)
      LOGICAL Qinreg(0:MAXCCD-1)

      DOUBLE PRECISION dtmp

      INTEGER lun, inlun, status, hdutype, blocksize
      INTEGER naxes(99) , i
      CHARACTER(255) comment
      CHARACTER(72) contxt
      CHARACTER(30) extname
      LOGICAL qexist
 
      character(40) taskname
      COMMON / task / taskname

      INTEGER LENACT
      EXTERNAL LENACT
 
      lun = 0
      dtmp = 0
      comment = ' '
      contxt = ' '
      DO i = 1 , 99
         naxes(i) = 0
      ENDDO
      
      status = 0
 
      IF ( Expos .GT. 0. ) THEN
         WRITE (comment,'(a,i8,a,1pg10.4,a)') ' Image            has ',
     &       Totpixf, ' counts for ', Totpixf/Expos, ' counts/sec'
      ELSE
         comment = ' Image has no exposure time'
      ENDIF
      CALL fcecho(comment) 

c     check for a valid image size

      IF ( Imszf1 .LE. 0 .OR. Imszf2 .LE. 0 ) THEN
         WRITE (comment,'(a,i8,a,i8)') 'Invalid size for image ',
     &                                 Imszf1, ' x ', Imszf2
         CALL fcecho(comment)
         RETURN
      ENDIF

c     ***********************************************
c     assign a unit number for the FITS file
 
      CALL GETLUN(lun)
 
      IF ( Imagefile.EQ.' ' ) RETURN

      INQUIRE(file=Imagefile(:lenact(Imagefile)),exist=qexist)
      IF ( qexist .AND. clobber ) 
     &         CALL delfil(Imagefile(:lenact(Imagefile))) 
      CALL FTINIT(lun,Imagefile,1,status)
 
C     Define the primary header
 
      naxes(1) = Imszf1
      naxes(2) = Imszf2
 
      CALL FTPHPR(lun,.TRUE.,32,2,naxes,0,1,.TRUE.,status)
      CALL FTRDEF(lun,status)

c Copy the event extension keywords into the header

      CALL GETLUN(inlun)
      CALL FTOPEN(inlun,Onefile,0,blocksize,status)
 
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

      CALL XCOPYNOSCALE(inlun,lun,status)
      contxt = 'Error copying header'
      IF ( status.NE.0 ) GOTO 999

      CALL FTCLOS(inlun,status)
      CALL FRELUN(inlun)

      CALL FTUKYS(lun,'CONTENT','IMAGE','image file ',status)
 
      CALL FTDKEY(lun,'hduname',status)
      IF ( status .NE. 0 ) status = 0
      CALL FTUKYS(lun,'hduname','IMAGE',' ',status)
      CALL FTUKYS(lun,'hduclass','ogip',
     &              'Format conforms to OGIP/GSFC conventions',status)
      CALL FTUKYS(lun,'hduclas1','IMAGE',
     &              'Extension contains an image',status)
      CALL FTPDAT(lun,status)

      CALL wstdky(lun, status)
      CALL FTUKYS(Lun,'creator',taskname,'Extractor',Status)
      CALL FTPDAT(Lun,status)
 
      CALL FTUKYD(lun,'RA_NOM',fcrval(1),15,
     &              'Right Ascension of target (deci. deg)',status)
      CALL FTUKYD(lun,'DEC_NOM',fcrval(2),15,
     &              'Declination of target (deci. deg)',status)
      IF ( detimage ) THEN 
         CALL FTUKYD(lun,'DROLLANG',nompnt(3),15,
     &               'Mean roll angle (deci. deg)',status)
      ELSE
         CALL FTUKYD(lun,'DROLLANG',0.d0,15,
     &               'Mean roll angle (deci. deg)',status)
      ENDIF         

      CALL FTUKYJ(lun,'IMGBIN',Binf,'Image binning factor',status)
      CALL UPC(Telescope)

c Write the DataModel keywords

      IF ( fmtype(1) .NE. ' ' ) THEN
         CALL FTPKYS(lun,'mtype1',fmtype(1),
     &               'DM Keyword: Descriptor name.', status)
         CALL FTPKYS(lun,'mform1',fmform(1),
     &               'DM Keyword: Descriptor value.', status)
      ENDIF
      IF ( fmtype(2) .NE. ' ' ) THEN
         CALL FTPKYS(lun,'mtype2',fmtype(2),
     &               'DM Keyword: Descriptor name.', status)
         CALL FTPKYS(lun,'mform2',fmform(2),
     &               'DM Keyword: Descriptor value.', status)
      ENDIF

c Write the data subspace keywords

      CALL W_DSKEYS(lun, status)

c Write the WCS coordinates

      CALL FTUKYS(lun,'ctype1',fctype(1),' ',status)
      IF ( fcname(1).NE.' ' ) CALL FTUKYS(lun,'cname1',fcname(1),
     &                                    ' ',status)
      dtmp = (fcrpix(1)-(fbbox(1)-1))/DBLE(Binf) 
     &         + DBLE(Binf-1)/DBLE(2*Binf)
      CALL FTUKYD(lun,'crpix1',dtmp,15,'X axis reference pixel',status)
      CALL FTUKYD(lun,'drpix1',fcrpix(1),15,
     &            'original X axis reference pixel',status)
      CALL FTUKYD(lun,'crval1',fcrval(1),15,
     &              'Coord of X ref pixel',status)
      CALL FTUKYD(lun,'cdelt1',fcrdelt(1)*DBLE(Binf),15,
     &              'X axis increment',status)
      CALL FTUKYD(lun,'ddelt1',fcrdelt(1),15,
     &              'original X axis increment',status)

      CALL FTUKYS(lun,'ctype2',fctype(2),'projection',status)
      IF ( fcname(2).NE.' ' ) CALL FTUKYS(lun,'cname2',fcname(2),
     &                                    ' ',status)
      dtmp = (fcrpix(2)-(fbbox(2)-1))/DBLE(Binf) 
     &         + DBLE(Binf-1)/DBLE(2*Binf)
      CALL FTUKYD(lun,'crpix2',dtmp,15,'Y axis reference pixel',status)
      CALL FTUKYD(lun,'drpix2',fcrpix(2),15,
     &            'original Y axis reference pixel',status)
      CALL FTUKYD(lun,'crval2',fcrval(2),15,
     &              'Coord of Y ref pixel',status)
      CALL FTUKYD(lun,'cdelt2',fcrdelt(2)*DBLE(Binf),15,
     &              'Y axis increment',status)
      CALL FTUKYD(lun,'ddelt2',fcrdelt(2),15,
     &              'original Y axis increment',status)

      IF ( fcrota .GT. -999.D0 ) THEN 
         CALL FTUKYD(lun,'crota2',fcrota,15,'Sky coord rotation angle',
     &               status)
      ENDIF

      IF ( foptic(1).GT.-999.D0 ) THEN
         dtmp = (foptic(1)-(fbbox(1)-1))/DBLE(Binf) 
     &         + DBLE(Binf-1)/DBLE(2*Binf)
         CALL FTUKYD(lun,'optic1',dtmp,15,'X Optical axis',
     &               status)
         dtmp = (foptic(2)-(fbbox(2)-1))/DBLE(Binf) 
     &         + DBLE(Binf-1)/DBLE(2*Binf)
         CALL FTUKYD(lun,'optic2',dtmp,15,'Y Optical axis',
     &               status)
      ENDIF

c Set physical WCS coordinates. Includes the LTV and LTM keywords written
c out by dmextract although I don't think these are standard WCS.

      CALL FTUKYS(lun,'wcsnamep','PHYSICAL',' ',status)

      CALL FTUKYS(lun,'wcsty1p','PHYSICAL',' ',status)
      dtmp = (1-(fbbox(1)-1))/DBLE(Binf) 
     &      + DBLE(Binf-1)/DBLE(2*Binf) - 1.0d0/binf
      CALL FTUKYD(lun,'ltv1',dtmp,15,
     &            'offset for logical to physical pixel',status)
      dtmp = 1.0d0/binf
      CALL FTUKYD(lun,'ltm1_1',dtmp,15,
     &            'slope for logical to physical pixel',status)
      CALL FTUKYS(lun,'ctype1p',xcolf,'Source of X-axis',status)
      CALL FTUKYD(lun,'crpix1p',1.0d0,15,'X axis reference pixel',
     &            status)
      dtmp = (fbbox(1)-1) + (Binf+1)/2.
      CALL FTUKYD(lun,'crval1p',dtmp,15,
     &            'coord of X ref pixel in original image',status)
      dtmp = binf
      CALL FTUKYD(lun,'cdelt1p',dtmp,15,'X axis increment',
     &            status)

      CALL FTUKYS(lun,'wcsty2p','PHYSICAL',' ',status)
      dtmp = (1-(fbbox(2)-1))/DBLE(Binf) 
     &      + DBLE(Binf-1)/DBLE(2*Binf) - 1.0d0/binf
      CALL FTUKYD(lun,'ltv2',dtmp,15,
     &            'offset for logical to physical pixel',status)
      dtmp = 1.0d0/binf
      CALL FTUKYD(lun,'ltm2_2',dtmp,15,
     &            'slope for logical to physical pixel',status)
      CALL FTUKYS(lun,'ctype2p',ycolf,'Source of Y-axis',status)
      CALL FTUKYD(lun,'crpix2p',1.0d0,15,'Y axis reference pixel',
     &               status)
      dtmp = (fbbox(2)-1) + (Binf+1)/2.
      CALL FTUKYD(lun,'crval2p',dtmp,15,
     &            'coord of Y ref pixel in original image', status)
      dtmp = binf
      CALL FTUKYD(lun,'cdelt2p',dtmp,15,'Y axis increment',
     &            status)

      IF ( status.NE.0 ) CALL EXTERRSTACK
 
      CALL FTUKYJ(lun,'totcts',Totpixf,'Total pixel count',status)

c Set the keywords specifying the energy range for which the image was
c produced

      IF ( needener ) THEN

         CALL FTUKYJ(lun,'CHANMIN',ebound(1),'Low energy channel',
     &               status)
         CALL FTUKYJ(lun,'CHANMAX',ebound(2),'High energy channel',
     &               status)
         CALL FTUKYS(lun,'CHANTYPE',Ecol,'Energy channel name',status)

      ENDIF

c Find out whether one of the cuts was in PHA/PI and if so modify
c the CHANMIN, CHANMAX keywords

      DO i = 1, nkeys
         IF ( key(i) .EQ. 'PHA' ) THEN
            CALL FTUKYJ(lun,'CHANMIN',NINT(keyval(1,i)),
     &                  'Low PHA channel',status)
            CALL FTUKYJ(lun,'CHANMAX',NINT(keyval(2,i)),
     &                  'High PHA channel',status)
            CALL FTUKYS(lun,'CHANTYPE','PHA','Energy channel name',
     &                  status)
         ELSEIF ( key(i) .EQ. 'PI' ) THEN
            CALL FTUKYJ(lun,'CHANMIN',NINT(keyval(1,i)),
     &                  'Low PI channel',status)
            CALL FTUKYJ(lun,'CHANMAX',NINT(keyval(2,i)),
     &                  'High PI channel',status)
            CALL FTUKYS(lun,'CHANTYPE','PI','Energy channel name',
     &                  status)
         ENDIF
      ENDDO

      CALL XWARN1FITS(lun)

c Now write the image
 
      CALL FTP2DE(lun,0,Imszf1,naxes(1),naxes(2),Imagef,status)

c Tack on the GTI extensions

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

      CALL xclsfl(Lun,Status)


 999  CONTINUE
 
      IF ( status.NE.0 ) THEN
         CALL FTGERR(status,contxt)
         CALL fcerr(' '//contxt)
      ENDIF
 
      RETURN
      END
