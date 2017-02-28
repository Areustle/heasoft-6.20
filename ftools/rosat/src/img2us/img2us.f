***************************************************************************
C SELECTOR TASK:
C      img2us
C
C FILE:
C      img2us.f 
C
C DESCRIPTION: 
C
c Program to read German ROSAT FITS files and write new FITS files with
c keywords adjusted such that they can be used with HEASARC data analysis
c program XIMAGE.  P. Tyler August 1993
C
C AUTHOR:  
C       Pat Tyler
C       August, 1993
CC
C MODIFICATION HISTORY:
C       3/17/94 EAG 1.0 Made into an FTOOL
C       4/18/94 EAG 1.1 Filter in changes to keywords
C       9/7/94  EAG 1.2 Allow for varied input filenames
C       1/10/95 LEB 1.3 added BN to numeric internal read formats,
C                       changed decsec to a double precision variable and
C                       fixed internal read formats accordingly,
C                       added return statement
C       1/25/95 LEB 1.3.1 changed some internal list directed reads to 
C                         explicit format
C       6/30/98 NG  1.3.2 The data-obs and date-end  are  writen                
C                         in new format yyyy-mm-dd.
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C***************************************************************************
      subroutine img2us
      implicit none
c
      INTEGER MAXDIM
      PARAMETER (MAXDIM=99)

      LOGICAL simple , extend , blocked , notend
      INTEGER luin , lout , block , length, fstatus
      INTEGER*2 hours , minutes , degrees , found 
      INTEGER ii , jj , kk , FCSTLN
      INTEGER status , naxes(MAXDIM) , pcount , gcount
      INTEGER bitpix , naxis , minch , maxch, position

      double precision equinox , crota1 , crota2
      double precision  seconds , rminch , rmaxch, aminch, amaxch
      double precision crpix1 , crpix2
      double precision crval1 , crval2 , cdelt1 , cdelt2
      double precision decsec

      CHARACTER sign
      character(2) daynum1 , monthnum1 , yearnum1
      character(2) daynum2 , monthnum2 , yearnum2
      character(3) monthstr1 , monthstr2, temp
      character(5) filter
      character(7) keyword
      character(8) telescop , instrume , radecsys
      character(68) time_obs , date_obs , time_end , date_end
      character(16) obs_id
      character(20) bunit , ctype1 , ctype2 , origin , object
      character(80) comment , crdstr , card, context
      character(70) history
      character(160) infile , outfile

      character(40) taskname
      common /task/ taskname

      taskname = 'img2us1.2'
      luin = 15
      lout = 16
      status = 0
      fstatus = 0
      kk =0
c
c Get the names of the German FITS files from parameter file
c
      infile = ' '
      call uclgst ('infile', infile, status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr (context)
         goto 99999
      endif
      call frmblk (infile)

c
c Set name for new FITS file
c
      position = max(index(infile, '_IMAGE1.'),
     &     index(infile, '_image1.'))
      if (position .gt. 0) outfile = infile(1:position) // 'im1.fits'

      position = max(index(infile, '_IMAGE2.'),
     &     index(infile, '_image2.'))
      if (position .gt. 0) outfile = infile(1:position) // 'im2.fits'

      position = max(index(infile, '_IMAGE3.'),
     &     index(infile, '_image3.'))
      if (position .gt. 0) outfile = infile(1:position) // 'im3.fits'

      position = max(index(infile, '_MEXMAP.'),
     &     index(infile, '_mexmap.'))
      if (position .gt. 0) outfile = infile(1:position) // 'mex.fits'

      position = max(index(infile, '_IMAGEEC.'),
     &     index(infile, '_imageec.'))
      if (position .gt. 0) outfile = infile(1:position) //
     &     'imageec.fits'

c      IF (( infile(10:15).EQ.'IMAGE1' ).OR.
c     &    ( infile(10:15).EQ.'image1' )) THEN
c         outfile = infile(1:9) // 'im1.fits'
c      ELSEIF (( infile(10:15).EQ.'IMAGE2' ).OR.
c     &        ( infile(10:15).EQ.'image2' )) THEN
c         outfile = infile(1:9) // 'im2.fits'
c      ELSEIF (( infile(10:15).EQ.'IMAGE3' ).OR.
c     &        ( infile(10:15).EQ.'image3' )) THEN
c         outfile = infile(1:9) // 'im3.fits'
c      ELSEIF (( infile(10:15).EQ.'MEXMAP' ).OR.
c     &        ( infile(10:15).EQ.'mexmap' )) THEN
c         outfile = infile(1:9) // 'mex.fits'
c      ELSEIF (( infile(10:16).EQ.'IMAGEEC' ).OR.
c     &        ( infile(10:16).EQ.'imageec' )) THEN
c         outfile = infile(1:9) // 'imageec.fits'
c      ENDIF
c
c Open the German FITS file for reading
c
      CALL FTOPEN(luin,infile,0,block,status)
      IF ( status.NE.0 ) THEN
         context = ' Unable to open infile  ' // infile
         call fcerr (context)
         CALL FTCLOS(luin,fstatus)
         GOTO 99999
      ENDIF
c
c Initialize the new FITS file for writing
c
      CALL FTINIT(lout,outfile,2880,status)
      if (status .ne. 0) then
         context = ' Error opening output file, may exist?'
         call fcerr (context)
         call ftclos (luin, fstatus)
         goto 99999
      endif
c
c Get keywords from the old FITS header and copy to new FITS header
c
      CALL FTGHPR(luin,MAXDIM,simple,bitpix,naxis,naxes,pcount,gcount,
     &     extend,status)
      CALL FTPHPR(lout,simple,bitpix,naxis,naxes,pcount,gcount,extend,
     &     status)
      if (status .ne. 0) then
         context = ' Error copying primary array keywords'
         call fcecho (context)
      endif
c
c Get keywords from the old primary header which will remain the same and
c copy them to new primary header
c
      CALL FTGKYL(luin,'BLOCKED',blocked,comment,status)
      CALL FTPKYL(lout,'BLOCKED',blocked,comment,status)
c
      CALL FTGKYS(luin,'ORIGIN',origin,comment,status)
      CALL FTPKYS(lout,'ORIGIN',origin,comment,status)
c
      CALL FTGKYS(luin,'BUNIT',bunit,comment,status)
      CALL FTPKYS(lout,'BUNIT',bunit,comment,status)
c
c Hardwire these keywords into the FITS file to be written
c
      CALL FTGKYD(luin,'CRPIX1',crpix1,comment,status)
      crpix1 = 2.561666E2
      CALL FTPKYD(lout,'CRPIX1',crpix1,6,comment,status)
c
      CALL FTGKYD(luin,'CRPIX2',crpix2,comment,status)
      crpix2 = 2.561666E2
      CALL FTPKYD(lout,'CRPIX2',crpix2,6,comment,status)
c
      CALL FTGKYD(luin,'CDELT1',cdelt1,comment,status)
      cdelt1 = -4.166666666E-3
      CALL FTPKYD(lout,'CDELT1',cdelt1,9,comment,status)
c
      CALL FTGKYD(luin,'CDELT2',cdelt2,comment,status)
      cdelt2 = 4.166666666E-3
      CALL FTPKYD(lout,'CDELT2',cdelt2,9,comment,status)
c
c These are new header keywords and some need comments
c
      ctype1 = 'RA---TAN'
      comment = 'Units of coordinate'
      CALL FTPKYS(lout,'CTYPE1',ctype1,comment,status)
c
      ctype2 = 'DEC--TAN'
      comment = 'Units of coordinate'
      CALL FTPKYS(lout,'CTYPE2',ctype2,comment,status)
c
      crota1 = 0.000
      comment = ' '
      CALL FTPKYD(lout,'CROTA1',crota1,3,comment,status)
c
      crota2 = 0.000
      comment = ' '
      CALL FTPKYD(lout,'CROTA2',crota2,3,comment,status)
c
      equinox = 2.000E3
      CALL FTPKYD(lout,'EQUINOX',equinox,4,
     &     'equinox of celestial coord. system',status)
c
      radecsys = 'FK5'
      CALL FTPKYS(lout,'RADECSYS',radecsys,'FK5 coordinate system used',
     &     status)

      if (status .ne. 0) then
         context = ' Error copying keywords'
         call fcerr (context)
         goto 999
      endif
c
c Now start reading in HISTORY block and getting information for
c other new keywords
c
      found = 0
      keyword = 'HISTORY'
      DO WHILE ( found.LT.13)
         
         CALL FTGCRD(luin,keyword,card,status)
c
         IF ( card(11:20).EQ.'MISSION_ID' ) THEN
            telescop = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:14),'(a)') telescop
            CALL FTPKYS(lout,'TELESCOP',telescop,
     &           'Telescope (mission) name',status)
            found = found + 1
c
c Instrument changed to add letter to 'PSPC' as 'PSPCB' 4/18/94
c
         ELSEIF ( card(11:21).EQ.'DETECTOR_ID' ) THEN
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:14),'(a)') instrume
            CALL FTPKYS(lout,'INSTRUME',instrume,
     &           'instrument used for observation',status)
            found = found + 1
c
c New keyword FILTER added 4/18/94
c
         ELSEIF ( card(11:19).EQ.'FILTER_ID' ) THEN
            temp = ' '
            filter = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:12),'(a)') temp
            IF (temp.EQ.'OFF') THEN
               filter = 'NONE'
            ELSEIF (temp.EQ.'ON ') THEN
               filter = 'BORON'
            ELSEIF (temp.EQ.'BOR') THEN
               filter = 'BORON'
            ENDIF
            CALL FTPKYS(lout,'FILTER',filter,
     &                  'filter id: NONE OR BORON',status)
            found = found + 1
c
         ELSEIF ( card(11:16).EQ.'OBS_ID' ) THEN
            obs_id = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:20),'(a)') obs_id
            comment = ' '
            CALL FTPKYS(lout,'XS-OBSID',obs_id,comment,status)
            found = found + 1
c
         ELSEIF ( card(11:20).EQ.'POINT_LONG' ) THEN
            CALL FTGCRD(luin,keyword,card,status)
            length = FCSTLN(card)
            DO 20 ii = 10 , length
               IF ( card(ii:ii).EQ.'H' ) THEN
                  jj = ii - 1
                  READ (card(10:jj),'(BN,I2)') hours
                  kk = ii + 1
               ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                  jj = ii - 1
                  READ (card(kk:jj),'(BN,I2)') minutes
                  kk = ii + 1
               ELSEIF ( card(ii:ii).EQ.'S' ) THEN
                  jj = ii - 1
                  READ (card(kk:jj),'(BN,g80.0)') seconds
               ENDIF
 20         CONTINUE
c
c Turn hours, minutes, seconds into decimal representation
c
            crval1 = (((hours)+(minutes/60.D0)+(seconds/3600.D0))/24.D0)
     &           *360.D0
c
c Add new keyword CRVAL1 taken from POINT_LONG
c
            comment = ' '
            CALL FTPKYD(lout,'CRVAL1',crval1,8,comment,status)
            found = found + 1
c
         ELSEIF ( card(11:19).EQ.'POINT_LAT' ) THEN
            sign = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:10),'(a)') sign
            IF ( sign.EQ.'-' ) THEN
               length = FCSTLN(card)
               DO 30 ii = 11 , length
                  IF ( card(ii:ii).EQ.'D' ) THEN
                     jj = ii - 1
                     READ (card(10:jj),'(BN,I2)') degrees
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,I2)') minutes
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'S' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,g80.0)') decsec
                  ENDIF
 30            CONTINUE
            ELSEIF ( sign.NE.'-' ) THEN
               sign = '+'
               length = FCSTLN(card)
               DO 40 ii = 10 , length
                  IF ( card(ii:ii).EQ.'D' ) THEN
                     jj = ii - 1
                     READ (card(10:jj),'(BN,I2)') degrees
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'M' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,I2)') minutes
                     kk = ii + 1
                  ELSEIF ( card(ii:ii).EQ.'S' ) THEN
                     jj = ii - 1
                     READ (card(kk:jj),'(BN,g80.0)') decsec
                  ENDIF
 40            CONTINUE
            ENDIF
c
c Turn degrees, minutes, seconds into decimal representation
c
            crval2 = abs(degrees) + (minutes+decsec/60.D0)/60.D0
            IF ( sign.EQ.'-' ) crval2 = crval2*(-1.)
c
c Add new keyword CRVAL2 taken from POINT_LAT
c
            comment = ' '
            CALL FTPKYD(lout,'CRVAL2',crval2,8,comment,status)
            found = found + 1
c
         ELSEIF ( card(11:19).EQ.'OBS_TITLE' ) THEN
            object = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:25),'(a)') object
            CALL FTPKYS(lout,'OBJECT',object,'name of observed object',
     &           status)
            found = found + 1
c
         ELSEIF ( card(11:18).EQ.'OBS_DATE' ) THEN
            daynum1 = ' '
            monthstr1 = ' '
            yearnum1 = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:11),'(a)') daynum1
            READ (card(13:15),'(a)') monthstr1
            IF ( monthstr1.EQ.'JAN' ) THEN
               monthnum1 = '01'
            ELSEIF ( monthstr1.EQ.'FEB' ) THEN
               monthnum1 = '02'
            ELSEIF ( monthstr1.EQ.'MAR' ) THEN
               monthnum1 = '03'
            ELSEIF ( monthstr1.EQ.'APR' ) THEN
               monthnum1 = '04'
            ELSEIF ( monthstr1.EQ.'MAY' ) THEN
               monthnum1 = '05'
            ELSEIF ( monthstr1.EQ.'JUN' ) THEN
               monthnum1 = '06'
            ELSEIF ( monthstr1.EQ.'JUL' ) THEN
               monthnum1 = '07'
            ELSEIF ( monthstr1.EQ.'AUG' ) THEN
               monthnum1 = '08'
            ELSEIF ( monthstr1.EQ.'SEP' ) THEN
               monthnum1 = '09'
            ELSEIF ( monthstr1.EQ.'OCT' ) THEN
               monthnum1 = '10'
            ELSEIF ( monthstr1.EQ.'NOV' ) THEN
               monthnum1 = '11'
            ELSEIF ( monthstr1.EQ.'DEC' ) THEN
               monthnum1 = '12'
            ENDIF
            READ (card(19:20),'(a)') yearnum1
C            date_obs = daynum1 // '/' // monthnum1 // '/' // yearnum1
            date_obs = '19'//yearnum1//'-'//monthnum1 //
     *                  '-' //daynum1
            daynum2 = ' '
            monthstr2 = ' '
            yearnum2 = ' '
            READ (card(22:23),'(a)') daynum2
            READ (card(25:27),'(a)') monthstr2
            IF ( monthstr2.EQ.'JAN' ) THEN
               monthnum2 = '01'
            ELSEIF ( monthstr2.EQ.'FEB' ) THEN
               monthnum2 = '02'
            ELSEIF ( monthstr2.EQ.'MAR' ) THEN
               monthnum2 = '03'
            ELSEIF ( monthstr2.EQ.'APR' ) THEN
               monthnum2 = '04'
            ELSEIF ( monthstr2.EQ.'MAY' ) THEN
               monthnum2 = '05'
            ELSEIF ( monthstr2.EQ.'JUN' ) THEN
               monthnum2 = '06'
            ELSEIF ( monthstr2.EQ.'JUL' ) THEN
               monthnum2 = '07'
            ELSEIF ( monthstr2.EQ.'AUG' ) THEN
               monthnum2 = '08'
            ELSEIF ( monthstr2.EQ.'SEP' ) THEN
               monthnum2 = '09'
            ELSEIF ( monthstr2.EQ.'OCT' ) THEN
               monthnum2 = '10'
            ELSEIF ( monthstr2.EQ.'NOV' ) THEN
               monthnum2 = '11'
            ELSEIF ( monthstr2.EQ.'DEC' ) THEN
               monthnum2 = '12'
            ENDIF
            READ (card(31:32),'(a)') yearnum2
C            date_end = daynum2 // '/' // monthnum2 // '/' // yearnum2
            date_end = '19'//yearnum2//'-'// monthnum2//
     *                 '-' //daynum2
            comment = ' '
            CALL FTPKYS(lout,'DATE-OBS',date_obs,comment,status)
            CALL FTPKYS(lout,'DATE-END',date_end,comment,status)
            found = found + 1
c
         ELSEIF ( card(11:16).EQ.'OBS_UT' ) THEN
            time_obs = ' '
            time_end = ' '
            CALL FTGCRD(luin,keyword,card,status)
            READ (card(10:17),'(a)') time_obs
            READ (card(26:33),'(a)') time_end
            length = FCSTLN(time_obs)
            DO 60 ii = 1 , length
               IF ( time_obs(ii:ii).EQ.' ' ) THEN
                  time_obs(ii:ii) = '0'
               ENDIF
               length = FCSTLN(time_end)
 60         CONTINUE
            DO 80 ii = 1 , length
               IF ( time_end(ii:ii).EQ.' ' ) THEN
                  time_end(ii:ii) = '0'
               ENDIF
 80         CONTINUE
            comment = ' '
            CALL FTPKYS(lout,'TIME-OBS',time_obs,comment,status)
            CALL FTPKYS(lout,'TIME-END',time_end,comment,status)
            found = found + 1
c
c ONTIME and XS-LIVTI keywords removed since they cannot be calculated
c from the German headers alone  4/18/94
c
c         ELSEIF ( card(11:21).EQ.'OBS_DUR_SEC' ) THEN
c            CALL FTGCRD(luin,keyword,card,status)
c            length = LENACT(card)
c            crdstr = card(10:length)
c            READ (crdstr,*) ontime
c            comment = ' '
c            CALL FTPKYD(lout,'ONTIME',ontime,6,comment,status)
c            CALL FTPKYD(lout,'XS-LIVTI',ontime,6,comment,status)
c            found = found + 2
c
         ELSEIF ( card(11:17).EQ.'RAW_SEL' ) THEN
            CALL FTGCRD(luin,keyword,card,status)
            length = FCSTLN(card)
            crdstr = card(10:length)
            READ (crdstr,*) rminch , rmaxch
            found = found + 2
         ELSEIF ( card(11:17).EQ.'AMP_SEL' ) THEN
            CALL FTGCRD(luin,keyword,card,status)
            length = FCSTLN(card)
            crdstr = card(10:length)
            READ (crdstr,*) aminch , amaxch
            found = found + 2
         ENDIF
      ENDDO
      if(aminch.eq.0.and.amaxch.eq.0) then
         minch = rminch
         maxch = rmaxch
      else
         minch = aminch
         maxch = amaxch
      endif
      comment = ' '
      CALL FTPKYJ(lout,'XS-MINCH',minch,comment,status)
      CALL FTPKYJ(lout,'XS-MAXCH',maxch,comment,status)
c
c After all new keywords are taken from the HISTORY block, go back and copy
c over all of the HISTORY information exactly as in the original file.
c
c First reset pointer to top of file
c
      CALL FTGKYL(luin,'SIMPLE',simple,comment,status)
c
      notend = .TRUE.
      DO WHILE ( notend )
         CALL FTGCRD(luin,keyword,card,status)
         IF ( card(10:30).EQ.' ' ) THEN
            history = ' '
         ELSE
            length = FCSTLN(card)
            history = card(10:length)
         ENDIF
         CALL FTPHIS(lout,history,status)
         IF ( card(10:28).EQ.'ESO-DESCRIPTORS END' ) THEN
            notend = .FALSE.
         ENDIF
      ENDDO
c
c Now copy the data over all in one move.
c
      CALL FTCPDT(luin,lout,status)
c
c Close infile and outfile
c
 999  if (status .ne. 0) call fcerrm (status)
      fstatus = 0
      CALL FTCLOS(luin,fstatus)
c
      fstatus = 0
      CALL FTCLOS(lout,fstatus)
c
99001 FORMAT (a40)
99002 FORMAT (a32,i4)
      return
99999 END
