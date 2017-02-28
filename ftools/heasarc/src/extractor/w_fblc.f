C*==w_fblc.spg  processed by SPAG 4.50J  at 15:13 on  8 Mar 1995
c*******************************************************************
c     SUBROUTINE:
c     W_fblc
c
c     DESCRIPTION:
c     This subroutine writes a fits binned light curve file
c
c*******************************************************************
 
      SUBROUTINE W_FBLC(Gti,Imaxgti,Gtis,Imaxgtis,Imaxccd,Gtihdunms,
     &                  Exposs,Qinreg,Status,Imaxlc,Lc,Lcsize,Mintime1,
     &                  Onefile,Area)

      IMPLICIT NONE

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'
 
      INTEGER Imaxlc , Lcsize
      INTEGER Lc(Imaxlc)
      DOUBLE PRECISION Gti(3,MAXGTI), Gtis(3,MAXGTI,MAXCCD), Mintime1
      DOUBLE PRECISION Exposs(MAXCCD)
      DOUBLE PRECISION Area
      INTEGER Imaxgti, Imaxgtis(MAXCCD), Imaxccd, Status
      CHARACTER*(*) Onefile, Gtihdunms(MAXCCD)
      LOGICAL Qinreg(0:MAXCCD-1)

 
      DOUBLE PRECISION binval , binval0, binsize
      DOUBLE PRECISION avglcbin, dtmp

      REAL rtmp

      INTEGER totphot , binnum, ierr
      INTEGER lun , inlun , blocksize
      INTEGER i , outbins , hdutype
      INTEGER bitpix , naxis , naxes(100) , pcount , gcount
      INTEGER photons , totbin , tfields

      character(30) ttype(100)
      character(10) tform(100)
      character(20) tunit(100)
      CHARACTER(255) comment, ctmp
      CHARACTER(50) tmpstr
      CHARACTER(30) extname
      character(80) inline, contxt

      LOGICAL simple , extend
      LOGICAL thwarn, qexist

      DOUBLE PRECISION CALCBINSIZE
      INTEGER LENACT
      EXTERNAL CALCBINSIZE, LENACT 
 
      character(40) taskname
      COMMON / task / taskname

      binval = 0.0
      binval0 = 0.0
      binnum = 0
      binsize = 0.0
      lun = 0
      inlun = 0
      blocksize = 0
      i = 0
      outbins = 0
      hdutype = 0
      tfields = 0
      extname = ' '
      DO 100 i = 1 , 100
         ttype(i) = ' '
         tform(i) = ' '
         tunit(i) = ' '
         naxes(i) = 0
 100  CONTINUE
      i = 0
      bitpix = 0
      naxis = 0
      pcount = 0
      gcount = 0
      photons = 0
      totbin = 0
      comment = ' '
      tmpstr = ' '
      inline = ' '
 
 
      IF ( Fitsbinlc.EQ.' ' ) RETURN
 
      photons = 0
      DO i = 1 , Lcsize
         photons = photons + Lc(i)
      ENDDO

      IF ( Expos .GT. 0. ) THEN
         WRITE (comment,'(a,i8,a,1pg10.4,a)') ' Fits light curve has ',
     &       photons, ' counts for ', photons/Expos, ' counts/sec'
      ELSE
         comment = ' Fits light curve has no exposure time'
      ENDIF
      CALL fcecho(comment) 
 
      Status = 0
 
C     ********************************************************

      CALL GETLUN(lun)
      INQUIRE(file=Fitsbinlc(:lenact(Fitsbinlc)),exist=qexist)
      IF ( qexist .AND. clobber ) 
     &         CALL delfil(Fitsbinlc(:lenact(Fitsbinlc))) 
      CALL FTINIT(lun,Fitsbinlc,1,Status)
      contxt = 'Failed to open '//Fitsbinlc(:len(contxt)-15)
      IF ( Status .NE. 0 ) GOTO 999
 
      simple = .TRUE.
      bitpix = 32
      naxis = 0
      pcount = 0
      gcount = 1
      extend = .TRUE.
 
C     write the required primary array keywords:
 
      CALL FTPHPR(lun,simple,bitpix,naxis,naxes,pcount,gcount,extend,
     &            Status)
      CALL FTRDEF(lun,Status)
      contxt = 'Failed to write primary array keywords'
      IF ( Status .NE. 0 ) GOTO 999
 
C Copy keywords from the input file event extension
 
      CALL GETLUN(inlun)
      CALL FTOPEN(inlun,Onefile,0,blocksize,Status)
 
      i = 2
      CALL UPC(Eventname)
      extname = ' '
      DO WHILE ( Status.EQ.0 .AND. extname.NE.Eventname )
         CALL FTMAHD(inlun,i,hdutype,Status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGKYS(inlun,'EXTNAME',extname,comment,Status)
            CALL UPC(extname)
         ENDIF
         i = i + 1
      ENDDO
      Status = 0
      CALL FTCLOS(inlun,Status)
      Status = 0
      CALL FRELUN(inlun)

C Write out the standard keywords
 
      CALL wstdky(lun, Status)
      CALL FTUKYS(lun,'creator',taskname,'Extractor version',status)
      CALL FTPDAT(lun,status)

 
      CALL FTDKEY(lun,'TIMEFRAM',Status)
      Status = 0
 
      CALL FTUKYD(lun,'TIMEDEL',Binlc,15,'Binning factor',Status)
 
C     define primary array structure:
 
      CALL XWARN1FITS(lun)
 
      CALL FTPDEF(lun,bitpix,naxis,naxes,pcount,gcount,Status)

c Create light curve extension

      CALL FTCRHD(lun,Status)
      contxt = 'Failed to create light curve extension'
      IF ( Status.NE.0 ) GOTO 999
 
C     Initialize the number of rows and define
c     the fields....
 
      tfields = 4
 
      ttype(1) = 'TIME'
      tform(1) = 'D'
      tunit(1) = timeunit(:lenact(timeunit))
 
      ttype(2) = 'RATE'
      tform(2) = 'E'
      tunit(2) = 'count/'//timeunit(:lenact(timeunit))
 
      ttype(3) = 'ERROR'
      tform(3) = 'E'
      tunit(3) = 'count/'//timeunit(:lenact(timeunit))
 
      ttype(4) = 'FRACEXP'
      tform(4) = 'E'
      tunit(4) = ' '
 
      outbins = 0
 
      CALL FTPHBN(lun,outbins,tfields,ttype,tform,tunit,'RATE',0,Status)
      CALL FTRDEF(lun,Status)
      contxt = 'Failed to initialize light curve extension'
      IF ( Status.NE.0 ) GOTO 999
 
C Copy keywords from the input file event extension
 
      CALL GETLUN(inlun)
      blocksize = 0
      CALL FTOPEN(inlun,Onefile,0,blocksize,Status)
      extname = ' '
      i = 2
      CALL UPC(Eventname)
      DO WHILE ( Status.EQ.0 .AND. extname.NE.Eventname )
         CALL FTMAHD(inlun,i,hdutype,Status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGKYS(inlun,'EXTNAME',extname,comment,Status)
            CALL UPC(extname)
         ENDIF
         i = i + 1
      ENDDO
      CALL XCOPYNOSCALE(inlun,lun,Status)
      Status = 0
      CALL FTCLOS(inlun,Status)
      Status = 0
      CALL FRELUN(inlun)
 
      CALL FTUKYS(lun,'hduclass','ogip',
     &              'Format conforms to OGIP/GSFC conventions',Status)
      CALL FTUKYS(lun,'hduclas1','LIGHTCURVE',
     &              'Extension contains a light curve',Status)
      CALL FTUKYS(lun,'hduclas2','TOTAL',' ',Status)
      CALL FTUKYS(lun,'hduclas3','RATE',' ',Status)

      CALL wstdky(lun, Status)
 
      dtmp = 0.0d0
      IF ( lctzero ) dtmp = Binlc/2.D0 + Mintime1
      CALL FTUKYD(lun,'TIMEZERO',dtmp,15,'Time Zero',Status)
      CALL FTUKYD(lun,'TIMEDEL',Binlc,15,'Binning factor',Status)
      CALL FTUKYD(lun,'TIMEPIXR',0.5d0,15,
     &            'Timestamps give center of bin',Status)

c Write out info about the energy band if an Ecol was set

      IF ( needener ) THEN
         tmpstr = 'Minimum '//Ecol(1:LENACT(Ecol))//' channel'
         CALL FTUKYJ(lun,'Phalcut',ebound(1),tmpstr,Status)
         tmpstr = 'Maximum '//Ecol(1:LENACT(Ecol))//' channel'
         CALL FTUKYJ(lun,'Phahcut',ebound(2),tmpstr,Status)

c If a pha/pi cut was given as an event filename argument then modify
c the phalcut and phahcut keywords.

         DO i = 1, nkeys
            IF ( key(i) .EQ. Ecol ) THEN
               tmpstr = 'Minimum '//Ecol(1:LENACT(Ecol))//' channel'
               CALL FTUKYJ(lun,'Phalcut',NINT(keyval(1,i)),tmpstr,
     &                     Status)
               tmpstr = 'Maximum '//Ecol(1:LENACT(Ecol))//' channel'
               CALL FTUKYJ(lun,'Phahcut',NINT(keyval(2,i)),tmpstr,
     &                     Status)
            ENDIF
         ENDDO
      ENDIF

 
      CALL FTUKYE(lun,'NPIXSOU',SNGL(Area),10,'Numbers of Pixels',
     &            Status)

      CALL FTUKYD(lun,'MINFREXP',Lcthresh,10,
     &            'Minimum value of FRACEXP included',Status)

c Write the data subspace keywords

      CALL W_DSKEYS(lun, Status)
 
C copy the region file into the comment section
C
      IF ( Regionfile.NE.' ' ) THEN
         CALL FTPCOM(lun,'Region file selection ',Status)
         CALL GETLUN(inlun)
         ctmp = Regionfile
         CALL OPENWR(inlun,ctmp,'OLD',' ',' ',0,1,ierr)
         IF ( ierr.EQ.0 ) THEN
            READ (inlun,'(a)',IOSTAT=ierr) inline
            DO WHILE ( ierr.EQ.0 )
               CALL FTPCOM(lun,inline,Status)
               READ (inlun,'(a)',IOSTAT=ierr) inline
            ENDDO
            CLOSE (inlun)
         ELSE
            inline = ' Error reopening region file: '//
     &               Regionfile(1:LENACT(Regionfile))
            CALL XWARN1(inline,5)
         ENDIF
         CALL FRELUN(inlun)
      ENDIF
 
      CALL XWARN1FITS(lun)
 
      CALL FTBDEF(lun,tfields,tform,0,outbins,Status)
 
C
C     Loop over lightcurve bins, and calculate elements of the table.
C
C     Note that there is an important subtlety here:  binval is actually the
C     *start time* of the bin, beginning at Mintime1, and the value written
C     in the file is the offset, dtmp=binval-Mintime1.  However, the TIMEZERO
C     keyword written above (which is *not* applied automatically, unlike
C     BZERO and TZEROn) is Mintime1+(Binlc/2), so when you read the file and
C     add TIMEZERO, dtmp+timezero = binval+(binlc/2) = the bin center!
C     If lctzero is false then the lightcurve times are written out in units of
C     spacecraft time.
C

      binval = Mintime1
      binval0 = binval
      binnum = 0
 
      totphot = 0
      totbin = 0
      avglcbin = 0.D0
      DO 400 i = 1 , MIN(Imaxlc,Lcsize)
         avglcbin = avglcbin + Lc(i)
 400  CONTINUE
      IF ( Imaxlc .LT. 1 ) THEN
         avglcbin = 0
      ELSE
         avglcbin = avglcbin/DBLE(Imaxlc)
      ENDIF
      thwarn = .FALSE.
 
      DO 500 i = 1 , Imaxlc

         binsize = CALCBINSIZE(binval,Binlc,Gti,Imaxgti,status)
         IF ( binsize.NE.0.D0 .AND. binsize/Binlc.GE.Lcthresh ) THEN
            totbin = totbin + 1
            dtmp = binval - binval0
            IF ( .NOT.lctzero ) dtmp = dtmp + Mintime1 + binsize/2
            CALL FTPCLD(lun,1,totbin,1,1,dtmp,Status)
            rtmp = SNGL(FLOAT(Lc(i))/binsize)
            CALL FTPCLE(lun,2,totbin,1,1,rtmp,Status)
            rtmp = SNGL(SQRT(FLOAT(Lc(i)))/binsize)
            CALL FTPCLE(lun,3,totbin,1,1,rtmp,Status)
            rtmp = SNGL(binsize/Binlc)
            CALL FTPCLE(lun,4,totbin,1,1,rtmp,Status)
         ELSE
              IF ( avglcbin*Lcthwarn.LT.Lc(i) ) thwarn = .TRUE.
         ENDIF
         totphot = totphot + Lc(i)
         binnum = binnum + 1
         binval = binval0 + Binlc*binnum

 500  CONTINUE

      contxt = 'Failure during lightcurve write'
      IF ( Status .NE. 0 ) GOTO 999
 
      CALL FTMKYJ(lun,'naxis2',totbin,'Number of bins',Status)
 
      IF ( thwarn ) THEN
         CALL fcecho(' Thresholding removed significant counts')
         CALL fcecho(
     &    ' Try exposure=0.0 on the extract command in xselect')
         CALL fcecho(
     &    ' or lcthresh=0.0 if running extractor stand-alone')
      ENDIF

      IF ( totbin .LT. 0.5*Lcsize ) THEN
         CALL fcecho(' Thresholding removed more than half the bins')
         CALL fcecho(
     &    ' Try exposure=0.0 on the extract command in xselect')
         CALL fcecho(
     &    ' or lcthresh=0.0 if running extractor stand-alone')
      ENDIF

c Write the GTI extensions. We have to ensure that an extension called
C 'GTI' is written out as this part of the standard. In the case of multiple
C CCDs the 'GTI' extension is the merged GTIs. 

      CALL w_gti(Lun, Imaxgti, MAXGTI, Gti, 'GTI', Expos, 0, 'GTI', 
     &           Status)
      IF ( Imaxccd .GT. 1 ) THEN
         DO i = 1, Imaxccd
            IF ( Qinreg(NINT(Gtis(3,1,i))) ) THEN
               CALL w_gti(Lun, Imaxgtis(i), MAXGTI, Gtis(1,1,i), 
     &                    Gtihdunms(i), Exposs(i), NINT(Gtis(3,1,i)), 
     &                    'GTI', Status)
            ENDIF
         ENDDO
      ELSE

C if DS keywords are used to refer to a GTI extension make sure we write one
C of that name

         CALL g1tblrf(NINT(Gtis(3,1,1)), 'TIME', extname, Status)
         IF ( Status .EQ. 0 ) THEN
            CALL w_gti(Lun, Imaxgtis(1), MAXGTI, Gtis(1,1,1), 
     &                 Gtihdunms(1), Exposs(1), NINT(Gtis(3,1,1)), 
     &                 'GTI', Status)
         ELSE
            Status = 0
         ENDIF
      ENDIF

c If a region file was defined then write a region extension

      CALL w_regions(Lun, Status)

c Close the file

      CALL xclsfl(lun,Status)

 999  CONTINUE
      IF ( Status.GT.0 ) THEN
         CALL FTGERR(Status,comment)
         CALL fcerr(' '//comment)
      ENDIF
 
      RETURN
 
      END
