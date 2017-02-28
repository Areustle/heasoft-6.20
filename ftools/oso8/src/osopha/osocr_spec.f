CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version

c Creates output OSO-8 (detector B/C) spectrum for a selected
c X-ray source
 
      SUBROUTINE OSOCR_SPEC(Fitsunit,filename,detector,Object,Ra,Dec,
     &                    bg,bnrm,tim,phnorm,clobber,Status)

 
      IMPLICIT NONE
 
      LOGICAL simple , extend, clobber
 
      INTEGER Fitsunit , blocksize , Status
      INTEGER bitpix , naxis , naxes(3) , pcount , gcount
      INTEGER tfield , nrows , detector, lenact
      REAL Ra,Dec,tim,bg,bnrm,phnorm
      DOUBLE PRECISION  mjdref , Ra_obj , Dec_obj
      character(16) ttype(24) , tform(24) , tunit(24) , extnam 
      CHARACTER *(*) Object,filename
      
      character(8) startdate , stopdate , starttime , stoptime
      character(70) errm,message
 
      DATA mjdref/42412.0d0/
 
   
 
      bitpix = 8
      simple = .TRUE.
      extend = .TRUE.
      naxis = 0
      pcount = 0
      gcount = 1
      Status = 0
      blocksize = 2880
      errm = ' '
      Status =0
 
c get free logical unit number
      CALL XGTLUN(Fitsunit,Status)
      IF ( Status.NE.0 ) THEN
         errm = ' Problem obtaining free unit number'
         CALL XAERROR(errm,1)
         return
      ENDIF

      message=' '
      call xwrite(message, 1)
      filename = Object(:lenact(Object))//'.spec'
      write(message,'('' Creating '', A40)') 
     & filename
      call xwrite(message, 1)
    
      CALL FTINIT(Fitsunit,filename,blocksize,Status)
      if (Status .ne. 0) then
         if (clobber) then
            message = ' File already exists, overwriting...'
            call xwrite(message,1)
            status = 0
            open(unit=Fitsunit, file=filename, status='old')
            close(Fitsunit, status='delete')
            CALL FTINIT(Fitsunit,filename,blocksize,Status)

         endif
         if (Status .ne. 0) then
            write(errm, '('' Error creating '', A40)') filename
            call xaerror(errm,1)
            call ftgmsg(errm)
            call xaerror(errm,1)
            write(errm, '('' FITSIO status = '', I3)') Status
            call xaerror(errm,1)
            return
         endif
      endif
      message=' '
      call xwrite(message, 1)


      Ra_obj=dble(Ra)
      Dec_obj=dble(Dec)
      
     
      CALL FTPHPR(Fitsunit,simple,bitpix,naxis,naxes,pcount,gcount,
     &            extend,Status)
      CALL FTPKYS(Fitsunit,'ORIGIN','HEASARC/GSFC/NASA',
     &            'Origin of FITS file',Status)
      CALL FTPDAT(Fitsunit,Status)
      CALL FTPKYS(Fitsunit,'TELESCOP','OSO-8',
     &            'Telescope (mission) name',Status)
      CALL FTPKYS(Fitsunit,'INSTRUME','GCXSE','Instrument name',Status)
      IF ( detector .EQ.1 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','DET-B','Detector Name',
     &               Status)
      ELSE
         CALL FTPKYS(Fitsunit,'DETNAM','DET-C','Detector Name',
     &               Status)
      ENDIF
      CALL FTPKYS(Fitsunit,'OBJECT',Object,'Name of observed object',
     &            Status)
      CALL FTPKYS(Fitsunit,'OBS_MODE','SCAN',
     &            'Observation mode: POINT, SLEW, or SCAN',Status)
      CALL FTPKYF(Fitsunit,'EQUINOX',1950.0,1,
     &            'Equinox for coordinate system',Status)
      CALL FTPKYS(Fitsunit,'RADECSYS','FK4',
     &            'Stellar reference frame in use',Status)
      CALL FTPKYG(Fitsunit,'RA_OBJ',Ra_obj,3,
     &            'Right Ascension of object (degrees)',Status)
      CALL FTPKYG(Fitsunit,'DEC_OBJ',Dec_obj,3,
     &            'Declination of object (degrees)',Status)

      startdate = '25/06/75'
      starttime = '23:25:34'
      stopdate = '30/09/78'
      stoptime = '18:44:35'
      
      CALL FTPKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTPKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTPKYS(Fitsunit,'DATE-END',stopdate,
     &            'Date of observation stop (dd/mm/yy)',Status)
      CALL FTPKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
 
      CALL FTPDEF(Fitsunit,bitpix,naxis,naxes,pcount,gcount,Status)
 
      IF ( Status.NE.0 ) WRITE (*,
     &        '('' FITSIO status= '', i3,'' at end of primary header'')'
     &        ) Status
 
           

     
C Create the binary table extension
 
      CALL FTCRHD(Fitsunit,Status)
      nrows = 63
      tfield = 3
      extnam = ' '
 
      tform(1) = 'I'
      ttype(1) = 'CHANNEL'
      tunit(1) = ' '
 
      tform(2) = '1E'
      ttype(2) = 'RATE'
      tunit(2) = 'count/s'
 
      tform(3) = '1E'
      ttype(3) = 'STAT_ERR'
      tunit(3) = 'count/s'
 
      CALL FTPHBN(Fitsunit,nrows,tfield,ttype,tform,tunit,extnam,pcount,
     &            Status)
   
      CALL FTBDEF(Fitsunit,tfield,tform,pcount,nrows,Status)
      CALL FTMKYS(Fitsunit,'ttype1','CHANNEL','PHA Channel',Status)
      CALL FTMKYS(Fitsunit,'ttype2','RATE',
     &'Count rate (count/s)' ,Status)
      CALL FTMKYS(Fitsunit,'ttype3','STAT_ERR',
     &'Error in count rate(count/s) ', Status)
      CALL FTPKYS(Fitsunit,'ORIGIN','HEASARC/GSFC/NASA',
     &            'Origin of FITS file',Status)
      CALL FTPDAT(Fitsunit,Status)
      CALL FTPKYS(Fitsunit,'TELESCOP','OSO-8',
     &            'Telescope (mission) name',Status)
      CALL FTPKYS(Fitsunit,'INSTRUME','GCXSE','Instrument name',Status)
      IF ( detector.EQ.1 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','DET-B','Detector Name',
     &               Status)
      ELSE
         CALL FTPKYS(Fitsunit,'DETNAM','DET-C','Detector Name',
     &               Status)
      ENDIF
      CALL FTPKYS(Fitsunit,'OBJECT',Object,'Name of observed object',
     &            Status)
      CALL FTPKYF(Fitsunit,'EQUINOX',1950.0,1,
     &            'Equinox for coordinate system',Status)
      CALL FTPKYS(Fitsunit,'RADECSYS','FK4',
     &            'Stellar reference frame in use',Status)
      CALL FTPKYG(Fitsunit,'RA_OBJ',Ra_obj,3,
     &            'Right Ascension of object (degrees)',Status)
      CALL FTPKYG(Fitsunit,'DEC_OBJ',Dec_obj,3,
     &            'Declination of object (degrees)',Status)
      CALL FTPKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTPKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTPKYS(Fitsunit,'DATE-END',stopdate,
     &            'Date of observation stop (dd/mm/yy)',Status)
      CALL FTPKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
      CALL FTPKYS(Fitsunit,'TASSIGN','SATELLITE',
     &            'Location reference where times assigned',Status)
      CALL FTPKYS(Fitsunit,'TIMEREF','LOCAL',
     &            'No barycentric corrections applied',Status)
      CALL FTPKYG(Fitsunit,'MJDREF',mjdref,10,
     &            'Modified Julian Date reference (Jan 0, 1975)',Status)
      CALL FTPKYS(Fitsunit,'TIMESYS','1975 1 0 00:00:00',
     &            'Reference frame for time assignment',Status)
      CALL FTPKYS(Fitsunit,'TIMEUNIT','s',
     &            'Unit for time (TSTART,TSTOP,TIMEDEL,etc.)',Status)
      CALL FTPKYG(Fitsunit,'TSTART',0.0d0,3,
     &            'Start time (seconds since MJDREF)',Status)
      CALL FTPKYG(Fitsunit,'TSTOP',0.0d0,3,
     &            'Stop time (seconds since MJDREF)',Status)
      call ftpkyg(Fitsunit, 'TELAPSE', 0.0D0, 3,
     +     'Time interval (Start - Stop) in seconds', Status)
      CALL FTPKYL(Fitsunit,'CLOCKAPP',.FALSE.,
     &            'No clock corrections are applied',Status)
      CALL FTPKYS(Fitsunit,'OBS_MODE','SCAN',
     &            'Observation mode: POINTING, SLEW, or SCAN',Status)
      CALL FTPKYS(Fitsunit,'HDUCLASS','OGIP',
     &            'Format conforms to OGIP standards',Status)
      CALL FTPKYS(Fitsunit,'HDUCLAS1','SPECTRUM',
     &            'SPECTRAL dataset (OGIP memo 93/003)',Status)
      If (bg .eq. 0.0) then
          CALL FTPKYS(Fitsunit,'HDUCLAS2','TOTAL',
     &            'gross spectrum (source + bkgd) ',Status)
      else
          CALL FTPKYS(Fitsunit,'HDUCLAS2','NET',
     &            'net source spectrum',Status)
      endif

      CALL FTPKYS(Fitsunit,'HDUCLAS3','RATE',
     &            'PHA data is in counts/s ',Status)
      CALL FTPKYS(Fitsunit,'HDUVERS1','1.1.0',
     &            'OGIP classification of FITS format ',Status)
      CALL FTPKYJ(Fitsunit,'TLMIN1',1,
     &            'Lowest legal channel number',Status)
    
      CALL FTPKYJ(Fitsunit,'TLMAX1',63,
     &            'Highest legal channel number',Status)
     
      CALL FTPKYF(Fitsunit,'EXPOSURE',tim,2,
     &            'Exposure (in seconds)',Status)
     
      CALL FTPKYJ(Fitsunit,'DETCHANS',63,
     &            'Total number possible channels',Status)
      CALL FTPKYF(Fitsunit,'AREASCAL',bnrm,2,
     &            'Area scaling factor',Status)
      CALL FTPKYF(Fitsunit,'BACKSCAL',1.0,2,
     &            'Background scaling factor',Status)
      CALL FTPKYF(Fitsunit,'CORRSCAL',0.0,2,
     &            'Correction scaling factor',Status)
      CALL FTPKYJ(Fitsunit,'GROUPING',0,
     &            'No grouping of the data has been defined',Status)
      CALL FTPKYJ(Fitsunit,'QUALITY',0,
     &            'No data quality information specified',Status)
      CALL FTPKYF(Fitsunit,'PHNORM',phnorm,2,
     &            'PHA normalisation factor (EXPOSURE*AREASCAL)',Status)


      IF ( Status.NE.0 ) WRITE (*,
     &         '('' FITSIO status ='', i3,'' at end of data bin head'')'
     &         ) Status
 
   
      RETURN
 
      END
