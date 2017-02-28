CH  Lorraine Breedon (1.0.0 05 Apr 1999) Original working version

c Creates output OSO-8 (detector A,B,C) rates data for a selected
c X-ray source
 
      SUBROUTINE OSOCR_RATES_ABC(Fitsunit,filename,detector,Object,Ra,
     &                     Dec,clobber,Status)

 
      IMPLICIT NONE
 
      LOGICAL simple , extend, clobber
 
      INTEGER Fitsunit , blocksize , Status
      INTEGER bitpix , naxis , naxes(3) , pcount , gcount
      INTEGER tfield , nrows , detector, lenact
      REAL Ra,Dec
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
      filename = Object(:lenact(Object))//'.lc'
      write(message,'('' Creating '', A40)') 
     & filename
      call xwrite(message, 1)
    
      CALL FTINIT(Fitsunit,filename,blocksize,Status)
      if (Status .ne. 0) then
         if (clobber) then
            message = ' File already exists, overwriting...'
            call xwrite(message,1)
            Status = 0
            open(unit=Fitsunit, file=filename, Status='old')
            close(Fitsunit, Status='delete')
            CALL FTINIT(Fitsunit,filename,blocksize,Status)

         endif
         if (Status .ne. 0) then
            write(errm, '('' Error creating '', A40)') filename
            call xaerror(errm,1)
            call ftgmsg(errm)
            call xaerror(errm,1)
            write(errm, '('' FITSIO Status = '', I3)') Status
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

      IF ( detector .EQ. 0 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','DET-A','Detector Name',
     &               Status)
      ELSEIF ( detector .EQ. 1) THEN
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
      call ftpkyf(Fitsunit, 'SRCCNT', 0.0, 3,
     +        'Average count rate for the source', Status)
      call ftpkyf(Fitsunit, 'SRCCNTE', 0.0, 3, 
     +        'Average count rate error for the source', Status)
 
      CALL FTPDEF(Fitsunit,bitpix,naxis,naxes,pcount,gcount,Status)
 
      IF ( Status.NE.0 ) WRITE (*,
     &        '('' FITSIO Status= '', i3,'' at end of primary header'')'
     &        ) Status
 
     
C Create the binary table extension
 
      CALL FTCRHD(Fitsunit,Status)
      nrows = 1
      tfield = 9

      tform(1) = '1D'
      ttype(1) = 'TIME'
      tunit(1) = 's'

      tform(2) = '1E'
      ttype(2) = 'RATE'
      tunit(2) = 'count/s/cm**2'

      tform(3) = '1E'
      ttype(3) = 'ERROR'
      tunit(3) = 'count/s/cm**2'

      tform(4) = '1E'
      ttype(4) = 'SOFT'
      tunit(4) = 'counts'

      tform(5) = '1E'
      ttype(5) = 'HARD'
      tunit(5) = 'counts'

      tform(6) = '1E'
      ttype(6) = 'TOTAL'
      tunit(6) = 'counts'


      tform(7) = '1E'
      ttype(7) = 'RATIO'
      tunit(7) = ' '

      tform(8) = '1E'
      ttype(8) = 'TOTNORM'
      tunit(8) = '  '

      tform(9) = '1E'
      ttype(9) = 'AREANORM'
      tunit(9) = 'cm**2 '


      extnam = 'RATES'
 
      CALL FTPHBN(Fitsunit,nrows,tfield,ttype,tform,tunit,extnam,pcount,
     &            Status)
      CALL FTBDEF(Fitsunit,tfield,tform,pcount,nrows,Status)
      CALL FTMKYS(Fitsunit,'ttype1','TIME', 
     +     'Time of observation (seconds since MJDREF)',Status)
      CALL FTMKYS(Fitsunit,'ttype2','RATE',
     &'Data from detector' ,Status)
      CALL FTMKYS(Fitsunit,'ttype3','ERROR',
     &'Error in rate', Status)
      CALL FTMKYS(Fitsunit,'ttype4','SOFT',
     &'Soft counts from detector' ,Status)
      CALL FTMKYS(Fitsunit,'ttype5','HARD',
     &'Hard counts from detector' ,Status)
      CALL FTMKYS(Fitsunit,'ttype6','TOTAL',
     &'Total counts from detector' ,Status)
      CALL FTMKYS(Fitsunit,'ttype7','RATIO',
     &'Hardness ratio (hard/soft)' ,Status)
      call ftmkys(Fitsunit, 'ttype8', 'TOTNORM',
     +     'Effective area of detector', Status)
      call ftmkys(Fitsunit, 'ttype9', 'AREANORM',
     +     'Actual area of detector', Status)

       
      CALL FTPKYS(Fitsunit,'ORIGIN','HEASARC/GSFC/NASA',
     &            'Origin of FITS file',Status)
      CALL FTPDAT(Fitsunit,Status)
      CALL FTPKYS(Fitsunit,'TELESCOP','OSO-8',
     &            'Telescope (mission) name',Status)
      CALL FTPKYS(Fitsunit,'INSTRUME','GCXSE','Instrument name',Status)
      IF ( detector .EQ. 0 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','DET-A','Detector Name',
     &               Status)
      ELSEIF ( detector .EQ. 1) THEN
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
      call ftpkyg(Fitsunit, 'TIMEDEL', 40.96D0, 2, 
     +     'Time step between rows (seconds)', Status)
      call ftpkyg(Fitsunit, 'TELAPSE', 0.0D0, 3,
     +     'Time interval (Start - Stop) in seconds', Status)
      CALL FTPKYL(Fitsunit,'CLOCKAPP',.FALSE.,
     &            'No clock corrections are applied',Status)
      CALL FTPKYS(Fitsunit,'OBS_MODE','SCAN',
     &            'Observation mode: POINTING, SLEW, or SCAN',Status)
      CALL FTPKYS(Fitsunit,'HDUCLASS','OGIP',
     &            'Format conforms to OGIP standards',Status)
      call ftpkys(Fitsunit, 'HDUCLAS2', 'TOTAL',
     +     'Data has NOT been background subtracted', Status)
      call ftpkys(Fitsunit, 'HDUCLAS3', 'RATE',
     +     'File contains RATES data', Status)
c      call ftpkyf(Fitsunit, 'GEOAREA', 0.0, 2, 
c     +     'Total geometric area of the detector', Status)
c      call ftpkyf(Fitsunit, 'E_MIN', 0.0, 2, 
c     +     'Lower energy range (keV)', Status)
c      call ftpkyf(Fitsunit, 'E_MAX', 0.0, 2, 
c     +     'Upper energy range (keV)', Status)
c      call ftpkys(Fitsunit, 'EUNIT', 'keV', 
c     +     'Units for E_MIN and E_MAX', Status)
      call ftpkyf(Fitsunit, 'SRCCNT', 0.0, 3, 
     +        'Average count rate for the source', Status)
      call ftpkyf(Fitsunit, 'SRCCNTE', 0.0, 3,
     +        'Average count rate error for the source', Status)
  
      IF ( Status.NE.0 ) WRITE (*,
     &         '('' FITSIO Status ='', i3,'' at end of data bin head'')'
     &         ) Status
 
      
      RETURN
 
      END
