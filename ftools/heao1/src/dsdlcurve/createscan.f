**==createscan.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C  This is a subroutine to create FITS headers for HEAO-1 A-2 DSDISK
C scan "light curve"
C
C Author: Jesse Allen
C History:
C  Version 0.0  03/24/1998  First rough draft
CH  Lorraine Breedon (1.1.0 26 Nov 1998) Remove hard wired Fitsunit 
CH                                       number \
CH                                       Introduce clobber option \

c Creates output scan lightcurve of a selected X-ray source
 
      SUBROUTINE CREATESCAN(Fitsunit,filename,I3fov,Object,Ra,Dec,
     &                      clobber,Status)

c Fitsunit O  i     logical unit no for output FITS scan file
c filename O  ch*70 Name of output FITS scan file
c I3fov   I  i      Integer flag for instrument fov
c                   (0 for 1.5x3.0 deg fov ; 1 for 3.0x3.0 deg fov)
c Object  I  ch*25  rootname of output scan file
c Ra      I  r*8    Source RA (decimal degrees)
c Dc      I  r*8    Source dec (decimal degrees)
c Clobber I  l      Flag to overwrite scan output if it already exists
c Status  O  i      Output error status flag
 
      IMPLICIT NONE
 
      LOGICAL simple , extend, clobber
 
      INTEGER Fitsunit , blocksize , Status
      INTEGER bitpix , naxis , naxes(3) , pcount , gcount
      INTEGER tfield , nrows , I3fov, lenact
      REAL*8  Ra,Dec
      DOUBLE PRECISION tstart , tstop , mjdref , Ra_obj , Dec_obj
 
      character(8) startdate , stopdate , starttime , stoptime
      character(16) ttype(24) , tform(24) , tunit(24) , extnam 
      CHARACTER *(*) Object,filename
      character(70) errm,message
 
      DATA mjdref/43143.0D0/
 
 
      bitpix = 8
      simple = .TRUE.
      extend = .TRUE.
      naxis = 0
      extnam = ' '
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
      filename = Object(:lenact(Object))//'.scan'
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
      CALL FTPKYS(Fitsunit,'TELESCOP','HEAO-1',
     &            'Telescope (mission) name',Status)
      CALL FTPKYS(Fitsunit,'INSTRUME','A-2','Instrument name',Status)
      IF ( I3fov.EQ.1 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','MEDl+HED-3l','Detector Name',
     &               Status)
      ELSE
         CALL FTPKYS(Fitsunit,'DETNAM','MEDs+HED-3s','Detector Name',
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
 
      tstart = (43367.0D0-mjdref)*8.64D4
      startdate = '12/08/77'
      starttime = '00:00:00'
      tstop = (43882.99999D0-mjdref)*8.64D4
      stopdate = '09/01/79'
      stoptime = '23:59:59'
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
      nrows = 100
      tfield = 3
 
      tform(1) = '1E'
      ttype(1) = 'SCAN_ANGLE'
      tunit(1) = 'deg'
 
      tform(2) = '1E'
      ttype(2) = 'RATE'
      tunit(2) = 'count/sec'
 
      tform(3) = '1E'
      ttype(3) = 'ERROR'
      tunit(3) = 'count/sec'
 
      CALL FTPHBN(Fitsunit,nrows,tfield,ttype,tform,tunit,extnam,pcount,
     &            Status)
      CALL FTBDEF(Fitsunit,tfield,tform,pcount,nrows,Status)
      CALL FTMKYS(Fitsunit,'ttype1','SCAN_ANGLE','Scan Angle',Status)
      CALL FTMKYS(Fitsunit,'ttype2','RATE','Summed count rate',Status)
      CALL FTMKYS(Fitsunit,'ttype3','ERROR','Error in count rate',
     &            Status)
      CALL FTPKYS(Fitsunit,'ORIGIN','HEASARC/GSFC/NASA',
     &            'Origin of FITS file',Status)
      CALL FTPDAT(Fitsunit,Status)
      CALL FTPKYS(Fitsunit,'TELESCOP','HEAO-1',
     &            'Telescope (mission) name',Status)
      CALL FTPKYS(Fitsunit,'INSTRUME','A-2','Instrument name',Status)
      IF ( I3fov.EQ.1 ) THEN
         CALL FTPKYS(Fitsunit,'DETNAM','MEDl+HED-3l','Detector Name',
     &               Status)
      ELSE
         CALL FTPKYS(Fitsunit,'DETNAM','MEDs+HED-3s','Detector Name',
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
      CALL FTPKYF(Fitsunit,'SCANANG',0.0,3,
     &            'Scan angle position of the source',Status)
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
     &            'Modified Julian Date reference (Jan 0, 1977)',Status)
      CALL FTPKYS(Fitsunit,'TIMESYS','1977 1 0 00:00:00',
     &            'Reference frame for time assignment',Status)
      CALL FTPKYS(Fitsunit,'TIMEUNIT','s',
     &            'Unit for time (TSTART,TSTOP,TIMEDEL,etc.)',Status)
      CALL FTPKYG(Fitsunit,'TSTART',tstart,3,
     &            'Start time (seconds since MJDREF)',Status)
      CALL FTPKYG(Fitsunit,'TSTOP',tstop,3,
     &            'Stop time (seconds since MJDREF)',Status)
      CALL FTPKYL(Fitsunit,'CLOCKAPP',.FALSE.,
     &            'No clock corrections are applied',Status)
      CALL FTPKYS(Fitsunit,'OBS_MODE','SCAN',
     &            'Observation mode: POINTING, SLEW, or SCAN',Status)
      CALL FTPKYS(Fitsunit,'HDUCLASS','OGIP',
     &            'Format conforms to OGIP standards',Status)
      CALL FTPKYS(Fitsunit,'HDUCLAS1','TEMPORALDATA',
     &            'TEMPORAL dataset (OGIP memo 93/003)',Status)
      CALL FTPKYF(Fitsunit,'E_MIN',1.5,1,'Lower energy range (keV)',
     &            Status)
      CALL FTPKYF(Fitsunit,'E_MAX',60.0,1,'Upper energy range (keV)',
     &            Status)
      CALL FTPKYS(Fitsunit,'EUNIT','keV','Units for E_MIN and E_MAX',
     &            Status)
 
      IF ( Status.NE.0 ) WRITE (*,
     &         '('' FITSIO status ='', i3,'' at end of data bin head'')'
     &         ) Status
 
      RETURN
 
      END
