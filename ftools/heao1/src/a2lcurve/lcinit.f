C----------------------------------------------------------------------------
C This subroutine initializes the HEAO-1 A2 light curve FITS file, putting 
C blank placeholding values in certain keywords where necessary.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.9  27 Jan 1998  Copied from existing XRATE code for Unix and 
C                            modified for FTOOLS use

      subroutine initlc(fitsunit, fitsfile, object, ra_obj, 
     +           dec_obj, detector, emin, emax, userbackflag, clobber, 
     +           status)

      implicit none

      integer fitsunit
      character*(*) fitsfile,object,detector
      
      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname




C Local variables

      logical simple, extend, userbackflag, clobber

      integer blocksize, status
      integer bitpix, naxis, naxes(3), pcount, gcount
      integer tfield, nrows

      real ra_obj, dec_obj, emin, emax

      double precision mjdref

      character(8) startdate, stopdate, starttime, stoptime
      character(16) ttype(4), tform(4), tunit(4)
      character(16) extnam,  obs_mode
      character(80) message

      data mjdref/43143.0D0/

      bitpix = 8
      simple = .TRUE.
      extend = .TRUE.
      naxis = 0
      pcount = 0
      gcount = 1
      status = 0
      blocksize = 2880

      write(message,'('' Creating '', A40)') fitsfile
      call xwrite(message, 1)
      call ftinit(fitsunit, fitsfile, blocksize, status)
      if (status .ne. 0) then
         if (clobber) then
            message = ' Files already exists, overwriting...'
            call xwrite(message,1)
            status = 0
            open(unit=fitsunit, file=fitsfile, status='old')
            close(fitsunit, status='delete')
            call ftinit(fitsunit, fitsfile, blocksize, status)
         endif
         if (status .ne. 0) then
            write(message, '('' Error creating '', A40)') fitsfile
            call xaerror(message,1)
            call ftgmsg(message)
            call xaerror(message,1)
            write(message, '('' FITSIO status = '', I3)') status
            call xaerror(message,1)
            go to 999
         endif
      endif

      call ftphpr(fitsunit, simple, bitpix, naxis, naxes, pcount,
     +     gcount, extend, status)
      call ftpkys(fitsunit, 'ORIGIN', 'HEASARC/GSFC/NASA',
     +     'Origin of FITS file', status)
      call ftpdat(fitsunit, status)
      call ftpkys(fitsunit, 'TELESCOP', 'HEAO-1',
     +     'Telescope (mission) name', status)
      call ftpkys(fitsunit, 'INSTRUME', 'A-2',
     +     'Instrument (detector) name', status)
      call ftpkys(fitsunit, 'DETNAM', detector, 
     +     'Detector name', status)
      call ftpkys(fitsunit, 'OBJECT', object, 
     +     'Name of observed object', status)
      obs_mode = 'SCAN'
C userbackflag indicates that a background model has been selected...  Data may
C be scanned observations or pointed observations.
C      if (userbackflag) obs_mode = 'POINTING'
      call ftpkys(fitsunit, 'OBS_MODE', obs_mode,
     +     'Observing mode: Pointing, Scan, or Slew', status)
      call ftpkyf(fitsunit, 'EQUINOX', 1950.0, 1, 
     +     'Equinox for coordinate system', status)
      call ftpkys(fitsunit, 'RADECSYS', 'FK4', 
     +     'Stellar reference frame in use', status)
      call ftpkyf(fitsunit, 'RA_OBJ', ra_obj, 3, 
     +     'Right Ascension of object (degrees)', status)
      call ftpkyf(fitsunit, 'DEC_OBJ',dec_obj, 3, 
     +     'Declination of object (degrees)', status)
 
      startdate = '12/08/77'
      starttime = '00:00:00'
      stopdate = '09/01/79'
      stoptime = '23:59:59'
      call ftpkys(fitsunit, 'DATE-OBS', startdate,
     +     'Date of observation start (dd/mm/yy)', status)
      call ftpkys(fitsunit, 'TIME-OBS', starttime, 
     +     'Time of observation start (hh:mm:ss)', status)
      call ftpkys(fitsunit, 'DATE-END', stopdate, 
     +     'Date of observation stop (dd/mm/yy)', status)
      call ftpkys(fitsunit, 'TIME-END', stoptime, 
     +     'Time of observation stop (hh:mm:ss)', status)
      call ftpkyf(fitsunit, 'SRCCNT', 0.0, 3,
     +        'Average count rate for the source', status)
      call ftpkyf(fitsunit, 'SRCCNTE', 0.0, 3, 
     +        'Average count rate error for the source', status)

      call wrhist(fitsunit, status)


      call ftpdef(fitsunit, bitpix, naxis, naxes, pcount, gcount, 
     +     status)
 
      if (status .ne. 0) then
         write(message, '('' Error writing primary array in '', A40)') 
     +     fitsfile
         call xaerror(message,1)
         call ftgmsg(message)
         call xaerror(message,1)
         write(message, '('' FITSIO status = '', I3)') status
         call xaerror(message,1)
         go to 999
      endif

C Create the binary table extension

      call ftcrhd(fitsunit, status)
      if (status .ne. 0) then
         write(message, '('' Error creating binary table in '', A40)') 
     +     fitsfile
         call xaerror(message,1)
         call ftgmsg(message)
         call xaerror(message,1)
         write(message, '('' FITSIO status = '', I3)') status
         call xaerror(message,1)
         go to 999
      endif

      nrows = 1
      tfield = 4

      tform(1) = '1D'
      ttype(1) = 'TIME'
      tunit(1) = 's'

      tform(2) = '1E'
      ttype(2) = 'RATE'
      tunit(2) = 'count/s'

      tform(3) = '1E'
      ttype(3) = 'ERROR'
      tunit(3) = 'count/s'

      tform(4) = '1E'
      ttype(4) = 'FRACEXP'
      tunit(4) = ' '
      extnam = 'RATES'

      call ftphbn(fitsunit, nrows, tfield, ttype, tform, tunit, 
     +      extnam, pcount, status)
      if (status .ne. 0) then
         write(message, '('' Error defining binary table in '', A40)') 
     +     fitsfile
         call xaerror(message,1)
         call ftgmsg(message)
         call xaerror(message,1)
         write(message, '('' FITSIO status = '', I3)') status
         call xaerror(message,1)
         go to 999
      endif

      call ftmkys(fitsunit, 'ttype1', 'TIME', 
     +     'Time of observation (seconds since MJDREF)', status)
      call ftmkys(fitsunit, 'ttype2', 'RATE', 'Data from detector', 
     +     status)
      call ftmkys(fitsunit, 'ttype3', 'ERROR', 'Error in rate', 
     +     status)
      call ftmkys(fitsunit, 'ttype4', 'FRACEXP',
     +     'Fractional effective cross section', status)
      call ftpkys(fitsunit, 'ORIGIN', 'HEASARC/GSFC', 
     +     'Origin of FITS file', status)
      call ftpdat(fitsunit, status)
      call ftpkys(fitsunit, 'TELESCOP', 'HEAO-1', 
     +     'Telescope (mission) name', status)
      call ftpkys(fitsunit, 'INSTRUME', 'A-2', 'Instrument name', 
     +     status)
      call ftpkys(fitsunit, 'DETNAM', detector, 
     +     'Detector name', status)
      call ftpkys(fitsunit, 'OBJECT', object, 
     +     'Name of observed object', status)
      call ftpkys(fitsunit, 'OBS_MODE', obs_mode,
     +     'Observing mode: Pointing, Scan, or Slew', status)
      call ftpkyf(fitsunit, 'EQUINOX', 1950.0, 1, 
     +     'Equinox for coordinate system', status)
      call ftpkys(fitsunit, 'RADECSYS', 'FK4', 
     +     'Stellar reference frame in use', status)
      call ftpkyf(fitsunit, 'RA_OBJ', ra_obj, 3, 
     +     'Right Ascension of object (degrees)', status)
      call ftpkyf(fitsunit, 'DEC_OBJ',dec_obj, 3, 
     +     'Declination of object (degrees)', status)
      call ftpkys(fitsunit, 'DATE-OBS', startdate,
     +     'Date of observation start (dd/mm/yy)', status)
      call ftpkys(fitsunit, 'TIME-OBS', starttime,
     +     'Time of observation start (hh:mm:ss)', status)
      call ftpkys(fitsunit, 'DATE-END', stopdate,
     +     'Date of observation stop (dd/mm/yy)', status)
      call ftpkys(fitsunit, 'TIME-END', stoptime,
     +     'Time of observation stop (hh:mm:ss)', status)
      call ftpkyg(fitsunit, 'MJDREF', mjdref, 7,
     +     'Modified Julian Date reference (0 Jan 1977, O UT)', status) 
      call ftpkys(fitsunit, 'TASSIGN', 'SATELLITE',
     +     'Location reference where times assigned', status)
      call ftpkys(fitsunit, 'TIMEREF', 'LOCAL', 
     +     'No barycentric corrections applied', status)
      call ftpkys(fitsunit, 'TIMESYS', '1977 1 0 00:00:00',
     +     'Reference frame for time assignment', status)
      call ftpkys(fitsunit, 'TIMEUNIT', 's', 
     +     'Unit for time (TSTART,TSTOP,TIMEDEL,etc.)', status)
      call ftpkyg(fitsunit, 'TSTART', 0.0D0, 3, 
     +     'Seconds since MJDREF at observation start', status)
      call ftpkyg(fitsunit, 'TSTOP', 0.0D0, 3, 
     +     'Seconds since MJDREF at observation end', status)
      call ftpkyg(fitsunit, 'TIMEDEL', 5.12D0, 2, 
     +     'Time step between rows (seconds)', status)
      call ftpkyj(fitsunit, 'TIMEPIXR', 0, 
     +     'Time pixel ref. for TIME col:0=start,1=end', status)
      call ftpkyg(fitsunit, 'TELAPSE', 0.0D0, 2,
     +     'Time interval (Start - Stop) in seconds', status)
      call ftpkyg(fitsunit, 'ONTIME', 0.0D0, 3, 
     +     'Time (s) on source for this extension', status)
      call ftpkyl(fitsunit, 'CLOCKAPP', .FALSE., 
     +     'No clock corrections are applied', status)
      call ftpkys(fitsunit, 'DATAMODE', 'RATE_5.12s',
     +     'RATE information with 5.12 s resolution', status)
      call ftpkys(fitsunit, 'HDUCLASS', 'OGIP', 
     +     'Format conforms to OGIP standards', status)
      call ftpkys(fitsunit, 'HDUCLAS1', 'LIGHTCURVE',
     +     'File contains light curve data', status)
      call ftpkys(fitsunit, 'HDUCLAS2', 'NET',
     +     'Data has been background subtracted', status)
      call ftpkys(fitsunit, 'HDUCLAS3', 'RATE',
     +     'File contains RATES data', status)
      call ftpkyf(fitsunit, 'GEOAREA', 0.0, 2, 
     +     'Total geometric area of the detector', status)
      call ftpkyf(fitsunit, 'E_MIN', emin, 2, 
     +     'Lower energy range (keV)', status)
      call ftpkyf(fitsunit, 'E_MAX', emax, 2, 
     +     'Upper energy range (keV)', status)
      call ftpkys(fitsunit, 'EUNIT', 'keV', 
     +     'Units for E_MIN and E_MAX', status)
      call ftpkyf(fitsunit, 'SRCCNT', 0.0, 3, 
     +        'Average count rate for the source', status)
      call ftpkyf(fitsunit, 'SRCCNTE', 0.0, 3,
     +        'Average count rate error for the source', status)

      if (status .ne. 0) then
         write(message, '('' Error writing binary table in '', A40)') 
     +     fitsfile
         call xaerror(message,1)
         call ftgmsg(message)
         call xaerror(message,1)
         write(message, '('' FITSIO status = '', I3)') status
         call xaerror(message,1)
         go to 999
      endif

 999  return

      end

C----------------------------------------------------------------------------
C This subroutine writes the HEAO-1 A2 history information, including the 
C options used to generate the FITS light curve
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.9  27 Jan 1998  Copied and modified from Vela 5B FTOOLS

      subroutine wrhist(outunit, status)

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables

c      integer outunit, i, status
      integer outunit,  status

c      character(65) hisheader(24)
      character(65) hisheader(2)

      data hisheader /
     +'This lightcurve was extracted using the HEAO-1 A2 raw 5.12',
     +'second data.'/
c     +'HEAO-1 was launched on 12 August 1977 in a 93 min low orbit.'
c     +'During the bulk of the mission, it operated in a scanning ',
c     +'mode with a rotation period of 35 min',
c     +'scanning out great circles of constant ecliptic longitude.'
c     +'Every twelve hours, the spin axis would be',
c     +'realigned with the Sun, adjusting the longitude of the great',
c     +'circle by roughly half a degree.  Thus, after six months, the',
c     +'spacecraft would have scanned the entire sky.  After the first',
c     +'100 days, the scanning was interrupted periodically to perform',
c     +'pointed observations.  These pointings became more frequent',
c     +'until 9 January, 1979, when the attitude control gas supply ran',
c     +'out. The satellite re-entered the Earth''s atmosphere on',
c     +'14 March, 1979.',
c     +' ',
c     +'HEAO-1 was equipped with four instrument packages.  The A2',
c     +'instrument package consisted of six multi-layer multi-anode',
c     +'collimated gas proportional counters.  These were two LEDs (Low',
c     +'Energy Detectors), a MED (Medium Energy Detector), and three',
c     +'HEDs (High Energy Detectors).',
c     +' ',
c     +'Light curve data from the A2 instrument were derived from the',
c     +'XRATE database.'/


      do 100 i = 1, 2
c         call ftphis(outunit, hisheader(i), status)
         call ftphis(outunit, hisheader(i), status)
 100  continue

      return

      end
