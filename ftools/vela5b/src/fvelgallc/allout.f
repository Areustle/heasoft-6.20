C This subroutine takes all the deconvolved light curves for each of the 
C sources and writes the results to FITS
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0               Priedhorsky's version
C          0.9   6 Mar 1995  FVELGALLC FTOOL first draft version
C          0.91  7 Mar 1995  Passes dynamically allocated data arrays
C          1.0  15 Aug 1995  Writes HISTORY keyword information
C          1.1  12 Sep 1995  Allows 20 sources to be fit
C          1.2  31 Jan 1996  Removes all NULL rows, BACKV and BACKE are 
C                             only uniform background

      subroutine allout(channel, mjdtime, ast, aerrst, csqst, ndofst,
     +           numofrows, minflux, maxflux, maxerr, spincheck, 
     +           pointcheck, weight, backopt, stimbin,
     +           long_cnr1, lat_cnr1, long_cnr2, lat_cnr2)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate

      character(16) sourcename(20)

C Local variables

      logical spincheck, pointcheck, weight

      integer channel, backopt, numofrows
      integer ndofst(numofrows)
      integer l, k, lcunit, status

      real ast(nsrc+3,numofrows), aerrst(nsrc+3,numofrows)
      real csqst(numofrows)
      real minflux, maxflux, maxerr, stimbin
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2

      double precision mjdtime(numofrows)

      character(20) fitsfile
      character(80) message


      lcunit = 10

      message = ' Summary of the FVELGALLC results '
      call fcecho(message)
      write(message,'(X, I4, '' data sets analyzed, '', I2, 
     +     '' sources fitted '')') numofrows, nsrc
      call fcecho(message)
      do 200 l = 1, nsrc
         write(fitsfile,'(''fvelgallc_'',I2.2,''_ch'',I1,''.lc'')') 
     +        l, channel
         write(message,'('' Writing channel '', I1, '' data to '',
     +         A19, '' for '', A16)') channel, fitsfile, 
     +         sourcename(l)
         call fcecho(message)
         call lcinit(fitsfile, lcunit, sourcename(l), long_src(l), 
     +        lat_src(l), channel, numofrows, minflux, maxflux, maxerr,
     +        spincheck, pointcheck, weight, backopt,.TRUE.,.FALSE., 
     +        (stimbin/86400.0D0), long_cnr1, lat_cnr1, long_cnr2, 
     +        lat_cnr2, status)
         if (status .ne. 0) then
            write(message,'('' Error initializing light curve for '',
     +        A16, '', status = '', I3)') sourcename(l), status
            call fcecho(message)
         else
            do 100 k = 1, numofrows
               call lcwrite(lcunit, k, mjdtime(k), ast(l+3,k), 
     +           aerrst(l+3,k), csqst(k), ndofst(k), ast(1,k), 
     +           aerrst(1,k), status)
               if (status .ne. 0) then 
                  write(message,'('' Error writing row '', I3, 
     +              '' for '', A16, '', status = '', I3)') 
     +               k, sourcename(l), status
                  call fcecho(message)
               endif
 100        continue
            call lcclose(lcunit, imax, jmax, status)
            if (status .ne. 0) then
               write(message,'('' Error closing light curve for '',
     +           A16, '', status = '', I3)') sourcename(l), status
               call fcecho(message)
            endif
         endif
 200  continue

      return

      end
         

C----------------------------------------------------------------------
C This subroutine sets up the light curve file, setting all the keyword
C values and setting up the binary table extension to record data.
C 
C Author: Jesse S. Allen (Hughes STX: HEASARC/GSFC/NASA)
C History:
C   Version 0.9   6 Mar 1995
C           1.0  15 Aug 1995  Handles information for HISTORY entries
C           1.1  11 Dec 1995  No longer provided with begin and end times
C           1.2  31 Jan 1996  BACKV and BACKE are single elements instead
C                              of three element vector

      subroutine lcinit(fitsfile, outunit, object, srclong, srclat, 
     +   channel, numofrows, minflux, maxflux, maxerr, spincheck, 
     +   pointcheck, weight, backopt, collim, bary, timestep, 
     +   long_cnr1, lat_cnr1, long_cnr2, lat_cnr2, status)

      implicit none

C Common block declarations

       common /TASK/ taskname
      
       character(40) taskname

C Local variable declarations

       logical simple, extend, collim, bary
       logical spincheck, pointcheck, weight

       integer bitpix, naxis, naxes(3), nrows, pcount, gcount, tfield
       integer outunit, status, blocksize, i
       integer channel, numofrows, backopt

       real srclong, srclat, minflux, maxflux, maxerr
       real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
 
       double precision ra_obj, dec_obj, alpha, delta
       double precision xa, ya, za, xb, yb, zb
       double precision timestep, pi, deg2rad, rad2deg

       character(8) extnam
       character(16) ttype(10), tform(10), tunit(10), object
       character(20) fitsfile


C Assign some constants used throughout the FITS file creation

       simple = .TRUE.
       bitpix = 8
       extend = .TRUE.
       naxis = 0
       pcount = 0
       gcount = 1
       naxes(1) = 1
       blocksize = 2880
       status = 0

       pi = 3.14159265359D0
       deg2rad = pi / 180.0D0
       rad2deg = 180.0D0 / pi

C  Create the new FITS file and write the primary header keywords

       call ftinit(outunit, fitsfile, blocksize, status)

       call ftphpr(outunit, simple, bitpix, naxis, naxes, pcount, 
     +      gcount, extend, status)
       call ftpkys(outunit, 'ORIGIN', 'HEASARC/NASA/GSFC',
     +      'Origin of this FITS file', status)
       call ftpkys(outunit, 'CREATOR', taskname,
     +      'HEASARC/GSFC FTOOL which created FITS file', status)
       call ftpdat(outunit, status)
       call ftpkys(outunit, 'TELESCOPE', 'Vela 5B',
     +      'Telescope (mission) name', status)
       call ftpkys(outunit, 'INSTRUMENT', 'XC',
     +      'Instrument (detector) name', status)
       call ftpkys(outunit, 'OBJECT', object, 
     +      'Name of observed object', status)
       call ftpkyf(outunit, 'EQUINOX', 1950.0, 1,
     +      'Equinox for coordinate system', status)
       call ftpkys(outunit, 'RADECSYS', 'FK4',
     +      'Stellar reference frame in use', status)

C Convert the source's galactic coordinates to 1950 celestial coordinates

       alpha = deg2rad * DBLE(srclong)
       delta = deg2rad * DBLE(srclat)
       xa = DCOS(alpha) * DCOS(delta)
       ya = DSIN(alpha) * DCOS(delta)
       za = DSIN(delta)
       call gal2cel(xa, ya, za, xb, yb, zb)
       ra_obj = DATAN2(yb, xb) * rad2deg
       ra_obj = DMOD(ra_obj+360.D0, 360.D0)
       dec_obj = DASIN(zb) * rad2deg

       call ftpkyg(outunit, 'RA_OBJ', ra_obj, 3,
     +      'Right Ascension of object (degrees)', status)
       call ftpkyg(outunit, 'DEC_OBJ', dec_obj, 3,
     +      'Declination of object (degrees)', status)

       call ftpkys(outunit, 'DATE-OBS', '01/01/69', 
     +      'Date of observation start (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-OBS', '00:00:00',
     +      'Time of observation start (hh:mm:ss)', status)
       call ftpkys(outunit, 'DATE-END', '01/01/80',
     +      'Date of observation stop (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-END', '23:59:59',
     +      'Time of observation stop (hh:mm:ss)', status)
       call wrthis(outunit, minflux, maxflux, maxerr, 
     +      spincheck, pointcheck, weight, backopt, collim, 
     +      timestep, long_cnr1, lat_cnr1, long_cnr2, lat_cnr2,
     +      status)
       call ftpdef(outunit, bitpix, naxis, naxes, pcount, gcount, 
     +      status)

C Create the binary table extension

       call ftcrhd(outunit, status)
       do 100 i = 1, 10
         tform(i) = ' '
         ttype(i) = ' '
         tunit(i) = ' '
 100   continue
       nrows = numofrows
       tfield = 7
       extnam = 'RATE'

       tform(1) = '1D'
       ttype(1) = 'TIME'
       tunit(1) = 'd'

       tform(2) = '1E'
       ttype(2) = 'RATE'
       tunit(2) = 'count/s'

       tform(3) = '1E'
       ttype(3) = 'ERROR'
       tunit(3) = 'count/s'

       tform(4) = '1E'
       ttype(4) = 'CHISQR'

       tform(5) = '1J'
       ttype(5) = 'DEGFREE'

       tform(6) = '1E'
       ttype(6) = 'BACKV'
       tunit(6) = 'count/s'

       tform(7) = '1E'
       ttype(7) = 'BACKE'
       tunit(7) = 'count/s'

       call ftphbn(outunit, nrows, tfield, ttype, tform, tunit, extnam,
     +      pcount, status)
       call ftbdef(outunit, tfield, tform, pcount, nrows, status)

       call ftmkys(outunit, 'ttype1', 'TIME',
     +      'Time of measurement (modified julian date)', status)
       call ftmkys(outunit, 'ttype2', 'RATE',
     +	    'Data from the detector channel', status)
       call ftmkys(outunit, 'ttype3', 'ERROR', 
     +      'Error in counting rate', status)
       call ftmkys(outunit, 'ttype4', 'CHISQR', 
     +      'Minimum chi squared of source fit', status)
       call ftmkys(outunit, 'ttype5', 'DEGFREE', 
     +      'Degrees of freedom in the fit', status)
       call ftmkys(outunit, 'ttype6', 'BACKV', 
     +      'Background trend from fit', status)
       call ftmkys(outunit, 'ttype7', 'BACKE', 
     +      'Error in background trend', status)
       call ftpkys(outunit, 'ORIGIN', 'HEASARC/NASA/GSFC',
     +      'Origin of this FITS file', status)
       call ftpkys(outunit, 'CREATOR', taskname,
     +      'HEASARC/GSFC FTOOL which created FITS file', status)
       call ftpdat(outunit, status)
       call ftpkys(outunit, 'TELESCOP', 'Vela 5B',
     +      'Telescope (mission) name', status)
       call ftpkys(outunit, 'INSTRUME', 'XC',
     +      'Instrument (detector) name', status)
       call ftpkys(outunit, 'OBJECT', object,
     +      'Name of observed object', status)
       call ftpkyf(outunit, 'EQUINOX', 1950.0, 1,
     +      'Equinox for coordinate system', status)
       call ftpkys(outunit, 'RADECSYS', 'FK4',
     +      'Stellar reference frame in use', status)
       call ftpkyg(outunit, 'RA_OBJ', ra_obj, 3,
     +      'Right Ascension of object (degrees)', status)
       call ftpkyg(outunit, 'DEC_OBJ', dec_obj, 3,
     +      'Declination of object (degrees)', status)
       call ftpkys(outunit, 'DATE-OBS', '01/01/69', 
     +      'Date of observation start (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-OBS', '00:00:00',
     +      'Time of observation start (hh:mm:ss)', status)
       call ftpkys(outunit, 'DATE-END', '01/01/80',
     +      'Date of observation stop (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-END', '23:59:59',
     +      'Time of observation stop (hh:mm:ss)', status)
       call ftpkyg(outunit, 'TSTART', 0.0D0, 8, 
     +      'Start of observation (Modified Julian Date)', status)
       call ftpkyg(outunit, 'TSTOP', 0.0D0, 8, 
     +      'End of observation (Modified Julian Date)', status)
       call ftpkyg(outunit, 'TIMEDEL', timestep, 8, 
     +      'Size of data bins (days)', status)
       if (bary) then
         call ftpkys(outunit,'TIMEREF','SOLARSYS',
     +        'Barycentric corrections applied to TIME',status)
       else
         call ftpkys(outunit,'TIMEREF','SATELLITE',
     +        'No barycentric corrections applied to TIME',status)
       endif
       call ftpkys(outunit, 'TIMEUNIT', 'd', 
     +      'Unit for TSTART, TSTOP, TIMEDEL', status)
       call ftpkys(outunit, 'TIMESYS', 'MJD',
     +      'Time arguments in Modified Julian Dates', status)
       call ftpkyd(outunit, 'COUFLU', 4.5D-10, 1,
     +      '3-12 keV, Crab Spectrum ergs/sq-cm/sec', status)
       if (backopt .eq. 0) then
         call ftpkyl(outunit, 'BACKAPP', .FALSE.,
     +        'No background subtracted', status)
       else
         call ftpkyl(outunit, 'BACKAPP', .TRUE.,
     +        'Background is subtracted', status)
       endif
       if (collim) then 
         call ftpkyl(outunit, 'VIGNAPP', .TRUE.,
     +      'Vignetting or collimator corrections applied', status)
       else
         call ftpkyl(outunit, 'VIGNAPP', .FALSE.,
     +      'No vignetting or collimator corrections applied', status)
       endif
       call ftpkyl(outunit, 'DEADAPP', .TRUE.,
     +      'Deadtime corrections applied', status)
       call ftpkys(outunit, 'HDUCLASS', 'OGIP',
     +             'Format conforms to OGIP standards', status)
       call ftpkys(outunit, 'HDUCLAS1', 'LIGHTCURVE',
     +      'Light curve dataset (OGIP memo 93/003)', status)
       call ftpkys(outunit, 'HDUCLAS2', 'RATE',
     +      'Light curve data stored as counts/s', status)
       if (backopt .ne. 0) then
          call ftpkys(outunit, 'HDUCLAS3', 'NET',
     +      'Background subtracted light curve', status)
       endif
       if (channel .eq. 1) then
          call ftpkyj(outunit, 'E_MIN', 3, 'Lower energy range (keV)', 
     +         status)
       else
          call ftpkyj(outunit, 'E_MIN', 6, 'Lower energy range (keV)', 
     +         status)
       endif
       call ftpkyj(outunit, 'E_MAX', 12, 'Upper energy range (keV)', 
     +      status)
       call ftpkys(outunit, 'EUNIT', 'keV', 
     +      'Units for E_MIN and E_MAX', status)

 999   return

       end

C----------------------------------------------------------------------------
C This subroutine writes in a single row of light curve data into the 
C binary table extension.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0  7 Mar 1995
C          1.1  31 Jan 1996  BACKV and BACKE are single elements instead
C                              of three element vector

      subroutine lcwrite(outunit, row, time, counts, error,
     +           chisq, ndof, backv, backe, status)

      implicit none

      integer outunit, row, ndof, status
       
      real counts, error, chisq, backv, backe

      double precision time

      call ftpcld(outunit, 1, row, 1, 1, time, status)
      call ftpcle(outunit, 2, row, 1, 1, counts, status)
      call ftpcle(outunit, 3, row, 1, 1, error, status)
      call ftpcle(outunit, 4, row, 1, 1, chisq, status)
      call ftpclj(outunit, 5, row, 1, 1, ndof, status)
      call ftpcle(outunit, 6, row, 1, 1, backv, status)
      call ftpcle(outunit, 7, row, 1, 1, backe, status)

      return

      end

C----------------------------------------------------------------------
C After all the data has been written into the data arrays, move all the 
C rows which contain data to the front of the file and delete all the  
C rows after the last row once the move is complete.  This DOES NOT shrink
C the FITS file (at least in the current realization of FITSIO).
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C   Vers. 0.9  28 Feb 1995
C         0.91 10 Mar 1995  Move all good rows to the top of the file, 
C                            then delete all the rows after them.
C         1.1  11 Dec 1995  Reads begin and end times from rate file
C         1.2  30 Jan 1996  Removes all NULL lines instead of leaving 
C                            the last line
C         1.21 31 Jan 1996  BACKV and BACKE are single elements instead
C                            of three element vector


      subroutine lcclose(mapunit, imax, jmax, status)

      implicit none

      logical anyf

      integer mapunit, status, numofrows, imax, jmax
      integer degfree, year, month, day, hour, min
      integer row, i, hdutype

      real rate, error, chisq, backv, backe, sec

      double precision begintime, endtime, jd, mjd

      character(8) startdate, starttime, stopdate, stoptime
      character(70) comment
      character(80) message

      row = 0
      call ftgkyj(mapunit, 'NAXIS2', numofrows, comment, status)
      do 300 i = 1, numofrows
         call ftgcvj(mapunit, 5, i, 1, 1, 0, degfree, anyf, status)
 200     if (degfree .ne. 0) then
            row = row + 1
            if (row .ne. i) then 
               call ftgcvd(mapunit, 1, i, 1, 1, 0.0D0, mjd, 
     +           anyf, status)
               call ftgcve(mapunit, 2, i, 1, 1, -100.0, rate,
     +           anyf, status)
               call ftgcve(mapunit, 3, i, 1, 1, -100.0, error,
     +           anyf, status)
               call ftgcve(mapunit, 4, i, 1, 1, -100.0, chisq,
     +           anyf, status)
               call ftgcve(mapunit, 6, i, 1, 1, -100.0, backv,
     +           anyf, status)
               call ftgcve(mapunit, 7, i, 1, 1, -100.0, backe,
     +           anyf, status)
               call ftpcnd(mapunit, 1, row, 1, 1, mjd, 0.0D0, status)
               call ftpcne(mapunit, 2, row, 1, 1, rate, -100.0, 
     +           status)
               call ftpcne(mapunit, 3, row, 1, 1, error, -100.0,
     +           status)
               call ftpcne(mapunit, 4, row, 1, 1, chisq, -100.0,
     +           status)
               call ftpcnj(mapunit, 5, row, 1, 1, degfree, 0, status)
               call ftpcne(mapunit, 6, row, 1, 1, backv, -100.0, 
     +           status)
               call ftpcne(mapunit, 7, row, 1, 1, backe, -100.0, 
     +           status)
            endif
         endif
 300  continue
      if (row .lt. numofrows)
     +   call ftdrow(mapunit, row + 1, numofrows - row, status)
      
C Modify the start and stop time keywords

      call ftgcvd(mapunit, 1, 1, 1, 1, 0.0D0, begintime, anyf, status)
      jd = begintime + 2400000.5D0
      call jd2ut(jd, year, month, day, hour, min, sec)
      write(startdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
      write(starttime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)
      call ftgcvd(mapunit, 1, row, 1, 1, 0.0D0, endtime, anyf, 
     +     status)
      jd = endtime + 2400000.5D0
      call jd2ut(jd, year, month, day, hour, min, sec)
      write(stopdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
      write(stoptime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)

      call ftmkys(mapunit, 'DATE-OBS', startdate, 
     +      'Date of observation start (dd/mm/yy)', status)
      call ftmkys(mapunit, 'TIME-OBS', starttime,
     +      'Time of observation start (hh:mm:ss)', status)
      call ftmkys(mapunit, 'DATE-END', stopdate,
     +      'Date of observation stop (dd/mm/yy)', status)
      call ftmkys(mapunit, 'TIME-END', stoptime,
     +      'Time of observation stop (hh:mm:ss)', status)
      call ftmkyg(mapunit, 'TSTART', begintime, 8, 
     +     'Start of observation (Modified Julian Date)', status)
      call ftmkyg(mapunit, 'TSTOP', endtime, 8, 
     +     'End of observation (Modified Julian Date)', status)
      if (status .ne. 0) then
         message = ' Error modifying the time keyword values '
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', I3)') status
         call fcerr(message)
         go to 999
      endif

      call ftmrhd(mapunit, -1, hdutype, status)      
      call ftmkys(mapunit, 'DATE-OBS', startdate, 
     +      'Date of observation start (dd/mm/yy)', status)
      call ftmkys(mapunit, 'TIME-OBS', starttime,
     +      'Time of observation start (hh:mm:ss)', status)
      call ftmkys(mapunit, 'DATE-END', stopdate,
     +      'Date of observation stop (dd/mm/yy)', status)
      call ftmkys(mapunit, 'TIME-END', stoptime,
     +      'Time of observation stop (hh:mm:ss)', status)
      if (status .ne. 0) then
         message = ' Error modifying the time keyword values '
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', I3)') status
         call fcerr(message)
         go to 999
      endif

      call ftclos(mapunit, status)

 999  return

      end
C----------------------------------------------------------------------------
C This subroutine write the Vela 5B History information, including the 
C options used to generate the FITS light curve
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0  15 Aug 1995
C          1.1  12 Sep 1995  Altered for 20 sources

      subroutine wrthis(outunit, minflux, maxflux, maxerr, 
     +           spincheck, pointcheck, weight, backopt, 
     +           collim, timestep,long_cnr1, lat_cnr1, 
     +           long_cnr2, lat_cnr2, status)

      implicit none

C Common block declarations

      common /SOURCE/ long_src, lat_src, longcntr, latcntr, longmin, 
     +  longmax, latmin, latmax, longavg, latavg, counts, backgnd, 
     +  frate, numpnts, nsrc, imax, jmax
      common /NAME/ sourcename

      integer nsrc, imax, jmax, numpnts(20,20)
      real long_src(20), lat_src(20)
      real longcntr(20,20), latcntr(20,20)
      real counts(20,20), backgnd(20,20)
      real longmin, longmax, latmin, latmax, longavg, latavg, frate
      character(16) sourcename(20)

      common /TASK/ taskname
      character(40) taskname

C Local variables

      logical spincheck, pointcheck, weight, collim 

      integer outunit, i, backopt, status

      real minflux, maxflux, maxerr
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2

      double precision timestep

      character(65) hisheader(6), insdes1(9), insdes2(12)
      character(65) insper1(14), insper2(8)
      character(65) deconv1(14), deconv2(9), deconv3(6)
      character(65) hischar1(7), hischar2(5), hischar3(14)
      character(65) noback(2), linback(2), sinback(9)
      character(65) templine

      data hisheader /
     +'Data for this source were obtained from the Vela 5B all-sky',
     +'XC detector. The comments given below include the following:',
     +'    (1) general description of the instrument,',
     +'    (2) discussion of instrument performance characteristics,',
     +'and (3) details concerning the generation of this particular',
     +'        light curve. ' /

      data insdes1 /
     +'The Vela 5B nuclear test detection satellite was placed in',
     +'a nearly circular orbit at a geocentric distance of ',
     +'~118,000 km on 23 May 1969; the orbital period was ~112 ',
     +'hours.  The satellite rotated about its spin axis with a ',
     +'~64-sec period. The X-ray detector was located ~90 degrees',
     +'from the spin axis, and so covered the celestial sphere twice',
     +'per satellite orbit. Data were telemetered in 1-sec count',
     +'accumulations. Vela 5B operated until 19 June 1979, although',
     +'telemetry tracking was poor after mid-1976.' /

      data insdes2 /
     +'The scintillation X-ray detectors (XC) aboard Vela 5B',
     +'consisted of two 1-mm-thick NaI(Tl) crystals mounted on',
     +'photomultiplier tubes and covered by 5-mil-thick beryllium',
     +'windows. Electronic thresholds provided two energy channels,',
     +'3-12 keV and 6-12 keV.  In front of each crystal was a slat',
     +'collimator providing a FWHM aperture of ~6.1x6.1 degrees.',
     +'The effective detector area was ~26 sq-cm. Sensitivity to ',
     +'celestial sources was severely limited by the intrinsic ',
     +'detector background of ~36 cts/sec.  The Vela 5B X-ray ',
     +'detector yielded ~40 cts/sec for the Crab, so 1 Vela ct/sec',
     +'~25 UFU ~4.5E-10 ergs/sq-cm/sec in the 3-12 keV response',
     +'band.' /

      data insper1 /
     +'One important detector performance characteristic which',
     +'affects the Vela 5B data is a gain variation due to a ',
     +'~60 deg C satellite temperature change from one side of the',
     +'orbit to the other. If the data for a source were taken when',
     +'the satellite was at one of its temperature extremes, a',
     +'profound modulation is introduced into the count rate at the',
     +'56-hour timescale between observation sequences of the',
     +'source.  In addition, the amplitude of the effect is',
     +'modulated by the ~300-day precession period of the Vela 5B',
     +'orbit. Lack of pre-launch testing precludes any quantitative',
     +'post-launch compensation. A temperature time history is',
     +'available to HEASARC users in a FITS file (VELA_TEMP) so',
     +'that they may check any suspicious source data against the',
     +'known times of temperature extremes.' /

      data insper2 /
     +'The time history of the Crab detected flux decreased by ~15%',
     +'between 1969 and 1979. It is believed that this decrease is',
     +'due to a gain change in the XC detector as it aged. No',
     +'attempt to correct for this trend has been made in the data',
     +'processing.  Users who desire to do so, or who want to',
     +'express detected source intensities in units of crabs, will',
     +'have to access the FITS file containing the Crab data to',
     +'extract the necessary information.' /

      data deconv1 /
     +'Despite the large field of view, good aspect information',
     +'for the satellite (+/- 0.2 degrees) allows the deconvolution',
     +'of source intensities in moderately crowded regions of the ',
     +'sky.  We have developed a computer code to perform this ',
     +'deconvolution (hereafter referred to as the ''mapping ',
     +'program'') based on algorithms discussed by Priedhorsky, ',
     +'W.C., Terrel, J., and Holt, S.S. (1983, Ap.J., 270, 233).',
     +'The mapping program divides the region of interest into a ',
     +'grid of 2 degree x 2 degree bins.  A detector response is ',
     +'determined at each grid location due to a source at a given',
     +'location; this is done for all k sources in the mapped region.',
     + 'A residual background fit is performed, modeled by',
     +' ',
     +'         background = b0 + b1*lii + b2*bii,' /

      data deconv2 /
     +'where lii, bii are the galactic latitudes and longitudes of',
     +'the centers of the 2 degree x 2 degree bin elements.  The ',
     +'mapping program then fits predicted count rate from sources ',
     +'and background to the observed intensity map.  The best fit ',
     +'is determined by minimizing chi-squared using the criterion',
     +' ',
     +'                d(chi-squared)',
     +'                --------------   = 0.0',
     +'                   d(a(k))' /

      data deconv3 /
     +'where a(k) is the predicted count rate for source k, for all',
     +'k.  The output of the mapping program is time, intensity for',
     +'each source, intensity error for each source, chi-squared of',
     +'the fit to the map, number of degrees of freedom for each map,',
     +'the residual background counts removed from the map, and the ',
     +'error associated with that residual background intensity fit.' /

       data hischar1 /
     +'The following sources were fitted in the map from which the',
     +'lightcurve in this datafile was extracted.  The galactic',
     +'latitudes and longitudes used for each source placed in the',
     +'map are also listed.',
     +' ',
     +'         Source            Lii            Bii',
     +'         ------            ---            ---' /

      data hischar2 /
     +'The boundaries of the region to which the deconvolution was',
     +'applied were defined to be the following:',
     +' ',
     +'            Lii            Bii',
     +'            ---            ---' /

      data hischar3 /
     +'The CHISQR column gives the resulting chi-squared reflecting',
     +'the goodness of fit to the whole map, not for any individual',
     +'source.',
     +' ',
     +'The DOF column contains the number of degrees of freedom for',
     +'the whole map.',
     +' ',
     +'BACKV (uniform background intensity) is the result, in cts/s,',
     +'of a background fit to the map region. It is allowed to vary',
     +'in both RA and DEC.',
     +' ',
     +'BACKE (uniform background error), also in cts/sec, is the ',
     +'error associated with the background fit based on the methods',
     +'discussed in the Priedhorsky et al. reference given earlier.' /

      data noback /
     +'Counting statistics have not been corrected for the ', 
     +'contribution of background events.' /

      data linback /
     +'A linear background model was removed from the counting', 
     +'statistics.' /

      data sinback /
     +'A background fit was done by fitting the function ', 
     +' ',
     +'    y(t) = a(t) cos(wt) + b(t) sin(wt) + c(t)',
     +' ',
     +'to a 64-point (~1 spin) span of data which was the average ', 
     +'of five consecutive 64-point spans. In general, this approach',
     +'to removing the background works well.  However, for some ',
     +'sources, removal of the fitted background leaves a slightly',
     +'negative average in the count rate' /


      do 100 i = 1, 6
         call ftphis(outunit, hisheader(i), status)
 100  continue
      call ftphis(outunit, ' ', status)
      call ftphis(outunit, 'INSTRUMENT DESCRIPTION', status)
      call ftphis(outunit, ' ', status)
      do 110 i = 1, 9
         call ftphis(outunit, insdes1(i), status)
 110  continue
      call ftphis(outunit, ' ', status)
      do 120 i = 1, 12
         call ftphis(outunit, insdes2(i), status)
 120  continue
      call ftphis(outunit, ' ', status)
      call ftphis(outunit, 'INSTRUMENT PERFORMANCE CHARACTERISTICS', 
     +     status)
      call ftphis(outunit, ' ', status)
      do 130 i = 1, 14
         call ftphis(outunit, insper1(i), status)
 130  continue
      call ftphis(outunit, ' ', status)
      do 140 i = 1, 8
         call ftphis(outunit, insper2(i), status)
 140  continue
      call ftphis(outunit, ' ', status)
      call ftphis(outunit, 'CROWDED REGION SOURCE DECONVOLUTION', 
     +     status)
      call ftphis(outunit, ' ', status)
      do 150 i = 1, 14
         call ftphis(outunit, deconv1(i), status)
 150  continue
      call ftphis(outunit, ' ', status)
      do 160 i = 1, 9
         call ftphis(outunit, deconv2(i), status)
 160  continue
      call ftphis(outunit, ' ', status)
      do 170 i = 1, 6
         call ftphis(outunit, deconv3(i), status)
 170  continue
      call ftphis(outunit, ' ', status)
      call ftphis(outunit, 'PARTICULARS CONCERNING THIS FILE', 
     +     status)
      call ftphis(outunit, ' ', status)
      do 180 i = 1, 7
         call ftphis(outunit, hischar1(i), status)
 180  continue

C List all the sources and their positions

      do 200 i = 1, nsrc
         write(templine,'(4X, A12, 9X, F6.2, 9X, F6.2)') sourcename(i),
     +         long_src(i), lat_src(i)
         call ftphis(outunit, templine, status)
 200  continue
      
      call ftphis(outunit, ' ', status)
      do 300 i = 1, 5
         call ftphis(outunit, hischar2(i), status)
 300  continue

C List the corners of the rectangular mapping region

      write(templine,'(10X, F6.2, 10X, F6.2)') long_cnr1, lat_cnr1
      call ftphis(outunit, templine, status)
      write(templine,'(10X, F6.2, 10X, F6.2)') long_cnr2, lat_cnr2
      call ftphis(outunit, templine, status)

C Specify the limits and criteria used to generate the light curves

      write(templine,'(''Points greater than '', F7.2,
     +     '' cts/s or less than '', F7.2, '' cts/s'')') maxflux, 
     +     minflux
      call ftphis(outunit, templine, status)
      write(templine, '(''or with a variance greater than '', F7.2, 
     +     '' cts/s were not included.'')') maxerr
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)
      write(templine,'(''Bintime = '', F5.2, '' days.'')') timestep
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)

C Record the spin checking and pointing error flag values

      if (spincheck) then
         call ftphis(outunit, 
     +'Data flagged for an unstable spin period were not included.', 
     +        status)
      else
         call ftphis(outunit, 
     +'Data flagged for an unstable spin period were included', 
     +        status)
      endif
      if (pointcheck) then
         call ftphis(outunit, 
     +        'Data flagged for pointing errors were not included.', 
     +        status)
      else
         call ftphis(outunit, 
     +        'Data flagged for pointing errors were included.', 
     +        status)
      endif
      call ftphis(outunit, ' ', status)

C Record the background model requested

      if (backopt .eq. 0) then
         call ftphis(outunit, noback(1), status)
         call ftphis(outunit, noback(2), status)
      else if (backopt .eq. 1) then
         call ftphis(outunit, linback(1), status)
         call ftphis(outunit, linback(2), status)
      else 
         do 400 i = 1, 9
            call ftphis(outunit, sinback(i), status)
 400     continue
      endif
      call ftphis(outunit, ' ', status)

C Record the request for collimator response correction

      if (collim) then
         call ftphis(outunit, 
     +    'Before binning, the 1 s records were corrected for the ',
     +        status) 
         call ftphis(outunit, 'collimator response.', status)
      else
         call ftphis(outunit,
     +'No correction for the collimator response function was made.',
     +        status)
      endif
      call ftphis(outunit, ' ', status)

C Record the use of weighted or unweighted binning

      if (weight) then
         call ftphis(outunit, 
     +'In summing the data into bins, data were weighted according ',
     +        status)
         call ftphis(outunit, 'to the relative error.', status)
      else
         call ftphis(outunit, 
     +'Data were not weighted by their relative errors when binning',
     +        status)
         call ftphis(outunit, 'the data.', status)
      endif
      call ftphis(outunit, ' ', status)

      do 500 i = 1, 14
         call ftphis(outunit, hischar3(i), status)
 500  continue

      call ftphis(outunit, ' ', status)
      templine = 'Program version: ' // taskname
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)

      return

      end
