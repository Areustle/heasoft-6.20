C This subroutine takes the 1 second data read in by previous routines and
C bins it according to the directions given by the user.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0              Original VAX program by Laura Whitlock (USRA)
C        2.0   5 Jan 1995  FTOOL version of BINIT
C        2.1   9 Jan 1995  Linked with FITS routines to make WRITEDATA
C        2.2  27 Jan 1995  Made generic for shared use with Vela 5B FTOOLS
C        2.21 13 Jul 1995  Modified to match new OGIP standard
C        2.22 15 Aug 1995  Added HISTORY comments

      subroutine wrvlc(fitsfile, fitsunit, object, longitude,
     +           latitude, channel, stimbin, backopt, binopt, collim, 
     +           bary, weight, mjdtime, cnts, backgnd, elements, 
     +           status)

      implicit none

      logical bary, collim, weight

      integer fitsunit, channel, backopt, binopt, elements, status
      integer itryit, start, numbin, i, pnts

      real longitude, latitude, stimbin
      real cnts(elements), backgnd(elements)
      real bincnts, binbkg, sumcnts, sumbkg, sumbinv, sumcinv

      double precision mjdtime(elements), dtimbin, bintime
      double precision firsttime, lasttime, timedel

      character(16) object
      character(20) fitsfile
      character(80) message
                 

C Initialize the FITS headers, etc.

      firsttime = mjdtime(1)
      lasttime = mjdtime(elements)
      call init_fits(fitsfile, fitsunit, firsttime, lasttime, 
     +     object, longitude, latitude, channel, binopt, backopt, 
     +     collim, bary, status)
      if (status .ne. 0) then
         write(message, '('' Error creating '', A20)') fitsfile
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', I3)') status
         call fcecho(message)
         go to 999
      endif

C If no binning is requested (binning option 0), go right to the FITS
C light curve file creation

      if (binopt .eq. 0) then
         dtimbin = 1.0D0 / 86400.0D0
         do 5 i = 1, elements
            call write_fits(fitsunit, i, mjdtime(i), cnts(i), 
     +           backgnd(i), binopt, dtimbin, 1, status)
            if (status .ne. 0) then
               write(message, '('' Error writing data to '', A20)') 
     +              fitsfile
               call fcecho(message)
               call ftgmsg(message)
               call fcecho(message)
               write(message, '('' FITSIO status = '', I3)') status
               call fcecho(message)
               go to 999
            endif
 5       continue
         call close_fits(fitsunit, elements, status)
         if (status .ne. 0) then
            write(message, '('' Error closing '', A20)') fitsfile
            call fcecho(message)
            call ftgmsg(message)
            call fcecho(message)
            write(message, '('' FITSIO status = '', I3)') status
            call fcecho(message)
            go to 999
         endif
         go to 999
      endif

      dtimbin = DBLE(stimbin) / 86400.0D0

 10   sumcnts = cnts(1)
      sumbkg = backgnd(1)
      sumcinv = cnts(1) / backgnd(1)
      sumbinv = 1.0 / backgnd(1)
      start = 2
      numbin = 1
       
 20   do 30 i = start, elements
         if (backgnd(i) .eq. 0) go to 30
         if (binopt .eq. 1) then
            if (mjdtime(i) .gt. (mjdtime(start) + 10.0D0)) go to 40
         else if (binopt .eq. 2) then
            if ((mjdtime(i) - mjdtime(i-1)) .gt. 1.0D0) go to 40
         else if ((binopt .eq. 3) .or. (binopt .eq. 4)) then
            if ((mjdtime(i) - mjdtime(start-1)) .ge. dtimbin)
     +          go to 40
         endif
         sumcnts = cnts(i) + sumcnts
         sumbkg = backgnd(i) + sumbkg
         sumcinv = (cnts(i) / backgnd(i)) + sumcinv
         sumbinv = (1.0 / backgnd(i)) + sumbinv
 30   continue

 40   firsttime = mjdtime(start-1)
      lasttime = mjdtime(i-1) + (1.0D0/86400.0D0)
      pnts = i - (start - 1)
      bintime = (firsttime + lasttime) / 2.0D0
      timedel = lasttime - firsttime
      
C Bin counts and background according to user request for weighted or 
C  straight binning.

      if (weight) then
         bincnts = sumcinv / sumbinv
         binbkg = 1.0 / SQRT(sumbinv)
      else
         bincnts = sumcnts / REAL(pnts)
         binbkg = SQRT(sumbkg / (REAL(pnts)**2))
      endif

      itryit = 1
      call write_fits(fitsunit, numbin, bintime, bincnts, binbkg, 
     +     binopt, timedel, pnts, status)
      if (status .ne. 0) then
         write(message, '('' Error writing data to '', A20)') 
     +        fitsfile
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', I3)') status
         call fcecho(message)
         go to 999
      endif

      if (i .lt. elements) then
         sumcnts = cnts(i)
         sumbkg = backgnd(i)
         sumcinv = cnts(i) / backgnd(i)
         sumbinv = 1.0 / backgnd(i)
         start = i + 1
         numbin = numbin + 1
         go to 20
      else
         call close_fits(fitsunit, numbin, status)
         if (status .ne. 0) then
            write(message, '('' Error closing '', A20)') fitsfile
            call fcecho(message)
            call ftgmsg(message)
            call fcecho(message)
            write(message, '('' FITSIO status = '', I3)') status
            call fcecho(message)
            go to 999
         endif
      endif
       
 999  return

      end


C---------------------------------------------------------------------------
C This subroutine initializes a FITS light curve file from the Vela 5B FITS
C data.
C
C Author: Jesse Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0   8 Dec 1994
C          1.1  26 Jan 1995  Made generic to share routines with other Vela 5B
C                             FTOOLS
C          1.2  11 Aug 1995  TIMEDEL in header for unbinned data

      subroutine init_fits(fitsfile, outunit, firsttime, lasttime, 
     +           object, long_src, lat_src, channel, binopt, backopt, 
     +           collim, bary, status)

C Common block declarations

       common /TASK/ taskname
      
       character(40) taskname

C Local variable declarations

       logical simple, extend, collim, bary

       integer bitpix, naxis, naxes(3), nrows, pcount, gcount, tfield
       integer outunit, status, i
       integer year, month, day, hour, min, channel, backopt, binopt

       real long_src, lat_src, sec
 
       double precision ra_obj, dec_obj, alpha, delta
       double precision xa, ya, za, xb, yb, zb
       double precision firsttime, lasttime, jd
       double precision pi, deg2rad, rad2deg

       character(8) extnam, startdate, starttime, stopdate, stoptime
       character(16) ttype(5), tform(5), tunit(5), object
       character(20) fitsfile

C Assign some constants used throughout the FITS file creation

       simple = .TRUE.
       bitpix = 8
       extend = .TRUE.
       naxis = 0
       pcount = 0
       gcount = 1

       pi = 3.14159265359D0
       deg2rad = pi / 180.0D0
       rad2deg = 180.0D0 / pi

C  Create the new FITS file and write the primary header keywords

       call ftinit(outunit, fitsfile, 2880, status)

       call ftphpr(outunit, simple, bitpix, naxis, naxes, pcount,
     +       gcount, extend, status)
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
     &      'Stellar reference frame in use', status)

C Convert the source's galactic coordinates to 1950 celestial coordinates

       alpha = deg2rad * DBLE(long_src)
       delta = deg2rad * DBLE(lat_src)
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

       jd = firsttime + 2400000.5D0
       call jd2ut(jd, year, month, day, hour, min, sec)
       write(startdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
       write(starttime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)
       jd = lasttime + 2400000.5D0
       call jd2ut(jd, year, month, day, hour, min, sec)
       write(stopdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
       write(stoptime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)

       call ftpkys(outunit, 'DATE-OBS', startdate, 
     +      'Date of observation start (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-OBS', starttime,
     +      'Time of observation start (hh:mm:ss)', status)
       call ftpkys(outunit, 'DATE-END', stopdate,
     +      'Date of observation stop (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-END', stoptime,
     +      'Time of observation stop (hh:mm:ss)', status)

       call wrhist(outunit, status)
       call ftpdef(outunit, bitpix, naxis, naxes, pcount, gcount, 
     +      status)

C Create the binary table extension

       call ftcrhd(outunit, status)
       do 100 i = 1, 5
         tform(i) = ' '
         ttype(i) = ' '
         tunit(i) = ' '
 100   continue
       nrows = 1
       if (binopt .eq. 0) then
          tfield = 3
       else
          tfield = 5
       endif
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

       if (binopt .ne. 0) then 
          tform(4) = '1D'
          ttype(4) = 'TIMEDEL'
          tunit(4) = 'd'

          tform(5) = '1J'
          ttype(5) = 'POINTS'
       endif

       call ftphbn(outunit, nrows, tfield, ttype, tform, tunit, extnam,
     +      pcount, status)
       call ftbdef(outunit, tfield, tform, pcount, nrows, status)

       call ftmkys(outunit, 'ttype1', 'TIME',
     +      'Time of measurement (modified julian date)', status)
       call ftmkys(outunit, 'ttype2', 'RATE',
     +	    'Data from the detector channel', status)
       call ftmkys(outunit, 'ttype3', 'ERROR', 
     +      'Error in counting rate', status)
       if (binopt .ne. 0) then
          call ftmkys(outunit, 'ttype4', 'TIMEDEL', 
     +         'Length of the bin (days)', status)
          call ftmkys(outunit, 'ttype5', 'POINTS', 
     +         'Number of points in the bin', status)
       endif
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
       call ftpkys(outunit, 'DATE-OBS', startdate,
     +      'Date of observation start (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-OBS', starttime,
     +      'Time of observation start (hh:mm:ss)', status)
       call ftpkys(outunit, 'DATE-END', stopdate,
     +      'Date of observation stop (dd/mm/yy)', status)
       call ftpkys(outunit, 'TIME-END', stoptime,
     +      'Time of observation stop (hh:mm:ss)', status)
       call ftpkyg(outunit, 'TSTART', firsttime, 6, 
     +      'Start of observation (Modified Julian Date)', status)
       call ftpkyg(outunit, 'TSTOP', lasttime, 6, 
     +      'End of observation (Modified Julian Date)', status)
       if (bary) then
         call ftpkys(outunit,'TIMEREF','SOLARSYS',
     +        'Barycentric corrections applied to TIME',status)
       else
         call ftpkys(outunit,'TIMEREF','SATELLITE',
     +        'No barycentric corrections applied to TIME',status)
       endif
       if (binopt .eq. 0)
     +    call ftpkyg(outunit, 'TIMEDEL', 1.0D0 / 86400.0D0, 7,
     +         'Time span of the data points', status)
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
C  Version 1.0  9 Jan 1995
C          1.1 11 Aug 1995  No TIMEDEL column for unbinned data

       subroutine write_fits(outunit, row, time, counts, error,
     +            binopt, timedel, pnts, status)

       integer outunit, binopt, row, pnts, status
       
       real counts, error

       double precision time, timedel


       call ftpcld(outunit, 1, row, 1, 1, time, status)
       call ftpcle(outunit, 2, row, 1, 1, counts, status)
       call ftpcle(outunit, 3, row, 1, 1, error, status)
       if (binopt .ne. 0) then 
          call ftpcld(outunit, 4, row, 1, 1, timedel, status)
          call ftpclj(outunit, 5, row, 1, 1, pnts, status)
       endif

       return

       end


C----------------------------------------------------------------------------
C This subroutine closes the FITS file, writing in the appropriate values
C for times, dates, number of rows, etc.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0  9 Jan 1995

       subroutine close_fits(outunit, numrows, status)

       integer outunit, numrows, bytelen, status

       character(80) comment

       call ftmkyj(outunit, 'NAXIS2', numrows, 
     +      'Number of rows in the binary table extension', status)
       call ftgkyj(outunit, 'NAXIS1', bytelen, comment, status)
       bytelen = bytelen * numrows
       call ftddef(outunit, bytelen, status)

       call ftclos(outunit, status)

       return

       end

C----------------------------------------------------------------------------
C This subroutine write the Vela 5B History information, including the 
C options used to generate the FITS light curve
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.9  14 Aug 1995

      subroutine wrhist(outunit, status)

C Common block declarations

       common /SOURCE/ begintime, endtime, long_src, lat_src, 
     +        searchrad, minflux, maxflux, maxerr, stimbin, 
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       
       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime

      common /TASK/ taskname
      
      character(40) taskname

C Local variables

      integer outunit, i, status

      character(65) hisheader(6), insdes1(9), insdes2(12)
      character(65) insper1(14), insper2(8), hischar(8)
      character(65) noback(2), linback(2), sinback(10)
      character(65) nobin(5), befbin(5), tenbin(3), natbin(9)
      character(65) resbin(2), errsour(3), templine

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

      data hischar /   
     +'This FITS light curve file was generated using the FVELALC',
     +'FTOOL program.  The FVELALC light curve extraction program',
     +'uses the Vela 5B coordinate ordered FITS files available at',
     +'the HEASARC.  These files have been processed to remove a',
     +'small number of data points which were corrupted during a',
     +'data transfer between computers with incompatible byte',
     +'storage formats.  Slightly less than 0.1% of the original',
     +'data was affected by this problem.' /

      data noback /
     +'-- Counting statistics have not been corrected for the ', 
     +'   contribution of background events.' /

      data linback /
     +'-- A linear background model was removed from the counting', 
     +'   statistics.' /

      data sinback /
     +'-- A background fit was done by fitting the function ', 
     +' ',
     +'       y(t) = a(t) cos(wt) + b(t) sin(wt) + c(t)',
     +' ',
     +'   to a 64-point (~1 spin) span of data which was the', 
     +'   average of five consecutive 64-point spans. In general,',
     +'   this approach to removing the background works',
     +'   well. However, for some sources, removal of the fitted',
     +'   background leaves a slightly negative average in the',
     +'   count rate.' /

      data nobin /
     +'-- The 1-sec data are corrected to barycentric time. Results',
     +'   are known to be good to 1 part in 1000. No attempt was',
     +'   made to correct for the timing error introduced by the',
     +'   deterioration of the satellite orbit over the ten years of',
     +'   operation.' /

      data befbin /
     +'-- Before binning, the 1-sec data were corrected to',
     +'   barycentric time. Results are known to be good to 1 part',
     +'   in 1000. No attempt was made to correct for the timing',
     +'   error introduced by the deterioration of the satellite',
     +'   orbit over the ten years of operation.' /

      data tenbin /
     +'-- The values for cts/sec given in this file were generated',
     +'   by computing the weighted average value of the cts/sec',
     +'   observed during a 10 day period.' /

      data natbin /
     +'-- The values for cts/sec given in this file were generated',
     +'   by computing the weighted average value of the cts/sec',
     +'   observed during a single observation sequence. An',
     +'   observation sequence is defined as the span of time',
     +'   during 1/2 of each satellite orbit that a given source is',
     +'   in the field-of-view of the XC detector. Typically, these',
     +'   sequences occur every 56 hours and last for 2-10',
     +'   hours. Sources near the pole, such as Cen A, are viewed',
     +'   for longer periods of time.' /

      data resbin /
     +'-- The values for cts/sec given in this file were generated',
     +'   by computing the weighted average value of the cts/sec' /

      data errsour /
     +'-- The error associated with each datum is the sum of errors',
     +'   introduced by background removal, collimator response ',
     +'   correction, and counting statistics.' /


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
      call ftphis(outunit, 'PARTICULARS CONCERNING THIS FILE', status)
      call ftphis(outunit, ' ', status)
      do 150 i = 1, 8
         call ftphis(outunit, hischar(i), status)
 150  continue
      call ftphis(outunit, ' ', status)
      call ftphis(outunit,
     +'The FVELALC program was run with the following data', status)
      call ftphis(outunit, 'selection criteria:', status)
      call ftphis(outunit, ' ', status)

C Record the source position and selection criteria used

      templine = 'Program version: ' // taskname
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)
      call ftphis(outunit, '-- Source location defined as ', status)
      call ftphis(outunit, ' ', status)
      write(templine,'(''           L: '', F6.2, '' degrees'')')
     +     long_src
      call ftphis(outunit, templine, status)
      write(templine,'(''           B: '', F6.2, '' degrees'')')
     +     lat_src 
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)
      write(templine, '(''   Data were included for '', F4.2,
     +     '' degrees around the source'')') searchrad
      call ftphis(outunit, templine, status)
      write(templine,'(''   location.  Points greater than '', F7.2,
     +     '' cts/sec or less'')') maxflux
      call ftphis(outunit, templine, status)
      write(templine, '(''   than '', F7.2, '' cts/s or with a '',
     +     ''variance greater than'')') minflux
      call ftphis(outunit, templine, status)
      write(templine, '(''   '', F8.2, '' cts/sec were not '',
     +     ''included.'')') maxerr
      call ftphis(outunit, templine, status)
      call ftphis(outunit, ' ', status)

C Record the spin checking and pointing error flag values

      if (spincheck) then
         call ftphis(outunit, 
     +   '-- Data flagged for an unstable spin period were not', 
     +        status)
         call ftphis(outunit, '   included.', status)
      else
         call ftphis(outunit, 
     +   '-- Data flagged for an unstable spin period were included', 
     +        status)
      endif
      if (pointcheck) then
         call ftphis(outunit, 
     +   '   Data flagged for pointing errors were not included.', 
     +        status)
      else
         call ftphis(outunit, 
     +   '   Data flagged for pointing errors were included.', status)
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
         do 200 i = 1, 10
            call ftphis(outunit, sinback(i), status)
 200     continue
      endif
      call ftphis(outunit, ' ', status)

C Record the binning request made 

      if (binopt .eq. 0) then
         do 300 i = 1, 5
            call ftphis(outunit, nobin(i), status)
 300     continue
      else 
         do 400 i = 1, 5
            call ftphis(outunit, befbin(i), status)
 400     continue
         call ftphis(outunit, ' ', status)
         if (binopt .eq. 1) then 
            do 500 i = 1, 3
               call ftphis(outunit, tenbin(i), status)
 500        continue
         else if (binopt .eq. 2) then
            do 600 i = 1, 9
               call ftphis(outunit, natbin(i), status)
 600        continue
         else 
            call ftphis(outunit, resbin(1), status)
            call ftphis(outunit, resbin(1), status)
            write(templine, '(''   observed during a '', I6, 
     +           '' s period.'')') stimbin
            call ftphis(outunit, templine, status)
         endif
      endif
      call ftphis(outunit, ' ', status)

C Record the request for collimator response correction

      if (collim) then
         if (binopt .eq. 0) then
            call ftphis(outunit, 
     +'-- The 1 s records were corrected for the collimator ',
     +           status)
            call ftphis(outunit, '   response.', status)
         else
            call ftphis(outunit, 
     +'-- Before binning, the 1 s records were corrected for the ',
     +           status) 
            call ftphis(outunit, '   collimator response.', status)
         endif
      else
         call ftphis(outunit,
     +'-- No correction for the collimator response function was ',
     +        status)
         call ftphis(outunit, '   made.', status)
      endif
      call ftphis(outunit, ' ', status)

C Record the use of weighted or unweighted binning

      if (binopt .ne. 0) then
         if (weight) then
            call ftphis(outunit, 
     +'-- In summing the data into bins, data were weighted ',
     +           status)
            call ftphis(outunit, 
     +           '   according to the relative error.', status)
         else
            call ftphis(outunit, 
     +'-- Data were not weighted by their relative errors when ',
     +           status)
            call ftphis(outunit, '   binning the data.', status)
         endif
         call ftphis(outunit, ' ', status)
      endif

C Explain the source of the error values

      do 700 i = 1, 3
         call ftphis(outunit, errsour(i), status)
 700  continue

      return

      end

