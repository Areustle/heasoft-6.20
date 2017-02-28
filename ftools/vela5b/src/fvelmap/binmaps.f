C This subroutine takes the list of boxes, reads in the data from the FITS
C  files, places them in arrays, bins the data, writes the binned data to
C  to FITS, and finally cleans out all the NULL entries in the final FITS
C  map file.
C
C Author: Jesse S. Allen
C History:
C  Vers. 0.0   5 Dec 1994  First draft
C        0.9  22 Feb 1995  Precalculates the first event time
C        0.91 23 Mar 1995  Processes both channels at once
C        1.0  31 Oct 1995  Eliminated address array
C        1.1  18 Dec 1995  Added a minimum size requirement for the 
C                           dynamical memory arrays to avoid udmget bug

      subroutine binmaps(boxlist, lngbx1, latbx1, numofboxes, 
     +           status)

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1, 
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,  
     +       stimbin, backopt, iwp, jwp, spincheck, 
     +       pointcheck, weight
      
      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

      common /TASK/ taskname
      character(40) taskname

C Declarations for the use of dynamic memory
C  the following MEM common block definition is in the system iraf77.inc file
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C       data_type       value
C       logical         1
C       integer*2       3
C       Integer         4
C       Long Integer    5
C       Real            6
C       Double          7
C       Complex         8

C Local variables

      integer inunit, rwmode, blocksize, nhdu, hdutype, status
      integer ch1unit, ch2unit, icnt, ii, jj, numofboxes, npts1, npts2
      integer boxlist(400), rowlist(400), nx1, nx2, numelems
      integer ptcnt1, ptbkg1, pttim1, ptcnt2, ptbkg2, pttim2

      real lngbx1, latbx1

      double precision timestep, fch1time, fch2time

      character(20) boxname, ch1name, ch2name
      character(80) message


      inunit = 10
      ch1unit = 11
      ch2unit = 12
      rwmode = 0
      nhdu = 1
      timestep = stimbin/86400.0D0
      ch1name = 'mapdata_ch1.bin'
      ch2name = 'mapdata_ch2.bin'

      message = ' Finding the first Vela 5B event in time range '
      call fcecho(message)
      call firstevent(inunit, 1, boxlist, rowlist, numofboxes, 
     +     fch1time, status)
      call firstevent(inunit, 2, boxlist, rowlist, numofboxes, 
     +     fch2time, status)
      npts1 = ((endtime - fch1time) / timestep) + 1
      write(message,'('' First channel 1 event at MJD '', F12.6)') 
     +  fch1time
      call fcecho(message)
      npts2 = ((endtime - fch2time) / timestep) + 1
      write(message,'('' First channel 2 event at MJD '', F12.6)') 
     +  fch2time
      call fcecho(message)

      call mapinit(ch1unit, ch1name, 1, fch1time, timestep, npts1,
     +  lngbx1, latbx1, status)
      if (status .ne. 0) then
         write(message, '('' Error creating '', A20)') ch1name
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', i3)') status
         call fcecho(message)
         go to 999
      endif
      call mapinit(ch2unit, ch2name, 2, fch2time, timestep, npts2,
     +  lngbx1, latbx1, status)
      if (status .ne. 0) then
         write(message, '('' Error creating '', A20)') ch2name
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message,'('' FITSIO status = '', i3)') status
         call fcecho(message)
         go to 999
      endif

C Loop through the boxes, terminating if too many points are read in

      do 200 jj = 1, jwp
         do 100 ii = 1, iwp
            icnt = ii + ((jj - 1) * iwp) 
            write(boxname,'(''b'',I5.5,''.raw'')') boxlist(icnt)
            if (numofboxes .ge. 100) then
               write(message,'('' Reading box '',i3,'' of '', i3, 
     +              '' from '', a20)') icnt, numofboxes, boxname
            else if (numofboxes .ge. 10) then 
               write(message,'('' Reading box '',i2,'' of '', i2, 
     +              '' from '', a20)') icnt, numofboxes, boxname
            else
               write(message,'('' Reading box '',i1,'' of '', i1, 
     +              '' from '', a20)') icnt, numofboxes, boxname
            endif
            call fcecho(message)
            call ftopen(inunit, boxname, rwmode, blocksize, status)
            call ftmrhd(inunit, nhdu, hdutype, status)

            if (rowlist(icnt) .lt. 50) then
               numelems = 50
            else
               numelems = rowlist(icnt)
            endif
            pttim1 = 0
            ptcnt1 = 0
            ptbkg1 = 0
            pttim2 = 0
            ptcnt2 = 0
            ptbkg2 = 0
            call udmget(numelems, 7, pttim1, status)
            if (status .ne. 0) then
               message = 
     +         ' Error allocating dynamic memory for CH1 time array'
               call fcerr(message)
               go to 999
            endif
            call udmget(numelems, 6, ptcnt1, status)
            if (status .ne. 0) then
               message = 
     +         ' Error allocating dynamic memory for CH1 counts array'
               call fcerr(message)
               go to 999
            endif
            call udmget(numelems, 6, ptbkg1, status)
            if (status .ne. 0) then
               message = 
     +     ' Error allocating dynamic memory for CH1 background array'
               call fcerr(message)
               go to 999
            endif
            call udmget(numelems, 7, pttim2, status)
            if (status .ne. 0) then
               message = 
     +         ' Error allocating dynamic memory for CH2 time array'
               call fcerr(message)
               go to 999
            endif
            call udmget(numelems, 6, ptcnt2, status)
            if (status .ne. 0) then
               message = 
     +         ' Error allocating dynamic memory for CH2 counts array'
               call fcerr(message)
               go to 999
            endif
            call udmget(numelems, 6, ptbkg2, status)
            if (status .ne. 0) then
               message = 
     +     ' Error allocating dynamic memory for CH2 background array'
               call fcerr(message)
               go to 999
            endif

C Read in the data from the current box which
C matches the selection criteria, bin it, and write the resultant file to
C FITS.  The FITS light curve will use the box number as the object name.

            call rdbox(inunit,MEMD(pttim1),MEMR(ptcnt1),MEMR(ptbkg1),
     +           MEMD(pttim2),MEMR(ptcnt2),MEMR(ptbkg2),rowlist(icnt),
     +           nx1, nx2, status)
            if (status .ne. 0) then
               write(message,'('' Error reading data from '', A20)')
     +              boxname
               call fcerr(message)
               go to 999
            endif

            call mapdata(ch1unit, fch1time, timestep, weight, icnt,
     +           MEMD(pttim1), MEMR(ptcnt1), MEMR(ptbkg1), nx1, 
     +           status)
            if (status .ne. 0) then
               message = ' Error writing data to mapdata_ch1.bin '
               call fcecho(message)
               call ftgmsg(message)
               call fcecho(message)
               write(message,'('' FITSIO status = '', I3)') status
               call fcecho(message)
               call ftclos(inunit, status)
               call udmfre(pttim1, 7 ,status)
               call udmfre(ptcnt1, 6, status)
               call udmfre(ptbkg1, 6, status)
               call udmfre(pttim2, 7 ,status)
               call udmfre(ptcnt2, 6, status)
               call udmfre(ptbkg2, 6, status)
               go to 999
            endif
            call mapdata(ch2unit, fch2time, timestep, weight, icnt, 
     +           MEMD(pttim2), MEMR(ptcnt2), MEMR(ptbkg2), nx2, 
     +           status)
            if (status .ne. 0) then
               message = ' Error writing data to mapdata_ch2.bin '
               call fcecho(message)
               call ftgmsg(message)
               call fcecho(message)
               write(message,'('' FITSIO status = '', I3)') status
               call fcecho(message)
               call ftclos(inunit, status)
               call udmfre(pttim1, 7 ,status)
               call udmfre(ptcnt1, 6, status)
               call udmfre(ptbkg1, 6, status)
               call udmfre(pttim2, 7 ,status)
               call udmfre(ptcnt2, 6, status)
               call udmfre(ptbkg2, 6, status)
               go to 999
            endif

            call ftclos(inunit, status)
            call udmfre(pttim1, 7 ,status)
            call udmfre(ptcnt1, 6, status)
            call udmfre(ptbkg1, 6, status)
            call udmfre(pttim2, 7 ,status)
            call udmfre(ptcnt2, 6, status)
            call udmfre(ptbkg2, 6, status)

 100     continue
 200  continue

      call mapclose(ch1unit, iwp, jwp, fch2time, endtime, status)
      if (status .ne. 0) then
         write(message,'('' Error closing '', A20)') ch1name
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', i3)') status
         call fcecho(message)
         go to 999
      endif
      call mapclose(ch2unit, iwp, jwp, fch2time, endtime, status)
      if (status .ne. 0) then
         write(message,'('' Error closing '', A20)') ch2name
         call fcecho(message)
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', i3)') status
         call fcecho(message)
      endif

 999  return

      end

C----------------------------------------------------------------------------
C This subroutine reads in the data from a raw FITS file and stores it in
C the appropriate array after eliminating unwanted points, making background
C calculations, and correcting for collimator response.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 0.0  26 Jan 1995
C        0.9  22 Feb 1995  Modifications for FVELMAP routines
C        1.0  23 Mar 1995  Reads in both XC channels at once
C        1.1   9 Nov 1995  Reject background points for which no background
C                           was available from the NOS side.
C        1.2   5 Feb 1996  Modified rejection criteria to reject data with
C                           bad absolute values

      subroutine rdbox(inunit, mjd1, cnts1, bgnd1, mjd2, cnts2, bgnd2,
     +           totalrows, nx1, nx2, status)

      implicit none

C Common block variables

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1, 
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,  
     +       stimbin, backopt, iwp, jwp, spincheck, 
     +       pointcheck, weight
      
      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

C Local variables

      logical anyf

      integer inunit, status
      integer stabflag, pntflag, ipos
      integer row, totalrows, nx1, nx2
      
      real r, cts1, cts2, spinper, eradd, rawcnts
      real bkg1(3), bkg2(3), bvar1(3), bvar2(3)
      real cnts1(totalrows), cnts2(totalrows)
      real bgnd1(totalrows), bgnd2(totalrows)

      double precision obstime, mjd1(totalrows), mjd2(totalrows)

      character(80) message

      nx1 = 0
      nx2 = 0
      do 300 row = 1, totalrows
         call ftgcvd(inunit, 4,row,1,1,0.D0, obstime, anyf, status)
         call ftgcve(inunit, 7,row,1,1,0.0, cts1, anyf, status)
         call ftgcve(inunit, 8,row,1,3,0.0, bkg1, anyf, status)
         call ftgcve(inunit, 9,row,1,3,0.0, bvar1, anyf, status)
         call ftgcvj(inunit,10,row,1,1,0, ipos, anyf, status)
         call ftgcve(inunit,12,row,1,1,0.0, cts2, anyf, status)
         call ftgcve(inunit,13,row,1,3,0.0, bkg2, anyf, status)
         call ftgcve(inunit,14,row,1,3,0.0, bvar2, anyf, status)
         call ftgcvj(inunit,19,row,1,1,0, stabflag, anyf, status)
         call ftgcvj(inunit,20,row,1,1,0, pntflag, anyf, status)
         call ftgcve(inunit,21,row,1,1,0.0, spinper, anyf, status)
         if (status .ne. 0) then
            call ftgmsg(message)
            call fcecho(message)
            write(message, '('' FITSIO status = '', i3)') status
            call fcerr(message)
            go to 999
         endif

C Skip data flagged for stability or pointing error problems (if requested)
C and data which does not fall within the requested time range.

         if ((spincheck) .and. (stabflag .ne. 0)) go to 300
         if ((pointcheck) .and. (pntflag .ne. 0)) go to 300
         if ((obstime .lt. begintime) .or. (obstime .gt. endtime))
     +      go to 300

C Process the counter, background, and variance data
C Reject points with zero counts and points with invalid background
C values (absolute value is 32.75 for sin and cos terms, 
C 327.5 for linear, or less than 0 for linear)

 100    if ((cts1 .eq. 0) .or. 
     +      (abs(bkg1(1)) .ge. 32.74) .or. 
     +      (abs(bkg1(2)) .ge. 32.74) .or. 
     +      (bkg1(3) .ge. 327.5) .or. 
     +      (bkg1(3) .lt. 0.0)) go to 200

         nx1 = nx1 + 1
         mjd1(nx1) = obstime
         if (nx1 .gt. 1) then
            if (mjd1(nx1) .eq. mjd1(nx1-1)) nx1 = nx1 - 1
         endif
         r = 1.0
         eradd = 0.0
         rawcnts = cts1
         call background(cts1,bkg1,bvar1,eradd,backopt,ipos,spinper)
         cnts1(nx1) = cts1 / r
         bgnd1(nx1) = (rawcnts + eradd) / (r**2)
         if ((cnts1(nx1) .lt. minflux) .or. 
     +       (cnts1(nx1) .gt. maxflux) .or. 
     +       (bgnd1(nx1) .gt. maxerr)) nx1 = nx1 - 1

C Process the counter, background, and variance data
C Reject points with zero counts and points with invalid background
C values (absolute values of 32.75 for sin and cos terms, 327.5 for 
C linear, or less than 0.0 for linear)

 200     if ((cts2 .eq. 0) .or. 
     +       (abs(bkg2(1)) .ge. 32.74) .or. 
     +       (abs(bkg2(2)) .ge. 32.74) .or. 
     +       (bkg2(3) .ge. 327.5) .or.
     +       (bkg2(3) .lt. 0.0))
     +      go to 300

         nx2 = nx2 + 1
         mjd2(nx2) = obstime
         if (nx2 .gt. 1) then
            if (mjd2(nx2) .eq. mjd2(nx2-1)) nx2 = nx2 - 1
         endif
         r = 1.0
         eradd = 0.0
         rawcnts = cts2
         call background(cts2,bkg2,bvar2,eradd,backopt,ipos,spinper)
         cnts2(nx2) = cts2 / r
         bgnd2(nx2) = (rawcnts + eradd) / (r**2)
         if ((cnts2(nx2) .lt. minflux) .or. 
     +       (cnts2(nx2) .gt. maxflux) .or. 
     +       (bgnd2(nx2) .gt. maxerr))
     +    nx2 = nx2 - 1

 300  continue

 999  return

      end


C----------------------------------------------------------------------
C This subroutine sets up the individual channel maps with keywords included
C to be passed to the FVELGALLC routines.  This is not an OGIP style 
C FITS files!
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C   Vers. 0.0  30 Jan 1995
C         0.9  22 Mar 1995  Resolved time bins only, weighting recorded
C         1.1  31 Oct 1995  Records the long & lat of the first box
C         1.2  15 Dec 1995  Remove v1.1 changes, naxis = 0

      subroutine mapinit(mapunit, mapname, channel, firsttime,  
     +           timestep, nrows, lngbx1, latbx1, status)

      implicit none

C Common block declarations

      common /SOURCE/ begintime, endtime, long_cnr1, lat_cnr1, 
     +       long_cnr2, lat_cnr2, minflux, maxflux, maxerr,  
     +       stimbin, backopt, iwp, jwp, spincheck, pointcheck, 
     +       weight
      
      logical spincheck, pointcheck, weight
      integer backopt, iwp, jwp
      real long_cnr1, lat_cnr1, long_cnr2, lat_cnr2
      real minflux, maxflux, maxerr, stimbin
      double precision begintime, endtime

      common /TASK/ taskname
      
      character(40) taskname

C Local variable declarations

      logical simple, extend

      integer bitpix, pcount, gcount, naxis, naxes(3), status
      integer group, mapunit, nrows, tfield, channel
      integer year, month, day, hour, min, product, row, inull(400)

      real sec, rnull(400), lngbx1, latbx1

      double precision jd, timestep, curtime, firsttime

      character(8) extname, startdate, starttime, stopdate, stoptime
      character(16) maptform, pnttform, ttype(4), tform(4), tunit(4)
      character(20) mapname
      character(80) message


      simple = .TRUE.
      bitpix = 8
      naxis = 0
      pcount = 0
      gcount = 1
      extend = .TRUE.
      group = 1

      call ftinit(mapunit, mapname, 2880, status)
 
      call ftphpr(mapunit, simple, bitpix, naxis, naxes, pcount, 
     +     gcount, extend, status)

      call ftpkys(mapunit, 'ORIGIN', 'HEASARC/NASA/GSFC',
     +     'Origin of this FITS file', status)
      call ftpkys(mapunit, 'CREATOR', taskname,
     +     'HEASARC/GSFC FTOOL which created FITS file', status)
      call ftpdat(mapunit, status)
      call ftpkys(mapunit, 'TELESCOPE', 'Vela 5B',
     +     'Telescope (mission) name', status)
      call ftpkys(mapunit, 'INSTRUMENT', 'XC',
     +     'Instrument (detector) name', status)
      jd = firsttime + 2400000.5D0
      call jd2ut(jd, year, month, day, hour, min, sec)
      write(startdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
      write(starttime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)
      jd = endtime + 2400000.5D0
      call jd2ut(jd, year, month, day, hour, min, sec)
      write(stopdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
      write(stoptime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)
      call ftpkys(mapunit, 'DATE-OBS', startdate, 
     +      'Date of observation start (dd/mm/yy)', status)
      call ftpkys(mapunit, 'TIME-OBS', starttime,
     +      'Time of observation start (hh:mm:ss)', status)
      call ftpkys(mapunit, 'DATE-END', stopdate,
     +      'Date of observation stop (dd/mm/yy)', status)
      call ftpkys(mapunit, 'TIME-END', stoptime,
     +      'Time of observation stop (hh:mm:ss)', status)
      call ftpdef(mapunit, bitpix, naxis, naxes, pcount, gcount, 
     +     status)

      if (status .ne. 0) then
         call ftgmsg(message)
         call fcecho(message)
         write(message, '('' FITSIO status = '', i3)') status
         call fcerr(message)
         go to 999
      endif

C Create the binary table extension which contains all the data

      call ftcrhd(mapunit, status)
      tfield = 4
      extname = 'MAPS'
      naxis = 2
      naxes(1) = iwp
      naxes(2) = jwp
      product = iwp*jwp
      if (product .lt. 10) then
         write(maptform,'(I1,''E'')') product
         write(pnttform,'(I1,''J'')') product
      else if (product .lt. 100) then
         write(maptform,'(I2,''E'')') product
         write(pnttform,'(I2,''J'')') product
      else
         write(maptform,'(I3,''E'')') product
         write(pnttform,'(I3,''J'')') product
      endif

      tform(1) = '1D'
      ttype(1) = 'TIME'
      tunit(1) = 'd'

      tform(2) = maptform
      ttype(2) = 'RATE'
      tunit(2) = 'count/s'

      tform(3) = maptform
      ttype(3) = 'ERROR'
      tunit(3) = 'count/s'

      tform(4) = pnttform
      ttype(4) = 'POINTS'
      tunit(4) = 'points/bin'

      call ftphbn(mapunit, nrows, tfield, ttype, tform, tunit, extname,
     +     pcount, status)
      call ftbdef(mapunit, tfield, tform, pcount, nrows, status)
      call ftptdm(mapunit, 2, naxis, naxes, status)
      call ftptdm(mapunit, 3, naxis, naxes, status)
      call ftptdm(mapunit, 4, naxis, naxes, status)
      call ftmkys(mapunit, 'ttype1', ttype(1), 
     +     'Modified Julian Date of observation map', status)
      call ftmkys(mapunit, 'ttype2', ttype(2), 
     +     'Arrayed data from the detector channel', status)
      call ftmkys(mapunit, 'ttype3', ttype(3), 
     +     'Arrayed error in counting rate', status)
      call ftmkys(mapunit, 'ttype4', ttype(4), 
     +     'Arrayed number of points in the bin', status)
      call fttnul(mapunit, 4, 0, status)

      call ftpkys(mapunit, 'ORIGIN', 'HEASARC/NASA/GSFC',
     +     'Origin of this FITS file', status)
      call ftpkys(mapunit, 'CREATOR', taskname,
     +     'HEASARC/GSFC FTOOL which created FITS file', status)
      call ftpdat(mapunit, status)
      call ftpkys(mapunit, 'TELESCOPE', 'Vela 5B',
     +     'Telescope (mission) name', status)
      call ftpkys(mapunit, 'INSTRUMENT', 'XC',
     +     'Instrument (detector) name', status)
      call ftpkys(mapunit, 'DATE-OBS', startdate, 
     +     'Date of observation start (dd/mm/yy)', status)
      call ftpkys(mapunit, 'TIME-OBS', starttime,
     +     'Time of observation start (hh:mm:ss)', status)
      call ftpkys(mapunit, 'DATE-END', stopdate,
     +     'Date of observation stop (dd/mm/yy)', status)
      call ftpkys(mapunit, 'TIME-END', stoptime,
     +     'Time of observation stop (hh:mm:ss)', status)
      call ftpkyg(mapunit, 'TSTART', begintime, 6, 
     +     'Start of observation (Modified Julian Date)', status)
      call ftpkyg(mapunit, 'TSTOP', endtime, 6, 
     +     'End of observation (Modified Julian Date)', status)
      call ftpkyg(mapunit, 'TIMEDEL', timestep, 6, 
     +     'Size of data bins (days)', status)
      call ftpkys(mapunit,'TIMEREF','SATELLITE',
     +     'No barycentric corrections applied to TIME',status)
      call ftpkys(mapunit, 'TIMEUNIT', 'd', 
     +     'Unit for TSTART, TSTOP, TIMEDEL', status)
      call ftpkys(mapunit, 'TIMESYS', 'MJD',
     +     'Time arguments in Modified Julian Dates', status)
      call ftpkyd(mapunit, 'COUFLU', 4.5D-10, 1,
     +     '3-12 keV, Crab Spectrum ergs/sq-cm/sec', status)
      if (backopt .eq. 0) then
         call ftpkyl(mapunit, 'BACKAPP', .FALSE.,
     +        'No background subtracted', status)
      else
         call ftpkyl(mapunit, 'BACKAPP', .TRUE.,
     +        'Background is subtracted', status)
      endif
       call ftpkyl(mapunit, 'VIGNAPP', .FALSE.,
     +      'No vignetting or collimator corrections applied', status)
       call ftpkyl(mapunit, 'DEADAPP', .TRUE.,
     +      'Deadtime corrections applied', status)
       call ftpkys(mapunit, 'HDUCLASS', 'OGIP',
     +             'Format conforms to OGIP standards', status)
       call ftpkys(mapunit, 'HDUCLAS1', 'LIGHTCURVE',
     +      'Light curve dataset (OGIP memo 93/003)', status)
       call ftpkys(mapunit, 'HDUCLAS2', 'RATE',
     +      'Light curve data stored as counts/s', status)
       if (backopt .ne. 0) then
          call ftpkys(mapunit, 'HDUCLAS3', 'NET',
     +      'Background subtracted light curve', status)
       endif
      if (channel .eq. 1) then 
         call ftpkyj(mapunit, 'E_MIN', 3, 'Lower energy range (keV)',
     +        status)
      else
         call ftpkyj(mapunit, 'E_MIN', 6, 'Lower energy range (keV)',
     +        status)
      endif
      call ftpkyj(mapunit, 'E_MAX', 12, 'Upper energy range (keV)', 
     +     status)
      call ftpkys(mapunit, 'EUNIT', 'keV', 
     +     'Units for E_MIN and E_MAX', status)

C Put in keyword values to pass information on to the FVELGALLC FTOOL

      call ftpkyf(mapunit, 'LNG_CNR1', long_cnr1, 3,
     +     'Galactic longitude of the first corner', status)
      call ftpkyf(mapunit, 'LAT_CNR1', lat_cnr1, 3,
     +     'Galactic latitude of the first corner', status)
      call ftpkyf(mapunit, 'LNG_CNR2', long_cnr2, 3,
     +     'Galactic longitude of the second corner', status)
      call ftpkyf(mapunit, 'LAT_CNR2', lat_cnr2, 3,
     +     'Galactic latitude of the second corner', status)
      call ftpkyf(mapunit, 'LNGBOX1', lngbx1, 3,
     +     'Galactic longitude of the first box', status)
      call ftpkyf(mapunit, 'LATBOX1', latbx1, 3,
     +     'Galactic latitude of the first box', status)
      call ftpkyj(mapunit, 'NUM_LONG', iwp,
     +     'Number of boxes in longitude', status)
      call ftpkyj(mapunit, 'NUM_LAT', jwp,
     +     'Number of boxes in latitude', status)
      call ftpkyj(mapunit, 'BACKOPT', backopt,
     +     'Background model (0 none, 1 straight, 2 sine)', status)
      call ftpkyf(mapunit, 'MINFLUX', minflux, 3, 
     +     'Minimum flux level for inclusion (counts/s)', status)
      call ftpkyf(mapunit, 'MAXFLUX', maxflux, 3, 
     +     'Maximum flux level for inclusion (counts/s)', status)
      call ftpkyf(mapunit, 'MAXVAR', maxerr, 3,
     +     'Maximum variance allowed for inclusion (counts/s)', status)
      call ftpkyl(mapunit, 'REJECTSP', spincheck,
     +     'Rejected data flagged for spin instability', status)
      call ftpkyl(mapunit, 'REJECTPT', pointcheck,
     +     'Rejected data flagged for pointing error(s)', status)
      call ftpkyl(mapunit, 'WEIGHTED', weight,
     +     'Background weighted summation used in binning', status)
      call ftpkyf(mapunit, 'BINSIZE', stimbin, 1,
     +     'Size of the resolved bins (s)', status)

      do 100 row = 1, product
         rnull(row) = -100.0
         inull(row) = 0
 100  continue

      do 200 row = 1, nrows
         curtime = firsttime + ((row-1) * timestep)
         call ftpcnd(mapunit, 1, row, 1, 1, curtime, 0.0D0, status)
         call ftpcne(mapunit, 2, row, 1, product, rnull, -100.0,
     +        status)
         call ftpcne(mapunit, 3, row, 1, product, rnull, -100.0,
     +        status)
         call ftpcnj(mapunit, 4, row, 1, product, inull, 0, status)
 200  continue

 999  return

      end


C----------------------------------------------------------------------
C This subroutine takes the 1 second data read in by previous routines and
C bins it according to the directions given by the user.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Vers. 1.0              Original VAX program by Laura Whitlock (USRA)
C        2.0  5 Jan 1995  FTOOL version of BINIT
C        2.1  9 Jan 1995  Linked with FITS routines to make WRITEDATA
C        2.2 27 Jan 1995  Made generic for shared use with Vela 5B FTOOLS
C        3.0  9 Feb 1995  Abandoned generic approach, specific to these maps
C        3.1 13 Nov 1995  Corrected bin indexing error

      subroutine mapdata(mapunit, firsttime, timestep, weight, icnt, 
     +                   mjdtime, cnts, backgnd, elements, status) 

      implicit none

      logical weight

      integer mapunit, icnt, elements, status
      integer start, i, pnts, index

      real cnts(elements), backgnd(elements)
      real bincnts, binbkg
      real sumcnts, sumbkg, sumbinv, sumcinv

      double precision mjdtime(elements), firsttime, timestep
      double precision avgtime, reltime, starttime, stoptime
      double precision newstop

C      character(80) message


 10   sumcnts = cnts(1)
      sumbkg = backgnd(1)
      sumcinv = cnts(1) / backgnd(1)
      sumbinv = 1.0 / backgnd(1)
      start = 2
      pnts = 1
      newstop = firsttime + timestep
 15   if (mjdtime(1) .ge. newstop) then
         newstop = newstop + timestep
         go to 15
      endif

 20   do 30 i = start, elements
         if (mjdtime(i) .ge. newstop) go to 40
         sumcnts = cnts(i) + sumcnts
         sumbkg = backgnd(i) + sumbkg
         sumcinv = (cnts(i) / backgnd(i)) + sumcinv
         sumbinv = (1.0 / backgnd(i)) + sumbinv
         pnts = pnts + 1
 30   continue

 40   starttime = mjdtime(start-1)
      stoptime = mjdtime(i-1) + (1.0D0 / 86400.0D0)
      avgtime = (starttime + stoptime) / 2.0D0

C Increment the stop time bumper until it exceeds the current time

 50   if (mjdtime(i) .ge. newstop) then
         newstop = newstop + timestep
         if (newstop .le. mjdtime(elements)) go to 50
      endif

C Bin counts and background according to user request for weighted or 
C   straight binning

      if (weight) then
         bincnts = sumcinv / sumbinv
         binbkg = 1.0 / SQRT(sumbinv)
      else 
         bincnts = sumcnts / REAL(pnts)
         binbkg = SQRT(sumbkg / (REAL(pnts)**2))
      endif

C Print out intermediate results 

      reltime = avgtime - firsttime
      index = INT(reltime/timestep) + 1

      call ftpcne(mapunit, 2, index, icnt, 1, bincnts, -100.0, status)
      call ftpcne(mapunit, 3, index, icnt, 1, binbkg, -100.0, status)
      call ftpcnj(mapunit, 4, index, icnt, 1, pnts, 0, status)

      if (i .lt. elements) then
         sumcnts = cnts(i)
         sumbkg = backgnd(i) 
         sumcinv = cnts(i) / backgnd(i)
         sumbinv = 1.0 / backgnd(i)
         start = i + 1
         pnts = 1
         go to 20
      endif
       
 999  return

      end

C----------------------------------------------------------------------
C After all the data has been written into the data arrays, move all the 
C rows which contain at least one data point to the front of the file
C and delete all the rows after the last row once the move is complete.
C This DOES NOT shrink the FITS file (at least in the current realization
C of FITSIO), but does speed up reading and writing the data in later
C tasks.
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C   Vers. 0.0  30 Jan 1995
C         0.1  27 Feb 1995  Move all good rows to the top of the file, 
C                            then delete all the rows after them.
C         0.9  29 Jan 1996  Install bug chasers to find out why last line
C                            is not being deleted.
C         1.0  30 Jan 1996  All empty rows deleted

      subroutine mapclose(mapunit, iwp, jwp, begintime, endtime,
     +           status)

      implicit none

      logical somedata, anyf, flagvals(400)

      integer mapunit, status, numofrows, jwp, iwp, pnts(400)
      integer year, month, day, hour, min, product
      integer row, i, ii, hdutype

      real counts(400), error(400), sec

      double precision mjdtime, begintime, endtime, jd

      character(8) startdate, starttime, stopdate, stoptime
      character(70) comment
      character(80) message

      row = 0
      product = iwp * jwp
      call ftgkyj(mapunit, 'NAXIS2', numofrows, comment, status)

C Find out if the entire count array is empty (all flag values are FALSE)
C Assume the entire array is filled with NULLs unless a real value 
C (flag value is TRUE) is found

      do 300 i = 1, numofrows
         somedata = .FALSE.
         call ftgcfe(mapunit, 2, i, 1, product, counts, flagvals,
     +        anyf, status)
         do 100 ii = 1, product
            if (.NOT. flagvals(ii)) then
               somedata = .TRUE.
               go to 200
            endif
 100     continue
 200     if (somedata) then
            row = row + 1

C If the current row (i) has data and a previous row or rows did not,
C move the current row to just after the last row to have data

            if (i .gt. row) then
               call ftgcvd(mapunit, 1, i, 1, 1, 0.0D0, mjdtime,
     +              anyf, status)
               call ftgcve(mapunit, 3, i, 1, product, -100.0, error,
     +              anyf, status)
               call ftgcvj(mapunit, 4, i, 1, product, 0, pnts,
     +              anyf, status)
               call ftpcnd(mapunit, 1, row, 1, 1, mjdtime, 0.0D0, 
     +              status)
               call ftpcne(mapunit, 2, row, 1, product, counts, 
     +              -100.0, status)
               call ftpcne(mapunit, 3, row, 1, product, error, -100.0,
     +              status)
               call ftpcnj(mapunit, 4, row, 1, product, pnts, 0, 
     +              status)
            elseif (i .lt. row) then
               write(message,'(''Cleaning out unused rows error: '', 
     +            i4, '' rows in file (so far), '', i4, 
     +            '' rows of data'')') i, row
               call fcecho(message)
            endif
         endif
 300  continue
      call ftdrow(mapunit, row + 1, numofrows - row, status)
      
C Modify the stop and/or start times if necessary

      call ftgkys(mapunit, 'DATE-OBS', startdate, comment, status)
      call ftgkys(mapunit, 'TIME-OBS', starttime, comment, status)
      call ftgkys(mapunit, 'DATE-END', stopdate, comment, status)
      call ftgkys(mapunit, 'TIME-END', stoptime, comment, status)

      call ftgcvd(mapunit, 1, 1, 1, 1, 0.0D0, mjdtime, anyf, status)
      jd = mjdtime + 2400000.5D0
      call jd2ut(jd, year, month, day, hour, min, sec)
      write(startdate,'(I2.2,''/'',I2.2,''/'',I2.2)') 
     +      day, month, (year-1900)
      write(starttime,'(I2.2,'':'',I2.2,'':'',I2.2)') 
     +      hour, min, INT(sec)

      call ftgcvd(mapunit, 1, row, 1, 1, 0.0D0, mjdtime, anyf, status)
      jd = mjdtime + 2400000.5D0
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
      call ftmkyg(mapunit, 'TSTART', begintime, 6, 
     +     'Start of observation (Modified Julian Date)', status)
      call ftmkyg(mapunit, 'TSTOP', endtime, 6, 
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









