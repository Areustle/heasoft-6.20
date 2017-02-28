C*****************************************************************************
C TASK NAME: bod2pha
C
C FILE NAME: bod2pha.f
C
C DESCRIPTION: FTOOL to convert BATSE occultation data FITS file (NHIS
C     type) into PHA FITS files in XSPEC format.  A time interval in
C     Truncated Julian Date must be specified, advisably within a single
C     CGRO viewing period.  Start and stop Truncated Julian Dates for
C     the CGRO viewing periods contained within a given BATSE occultation
C     data FITS file can be obtained with the help of the FTOOL bodgetvp.
C     Note that each BATSE occultation data FITS file corresponds to a
C     single astronomical source on the sky.
C
C AUTHOR/DATE: Peter J.T. Leonard, COSSC/GSFC/NASA, Hughes STX, oct-1997
C
C NOTES:
C
C USAGE:
C     Host: bod2pha
C     IRAF: bod2pha
C
C ROUTINES IMPLEMENTED IN THIS FILE:
C     subroutine bod2pa   - top level subroutine, called by IRAF or host
C                           C wrapper in hbod2pha.c
C     subroutine read_bod - reads in BATSE occultation data FITS file
C     subroutine wftspf2  - writes out PHA FITS file in XSPEC format
C
C MODIFICATION HISTORY:
C     James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 15-jan-1998
C          Changed wftspf to wftspf2 to avoid collision with subroutines 
C          of the same name in other tools, when linking the gro
C          package together.
C     Ning Gan, HEASARC/GSFC/NASA, Raytheon STX, 1-jul-1998
C          Updates for the new format of DATE keywords.
C     Peter J.T. Leonard, COSSC/GSFC/NASA, Raytheon STX, 31-aug-1998
C          Added option to allow user to input a string (phastr) to
C          help name output PHA files.
C*****************************************************************************

C*****************************************************************************
C SUBROUTINE: bod2pa
C
C DESCRIPTION: Program to convert BATSE occultation data FITS file (NHIS
C     type) into PHA FITS files in XSPEC format.  A time interval in
C     Truncated Julian Date must be specified, advisably within a single
C     CGRO viewing period.  Start and stop Truncated Julian Dates for
C     the CGRO viewing periods contained within a given BATSE occultation
C     data FITS file can be obtained with the help of the FTOOL bodgetvp.
C     Note that each BATSE occultation data FITS file corresponds to a
C     single astronomical source on the sky.
C
C AUTHOR/DATE: Peter J.T. Leonard, COSSC/GSFC/NASA, Hughes STX, oct-1997
C
C NOTES: Based on Chris Shrader's IDL code NHIS2PHA.PRO.
C
C ARGUMENTS:
C     bodfil - name of BATSE occultation data FITS file (NHIS type)
C     tjdsta - desired start time in Truncated Julian Date
C     tjdstp - desired stop time in Truncated Julian Date
C     minfds - minimum number of flux determinations by detector for
C              inclusion in summation
C     srcnam - source name
C     phastr - string to help name output PHA file
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C     subroutine read_bod - reads in BATSE occultation data FITS file
C     subroutine namepha  - creates name phafil from phastr and chidet
C     subroutine wftspf2  - writes out PHA FITS file in XSPEC format
C
C MODIFICATION HISTORY:
C     Sandhia Bansal, GRO/GSFC/NASSA, SSAI, 07-Jan-2002
C          Added a new input parameter, viewper.
C          User can specify this instead of start/stop (TJD) times.
C          program will use a lookup table to get the times.
C     James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 12-oct-1999
C          Changes to warning message, submitted by Peter Leonard.
C     James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 15-jan-1998
C          Changed wftspf to wftspf2 to avoid collision with subroutines 
C          of the same name in other tools, when linking the gro
C          package together.
C     Peter J.T. Leonard, COSSC/GSFC/NASA, Raytheon STX, 31-aug-1998
C          Added option to allow user to input a string (phastr) to
C          help name output PHA files.
C     Colleen A. Wilson-Hodge NASA/MSFC, 19-Mar-2004
C          1. Increased array sizes to 300000 elements to read in mission long
C               and single step fits files.
C          2. Changed tjd1 and tjd2 to real*8 in both bod2pha and read_bod.
C          3. Changed read_bod to also read in the flags and added code to
C               sum only data that does not have flag bit 16 set (manual flag
C               for bad data).
C          4. Fixed bugs in code that summed data. See line by line for specific
C               fixes.
C    Colleen A. Wilson-Hodge NASA/MSFC, cawh 30-apr-2004
C    Added computation of exposure time based upon number of occultation steps
C    in the measurement. Exposure time is now written to pha file header.
C*****************************************************************************

      subroutine bod2pa ()
 
      implicit none
 
      real*4 tjdsta, tjdstp, equinx, ranom, decnom, roll, texpos
      real*4 ascale, bscale, cscale, sum, tot_cts, tot_sig
      real*4 cts(16), sig(16), sysfrc(16)
      real*8 tjd1(300000), tjd2(300000), w(300000)
      real*4 rates(16,300000), err(16,300000), fdum(4)

      integer*2 qualty(16), chan(16), group(16), ipha(16)
      integer*4 status, minfds, nrows, nhist, nchann, j, viewper
      integer*4 j1, j2, nsum, idet, i, dtype, k, npts, ierr
      integer*4 ndet(8), clobber
      integer*4 detid(300000), flags(300000)
      integer*2 nsteps(300000) !cawh 30-apr-2004
      character(80) data_dir, bodfil, srcnam, orgfil, tlscpe, filter
      character(80) shsstr, edsstr, ehsstr, phafil, chidet, instrm
      character(80) assfls(4), sdsstr, output_dir
      character(80) hist(20)
      character(120) phastr
 
      logical qqual, qgroup, qerror, qsys
 
C Setup common block for taskname - necessary for fcerr.
      character(80) msg
      character(40) taskname
      common /tasknm/taskname
      taskname = 'bod2pha'
 
C Give warning to use only data from single CGRO viewing period.
      msg =
     *    '************************************************************'
      call fcerr (msg)
      msg =
     *    '*    Please Note:  It is up to the user to verify that     *'
      call fcerr (msg)
      msg =
     *    '*    the TJD start and stop times are contained within     *'
      call fcerr (msg)
      msg =
     *    '*    a single CGRO viewing period.  Combining data from    *'
      call fcerr (msg)
      msg =
     *    '*   multiple viewing periods is inadvisable.  Please use   *'
      call fcerr (msg)
      msg =
     *    '*    FTOOL bodgetvp before running bod2pha to determine    *'
      call fcerr (msg)
      msg =
     *    '*              the viewing period boundaries.              *'
      call fcerr (msg)
      msg =
     *    '************************************************************'
      call fcerr (msg)

C     Read in name of directory containing BATSE occultation data FITS file.
      status = 0
      call uclgst ('datadir', data_dir, status)
      if (status.ne.0) then
         msg = 
     *        'Problem getting name of the ' //
     *        'directory containing input data !'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

C     Read in name of BATSE occultation data FITS file.
      status = 0
      call uclgst ('bodfil', bodfil, status)
      if (status.ne.0) then
         msg = 'Problem getting name of BATSE occultation data file!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
      
Csb Create full name (including path name) of the BATSE occultation data FITS file
      i = index(data_dir, ' ') - 1
      j = index(bodfil, ' ') - 1
      bodfil = data_dir(1:i) // bodfil(1:j) // '.fits'

C     Read in viewing period.
      viewper = 0
      call uclgsi ('viewper', viewper, status)
      if (status.ne.0) then
         msg = 'Problem getting viewing period!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
      if (viewper .eq. 0) then
C     Read in desired start time in Truncated Julian Date.
         call uclgsr ('tjdsta', tjdsta, status)
         if (status.ne.0) then
            msg = 'Problem getting desired start time!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif

C     Read in desired stop time in Truncated Julian Date.
         call uclgsr ('tjdstp', tjdstp, status)
         if (status.ne.0) then
            msg = 'Problem getting desired stop time!'
            call fcerr (msg)
            msg = 'Aborting program!'
            call fcerr (msg)
            call exit (1)
         endif
      else
c        msg = 'This option may produce incorrect results!'
c        call fcerr (msg)
c        msg = 'TJD_STOP (vp_list.fits) after viewing period boundary.'
c        call fcerr (msg)
c        msg = 'Data with different detector responses may be combined!'
c        call fcerr (msg)
c        msg = 'User must assure source is in different detectors in '
c        call fcerr (msg)
c        msg = 'adjacent pointings before using viewper option!'
c        call fcerr (msg)
      endif
C     Read in minimum number of flux determinations by detector for inclusion
C     in summation.
      call uclgsi ('minfds', minfds, status)
      if (status.ne.0) then
         msg = 'Problem getting minimum number of flux determinations!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     Read in source name.
      call uclgst ('srcnam', srcnam, status)
      if (status.ne.0) then
         msg = 'Problem getting name of source!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
      
C     Read in name of the output directory.
      call uclgst ('outputdir', output_dir, status)
      if (status.ne.0) then
         msg = 'Problem getting name of the output directory!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

C     Read in string to help name output PHA file.
      call uclgst ('phastr', phastr, status)
      if (status.ne.0) then
         msg = 'Problem getting string to help name output PHA file!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif

Csb Create full-partial name (including path name) of the output file
      i = index(output_dir, ' ') - 1
      phastr = output_dir(1:i) // phastr

C     Read in clobber flag.
      call uclgsi ('clobber', clobber, status)
      if (status.ne.0) then
         msg = 'Problem getting clobber flag!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     If a viewing period is specified, read the viewper table and get
C     corresponding start and stop (TJD) times.
      if (viewper .gt. 0) then
         call readtable(data_dir, viewper, tjdsta, tjdstp, fdum(1), 
     *        fdum(2), fdum(3), fdum(4), 0, status)
      endif
      if (status.ne.0) then
         msg = 
     *        'Cannot find TJD start and stop times for given view per!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     Read in data from BATSE occultation data FITS file.
      write(*,*) 'bodfil: ', bodfil
      call read_bod (bodfil, orgfil, nrows, tjd1, tjd2, rates, err, 
     *     detid, flags, nsteps, status) !cawh 30-apr-2004
 
C     Check for error during read in of data.
      if (status.ne.0) then
         msg = 'Problem with reading FITS data.'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     Check whether desired start time is inconsistent with data set.
      write(*,*) 'tjdsta: ', tjdsta, ' tjd1: ', tjd1(1)
      if (tjdsta.lt.tjd1(1)-0.1) then
         msg = 'Data begins after desired start time!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     Check whether desired stop time is inconsistent with data set.
      if (tjdstp.gt.tjd2(nrows)+0.1) then
         msg = 'Data ends before desired stop time!'
         call fcerr (msg)
         msg = 'Aborting program!'
         call fcerr (msg)
         call exit (1)
      endif
 
C     Include name of original data file in history.
      nhist = 4
      write (hist(1), 10) orgfil
 10   format ('Name of original data file is ', a50)
 
C     Note that name of source may be first part of original data filename.
      hist(2) = 
     *     'Name of source may be first part of original data filename'

C     Note start and stop times in history.
      write (hist(3), 20) tjdsta, tjdstp
 20   format ('Range in Truncated Julian Date is', f9.2, ' to', f9.2)
 
C     Note origin of PHA file in history.
      hist(4) = 
     *     'XSPEC PHA FITS file produced from BATSE occultation data'
 
C     Use all 16 channels.
      nchann = 16
 
C     Define start day.
      j1 = 0
      do j = 0, nrows-1
         if (tjd1(j+1).le.tjdsta) j1 = j1 + 1
      enddo
 
C     Define stop day.
      j2 = j1 - 1
      do j = j1-1, nrows-1
         if (tjd2(j+1).le.tjdstp) j2 = j2 + 1
      enddo
 
C     Size of summation interval.
C     Bug fix in next line - nsum was too small by 1. I removed the -1. 
C      cawh 19-mar-2004
      nsum = j2 - j1 
      
C     Initialize "number of flux determinations" vector.
      do idet = 0, 7
         ndet(idet+1) = 0
      enddo
 
C     Read grouping card, and set quality and grouping flags.
      do i = 1, nchann
         qualty(i) = 0
      enddo
      qqual = .FALSE.
      qgroup = .FALSE.

C     Set channel number vector.  These are assumed to be monotonic
C     starting at unity.
      do i = 1, nchann
         chan(i) = i
      enddo

C     Telescope is BATSE.
      tlscpe = 'BATSE'
      
C     Uncertainties are included in data, systematic errors are not.
      qerror = .true.
      qsys = .false.
 
C     No filter.
      filter = 'none'
 
C     Right ascension, declination and roll angle irrelevant.
      equinx = 2000.0
      ranom = 0.0
      decnom = 0.0
      roll = 0.0
      
C     Data and uncertainties are in counts/sec.
      dtype = 2
      
C     Set exposure time to unity. commented out cawh 30-apr-2004
c      texpos = 1.0 
	
C     BATSE does not have associated files (back, RMF, corr, ARF).
      do i = 1, 4
         assfls(i) = 'none'
      enddo

C     BATSE data are too complicated to be described by single start and stop.
      sdsstr = ' '
      shsstr = ' '
      edsstr = ' '
      ehsstr = ' '

C     Area, background, and correction scale factors.
      ascale = 1.0
      bscale = 1.0
      cscale = 0.0
 
C     Some artwork.
      msg = 
     * '************************************************************'
      call fcerr (msg)
      write (msg, 30) srcnam
 30   format
     * ('*   Count totals for each detector from source ', a8,':   *')
      call fcerr (msg)
      
C     Loop over individual detectors.
      do idet = 0, 7
         
C     Count number of flux determinations by each detector.
         do k = 0, nsum-1
            if (detid(k+j1+1).eq.idet) then
C     changed cawh 30-apr-2004  - now counts number of occultation steps            
               ndet(idet+1) = ndet(idet+1) + nsteps(k+j1+1) 
            endif
         enddo
C     added by cawh 30-apr-2004: Calculate exposure time
c     exposure time = number of steps * 110 s (occultation half range)
c     Even though 240 s of data are extracted for each step - 110 s
c     either side + a 20-s wide step, the exposure is approximately only
c     110s because during the rest of the interval, the source was occulted
c     by the Earth.
      texpos = ndet(idet+1)*110.   !cawh 30-apr-2004
C     Consider all detectors with more than minimum number of flux determinations.
         if (ndet(idet+1).ge.minfds) then            
C     Name output file.
            if (phastr.eq.'      ') then
               if (tjdsta.lt.10000.0.and.tjdstp.lt.10000.0) then
                  write (phafil, 40) tjdsta, tjdstp, idet
 40               format ('bod_', f6.1, '-', f6.1, '_', i1, '.pha')
               endif
               if (tjdsta.lt.10000.0.and.tjdstp.ge.10000.0) then
                  write (phafil, 50) tjdsta, tjdstp, idet
 50               format ('bod_', f6.1, '-', f7.1, '_',i1, '.pha')
               endif
               if (tjdsta.ge.10000.0.and.tjdstp.ge.10000.0) then
                  write (phafil, 60) tjdsta, tjdstp, idet
 60               format ('bod_', f7.1, '-', f7.1, '_', i1, '.pha')
               endif
            else
               write (chidet, 65) idet
 65            format (i1)
C     Subroutine namepha (written in C) creates name phafil from phastr and chidet.
               call namepha (phastr, chidet, phafil)
	       call FileExists(phafil, clobber, status)
	       if (status .ne. 0) return
            endif
            
C     Name detector.
            write (instrm, 70) idet
 70         format ('LAD', i1)
 
C     Sum over nsum data intervals for nchann channels by accumulating
C     total counts per channel, exposure, and sigma squared for each
C     detector individually.
            do i = 0, nchann-1
               cts(i+1) = 0.0
               sig(i+1) = 0.0
               npts = 0
               
C     Calculate and sum array of statistical weights.
               sum = 0.0
               do k = 0, nsum-1
                  w(k+1) = 0.0                  
C     Select only data for the chosen detector where bad data flags are not set                  
                  if ((detid(k+j1+1).eq.idet).and.
     *                (ibits(flags(k+j1+1),16,1).eq.0)) then
C     Bug Fix in next line: **2 was missing. cawh 19-mar-2004     
                     w(k+1) = 1.0 / err(i+1,j1+k+1)**2 
                     sum = sum + w(k+1)
                  endif
               enddo
               
C     Normalize array of statistical weights.
               do k = 0, nsum-1
                  w(k+1) = w(k+1) / sum
               enddo
 
C     Sum counts, sum sigma in quadrature, and count number of non-zero points.
               do k = 0, nsum-1
C     Select only data for the chosen detector where bad data flags are not set                  
                  if ((detid(k+j1+1).eq.idet).and.
     *                (ibits(flags(k+j1+1),16,1).eq.0)) 
     *             then
                     cts(i+1) = cts(i+1) + w(k+1)
     *                    * rates(i+1,j1+k+1)
C     Bug fix in next line: w(k+1)**2 was missing. cawh 19-mar-2004     
                     sig(i+1) = sig(i+1) + err(i+1,j1+k+1)**2*w(k+1)**2
                     npts = npts + 1
                  endif
               enddo
 
C     Take square root of sum of sigma squared.
C     Bug fix in next line - original code was dividing sig by npts. This was
C     removed because it was wrong.
               sig(i+1) = sqrt(sig(i+1)) 
            enddo

C     Write out FITS file.
            call wftspf2 (phafil, bodfil, hist, nhist, assfls, equinx, 
     *           sdsstr, shsstr, edsstr, ehsstr, srcnam, tlscpe, instrm, 
     *           filter, ranom, decnom, roll, texpos, ascale, bscale, 
     *           cscale, qgroup, group, qqual, qualty, nchann, dtype, 
     *           chan, ipha, cts, qerror, sig, qsys, sysfrc, ierr, 
     *           status)
 
C     Check for error during write out of data.
            if (status.ne.0) then
               msg = 'Problem with writing output files!'
               call fcerr (msg)
               msg = 'Do output files with same names already exist?'
               call fcerr (msg)
               msg = 'Aborting program!'
               call fcerr (msg)
               call exit (1)
            endif
 
C     Calculate and write out totals.
            tot_cts = 0.0
            tot_sig = 0.0
            do i = 0, nchann-1
               tot_cts = tot_cts + cts(i+1)
               tot_sig = tot_sig + sig(i+1)**2
            enddo
            tot_sig = sqrt(tot_sig)
 
C     Write totals to screen.
            write (msg, 80) tot_cts, tot_sig, instrm
 80         format ('*    total counts/sec =', e10.3,
     *           ' +-', e10.3, ' for ', a4, '    *')
            call fcerr (msg)
            
         endif
      enddo
      
C     More artwork.
      msg = 
     *  '************************************************************'
      call fcerr (msg)
 
      end
 
C*****************************************************************************
C SUBROUTINE: read_bod
C
C DESCRIPTION: Reads in BATSE occultation data FITS file (NHIS type).
C
C AUTHOR/DATE: Peter J.T. Leonard, COSSC/GSFC/NASA, Hughes STX, oct-1997
C
C NOTES:
C
C ARGUMENTS:
C     bodfil - input BATSE occultation data FITS filename (NHIS type)
C     orgfil - original data filename
C     nrows  - number of rows in FITS file
C     tjd1   - vector of start times in Truncated Julian Date
C     tjd2   - vector of stop times in Truncated Julian Date
C     rates  - array of count rates
C     err    - array of count rate uncertainties
C     detid  - vector of detector identities
C     flags  - array containing flags. See FITS file header for details.
C     status - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C MODIFICATION HISTORY:
C  Colleen Wilson-Hodge NASA/MSFC - 19 mar 2004. Code added to read in the flags
C    row. Array sizes increased to 300000 elements to allow reading of mission 
C    long and single step files.
C  Colleen Wilson-Hodge NASA/MSFC - cawh 30-apr-2004. Code added to read in nsteps
c    row.
C*****************************************************************************
 
      subroutine read_bod (bodfil, orgfil, nrows, tjd1, tjd2, rates, 
     *     err, detid, flags, nsteps, status) !cawh 30-apr-2004
 
      implicit none
      
      real*4 nulle, st, et
      real*4 rt(16), rte(16)
      real*8 tjd1(300000), tjd2(300000)
      real*4 rates(16,300000), err(16,300000)

      integer*4 status, unit, dummy, hdutype, nrows, felem, nullj
      integer*4 irow, colnum, nelems, sd, ed, j,flg
      integer*4 detid(300000),flags(300000)
      integer*2 nsteps(300000), nstps, nulli 
 
      character(1) nullb, didb
      character(80) bodfil, orgfil, comment
 
      logical anynull
 
C     Find unused logical unit.
      call ftgiou (unit, status)
 
C     Open BATSE occultation data FITS file.
      dummy = 0
      call ftopen (unit, bodfil, 0, dummy, status)
      
C     Read name of original data file.
      call ftgkys (unit, 'FILENAME', orgfil, comment, status)
      
C     Move to next FITS extension.
      call ftmrhd (unit, 1, hdutype, status)
 
C     Move to next FITS extension.
      call ftmrhd (unit, 1, hdutype, status)
 
C     Move to next FITS extension.
      call ftmrhd (unit, 1, hdutype, status)
 
C     Find number of rows from FITS extension header, which is equal
C     to total number of data points.
      call ftgkyj (unit, 'NAXIS2', nrows, comment, status)
 
C     Read data from FITS table.
      felem = 1
      nullj = 0
      nulle = 0.0
      nullb = char(0)
      do irow = 1, nrows
 
C     Extract start and stop dates and times of data intervals.
         colnum = 1
         nelems = 1
         call ftgcvj (unit, colnum, irow, felem, nelems, nullj, sd, 
     *        anynull, status)
         colnum = 2
         call ftgcve (unit, colnum, irow, felem, nelems, nulle, st, 
     *        anynull, status)
         colnum = 3
         call ftgcvj (unit, colnum, irow, felem, nelems, nullj, ed, 
     *        anynull, status)
         colnum = 4
         call ftgcve (unit, colnum, irow, felem, nelems, nulle, et, 
     *        anynull, status)
 
C     Extract count rates.
         colnum = 14
         nelems = 16
         call ftgcve (unit, colnum, irow, felem, nelems, nulle, rt, 
     *        anynull, status)
         
C     Extract count rate uncertainties.
         colnum = 15
         call ftgcve (unit, colnum, irow, felem, nelems, nulle, rte, 
     *        anynull, status)
         
C     Extract detector identities.
         colnum = 17
         nelems = 1
         call ftgcvb (unit, colnum, irow, felem, nelems, nullb, didb, 
     *        anynull, status)
C     Extract nsteps cawh 30-apr-2004
         colnum = 18
         nelems = 1
         call ftgcvi (unit, colnum, irow, felem, nelems, nulli, nstps, 
     *        anynull, status)
C     Extract Flags cawh 19-mar-2004
         colnum = 21
         nelems = 1
         call ftgcvj (unit, colnum, irow, felem, nelems, nullj, flg, 
     *        anynull, status)
              
C     Combine start and stop dates and times into Truncated Julian Dates.
         tjd1(irow) = float(sd) + (st / 86400.0)
         tjd2(irow) = float(ed) + (et / 86400.0)
         
C     Append rt and rte vectors to rates and err arrays.
         do j = 1, 16
            rates(j,irow) = rt(j)
            err(j,irow) = rte(j)
         enddo
 
C     Convert detector identities to 0 through 7.
         detid(irow) = int(alog10(float(ichar(didb))) / alog10(2.0) 
     *        + 0.1)
C     Append nstps to nsteps array cawh 30-apr-2004
         nsteps(irow) = nstps
C     Append flg to flags array cawh 19-mar-2004
         flags(irow) = flg              
      enddo
C     Close BATSE occultation data FITS file, and free logical unit.
      call ftclos (unit, status)
      call ftfiou (unit, status)
      
      return
 
      end

C*****************************************************************************
C SUBROUTINE: wftspf2
C
C DESCRIPTION: Writes out FITS file in XSPEC format.
C
C AUTHOR/DATE: Keith A. Arnaud, 199?
C
C NOTES:
C
C ARGUMENTS
C     phafil    - output XSPEC PHA FITS filename
C     bodfil    - input BATSE occultation data FITS filename
C     hist(*)   - history records
C     nhist     - number of history records
C     assfls(4) - associated files (back, RMF, corr, ARF)
C     equinx    - observation epoch
C     sddstr    - start day of observation
C     shhstr    - start time of observation
C     eddstr    - stop day of observation
C     ehhstr    - stop time of observation
C     srcnam    - source observed
C     tlscpe    - telescope/satellite name
C     instrm    - instrument name
C     filter    - instrument filter
C     ranom     - observation RA (decimal degrees)
C     decnom    - observation DEC (decimal degrees)
C     roll      - observation roll angle (decimal degrees)
C     texpos    - exposure time
C     ascale    - area scaling factor
C     bscale    - background scaling factor
C     cscale    - correction scaling factor
C     qgroup    - data is grouped
C     group(*)  - channel group
C     qqual     - data has quality flags
C     qualty(*) - channel quality
C     nchann    - number of channels
C     dtype     - data type: 1=counts, 2=counts/sec
C     chan(*)   - channel numbers
C     ipha(*)   - data in counts
C     cts(*)    - data in counts/sec
C     qerror    - statistical errors included
C     sig(*)    - errors in counts/sec
C     qsys      - systematic errors included
C     sysfrc(*) - fractional systematics
C     ierr      - error
C                    1 = failed to open FITS file
C                    2 = failed to write primary header
C                    3 = failed to set up spectrum extension
C                    4 = failed to write XSPEC keywords
C                    5 = failed to write extra extension keywords
C                    6 = failed to write spectrum data
C                    7 = failed to close FITS file
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C MODIFICATION HISTORY:
C     Peter J.T. Leonard, COSSC/GSFC/NASA, Hughes STX, jan-1997
C          Some cosmetic changes.
C     Peter J.T. Leonard, COSSC/GSFC/NASA, Hughes STX, oct-1997
C          Some tailoring to BATSE occultation data.
C     James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 15-jan-1998
C          Changed wftspf to wftspf2 to avoid collision with subroutines 
C          of the same name in other tools, when linking the gro
C          package together.
C     Ning Gan, HEASARC/GSFC/NASA, Raytheon STX, 1-jul-1998
C          Updates for the new format of DATE keywords.
C*****************************************************************************
 
      SUBROUTINE wftspf2 (phafil, bodfil, hist, nhist, assfls, equinx, 
     *     sdsstr, shsstr, edsstr, ehsstr, srcnam, tlscpe, instrm, 
     *     filter, ranom, decnom, roll, texpos, ascale, bscale, 
     *     cscale, qgroup, group, qqual, qualty, nchann, dtype, chan, 
     *     ipha, cts, qerror, sig, qsys, sysfrc, ierr, status)
      
      implicit none

      REAL*4 cts(*), sig(*), sysfrc(*)
      REAL*4 equinx, ranom, decnom, roll, texpos
      REAL*4 ascale, bscale, cscale

      INTEGER*4 nhist, nchann, dtype, ierr, status
      INTEGER*4 ounit, i, tfields, colnum
      INTEGER*2 group(*), qualty(*), chan(*), ipha(*)
 
      CHARACTER*(*) bodfil
      CHARACTER*(*) phafil
      CHARACTER*(*) sdsstr, shsstr, edsstr, ehsstr
      CHARACTER*(*) srcnam, tlscpe, instrm, filter
      CHARACTER*(*) hist(*), assfls(4)
      character(20) ttype(6), tunit(6)
      character(5)  tform(6)
 
      LOGICAL qgroup, qqual, qerror, qsys

C     Open output FITS file.
      CALL getlun (ounit)
      CALL ftinit (ounit, phafil, 1, status) 
      IF (status .NE. 0) THEN
         ierr = 1
         RETURN
      ENDIF
      
C     Write primary header.
      CALL ftpdef (ounit, 8, 0, 0, 0, 1, status)

C     Write basic primary array keywords.
      CALL ftphpr (ounit, .TRUE., 8, 0, 0, 0, 1, .TRUE., status)

C     Write out additional keywords about creation of FITS file.
      CALL ftpkys (ounit, 'CONTENT', 'SPECTRUM',
     &     'spectrum file contains time intervals and event', status)
 
      CALL ftpkys (ounit, 'FILENAME', bodfil,
     &     'file that FITS was produced from', status)
 
      CALL ftpkys (ounit, 'ORIGIN', 'FTOOLS',
     &     'origin of FITS file', status)

C     Write telescope and instrument keywords.
      CALL ftpkys (ounit,'TELESCOP', tlscpe,
     &     'telescope (mission) name', status)
 
      CALL ftpkys (ounit,'INSTRUME', instrm,
     &     'instrument name', status)
      
C     Write source name.
      CALL ftpkys (ounit, 'OBJECT', srcnam,
     &     'name of observed source', status)

C     Write pointing data.
      CALL ftpkye (ounit,'RA-NOM', ranom, 4,
     &     'right ascension of target (deci. deg)', status)
 
      CALL ftpkye (ounit,'DEC-NOM', decnom, 4,
     &     'declination of target (deci. deg)', status)
      
      CALL ftpkye (ounit,'DROLLANG', roll, 4,
     &     'mean roll angle (deci. deg)', status)
      
C     Write start and stop times.
      CALL ftpkys (ounit, 'DATE-OBS', sdsstr,
     &     'date observations were made (yyyy-mm-dd)', status)
      
      CALL ftpkys (ounit, 'TIME-OBS', shsstr,
     &     'time observations were made (hh:mm:ss)', status)
      
      CALL ftpkys (ounit, 'DATE-END', edsstr,
     &     'date observations were made (yyyy-mm-dd)', status)
      
      CALL ftpkys (ounit, 'TIME-END', ehsstr,
     &     'time observations were made (hh:mm:ss)', status)

C     Write any history records to FITS file as history.
      DO i = 1, nhist
         CALL ftphis (ounit, hist(i), status)
      ENDDO

      IF (status .NE. 0) THEN
         ierr = 2
         RETURN
      ENDIF

C     Write data extension with spectrum data.

C     Create data extension.
      CALL ftcrhd (ounit, status)
      
C     Set up header keywords for binary extension.
      tfields = 2
      ttype(1) = 'CHANNEL'
      tform(1) = 'I'
      tunit(1) = ' '
 
      IF (dtype .EQ. 1) THEN
         ttype(2) = 'COUNTS'
         tform(2) = 'I'
         tunit(2) = 'counts'
      ELSEIF (dtype .EQ. 2) THEN
         ttype(2) = 'RATE'
         tform(2) = 'E'
         tunit(2) = 'counts/sec'
      ENDIF
 
      IF (qerror) THEN
         tfields = tfields + 1
         ttype(tfields) = 'STAT_ERR'
         tform(tfields) = 'E'
         tunit(tfields) = 'counts/sec'
      ENDIF
 
      IF (qsys) THEN
         tfields = tfields + 1
         ttype(tfields) = 'SYS_ERR'
         tform(tfields) = 'E'
         tunit(tfields) = ' '
      ENDIF
 
      IF (qqual) THEN
         tfields = tfields + 1
         ttype(tfields) = 'QUALITY'
         tform(tfields) = 'I'
         tunit(tfields) = ' '
      ENDIF
 
      IF (qgroup) THEN
         tfields = tfields + 1
         ttype(tfields) = 'GROUPING'
         tform(tfields) = 'I'
         tunit(tfields) = ' '
      ENDIF

C     Write main header keywords.
      CALL ftphbn (ounit, nchann, tfields, ttype, tform, tunit,
     &     'SPECTRUM', 0, status)

C     Write additional keywords describing data stored.
      IF (dtype .EQ. 1) THEN
         CALL ftpkyl (ounit, 'POISSERR', .TRUE.,
     &        'Poissonian errors to be assumed', status)
      ENDIF
 
      IF (.NOT. qerror) THEN
         CALL ftpkyj (ounit, 'STAT_ERR', 0,
     &        'no statistical error specified', status)
      ENDIF
 
      IF (.NOT. qsys) THEN
         CALL ftpkyj (ounit, 'SYS_ERR', 0,
     &        'no systematic error specified', status)
      ENDIF
      
      IF (.NOT. qgroup) THEN
         CALL ftpkyj (ounit, 'GROUPING', 0,
     &        'no grouping of the data has been defined', status)
      ENDIF
 
      IF (.NOT. qqual) THEN
         CALL ftpkyj (ounit, 'QUALITY', 0,
     &                'no data quality information specified', status)
      ENDIF

      IF (status .NE. 0) THEN
         ierr = 3
         RETURN
      ENDIF

C     Write XSPEC mandatory keywords.

C     Write telescope and instrument keywords.
      CALL ftpkys (ounit, 'TELESCOP', tlscpe,
     &     'telescope (mission) name', status)
 
      CALL ftpkys (ounit,'INSTRUME', instrm,
     &     'instrument name', status)

C     Write filter keyword.
      CALL ftpkys (ounit, 'FILTER', filter,
     &     'instrument filter in use', status)

C     Write exposure keyword. cawh 30-apr-2004
      CALL ftpkye (ounit, 'EXPOSURE', texpos, 4,
     &     'exposure time = 110s * number occultation steps', status)

C     Write area scaling factor.
      CALL ftpkye (ounit, 'AREASCAL', ascale, 4,
     &     'nominal effective area', status)

C     Write background scaling factor.
      CALL ftpkye (ounit, 'BACKSCAL', bscale, 4,
     &     'background scale factor', status)

C     Write correlation scaling factor.
      CALL ftpkye (ounit, 'CORRSCAL', cscale, 4,
     &     'correction scale factor', status)

C     Write background, correction, RMF and ARF filenames.
      CALL ftpkys (ounit, 'BACKFILE', assfls(1),
     &     'background FITS file for this source', status)
      
      CALL ftpkys (ounit, 'CORRFILE', assfls(3),
     &     'correction FITS file for this source', status)
      
      CALL ftpkys (ounit, 'RESPFILE', assfls(2),
     &     'redistribution matrix (RMF)', status)
      
      CALL ftpkys (ounit, 'ANCRFILE', assfls(4),
     &     'ancillary response matrix (ARF)', status)

C     Write any XSPEC filter keywords.

C     Write channel type.
      CALL ftpkys (ounit, 'CHANTYPE', 'PHA',
     &     'channels assigned by detector electronics', status)

C     Write number of detector channels.
      CALL ftpkyj (ounit, 'DETCHANS', nchann,
     &     'total no. detector channels available', status)

C     Write FITS format used.
      CALL ftpkys (ounit, 'PHAVERSN', '1992a',
     &     'OGIP classification of FITS format style', status)

      IF (status .NE. 0) THEN
         ierr = 4
         RETURN
      ENDIF

C     Write out optional keywords - these are mostly repeats of those 
C     in primary header.
      CALL ftpkys (ounit, 'OBJECT', srcnam,
     &     'name of observed source', status)

      CALL ftpkys (ounit, 'FILENAME', bodfil,
     &     'file that FITS was produced from', status)

      CALL ftpkys (ounit, 'ORIGIN', 'FTOOLS',
     &     'origin of FITS file', status)

      CALL ftpdat (ounit, status)

      CALL ftpkys (ounit,'TELESCOP', tlscpe,
     &     'telescope (mission) name', status)
      
      CALL ftpkys (ounit,'INSTRUME', instrm,
     &     'instrument name', status)

      CALL ftpkye (ounit,'RA-NOM', ranom, 4,
     &     'right ascension of target (deci. deg)', status)

      CALL ftpkye (ounit,'DEC-NOM', decnom, 4,
     &     'declination of target (deci. deg)', status)

      CALL ftpkye (ounit,'DROLLANG', roll, 4,
     &     'mean roll angle (deci. deg)', status)

      CALL ftpkyf (ounit, 'EQUINOX', equinx, 1,
     &     'equinox of celestial coord system', status)

      CALL ftpkys (ounit, 'DATE-OBS', sdsstr,
     &     'date observations were made (yyyy-mm-dd)', status)
 
      CALL ftpkys (ounit, 'TIME-OBS', shsstr,
     &     'time observations were made (hh:mm:ss)', status)

      CALL ftpkys (ounit, 'DATE-END', edsstr,
     &     'date observations were made (yyyy-mm-dd)', status)
 
      CALL ftpkys (ounit, 'TIME-END', ehsstr,
     &     'time observations were made (hh:mm:ss)', status)

      IF (status .NE. 0) THEN
         ierr = 5
         RETURN
      ENDIF

C     Write out spectrum data.
      
C     Define binary table.
      CALL ftbdef (ounit, tfields, tform, 0, nchann, status)
      
C     Write elements into table.
      CALL ftpcli (ounit, 1, 1, 1, nchann, chan, status)

      IF (dtype .EQ. 1) THEN
         CALL ftpcli (ounit, 2, 1, 1, nchann, ipha, status)
      ELSEIF (dtype .EQ. 2) THEN
         CALL ftpcle (ounit, 2, 1, 1, nchann, cts, status)
      ENDIF

      colnum = 2
      IF (qerror) THEN
         colnum = colnum + 1
         CALL ftpcle (ounit, colnum, 1, 1, nchann, sig, status)
      ENDIF
      IF (qsys) THEN
         colnum = colnum + 1
         CALL ftpcle (ounit, colnum, 1, 1, nchann, sysfrc, status)
      ENDIF
      IF (qqual) THEN
         colnum = colnum + 1
         CALL ftpcli (ounit, colnum, 1, 1, nchann, qualty, status)
      ENDIF
      IF (qgroup) THEN
         colnum = colnum + 1
         CALL ftpcli (ounit, colnum, 1, 1, nchann, group, status)
      ENDIF

      IF (status .NE. 0) THEN
         ierr = 6
         RETURN
      ENDIF
      
C     Close FITS file.
      CALL ftclos (ounit, status)
      
      IF (status .NE. 0) THEN
         ierr = 7
         RETURN
      ENDIF

      RETURN
      END
