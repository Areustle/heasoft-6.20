C  FTOOLs info $Header: /headas/headas/ftools/asca/src/ghkcurve/ghkcurve.f,v 3.16 2013/05/21 19:08:06 irby Exp $
C
C     ghkcurve: Extract light curve from ASCA GIS housekeeping file.
C
C     This program constructs a light curve file from the specified
C     monitor-count entries in the ASCA GIS housekeeping files.  A
C     monitor count is a simple running count, modulo 256, of the
C     received photons that fall in certain ranges of the pulse-height
C     and rise-time discriminators (the L1 count is included in the GIS
C     PH mode data).  The program is based on Ken Ebisawa's
C     make_l1_curve.f.
C
C     The monitor count entries are written to the housekeeping (HK)
C     file at several intervals, depending on the bit rate in effect.
C     This program will integrate at the HIGH (0.125 sec) or MEDIUM (1.0
C     sec) (for LDHIT, 0.0625 and 0.5 sec) rates, selecting only entries
C     at those rates, or at the MEDIUM rate using entries at both.  Any
C     one HK file may contain all bit rates; you would expect the GTI
C     file to select the right one, but the 'maketime' program includes
C     half the previous interval in a GTI, so we have to keep track of
C     the bit rates ourselves.
C
C     As of version 3.0, ghkcurve checks the input HK file for an ALLGTI
C     extension.  If one is found, it is ANDed with the user's input GTI.
C
C     ASCA time is assumed throughout (in particular, in gtifile), and this
C     is currently NOT checked.  It should be... better yet, use MJDREF etc
C     to do conversions.
C
C     Jeff Guerber, HSTX, Feb. 1996
C
C
C     PARAMETERS:
C
C     infile, outfile, gtifile = input, output, and Good Time Interval files
C     irate = integration rate, 'medium', 'high', or 'med+hi' (actually any
C             string that has both 'med' and 'hi' as substrings)
C
C
C     EXTERNAL SUBPROGRAMS CALLED:
C
C     gtimerge -- AND or OR two gti lists, from gtilib.f
C     ft*  -- FITSIO
C     copyhead -- copy header keywords, from ftools/library/misc.for
C     fcpars -- parse filespec for name and extension, from misc.for
C     fcecho -- write message to standard output, from misc.for
C     fcerr -- write message to standard error, from misc.for
C
C $Log: ghkcurve.f,v $
C Revision 3.16  2013/05/21 19:08:06  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.15  2002/12/26 16:30:41  irby
C Split data statement out of rcsid declaration for f90 compatibility.
C
C Revision 3.14  2001/12/06 21:18:30  irby
C Uninitialized variable fixes from K. Mukai.
C
C Revision 3.13  1997/05/03 07:32:28  guerber
C ghkcurve 3.0:  Read ALLGTI from infile, if found merge with user gti.
C Write merged gti to outfile, don't just copy user's. Write checksum keywords.
C Handle timezero. More lenient on gtifile extnames. Larger gti arrays. Etc.
C
C Revision 3.12  1997/03/20 04:33:00  guerber
C Reset integration on SF_LOST, indicating telemetry loss.
C User can specify which monitor count (L1,LDHIT,etc), via new `monitor' param.
C Skip GTIs by specifying `gtifile=none'.
C TSTART, TSTOP keywords now the actual first and last times that went into
C the lightcurve.  Various comment changes.
C
C Revision 3.11  1996/10/24 23:27:13  dunfee
C jdd -- syncing main develop trunk with release branch.
C
C Revision 3.10.2.1  1996/09/19 03:50:55  guerber
C Updated comments.
C
C Revision 3.10  1996/09/04  20:07:32  guerber
C Rewrote file-processing section.  HKL1GET now handles bitrate and GTI
C checking and only returns good L1 values, simplifying things.  Reset
C time-step bases after a bitrate change or new GTI.  Warn on missing EXTNAME.
C
C Revision 3.9  1996/08/24  05:15:38  guerber
C Irate can combine "high", "medium" to use both bitrates (integration is done
C at 1 sec unless *only* "high"). Improved error reporting; increased buffers.
C
C Revision 3.8  1996/07/09  04:40:38  guerber
C Added "GTI" as an acceptable GTI extname.  Changed GTI and infile extname
C errors to warnings.  Also print status number with fitsio errors.
C
C Revision 3.7  1996/06/21 05:08:01  guerber
C Made program aware of bit rates.  Also make sure *both* ends of an
C interval are in a GTI and have the right bit rate.  Other minor changes.
C
C
C--------------------------------------------------------------------------
C
      subroutine ghkcue
C
      implicit none
C
C     PARAMETERS
C
C     infile, outfile, gtifile = input, output, and Good Time Interval files
C     irate = integration rate, 'medium', 'high', or 'med+hi'
C     monitor = which monitor count (eg, L1, H0, LDHIT)
C
      character(180)  infile, outfile, gtifile
      character(10)  irate, monitor
C
C     LOCAL VARIABLES
C
C     Good Time Intervals
C
      integer gtisz
      parameter (gtisz = 2000)
C         = size of GTI arrays
      logical realgti, allgti
C         = are we using a real gti, or fake one because gtifile='NONE'?
C           did the input HK file have an allgti or is it faked?
      integer  gtiunit
C         = unit number
      character(180)  gtiname
C         = name of gti file w/o extension number
      integer  gtiext
C         = extension number
      double precision  gtistart(gtisz), gtistop(gtisz)
C         = user gti data (allgti gets merged in)
      double precision  algstart(gtisz), algstop(gtisz)
C         = HK file ALLGTI data
      double precision  gtitime0, algtime0
C         = TIMEZERO keywords from user GTI and HK ALLGTI
      integer gtirows, algrows
C         = number of rows in gtifile; in allgti table
      integer gtihdutyp
C         = type of HDU
      character(6)  gtinone
C         = temp variable to check for gtifile='none'
C
C     Output File
C
C     . size of output buffers
      integer obufsz
      parameter (obufsz = 5000)
C     . column buffers: time and counts
      double precision  otimbuf(obufsz)
      integer  ocntbuf(obufsz)
C     . output buffer counter; total number of records written
      integer  nout, outrows
C     . file characteristics
      character(10)  otype(2), oform(2), ounits(2)
      integer outunit
C
C     Input File
C
C     . unit number
      integer  inunit
C     . HDU type
      integer  hdutype
C     . name of instrument; name of HK file entries
      character(8)  instr, l1name
C     . number of rows; number of L1 records read
      integer  inrows, nl1
C     . flag ret. by hkl1get when input file exhausted
      logical  done
C     . were any read values set to the null value?
      logical  anyf
      character(80)  sctime(4)
C        = spacecraft time keywords (MJDREF, TIMEREF, TIMESYS, TIMEUNIT)
C
C     Time intervals
C
C     . bottom, middle, top of time interval, and size
      double precision  tlo, tmid, thi, dt
C     . midpoint of 1st interval, and absolute range
      double precision  tzero, tstart, tstop
C     . tolerance on time endpoints
      double precision  eps
      parameter (eps = 0.05d0)
      logical reset
C
C     Data
C
      double precision l1time
C         = time of current entry
      integer  l1value, l1prev
C         = current and previous values
      integer  counts
C         = sum of counts in the current time interval
      double precision  hktime0
C         = TIMEZERO keyword, if any, else 0.
C
C     Bit Rate
C
      integer  brvalue
C         = current bit rates
      character(8)  brname
C         = 'G2_BRATE' or 'G3_BRATE', depending on which detector
      double precision  brtime
C         = time of last BRATE
      logical  brtbl(4)
      data brtbl/4*.false./
C         = brtbl(i) is true if 'i' is an acceptable bit rate
C
C     Miscellaneous
C
      character(80)  msg, comment
C         = output messages; comment read from fits header line
      character(8)  extname
C         = name of fits-file extension
      integer  status
C         = fitsio status
      integer  blocksize
C         = blocksize of fits file
      logical  ingti
C         = logical function to check GTI, later in file
      integer  i
C
C     The following are functions used with copyhead()
C
      logical  gcrvpbad, gcrvrbad, gcrvrgood, gooddummy
      external gcrvpbad, gcrvrbad, gcrvrgood, gooddummy
C
      character(40) taskname
      common /task/ taskname
      character(70) rcsid
      data rcsid
     &/'$Id: ghkcurve.f,v 3.16 2013/05/21 19:08:06 irby Exp $'/
C
C----------------------------------------------------------------------
C     cvs rev <-> taskname rev: 3.5,3.6 <-> 1.0; 3.7 <-> 1.2; 3.8 <-> 1.3;
C     3.9 <-> 1.4; 3.10,3.11 <-> 2.0; 3.12 <-> 2.1; 3.13 <-> 3.0
C
      taskname = 'ghkcurve 3.0'
      status = 0
C
C     Call gcrvparm to get parameters
C
      call gcrvparm( infile, outfile, gtifile, irate, monitor, status )
      if (status .ne. 0) goto 990
      call ftupch( monitor )
C
C     Set dt for specified integration rate, and fill in the table of
C     acceptable intrument bit rates.  There are 3 bit rates: low=1,
C     medium=2, and high=4, of which we can use medium and high.  brtbl can
C     be indexed by the value of the bit rate to see if it's acceptable.
C     They aren't mutually exclusive: "HIGH+MEDIUM" is OK; note that the
C     medium dt overrides the high one.  The LDHIT monitor count has twice
C     the time resolution, so divide dt by 2 for it.
C
C     . fitsio subr to upcase a string
      call ftupch( irate )
      if (index(irate,'HI') .ne. 0) then
          dt = 0.125
          brtbl(4) = .true.
      endif
      if (index(irate,'MED') .ne. 0) then
          dt = 1.0
          brtbl(2) = .true.
      endif
      if (.not.( brtbl(1) .or. brtbl(2) .or. brtbl(3) .or. brtbl(4) ))
     &    then
          msg = 'Unsupported integration rate: ' // irate
          goto 995
      endif
      if (monitor .eq. 'LDHIT')  dt = dt / 2.
C
C     . get 3 unit numbers
      call ftgiou( inunit, status )
      call ftgiou( outunit, status )
      call ftgiou( gtiunit, status )
C
C     OPEN AND READ THE GTI FILE (or if gtifile=NONE, make a fake GTI)
C     (Note: It might be a good idea to run checks on the GTIs.)
C
      gtinone = gtifile(1:6)
      call ftupch( gtinone )
      realgti = .not. (gtinone .eq. 'NONE  ')
      if (realgti) then
C
C         fcpars (ftools/library/misc.for) parses gtifile for name and
C         extension; assume extension 1 if not specified (1st ext in
C         ftools = 2nd HDU in fitsio)
C
          call fcpars( gtifile, gtiname, gtiext, status )
          if (gtiext .lt. 0) gtiext = 1

C         Open GTI file
          call ftopen( gtiunit, gtiname, 0, blocksize, status )
C         Advance to extension
          call ftmahd( gtiunit, gtiext+1, gtihdutyp, status )
C         Check extension name
          call ftgkys( gtiunit, 'EXTNAME', extname, comment, status )
          if ((status .ne. 0) .and. (status .ne. 202)) goto 990
          status = 0
          if (index(extname, 'GTI') .eq. 0) then
              msg = 'WARNING: gtifile extname says this is not a GTI: '
     &            //  extname
              call fcerr( msg )
          endif

C         How many rows in gti table?
          call ftgkyj( gtiunit, 'NAXIS2', gtirows, comment, status )
          if (status .ne. 0) goto 990
          if (gtirows .gt. gtisz) then
              write (msg,*) 'Internal error!  GTI array too small,',
     &            ' NAXIS2=', gtirows
              goto 995
          endif

C         Read the timezero keyword. 202 means there isn't one.
          gtitime0 = 0.d0
          call ftgkyd( gtiunit, 'TIMEZERO', gtitime0, comment, status )
          if (status .eq. 202) status = 0

C         Read the two columns of the gti table, then add timezero. (nulls??)
          call ftgcvd( gtiunit, 1, 1, 1, gtirows, 0.d0, gtistart,
     &        anyf, status )
          call ftgcvd( gtiunit, 2, 1, 1, gtirows, 0.d0, gtistop,
     &        anyf, status )
          if (status .ne. 0) goto 990
          do i = 1, gtirows
              gtistart(i) = gtistart(i) + gtitime0
              gtistop(i) = gtistop(i) + gtitime0
          enddo
      else
C
C         Gtifile was NONE, so fake it
C
          gtirows = 1
          gtistart(1) = 0.d0
          gtistop(1) = 1.d10
      endif
C
C     OPEN THE INPUT FILE, READONLY
C
      call ftopen( inunit, infile, 0, blocksize, status )
      if (status .ne. 0) goto 990
C
C     SET UP THE OUTPUT FILE
C
C     . initialize it; blocksize=1 gives standard 2880-byte FITS blocks
      call ftinit( outunit, outfile, 1, status )
      if (status .ne. 0) goto 990
C
C     Create the primary HDU. Use copyhead to copy the input primary HDU
C     header to the output file, including the basic and scale keywords,
C     except for those specified in the function gcrvpbad (in this file).
C     For an HK file, the primary HDU has no data.
C
      call copyhead( inunit, outunit, .false., .false., gcrvpbad,
     &    gooddummy, status )
      if (status .ne. 0) goto 990
C
C     Add extra keywords.  TSTART and TSTOP will be updated later with
C     their true values.  Reserve 2 extras for the checksum keywords.
C
      call ftpkyd( outunit, 'TSTART', 0.d0, 15,
     &    'data start time', status )
      call ftpkyd( outunit, 'TSTOP', 0.d0, 15,
     &    'data stop time', status )
C     . write a date header record
      call ftpdat( outunit, status )
C     . put string-value header records
      call ftpkys( outunit, 'FNAME', outfile, 'File name', status )
      call ftpkys( outunit, 'CREATOR', taskname,
     &    'Program creating this FITS file', status )
C     . put history header records
      call ftphis( outunit, ' ', status )
      comment = 'Created by ' // taskname
      call ftphis( outunit, comment, status )
      comment = '  from HK file: ' // infile
      call ftphis( outunit, comment, status )
      comment = '  writing to:   ' // outfile
      call ftphis( outunit, comment, status )
      comment = '  using GTI:    ' // gtifile
      call ftphis( outunit, comment, status )
      comment = '  integration rate: ' // irate
      call ftphis( outunit, comment, status )
      comment = '  monitor count: ' // monitor
      call ftphis( outunit, comment, status )
      call ftphis( outunit, ' ', status )
      call fthdef( outunit, 2, status )
      if (status .ne. 0) goto 990
C
C     Advance input file to the second hdu
C
      call ftmahd( inunit, 2, hdutype, status )
      if (status .ne. 0) goto 990
C
C     Define and create the new RATE hdu.  Yes, many of the header-keyword
C     calls are the same as for the primary header, above.  TSTART and TSTOP
C     will be updated later with their true values.
C
      otype(1) = 'TIME'
      oform(1) = '1D'
      ounits(1) = 'sec'
      otype(2) = 'COUNTS'
      oform(2) = '1J'
      ounits(2) = 'counts/bin'
C     Initialize a binary table; nrows=1 is dummy
      call ftibin( outunit, 1, 2, otype, oform, ounits, 'RATE', 0,
     &    status)
C     Reserve space for 35 more header keywords (these, copyhead, checksums)
      call fthdef( outunit, 35, status )
C
      call ftpkyd( outunit, 'TSTART', 0.d0, 15,
     &    'data start time', status )
      call ftpkyd( outunit, 'TSTOP', 0.d0, 15,
     &    'data stop time', status )
      call ftpkyd( outunit, 'TIMEZERO', 0.0d0, 15,
     &    'Time Zero', status )
      call ftpkyd( outunit, 'TIMEDEL', dt, 4,
     &    'Minimum Time Resolution', status )
      if (status .ne. 0) goto 990
C
C     Call copyhead to copy the keywords specified by gcrvrgood, but no others.
C
      call copyhead( inunit, outunit, .true., .true., gcrvrbad,
     &    gcrvrgood, status )
      if (status .ne. 0) goto 990
C
C     . write a date header record
      call ftpdat( outunit, status )
C     . put string-value header records
      call ftpkys( outunit, 'FNAME', outfile, 'File name', status )
      call ftpkys( outunit, 'CREATOR', taskname,
     &    'Program creating this FITS file', status )
C     . put history header records
      call ftphis( outunit, ' ', status )
      comment = 'Created by ' // taskname
      call ftphis( outunit, comment, status )
      comment = '  from HK file: ' // infile
      call ftphis( outunit, comment, status )
      comment = '  writing to:   ' // outfile
      call ftphis( outunit, comment, status )
      comment = '  using GTI:    ' // gtifile
      call ftphis( outunit, comment, status )
      comment = '  integration rate: ' // irate
      call ftphis( outunit, comment, status )
      comment = '  monitor count: ' // monitor
      call ftphis( outunit, comment, status )
      call ftphis( outunit, ' ', status)
      if (status .ne. 0) goto 990
C
C     READ THE HK FILE ALLGTI EXTENSION.  For now, assume 2nd extension (HDU 3)
C
      call ftmahd( inunit, 3, hdutype, status )
      if ( status .eq. 107 ) then
C         No ALLGTI extension, so fake it.
          status = 0
          allgti = .false.
          algrows = 1
          algstart(1) = 0.d0
          algstop(1) = 1.d10
          call fcecho('ghkcurve: Input file does not have an ALLGTI.')
C
      elseif( status .ne. 0 ) then
          goto 990
      else
C
C         Extension name must be ALLGTI.  202 means no such keyword.
          call ftgkys( inunit, 'EXTNAME', extname, comment, status )
          if ( (extname .ne. 'ALLGTI') .or. (status .eq. 202)) then
              msg = '3rd HDU in infile is not an ALLGTI extension'
              goto 995
          endif
          if (status .ne. 0) goto 990
C
C         How many rows?
          call ftgkyj( inunit, 'NAXIS2', algrows, comment, status)
          if (status .ne. 0) goto 990
          if (algrows .gt. gtisz) then
              write(msg,*) 'Internal error! ALLGTI array too small,',
     &            ' NAXIS2=', algrows
              goto 995
          endif

C         Read timezero keyword, if any.
          algtime0 = 0.d0
          call ftgkyd( inunit, 'TIMEZERO', algtime0, comment, status )
          if (status .eq. 202) status = 0

C         Read the allgti table
          call ftgcvd( inunit, 1, 1, 1, algrows, 0.d0, algstart,
     &        anyf, status )
          call ftgcvd( inunit, 2, 1, 1, algrows, 0.d0, algstop,
     &        anyf, status )
          if (status .ne. 0) goto 990
          do i = 1, algrows
              algstart(i) = algstart(i) + algtime0
              algstop(i) = algstop(i) + algtime0
          enddo
          allgti = .true.
          call fcecho('ghkcurve: ALLGTI found in input HK file, ' //
     &        'will be applied to data.' )
      endif
C
C     Now AND the two lists.  Put result in gtistart/gtistop/gtirows.
C
      call gtimerge( 'AND', gtistart, gtistop, gtirows, gtisz,
     &    gtistart, gtistop, gtirows, algstart, algstop, algrows,
     &    status)
      if (status .ne. 0) then
          write (msg,*) 'Error from GTIMERGE, status= ', status
          goto 995
      endif
      if (gtirows .le. 0) then
          msg = 'No valid GTIs after merge, check the input lists!'
          goto 995
      endif
C
C     REPOSITION THE INPUT FILE and run a few checks on it
C
      call ftmahd( inunit, 2, hdutype, status )
      if (hdutype .ne. 2) then
          msg = '2nd HDU in infile is not a binary table'
          goto 995
      endif
C
C     Get extension name, should be 'HK' (Housekeeping)
      call ftgkys( inunit, 'EXTNAME', extname, comment, status )
      if ((status .ne. 0) .and. (status .ne. 202)) goto 990
      status = 0
      if (extname .ne. 'HK      ') then
          msg = 'WARNING: infile extname is not ''HK'': '//extname
          call fcerr( msg )
      endif
C
C     Get instrument name (must be GIS2 or GIS3), set l1name and brname
      call ftgkys( inunit, 'INSTRUME', instr, comment, status)
      if (instr .eq. 'GIS2    ') then
          l1name = 'G2_' // monitor
          brname = 'G2_BRATE'
      elseif (instr .eq. 'GIS3    ') then
          l1name = 'G3_' // monitor
          brname = 'G3_BRATE'
      else
          msg = 'bad instrument: ' // instr
          goto 995
      endif
C
C     Get the TIMEZERO keyword, if any
      hktime0 = 0.d0
      call ftgkyd( inunit, 'TIMEZERO', hktime0, comment, status )
      if (status .eq. 202) status = 0
C
C     Save the S/C time keywords for the output GTI table. Should really
C        compare with gtifile's; for now assume gtifile has ASCA time.
      call ftgcrd( inunit, 'MJDREF', sctime(1), status )
      call ftgcrd( inunit, 'TIMEREF', sctime(2), status )
      call ftgcrd( inunit, 'TIMESYS', sctime(3), status )
      call ftgcrd( inunit, 'TIMEUNIT', sctime(4), status )
C
C     NOW PROCESS THE FILES (finally!)
C
C     Initialize: read first L1 and use it to calculate first time step
C
C     Get number of rows in input file
      call ftgkyj( inunit, 'NAXIS2', inrows, comment, status )
      if (status .ne. 0) goto 990
C
      call hkl1get( inunit, inrows, hktime0, l1name, l1time, l1value,
     &    brname, brtbl, brtime, brvalue, gtirows, gtistart, gtistop,
     &    done, reset, status )
      if (status .ne. 0) goto 990
      nl1 = 1
      reset = .true.
      tstart = l1time
      tzero = l1time + dt/2.d0
C Added initialization of tlo and tmid --- KM, 2001 December
      tlo = l1time
      tmid = l1time + dt/2.d0
      thi = -1.
      l1prev = l1value
C
C     Loop on the time steps.  Exit when HKL1GET returns done=.true.,
C     indicating no more entries in the housekeeping file.
C
      do while (.not. done)
          counts = 0
          tstop = thi
C
C         While l1time is in range, sum up counts then read next L1.
C         If HKL1GET returns reset=.true., there was a bitrate change,
C         telemetry loss, or a new GTI since the last good L1, so stop
C         integrating and begin a new series of time intervals as if
C         starting over!
C
          do while( (tlo-eps .lt. l1time) .and. (l1time .lt. thi+eps)
     &          .and. .not. done .and. .not. reset )
C
              if (l1prev .le. l1value) then
                  counts = counts + l1value - l1prev
              else
                  counts = counts + l1value - l1prev + 256
              endif
C
              l1prev = l1value
C
              call hkl1get( inunit, inrows, hktime0, l1name, l1time,
     &            l1value, brname, brtbl, brtime, brvalue, gtirows,
     &            gtistart, gtistop, done, reset, status )
C
              if (status .ne. 0) goto 990
              if (.not. done)  nl1 = nl1 + 1

          enddo
C
C         Write the output record, if there are counts and it's in a GTI.
C
          if ( (counts .ne. 0) .and.
     &          ingti( tlo, thi, gtirows, gtistart, gtistop ) ) then
              nout = nout + 1
              otimbuf(nout) = tmid
              ocntbuf(nout) = counts
          endif
C
C         Flush the output buffer if necessary.
C
          if ((nout .eq. obufsz) .or. done) then
C             . put otimbuf in column 1
              call ftpcld( outunit, 1, outrows+1, 1, nout, otimbuf,
     &            status )
C             . put ocntbuf in column 2
              call ftpclj( outunit, 2, outrows+1, 1, nout, ocntbuf,
     &            status )
              if (status .ne. 0) goto 990
              outrows = outrows + nout
              nout = 0
          endif
C
C         Calculate the next time interval.  If HKL1GET returned reset=.true.
C         (indicating a bitrate change, telemetry loss, or a new GTI),
C         start a new series of intervals based on this l1time.
C
          if (reset) then
              tlo = l1time
              tmid = l1time + dt/2.d0
              thi = l1time + dt
              reset = .false.
              l1prev = l1value
          else
              tmid = tmid + dt
              thi = tmid + dt / 2.d0
              tlo = tmid - dt / 2.d0
          endif
C
      enddo
C
C     update NAXIS2, TSTART, and TSTOP keywords and write checksums
C     '&' keeps same comment
C
      call ftmkyj( outunit, 'NAXIS2', outrows, '&', status )
      call ftmkyd( outunit, 'TSTART', tstart, 15, '&', status )
      call ftmkyd( outunit, 'TSTOP', tstop, 15, '&', status )
      call ftpcks( outunit, status )
      if (status .ne. 0) goto 990
C
      write (msg, 10)  outrows, nl1, l1name
   10 format ('ghkcurve:', i7, ' output records from ', i7,
     &    '  good input ', a8, ' records.')
      call fcecho( msg )
C
C     WRITE THE GTI TABLE
C
      if (realgti .or. allgti) then
C
          otype(1) = 'START'
          oform(1) = '1D'
          ounits(1) = 'sec'
          otype(2) = 'STOP'
          oform(2) = '1D'
          ounits(2) = 'sec'
          call ftibin( outunit, gtirows, 2, otype, oform, ounits, 'GTI',
     &        0, status )
C
C         MJDREF, TIMEREF, TIMESYS, TIMEUNIT copied from HK table:
          do i = 1, 4
              call ftprec( outunit, sctime(i), status )
          enddo
C
          call ftpkyd( outunit, 'TIMEZERO', 0.0d0, 15,
     &        'Time Zero', status )
          call ftpdat( outunit, status )
          call ftpkys( outunit, 'CREATOR', taskname,
     &        'Program creating this FITS file', status )
          call ftphis( outunit, ' ', status )
          comment = 'Created by ' // taskname
          call ftphis( outunit, comment, status )
          if (realgti) then
              comment = '   using GTI in ' // gtifile
              call ftphis( outunit, comment, status )
          endif
          if (allgti) then
              comment = '   using ALLGTI in ' // infile
              call ftphis( outunit, comment, status )
          endif
          call ftphis( outunit, ' ', status )
C         2 extra keywords for the checksums:
          call fthdef( outunit, 2, status )
C
C         Write the data:
          call ftpcld( outunit, 1, 1, 1, gtirows, gtistart, status )
          call ftpcld( outunit, 2, 1, 1, gtirows, gtistop, status )
C
C         Write checksums:
          call ftpcks( outunit, status )
C
          if (status .ne. 0) goto 990
      endif
C
C     GO BACK TO THE PRIMARY HDU TO UPDATE TSTART, TSTOP; WRITE CHECKSUMS
C
      call ftmahd( outunit, 1, hdutype, status )
      call ftmkyd( outunit, 'TSTART', tstart, 15, '&', status )
      call ftmkyd( outunit, 'TSTOP', tstop, 15, '&', status )
      call ftpcks( outunit, status )
      if (status .ne. 0) goto 990
C
C     CLOSE THE FITS FILES AND FREE THE UNIT NUMBERS
C
      call ftclos(inunit, status)
      call ftfiou(inunit, status)
      call ftclos(outunit, status)
      call ftfiou(outunit, status)
      if (realgti) call ftclos(gtiunit, status)
      call ftfiou(gtiunit, status)
      if (status .ne. 0) goto 990
C
      return
C
C     FITSIO ERROR OCCURRED
C
  990 write (msg, '(''FITSIO error, status = '', i5)') status
      call fcerr(msg)
      call ftgerr(status, msg)
      do while (msg .ne. ' ')
          call fcerr(msg)
C         .ftgmsg returns blank when stack is empty
          call ftgmsg(msg)
      enddo
      call fcerr('Fatal error occurred, ghkcurve aborted.')
      return
C
C     OTHER (NON-FITSIO) ERROR OCCURRED
C
  995 call fcerr(msg)
      call fcerr('Fatal error occurred, program aborted.')
      return
      end
C
C----------------------------------------------------------------------
C     subroutine gcrvparm: get and parse the parameters
C
C     infile = input housekeeping file
C     outfile = output light curve file
C     gtifile = good time interval file (with extension)
C     irate = integration rate, 'high' or 'medium' or combination
C
      subroutine gcrvparm( infile, outfile, gtifile, irate,
     &    monitor, status )
C
      implicit none
      character*(*)  infile, outfile, gtifile, irate, monitor
      integer  status

      call uclgst('infile', infile, status)
      if ( status .ne. 0 ) then
          call fcerr( 'gcrvparm: could not get infile' )
          goto 1999
      endif

      call uclgst('outfile', outfile, status)
      if ( status .ne. 0 ) then
          call fcerr( 'gcrvparm: could not get outfile' )
          goto 1999
      endif

      call uclgst('gtifile', gtifile, status)
      if ( status .ne. 0 ) then
          call fcerr( 'gcrvparm: could not get gtifile' )
          goto 1999
      endif

      call uclgst('irate', irate, status)
      if ( status .ne. 0 ) then
          call fcerr( 'gcrvparm: could not get irate' )
          goto 1999
      endif

      call uclgst('monitor', monitor, status)
      if ( status .ne. 0 ) then
          call fcerr( 'gcrvparm: could not get monitor' )
          goto 1999
      endif

 1999 return
      end
C------------------------------------------------------------------------
C     hkl1get -- return next good specified monitor count from an ASCA
C     GIS House Keeping file, skipping those that have the wrong bit
C     rate or aren't in a GTI
C
C     Note: This routine assumes that the file has been opened, positioned
C     to the correct HDU, and that the number of rows has been placed in
C     nrows.  Furthermore, it assumes the standard structure for GHK files:
C     Field 1 has an 8A name, 2 has a 1J value, and 3 has a 1D timestamp.
C     (It would be wise to check in the calling program that this is true.)
C
      subroutine hkl1get( inunit, nrows, hktime0, l1name, l1time,
     &    l1value, brname, brtbl, brtime, brvalue, gtirows, gtistart,
     &    gtistop, done, reset, status )
C
C     Arguments:
C
C     inunit = unit number of input file (input)
C     nrows = number of rows in the table (input)
C     hktime0 = time to be added for comparisons (input)
C     l1name = of desired line, 'G2_L1' or 'G3_L1' (input)
C     l1time = time stamp (output)
C     l1value = the value (output)
C     brname = of desired bit rate line, 'G2_BRATE' or 'G3_BRATE' (input)
C     brtbl = brtbl(bitrate)=.true. if bitrate is acceptable (input)
C     brtime = time of last BRATE line (output or unchanged)
C     brvalue = last bit rate value (output or unchanged)
C     gtirows = number of rows in the GTI table (input)
C     gtistart = starting times of GTIs (input)
C     gtistop = ending times of GTIs (input) (assume GTI's timezero applied)
C     done = set to .true. if no more entries in HK file (output)
C     reset = .true. if bit rate or GTI changed since last L1 returned (out)
C     status = fitsio status (out)
C
      implicit none
      integer inunit, nrows, l1value, brvalue, status, gtirows
      character*(*) l1name, brname
      double precision hktime0, l1time, brtime, gtistart(*), gtistop(*)
      logical done, reset, brtbl(*)
C
C     Local variables
C
C     bufsz = size of the buffers, a parameter
C     nambuf, valbuf, timbuf = buffers for the table
C     rowsread = total number of rows read from the table
C     ibuf = counter for current element in the buffers
C     nbuf = how many rows to read this time (bufsz or number left)
C     anyflag = did fitsio say anything is flagged as null?
C     ingti = is it in a GTI
C
      integer  bufsz
      parameter (bufsz = 5000)
      character(8)  nambuf(bufsz)
      integer  valbuf(bufsz)
      double precision  timbuf(bufsz)
      integer  rowsread, ibuf, nbuf
      data  rowsread/0/, ibuf/bufsz/, nbuf/0/
      save  nambuf, valbuf, timbuf, rowsread, ibuf, nbuf
      logical  anyflag
      logical  ingti
C
C     Loop until we find a NAME that matches L1NAME.  If we reach the end of
C     the buffer, read another.  Note that ibuf is initialized to bufsz, which
C     forces buffer reads on the first call.  Watch for bit rates and telemetry
C     loss flags, and set reset if we see one or if an L1 is outside a gti.
C
      do while (.true.)
          ibuf = ibuf + 1

          if (ibuf .gt. nbuf) then
C
C             If there's no more to read, return with done=.true.
C
              if (rowsread .eq. nrows) then
                  done = .true.
                  return
              endif
C
C             Read appropriate columns into the input buffers.
C             Read (nbuf) elements, starting at the (rowsread+1) row of
C             the table and the first element.  Reset rowsread and ibuf.
C             Return immediately on bad status (this may need to be changed).
C
              nbuf = min( bufsz, nrows-rowsread )
              call ftgcvs( inunit, 1, rowsread+1, 1, nbuf, ' ',
     &            nambuf, anyflag, status )
              call ftgcvj( inunit, 2, rowsread+1, 1, nbuf, 0,
     &            valbuf, anyflag, status )
              call ftgcvd( inunit, 3, rowsread+1, 1, nbuf, 0.d0,
     &            timbuf, anyflag, status )
              if (status .ne. 0) return

              rowsread = rowsread + nbuf
              ibuf = 1
          endif
C
C         If it happens to be a BRATE line, set the bitrate
C
          if ( nambuf(ibuf) .eq. brname ) then
              brtime = timbuf(ibuf) + hktime0
              brvalue = valbuf(ibuf)
              reset = .true.
          endif
C
C         Telemetry loss flag, need to start a new series.
C
          if ( nambuf(ibuf) .eq. 'SF_LOST ' )  reset = .true.
C
C         If we've found a match that's in a GTI, set output args and return.
C
          if ( nambuf(ibuf) .eq. l1name ) then
              if ( ingti(timbuf(ibuf)+hktime0, timbuf(ibuf)+hktime0,
     &                 gtirows, gtistart, gtistop)
     &            .and. brtbl(brvalue) ) then

                  l1value = valbuf(ibuf)
                  l1time = timbuf(ibuf) + hktime0
                  done = .false.
                  return

              else
                  reset = .true.
              endif
          endif
      enddo
      end
C
C-----------------------------------------------------------------------
C
C     ingti -- Is the time range TLO to THI (could be the same) completely
C     within one of the GTIROWS Good Time Intervals, whose endpoints are
C     specified in the arrays GTISTART and GTISTOP?
C
      logical function ingti( tlo, thi, gtirows, gtistart, gtistop )
C
      implicit none
      double precision tlo, thi, gtistart(*), gtistop(*)
      integer  gtirows, i
C
      do i = 1, gtirows
          if ((gtistart(i) .le. tlo) .and. (thi .le. gtistop(i))) then
              ingti = .true.
              return
          endif
      enddo
      ingti = .false.
      return
      end
C
C-----------------------------------------------------------------------
C
C     This routine tells copyhead which keywords to NOT copy from the
C     input HK-file's *primary* HDU to the output file's *primary* HDU.
C     These are things that are irrelevant to a rate file or are superceded
C     by the real values.
C
      logical function gcrvpbad(keyrec)
      implicit none
      character(80)  keyrec
      integer nbad, i
      parameter (nbad=15)
      character(8)  badhdrs(nbad)
      data  badhdrs/'TSTART','TSTOP','TELAPSE','COORDPRO','NEVENTS',
     &    'BIT_RATE','DATAMODE','DATE','CONTENT','CREATOR','QPOENAME',
     &    'FNAME','ONTIME','CHECKSUM','DATASUM'/
      do i = 1, nbad
          if (index(keyrec(1:8), badhdrs(i)) .ne. 0) then
              gcrvpbad = .true.
              return
          endif
      enddo
      gcrvpbad = .false.
      return
      end
C-----------------------------------------------------------------------
C
C     These two routines should tell copyhead to copy ONLY the specified
C     keywords from the input HK-file's 'HK' extension header to the
C     header of the output file's 'RATE' extension.  Note that the files
C     must already be properly positioned to these HDUs.  For copyhead,
C     the GOOD header list only *overrides* the BAD one.  Therefore we'll
C     initially call everything bad, then specify the ones to copy anyway.
C
      logical function gcrvrbad(keyrec)
      implicit none
      character(80) keyrec
      gcrvrbad = .true.
      return
      end
C
      logical function gcrvrgood(keyrec)
      implicit none
      character(80) keyrec
      integer ngood, i
      parameter (ngood=18)
      character(8)  goodhdrs(ngood)
      data  goodhdrs/'INSTRUME','ORIGIN','OBJECT','OBSERVER','RA_NOM',
     &    'DEC_NOM','TELESCOP','MJDREF','TIMEREF','TIMESYS','TIMEUNIT',
     &    'DATE-OBS','TIME-OBS','DATE-END','TIME-END','MJD-OBS',
     &    'CLOCKAPP','TASSIGN'/
      do i = 1, ngood
          if (index(keyrec(1:8), goodhdrs(i)) .ne. 0) then
              gcrvrgood = .true.
              return
          endif
      enddo
      gcrvrgood = .false.
      return
      end
