C  FTOOLs info $Header: /headas/headas/ftools/asca/src/timeconv/timeconv.f,v 3.11 2013/05/21 19:08:08 irby Exp $
C
C*****************************************************************************c
C SELECTOR TASK:
C
C     timeconv
C
C FILE:
C
C     timeconv.f
C
C DESCRIPTION:
C Routine to transform TIME column in a fitsfile from ASCATIME at satellite
C to ASCATIME at geocenter or Barycentric Dynamical Time (TDB).
C
C
C AUTHOR/DATE:
C
C       Yutaro Sekimoto,    Sep  1993
C       Kamae-Group, Dept. of Physics Univ. of Tokyo
C       ysekimoto@tkyvax.phys.s.u-tokyo.ac.jp
C
C MODIFICATION HISTORY:
C     Sep 6, 1993
C         Ver. 0.6 first release
C     Sep 8, 1993  Ver. 0.65
C         Change Keywords TSTART, TSTOP, EXPOSURE
C         Protection for the corrected time
C     Oct 1, 1993 Ver. 1.0 EAG
C	  Add frf.orbit file to parameter list
C	  allow for either ONTIME or EXPOSURE keyword
C	  default extension is 1
C     Dec 10, 1993 Ver 1.1 EAG
C         allow for either TIMEREF or TIMEFRAM keywords
C     May 22, 1994 Ver 1.2 YS
C         Change GTI time
C         Modification of geocen.c
C         Add Barycentric Time on ASCATIME
C     Jun 20, 1994 Ver 1.4 M.Hirayama
C         Change names of correction routines
C            barycen -> asca_barycen
C            barycen_init -> asca_barycen_init
C            geocen -> asca_geocen
C         Add arguments to open_ascadata (file_open, tstart, tstop)
C         Add arguments to asca_gticonv (file_open)
C         Convert TSTART and TSTOP in EVENT extension
C         Add subroutine move_to_extention
C         Remove ftmahd in open_ascadata and in asca_gticonv
C         Modify update_key drastically
C            to check if it is a primary header or not
C            to check keywords first at the beginning of the routine
C            to set comment at the beginning of the routine
C         Convert TSTART and TSTOP in PRIMARY extension
C         Add subroutine correct_asca_time
C         Modify to care TIMESYS in headers
C     Sep 16, 1994 Ver 1.4a M.Hirayama
C         Remove unused variables:
C            In subroutine open_ascadata:
C              "i", "xsize", "ival", "tcstln", "dmode", "bitrate", "keywd",
C              "pos_det", "detect", and "info"
C            In subroutine asca_timeconv:
C              "i", "j", "pha_bin", "ip", "jp", "pi_bin", "geotime",
C              "barytime", "tscale", "tzero", "info", "lastchar", and "fcstln"
C            In update_keyword:
C              "keyvals" and "keyvald"
C            In asca_gticonv:
C              "i", "blocksize", "geotime", "barytime", and "info"
C            In get_asca_event:
C              "ival"
C            In put_asca_event:
C              "ival" and "ival2"
C         Update keywords in STDGTI:
C         Remove tstart/tstop in arguments of update_key:
C         Remove tstart/tstop in arguments of open_ascadata:
C         Not to extract TSTART/TSTOP in open_ascadata:
C         Not to stop task in case of errors in updating keywords
C         Divide asca_geocen() into three functions:
C            asca_geocen_init().......Initializes asca_geocen
C            asca_geocen()............convert ASCATIME to geocentric time
C            asca_geocen_asca2mjd()...convert ASCATIME to MJD
C         Not to initialize asca_geocen/asca_barycen when not necessary
C         Add "extrapolate" to timeconv.par
C         Add "history" to timeconv.par
C         Add subroutine append_history to append HISTORY keywords
C     Apr 10, 1995 Ver 1.50 M.Hirayama
C         Update MJD-OBS keyword  (requested by GSFC)
C         Enrich error handling for errors returned by move_to_extension
C         Use ftgiou/ftfiou to get/free logical unit number
C         Fixed a bug in asca_gticonv()
C            to accept ONTIME instead of EXPOSURE if EXPOSURE not found
C     Feb 24, 1997   Jeff Guerber, GSFC 664/HSTX
C         open_asca_data and asca_gticonv: Changed exact to .false. so ftgcno
C             calls are case-insensitive.
C     Aug 20, 1998  Jeff Guerber, GSFC/HSTX
C         asca_timeconv, asca_gticonv:  Check number of events before
C             get_asca_event(), to avoid new fitsio error.
C         timeconvert: Improved error handling. Still soldiers on, though.
C         get_asca_time: ftgcvd needs a d.p. nulval argument.
C     Dec 12, 1999  Peter Wilson
C          Modify asca_geocen_asca2mjd to take leapseconds into account
C
C NOTES:
C
C
C MAKE:
C
C
C USAGE:
C
C      HOST: htimeconv
C      IRAF: timeconv
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C      data_name - input FITS file and extension number
C      timcolname   - input time column name
C      timeof    - option 1. geocen
C                         2. barycen on TDB
C                         3. barycen on ASCATIME
C      geofile   - database file for barycen
C      ra        - right ascension of target
C      dec       - declination of target
C      frforbit   - frf orbit file
C      extrapolate  - whether to extrapolate orbital elements of the satellite
C                     more than two weeks after
C      verbose   - whether to write information to screen
C      history    - whether to add history record to header
C      status    - FITSIO status
C
C CALLED ROUTINES:
C
C      subroutine time_params - gets parameters from environment.
C      subroutine timeconvert - run main program.
C
C******************************************************************************
C******************************************************************************

      Subroutine timecv
      Implicit none

      Integer status
      character(160) data_name
      character(80) timcolname
      character(160) geofile, frforbit
      character(40) taskname
      Integer timeop
      Double Precision ra, dec
      Logical extrapolate
      Logical verbose
      Logical history

C SET UP MAIN PROGRAM COMMON BLOCKS:

      Include 'timedefs.inc'
      Common /task/ taskname

C INITIALIZE VARIABLES:

      taskname = 'TIMECONV_V1.53'

C START:
C     GET PARAMETERS FROM PARAM FILE:

      Call time_params (data_name, timcolname,timeop, ra, dec,
     &     geofile, frforbit, extrapolate, verbose, history, status)

      If (status .eq. 0) then

         If (verbose) call fcecho (taskname)

C     RUN MAIN PROGRAM:

         Call timeconvert (data_name, timcolname, timeop, ra, dec,
     &        geofile, frforbit, extrapolate, verbose, history, status)

      End if

C     CHECK FOR ERROR:

      If (status .ne. 0) call fcerrm (status)

      End

C******************************************************************************
C******************************************************************************
C SUBROUTINE:
C
C      time_params
C
C DESCRIPTION:
C
C      gets parameters from the parameter file
C
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C       time_params uses F77/VOS like calls to read parameter from .par file
C
C ARGUMENTS:
C
C      datafile  - input FITS file and extension number
C      timcolname   - input time column name
C      timeof    - option 1. geocen
C                         2. barycen on TDB
C                         3. barycen on ASCATIME
C      geofile   - database file for barycen
C      ra        - right ascension of target
C      dec       - declination of target
C      extrapolate  - whether to extrapolate orbital elements of the satellite
C                     more than two weeks after
C      verbose   - whether to write informational messages
C      history    - whether to add history record to header
C      status    - FITSIO status
C
C LOCAL VARIABLES:
C
C      contxt    - error discription string
C
C CALLED ROUTINES:
C
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerr  - write error message to terminal
C
C******************************************************************************

      Subroutine time_params(datafile, timcolname, timeop, ra, dec,
     &         geofile, frforbit, extrapolate, verbose, history, status)
      Implicit none
C Argument:
      Character*(*) datafile, timcolname
      Integer timeop
      Double precision  ra
      Double precision   dec
      Character*(*)  geofile, frforbit
      Logical extrapolate, verbose, history
C Local:
      Real ra4
      Real dec4
      character(80) contxt
      Integer      status
C
C Initialize variables

      status = 0

C Get the name of the input FITS file

      Call uclgst('infile', datafile, status)
      contxt = 'Could not get INFILE (data file) parameter'
      If (status .ne. 0) Goto 999

C Get timeop (time option) geocen or barycen

      Call uclgsi ('timeop', timeop, status)
      contxt = ' Could not get TIMEOP parameter'
      If (status .ne. 0) Goto 999

C Get right ascension of the target

      Call uclgsr ('ra', ra4, status)
      contxt = ' Could not get RA parameter'
      If (status .ne. 0) Goto 999

      ra = ra4

C Get declination of the target

      Call uclgsr ('dec', dec4, status)
      contxt = ' Could not get dec parameter'
      If (status .ne. 0) Goto 999

      dec = dec4

C Get the input TIME column name - hidden parameter

      Call uclgst ('timcolname', timcolname, status)
      contxt = ' Could not get TIMCOLNAME parameter'
      If (status .ne. 0) Goto 999

C Get the geometoryfile for barycentric correction

      Call  uclgst ('geofile', geofile, status)
      contxt = ' Could not get GEOFILE parameter'
      If (status .ne. 0) Goto 999

C Get the frf orbit file

      Call  uclgst ('frforbit', frforbit, status)
      contxt = ' Could not get FRFORBIT parameter'
      If (status .ne. 0) Goto 999

C Get whether to extrapolate orbital elements of the satellite
C more than two weeks after  - hidden paramter

      Call uclgsb ('extrapolate', extrapolate, status)
      contxt = ' Could not get EXTRAPOLATE parameter'
      If (status .ne. 0) Goto 999

C Get whether to write information to screen - hidden paramter

      Call uclgsb ('verbose', verbose, status)
      contxt = ' Could not get VERBOSE parameter'
      If (status .ne. 0) Goto 999

C Get whether to add history record - hidden paramter

      Call uclgsb ('history', history, status)
      contxt = ' Could not get HISTORY parameter'
      If (status .ne. 0) Goto 999

C Handle error

 999  Continue
      If (status .ne. 0) Call fcerr (contxt)

      End

C******************************************************************************
C******************************************************************************
C     SUBROUTINE:
C
C     timeconvert
C
C     DESCRIPTION:
C
C     main program
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C
C     ARGUMENTS:
C
C     data_name - input FITS file and extension number
C     timcolname   - input time column name
C      timeof    - option 1. geocen 2. barycen
C      geofile   - database file for barycen
C      ra        - right ascension of target
C      dec       - declination of target
C     frforbit   - frf orbit file
C     extrapolate - whether to extrapolate orbital elements of the satellite
C                   more than two weeks after
C     verbose   - whether to write informational messages
C     history    - whether to add history record to header
C     status    - FITSIO status
C
C     CALLED ROUTINES:
C
C     subroutine fcecho - echo message to terminal
C     function   fcstln - returns index of last non-blank character
C     subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

      Subroutine timeconvert(data_name, timcolname, timeop, ra, dec,
     &     geofile, frforbit, extrapolate, verbose, history, status)
      Implicit none
C Argument:
      Character*(*) data_name
      Character*(*) timcolname
      Character*(*) geofile, frforbit
      Integer timeop
      Integer status
      Double Precision ra, dec
      Logical extrapolate, verbose, history
C Include:
      Include 'timedefs.inc'
C constant:
      Double Precision MAXEXTRAPOLATE
      Parameter (MAXEXTRAPOLATE = 14.0D0)
C Local:
      Integer data_unit, stat
      Integer n_events
      Integer time_col
      Double Precision mjdobs, mjdref
      Double Precision exposure
      Integer timefram
      data timefram /-1/
      Logical file_open
      character(80) info
      Integer hdutype
      Double Precision epoch, orbelem(15)

C     INITIALIZE VARIABLES:

      stat = 0
      Call ftgiou(data_unit, status)
      file_open = .FALSE.

C Start:

      If (status .ne. 0) Goto 999

C Open data file:

      Write(info, 222)
     &     'reading data file: ', data_name(1:57)
      If (verbose) call fcecho (info)

      Call open_ascadata (data_name, data_unit,
     &     time_col, timcolname, file_open, mjdobs, mjdref,
     &     timefram, n_events, status)

      If (status .ne. 0) Goto 999

      If (timeop.le.timefram) then
         Call fcecho (' Time convert has aleady done ! ')
         status = 1
         Return
      End if

C Time conversion:

C Intialize geocentric correction and check epoch of orbital elements:

      If ((timefram.lt.1).and.(timeop.ge.1)) then
         Call asca_geocen_init(mjdobs, ra, dec, frforbit,
     &        epoch, orbelem, status)
         If (status.ne.0) then
            Call fcerr('Error: Could not set orbital elements')
            Goto 999
         End if
         Write(info,*) ' the data start time (MJD) :', mjdobs
         If (verbose) Call fcecho(info)
         Write(info,*) ' epoch of orbital elements (MJD) :', epoch
         If (verbose) Call fcecho(info)
         If (abs(mjdobs-epoch) .gt. MAXEXTRAPOLATE) then
            Write(info,*) 'Warning: Could not get orbital elements',
     &           ' close enough to the observation date.'
            Call fcerr(info)
            If (.not.extrapolate) Goto 999
         End if
      End if

C Initialize barycentric correction:

      If ((timefram.lt.2).and.(timeop.ge.2)) then
         Call asca_barycen_init(ra, dec, geofile)
      End if

C Convert ASCATIME in EVENTS extension:

      If (verbose) Call fcecho('processing data...')

      status = 0
      info = 'ERROR: Error in converting TIME in EVENTS extension'
      Call move_to_extension(data_unit, 'EVENTS', hdutype, status)
      If (status .eq. 0)
     &    Call asca_timeconv(data_unit,timeop, time_col,
     &        mjdref, n_events, timefram, verbose, status)

      If (status .ne. 0) then
          call fcerr(info)
          call fcerrm(status)
      Endif

C Convert ASCATIME in STDGTI:

      Call fcecho('changing STDGDI ...')

      status = 0
      info = 'ERROR: Error in converting STDGTI'
      Call move_to_extension(data_unit,'STDGTI', hdutype, status)
      If (status .eq. 0)
     &    Call asca_gticonv(data_unit, timefram, timeop,
     &        mjdref, verbose, file_open, exposure, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

C Update keywords in primary header:

      If (verbose) Call fcecho('updating primary header...')

      status = 0
      info = 'Warning: Error in updating primary header'
      Call move_to_extension(data_unit, 'PRIMARY', hdutype, status)
      If (status .eq. 0)
     &    Call update_key(data_unit, timefram, timeop,
     &        mjdref, exposure, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

      Call fcecho('end of update')

C Update keywords in EVENTS extension:

      If (verbose) Call fcecho('updating EVENTS extension header...')

      status = 0
      info = 'Warning: error in updating EVENTS extension header'
      Call move_to_extension(data_unit, 'EVENTS', hdutype, status)
      If (status .eq. 0)
     &    Call update_key (data_unit, timefram, timeop,
     &         mjdref, exposure, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

      Call fcecho('end of update')

C Update keywords in STDGTI extension:

      If (verbose) Call fcecho('updating STDGTI extension header...')

      status = 0
      info = 'Warning: error in updating STDGTI extension header'
      Call move_to_extension(data_unit, 'STDGTI', hdutype, status)
      If (status .eq. 0)
     &    Call update_key (data_unit, timefram, timeop,
     &        mjdref, exposure, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

      Call fcecho('end of update')

C Appending HISTORY keywords:

      If (verbose) Call fcecho('appending HISTORY keywords...')

      status = 0
      info = 'Warning: error in appending HISTORY keywords'
     &    //' to PRIMARY header'
      Call move_to_extension(data_unit, 'PRIMARY', hdutype, status)
      If (status .eq. 0)
     &    Call append_history(data_unit, data_name, timefram,
     &        timeop, ra, dec, geofile, frforbit, epoch, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

      status = 0
      info = 'Warning: error in appending HISTORY keywords'
     &    //' to EVENTS extension header'
      Call move_to_extension(data_unit, 'EVENTS', hdutype, status)
      If (status .eq. 0)
     &    Call append_history(data_unit, data_name, timefram,
     &        timeop, ra, dec, geofile, frforbit, epoch, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

      status = 0
      info = 'Warning: error in appending HISTORY keywords'
     &    //' to STDGTI extension header'
      Call move_to_extension(data_unit, 'STDGTI', hdutype, status)
      If (status .eq. 0)
     &    Call append_history(data_unit, data_name, timefram,
     &        timeop, ra, dec, geofile, frforbit, epoch, status)

      If (status .ne. 0) then
          Call fcerr(info)
          Call fcerrm(status)
      End if

C Close data file:

 999  Continue
      If (file_open) then
         If (verbose) call fcecho('closing data file...')
         Call fcecho('file closed')
         Call ftclos(data_unit, status)
         Call ftfiou(data_unit, status)
      End if

      If (status .ne. 0) then
          Call fcerrm(status)
      Endif

C Format

 222  Format(a23, a57)

      End

C******************************************************************************

      Subroutine open_ascadata (data_name, iunit,
     &     time_col, timcolname, file_open, mjdobs, mjdref,
     &     timefram, n_events, status)
      Implicit none

C Arguments:
      Character*(*) data_name
      Integer iunit
      Integer time_col
      Character*(*) timcolname
      Logical file_open
      Double precision mjdobs, mjdref
      Integer timefram
      Integer n_events
      Integer status
C Include:
      Include 'timedefs.inc'
C Common:
      Common /task/ taskname
C Constant:
      Integer maxcl
      Parameter (maxcl = 512)
C Local:
      Integer stat
      Double precision tscale, tzero
      Integer hdutype, nrecords, xtensn, rwmode, block, rowlen
      Integer vardat, tfield, tbcol(maxcl)
      character(80) filenm, ttype(maxcl), tform(maxcl), tunit(maxcl)
      character(80) extnam, contxt, comment
      character(40) taskname
      Logical exact
      character(80) timefram_s, timesys
      Logical history

C Initialize variables

      tzero  = 0.0
      tscale = 1.0
      exact = .false.
      rwmode = 1

C Get input file name and extension

      Call fcpars (data_name, filenm, xtensn, status)
      contxt = ' Unable to parse data file name and extension '
      If (status .ne. 0) Goto 999

C Default to 1st extension

      If (xtensn .eq. -99) xtensn = 1

C Open data file

      Call ftopen (iunit, filenm, rwmode, block, status)
      contxt = ' Unable to open ASCA data file: '//filenm
      If (status .ne. 0) Goto 999
      file_open = .TRUE.

C Check for reasonable extension number

      contxt = ' Primary array not supported'
      If (xtensn .le. 0) then
         status = 999
         Goto 999
      End if

C Move to the correct extension

C      Call ftmahd (iunit, 2, hdtype, status)
      Call move_to_extension(iunit, 'EVENTS', hdutype, status)
      contxt = ' Error moving to requested extension'
      If (status .ne. 0) Goto 999

C Get header depending on extension type

      If (hdutype .eq. 1) then
         Call ftghtb (iunit, maxcl, rowlen, nrecords, tfield,
     &        ttype, tbcol, tform, tunit, extnam, status)
      Else If (hdutype .eq. 2) then
         Call ftghbn (iunit, maxcl, nrecords, tfield, ttype,
     &        tform, tunit, extnam, vardat, status)
      Else
         contxt = ' file extension type not supported'
         Goto 999
      End if

      contxt = ' error getting extension header info'
      If (status .ne. 0) Goto 999

Ccc
C     reset scaling for table columns (ignore scaling parameters):
c
c        do i=1, tfield
c           call fttscl(iunit, i, tscale, tzero, status)
c        end do
c
c        contxt = ' error reseting TSCALE/TZERO values'
c        if (status .ne. 0) goto 999
Ccc
      n_events = nrecords

C Get extension observation parameters

      Call ftgkyd(iunit, 'MJD-OBS', mjdobs, comment, status)
      contxt = ' cannot get MJD-OBS'
      If (status .ne. 0) Goto 999

      Call ftgkyd(iunit, 'MJDREF', mjdref, comment, status)
      contxt = ' cannot get MJDREF'
      If (status .ne. 0) Goto 999

      Call ftgkys(iunit,'TIMEFRAM', timefram_s,comment,status)
CEAG
      If (status .eq. 202) then
         status = 0
         Call ftgkys(iunit,'TIMEREF', timefram_s,comment,status)
      End if
CEAG
CEAG
C      contxt = ' cannot get TIMEFRAM'
      contxt = ' cannot get TIMEFRAM or TIMEREF'
CEAG
      If (status .ne. 0) Goto 999

      Call ftgkys(iunit,'TIMESYS', timesys, comment, status)
      contxt = ' cannot get TIMESYS from EVENTS extention'
      If (status .ne. 0) Goto 999

      If(timefram_s .eq. 'LOCAL ') then
         timefram = 0
      Else If(timefram_s .eq. 'GEOCENTRIC ') then
         timefram = 1
      Else If(timefram_s .eq. 'SOLARSYSTEM ') then
         If (timesys .eq. 'TDB') then
            timefram = 2
         Else If (timesys .eq. '1993.0') then
            timefram = 3
         Else
            contxt = ' unknown TIMESYS detected'
            Goto 999
         Endif
      Else
         contxt = ' unknown TIMEFRAM detected'
         Goto 999
      Endif

C Check that the TIME columns exist

      Call ftgcno (iunit, exact, timcolname, time_col, status)
      contxt = ' TIME column does not exist'
      If (status .ne. 0) Goto 999

C Return:

      Return

C Handle error

 999  Call fcerr(contxt)
      Write(contxt,*) 'Umm... status = ', status
      Call fcerr(contxt)
      Call ftclos (iunit, stat)
      file_open = .FALSE.

      End

C******************************************************************************
      Subroutine asca_timeconv(iunit, timeop, time_col, mjdref,
     &     n_events, timefram, verbose, status)
      Implicit none
C Argument:
      Integer iunit
      Integer timeop
      Integer time_col
      Double Precision mjdref
      Integer n_events
      Integer timefram
      Logical verbose
      Integer status
C Local:
      Integer counts
      Integer start, event
      Double precision asca_time
      Double precision correct_time
      character(80) string

c      parameter(deg_to_rad = 0.01745329)

C     check if valid file:
c
c        if (.not.(datamode.eq.PH_mode .or. datamode.eq.PH2_mode)) then
c        if (.not.(datamode.eq.PH_mode .or. datamode.eq.MPC_mode)) then
c           call fcerr(' ERROR: non-timing data mode!')
c           status = 999
c           return
c        end if

C Intialize local variables:

      start = 0
      status = 0
      event = 1
      counts = n_events

C Read the first event record:

      Call get_asca_event (iunit, event, asca_time, time_col, status)

C Loop through events:

      Do while (status .eq. 0 .and. event .le. counts)

         Call correct_asca_time(asca_time, timefram, timeop,
     &        mjdref, correct_time, status)
         Call put_asca_event(iunit,event,correct_time,time_col,status)

C Get next event:

         event = event + 1
         if (event .le. counts)  Call get_asca_event(iunit, event,
     &       asca_time, time_col, status)

      End do

C Loop end:

c     tstop = correct_time

      If (verbose) then
         Write(string, 1000) event - 1
 1000    Format (' Number of EVENT records processed = ', I10)
         Call fcecho (string)
      End if

      Return
      End

C******************************************************************************
      Subroutine update_key(data_unit, timefram, timeop,
     &     mjdref, exposure, status)
      Implicit none
C Input:
      Integer  data_unit
      Integer timefram, timeop
      Double Precision mjdref
      Double Precision exposure
C Output:
      Integer status
C Include:
      include 'timedefs.inc'
C Constant:
      Double Precision DAYSEC
      Parameter (DAYSEC=86400.0D0)
C Local:
      Integer decimals
      character(80) comment
      character(40) taskname
      character(80) cmt_tstart, cmt_tstop, cmt_exposure
      character(80) cmt_timefram, cmt_timesys, cmt_mjdobs
      character(8) key_tstart, key_tstop, key_exposure
      character(8) key_timefram, key_timesys, key_mjdobs
      character(80) cval, timefram_s, timesys
      Double Precision tstart, tstop, mjdobs
      Double Precision tstart_original, tstop_original
      Logical ok_tstart, ok_tstop, ok_exposure
      Logical ok_timefram, ok_timesys, ok_mjdobs
c      Logical primary
c      Integer hdutype
C Common:
      Common /task/ taskname

C Initialize local variables:

      decimals = 14

C Chack if it is a primary header or not:
c
c      Call ftmrhd(data_unit, 0, hdutype, status)
c      If (status.eq.0) then
c         If (hdutype.eq.0) then
c            primary = .true.
c         Else
c            primary = .false.
c         End if
c      Else
c         Goto 999
c      End if
c
C Check keywords:

      status = 0
      key_tstart = 'TSTART'
      comment = 'Warning: missing TSTART keywords'
      Call ftgkey(data_unit, key_tstart, cval, comment, status)
      If (status.ne.0) Call fcerr(comment)
      ok_tstart = (status.eq.0)

      status = 0
      key_tstop = 'TSTOP'
      comment = 'Warning: missing TSTOP keywords'
      Call ftgkey(data_unit, key_tstop, cval, comment, status)
      If (status.ne.0) Call fcerr(comment)
      ok_tstop = (status.eq.0)

      status = 0
      key_exposure = 'EXPOSURE'
      comment = 'Warning: missing EXPOSURE or ONTIME keywords'
      Call ftgkey(data_unit, key_exposure, cval, comment, status)
      If (status.eq.202) then
         status = 0
         key_exposure = 'ONTIME'
         Call ftgkey(data_unit, key_exposure, cval, comment, status)
      End if
      If (status.ne.0) Call fcerr(comment)
      ok_exposure = (status.eq.0)

      status = 0
      key_timefram = 'TIMEFRAM'
      comment = 'Warning: missing TIMEFRAM or TIMEREF keywords'
      Call ftgkey(data_unit, key_timefram, cval, comment, status)
      If (status.eq.202) then
         status = 0
         key_timefram = 'TIMEREF'
         Call ftgkey(data_unit, key_timefram, cval, comment, status)
      End if
      If (status.ne.0) Call fcerr(comment)
      ok_timefram = (status.eq.0)

      status = 0
      key_timesys = 'TIMESYS'
      comment = 'Warning: missing TIMESYS keywords'
      Call ftgkey(data_unit, key_timesys, cval, comment, status)
      If (status.ne.0) Call fcerr(comment)
      ok_timesys = (status.eq.0)

      status = 0
      key_mjdobs = 'MJD-OBS'
      comment = 'Warning: missing MJD-OBS keywords'
      Call ftgkey(data_unit, key_mjdobs, cval, comment, status)
      If (status.ne.0) Call fcerr(comment)
      ok_mjdobs = (status.eq.0)

C Get TSTART/TSTOP:

      If (ok_tstart) then
         status = 0
         comment = 'Warning: cannot get TSTART'
         Call ftgkyd(data_unit,'TSTART',tstart_original,comment,status)
         If (status.ne.0) Call fcerr(comment)
         ok_tstart = (status.eq.0)
      End if

      If (ok_tstop) then
         status = 0
         comment = 'Warning: cannot get TSTOP'
         Call ftgkyd(data_unit,'TSTOP',tstop_original,comment,status)
         If (status.ne.0) Call fcerr(comment)
         ok_tstop = (status.eq.0)
      End if

C Convert TSTART/TSTOP and calculate MJD-OBS:

      If (ok_tstart) then
         status = 0
         comment = 'Warning: failed to convert TSTART'
         Call correct_asca_time(tstart_original, timefram, timeop,
     &        mjdref, tstart, status)
         If (status.ne.0) Call fcerr(comment)
         ok_tstart = (status.eq.0)
      End if

      If (ok_tstop) then
         status = 0
         comment = 'Warning: failed to convert TSTOP'
         Call correct_asca_time(tstop_original, timefram, timeop,
     &        mjdref, tstop,status)
         If (status.ne.0) Call fcerr(comment)
         ok_tstop = (status.eq.0)
      End if

      If (ok_mjdobs.and.ok_tstart) then
         status = 0
         comment = 'Warning: failed to calculate MJD-OBS'
         mjdobs = tstart/DAYSEC + mjdref
         If (status.ne.0) Call fcerr(comment)
         ok_mjdobs = (status.eq.0)
      End if

C Set variables depending on timeop:

      If (timeop.eq.1) then

         cmt_tstart = ' data start time on geocenteric time '
         cmt_tstop = ' data end time on geocenteric time '
         cmt_exposure = ' exposure time on geocenteric time '
         cmt_timefram = 'Geocentric correction is applied to times'
         timefram_s = 'GEOCENTRIC'
         cmt_timesys = 'Time measured from 1993 Jan 1 00:00 UT'
         cmt_mjdobs = 'Modified Julian date of the data start time'
         timesys = '1993.0'

      Else If (timeop.eq.2) then

         cmt_tstart = ' data start time on barycenteric time '
         cmt_tstop  = ' data end time on barycenteric time '
         cmt_exposure = ' exposure time on barycenteric time '
         cmt_timefram = 'Barycentric correction is applied to times'
         timefram_s = 'SOLARSYSTEM'
         cmt_timesys = 'Time measured from 1993 Jan 1 00:00 TDB'
         cmt_mjdobs = 'Modified Julian date of the data start time'
         timesys = 'TDB'

      Else if (timeop.eq.3) then

         cmt_tstart = ' data start time on barycenteric asca-time '
         cmt_tstop = ' data end time on barycenteric asca-time '
         cmt_exposure = ' exposure time on barycenteric asca-time '
         cmt_timefram = 'Barycentric correction is applied to times'
         timefram_s = 'SOLARSYSTEM'
         cmt_timesys = 'Time measured from 1993 Jan 1 00:00 UT'
         cmt_mjdobs = 'Modified Julian date of the data start time'
         timesys = '1993.0'
      End if

C Update keyword:

      If (ok_tstart) then
         status = 0
         comment = 'Warning: failed to update TSTART keywords'
         Call ftmkyd(data_unit, key_tstart, tstart, decimals,
     &        cmt_tstart,status)
         If (status.ne.0) Call fcerr(comment)
      End if

      If (ok_tstop) then
         status = 0
         comment = 'Warning: failed to update TSTOP keywords'
         Call ftmkyd(data_unit,key_tstop, tstop, decimals,
     &        cmt_tstop,status)
         If (status.ne.0) Call fcerr(comment)
      End if

      If (ok_exposure) then
         status = 0
         comment = 'Warning: '//
     &        'failed to update EXPOSURE or ONTIME keyword'
         Call ftmkyd(data_unit, key_exposure, exposure, decimals,
     &        cmt_exposure,status)
         If (status.ne.0) Call fcerr(comment)
      End if

      If (ok_timefram) then
         status = 0
         comment = 'Warning: '//
     &        'failed to update TIMEFRAM or TIMEREF keyword'
         Call ftmkys(data_unit, key_timefram, timefram_s,
     &        cmt_timefram, status)
         If (status.ne.0) Call fcerr(comment)
      End if

      If (ok_timesys) then
         status = 0
         comment = 'Warning: failed to update TIMESYS keywords'
         Call ftmkys(data_unit, key_timesys, timesys,
     &        cmt_timesys, status)
         If (status.ne.0) Call fcerr(comment)
      End if

      If (ok_mjdobs.and.ok_tstart) then
         status = 0
         comment = 'Warning: failed to update MJD-OBS keywords'
         Call ftmkyd(data_unit, key_mjdobs, mjdobs, decimals,
     &        cmt_mjdobs,status)
         If (status.ne.0) Call fcerr(comment)
      End if

      Return
      End

C**************************************************************************
      Subroutine asca_gticonv(iunit, timefram, timeop,
     &     mjdref, verbose, file_open, exposure, status)
      Implicit none
C Input:
      Integer iunit
      Integer timefram, timeop
      Double Precision mjdref
      Double Precision ra, dec
      character(160) frforbit
      Logical verbose
C Modify:
      Logical file_open
C Output:
      Double precision exposure
      Integer status
C Local:
      Integer hdutype, rowlen, nrecords, tfield
      Integer maxcl, stat
      Parameter (maxcl = 512)
      Integer tbcol(maxcl)
      character(80) ttype(maxcl), tunit(maxcl), tform(maxcl)
      Integer vardat
      Integer  lastchar, fcstln
      Integer start_col, stop_col
      Integer start, event, n_events, counts
      Double precision start_time, stop_time
      Double precision cstart_time, cstop_time
      Double precision tscale, tzero
      character(80) string, contxt, extnam, extype, comment
      Include 'timedefs.inc'
      Logical exact
      Double precision TDB2ASCA
      Parameter (TDB2ASCA = 59.184)
      Integer decimals

C Initialize local variables:

      tzero = 0.0
      tscale = 1.0
      exact = .false.
      decimals = 14

C Get header depending on extension type

      Call ftgkys(iunit, 'XTENSION', extype, comment, status)
      If (status.ne.0) then
         contxt = ' failed to get a file extension type'
         Goto 998
      Else if (extype.eq.'TABLE') then
         Call ftghtb (iunit, maxcl, rowlen, nrecords, tfield,
     &        ttype, tbcol, tform, tunit, extnam, status)
      Else if (extype.eq.'BINTABLE') then
         Call ftghbn (iunit, maxcl, nrecords, tfield, ttype,
     &        tform, tunit, extnam, vardat, status)
      Else
         contxt = ' file extension type not supported'
         Goto 998
      End if

C
C Reset scaling for table columns (ignore scaling parameters):

      status = 0
      n_events = nrecords

C Check if the START and STOP columns exist

      Call ftgcno (iunit, exact,'START ', start_col, status)
      contxt = ' START column does not exist'
      If (status .ne. 0) Goto 998

      Call ftgcno (iunit, exact,'STOP ', stop_col, status)
      contxt = ' STOP column does not exist'
      If (status .ne. 0) Goto 998

C Intialize local variables:

      start = 0
      status = 0
      event = 1
      counts = n_events
      exposure = 0.0

C Read the first event record:

      Call get_asca_event (iunit, event, start_time, start_col, status)
      Call get_asca_event (iunit, event, stop_time, stop_col, status)

C Loop through events:

      Do while (status .eq. 0 .and. event .le. counts)

C Get an event:

         lastchar = fcstln(frforbit)
         frforbit(lastchar+1:) = char(0)

         Call correct_asca_time(start_time, timefram, timeop,
     &        mjdref, cstart_time, status)
         Call correct_asca_time(stop_time, timefram, timeop,
     &        mjdref, cstop_time, status)

         Call put_asca_event(iunit,event,cstart_time,start_col,status)
         Call put_asca_event(iunit,event,cstop_time,stop_col,status)
         exposure = exposure + (cstop_time - cstart_time)

C Get next event

         event = event + 1
         if (event .le. counts) then
             Call get_asca_event(iunit,event,start_time,start_col,
     &           status)
             Call get_asca_event(iunit,event,stop_time,stop_col,status)
         endif

C Loop end:

      End do

      contxt = ' error occurred in converting STDGTI'
      If (status.ne.0) Goto 998

C Update EXPOSURE:

      If (timeop.eq.1) then
         comment = ' exposure time on geocenteric time '
      Else If (timeop.eq.2) then
         comment = ' exposure time on barycenteric time '
      Else if (timeop.eq.3) then
         comment = ' exposure time on barycenteric asca-time '
      End if

      Call ftmkyd(iunit, 'EXPOSURE', exposure, decimals,
     &     comment ,status)
      If (status.eq.202) then
         status = 0
         Call ftmkyd(iunit, 'ONTIME', exposure, decimals,
     &        comment ,status)
      End if

      contxt = ' error occurred in updating EXPOSURE or ONTIME keywords'
      If (status.ne.0) Goto 998

C Print out messages:

      If (verbose) then
         Write(string, 1000) event - 1
 1000    Format (' Number of GTI records processed = ', I10)
         Call fcecho (string)
      End if
      Return

 998  call fcerr(contxt)
      call  ftclos (iunit, stat)
      file_open = .FALSE.

      End

C******************************************************************************
      Subroutine get_asca_event (iunit, k, asca_time, time_col, status)
      Implicit None
C Argument:
      Integer iunit, k
      double precision asca_time
      Integer time_col
C Include:
       include 'timedefs.inc'
C Local:
      Integer anyf, status
      double precision dval

      Call ftgcvd(iunit, time_col, k, 1, 1, 0.0d0, dval, anyf, status)
      asca_time = dval

      Return
      End

C******************************************************************************
      Subroutine put_asca_event (iunit, k, asca_time, time_col, status)
      Implicit None
C Argument:
      Integer iunit, k
      Double precision asca_time
      Integer time_col
C Include:
      Include 'timedefs.inc'
C Local:
      Integer status
      Double precision dval

      dval = asca_time
      Call ftpcld(iunit, time_col, k, 1, 1, dval, status)

      Return
      End

C******************************************************************************
      Subroutine correct_asca_time (asca_time, timefram, timeop,
     &     mjdref, correct_time, status)
      Implicit None
C Input:
      Double precision asca_time
      Integer timefram, timeop
      Double precision mjdref
C Output:
      Double precision correct_time
      Integer status
C Include:
      Include 'timedefs.inc'
C Constant:
      Double precision TDB2ASCA
      Parameter (TDB2ASCA = 59.184)
      Double precision MJDREFERENCE
      Parameter (MJDREFERENCE = 48988.0)
      Double precision EPS
      Parameter (EPS = 1.0D-10)
C Local:
      Double precision dif
      Double precision geotime, barytime, ascabary
      character(80) info

C Check MJDREF

      dif = abs(mjdref - MJDREFERENCE)
      If (dif.gt.EPS) then
         Write(info,*) 'Warning : Unexpected MJDREF',
     &        mjdref, ' is found.'
         Call fcerr(info)
         Call fcerr('Warning : MJDREF=48988.0 is assumed.')
      End if

C Geocentric correction (asca_time -> geotime):

      If ((timefram.lt.1).and.(timeop.ge.1)) then
         Call asca_geocen(asca_time, geotime)
      Else
         geotime = asca_time
      End if
      correct_time = geotime

C Barycentric correction (geotime -> barytime):

      If ((timefram.lt.2).and.(timeop.ge.2)) then
         Call asca_barycen(geotime, barytime)
      Else
         barytime = geotime
      End if
      correct_time = barytime

C Correction to barycentric ASCATIME (barytime -> ascabary):

      If ((timefram.lt.3).and.(timeop.ge.3)) then
         ascabary = barytime - TDB2ASCA
      Else
         ascabary = barytime
      End if
      correct_time = ascabary

      Return
      End

C******************************************************************************
      Subroutine move_to_extension (iunit, extname, hdutype, status)
      Implicit None
C Input:
      Integer iunit
      Character*(*) extname
C Output:
      Integer status
      Integer hdutype
C Local:
      character(80) ext, comment, contxt

C Initialize local variables:
      status = 0
      Write(contxt,'(A,A8,A)') 'Error moving to ', extname, ' extension'

C Check current extension name:

      Call ftgkys(iunit, 'EXTNAME', ext, comment, status)
      If ((status.eq.0).and.(ext.eq.extname)) Return
      status = 0

C Move to primary header:

      Call ftmahd(iunit, 1, hdutype, status)
      If (status.ne.0) Goto 999
      If ((extname.eq.' ').or.(extname.eq.'PRIMARY')) Return

C Move to specified extension:

      Do while ((status.eq.0).and.(ext.ne.extname))
         Call ftmrhd(iunit, 1, hdutype, status)
         If (status.eq.0) then
            Call ftgkys(iunit, 'EXTNAME', ext, comment, status)
         End if
      End do

      If (status.ne.0) Goto 999

C Return:

      Return

C Handle error:

 999  Call fcerr(contxt)
      Return

      End

C******************************************************************************
      Subroutine append_history(data_unit, data_name,
     &     timefram, timeop, ra, dec, geofile, frforbit, epoch, status)
      Implicit None
C Input:
      Integer data_unit
      Character*(*) data_name
      Integer timefram, timeop
      Double Precision ra, dec
      Character*(*) geofile, frforbit
      Double precision epoch
C Output:
      Integer status
C Task name:
      character(40) taskname
      Common /task/ taskname
      Integer length, fcstln
C Local:
      character(256) info
      character(80) timesys(0:3)
      data timesys
     &     /'ASCATIME at the satellite',
     &      'ASCATIME at the geocenter',
     &      'Barycentric Dynamical Time (TDB)',
     &      'ASCATIME at the solar system barycenter'/

C Task name:

      length = fcstln(taskname)
      Write(info,*) 'TASK ', taskname(:length), ' on ', data_name
      Call ftphis(data_unit, info, status)
      Write(info,*) '   converted from ', timesys(timefram)
      Call ftphis(data_unit, info, status)
      Write(info,*) '   converted to ', timesys(timeop)
      Call ftphis(data_unit, info, status)

C NOT to append in case of barycen -> barycen-ASCA

      If (timefram.lt.2) then
         Write(info,*) '   R.A. in degree (J2000) = ', ra
         Call ftphis(data_unit, info, status)
         Write(info,*) '   Dec. in degree (J2000) = ', dec
         Call ftphis(data_unit, info, status)
      End if

C Geocentric correction:

      If ((timefram.lt.1).and.(timeop.ge.1)) then
         Write(info,*) '   Orbital parameter file = ', frforbit
         Call ftphis(data_unit, info, status)
         Write(info,*) '   Epoch of orbital elements (MJD) = ', epoch
         Call ftphis(data_unit, info, status)
      End if

C Barycentric correction:

      If ((timefram.lt.2).and.(timeop.ge.2)) then
         Write(info,*) '   Earth''s geometry file = ', geofile
         Call ftphis(data_unit, info, status)
      End if

      Return
      End

