*+SISPI
        subroutine SISPI

        integer m_file
        parameter( m_file = 1024 )

*       Description:
*         Fills the PI column of SIS Bright or Bright2 mode science files.
*         This is the main subroutine to be called as an IRAF task or
*         stand-alone task (via SAOhost or XPI), subsequently calling
*         various other subroutines donwstream.
*
*       Author and History:
*         Koji Mukai, ASCA GOF, NASA/GSFC, June 1994
*         Koji Mukai, ASCA GOF, NASA/GSFC, Sep 1994 - minor modifications
*         Koji Mukai, ASCA GOF, NASA/GSFC, Feb 1996
*                     A major rewrite to share subroutines with ascalin
*         Koji Mukai, ASCA GOF, NASA/GSFC, Oct 1996
*                     A minor correction of a keyword, graceful exit on error
*         Jeff Guerber, GSFC 664/HSTX, Feb. 24 1997.
*             In sp_sbopn, exact=.false. so ftgcno calls are case-insensitive.
*         Jeff Guerber, GSFC 664/HSTX, May 16, 1997.  In locate_sgh, changed
*              hardcoded refdata gain-history file to default from parfile.
*         Jeff Guerber, June 2, 1997.  locate_sgh: FGFCAL with "sisdata/<file>"
*              loses on VMS, so use CPTHNM() instead of FGFCAL().
*         Jeff Guerber, Nov 13, 1997. sp_1file: a call to sispi_key() was
*              missing argument `history'
*         Jeff Guerber, 1998-07-22. Y2K fixes: locate_sgh: write dates in
*              new format.  sispi_key: use ftgstm not getdat (and write full
*              date/time string).
*         Jeff Guerber, 1998-08-19. sp_1file: always try to close data_unit,
*              call fcerrm to report any fitsio errors
*         Jeff Guerber, 1998-10-07. locate_sgh: fix bug writing dates and
*              add verbosity if verbose=yes
*         Peter D Wilson, 1999-08-16, replace gtcal with gtcalf
*         Koji Mukai, 2001-03-21.  New version that corresponds to
*              Dotani-san's new formula for CTI correction.
*              Most changes are in the ascalib library (ascalin subdir)
*              but need to read SnCCDMOD from data file so the library
*              routine knows how to read the correct extension of the
*              new gain history file.
*         Koji Mukai - 2001-04-11 - Modified to use update_pi_axis_key.f
*         Bryan Irby - 2001-04-11 - Removed (now-unused) "SISPI_KEY" routine
*              per request of K. Mukai & B. Pence
*         Koji Mukai - 2005-03-25 - Updated routines in ascalib/src/ascalin
*              to be able to handle a Type 3 CTI file.
*              
*-SISPI

        character(160) datafile, calfile, defsfile
        character(40) rawxcol, rawycol, phacol, gradecol
        character(40) timecol, idcol, picol
        real gainnom, offset
        logical launch, verbose, history
        integer n_file, f
        integer status
        character(160) context
        character(160) df_names( m_file )

        character(40) taskname
        common / task / taskname

*       (echo task name and version number)
        taskname = 'SISPI v1.3'
        call FCECHO( taskname )

*       Get necessary parameters
        call SP_PARAM( datafile, calfile, rawxcol, rawycol, idcol,
     &                phacol, gradecol, timecol, picol, gainnom, offset,
     &                   launch, verbose, history, defsfile, status )

        if( status .eq. 0 ) then

*         Parse the datafile
          call FCGCLS( datafile, df_names, n_file, status )

          if( status .eq. 0 ) then
*           Loop through input files
            do f = 1, n_file

              status = 0
              call SP_1FILE( df_names( f ), calfile, rawxcol, rawycol,
     &                       idcol, phacol, gradecol, timecol, picol,
     &                       gainnom, offset, launch, verbose, history,
     &                       defsfile, status )
              if( status .ne. 0 ) then
                context = 'Error processing file ' // df_names( f )
                call FCERR( context )
              end if

*           End of loop through input files
            end do

          else
*           Error in processng input file name
            context = 'Could not process input file name'
            call FCERR( context )
          end if

*       End of if block testing SP_PARAM success
        end if

        end



*+SP_PARAM

        subroutine SP_PARAM( datafile, calfile, rawxcol, rawycol, idcol,
     &                phacol, gradecol, timecol, picol, gainnom, offset,
     &                      launch, verbose, history, defsfile, status )

        implicit none

        character*( * ) datafile, calfile, defsfile
        character*( * ) rawxcol, rawycol, phacol, gradecol, idcol
        character*( * ) timecol, picol
        real gainnom, offset
        logical launch, verbose, history
        integer status

*       Description:
*         Gets parameters from the parameter file
*
*       Arguments:
*         datafile (o) : Science data file name(s)
*         calfile  (o) : SIS gain/CTI history file name
*         rawxcol  (o) : Raw X coordinate column name (hidden)
*         rawycol  (o) : Raw Y coordinate column name (hidden)
*         idcol    (o) : CCDID column name (hidden)
*         phacol   (o) : PHA channel column name (hidden)
*         gradecol (o) : Event grade column name (hidden)
*         timecol  (o) : Event time column name (hidden)
*         picol    (o) : Output PI channel column name (hidden)
*         gainnom  (o) : Nominal gain to use for the PI column (hidden)
*         offset   (o) : Offset in the channel/energy relationship (hidden)
*         launch   (o) : If true, uses gain values at launch (hidden)
*         verbose  (o) : Controls amount of output from this FTOOL (hidden)
*         history  (o) : Controls the history output in FITS header (hidden)
*         defsfile (o) : Default SIS gain history file name
*         status   (o) : Returns a status flag (0 - no error)
*
*       Dependencies:
*         Paramter interface routine (SAOhost/XPI/IRAF???)
*         Terminal I/O routines
*
*       Author and History:
*         Koji Mukai, 1994 June
*         Koji Mukai, 1996 February, added 3 new parameters
*-SP_PARAM

        character(80) context

*       Reset status flag to no error
        status = 0

*       Get the name of the input science FITS file
        call UCLGST( 'datafile', datafile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the input science FITS file name'
          goto 900
        end if

*       Get the name of the calibration (SIS gain/CTI history) file
        call UCLGST( 'calfile', calfile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the calibration file name'
          goto 900
        end if

*       Get the name of the rawx coordinate column
        call UCLGST( 'rawxcol', rawxcol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the RAWX column name'
          goto 900
        end if

*       Get the name of the rawy coordinate column
        call UCLGST( 'rawycol', rawycol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the RAWY column name'
          goto 900
        end if

*       Get the name of the ccdid column
        call UCLGST( 'idcol', idcol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the CCDID column name'
          goto 900
        end if

*       Get the name of the pha column
        call UCLGST( 'phacol', phacol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the PHA column name'
          goto 900
        end if

*       Get the name of the grade column
        call UCLGST( 'gradecol', gradecol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the GRADE column name'
          goto 900
        end if

*       Get the name of the time column
        call UCLGST( 'timecol', timecol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the TIME column name'
          goto 900
        end if

*       Get the name of the pi column
        call UCLGST( 'picol', picol, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the output PI column name'
          goto 900
        end if

*       Get the nominal gain (or negative for default) value
        call UCLGSR( 'gainnom', gainnom, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the nominal gain value'
          goto 900
        end if

*       Get the energy/channel offset value
        call UCLGSR( 'offset', offset, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the offset value'
          goto 900
        end if

*       Get the launch flag value
        call UCLGSB( 'launch', launch, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the launch flag value'
          goto 900
        end if

*       Get the verbose flag value
        call UCLGSB( 'verbose', verbose, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the verbose flag value'
          goto 900
        end if

**       Get the history flag value
*        call UCLGSB( 'history', history, status )
*        if( status .ne. 0 ) then
*          context = 'Failed to get the history flag value'
*          goto 900
*        end if
        history = .true.

*       Get the default SIS gain history file name
        call UCLGST( 'defSISfile', defsfile, status )
        if( status .ne. 0 ) then
          context = 'Failed to get the default gain history file name'
          goto 900
        end if

*       No error - return cleanly
        return

900     continue
*       Error condition occured --- return context
        call FCERR( context )

        end



*+SP_1FILE

        subroutine SP_1FILE( if_name, cal_name, rawxcol, rawycol, idcol,
     &               phacol, gradecol, timecol, picol, gain_nom, offset,
     &                      launch, verbose, history, defsfile, status )

        implicit none

        character*( * ) if_name, cal_name, defsfile
        character*( * ) rawxcol, rawycol, timecol
        character*( * ) idcol, phacol, gradecol
        character*( * ) picol
        real gain_nom, offset
        logical launch, verbose, history
        integer status

*       Description:
*         Called by SISPI and processes a single SIS science file
*
*       Arguments:
*         if_name   (i) : Input (=output) science file name
*         cal_name  (i) : Calibration file name
*         rawxcol   (i) : Column name for RAWX
*         rawycol   (i) : Column name for RAWY
*         idcol     (i) : Column name for CCDID
*         phacol    (i) : Column name for PHA
*         gradecol  (i) : Column name for GRADE
*         timecol   (i) : Column name for TIME
*         picol     (i) : Column name for PI
*         gain_nom  (i) : Nominal gain for the PI channel
*         offset    (i) : Offset in channel/energy relationship
*         launch    (i) : Flag to use gain/CTI values at launch
*         verbose   (i) : Flag for diagnostic output
*         history   (i) : Flag for adding history records in FITS header
*         defsfile  (i) : Default name for SIS gain history file
*         status    (o) : Status flag, 0 if no error
*
*       Dependencies:
*         SP_SBOPN to open SIS BiRIGHT/BRIGHT2 science file
*         SP_CALOP to open SIS gain/CTI history file
*         SP_FILPI to actually fill the PI values
*         Various FITSIO and sundry routines
*
*       Modifications:
*         Added GAIN_NOM keyword in output and more diagnostic
*              outputs: Koji Mukai, 1994 September
*         Major internal rework, 1996 February
*
*       Author:
*         Koji Mukai, 1994 June
*-SP_1FILE

        integer data_unit, cal_unit
        integer n_row
        integer rawx_n, rawy_n, ccdid_n, pha_n, grade_n
        integer time_n, pi_n, chip
        integer clostat
        real g1, gl, o1, ol
        double precision t_strt

        character(160) cal_name2, info, gain_time
        character(40) cal_time
        character(8) keyword

        character(40) taskname
        common / task / taskname

        cal_unit = 2
        data_unit = 1

*       Open data file and get necessary information
        if( verbose ) then
          info = 'Opening data file : ' // if_name
          call FCECHO( info )
        end if
        call SP_SBOPN( data_unit, if_name, rawxcol, rawycol, idcol,
     &                 phacol, gradecol, timecol, picol, verbose, n_row,
     &                 t_strt, rawx_n, rawy_n, ccdid_n, pha_n, grade_n,
     &                 time_n, pi_n, status )
        if( status .ne. 0 ) then
*         Error occured, and already reported
          goto 800
        else if( n_row .eq. 0 ) then
          if( verbose ) then
            info = 'File contained 0 event, ignoring'
            call FCECHO( info )
c            call SISPI_KEY( data_unit, 'NONE', 'NONE', history, status )
            call UPDATE_PI_AXIS_KEY
     &     ( data_unit, 'NONE', 'NONE', 'NONE', 'NONE', launch, status )
          end if
          goto 800
        else
          if( verbose ) then
            info = 'Successfully opened : ' // if_name
            call FCECHO( info )
          end if
        end if

*       Now get the appropriate calibration information
        if( verbose ) then
          info = 'Opening calibration file : ' // cal_name
          call FCECHO( info )
        end if

        call LOCATE_SGH( cal_name, defsfile, t_strt, verbose,
     &                   cal_name2, status )
        if( status .ne. 0 ) goto 800

        call READ_SIS_GAIN_FILE( cal_name2, cal_unit, gain_time,
     &                       gain_nom, offset, launch, status )
        if( status .ne. 0 ) then
*         Some error condition occured in CALOP and was presumably reported
          goto 800
        else
          if( verbose ) then
            info = 'Successfully read : ' // cal_name
            call FCECHO( info )
          end if
        end if

*       SP_FILPI actually goes through the file row by row and
*       figures out the PI value for each
        if( verbose ) then
          info = 'Now processing data file : ' // if_name
          call FCECHO( info )
        end if
        call SP_FILPI( data_unit, n_row, rawx_n, rawy_n, ccdid_n,
     &                 pha_n, grade_n, time_n, pi_n, status )
        if( status .ne. 0 ) then
*         Some error condition occured in FILPI and was presumably reported
          goto 800
        else
          if( verbose ) then
            info = 'Successfully processed : ' // if_name
            call FCECHO( info )
          end if
        end if

c        call SISPI_KEY
c     &              ( data_unit, cal_name2, gain_time, history, status )
        call UPDATE_PI_AXIS_KEY( data_unit, 'NONE', 'NONE',
     &                            cal_name2, gain_time, launch, status )

        if( verbose ) then
          call FCECHO( 'Closing data file ...' )
        end if

*       Close the data file, and if there were Fitsio errors dump them
 800    continue
        clostat = 0
        call FTCLOS( data_unit, clostat )
        call FCERRM( status )

        end



*+SP_SBOPN
        subroutine SP_SBOPN( lun, data_name, rawx_c, rawy_c, ccdid_c,
     &                       pha_c, grade_c, time_c, pi_c, verbose,
     &                       n_row, t_strt, rawx_n, rawy_n, ccdid_n,
     &                       pha_n, grade_n, time_n, pi_n, status )

        implicit none

        integer m_col
        parameter( m_col = 512 )

        integer lun
        character*( * ) data_name
        character*( * ) rawx_c, rawy_c, ccdid_c, pha_c
        character*( * ) grade_c, time_c, pi_c
        logical verbose
        integer n_row
        double precision t_strt
        integer rawx_n, rawy_n, ccdid_n, pha_n, grade_n, time_n, pi_n
        integer status

*       Description:
*         Opens the FITS file, reads the important keywords in the primary
*         header, goes to the right extension and figures out what the
*         column numbers to use.
*
*       Arguments:
*         lun       (i) : Fortran unit number to use for the FITS file
*         data_name (i) : FITS file name (with optional extension)
*         rawx_c    (i) : Name of the RAWX (default) column
*         rawy_c    (i) : Name of the RAWY (default) column
*         ccdid_c   (i) : Name of the CCDID (default) column
*         pha_c     (i) : Name of the PHA (default) column
*         grade_c   (i) : Name of the GRADE (default) column
*         time_c    (i) : Name of the TIME (default) column
*         pi_c      (i) : Name of the PI (default) column
*         verbose   (i) : Flag for diagnostic output
*         n_row     (o) : Number of rows in file
*         t_strt    (o) : Start time of file
*         rawx_n    (o) : Column number for RAWX
*         rawy_n    (o) : Column number for RAWY
*         ccdid_n   (o) : Column number for CCDID
*         pha_n     (o) : Column number for PHA
*         grade_n   (o) : Column number for GRADE
*         time_n    (o) : Column number for TIME
*         pi_n      (o) : Column number for PI
*         status    (o) : Flag, 0 if no errors
*
*       Dependencies:
*         FITSIO routines etc.
*
*       Author:
*         Koji Mukai, 1994 June
*         Updated by KM, 2001 March, to read SnCCDMOD into the
*         asca_common.inc variable, ccd_mode, so that ascalib
*         library routine can read the right extension of the
*         new style CTI file.
*-SP_SBOPN

        double precision t_scale, t_zero
        integer hd_type, ext_num, rw_mode, block, row_len
        integer t_bcol( m_col ), t_field, pcount
        integer t
        character(160) file_name, context, c_value, comment, ext_nam
        character(80) t_type( m_col ), t_form( m_col ), t_unit( m_col )
        character(8) keyword
        logical exact

        include 'asca_defs.inc'
        include 'asca_common.inc'

        t_zero  = 0.0
        t_scale = 1.0
        exact = .false.

*       Decompose data_name into file name and extension number
        call FCPARS( data_name, file_name, ext_num, status )
        if( status .ne. 0 ) then
          context =
     &             'Unable to parse data file name and extension number'
          goto 900
        end if

*       Check extension number for validity
        if( ext_num .eq. -99 ) then
          ext_num = 1
        else if( ext_num .eq. 0 ) then
          context = 'Primary array not supported'
          status = 999
          goto 900
        end if

*       Open the FITS file with rw_mode=1 (read & write)
        rw_mode = 1
        call FTOPEN( lun, file_name, rw_mode, block, status )
        if( status .ne. 0 ) then
          context = 'Unable to open FITS file: ' // file_name
          goto 900
        end if

* Now check a bunch of FITS keywords in the primary header

*       TELESCOP keyword, should be 'ASCA'
        call FTGKYS( lun, 'TELESCOP', c_value, comment, status )
        if( status .ne. 0 ) then
          context = 'Failure reading TELESCOP keyword'
          goto 900
        end if
        if( index( c_value, 'ASCA' ) .eq. 0 ) then
          status = 1001
          context = 'Not an ASCA file'
          goto 900
        end if

*       INSTRUME keyword, only SIS0 and SIS1 are of interest
        call FTGKYS( lun, 'INSTRUME', c_value, comment, status )
        if( status .ne.0 ) then
          context = 'Failure reading INSTRUME keyword'
          goto 900
        end if
        if( index( c_value, 'SIS' ) .eq. 0 ) then
          status = 1002
          context = 'Not a SIS science file'
          goto 900
        else
          dettype = SIS
          if( index( c_value, 'SIS0' ) .gt. 0 ) then
            detector = SIS0
          else if( index( c_value, 'SIS1' ) .gt. 0 ) then
            detector = SIS1
          else
            status = 1003
            context = 'Invalid detector designation'
            goto 900
          end if
        end if

*       Now check the start and stop times
        call FTGKYD( lun, 'TSTART', tstart, comment, status )
        if( status .ne. 0 ) then
          context = 'Failed to get TSTART'
          goto 900
        end if
        t_strt = tstart
        call FTGKYD( lun, 'TSTOP', tstop, comment, status )
        if( status .ne. 0 ) then
          context = 'Failed to get TSTOP'
          goto 900
        end if

*       Now let's check DATAMODE
        call FTGKYS( lun, 'DATAMODE', c_value, comment, status )
        if( status .ne.0 ) then
          context = 'Failure reading DATAMODE keyword'
          goto 900
        end if
        if( index( c_value, 'BRIGHT2' ) .gt. 0 ) then
          datamode = BRIGHT2_mode
          pha_size = 4096
        else if( index( c_value, 'BRIGHT' ) .gt. 0 ) then
          datamode = BRIGHT_mode
          pha_size = 2048
        else
          status = 1004
          context = 'This DATAMODE unsupported by SISPI: ' // c_value
          goto 900
        end if
        det_pi_size = pha_size

*       Need to check SnCCDMOD
        if( detector .eq. SIS0 ) then
          keyword = 'S0CCDMOD'
        else
          keyword = 'S1CCDMOD'
        end if
        call FTGKYJ( lun, keyword, ccd_mode, comment, status )
        if( status .ne. 0 ) then
          context = 'Failure reading SnCCDMOD keyword'
          goto 900
        end if

* Now move to the extension requested
        call FTMRHD( lun, ext_num, hd_type, status )
        if( status .ne. 0 ) then
          context = 'Error moving to requested extension'
          goto 900
        end if
        if( hd_type .eq. 1 ) then
          call FTGHTB( lun, m_col, row_len, n_row, t_field,
     &                 t_type, t_bcol, t_form, t_unit, ext_nam, status )
        else if( hd_type .eq. 2 ) then
          call FTGHBN( lun, m_col, n_row, t_field, t_type,
     &                 t_form, t_unit, ext_nam, pcount, status )
        else
          context = 'File extension type not supported'
          goto 900
        end if
        do t = 1, t_field
          call FTTSCL( lun, t, t_scale, t_zero, status )
        end do
        if( status .ne. 0 ) then
          context = 'Error resetting TSCALE/TZERO values'
          goto 900
        end if

*       Now find out the column numbers of 7 essential columns
        exact = .false.
        call FTGCNO( lun, exact, rawx_c, rawx_n, status )
        if( status .ne. 0 ) then
          context = 'Raw X colum not found: ' // rawx_c
          goto 900
        end if
        call FTGCNO( lun, exact, rawy_c, rawy_n, status )
        if( status .ne. 0 ) then
          context = 'Raw Y colum not found: ' // rawy_c
          goto 900
        end if
        call FTGCNO( lun, exact, ccdid_c, ccdid_n, status )
        if( status .ne. 0 ) then
          context = 'CCD ID colum not found: ' // ccdid_c
          goto 900
        end if
        call FTGCNO( lun, exact, pha_c, pha_n, status )
        if( status .ne. 0 ) then
          context = 'PHA colum not found: ' // pha_c
          goto 900
        end if
        call FTGCNO( lun, exact, grade_c, grade_n, status )
        if( status .ne. 0 ) then
          context = 'Grade colum not found: ' // grade_c
          goto 900
        end if
        call FTGCNO( lun, exact, time_c, time_n, status )
        if( status .ne. 0 ) then
          context = 'Time colum not found: ' // time_c
          goto 900
        end if
        call FTGCNO( lun, exact, pi_c, pi_n, status )
        if( status .ne. 0 ) then
          context = 'PI colum not found: ' // pi_c
          goto 900
        end if
        pi_col = pi_n
        iseed = -1956

*       Initialize the ascalin norm/offset numbers
        gain_renorm = 1.0
        gain_offset = 0.0

*       This is where not-so-serious error conditions should jump to
 800    continue
        return

*       This is where serious error conditions should jump to
 900    continue
        call FCERR( context )
        call FTCLOS( lun, status )

        end



*+LOCATE_SGH
        subroutine LOCATE_SGH( gname_in, defSISfile, t_start, verbose,
     &                         gain_name, status )

        implicit none

        character*( * ) gname_in, defSISfile, gain_name
        double precision t_start
        logical verbose
        integer status

*       Returns the path name of the SIS gain history file
*
*       gname_in   --- (i) SIS gain history file name, or
*                          AUTO, CALDB, FTOOLS, DEFAULT
*       defSISfile --- (i) the "Default" name provided in the par file
*       t_start    --- (i) the start time of the file, for CALDB access
*       gain_name  --- (o) If gname_in is AUTO, CALDB, FTOOLS or DEFAULT,
*                          then decoded name is passed back as gain_name;
*                          otherwise, gname_in is copied into this string.
*       status     --- (o) 0 if no errors
*
*       This requires ASCATOUT (ascalib), GTCALF (callib) and CPTHNM
*       (library/utilities)
*-LOCATE_SGH

        integer year, month, day, hour, min
        integer maxret, ext_no, nret, nfound, chatter
        real sec
        character(70) date_s, time_s
        character(160) online, info

        chatter = 5
        if( verbose ) chatter = 15
        maxret = 1
C
        if( gname_in .eq. 'AUTO'
     &                .or. gname_in .eq. 'auto' ) then
*         Everything in one case, for use in XSELECT
*         Try CALDB first, then FTOOLS (refdata), then DEFAULT.
          call ASCATOUT( t_start, year, month, day, hour, min, sec )
          write( date_s, 101 ) year, month, day
          write( time_s, 102 ) hour, min, int( sec )
          call GTCALF( chatter, 'ASCA', 'SIS0', '-', '-', 'GAIN HIST',
     &                date_s, time_s, date_s, time_s, '-', maxret,
     &                gain_name, ext_no, online, nret, nfound, status )
          if( status .ne. 0 ) then
            info = 'Could not get SIS gain hist from CALDB, ' //
     &            'trying $LHEA_DATA/sisdata'
            if (verbose) call fcerr(info)
            status = 0
            gain_name = defSISfile
            call CPTHNM( 'LHEA_DATA', 'sisdata', gain_name, status )
            if( status .ne. 0 ) then
              info = 'SIS gain history not in $LHEA_DATA/sisdata, ' //
     &              'using default'
              if (verbose) call fcerr(info)
              gain_name = defSISfile
              status = 0
            end if
          end if

        else if( gname_in .eq. 'CALDB'
     &           .or. gname_in .eq. 'caldb' ) then
          call ASCATOUT( t_start, year, month, day, hour, min, sec )
          write( date_s, 101 ) year, month, day
          write( time_s, 102 ) hour, min, int( sec )
          call GTCALF(chatter, 'ASCA', 'SIS0', '-', '-', 'GAIN HIST',
     &                date_s, time_s, date_s, time_s, '-', maxret,
     &                gain_name, ext_no, online, nret, nfound, status )

        else if( gname_in .eq. 'FTOOLS'
     &           .or. gname_in .eq. 'ftools' ) then
            gain_name = defSISfile
            call CPTHNM( 'LHEA_DATA', 'sisdata', gain_name, status )

        else if ( gname_in .eq. 'DEFAULT'
     &            .or. gname_in .eq. 'default') then
          gain_name = defSISfile

        else
          gain_name = gname_in
        end if

        if (status .eq. 0) then
            if (verbose) then
                info = 'SIS gain history: ' // gain_name
                call fcecho(info)
            endif
        else
            info = 'Error locating the SIS gain history file: ' //
     &          gname_in
            call FCERR( info )
        endif

 101    format(i4.4,'-',i2.2,'-',i2.2)
 102    format(i2.2,':',i2.2,':',i2.2)

        end



*+SB_FILPI
        subroutine SP_FILPI( lun, n_row, n_rawx, n_rawy, n_ccdid,
     &                       n_pha, n_grade, n_time, n_pi, status )

        implicit none

        integer lun
        integer n_row
        integer n_rawx, n_rawy, n_ccdid
        integer n_pha, n_grade, n_time, n_pi
        integer status

*       Description:
*         This is the heart of the subroutine: go through the event
*         records in a pre-opened SIS bright/brigh2 data file, figure
*         out the PI value for each event and put it in the (pre-existing)
*         space.  Uses a neat (probably too neat) algorithm to convert
*         from BRIGHT/BRIGHT2 to linear 4096 channel scale and back
*         without a lot of "if" statements.
*
*       Arguments:
*         lun        (i) : Fortran unit number for the FITS file
*         n_row      (i) : Number of rows in this FITS file
*         n_rawx     (i) : Column no for RAWX
*         n_rawy     (i) : Column no for RAWY
*         n_ccdid    (i) : Column no for CCDID
*         n_pha      (i) : Column no for PHA
*         n_grade    (i) : Column no for GRADE
*         n_time     (i) : Column no for TIME
*         n_pi       (i) : Column no for PI
*         status     (i) : Status flag, 0 if no error
*
*       Dependencies:
*         FITSIO routines
*
*       Author:
*         Koji Mukai, 1994 June
*-SB_FILPI

        integer x, y, id, grade, pha_in, pi, null
        double precision event_time, dnull
        logical anyf
        real pha, gain_fact, offset
        integer row
        character(80) info

        integer SIS_PI

*       Now process all rows
        do row = 1, n_row
*         Get the relevant entries from the FITS table
          call FTGCVJ( lun, n_rawx, row, 1, 1, null, x, anyf, status )
          call FTGCVJ( lun, n_rawy, row, 1, 1, null, y, anyf, status )
          call FTGCVJ( lun, n_ccdid, row, 1, 1, null, id, anyf, status )
          call FTGCVJ( lun, n_grade, row, 1, 1,
     &                                       null, grade, anyf, status )
          call FTGCVJ( lun, n_pha, row, 1, 1,
     &                                      null, pha_in, anyf, status )
          call FTGCVD( lun, n_time, row, 1, 1,
     &                                 dnull, event_time, anyf, status )

*         All actual calculations are now done in function SIS_PI
          pi = SIS_PI( pha_in, x, y, id, grade, event_time )

*         Now write it back into the FITS table
          call FTPCLJ( lun, n_pi, row, 1, 1, pi, status )

          if( status .ne. 0 ) goto 800

        end do

        return

 800    continue
        write( info, '(''Failed to process at row '',i10)' ) row
        call FCECHO( info )

        end


*+LENTRIM
        integer function LENTRIM( string )

        character*( * ) string

*       Description:
*         Emulates the Fortran 90 intrinsic function of the same name
*         (length of the string without trailing blanks)
*
*       Arguments:
*         string          (i) : Input string of unknown length
*         <LENTRIM>       (r) : Length without trailing blanks
*
*       Dependencies:
*         None
*
*       Origin:
*         Description of Fortran 90 intrinsic in the Metcalf book
*
*       Author:
*         Koji Mukai,  1992 December, original version
*-LENTRIM

        integer l, lmax, lmax2

        lmax = LEN( string )
        lmax2 = 0
        do l = 1, lmax
          if( string( l: l ) .gt. ' ' ) lmax2 = l
        end do
        LENTRIM = lmax2

        end
