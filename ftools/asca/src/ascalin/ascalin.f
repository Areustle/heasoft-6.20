C*****************************************************************************
C SELECTOR TASK:
C
C     ascaln
C
C FILE:
C
C     ascalin.f
C
C DESCRIPTION:
C
C     Program to transform from ASCA raw telemetry values to physical values.
C     The results are written out to the input science file DETX, DETY
C     (detector coordinates), X, Y (sky coordinates), RTI (Rise Time
C     Invariant), and PI (Pulse Invariant) columns.
C
C     ASCALIN reads an input Science file, a Telescope Definition file,
C     a Temporal History file.
C
C     The Telescope Definition file is a calibration file which defines the
C     coordinate transformations for each instrument.
C
C     For the SIS, the detector DETX and DETY are constucted from the RAWX,
C     RAWY, and CCDID values by placing each chip image in the correct
C     relative position on the focal plane to define a SIS detector system.
C
C     For the GIS, the non-linear spatial and gain distortions are corrected
C     for using the spatial and gain response maps given in the Telescope
C     Definition file.
C
C     The SIS and GIS temporal gain variations are given by the Gain History
C     files. For the SIS this includes CTI corrections.
C
C     The detector coordinates for all four instruments are reconstructed
C     using 'look up' convention. This system is parallel to the S/C
C     coordinate system with the S/C Y-axis flipped.
C
C     the sky X, Y binned R.A. and DEC. coordinates for each event is
C     determined by calculating the linear pixel offsets relative to a
C     sky pointing from the linearized detector DETX, DETY coordinates.
C
C     The Attitude file contains the reconstucted aspect interpolated over
C     a time grid and gives the instantaneous S/C boresight pointing. This
C     infomation, along with the telescope/boresight mis-alignment, is use
C     to transform the detector pixels onto a tangent plane projection in a
C     linear binned R.A. and DEC. pixel image coordinates centered on the
C     SIS0 detector center.
C
C     The sky reference point for the aspecting may be selected from either
C     the science file nominal R.A. and Dec. keywords (KEY), a given input
C     value (USER), or the file average defined as the Attitude file mean
C     R.A. and Dec. during the science file stop and start time (ATT).
C
C     The option is available to perform a 'fixed aspect' using a set Euler
C     angles to calculate the sky X, Y binned R.A. and DEC. coordinates
C     for each event (FIXEDASP).
C
C PROGRAM LOGIC:
C
C     Initialize variables.
C     Open input Science file and read header info.
C     Open Telescope Definition file and read in data.
C     Open Temporal History file and read in data.
C     Open Attitude file.
C     Linearize ASCA coordinates.
C     Update Science file det/sky axis keywords and add history cards.
C     Close Science and Attitude files.
C
C AUTHOR/DATE:
C
C     Eric Gotthelf,    July 1993
C     ASCA GOF, NASA/GSFC
C
C     GIS temporal gain algorithms by Yasushi Ikebe (Univ. of Tokyo)
C
C     Aspecting algorithms by Emily Greene (GSFC), R. Fujimoto (ISAS),
C     and Eric Gotthelf (GSFC).
C
C     SIS PI algorithm from SISPI.
C
C MODIFICATION HISTORY:
C
C     ASCALIN is the re-unification of GISLIN/SISLIN/COORD/FIXEDASP.
C
C     Apr 1996. Added code to allow for a user specified PI gain/offset shift.
C
C     Feb 1997. Added code to correct for radial dependent long-term
C               secular gain drift (see Gain History File, V4+, for
C               details). E.Gotthelf/K.Ebisawa/T.Ishisaki.
C
CCCC?     Mar 1997.  Correct support for rebinned PHA modes. EVG.
C
C     May 1998. Improved error handling for CALDB temporal file access.
C               GIS gain correctly handled for pointing=none, Broken
C               last update.
C     Nov 1998. Jeff Guerber. Use new-format date in datestr.
C     Aug 1999. Peter Wilson. Replace gtcal with gtcalf
C     Mar 2005. Koji Mukai.  Updated ascalib/src/ascalin files
C                       (1) to handle Type 3 SIS CTI files; and
C                       (2) to include the "Gotthelf Correction"
C
C NOTES:
C
C     This is a post-flight version.
C
C     This version of ASCALIN is compatible with FRFREAD version 2.995
C     and higher.
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C      data_name - input FITS file and extension number
C      cal_name  - input calibration filename and extension number
C      rawxcol   - raw electronic coordinate input X column name
C      rawycol   - raw electronic coordinate input Y column name
C      phacol    - raw electronic input PHA column name
C      timecol   - input time column name
C      detxcol   - physical coordinate output X column name
C      detycol   - physical coordinate output Y column name
C      picol     - physical coordinate output PI column name
C      histry    - whether to add history record to header
C      verbose   - whether to write information to screen
C      status    - FITSIO status
C
C CALLED ROUTINES:
C
C      subroutine ascalin_params - gets parameters from environment.
C      subroutine ascalin     - run main program.
C
C******************************************************************************

        subroutine ASCALN

        implicit none

        integer status

        character(160) data_name, cal_name, asp_name, gain_name
        character(160) defSISfile, defGISfile, defS0file, defS1file
        character(160) defS2Ffile, defS3Ffile, defS2Pfile
        character(160) defS3Pfile, defATTpath, outfile
        character(40) timecol, phacol, picol, skyxcol, skyycol
        character(40) rawxcol, rawycol, chipcol, detxcol, detycol
        character(40) qcol, atimecol, pointing
        character(40) taskname

        real ranom_euler_phi, decnom_euler_theta, euler_psi
        real gainrenorm, gainoffset
        logical histry, verbose

C     SET UP MAIN PROGRAM COMMON BLOCKS:

        include 'asca_defs.inc'
        include 'asca_common.inc'

        common /task/ taskname

C     INITIALIZE VARIABLES:

        taskname = 'ASCALIN_V1.0'

C     START:

C     ECHO TASK NAME AND VERSION NUMBER:

        call fcecho (taskname)

C     GET PARAMETERS FROM PARAM FILE:

        call ascalin_params (data_name, cal_name, asp_name, gain_name,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile, defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, outfile,
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol,
     &       skyxcol, skyycol, phacol, picol, qcol, atimecol, pointing,
     &       ranom_euler_phi, decnom_euler_theta, euler_psi,
     &       gainrenorm, gainoffset, histry, verbose, status)

        if (status .eq. 0) then

C     RUN MAIN PROGRAM:

           call ascalinear(data_name, cal_name, asp_name, gain_name,
     &          defS0file, defS1file, defS2Ffile, defS3Ffile,
     &          defATTpath, defS2Pfile, defS3Pfile, defSISfile,
     &          defGISfile, outfile, timecol, rawxcol, rawycol, chipcol,
     &          detxcol, detycol, skyxcol, skyycol, phacol, picol,
     &          qcol, atimecol, pointing, ranom_euler_phi,
     &          decnom_euler_theta, euler_psi, gainrenorm, gainoffset,
     &          histry, verbose, status)

        end if

C     CHECK FOR ERROR:

        if (status .ne. 0) then
           if (status .gt. 99) call  fcerrm(status)
           call  fcerr('Program terminated prematurely')
        end if

        end

C******************************************************************************
C SUBROUTINE:
C
C      ascalin_params
C
C DESCRIPTION:
C
C      gets parameters from the parameter file
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,    Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C       ascalin_params uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C
C      call ascalin_params(infile,aspect, rawxcol, rawycol, phacol
C               timcol, detxcol, detycol, ascalincol, histry, verbose, status)
C
C ARGUMENTS:
C
C      datafile  - input FITS file and extension number
C      calfile   - input calibration filename and extension number
C      rawxcol   - raw electronic coordinate input X column name
C      rawycol   - raw electronic coordinate input Y column name
C      rawycol   - raw electronic coordinate input Y column name
C      phacol    - raw electronic input PHA column name
C      timecol   - input time column name
C      detxcol   - physical coordinate output X column name
C      detycol   - physical coordinate output Y column name
C      picol     - physical coordinate output PI column name
C      histry    - whether to add history record to header
C      verbose   - whether to write informational messages
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
C      subroutine fcerrm - write error message to terminal
C
C******************************************************************************

        subroutine  ascalin_params (datafile,calfile,aspfile,gainfile,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile, defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, outfile,
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol,
     &       skyxcol, skyycol, phacol, picol, qcol, atimecol,
     &       pointing, ranom_euler_phi, decnom_euler_theta,
     &       euler_psi, gainrenorm, gainoffset, histry, verbose, status)

        implicit none

        character*(*) datafile, calfile, gainfile, aspfile, timecol
        character*(*) defS0file, defS1file, defS2Ffile, defS3Ffile
        character*(*) defS2Pfile, defS3Pfile, defSISfile
        character*(*) defGISfile, defATTpath, outfile
        character*(*) detxcol, detycol, rawxcol, rawycol, qcol
        character*(*) chipcol, phacol, picol, skyxcol, skyycol
        character*(*) pointing, atimecol
        real ranom_euler_phi, decnom_euler_theta, euler_psi
        real gainrenorm, gainoffset
        logical histry, verbose
        integer      status

        character(160) contxt

C Initialize variables

        status = 0
        gainrenorm = 1.0
        gainoffset = 0.0

C Get the name of the input FITS file

        call uclgst('datafile', datafile, status)
        contxt = 'Could not get DATAFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input filter CALIBRATION file

        call uclgst('calfile', calfile, status)
        contxt = 'Could not get CALFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input GAIN HISTORY  file

        call uclgst('tempofile', gainfile, status)
        contxt = 'Could not get GAINFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input ATTITUDE file

        call uclgst('attitude', aspfile, status)
        contxt = 'Could not get GAINFILE parameter'
        if (status .ne. 0) go to 999

        if (aspfile .eq. 'euler' .or.
     &       aspfile .eq. 'EULER' .or.
     &       aspfile .eq. 'fixedasp' .or.
     &       aspfile .eq. 'FIXEDASP') then

           pointing = 'euler'

C     Get the Euler phi parameter

           call uclgsr('eulerphi', ranom_euler_phi, status)
           contxt = 'Could not get PHI parameter'
           if (status .ne. 0) go to 999

C     Get the Euler theta parameter

           call uclgsr('eulertheta', decnom_euler_theta, status)
           contxt = 'Could not get THETA parameter'
           if (status .ne. 0) go to 999

C     Get the Euler psi parameter

           call uclgsr('eulerpsi', euler_psi, status)
           contxt = 'Could not get PSI parameter'
           if (status .ne. 0) go to 999

        else

           if ((aspfile .eq. 'none' .or.
     &          aspfile .eq. 'NONE') ) then

              pointing = 'none'

           else

C     Get the value of the pointing parameter

              call uclgst('pointing', pointing, status)
              contxt = 'Could not get POINTING parameter'
              if (status .ne. 0) go to 999

              if (pointing.eq.'user'.or.pointing.eq.'USER') then

C     Get the nominal ra (or Euler phi) , phi parameter

                 call uclgsr('ranom', ranom_euler_phi, status)
                 contxt = 'Could not get RA/PHI parameter'
                 if (status .ne. 0) go to 999

C     Get the nominal dec (or Euler theta) , phi parameter

                 call uclgsr('decnom', decnom_euler_theta, status)
                 contxt = 'Could not get DEC/THETA parameter'
                 if (status .ne. 0) go to 999

              else

                 if (pointing .eq. 'att' .or.
     &                pointing .eq. 'ATT' .or.
     &                pointing .eq. 'mean' .or.
     &                pointing .eq.'MEAN'.or.
     &                pointing .eq. 'key' .or.
     &                pointing .eq. 'KEY') then

                 else
                    status = 1
                    contxt =
     & 'Error parsing pointing parameter, not recognized'
                    go to 999
                 end if

              end if

           end if

        end if

C     Get the nominal gain scale renormalization parameter

        call uclgsr('gainnorm', gainrenorm, status)
        contxt = 'Could not get GAINNORM parameter'
        if (status .ne. 0) go to 999

C     Get the nominal gain scale offset correction parameter

        call uclgsr('gainoff', gainoffset, status)
        contxt = 'Could not get GAINOFF parameter'
        if (status .ne. 0) go to 999

C     Get whether to write information to screen - hidden paramter

        call uclgst ('outfile', outfile, status)
        contxt = ' Could not get OUTFILE parameter'
        if (status .ne. 0) go to 999

C     Get the input ELECTRONIC X intermediate column name - hidden parameter

        call uclgst ('rawxcol', rawxcol, status)
        contxt = ' Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the input ELECTRONIC X intermediate column name - hidden parameter

        call uclgst ('rawycol', rawycol, status)
        contxt = ' Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the input CHIP column name - hidden parameter

        call uclgst ('chipcol', chipcol, status)
        contxt = ' Could not get CHIPCOL parameter'
        if (status .ne. 0) go to 999

C     Get the input TIME column name - hidden parameter

        call uclgst ('timecol', timecol, status)
        contxt = ' Could not get TIMECOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output DETECTOR X column - hidden parameter

        call uclgst ('detxcol', detxcol, status)
        contxt = ' Could not get DETXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput DETECTOR Y column - hidden parameter

        call uclgst ('detycol', detycol, status)
        contxt = ' Could not get DETYCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output SKY X column - hidden parameter

        call uclgst ('skyxcol', skyxcol, status)
        contxt = ' Could not get SKYXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput SKY Y column - hidden parameter

        call uclgst ('skyycol', skyycol, status)
        contxt = ' Could not get SKYYCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput PHABIN column - hidden parameter

        call uclgst ('phacol', phacol, status)
        contxt = ' Could not get PHACOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput PI BIN column - hidden parameter

        call uclgst ('picol', picol, status)
        contxt = ' Could not get PICOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput GRADE column - hidden parameter

c     call uclgst ('gradecol', gradecol, status)
c     contxt = ' Could not get GRADE parameter'
c     if (status .ne. 0) goto 999

C     Get the attitude time column name - hidden parameter

	call uclgst ('atimecol', atimecol, status)
        contxt = ' Could not get ATIMECOL parameter'
        if (status .ne. 0) go to 999

C     Get the attitude q column name - hidden parameter

	call uclgst ('qcol', qcol, status)
        contxt = ' Could not get QCOL parameter'
        if (status .ne. 0) go to 999

C     Get whether to write information to screen - hidden paramter

        call uclgsb ('verbose', verbose, status)
        contxt = ' Could not get VERBOSE parameter'
        if (status .ne. 0) go to 999

C     Get whether to add history record - hidden paramter

        call uclgsb ('history', histry, status)
        contxt = ' Could not get HISTORY parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input SIS TEMPORAL file

        call uclgst('defSISfile', defSISfile, status)
        contxt = 'Could not get DEFSISFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input SIS TEMPORAL file

        call uclgst('defGISfile', defGISfile, status)
        contxt = 'Could not get DEFGISFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input SIS0 CALIBRATION file

        call uclgst('defS0file', defS0file, status)
        contxt = 'Could not get DEFS0FILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input SIS1 CALIBRATION file

        call uclgst('defS1file', defS1file, status)
        contxt = 'Could not get DEFS1FILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input GIS2 FLF CALIBRATION file

        call uclgst('defS2Ffile', defS2Ffile, status)
        contxt = 'Could not get DEFS2FFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input GIS3 FLF CALIBRATION file

        call uclgst('defS3Ffile', defS3Ffile, status)
        contxt = 'Could not get DEFS3FFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input GIS2 POW2 CALIBRATION file

        call uclgst('defS2Pfile', defS2Pfile, status)
        contxt = 'Could not get DEFS2PFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input GIS3 POW2 CALIBRATION file

        call uclgst('defS3Pfile', defS3Pfile, status)
        contxt = 'Could not get DEFS3PFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the default input ATTITUDE file path

        call uclgst('defATTpath', defATTpath, status)
        contxt = 'Could not get defATTpath parameter'
        if (status .ne. 0) go to 999

 999    continue

        if (status .ne. 0) call fcerr (contxt)

        end

C******************************************************************************
C     SUBROUTINE:
C
C     ascalinear
C
C     DESCRIPTION:
C
C     main program
C
C     AUTHOR/DATE:
C
C     Eric Gotthelf,      Jan 1993
C     ASTRO-D GOF, GSFC
C
C     MODIFICATION HISTORY:
C        PDW 08/16/99: Replace gtcal with gtcalf
C
C     NOTES:
C
C     USAGE:
C
C     call ascalinear(data_name, cal_name, detxcol, detycol,
C     rawxcol, rawycol, histry, verbose, status)
C
C     ARGUMENTS:
C
C     data_name - input FITS file and extension number
C     cal_name  - input calibration filename and extension number
C     rawxcol   - raw electronic coordinate input X column name
C     rawycol   - raw electronic coordinate input Y column name
C     timecol   - input time column name
C     detxcol   - physical coordinate output X column name
C     detycol   - physical coordinate output Y column name
C     detycol   - physical Y column name
C     phacol    - raw electronic input PHA column name
C     picol     - physical coordinate output PI column name
C     histry    - whether to add history record to header
C     verbose   - whether to write informational messages
C     status    - FITSIO status
C
C     CALLED ROUTINES:
C
C     subroutine fcecho - echo message to terminal
C     function   fcstln - returns index of last non-blank character
C     subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

        subroutine ascalinear(data_name, cal_name, asp_name, gain_name,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile, defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, outfile,
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol,
     &       skyxcol, skyycol, phacol, picol, qcol, atimecol,
     &       pointing, ra_phi_user, dec_theta_user, psi_user,
     &       gainrenorm, gainoffset, history, verbose, status)

        implicit none

        character*(*) data_name, cal_name, gain_name, asp_name, pointing
        character*(*) defS0file, defS1file, defS2Ffile, defS3Ffile
        character*(*) defS2Pfile, defS3Pfile, defSISfile
        character*(*) defGISfile, defATTpath, outfile
        character*(*) timecol, rawxcol, rawycol, chipcol, atimecol, qcol
        character*(*) detxcol, detycol, skyxcol, skyycol, phacol, picol
        real ra_phi_user, dec_theta_user, psi_user
        real gainrenorm, gainoffset
        logical verbose, history, c_std, t_std, oldfrf
        integer status

C     LOCAL VARIABLES:

        include 'asca_defs.inc'

        integer data_unit, cal_unit, asp_unit, temp_unit, out_unit
        integer stat, calxo, calyo, coordpro, exten, extno, hdtype
        integer maxret, nfound, nret, day, month, year, hour, min
        integer j, fcstln
        real gain_norm, off_norm, sec, rmax
        logical file_open, asp_open, cal_open, fixed_asp, out_open
        logical launch, nobgd, acmflag, do_asp
        character(160) info, cal_time, gain_time, creator
        character(164) ascfile
        character(160) asp_time, data_file, frf_name, online, calname
        character(40) detect, pos_det, datestr, timestr, codename, qstat

        include 'asca_common.inc'

C     INITIALIZE VARIABLES:

*       J initialization added by KM in 2001 November
        j = 0
        rmax = 0.05
        asp_unit  = 4
        cal_unit  = 3
        temp_unit = 2
        data_unit = 1
        out_unit  = 10

        t_std     = .TRUE.
        c_std     = .FALSE.
        nobgd     = .TRUE.
        oldfrf    = .FALSE.
        acmflag   = .FALSE.

        asp_open  = .FALSE.
        cal_open  = .FALSE.
        out_open  = .FALSE.
        file_open = .FALSE.

        do_asp    = .TRUE.
        fixed_asp = .FALSE.
        coordpro  = -1

        ea_phi    = ra_phi_user
        ea_psi    = psi_user
        ea_theta  = dec_theta_user

        gain_renorm = gainrenorm
        gain_offset = gainoffset

        launch = .FALSE.

        qstat = 'SENSOR'

C     START:

        if (status .eq. 0) then

C     OPEN DATA FILE:

           write(info, 222) 'reading data file: ', data_name(1:137)
           if (verbose) then
              call fcecho (' ')
              call fcecho (info)
           end if

           write(data_file, '(a)') data_name

           call open_asca_data (data_name, frf_name, creator,
     &          data_unit, exten, detect, pos_det, timecol, rawxcol,
     &          rawycol, chipcol, detxcol, detycol, skyxcol,
     &          skyycol, phacol, picol, history, status)

C     if (verbose) then
C     write(info, *) telscope, detector, datamode,
C     &          obsmode, bitrate, pos_det, nevents
C     call  fcecho (info)
C     end if

           if (n_events .eq. 0 .and .status .eq. 0) then
              call fcerr('File contains zero events, ignoring')
              status = 1
           end if

           if (status .eq. 0) then

              file_open = .TRUE.

C     OPEN TELESCOPE DEFINITION FILE AND READ IN DATA:

              if (cal_name .eq. 'CALDB' .or.
     &             cal_name .eq. 'caldb') then
                 call ascatout(tstart, year, month, day, hour,min,sec)
                 write(datestr, 333) year, month, day
                 write(timestr, 444) hour, min, int(sec)
                 if (dettype .eq. SIS) then
                    codename = 'ASCALIN'
                 else if (dettype .eq. GIS) then
                    if (pos_meth .eq. FLF) then
                       codename = 'ASCALIN_FLF'
                    else if (pos_meth .eq. POW2) then
                       codename = 'ASCALIN_POW2'
                    else
                       codename = 'ERROR'
                    end if
                 end if
                 maxret = 1
                 call gtcalf(5,'ASCA',detect,'-','-',codename,datestr,
     &                timestr,datestr,timestr,'-',maxret,cal_name,extno,
     &                online,nret,nfound,status)
              else if (cal_name .eq. 'DEFAULT' .or.
     &                cal_name .eq. 'default') then
                 if (detector .eq. SIS0) then
                    cal_name = defS0file
                 else if (detector .eq. SIS1) then
                    cal_name = defS1file
                 else if (detector .eq. GIS2) then
                    if (pos_meth .eq. FLF) then
                       cal_name = defS2Ffile
                    else if (pos_meth .eq. POW2) then
                       cal_name = defS2Pfile
                    end if
                 else if (detector .eq. GIS3) then
                    if (pos_meth .eq. FLF) then
                       cal_name = defS3Ffile
                    else if (pos_meth .eq. POW2) then
                       cal_name = defS3Pfile
                    end if
                 end if
              end if

              if (status .eq. 0) then

                 write(info, 222)
     &                'reading cal  file: ', cal_name(1:137)
                 if (verbose) call fcecho (info)

                 if (dettype .eq. GIS) then
                    call read_gis_cal (cal_name, cal_unit, cal_time,
     &                   calxo, calyo, status)
                 else
                    call read_sis_cal (cal_name, cal_unit, cal_time,
     &                   status)
                 end if

              else

                 call fcerr('Error accessing the CALDB')

              end if

              if (status .eq. 0) then

C     OPEN GAIN HISTORY CALIBRATION FILE AND READ IN DATA:

                 if (gain_name .eq. 'CALDB' .or.
     &                gain_name .eq. 'caldb') then
                    call ascatout(tstart, year, month, day, hour,
     &                   min,sec)
                    write(datestr, 333) year, month, day
                    write(timestr, 444) hour, min, int(sec)
                    if (dettype .eq. SIS) then
                       codename = 'GAIN HIST'
                    else
                       codename = 'GAIN HIST'
                    end if
                    maxret = 1
                    call gtcalf(5,'ASCA',detect,'-','-',codename,
     &                   datestr, timestr, datestr, timestr, '-',
     &                   maxret, gain_name, extno, online, nret,
     &                   nfound, status)
                 else if (gain_name .eq. 'FTOOLS' .or.
     &                   gain_name .eq. 'ftools' ) then
                    if (dettype .eq. SIS) then
                       codename = 'sisph2pi.fits'
                       call fgfcal(gain_name, codename, status)
                    else
                       codename = 'gisph2pi.fits'
                       call fgfcal(gain_name, codename, status)
                    end if
                 else if (gain_name .eq. 'DEFAULT' .or.
     &                   gain_name .eq. 'default') then
                    if (dettype .eq. SIS) then
                       gain_name = defSISfile
                    else if (dettype .eq. GIS) then
                       gain_name = defGISfile
                    end if
                 end if

                 if (status .eq. 0) then

                    write(info, 222)
     &                   'reading gain file: ', gain_name(1:137)
                    if (verbose) call fcecho (info)

                    if (dettype .eq. GIS) then

                       call read_gain_hist (gain_name, temp_unit,
     &                      gain_time, calxo, calyo, status)

                    else

                       gain_norm = -1.0
                       off_norm  =  0.0
                       call read_sis_gain_file (gain_name, temp_unit,
     &                      gain_time, gain_norm, off_norm, launch,
     &                      status)

                    end if

                 else

                    call fcerr('Error accessing the CALDB')

                 end if

                 if (status .eq. 0) then

C     OPEN ATTITUDE FILE:

                    if (pointing .eq. 'none') then
                       do_asp = .FALSE.
                       asp_unit = -1
                       fixed_asp = .FALSE.
                    end if

                    if (pointing .eq. 'EULER' .or. pointing .eq.
     &                   'euler') fixed_asp = .TRUE.

                    if (fixed_asp) then

                       if (verbose) call fcecho(' ')
                       write(asp_name, '(a45)')
     &                   ' FIXED ASPECT PERFORMED USING EULER ANGLES: '
                       write(asp_time, '(28x,3(1x,f9.4))')
     &                      ea_phi, ea_theta, ea_psi
                       write(info, 222) '  ', asp_name(1:137)
                       if (verbose) call fcecho(info)
                       if (verbose) call fcecho(asp_time)

                    else if (do_asp) then

                       if (asp_name .eq. 'DEFAULT' .or.
     &                      asp_name .eq. 'default') then
                          call make_att_name(asp_name, defATTpath,
     &                         frf_name)
                       end if

                       write(info, 222) 'reading att  file: ',
     &                      asp_name(1:137)
                       if (verbose) call fcecho (info)

                       call open_asp_file (asp_name, asp_unit,
     &                      asp_time, atimecol, qcol, qstat, status)

                       if (status .eq. 0) then

                          asp_open = .TRUE.

C     OPEN OUTPUT FILE:

                          if (.not.(outfile .eq. 'none' .or.
     &                         outfile .eq. 'NONE') ) then
*          moved this to outside the following if statement
*          as this may have caused funny outputs.  KM, 2001 Nov
                             j = fcstln(asp_name)
                             if (outfile .eq. 'default' .or.
     &                            outfile .eq. 'DEFAULT') then
                                write(ascfile, '(a,a4)')
     &                               asp_name(1:j),'.asc'
                             else
                                write(ascfile, '(a160)') outfile
                             end if

                             open (unit=out_unit, file = ascfile,
     &                            status='unknown', err=100)
                             out_open = .TRUE.

                             write(out_unit, '(a)')
     &    ' !       QDP file containing aspect info from attitude file:'
                             write(out_unit, '(a6,a)') 'LA OT ',
     &                            asp_name(1:j)

 100                         continue

                             if (out_open) then
                                if (verbose) then
                                   write(info, 222)
     &                                  '   open asc  output file: ',
     &                                  ascfile(1:135)
                                   call fcecho (info)
                                end if
                             else
                                info =
     &                            'Error opening ascii output file. '//
     &                            'Proceed without...'
                                call fcecho (info)
                             end if

                          end if

                          if (verbose) then
                             call fcecho(' ')
                             call fcecho(
     &                     ' AVERAGE ASPECT AND OFFSET FOR DATA FILE:')
                          end if

                          call nominal_aspect (asp_unit, out_unit,
     &                         ra_phi_user, dec_theta_user, rmax,
     &                         pointing, verbose, out_open, acmflag,
     &                         status)

c                       else
c
c                          call fcerr('Error reading attitude file')
c
                       end if

                    end if

                    if (status .eq. 0) then

C     READY TO PROCESS DATA, RESET SUCCESS FLAG:

                       call ftmahd (data_unit, 1, hdtype, status)
                       if (status .ne. 0) then
                          call fcerr
     &                         ('Error moving to primary header')
                       else
                          info = 'INTERRUPT_ERROR'
                          call ftmkys(data_unit, 'COORDPRO', info,
     &                         '&',status)
                          if (status .eq. 202) status = 0
                          call ftmrhd (data_unit, exten, hdtype,
     &                         status)
                       end if

                       if (status .eq. 0) then

                          coordpro = 0

C     LINEARIZE ASCA COORDINATES:

                          if (verbose) call fcecho(' ')

                          if (verbose)
     &                         call fcecho('    processing data...')

                          if (dettype .eq. GIS) then
                             call gis_ele2det (data_unit, asp_unit,
     &                            fixed_asp, nobgd, verbose, status)
                          else
                             call sis_ele2det (data_unit, asp_unit,
     &                            fixed_asp, verbose, status)
                          end if

                          if (status .eq. 0) then

C     ADD HISTORY CARDS AND UPDATE AXIS AND SKY KEYWORDS:

                             if (verbose) call fcecho(' ')

                             if (do_asp) then

                                if (verbose)
     &                               call fcecho
     &                      ('    updating data file DET keywords...')

                                if (index(creator, '2.995') .ne. 0)
     &                               oldfrf = .TRUE.

                                if (dettype .eq. GIS) then
                                   call update_gis_axis_key(data_unit,
     &                                  cal_name, cal_time, gain_name,
     &                                  gain_time, oldfrf, status)
                                else
                                   call update_sis_axis_key(data_unit,
     &                                  cal_name, cal_time, gain_name,
     &                                  gain_time, oldfrf, status)
                                end if

                             end if

                             if (verbose)
     &                            call fcecho
     &                     ('    updating data file PI  keywords...')

                             call update_pi_axis_key (data_unit,
     &                            cal_name, cal_time, gain_name,
     &                            gain_time, launch, status)

                             if (do_asp) then
                                if (verbose)
     &                               call fcecho
     &                     ('    updating data file SKY keywords...')

                                if (do_asp) then
                                   call update_sky_keywords(data_unit,
     &                                  cal_name, cal_time, asp_name,
     &                                  asp_time, fixed_asp, c_std,
     &                                  t_std, oldfrf, status)
                                end if

                             end if

                             if (status .ne. 0) then

                                call fcerr
     &                               ('Error updating data keywords')

                             else

                                coordpro = 1

                             end if

                          else

                             call fcerr('Error processing data')

                          end if

                       else

                          call fcerr
     &                         ('Error resetting COORDPRO keyword')

                       end if

                    else

                       call fcerr('Error reading attitude file')

                    end if

                 else

                    call fcerr('Error reading temporal file')

                 end if

              else

                 call fcerr('Error reading telescope definition file')

              end if

           else

              call fcerr('Error reading data file')

           end if

        end if

C     UPDATE RUN STATUS:

        if (file_open)
     &       call update_coordpro(data_unit, coordpro, status)

C     CLOSE DATA FILE:

        if (asp_open) then
           stat = 0
           if (verbose) call fcecho('    closing attitude file...')
           call  ftclos(asp_unit, stat)
        end if

        if (file_open) then
           stat = 0
           if (verbose) call fcecho('    closing data file...')
           call  ftclos(data_unit, stat)
        end if

 222    format(a23, a137)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)

        end
