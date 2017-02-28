C******************************************************************************
C SELECTOR TASK:
C
C      gisexo
C
C FILE:
C      ascaexpo.f
C
C DESCRIPTION:
C
C     Routine to calculate the net exposure time per sky pixel for a given
C     observation. The total time seen by each sky pixel on the detector is
C     computed using an instrument map and the reconstructed aspect. The
C     obseravtion is uniformly sampled between GTIs (on a time scale
C     consistent with the systematic drift of the spacecraft pointing
C     boresight). [Each sampled interval is corrected for the rate dependent
C     dead-time (whether telemetry or instrument dominated)]. The resultant
C     map depends on the type of input map supplied to this routine:
C
C           INPUT INSTRUMENT MAP              OUTPUT EXPOSURE MAP
C
C        NONE (Uniform detector mask)            Net observation time per pixel
C        Detector mask (0/1's shadow mask)       Geometric exposure time
C        Detector efficiency                     Detector flatfield
C        Detector + mirror efficiency            Observation flatfield
C
C     The output exposure map is created with the same dimensions as the
C     input instument map or may be rebined.
C
C     [To allow for efficent production of multiple energy dependent
C     flatfields the aspect and dead-time for each interval is stored. The
C     routine re-uses the derived data for multiple passes with individual
C     input maps.]
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,    Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C     Oct 94, EVG. First  FTOOLS beta version release.
C     Jan 95, EVG. Second FTOOLS delta release version.
C     Feb 96, EVG. Third  FTOOLS 0.9 release version.
C     Jan 98, EVG. Correctly trapped rebined instrument map size error.
C     Jun 98, EVG. Only prompt for rebin if instmap=none.
C     Jul 98, EVG. Fix hp array overflow for Solaris.
C     Dec 98, NG.  Fix initialization of hp_map array in makesismap and 
C                  makegismap.
C     Aug 99, PDW. Replace gtcal with gtcalf
C     Mar 05, KM.  Added "Gotthelf Correction."
C
C NOTES:
C
C     This program is under development.
C
C     This version is energy independent.
C     Does not include GIS deatime or SIS telemetry saturation.
C
C MAKE:
C
C      HOST: make -f mkhascaexpo
C      IRAF: xc -c xascaexpo.o xascaexpo.f
C            xc -p stsdas xascaexpo.o ascaexpo.o -lmisc -lfitsio -liraf77 -o ascaexpo.x
C
C USAGE:
C
C      HOST: hascaexpo
C      IRAF: gisexp
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C      data_name - input FITS file and extension number
C      cal_name  - input calibration filename and extension number
C      rawxcol   - ccd pixel coordinate input X column name
C      rawycol   - ccd pixel coordinate input Y column name
C      chipcol   - ccd chip number input CHIP column name
C      detxcol   - physical coordinate output X column name
C      detycol   - physical coordinate output Y column name
C      histry    - whether to add history record to header
C      data_unit - FORTRAN I/O unit for opened data file
C      status    - FITSIO status
C
C CALLED ROUTINES:
C
C      subroutine gascaexpo - gets parameters from environment.
C      subroutine ascaexpo  - run main program.
C
C******************************************************************************

        subroutine ascaeo

        implicit none

        integer attstep, rebin, status
        real    binoff

        character(160) data_name, cal_name, asp_name, inst_name
        character(160) expofile, instfile, skyfile, defATTpath, imath
        character(160) qcol, atimecol, qstat, gtiext, hpext, rate_name
        character(160) smcol, aecol, lastcol, satfcol
        character(160) rtcol, rncol, rvcol
        character(40) taskname
        logical histry, verbose

C     SET UP MAIN PROGRAM COMMON BLOCKS:

        include 'asca_defs.inc'
        include 'asca_common.inc'

        common /task/ taskname

C     INITIALIZE VARIABLES:

        taskname = 'ASCAEXPO_V01.0'

C     START:

C     ECHO TASK NAME AND VERSION NUMBER:

        call fcecho (taskname)

C     GET PARAMETERS FROM PARAM FILE:

        call ascaexpo_params (data_name, cal_name, asp_name,rate_name,
     &       inst_name, expofile, instfile, skyfile, defATTpath, imath,
     &       qcol, atimecol, qstat, gtiext, hpext, attstep, rebin,
     &       verbose, rtcol, rncol, rvcol, smcol, aecol, lastcol,
     &       satfcol, binoff, status)

        if (status .eq. 0) then

C     RUN MAIN PROGRAM:

           call  ascaexpomap (data_name, cal_name,asp_name,rate_name,
     &          inst_name, expofile, instfile, skyfile, defATTpath,
     &          imath, qcol, atimecol, qstat, gtiext, hpext, attstep,
     &          rebin, verbose, rtcol, rncol, rvcol, smcol, aecol,
     &          lastcol, satfcol, binoff, status)

        end if

C     CHECK FOR ERROR:

        if (status .ne. 0) then
           if (status .ne. 999) call  fcerrm(status)
           call  fcerr('Program terminated prematurely')
        end if

        end

C******************************************************************************
C SUBROUTINE:
C
C      ascaexpo_params
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

       subroutine ascaexpo_params (datafile, calfile,aspfile,ratefile,
     &       instfile, expofile, imapfile, skyfile, defATTpath, imath,
     &       qcol, atimecol, qstat, gtiext, hpext, attstep, rebin,
     &       verbose, rtcol, rncol, rvcol, smcol, aecol, lastcol,
     &       satfcol, binoff, status)

        implicit none

        character*(*) datafile, calfile, ratefile, aspfile, instfile
        character*(*) hpext, expofile, imapfile, skyfile, defATTpath
        character*(*) imath, qcol, atimecol, qstat, gtiext
        character*(*) smcol, aecol, lastcol, satfcol
        character*(*) rtcol, rncol, rvcol
        logical verbose
        real attreal, binoff
        integer attstep, rebin, status

        character(80) contxt

C Initialize variables

        rebin=-1
        attstep=-1
        status = 0

C Get the name of the input FITS file

        call uclgst('datafile', datafile, status)
        contxt = 'Could not get DATAFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input filter CALIBRATION file

        call uclgst('calfile', calfile, status)
        contxt = 'Could not get CALFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input ATTITUDE file

        call uclgst('attitude', aspfile, status)
        contxt = 'Could not get GAINFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input RATE HISTORY  file

        call uclgst('ratefile', ratefile, status)
        contxt = 'Could not get RATEFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input INSTRUMENT MAP  file

        call uclgst('instfile', instfile, status)
        contxt = 'Could not get INSTFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the input EXPOSURE MAP file

        call uclgst('expofile', expofile, status)
        contxt = 'Could not get EXPOFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output INSTRUMENT MAP file

        call uclgst('imapfile', imapfile, status)
        contxt = 'Could not get IMAPFILE parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output SKY MAP file

        call uclgst('skyfile', skyfile, status)
        contxt = 'Could not get SKYFILE parameter'
        if (status .ne. 0) go to 999

C     Get the attitude record skip factor

        if (.not. (aspfile .eq. 'NONE' .or. aspfile
     &       .eq.  'none') ) then

           call uclgsi ('attstep', attstep, status)
           contxt = ' Could not get ATTSTEP parameter'
           if (status .ne. 0) go to 999

        end if

C     Get the map rebinning factor

        if ( instfile .eq. 'NONE' .or. instfile
     &       .eq.  'none' ) then

           call uclgsi ('rebin', rebin, status)
           contxt = ' Could not get REBIN parameter'
           if (status .ne. 0) go to 999

        end if

C     Get the CRPIX rebinned offset

	call uclgsr ('binoff', binoff, status)
        contxt = ' Could not get BINOFF parameter'
        if (status .ne. 0) go to 999

C     Get the imath operator - hidden parameter

	call uclgst ('imath', imath, status)
        contxt = ' Could not get IMATH parameter'
        if (status .ne. 0) go to 999

C     Get the attitude q column name - hidden parameter

	call uclgst ('atimecol', atimecol, status)
        contxt = ' Could not get ATIMECOL parameter'
        if (status .ne. 0) go to 999

C     Get the attitude q column name - hidden parameter

	call uclgst ('qcol', qcol, status)
        contxt = ' Could not get QCOL parameter'
        if (status .ne. 0) go to 999

C     Get the attitude q column name - hidden parameter

	call uclgst ('qstat', qstat, status)
        contxt = ' Could not get QSTAT parameter'
        if (status .ne. 0) go to 999

C     Get the name of GTI extension for DATAFILE, hidden param

        call uclgst('gtiext', gtiext, status)
        contxt = 'Could not get GTIEXT parameter'
        if (status .ne. 0) go to 999

C     Get the RN column name, hidden param

        call uclgst('rncol', rncol, status)
        contxt = 'Could not get RNCOL parameter'
        if (status .ne. 0) go to 999

C     Get the RV column name, hidden param

        call uclgst('rvcol', rvcol, status)
        contxt = 'Could not get RVCOL parameter'
        if (status .ne. 0) go to 999

C     Get the RT column name, hidden param

        call uclgst('rtcol', rtcol, status)
        contxt = 'Could not get RTCOL parameter'
        if (status .ne. 0) go to 999

C     Get the SATF column name, hidden param

        call uclgst('satfcol', satfcol, status)
        contxt = 'Could not get SATFCOL parameter'
        if (status .ne. 0) go to 999

C     Get the LAST column name, hidden param

        call uclgst('lastcol', lastcol, status)
        contxt = 'Could not get LASTCOL parameter'
        if (status .ne. 0) go to 999

C     Get the SISMODE column name, hidden param

        call uclgst('smcol', smcol, status)
        contxt = 'Could not get SMCOL parameter'
        if (status .ne. 0) go to 999

C     Get the ARENA column name, hidden param

        call uclgst('aecol', aecol, status)
        contxt = 'Could not get AECOL parameter'
        if (status .ne. 0) go to 999

C     Get the HOT PIXEL extention name, hidden param

        call uclgst('hpext', hpext, status)
        contxt = 'Could not get HOT PIXEL extension parameter'
        if (status .ne. 0) go to 999

C     Get the name of default attitude file path

        call uclgst('defattpath', defATTpath, status)
        contxt = 'Could not get DEFATTPATH parameter'
        if (status .ne. 0) go to 999

C     Get whether to write information to screen - hidden parameter

        call uclgsb ('verbose', verbose, status)
        contxt = ' Could not get VERBOSE parameter'
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

        subroutine ascaexpomap(data_name, cal_name, asp_name,
     &       rate_name, inst_name, expo_name, inst_out, sky_name,
     &       defATTpath, imath, qcol, atimecol, qstat, gtiext,
     &       hpext, attstep, rebin, verbose, rtcol, rncol, rvcol,
     &       smcol, aecol, lastcol, satfcol, binoff, status)

        implicit none

        character*(*) data_name, cal_name, rate_name, asp_name
        character*(*) inst_name, expo_name, sky_name, defATTpath
        character*(*) imath, qcol, qstat, atimecol, gtiext, hpext
        character*(*) smcol, aecol, lastcol, satfcol
        character*(*) rtcol, rncol, rvcol
        logical verbose
        integer attstep, status
        real binoff

C     LOCAL VARIABLES:

        include 'asca_defs.inc'

        integer  ngtimax, nhpmax, nexpo, expo_size, sedmax, tsatmax
        parameter( ngtimax = 10000)
        parameter( nhpmax  = 40000)
        parameter( nexpo   = 10000)
        parameter( sedmax   = 10000)
        parameter( tsatmax   = 10000)
        parameter( expo_size = sis_size)

        integer data_unit, cal_unit, asp_unit, rate_unit, inst_unit
        integer expo_unit, sky_unit, stat, idim, jdim, calxo, calyo
        integer ngti, time_unit, i, j, rname_col, rvalue_col, rtime_col
        integer rebin, exten, fcstln, satuh, satuv
        integer year, month, day, hour,min, maxret, nfound, nret, extno
        integer nhp, n_hp,hp_ccd(nhpmax),hp_rawx(nhpmax),hp_rawy(nhpmax)
        integer lasth_col(4), lastv_col(4), arena_col, satf_col(4)
        integer nsed, sed_v(5,sedmax)
        integer ntsat, tsat_h(0:3,tsatmax), tsat_v(0:3,tsatmax)
        real r, sec, expo
        real inst_map(expo_size, expo_size)
        real expo_map(expo_size, expo_size)
        double precision gti_start(ngtimax), gti_stop(ngtimax)
        double precision tsat_t(tsatmax), sed_t(sedmax)
        logical file_open, asp_open, cal_open, fixed_asp, rate_open
        logical history, new_expo_file, time_open, satu, read_inst
        logical doimath
        character(80) info,cal_time,rate_time,asp_time,frf_name
        character(80) codename, datestr, timestr, online, seq
        character(80) object, creator, detect, pos_det, inst_out

        include 'asca_common.inc'

C     INITIALIZE VARIABLES:

        time_unit = 9
        expo_unit = 8
        asp_unit = 7
        sky_unit = 10
        inst_unit = 4
        cal_unit = 3
        rate_unit = 2
        data_unit = 1

        asp_open = .FALSE.
        cal_open = .FALSE.
        file_open = .FALSE.
        rate_open = .FALSE.
        new_expo_file = .TRUE.

c        expo_size = sis_size

        fixed_asp = .FALSE.

C     START:

        if (status .eq. 0) then

C     check for instrument map modes:

           if ((inst_name .eq. 'none' .or.
     &          inst_name .eq. 'NONE')) then
              read_inst = .FALSE.
           else
              read_inst = .TRUE.
           end if

           if ( imath(1:3) .eq. 'mul' .or.
     &          imath(1:3) .eq. 'div' .or.
     &          imath(1:3) .eq. 'add' .or.
     &          imath(1:3) .eq. 'sub' ) then
              doimath = .TRUE.
           else
              doimath = .FALSE.
           end if

C     OPEN DATA FILE:

           write(info, 222) 'reading data file: ', data_name(1:57)
           if (verbose) then
              call fcecho (' ')
              call fcecho (info)
           end if

           call read_asca_data (data_name, frf_name, creator,
     &          object, seq, data_unit, exten, detect, pos_det,
     &          'TIME', 'RAWX', 'RAWY', 'CCDID', 'DETX', 'DETY',
     &          'X', 'Y', 'PHA', 'PI', history, status)

           if (status .eq. 0) then

              file_open = .TRUE.

C     GET GTI INTERVALS:

              write(info, 222)
     &             'reading   gti ext: ', gtiext(1:58)
              if (verbose) call fcecho (info)

              ngti = ngtimax
              call get_gti(data_unit, gtiext, ngti, gti_start,
     &             gti_stop, status)

              expo = 0.0
              do i = 1, ngti
                 expo = expo + real(gti_stop(i)-gti_start(i))
              end do

              if (dettype .eq. SIS .and. status .eq. 0) then

                 if (.not. (hpext .eq. 'none' .or.
     &                hpext .eq. 'NONE') ) then
                    write(info, 222)
     &                   'reading    hp ext: ', hpext(1:58)
                    if (verbose) call fcecho (info)

                    n_hp = nhpmax
                    call get_hp(data_unit, hpext, nhp, n_hp, hp_ccd,
     &                   hp_rawx, hp_rawy, status)
                    if (status .eq. 1) then
                       write(info,*)
     &      'NOTE: Could not read HOT PIXEL extension. ignoring...'
                       call fcecho(info)
                       status = 0
                       n_hp = 0
                    end if
                 else
                    n_hp = 0
                 end if

              end if

              if (status .eq. 0) then

C     OPEN TELESCOPE DEFINITION FILE AND READ IN DATA:

                 if (cal_name .eq. 'CALDB' .or.
     &                cal_name .eq. 'caldb') then
                    call ascatout(tstart, year, month, day, hour,
     &                   min, sec)
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

                    write(info, '(a22)') 'querying  CALDB...'
                    if (verbose) call fcecho (info)

                    maxret = 1
                    call gtcalf(5, 'ASCA', detect, '-', '-', codename,
     &                      datestr, timestr, datestr, timestr, '-',
     &                      maxret, cal_name, extno, online, nret,
     &                      nfound, status)
                 end if

                 if (status .eq. 0) then

                    write(info, 222)
     &                   'reading  cal file: ', cal_name(1:137)
                    if (verbose) call fcecho (info)

                    if (dettype .eq. GIS) then
                       call read_gis_cal(cal_name, cal_unit,
     &                      cal_time, calxo, calyo, status)

C                      For GIS event files with non-standard bit assignment
C                      we may have to rescale some of the info from the teldef
C                      file. This is a cludge added by Ed Pier 1998-10-07
C                      to fix a bug where 64x64 pixel GIS exposure maps
C                      were coming out blank.
                       if(pos_size .ne. det_x_size ) then
                           det_x_center=(det_x_center-.5)
     &                                  *pos_size/det_x_size+.5
                           det_y_center=(det_y_center-.5)
     &                                  *pos_size/det_y_size+.5
            
                           det_x_scale=det_x_scale*det_x_size/pos_size
                           det_y_scale=det_y_scale*det_y_size/pos_size
                       endif

                    else
                       call read_sis_cal (cal_name, cal_unit,
     &                      cal_time, status)
                    end if

                 else

                    call fcerr('Error accessing the CALDB')

                 end if

                 if (status .eq. 0) then

C     DO INSTRUMENT MAP:

                    if (read_inst) then

                       write(info, 222)
     &                      'reading inst file: ', inst_name(1:57)
                       if (verbose) call fcecho (info)

                       call open_inst (inst_name, inst_unit,expo_size,
     &                      idim, jdim, inst_map, status)

                       if (dettype .eq. SIS) then
                          rebin = max(sis_size / idim, 1)
                       else
c                          rebin = max(gis_size / idim, 1)
                          rebin = max(pos_size / idim, 1)
c                          rebin = 1
c                          print*, 'rebin: ', rebin
c                          xyscale = real(gis_size)/real(pos_size)
c                          print*, xyscale
c                          xyscale = real(rebin)/(real(gis_size)/
c     &                         real(pos_size))
c                          print*, xyscale, real(rebin), real(gis_size),
c     &                         real(pos_size)
c                          xyscale = real(rebin)
c                          r = real(rebin)
c                          r = xyscale
c                          print*, det_x_center
c                          det_x_center = det_x_center / r
c                          print*, det_x_center
c                          det_y_center = det_y_center / r
c                          det_x_scale = det_x_scale * r
c                          det_y_scale = det_y_scale * r
c                          fov_x_size = fov_x_size
c                          fov_y_size = fov_y_size
c                          det_x_size = det_x_size / r
c                          det_y_size = det_y_size / r
                          if (idim .ne. pos_size/rebin) then
c                          if (idim .ne. pos_size) then
                             write(info,'(a,i5,i5)')
     &           'ERROR: IMAP SIZE != DATA SIZE: (IMAP,DATA): ',
c     &                            idim/rebin, pos_size
     &                            idim, pos_size
                             call fcerr(info)
                             status = 1
                          end if
                       end if

                    else

                       if (dettype .eq. SIS) then
                          if (rebin .lt. 0) rebin = 4
                          idim = sis_size/rebin
                          jdim = sis_size/rebin
                       else
                          if (rebin .lt. 0) rebin = 1
c                          xyscale = real(rebin)
c                          xyscale = real(gis_size)/real(pos_size)
c                          r = xyscale
c                          xyscale=1.0
c                          print*, rebin, xyscale, r
c                          det_x_center = det_x_center / r
c                          det_y_center = det_y_center / r
c                          det_x_scale = det_x_scale * r
c                          det_y_scale = det_y_scale * r
c                          fov_x_size = fov_x_size
c                          fov_y_size = fov_y_size
c                          det_x_size = det_x_size / r
c                          det_y_size = det_y_size / r
                          idim = pos_size/rebin
                          jdim = pos_size/rebin
                       end if

                    end if

                    if (.not. read_inst .or. doimath .and.
     &                      status .eq. 0) then

                       if (verbose) then
                          write(info, '(a25)')

     &                         '    making an inst map...'
                          call fcecho (' ')
                          call fcecho (info)
                       end if

                       if (dettype .eq. SIS) then

                          satu = .FALSE.

                          call makesismap (n_hp, hp_ccd, hp_rawx,
     &                         hp_rawy, expo_size, idim, jdim,
     &                         expo_map, rebin, rebin, satu,
     &                         ntsat, tsat_h, tsat_v, tsat_t,
     &                         verbose, status)

                       else

                          r = sqrt( abs(fov_x_size/det_x_scale) *
     &                         abs(fov_y_size/det_y_scale)/4.0)

                          call makegismap (r, expo_size, pos_size,
     &                         pos_size, expo_map, rebin, status)

                       end if

                    end if

                    if (read_inst .and. doimath .and.
     &                   status .eq. 0) then

                       if (imath(1:3) .eq. 'mul') then
                          if (verbose) then
                             write(info, '(a25)')
     &                            '    multiply inst map...'
                             call fcecho (info)
                          end if
                          do i = 1, idim
                             do j = 1, jdim
                                inst_map(i,j)=inst_map(i,j) *
     &                               expo_map(i,j)
                             end do
                          end do
                       else if (imath(1:3) .eq. 'div') then
                          if (verbose) then
                             write(info, '(a25)')
     &                            '    divide inst map...'
                             call fcecho (info)
                          end if
                          do i=1,idim
                             do j=1,jdim
                                r = expo_map(i,j)
                                if (abs(r) .gt. 0.0e-12) then
                                   inst_map(i,j)=inst_map(i,j)/r
                                else
                                   inst_map(i,j)=0.0
                                end if
                             end do
                          end do
                       else if (imath(1:3) .eq. 'add') then
                          if (verbose) then
                             write(info, '(a25)')
     &                            '    add inst map...'
                             call fcecho (info)
                          end if
                          do i=1,idim
                             do j=1,jdim
                                inst_map(i,j)=inst_map(i,j) +
     &                               expo_map(i,j)
                             end do
                          end do
                       else if (imath(1:3) .eq. 'sub') then
                          if (verbose) then
                             write(info, '(a25)')
     &                            '    subtract inst map...'
                             call fcecho (info)
                          end if
                          do i=1,idim
                             do j=1,jdim
                                inst_map(i,j)=inst_map(i,j) -
     &                               expo_map(i,j)
                             end do
                          end do
                       end if
                       if (verbose) call fcecho (' ')
                    else
                       if (status .eq. 0) then
                          do i=1,idim
                             do j=1,jdim
                                inst_map(i,j) = expo_map(i,j)
                             end do
                          end do
                       end if
                    end if

                    if (.not.(inst_out .eq. 'none' .or.
     &                   inst_out .eq. 'NONE') .and.
     &                   status .eq. 0) then

                       if ((inst_out .eq. 'default' .or.
     &                      inst_out .eq. 'DEFAULT')) then

                          i = fcstln(data_name)
                          inst_out = data_name(1:i)//'.imap'

                       end if

                       write(info, 222)
     &                      'writing inst file: ',inst_out(1:57)
                       if (verbose) call fcecho (info)

                       call write_inst_map(inst_out, info,
     &                      expo_unit, data_name, object, seq, expo,
     &                      data_unit, expo_size, idim, jdim,
     &                      inst_map, rebin, binoff, status)

                       if (status .ne. 0) status = 0

                    end if

                    if (status .eq. 0) then

C     OPEN ATTITUDE FILE:

                       if (asp_name .eq. 'NONE' .or. asp_name
     &                      .eq.  'none') fixed_asp = .TRUE.

                       if (fixed_asp) then

                          if (ramean .le. 0.0) then
                             write(info, '(a26,a30)')
     &                            'Data file needs to be run ',
     &                            'through lastest ASCALIN first '
                             call fcerr(info)
                             write(info, '(a19,a30)')
     &                            'to use this option, ',
     &                            'or use attitude file instead.'
                             call fcerr(info)
                             status = 1
                          end if

                       else

                          if (asp_name .eq. 'DEFAULT' .or.
     &                         asp_name .eq. 'default') then
                             call make_att_name(asp_name,
     &                            defATTpath, frf_name)
                          end if

                          write(info, 222) 'reading att  file: ',
     &                         asp_name(1:57)
                          if (verbose) call fcecho (info)
                          call open_asp_file (asp_name, asp_unit,
     &                         asp_time, atimecol, qcol, qstat,
     &                         status)

                       end if

                       if (status .eq. 0) then

                          asp_open = .TRUE.

C     OPEN RATE FILE:

                          if (rate_name .eq. 'none' .or.
     &                         rate_name .eq. 'NONE' ) then

                             nsed = -1
                             ntsat = -1
                             rate_open = .FALSE.
                             new_expo_file = .FALSE.

                          else

                             write(info, 222)
     &                            'opening rate file: ',
     &                            rate_name(1:57)
                             if (verbose) call fcecho (info)

                             call open_sis_rate_file (rate_name,
     &                            rate_unit, rtcol, rncol, rvcol,
     &                            smcol, aecol, lastcol, satfcol,
     &                            arena_col, satf_col,lasth_col,
     &                            lastv_col, rtime_col, status)

                             if (status .eq. 0) then
                                nsed = sedmax
                                ntsat = tsatmax
                                rate_open = .TRUE.
                                new_expo_file = .TRUE.
                             end if

                          end if

                          if (status .eq. 0) then

                             if (new_expo_file) then

C     CREATE EXPOSURE FILE:

                                if (verbose) call fcecho(' ')
                                if (verbose) call
     &                               fcecho
     &                 ('    creating exposure time file...')

                                call make_expo_file(rate_unit,
     &                               asp_unit, time_unit, fixed_asp,
     &                               nexpo, ngti, gti_start, gti_stop,
     &                               attstep, ccds, nsed, sed_v, sed_t,
     &                               ntsat, tsat_h, tsat_v, tsat_t,
     &                               lasth_col, lastv_col, arena_col,
     &                               satf_col, rtime_col,
     &                               status)

                             end if

                             if (status .eq. 0) then

                                time_open = .TRUE.

C     MAKE SKY IMAGE:

                                if (.not. (sky_name .eq. 'none' .or.
     &                               sky_name .eq. 'NONE') ) then

                                   if (verbose) then
                                      write(info, '(a25)')
     &                               '    making a sky image...'
                                      call fcecho (' ')
                                      call fcecho (info)
                                   end if

                                   if ((sky_name .eq. 'default' .or.
     &                                  sky_name .eq. 'DEFAULT')) then
                                      i = fcstln(data_name)
                                      sky_name=data_name(1:i)//'.sky'
                                   end if

                                   call make_sky_image(data_unit,
     &                                  rebin, expo_size, idim, jdim,
     &                                  exten+1, ngti, gti_start,
     &                                  gti_stop, expo_map, status)

                                   if (status .eq. 0) then

                                      write(info, 222)
     &                           'writing sky image: ',sky_name(1:57)
                                      if (verbose) call fcecho (info)

                                      call write_sky_image(sky_name,
     &                                     info, sky_unit, data_name,
     &                                     object, seq,expo,data_unit,
     &                                     expo_size, idim, jdim,
     &                                     expo_map, rebin, rebin,
     &                                     binoff, status)

                                   else

                                      write(info,'(a,a,i4)')
     &              'NOTE: Could not make sky image. ignoring...',
     &                                     ' FITSIO ERR# ', status
                                      call fcecho(info)

                                   end if

                                   if (status .ne. 0) status = 0

                                end if

                                if (.not. (expo_name .eq. 'none'
     &                               .or. expo_name .eq.
     &                               'NONE')) then

C     COMPUTE EXPOSURE TIME:

                                   if (verbose) call fcecho(' ')
                                   if (verbose) call
     &                       fcecho('    making an exposure map...')

                                   call exposure_map(time_unit,
     &                                  asp_unit, fixed_asp, verbose,
     &                                  expo_size, idim, jdim,
     &                                  inst_map, expo_map,
     &                                  rebin, rebin, ngti, gti_start,
     &                                  gti_stop, nhp, attstep, nsed,
     &                                  ntsat, tsat_h, tsat_v, tsat_t,
     &                                  status)

                                   if (status .eq. 0) then

C     WRITE OUT EXPOSURE MAP:

                                      if ((expo_name .eq. 'default'
     &                                     .or. expo_name .eq.
     &                                     'DEFAULT')) then

                                         i = fcstln(data_name)
                                         expo_name = data_name(1:i)
     &                                        //'.expo'

                                      end if

                                      if (verbose) then
                                         write(info, 222)
     &                                        'writing expo file: ',
     &                                        expo_name(1:57)
                                         call fcecho (info)
                                      end if

                                      call write_exposure_map(
     &                                     expo_name, info, expo_unit,
     &                                     data_name, object,seq,expo,
     &                                     data_unit, expo_size, idim,
     &                                     jdim, expo_map, rebin,
     &                                     rebin, binoff, status)

                                      if (status .ne. 0) then

                                         call fcerr
     &                              ('Error writing out exposure map')

                                      end if

                                   else

                                      call fcerr
     &                                   ('Error making exposure map')

                                   end if

                                end if

                             else

                               call fcerr('Error opening summary file')

                             end if

                          else


                             call fcerr('Error reading rate file')

                          end if

                       else

                          call fcerr('Error reading attitude file')

                       end if

                    else

                       call fcerr('Error opening inst map file')

                    end if

                 else

                    call fcerr
     &                   ('Error reading telescope definition file')

                 end if

              else

                 call fcerr('Error reading GTIs from data file')

              end if


           else

              call fcerr('Error reading data file')

           end if

        end if

C     CLOSE DATA FILE:

        if (.not. fixed_asp .and. asp_open) then
           stat = 0
           if (verbose) call fcecho('    closing attitude file...')
           call  ftclos(asp_unit, stat)
        end if

        if (rate_open) then
           stat = 0
           if (verbose) call fcecho('    closing rate file...')
           call  ftclos(rate_unit, stat)
        end if

        if (file_open) then
           stat = 0
           if (verbose) call fcecho('    closing data file...')
           call  ftclos(data_unit, stat)
        end if

 222    format(a23, a57)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)

        end

        subroutine open_inst (inst_name, iunit, isize, idim, jdim,
     &       inst_map,  status)

        implicit none

        character*(*) inst_name
        integer isize, idim, jdim, iunit, status
        real inst_map(isize, *)

        integer rwmode, block, stat
        character(80) contxt, info

        rwmode = 0

C     open instrument file

        call ftopen (iunit, inst_name, rwmode, block, status)
        contxt = ' Unable open to instrument file: '//inst_name
        if (status .ne. 0) goto 999

        call read_binext_image (iunit, 0, isize, idim, jdim,
     &       inst_map, status)
        contxt = ' error reading in instrument map'

c        write(info,*) 'Read in inst map size: ',idim,jdim,'(',isize,')'
c        call fcecho (info)

 998    continue

        if(status .ne. 0) call fcerr(contxt)
        stat = 0
        call  ftclos (iunit, stat)

        return

 999    continue
        call fcerr(contxt)

        return

        end

        subroutine write_exposure_map(expo_name, comment, ounit,
     &       data_name, object,sequence,expo,iunit, isize, idim, jdim,
     &       expo_map, xbin, ybin, binoff, status)

        implicit none

        integer ounit, iunit, isize, idim, jdim, xbin, ybin
        integer status
        character*(*) expo_name,data_name,object,comment,sequence
        real expo, binoff, expo_map(isize, *)

        include 'asca_defs.inc'

        integer type, year, month, day, hour, min
        real sec
        character(80) startdate, starttime, stopdate, stoptime
        character(80) hdr(2), istr

        character(40) taskname

        common /task/ taskname

        include 'asca_common.inc'

        nhdr = 1
        type = -32
        equinox = 2000.0
        write(hdr(1),'(a,a34)')
     &       'HISTORY   Exposure map file created by ',
     &       taskname
        if (detector .eq. SIS0) istr = 'SIS0'
        if (detector .eq. SIS1) istr = 'SIS1'
        if (detector .eq. GIS2) istr = 'GIS2'
        if (detector .eq. GIS3) istr = 'GIS3'
        call ascatout(tstart, year, month, day, hour, min, sec)
        write(startdate, 333) year, month, day
        write(starttime, 444) hour, min, int(sec)
        call ascatout(tstop, year, month, day, hour, min, sec)
        write(stopdate, 333) year, month, day
        write(stoptime, 444) hour, min, int(sec)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)

        call write_sky_fits (hdr, nhdr, expo_name, data_name,
     &       isize, idim, jdim, 'ASCA', istr, sequence,
     &       object, startdate, starttime, stopdate, stoptime,
     &       'FK5', 'RA---TAN','DEC--TAN',equinox,cdelt1*real(xbin),
     &       cdelt2*real(ybin), (crpix1-binoff)/real(xbin)+binoff,
     &       (crpix2-binoff)/real(ybin)+binoff,
     &       crval1, crval2, rollavg, expo, type, expo_map, status)

        end

        subroutine write_inst_map(expo_name, comment, ounit,
     &       data_name, object,sequence,expo,iunit,isize, idim, jdim,
     &       expo_map, rebin, binoff, status)

        implicit none

        integer ounit, iunit, isize, idim, jdim, rebin
        integer status
        character*(*) expo_name,data_name,object,comment,sequence
        real binoff, expo, expo_map(isize, *)

        include 'asca_defs.inc'

        integer type, year, month, day, hour, min
        real sec
        character(80) startdate, starttime, stopdate, stoptime
        character(80) hdr(2), istr

        character(40) taskname

        common /task/ taskname

        include 'asca_common.inc'

        nhdr = 1
        type = -32
        equinox = 2000.0
        write(hdr(1),'(a,a34)')
     &       'HISTORY   Instrument map file created by ',
     &       taskname
        if (detector .eq. SIS0) istr = 'SIS0'
        if (detector .eq. SIS1) istr = 'SIS1'
        if (detector .eq. GIS2) istr = 'GIS2'
        if (detector .eq. GIS3) istr = 'GIS3'
        call ascatout(tstart, year, month, day, hour, min, sec)
        write(startdate, 333) year, month, day
        write(starttime, 444) hour, min, int(sec)
        call ascatout(tstop, year, month, day, hour, min, sec)
        write(stopdate, 333) year, month, day
        write(stoptime, 444) hour, min, int(sec)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)

c        print*, tstart, tstop
c        print*, startdate, starttime, stopdate, stoptime

c        print*, nhdr, expo_name, data_name,
c     &       isize, idim, jdim, 'ASCA', istr, sequence

c        print*,
c     &       object, startdate, starttime, stopdate, stoptime,
c     &       'FK5', 'RA---TAN', 'DEC--TAN', equinox, cdelt1*real(rebin)

c        print*,
c     &       cdelt2*real(ybin), (crpix1-binoff)/real(xbin)+binoff,
c     &       (crpix2-binoff)/real(ybin)+binoff,
c     &       crval1, crval2, rollavg, expo, type,status

       call write_sky_fits (hdr, nhdr, expo_name, data_name,
     &       isize, idim, jdim, 'ASCA', istr, sequence,
     &       object, startdate, starttime, stopdate, stoptime,
     &       'FK5', 'RA---TAN', 'DEC--TAN', equinox,
     &       cdelt1*real(rebin), cdelt2*real(rebin),
     &       (crpix1-binoff)/real(rebin)+binoff,
     &       (crpix2-binoff)/real(rebin)+binoff,
     &       raavg, decavg, rollavg, expo, type, expo_map,status)

        end

        subroutine write_sky_image(expo_name, comment, ounit,
     &       data_name, object,sequence,expo,iunit,isize,idim,jdim,
     &       expo_map, xbin, ybin, binoff, status)

        implicit none

        integer ounit, iunit, isize, idim, jdim, xbin, ybin
        integer status
        character*(*) expo_name, data_name,object,comment,sequence
        real binoff, expo, expo_map(isize, *)

        include 'asca_defs.inc'

        integer type, year, month, day, hour, min
        real sec
        character(80) startdate, starttime, stopdate, stoptime
        character(80) hdr(2), istr

        character(40) taskname

        common /task/ taskname

        include 'asca_common.inc'

        nhdr = 1
        type = -32
        equinox = 2000.0
        write(hdr(1),'(a,a34)') 'HISTORY   Sky image file created by ',
     &       taskname
        if (detector .eq. SIS0) istr = 'SIS0'
        if (detector .eq. SIS1) istr = 'SIS1'
        if (detector .eq. GIS2) istr = 'GIS2'
        if (detector .eq. GIS3) istr = 'GIS3'

        call ascatout(tstart, year, month, day, hour, min, sec)
        write(startdate, 333) year, month, day
        write(starttime, 444) hour, min, int(sec)
        call ascatout(tstop, year, month, day, hour, min, sec)
        write(stopdate, 333) year, month, day
        write(stoptime, 444) hour, min, int(sec)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)

        call write_sky_fits (hdr, nhdr, expo_name, data_name,
     &       isize, idim, jdim, 'ASCA', istr, sequence,
     &       object, startdate, starttime, stopdate, stoptime,
     &       'FK5', 'RA---TAN', 'DEC--TAN', equinox, cdelt1*real(xbin),
     &       cdelt2*real(ybin), (crpix1-binoff)/real(xbin)+binoff,
     &       (crpix2-binoff)/real(ybin)+binoff,
     &       crval1, crval2, rollavg, expo, type, expo_map,status)

        end

C******************************************************************************
C SUBROUTINE:
C
C     get_gti
C
C DESCRIPTION:
C
C      find GTI extension and read intervals into array
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C     call get_gti()
C
C ARGUMENTS:
C
C     filename  - input calibration FITS file and extension number
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     iunit       - FORTRAN I/O unit for calibration file
C     matrix_size - calibration map size
C     cal         - common block containing calibration data
C
C     fitsio variables - nullval, iunit, status, bitpix, naxis, naxes(99)
C                        pcount, group, rwmode, blocksize, hdutyp, gcount
C                        simple, extend, anyf
C
C CALLED ROUTINES:
C
C      subroutine parse_CAL_header  - parse header information
C      subroutine read_binext_image - read in a FITS image from a bin xtention
C      subroutine fcecho            - echo message to terminal
C      function fcstln              - returns index of last non-blank character
C      subroutine ftxxxx            - FITSIO calls
C
C******************************************************************************

        subroutine get_gti(lun, ext_name, n_gti, gti_start, gti_stop,
     &       status)

        implicit none

        integer lun, status, n_gti
        character*(*) ext_name
        integer idim, jdim, ioffset, joffset
        double precision gti_start(n_gti), gti_stop(n_gti)

        integer i, k, iext
        integer tfields, nrows, varidat, hdutype, inull
        character(80) errmsg, extname, comment

        logical found

        real nullval
        integer bitpix,naxis,naxes(99),pcount,gcount
        integer group, rwmode, blocksize, stat
        logical simple,extend,anyf
        double precision duration, dnull

        integer maxcl
        parameter (maxcl = 512)

        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam

        iext  = 0
        found = .FALSE.

        if ( status. ne. 0) return

        if (index('PRIMARY',ext_name) .ne. 0) found = .TRUE.


        do while ( status. eq. 0 .and. .not. found)

           stat = 0
           iext = iext + 1
           call ftmahd(lun, iext, hdutype, status)
           call ftgkys(lun, 'EXTNAME', extname, comment, stat)
           if (index(ext_name, extname) .ne. 0) found = .TRUE.

        end do

c     Now we have extension, get the binary table keywords

        if (.not. found) then

           nrows = 0
           status = 1
           write(errmsg, '(a30,a21)')
     &          'NOTE: did not find GTI table: ', ext_name

        else

C     move to the GTI extension

           call ftmahd (lun, iext, hdutype, status)
           if (status .ne. 0) then
              errmsg = 'Error moving to requested extension'
              goto 998
           endif

C     get header depending on extension type

           if (hdutype .eq. 1) then
              call ftghtb (lun, maxcl, rowlen, nrows, tfield, ttype,
     &             tbcol, tform, tunit, extnam, status)
           else if (hdutype .eq. 2) then
              call ftghbn (lun, maxcl, nrows, tfield, ttype, tform,
     &             tunit, extnam, vardat, status)
           endif

           if (status .ne. 0) then
              errmsg = 'Error reading extension header'
              goto 998
           endif

c     loop through GTI intervals:

           do i=1, nrows
              call ftgcvd(lun, 1, i, 1, 1, dnull, gti_start(i),
     &             anyf, status)
              call ftgcvd(lun, 2, i, 1, 1, dnull, gti_stop(i),
     &             anyf, status)
           end do

           n_gti = nrows

           do i=1, n_gti
              duration = duration + gti_stop(i) - gti_start(i)
           end do

           if (status .ne. 0) then
              errmsg = 'Error reading in GTI records'
              goto 998
           endif

           return

        end if

 998    if (status .ne. 0) then
           if (status .ne. 1) call ftclos (lun, status)
           call fcerr(errmsg)
        end if

        return

        end

C******************************************************************************
C        Exposure_map by Eric Gotthelf, Jan 1994.
C        Last revision June 18, 1995.
C******************************************************************************

        subroutine exposure_map(rate_unit, aunit, fixedasp, verbose,
     &       isize, idim, jdim, inst_map, expo_map, xbin, ybin,
     &       ngti, gti_start, gti_stop, nhp, attstep, nsed,
     &       ntsat, tsat_h, tsat_v, tsat_t, status)

        implicit none

        include 'asca_defs.inc'

        integer rate_unit, aunit, isize, idim, jdim, xbin, ybin
        integer ngti, nhp, attstep, nsed, ntsat, status
        real inst_map(isize, *), expo_map(isize, *)
        integer tsat_h(0:3,ntsat), tsat_v(0:3,ntsat)
        double precision gti_start(ngti),gti_stop(ngti),tsat_t(ntsat)

        logical fixedasp, verbose

        integer i, j, k, p, ip, jp, id, jd, pi_bin, pha_bin, counts
        integer event_start, asp_start, event, count, asp, acount
        real x, y, dx, dy, dx2, dy2, sx, sy, tx, ty, fx, fy, rx, ry
        real ax, ay, cx, cy, cx_off, cy_off, ra_nom_rad, dec_nom_rad
        real det_x_offset, det_y_offset, xyoffset, phascale, random
        real x_flip, y_flip, cos_ang, sin_ang, delxsum, delysum
        real roll, angle, cos_roll, sin_roll, theta, phi
        real delx, dely, ra_off, dec_off, rpixtodata, active2
        real ra, dec, delta_ra, delta_dec, sep, astep, newx, newy
        real min_x_size, min_y_size, max_x_size, max_y_size
        real dtmap(sis_size, sis_size), itime
        double precision rasum, decsum, rasum2, decsum2, tscale, tzero
        double precision event_time, asp_time, asp_time1, asp_time2
        double precision q(4), q1(4), q2(4), slope, last_event
        character(80) info
        logical error, nobgd, loop, fixed_step, satu

        integer n, m, kk, rate_first, quality, skycnt, atotal
        integer first_n_bin, first_m_bin, last_n_bin, last_m_bin
        integer first_i_bin, first_j_bin, last_i_bin, last_j_bin
        real total_time, delta_time, total_live, live_time, all_time
        real expo_frac, expo_time, dt_correction, t
        real tstop_plus, tstop_minus, tstart_plus, tstart_minus
        double precision gtitime, t1, t2

        include 'asca_common.inc'

C     intialize local variables:

        p = 0
        asp = 1
        event = 1
        count = 0
        atotal = 0
        acount = 0
        status = 0
        asp_start = 0
        satu = .FALSE.
        error = .FALSE.
        event_start = 0
        delxsum = 0.0
        delysum = 0.0
        ra_nom_rad = crval1 * deg_to_rad
        dec_nom_rad = crval2 * deg_to_rad
*       Additional initialization of astep inserted by KM, 2001 Nov
*          (It was initialized whenver actually used, before I added this)
        astep = 0.0

        active2 = fov_x_size*fov_y_size/det_x_scale/det_y_scale/4.0

        if (attstep .lt. 0) then
           fixed_step = .TRUE.
           attstep = abs(attstep)
        else
           fixed_step = .FALSE.
           astep = real(attstep) / 3600.0 / sqrt(abs(cdelt1*cdelt2))
           newx = 0.0
           newy = 0.0
        end if

        if (dettype .eq. GIS) then
           nobgd = .TRUE.
        else
           nobgd = .FALSE.
        end if

        first_i_bin = 1
        last_i_bin = idim
        first_j_bin = 1
        last_j_bin = jdim

        first_n_bin = 1
        last_n_bin = idim
        first_m_bin = 1
        last_m_bin = jdim

C     start:

        all_time = 0.0
        do k=1, ngti
           all_time = all_time + gti_stop(k) - gti_start(k)
        end do

        if (verbose) then

           call fcecho(' ')
           write(info,'(a, 3(1x,f12.4))')    ' Aspect RA/DEC/ROLL : ',
     &          crval1, crval2, rollavg
           call fcecho(info)
           if (ramean .gt. 0.0) then
              write(info,'(a, 3(1x,f12.4))') ' Mean   RA/DEC/ROLL : ',
     &             ramean, decmean, rollavg
              call fcecho(info)
           end if
           write(info,'(a, 3(1x,f12.4))')    ' Pnt    RA/DEC/ROLL : ',
     &          raavg, decavg, rollavg
           call fcecho(info)
c           write(info,'(a, 2(1x,f12.6))')    ' Degrees per pixel  : ',
c     &          cdelt1, cdelt2
c           call fcecho(info)
           call fcecho(' ')
           write(info,'(a, (1x,i12))')       ' Image rebin factor : ',
     &          xbin
           call fcecho(info)
           write(info,'(a, (1x,i12))')       ' Attitude Records   : ',
     &          nattitude
           call fcecho(info)
           if (dettype .eq. SIS) then
              write(info,'(a, (1x,i12))')    ' Hot Pixels         : ',
     &             nhp
              call fcecho(info)
              if (nsed .gt. -1) then
                 write(info,'(a, (1x,i12))') ' Area Disc Resets   : ',
     &                nsed
                 call fcecho(info)
              end if
              if (ntsat .gt. -1) then
                 write(info,'(a, (1x,i12))') ' Saturated Frames   : ',
     &                ntsat
                 call fcecho(info)
              end if
           end if
           write(info,'(a, (1x,i12))')       ' GTI intervals      : ',
     &          ngti
           call fcecho(info)
           write(info,'(a, (1x,f12.3))')     ' Total GTI (secs)   : ',
     &          all_time
           call fcecho(info)
           call fcecho(' ')

           if (nsed .gt. 0) then
              write(info,'(a)')
     &             ' WARNING: Area Discriminator Reset during GTIs: '
              call fcecho(info)
              write(info,'(a)')
     &    ' WARNING: File may contains several different AD areas'
              call fcecho(info)
              write(info,'(a)')
     &     ' WARNING: Using AD values from science file header...'
              call fcecho(info)
              call fcecho(' ')
           end if

        end if

        if (ntsat .gt. -1) satu = .TRUE.

        call zero_real_array(expo_map, isize*isize)

        if (fixedasp) then

           roll =  rollavg * deg_to_rad

           cos_roll = cos(-roll)
           sin_roll = sin(-roll)

           dx = ( ramean - crval1) * abs(cos(decmean*deg_to_rad))/cdelt1
           dy = (decmean - crval2) / cdelt2

           delx =  dx * cos_roll + dy * sin_roll
           dely = -dx * sin_roll + dy * cos_roll

           if (verbose) then
              write(info,'(a, 2(1x,f12.4))')
     &             ' Mean RA/DEC pixel offset: ',
     &             delx, dely
              call fcecho(info)
           end if

           total_time = 0.0
           total_live = 0.0

           do k = 1, ngti

              if (dettype .eq. SIS) then

                 expo_frac = 1.0
                 delta_time = gti_stop(k) - gti_start(k)
                 total_time = total_time + delta_time
                 total_live = total_live + expo_frac * delta_time

                 itime = 4.0
                 call make_sis_dtmap (isize, idim, jdim, dtmap,
     &                gti_start(k), gti_stop(k), itime, xbin, ybin,
     &                satu, ntsat, tsat_h, tsat_v, tsat_t, verbose,
     &                status)

              else

                 expo_frac = 1.0
                 delta_time = gti_stop(k) - gti_start(k)
                 total_time = total_time + delta_time
                 total_live = total_live + expo_frac * delta_time

              end if

              do i = first_i_bin, last_i_bin
                 do j = first_j_bin, last_j_bin

                    cx_off=real(i*xbin-xbin)-crpix1+
     &                   random(iseed)*real(xbin)
                    cy_off=real(j*ybin-ybin)-crpix2+
     &                   random(iseed)*real(ybin)

                    ra_off =   cx_off*cos_roll +
     &                   cy_off*sin_roll + crpix1
                    dec_off = -cx_off*sin_roll +
     &                   cy_off*cos_roll + crpix2

                    n = int(ra_off  + delx)
                    m = int(dec_off + dely)

                    ax = (real(n-1) - det_x_center)
                    ay = (real(m-1) - det_y_center)

                    if (.not. nobgd .or. ax*ax+ay*ay .le.
     &                   active2) then

c     n = (n-1) / xbin + 1
c     m = (m-1) / ybin + 1

                       n = n / xbin + 1
                       m = m / ybin + 1

                       if (n.ge.first_n_bin .and.
     &                      n.le.last_n_bin) then
                          if (m.ge.first_m_bin .and.
     &                         m.le.last_m_bin) then
                             expo_map(i,j) = expo_map(i,j) +
     &                            expo_frac * delta_time *
     &                            inst_map(n,m) * dtmap(n,m)
                          end if
                       end if

                    end if

                 end do

              end do

           end do

        else

           if (verbose) then
              if (fixed_step) then
                 write(info,'(a, (1x,f12.3))')
     &                ' Attitude records to skip: ', attstep
                 call fcecho(info)
              else
                 write(info,'(a, (1x,f12.3))')
     &                ' Max attitude excursion (arcsecs) : ',
     &                astep * sqrt(abs(cdelt1*cdelt2)) * 3600.0
                 call fcecho(info)
              end if
           end if

           tstop_plus  = tstop + 32.0D0
           tstart_plus  = tstart + 32.0D0
           tstop_minus = tstop - 32.0D0
           tstart_minus = tstart - 32.0D0

           if ( astart .gt. tstart_plus .or. astop .lt.
     &          tstop_minus ) then
              if ( astart .gt. tstart_plus) then
                 info = 'Event start time preceeds attitude file'
              else
                 info='Event end time is not covered by attitude file'
              endif
              call fcerr (info)
              status = 1
              return
           end if

           call asp_record (aunit, asp_start, asp, asp_time2, q,
     &          quality, status)
           do while (asp_time2 .lt. gti_start(1) .and. status .eq. 0)
              asp_time1 = asp_time2
              call asp_record (aunit, asp_start, asp, asp_time2, q,
     &             quality, status)
           end do

           if (status .ne. 0) then
              info = 'Error reading attitude file (1)'
              call fcerr (info)
           end if
           if (asp_start .eq. -1 .and. asp_time2 .gt. tstop_plus) then
              info = 'End of attitude file before data end (1)'
              call fcerr (info)
              status = 1
           endif
           if (asp_time1 .gt. tstop) then
              info = 'Beginning of attitude file after data (1)'
              call fcerr (info)
              status = 1
           end if

           if (status .ne. 0) return

           if (verbose) then
              call fcecho(' ')
              write(info,'(1x,i3,a,1x,f12.2,1x,f12.2)') 0,
     &             ' Percent Complete: Total/live time: ', 0.0,0.0
              call fcecho(info)
           end if

           do i=1, isize
              do j = 1, isize
                 dtmap(i,j) = 1.0
              end do
           end do

           call aberration (asp_time1, ra_nom_rad,
     &          dec_nom_rad, delta_ra, delta_dec, sun_long)

           ra = ra_nom_rad + delta_ra
           dec = dec_nom_rad + delta_dec

C          Added the term for the attitude correction: KM, March 2005
           call rd2sat (ra, dec, q, theta, phi, roll, error)
           delta_ra =
     &          (gdetxoff * cos(roll) + gdetyoff * sin(roll)) / cos(dec)
           delta_dec = gdetxoff * sin(roll) - sdetyoff * cos(roll)
           ra = ra + delta_ra
           dec = dec + delta_dec
                    
           call rd2sat (ra, dec, q, theta, phi, roll, error)

           newx=theta*cos(phi)/cdelt1/deg_to_rad
           newy=theta*sin(phi)/cdelt2/deg_to_rad

           p = 10
           kk = 1
           atotal = 1
           skycnt = 0
           loop = .TRUE.
           total_live = 0.0
           total_time = 0.0
           tstop_plus = tstop + dble(16*abs(attstep))
           delta_time  = 0.0

           do while (status .eq. 0 .and. .not. error .and. loop)

              call aberration (asp_time1, ra_nom_rad,
     &             dec_nom_rad, delta_ra, delta_dec, sun_long)

              ra = ra_nom_rad + delta_ra
              dec = dec_nom_rad + delta_dec

C             Added the term for the attitude correction: KM, March 2005
              call rd2sat (ra, dec, q, theta, phi, roll, error)
              delta_ra =
     &          (gdetxoff * cos(roll) + gdetyoff * sin(roll)) / cos(dec)
              delta_dec = gdetxoff * sin(roll) - sdetyoff * cos(roll)
              ra = ra + delta_ra
              dec = dec + delta_dec
                    
              call rd2sat (ra, dec, q, theta, phi, roll,
     &             error)

              delx=theta*cos(phi)/cdelt1/deg_to_rad
              dely=theta*sin(phi)/cdelt2/deg_to_rad

              delxsum = delxsum+delx
              delysum = delysum+dely

              cos_roll = cos(-roll)
              sin_roll = sin(-roll)

              delta_time  = 0.0
              do k=1, ngti
                 delta_time = delta_time + max(min(asp_time2,
     &                gti_stop(k)) - max(asp_time1, gti_start(k)),
     &                0.0d0)
              end do

c     if there is time during this step, then add it to the expo map:

              if (delta_time .gt. 0.0) then

                 if (dettype .eq. SIS) then
c
c     SIS: compute possible telemetry saturation for this time interval
c
                    if (satu) then
                       itime = 4.0
                       call make_sis_dtmap (isize, idim, jdim,
     &                      dtmap, asp_time1, asp_time2, itime,
     &                      xbin, ybin, satu, ntsat, tsat_h, tsat_v,
     &                      tsat_t, verbose, status)
                    end if

                 else
c
c     GIS: compute total deadtime factor between for this time interval
c
                 end if

                 expo_frac  = 1.0
                 total_time = total_time + delta_time
                 total_live = total_live + expo_frac * delta_time

                 do i = first_i_bin, last_i_bin
                    do j = first_j_bin, last_j_bin

                       cx_off=real(i*xbin-xbin)-crpix1+
     &                      random(iseed)*real(xbin)
                       cy_off=real(j*ybin-ybin)-crpix2+
     &                      random(iseed)*real(ybin)

                       ra_off =   cx_off*cos_roll +
     &                      cy_off*sin_roll + crpix1
                       dec_off = -cx_off*sin_roll +
     &                      cy_off*cos_roll + crpix2

                       n = int(ra_off  + delx)
                       m = int(dec_off + dely)

                       ax = (real(n-1) - det_x_center)
                       ay = (real(m-1) - det_y_center)

                       if (.not. nobgd .or. ax*ax+ay*ay .le.
     &                      active2) then

                          n = n / xbin + 1
                          m = m / ybin + 1

c                         n = (n-1) / xbin + 1
c                         m = (m-1) / ybin + 1

                          if (n.ge.first_n_bin .and.
     &                         n.le.last_n_bin) then
                             if (m.ge.first_m_bin .and.
     &                            m.le.last_m_bin) then
                                if (satu) then
                                   expo_map(i,j) = expo_map(i,j) +
     &                                  expo_frac * delta_time *
     &                                  inst_map(n,m) * dtmap(n,m)
                                else
                                   expo_map(i,j) = expo_map(i,j) +
     &                                  expo_frac * delta_time *
     &                                  inst_map(n,m)
                                end if
                             end if
                          end if

                       end if

                    end do

                 end do
CCCCCCCCCCCCCCCCCCCCC test
C                 write(102,*) acount, asp_start, n, m, asp_time1,
C     &                asp_time2, delta_time
CCCCCCCCCCCCCCCCCCCCC
                 acount = acount + 1
                 asp_time1 = asp_time2

              end if

              if (asp_time2 .gt. tstop_plus .or. asp_start
     &             .eq. -1 .or. asp .ge. nattitude .or.
     &             total_time .eq. all_time) loop = .FALSE.

              if (loop) then
                 if (fixed_step) then
                    do i = 1, attstep
                       if (asp .le. nattitude)
     &                      call asp_record (aunit, asp_start,
     &                      asp, asp_time2, q, quality, status)
                       atotal = atotal + 1
                    end do
                 else
                    sep = sqrt((newx-delx)**2+(newy-dely)**2)
                    do while (sep .lt. astep .and.
     &                   asp .le. nattitude-1 .and.
     &                    asp_time2 .lt. tstop_plus
     &                    .and. asp_start .ne. -1)
                        call asp_record (aunit, asp_start, asp,
     &                       asp_time2, q, quality, status)
                        delta_time  = 0.0
                        do k=1, ngti
                           delta_time = delta_time +
     &                          max(min(asp_time2, gti_stop(k)) -
     &                          max(asp_time1, gti_start(k)),0.0d0)
                        end do
                        if (delta_time .gt. 0.0) then
                           atotal = atotal + 1
c                        end if
                           call rd2sat(ra,dec,q,theta,phi,roll,error)
                           newx=theta*cos(phi)/cdelt1/deg_to_rad
                           newy=theta*sin(phi)/cdelt2/deg_to_rad
                           sep = sqrt((newx-delx)**2+(newy-dely)**2)
                        end if
                     end do
                     newx=delx
                     newy=dely
                  end if
               end if

               if (int(100.0*total_time/all_time) .gt. p) then
                  if (verbose) then
                     write(info,'(1x,i3,a,1x,f12.2,1x,f12.2)') p,
     &                    ' Percent Complete: Total/live time: ',
     &                    total_time, total_live
                     call fcecho(info)
                     p = p + 10
                  end if
               end if

            end do

            if (error) then
               call fcecho('ERROR: error during aspect calculation')
               status = 1
            end if

         end if

         if (p .lt. 101 .and. verbose) then
            p=100
            write(info,'(1x,i3,a,1x,f12.2,1x,f12.2)') p,
     &           ' Percent Complete: Total/live time: ',
     &           total_time, total_live
            call fcecho(info)
         end if
         call fcecho(' ')

         if (.not. fixedasp) then
            if (acount .gt. 0) then
               delx = delxsum / acount
               dely = delysum / acount
            else
               delx = 0.0
               dely = 0.0
            end if

            if (verbose) then
               write(info,'(a, i12)')
     &              ' Number of attitude steps  used: ', acount
               call fcecho(info)
               write(info,'(a, i12)')
     &              ' Number of attitude steps avail: ', atotal
               call fcecho(info)
               write(info,'(a, 2(1x,f12.4))')
     &              ' Mean RA/DEC pixel offset: ',
     &              delx, dely
               call fcecho(info)
            end if
         end if
         call fcecho(' ')

        end

C******************************************************************************

        subroutine live_time(rate_file, first, testtime)
        integer rate_file, first
        double precision testtime

        end

C******************************************************************************
C SUBROUTINE:
C
C       write_sky_fits
C
C DESCRIPTION:
C
C      write out a FITS image file
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  Jan 1993
C       ASTRO-D GOF, GSFC
C
C******************************************************************************

        subroutine write_sky_fits (hdr, nhdr, filename, input_name,
     &       isize, idim, jdim, telescope, instrument, sequence,
     &       object, startdate, starttime, stopdate, stoptime,
     &       radecsys, ctype1, ctype2, equinox, cdelt1,
     &       cdelt2, crpix1, crpix2, crval1, crval2, roll,
     &       exposure, type, matrix,status)

c	subroutine arguments:

	implicit none

        integer nhdr
	character(80) hdr(nhdr)
        character*(*) filename, input_name, sequence, object
        character*(*) telescope, instrument, radecsys, ctype1, ctype2
        character*(*) startdate, starttime, stopdate, stoptime
	integer idim, jdim, isize, type, status
	real cdelt1, cdelt2, crpix1, crpix2, crval1, crval2
        real exposure, equinox, roll
        real matrix(isize,*)

c	FITS declarations

        integer iunit,bitpix,naxis,naxes(99),pcount,gcount
        integer group, i
        logical simple,extend
        character(80) errtxt, comtxt

c	start:

c	intialize variables:

        status = 0
        iunit = 15
        simple =.true.
        bitpix = type
        naxis = 2
        naxes(1) = idim
        naxes(2) = jdim
        pcount = 0
        gcount = 1
        extend = .false.

        group  = 1

c	start:

        call ffinit(iunit,filename,status)

C       write the required primary array keywords:

        call ftpprh(iunit,simple,bitpix,naxis,naxes,pcount,gcount,
     &       extend,status)

        call ftpkys (iunit, 'TELESCOP', telescope, ' ', status)
        call ftpkys (iunit, 'INSTRUME', instrument, ' ', status)
        call ftpkys (iunit, 'SEQNUM', sequence, ' ', status)
        call ftpkys (iunit, 'OBJECT', object, ' ', status)
        call ftpkys (iunit, 'DATE-OBS', startdate, ' ', status)
        call ftpkys (iunit, 'TIME-OBS', starttime, ' ', status)
        call ftpkys (iunit, 'DATE-END', stopdate, ' ', status)
        call ftpkys (iunit, 'TIME-END', stoptime, ' ', status)
        call ftpkys (iunit, 'RADECSYS', radecsys, ' ', status)
        call ftpkys (iunit, 'CTYPE1', ctype1, ' ', status)
        call ftpkys (iunit, 'CTYPE2', ctype2, ' ', status)
        call ftpkyf (iunit, 'EQUINOX', equinox, 6, ' ', status)
        call ftpkyf (iunit, 'CDELT1', cdelt1, 6, ' ', status)
        call ftpkyf (iunit, 'CDELT2', cdelt2, 6, ' ', status)
        call ftpkyf (iunit, 'CRPIX1', crpix1, 6, ' ', status)
        call ftpkyf (iunit, 'CRPIX2', crpix2, 6, ' ', status)
        call ftpkyf (iunit, 'CRVAL1', crval1, 6, ' ', status)
        call ftpkyf (iunit, 'CRVAL2', crval2, 6, ' ', status)
        call ftpkyf (iunit, 'ROLLANG', roll, 6, ' ', status)
        call ftpkyf (iunit, 'EXPOSURE', exposure, 6, ' ', status)

        if (nhdr .gt. 0) then
           do i=1, nhdr
              call ftprec (iunit, hdr(i), status)
           end do
        end if
	comtxt = 'For file '//input_name(1:77)
	call ftphis (iunit, comtxt, status)

C	define primary array structure:

        call ftpdef(iunit,bitpix,naxis,naxes,pcount,gcount,status)

C	write the primary array of data:

        call ftp2de(iunit,group,isize,idim,jdim,matrix,status)

        call ftclos(iunit,status)

        if (status .ne. 0) then

           write(comtxt,*)
     &          'NOTE: Could not write out image, FITSIO status:  ',
     &          status
           call fcecho(comtxt)

        end if

	return

        end


C******************************************************************************
C     SUBROUTINE:
C
C     read_rate
C
C     DESCRIPTION:
C
C     opens rate file for the given observation file
C
C     AUTHOR/DATE:
C
C     Eric Gotthelf,      Jan 1993
C     ASTRO-D GOF, GSFC
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C
C     call read_rate(rate_name, status)
C
C     ARGUMENTS:
C
C     data_name - input FITS file and extension number
C     status    - FITSIO status
C
C     PRIMARY LOCAL VARIABLES:
C
C     dmode  - datamode string
C     detect - detector string
C     maxcl  - maximum number of FITS table columns
C     contxt - error discription string and temp string
C     mode   - common block containing various det mode and data file info
C
C     fitsio variables - hdtype, nrows, filenm, xtensn, rwmode, block, rowlen
C     vardat, fcstln, crpix1, crpix2, tfield, exact
C     tbcol, ttype, tform, tunit, extnam, comment,  keywd
C
C     CALLED ROUTINES:
C
C     subroutine fcecho - echo message to terminal
C     function fcstln   - returns index of last non-blank character
C     subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

        subroutine  open_sis_rate_file (file_name, iunit,
     &       rtcol, rncol, rvcol, smcol, aecol,
     &       lastcol, satfcol,
     &       arena_col, satf_col,lasth_col, lastv_col,
     &       rtime_col, status)

        implicit none

        character*(*) file_name, rtcol, rncol, rvcol, smcol, aecol,
     &       lastcol, satfcol

        integer iunit, status
        integer arena_col,satf_col(0:3),lasth_col(0:3),lastv_col(0:3)

        include 'asca_defs.inc'

        integer xsize, ival, i, stat, det

        integer maxcl
        parameter (maxcl = 512)

        integer inst, row, hdtype, nrecords, xtensn, rwmode
        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        integer name_col, value_col, rtime_col
        real tscale, tzero
        double precision rstart, rstop

        character(80) filenm, ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam, contxt, comment, telescope, string
        character(15) keywd, dmode

        logical anyf, exact, flgval

        include 'asca_common.inc'

        tzero  = 0.0
        tscale = 1.0
        stat = 0
        exact = .false.
        rwmode = 0

C     get the input file name and extension

        call fcpars (file_name, filenm, xtensn, status)
        call ftopen (iunit, filenm, rwmode, block, status)

        contxt = 'Unable to open RATE file: '//filenm

        if (status .ne. 0) goto 999

C     check for reasonable extension number

        if (xtensn .eq. -99) xtensn = 1
        contxt = 'Primary array not supported'
        if (xtensn .le. 0) goto 998

C     move to the correct extension

        call ftmrhd (iunit, xtensn, hdtype, status)
        contxt = 'Error moving to requested extension'
        if (status .ne. 0) goto 998

C     get header depending on extension type

        if (hdtype .eq. 1) then
           call ftghtb (iunit, maxcl, rowlen, nrecords, tfield, ttype,
     &          tbcol, tform, tunit, extnam, status)
        else if (hdtype .eq. 2) then
           call ftghbn (iunit, maxcl, nrecords, tfield, ttype, tform,
     &          tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           goto 998
        endif

C     get start and stop time for this gain history file

        call ftgkyd(iunit, 'TSTART', rstart, comment, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTART'
           call fcecho (contxt)
           goto 998
        endif

        call ftgkyd(iunit, 'TSTOP', rstop, comment, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTOP'
           call fcecho (contxt)
           goto 998
        endif

C     check if event file and gain history times are in range

        if (rstart-60.0D0.gt.tstart.or.rstop+60.0D0.lt.tstop) then

           if (rstart .gt. tstart) then
              contxt =
     & 'WARNING: Event start time preceeds rate file'
           else
              contxt =
     & 'WARNING: Event end time is not covered by rate file'
           endif

           go to 998

        end if

C     check that the TIME columns exist

        call ftgcno (iunit, exact, 'TIME', rtime_col, status)
        contxt = 'TIME column do not exist'
        if (status .ne. 0) goto 998

        det = 0
        if (detector .eq. SIS1) det = 1

C     check that the ARENA columns exist

        write(string,'(a1,i1,a1,a10)') 'S',det,'_',aecol
        call ftgcno (iunit, exact, string, arena_col, status)
        contxt = 'ARENA column do not exist: '//string
        if (status .ne. 0) goto 998

C     check that the  TSAT columns exist

        do i = 0, 3
           write(keywd,'(a1,i1,a1,a5)') 'S',det,'_',satfcol
           call catnum(string,keywd,i)
           call ftgcno (iunit, exact, string, satf_col(i), status)
           contxt = 'SATURATION column do not exist: '//string
           if (status .ne. 0) goto 998
        end do

C     check that the LAST_H/V columns exist

        do i = 0, 3
           write(string,'(a1,i1,a1,a3,a1,i1)')
     &          'S',det,'_',lastcol,'X',i
           call ftgcno (iunit, exact, string, lasth_col(i), status)
           write(string,'(a1,i1,a1,a3,a1,i1)')
     &          'S',det,'_',lastcol,'Y',i
           call ftgcno (iunit, exact, string, lastv_col(i), status)
           contxt = 'LAST column do not exist: '//string
           if (status .ne. 0) goto 998
        end do

        return

 998    call fcerr (contxt)
        stat = 0
        call ftclos (iunit, stat)

        return

 999    call fcerr (contxt)

        return

        end

C  NOTE: The following function, dt_calc(), is not currently called, and
C  is not even quite complete (for instance, the column number variables are
C  never declared or given values).  It has been commented out because of
C  compiler complaints.  However, Eric says that it's a prototype for the
C  next version of ascaexpo, so DON'T REMOVE IT!   -- JRG 1999-03-05
C******************************************************************************
C FUNCTION:
C
C      dt_calc
C
C DESCRIPTION:
C
C      get_cal_peak
C
C AUTHOR/DATE:
C
C       Eric Gotthelf.
C       NASA / GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C
C ARGUMENTS:
C
C     iunit     - FORTRAN I/O unit for opened data file
C     verbose   - whether to print out diagnostic info
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     fitsio variables - hdtype, rows, filenm, xtensn, rwmode, block, rowlen
C                        vardat, fcstln, crpix1, crpix2, tfield, exact
C                        tbcol, ttype, tform, tunit, extnam, comment,  keywd
C
C CALLED ROUTINES:
C
C      subroutine fcecho - echo message to terminal
C      subroutine ftxxxx - FITSIO calls
C
C******************************************************************************
C
C        subroutine dt_calc(rate_unit, dt_tzero, dt_delta, dt,
C     &       nrecords, status)
C        integer rate_unit, nrecords, status
C        real dt_delta, dt
C        double precision dt_tzero
C
CC     loop through file and get values
C
C        include 'asca_defs.inc'
C
C        real cal_pi, cal_peak
C        integer i, nrow
C        character(80) warning
C
C        include 'asca_common.inc'
C
C        row = 1
C        event_time = 0.0d0
C
C        if( gh_start(1).le.event_time .and.
C     &       event_time.lt.gh_stop(gh_records)) then
C           i = 1
CCebi  do while (gh_stop(i).lt.event_time)
C 2         continue
C           if (.not.(gh_stop(i).lt.event_time)) go to 1
C           i = i + 1
C           go to 2
C 1         continue
CCebi  end do
C
C           if(event_time.ge.gh_start(i)) then
C              cal_peak = gh_iron(i)
C           else
C              cal_peak = gh_iron(i-1)+(gh_iron(i)-gh_iron(i-1))*
C     &             (2*event_time - gh_start(i-1) - gh_stop(i-1)) /
C     &             (gh_start(i)+gh_stop(i)-gh_start(i-1)-gh_stop(i-1))
C           end if
C
C        else
C
C           write(warning, '(a,f16.8)')
C     &          'CAUTION: TIME is out of range of rate file: ',
C     &          event_time
C           call fcecho(warning)
C           cal_peak = 1
C           cal_pi = 0
C        end if
C
C        nrow = 1
C
C        if (row .eq. 1) then
C           call ftgcfd (iunit, tstart_col, nrow, 1, 1, start,
C     &          flgval, anyf, status)
C
C           do while (start .lt. tstart-60.0d0 .and. status .eq. 0 .and.
C     &          nrow .le. nrecords)
C
C              call ftgcfd (iunit, tstart_col, nrow, 1, 1, start,
C     &             flgval, anyf, status)
C
C              nrow = nrow + 1
C
C           end do
C
C        end if
C        i = 1
C        nrow = max(nrow - 2, 1)
C
C        do while (stop .lt. tstop+60.0d0 .and. status .eq. 0 .and.
C     &       nrow .le. nrecords)
C
C           call ftgcfd (iunit, tstart_col, nrow, 1, 1, gh_start(i),
C     &          flgval, anyf, status)
C
C           call ftgcfd (iunit, tstop_col, nrow, 1, 1, gh_stop(i),
C     &          flgval, anyf, status)
C           stop = gh_stop(i)
C
C           call ftgcfj (iunit, hvl_col, nrow, 1, 1, gh_hvl(i),
C     &          flgval, anyf, status)
C
C           call ftgcfj (iunit, hvh_col, nrow, 1, 1, gh_hvh(i),
C     &          flgval, anyf, status)
C
C           call ftgcfe (iunit, hvh_col, nrow, 1, 1, gh_temp(i),
C     &          flgval, anyf, status)
C
C           call ftgcfe (iunit, iron_col, nrow, 1, 1, gh_iron(i),
C     &          flgval, anyf, status)
C
C           call ftgcfe (iunit, rise_col, nrow, 1, 1, gh_rise(i),
C     &          flgval, anyf, status)
C
C           i  = i + 1
C           nrow = nrow + 1
C
C        end do
C
C        if (status .eq. 0)  then
C           gh_records = i - 1
C        else
C           gh_records = max(i - 2, 0)
C        end if
C
C        end
C
C******************************************************************************
C SUBROUTINE:
C
C      makesisinst
C
C DESCRIPTION:
C
C      create instrument map
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C      call makeimap (ccds, isize, idim, jdim, imap,
C     &       xbin, ybin, error)
C
C******************************************************************************

      subroutine makesismap (n_hp, hp_ccd, hp_rawx, hp_rawy,
     &     isize, idim, jdim, imap,
c    &     t1, t2, itime,
c    &     area_io, area_hstart, area_hstop, area_vstart, area_vstop,
     &     xbin, ybin, satu, nsat, tsat_h, tsat_v, tsat_t,
     &     verbose, status)

        implicit none

        include 'asca_defs.inc'

        integer n_hp,hp_ccd(n_hp),hp_rawx(n_hp),hp_rawy(n_hp)
        integer isize, idim, jdim, xbin, ybin, nsat, status
        integer tsat_h(0:3,nsat), tsat_v(0:3,nsat)
        real imap(isize, *)
        double precision t1, t2, tsat_t(nsat)
        logical satu, verbose

        integer area_hstart(0:3), area_hstop(0:3)
        integer area_vstart(0:3), area_vstop(0:3), area_io(0:3)
        integer chipsize
        parameter (chipsize = 640)
        integer hp_map(chipsize, chipsize)
        integer sat_hist(chipsize), sat_hist_h(chipsize)
        integer i, j, k, x_flip, y_flip, x_pix1, y_pix1, chip,xdet,ydet
        real sx, sy, rx, ry, fx, fy, cos_ang, sin_ang
        real x, y, norm
        real dt, itime, delta
        character(3) on_off(0:2), in_out(0:2)

        data on_off/ 'OFF', ' ON', 'N/A'/
        data in_out/ 'OUT', ' IN', 'N/A'/

        include 'asca_common.inc'

        if (status .ne. 0) return

C     start:

c     create instrument map:

        satu =.FALSE.

        x_flip =  +1.0
        y_flip =  -1.0

        x_pix1 = int(det_x_pix1)
        y_pix1 = int(det_y_pix1)

        cos_ang = cos(det_rotation * deg_to_rad)
        sin_ang = sin(det_rotation * deg_to_rad)

        area_io(0) = sis_ob(ARIO0)
        area_io(1) = sis_ob(ARIO1)
        area_io(2) = sis_ob(ARIO2)
        area_io(3) = sis_ob(ARIO3)

        area_hstart(0) = sis_ob(STAH0)
        area_hstart(1) = sis_ob(STAH1)
        area_hstart(2) = sis_ob(STAH2)
        area_hstart(3) = sis_ob(STAH3)

        area_hstop(0) = sis_ob(ENDH0)
        area_hstop(1) = sis_ob(ENDH1)
        area_hstop(2) = sis_ob(ENDH2)
        area_hstop(3) = sis_ob(ENDH3)

        area_vstart(0) = sis_ob(STAV0)
        area_vstart(1) = sis_ob(STAV1)
        area_vstart(2) = sis_ob(STAV2)
        area_vstart(3) = sis_ob(STAV3)

        area_vstop(0) = sis_ob(ENDV0)
        area_vstop(1) = sis_ob(ENDV1)
        area_vstop(2) = sis_ob(ENDV2)
        area_vstop(3) = sis_ob(ENDV3)

        if (verbose) then
           write(*,*) ' '
           write(*,91) ' SIS AREA DISC ENABLE: ',
     &          on_off(min(max(sis_ob(ARENA),0),2))

c           if (sis_ob(ARENA) .ne. 0) then
              write(*,*) ' '
              write(*,90) '              CHIP: ', (i, i=0,3)
              write(*,*) ' '
              write(*,92) ' CCD POWER  ON/OFF: ',
     &             (on_off(min(max(ccds(i+1),0),2)), i=0,3)
              write(*,92) ' AREA DISC  IN/OUT: ',
     &             (in_out(min(max(area_io(i),0),2)), i=0,3)
              write(*,90)' AREA DISC H START: ',(area_hstart(i),i=0,3)
              write(*,90)' AREA DISC H  STOP: ',(area_hstop(i),i=0,3)
              write(*,90)' AREA DISC V START: ',(area_vstart(i),i=0,3)
              write(*,90)' AREA DISC V  STOP: ',(area_vstop(i),i=0,3)
c           end if
           write(*,*) ' '
        end if

 90    format(a20, 4i12)
 91    format(a20, a12)
 92    format(a20, 4a12)

       call zero_real_array(imap, isize * isize)

       itime = 4.0
       t1 = 0.0d0
       t2 = 0.0d0

       delta = (t2-t1)

       do chip = 0, 3

c           print*, ' CCD ON: ', (chip+1), ccds(chip+1)

           if (ccds(chip+1) .eq. 1) then

              call zero__int_array(hp_map, chipsize * chipsize)

              if (n_hp .gt. 0) then
                 do k=1,n_hp
                    if (chip.eq.hp_ccd(k))
     &                   hp_map(hp_rawx(k),hp_rawy(k)) = 1
                 end do
              end if

              call zero__int_array(sat_hist, chipsize)
              call zero__int_array(sat_hist_h, chipsize)

              dt = 0.0

              if (satu .and. delta .gt. 0.0) then

                 do k=1,nsat
                    if (tsat_t(k) .gt. t1 .and. tsat_t(k) .le. t2) then
                        sat_hist_h(tsat_h(chip,k)) =
     &                      sat_hist_h(tsat_h(chip,k)) + 1
                    end if
                 end do

                 do i = 1, chipsize
                    sat_hist(i+1) = sat_hist(i) + sat_hist_h(i)
                 end do

                 dt = itime / delta

              end if

c              print*, 'AD ON/OFF: ',(chip+1), sis_ob(ARENA)

              if (sis_ob(ARENA) .eq. 0) then

c                 print*, 'AD OFF: ',(chip+1), sis_ob(ARENA)

c                 do i = cdead+1, cdead+col_width-1
c                    do j = rdead+1, rdead+row_width-1

c                 do i = cdead, cdead+col_width
c                    do j = rdead, rdead+row_width

                 do i = 6, 424
                    do j = 2, 421

                       if (hp_map(i,j) .eq. 0) then

                          call four_chip_image(i, j, chip, x, y)

                          sx = (x) * xyscale - det_x_center
                          sy = (y) * xyscale - det_y_center

                          rx = sx * cos_ang - sy * sin_ang
                          ry = sx * sin_ang + sy * cos_ang

                          fx = (rx * x_flip) + det_x_center
                          fy = (ry * y_flip) + det_y_center

                          xdet = max(min(int(fx)+x_pix1,isize),
     &                         x_pix1)
                          ydet = max(min(int(fy)+y_pix1,isize),
     &                         y_pix1)

c                          imap(xdet, ydet) = 1.0
                          imap(xdet, ydet) = 1.0 - dt * sat_hist(i)

                       end if

                    end do
                 end do

              else

c                 print*, ' AD ON: ',(chip+1), sis_ob(ARENA)
c                 print*, ' IN/OUT: ', (chip+1), area_io(chip)

                 if (area_io(chip) .eq. 0) then

c                    print*, 'OUT ', (chip+1)

                    do i = area_hstart(chip)+1,
     &                   area_hstop(chip)-1
                       do j = area_vstart(chip)+1,
     &                      area_vstop(chip)-1

                          if (hp_map(i,j) .eq. 0) then

                             call four_chip_image(i, j, chip, x, y)

                             sx = (x) * xyscale - det_x_center
                             sy = (y) * xyscale - det_y_center

                             rx = sx * cos_ang - sy * sin_ang
                             ry = sx * sin_ang + sy * cos_ang

                             fx = (rx * x_flip) + det_x_center
                             fy = (ry * y_flip) + det_y_center

                             xdet = max(min(int(fx)+x_pix1,isize),
     &                            x_pix1)
                             ydet = max(min(int(fy)+y_pix1,isize),
     &                            y_pix1)

c                             imap(xdet, ydet) = 1.0
                          imap(xdet, ydet) = 1.0 - dt * sat_hist(i)

                          end if

                       end do
                    end do

                 else

c                    print*, ' IN ', (chip+1)

c                    do i = cdead+1, cdead+col_width-1
c                       do j = rdead+1, rdead+row_width-1

c                    do i = cdead, cdead+col_width
c                       do j = rdead, rdead+row_width

                    do i = 6, 424
                       do j = 2, 421

                          if ( .not.(
     &                         i .gt. area_hstart(chip) .and.
     &                         i .lt. area_hstop(chip)  .and.
     &                         j .gt. area_vstart(chip) .and.
     &                         j .lt. area_vstop(chip))) then

                             if (hp_map(i,j) .eq. 0) then

                                call four_chip_image(i, j,
     &                               chip, x, y)

                                sx = (x) * xyscale - det_x_center
                                sy = (y) * xyscale - det_y_center

                                rx = sx * cos_ang - sy * sin_ang
                                ry = sx * sin_ang + sy * cos_ang

                                fx = (rx * x_flip) + det_x_center
                                fy = (ry * y_flip) + det_y_center

                                xdet = max(min(int(fx)+x_pix1,isize),
     &                               x_pix1)
                                ydet = max(min(int(fy)+y_pix1,isize),
     &                               y_pix1)

c                                imap(xdet, ydet) = 1.0
                          imap(xdet, ydet) = 1.0 - dt * sat_hist(i)

                             end if

                          end if

                       end do
                    end do

                 end if

              end if

           end if

        end do

        if (xbin.gt.1) then

c           call zero__int_array(hp_map, isize * isize)
           call zero__int_array(hp_map, chipsize * chipsize)

           do i=1, isize
              do j=1, isize
                 hp_map((i-1)/xbin+1, (j-1)/ybin+1) =
     &                hp_map((i-1)/xbin+1, (j-1)/ybin+1) +
     &                imap(i, j)
              end do
           end do

           norm = real(xbin*ybin)

           do i = 1, isize/xbin+1
              do j = 1, isize/ybin+1
                 imap(i,j) = hp_map(i,j) / norm
              end do
           end do

        end if
        end

C******************************************************************************
C SUBROUTINE:
C
C      make_sis_dtmap
C
C DESCRIPTION:
C
C      create telemetry deadtime  instrument map
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  April 1995
C       ASTRO-D GOF, GSFC
C
C******************************************************************************

      subroutine make_sis_dtmap (isize, idim, jdim, imap, t1, t2,
     &     itime, xbin, ybin, satu, nsat, tsat_h, tsat_v, tsat_t,
     &     verbose, status)

      implicit none

      include 'asca_defs.inc'

      integer isize, idim, jdim, xbin, ybin, nsat, status
      real itime, imap(isize, isize)
      integer tsat_h(0:3,nsat), tsat_v(0:3,nsat)
      double precision t1, t2, tsat_t(nsat)
      logical satu, verbose

      integer chipsize
      parameter (chipsize = 640)
      integer sat_hist(chipsize), sat_hist_h(chipsize)
      integer i, j, k, x_flip, y_flip, x_pix1, y_pix1, chip,xdet,ydet
      real sx, sy, rx, ry, fx, fy, cos_ang, sin_ang
      real hp_map(sis_size, sis_size)
      real x, y, norm
      real dt, delta

      include 'asca_common.inc'

      if (status .ne. 0) return

C     start:

c     create telemetry deadtime map:

      x_flip =  +1.0
      y_flip =  -1.0

      x_pix1 = int(det_x_pix1)
      y_pix1 = int(det_y_pix1)

      cos_ang = cos(det_rotation * deg_to_rad)
      sin_ang = sin(det_rotation * deg_to_rad)

      call zero_real_array(imap, isize * isize)

      delta = real(t2-t1)

      if (satu .and. delta .gt. 0.0) then

         do chip = 0, 3

            if (ccds(chip+1) .eq. 1) then

               call zero__int_array(sat_hist, chipsize)
               call zero__int_array(sat_hist_h, chipsize)

               do k=1,nsat
                  if (tsat_t(k).gt.t1 .and. tsat_t(k).le.t2) then
                     sat_hist_h(tsat_v(chip,k)) =
     &                    sat_hist_h(tsat_v(chip,k)) + 1
                  end if
               end do

               sat_hist(1) = sat_hist_h(1)
               do i = 2, chipsize
                  sat_hist(i) = sat_hist(i-1) + sat_hist_h(i)
               end do

c               do i = 1, chipsize
c                  write(21,*) chip, i, sat_hist_h(i), sat_hist(i)
c               end do

               dt = itime / delta

               do i = 6, 424
                  do j = 2, 421

                     call four_chip_image(i, j, chip, x, y)

                     sx = (x) * xyscale - det_x_center
                     sy = (y) * xyscale - det_y_center

                     rx = sx * cos_ang - sy * sin_ang
                     ry = sx * sin_ang + sy * cos_ang

                     fx = (rx * x_flip) + det_x_center
                     fy = (ry * y_flip) + det_y_center

                     xdet = max(min(int(fx)+x_pix1,isize),
     &                    x_pix1)
                     ydet = max(min(int(fy)+y_pix1,isize),
     &                    y_pix1)

                     imap(xdet, ydet) = 1.0 - dt * real(sat_hist(j))

                  end do
               end do

            end if

         end do

         if (xbin.gt.1) then

            call zero_real_array(hp_map, sis_size * sis_size)

            do i=1, isize
               do j=1, isize
                  hp_map((i-1)/xbin+1, (j-1)/ybin+1) =
     &                 hp_map((i-1)/xbin+1, (j-1)/ybin+1) +
     &                 imap(i, j)
               end do
            end do

            norm = real(xbin*ybin)

            do i = 1, isize/xbin+1
               do j = 1, isize/ybin+1
                  imap(i,j) = hp_map(i,j) / norm
               end do
            end do

         end if

      end if

      end

        subroutine zero_real_array(array, nreal)
        implicit none
        integer nreal
        real array(nreal)

        integer i

        do i = 1, nreal
           array(i) = 0.0
        end do

        end

        subroutine zero__int_array(array, nint)
        implicit none
        integer nint, array(nint)

        integer i

        do i = 1, nint
           array(i) = 0
        end do

        end

C******************************************************************************
C SUBROUTINE:
C
C      makegisinst
C
C DESCRIPTION:
C
C      create instrument map
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C      call makegismap (ccds, isize, idim, jdim, imap,
C     &       xbin, ybin, error)
C
C******************************************************************************

      subroutine makegismap (fov, isize, idim, jdim, imap,
     &     rebin, status)

        implicit none

        include 'asca_defs.inc'

        integer isize, idim, jdim, rebin, status
        real fov, norm, imap(isize, *)

        integer i, j, x_flip, y_flip, x_pix1, y_pix1, xdet, ydet
        real sx, sy, rx, ry, fx, fy, cos_ang, sin_ang, r, sum
        real det_x_off, det_y_off
        real g_map(sis_size, sis_size)

        include 'asca_common.inc'

C     start:

        det_y_off = det_x_center
        det_x_off = det_y_center

        if (status .ne. 0) return

c     create instrument map:

        x_flip =  +1.0
        y_flip =  -1.0

        x_pix1 = int(det_x_pix1)
        y_pix1 = int(det_y_pix1)

        cos_ang = cos(det_rotation * deg_to_rad)
        sin_ang = sin(det_rotation * deg_to_rad)

        call zero_real_array(imap, isize*isize)

        do i = 1, idim
           do j = 1, jdim

              sx = real(i) - det_x_off
              sy = real(j) - det_y_off

              rx = sx * cos_ang - sy * sin_ang
              ry = sx * sin_ang + sy * cos_ang

              r = sqrt(rx*rx+ry*ry)

              if (r .le. fov) then

                 fx = (rx * x_flip) + det_x_off
                 fy = (ry * y_flip) + det_y_off

                 xdet = max(min(int(fx) + x_pix1,idim), x_pix1)
                 ydet = max(min(int(fy) + y_pix1,jdim), y_pix1)

                 imap(xdet,ydet)=1.0

              end if

           end do
        end do

        print*, 'rebin gis:', rebin
        if (rebin.gt.1 ) then
        print*, 'rebin gis:', rebin

c .and. isize .lt. gis_size) then

           call zero_real_array(g_map, isize*isize)

           do i=1, isize
              do j=1, isize
                 g_map((i-1)/rebin+1, (j-1)/rebin+1) =
     &                g_map((i-1)/rebin+1, (j-1)/rebin+1) +
     &                imap(i, j)
              end do
           end do

           norm = real(rebin*rebin)

           do i = 1, isize/rebin+1
              do j = 1, isize/rebin+1
                 imap(i,j) = g_map(i,j) / norm
                 sum = sum + imap(i,j)
              end do
           end do

        end if

c        print*, 'inst sum: ',sum

        end

C******************************************************************************
C SUBROUTINE:
C
C     gethp
C
C DESCRIPTION:
C
C      find HP extension and read into array
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  Oct 1994
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C     call get_hp()
C
C ARGUMENTS:
C
C     filename  - input calibration FITS file and extension number
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     iunit       - FORTRAN I/O unit for calibration file
C     matrix_size - calibration map size
C     cal         - common block containing calibration data
C
C     fitsio variables - nullval, iunit, status, bitpix, naxis, naxes(99)
C                        pcount, group, rwmode, blocksize, hdutyp, gcount
C                        simple, extend, anyf
C
C CALLED ROUTINES:
C
C      subroutine parse_CAL_header  - parse header information
C      subroutine read_binext_image - read in a FITS image from a bin xtention
C      subroutine fcecho            - echo message to terminal
C      function fcstln              - returns index of last non-blank character
C      subroutine ftxxxx            - FITSIO calls
C
C******************************************************************************

        subroutine get_hp(lun, ext_name, nhp, n_hp, hp_ccd,
     &       hp_rawx, hp_rawy, status)

        implicit none

        integer lun, status, n_hp, nhp
        character*(*) ext_name
        integer hp_ccd(n_hp), hp_rawx(n_hp), hp_rawy(n_hp)

        integer i, k, iext
        integer tfields, nrows, varidat, hdutype, inull
        character(80) errmsg, extname, comment

        logical found

        real nullval
        integer bitpix,naxis,naxes(99),pcount,gcount
        integer group, rwmode, blocksize, stat, nmax
        logical simple,extend,anyf

        integer maxcl
        parameter (maxcl = 512)

        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam

        nhp   = 0
        iext  = 0
        found = .FALSE.

        if ( status. ne. 0) return

        if (index('PRIMARY', ext_name).ne.0) found = .TRUE.

        do while ( status. eq. 0 .and. .not. found)

           stat = 0
           iext = iext + 1
           call ftmahd(lun, iext, hdutype, status)
           call ftgkys(lun, 'EXTNAME', extname, comment, stat)
           if (index(ext_name, extname).ne.0) found = .TRUE.

        end do

c     Now we have extension, get the binary table keywords

        if (.not. found) then

           n_hp = 0
           nrows = 0
           status = 1
           write(errmsg, '(a29,a21)')
     &          'NOTE: did not find HP table: ', ext_name

        else

C     move to the GTI extension

           call ftmahd (lun, iext, hdutype, status)
           if (status .ne. 0) then
              errmsg = 'Error moving to requested extension'
              goto 998
           endif

C     get header depending on extension type

           if (hdutype .eq. 1) then
              call ftghtb (lun, maxcl, rowlen, nrows, tfield, ttype,
     &             tbcol, tform, tunit, extnam, status)
           else if (hdutype .eq. 2) then
              call ftghbn (lun, maxcl, nrows, tfield, ttype, tform,
     &             tunit, extnam, vardat, status)
           endif

           if (status .ne. 0) then
              errmsg = 'Error reading extension header'
              goto 998
           endif

c     loop through GTI intervals:

           if (nrows .gt. n_hp) then
              errmsg = 'INFO: Number of HP rows exceed HP array size'
              call fcecho(errmsg)
           end if

           n_hp=min(nrows,n_hp)
           do i=1,n_hp
              call ftgcvj(lun, 1, i, 1, 1, inull, hp_ccd(i),
     &             anyf, status)
              call ftgcvj(lun, 2, i, 1, 1, inull, hp_rawx(i),
     &             anyf, status)
              call ftgcvj(lun, 3, i, 1, 1, inull, hp_rawy(i),
     &             anyf, status)
           end do

           nhp=nrows

           if (status .ne. 0) then
              errmsg = 'Error reading in HOT PIXEL records'
              goto 998
           endif

           return

        end if

 998    if (status .ne. 0) then
           if (status .ne. 1) call ftclos (lun, status)
           call fcerr(errmsg)
        end if

        return

        end

        subroutine make_expo_file(rate_unit,
     &       asp_unit, time_unit, fixed_asp,
     &       nexpo, ngti, gti_start, gti_stop,
     &       attstep, ccds, nsed, sed_v, sed_t,
     &       ntsat, tsat_h, tsat_v, tsat_t,
     &       lasth_col, lastv_col, arena_col,
     &       satf_col, rtime_col,
     &       status)

        implicit none

        integer rate_unit, asp_unit, time_unit
        integer lun, status, nexpo, ngti, nsed, ntsat
        integer ccds(0:3), lasth_col(0:3), lastv_col(0:3)
        integer rtime_col, arena_col, satf_col(0:3)

        integer attstep, tsat_h(0:3,*), tsat_v(0:3,*), sed_v(5,*)
        double precision gti_start(*),gti_stop(*),tsat_t(*),sed_t(*)
        logical fixed_asp, satf

        integer i, j, k, m, chip, iext, arena, last_arena
        integer maxrows
        integer tfields, nrows, varidat, hdutype, inull
        character(80) errmsg, extname, comment

        logical found

        real nullval
        integer bitpix,naxis,naxes(99),pcount,gcount
        integer group, rwmode, blocksize, stat
        logical simple,extend,anyf, in_good_time

        integer maxcl
        parameter (maxcl = 512)

        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        double precision dnull, time, last_time
        character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam

        iext  = 2
        lun = rate_unit
        found = .FALSE.

C     move to the HK extension

        call ftmahd (lun, iext, hdutype, status)

        if (status .ne. 0) then
           errmsg = 'Error moving to requested extension'
           goto 998
        endif

C     get header depending on extension type

        if (hdutype .eq. 1) then
           call ftghtb (lun, maxcl, rowlen, nrows, tfield, ttype,
     &          tbcol, tform, tunit, extnam, status)
        else if (hdutype .eq. 2) then
           call ftghbn (lun, maxcl, nrows, tfield, ttype, tform,
     &          tunit, extnam, vardat, status)
        endif

        if (status .ne. 0) then
           errmsg = 'Error reading extension header'
           goto 998
        endif

c        time = 0.0d0
c        do m=1,ngti
c           time = gti_stop(m)-gti_start(m) + time
c           print*, gti_start(m), gti_stop(m),
c     &          gti_stop(m)-gti_start(m), time
c        end do

c     loop through file:

        i = 1
        j = 1
        k = 1
        m = 1
        arena = 0
        time = 0.0d0
        last_arena = -1
        last_time = 0.0d0

        do while (i.le.nrows .and. j.le.nsed .and. k.le.ntsat-4)

           call ftgcvd(lun, rtime_col, i, 1, 1, dnull, time,
     &          anyf, status)
           call ftgcvj(lun, arena_col, i, 1, 1, inull, arena,
     &          anyf, status)

           in_good_time=.false.
           do m=1,ngti
              if (time.ge.gti_start(m).and.time.lt.gti_stop(m))
     &             in_good_time = .true.
           end do

c     the much slicker way... but harder to debug for now!
c
c           do while (m .lt. ngti .and. time .lt. gti_stop(m))
c                 m = m + 1
c           end do
c
c           do while (m .le. ngti .and. time .gt. gti_stop(m))
c              print*, m, gti_start(m), time, gti_stop(m)
c              m = m + 1
c           end do
c
c           if (time .ge. gti_start(m) .and.
c     &          time .le. gti_stop(m)) then

           if (in_good_time.and.abs(time-last_time).gt.3.99d0) then

c              write(333,*) i, k, time

              if (arena .ne. last_arena) then
c                print*, 'ae: ',i, j, time, arena, status
                 sed_v(1,j) = arena
                 last_arena = arena
                 sed_t(j) = time
                 j = j + 1
              end if

              do chip = 0, 3
                 if (ccds(chip) .eq. 1) then
                    call ftgcl(lun, satf_col(chip), i, 1, 1,
     &                   satf, status)
c     call ftgcl(lun, telem_col(chip), i, 1, 1, telm, status)
c     if (telem .gt. 0) then
                    if (satf) then
                       call ftgcvj(lun, lasth_col(chip), i, 1, 1,
     &                      inull, tsat_h(chip,k), anyf, status)
                       call ftgcvj(lun, lastv_col(chip), i, 1, 1,
     &                      inull, tsat_v(chip,k), anyf, status)
                       tsat_t(k) = time
                       k = k + 1
                    end if
                 end if
              end do

              last_time = time

           end if

           i = i + 1

        end do

c        print*, 'debug...', i, j, k

        if (j .ge. nsed) then
           call fcecho
     &('WARNING: Number of AREA ENABLE RESETS rows exceed array size')
        end if
        if (k .ge. ntsat-4) then
           call fcecho
     &  ('WARNING: Number of TLM SATURATION FRAMES exceed array size')
        end if

        nsed  = j - 2
        ntsat = k - 1

        if (status .ne. 0) then
           errmsg = 'Error reading in rate records'
           goto 998
        endif

        return

998    if (status .ne. 0) then
           call ftclos (lun, status)
           call fcerr(errmsg)
        end if

        return

        end

        subroutine make_sky_image(iunit, rebin, isize, idim, jdim,
     &       exten, ngti, gti_start, gti_stop, sky_image, status)
        implicit none
        integer iunit, rebin, isize, idim, jdim, exten, ngti
        real    sky_image(isize, isize)
        double precision gti_start(ngti), gti_stop(ngti)

        include 'asca_defs.inc'

        integer i, j, count, inull, hdtype, status
        integer i_index, j_index, x, y, dx, dy
        logical anyf
        double precision t, dnull

        include 'asca_common.inc'

        j = 1
        i = 0
        hdtype = 1
        count = 0

C     zero image array:

        call zero_real_array(sky_image, isize*isize)

C     move to the correct extension

        call ftmahd (iunit, exten, hdtype, status)

        if (status .ne. 0) return

c     Start:

        call ftgcvd(iunit, time_col, 1, 1, 1, dnull, t, anyf,
     &       status)

c     loop for each GTI record:

        do while (j .le. ngti)

c     get every photon for this GTI:

           do while (t .le. gti_stop(j) .and. t .le. tstop .and.
     &          i .lt. n_events .and. status .eq. 0)

c     get next event:

              i = i + 1

              call ftgcvd(iunit, time_col, i, 1, 1, dnull, t, anyf,
     &             status)

              if (t .ge. gti_start(j)) then

                 call ftgcvj(iunit, x_det_col, i, 1, 1, inull, dx,
     &                anyf, status)

                 call ftgcvj(iunit, y_det_col, i, 1, 1, inull, dy,
     &                anyf, status)

                 call ftgcvj(iunit, x_sky_col, i, 1, 1, inull, x,
     &                anyf, status)

                 call ftgcvj(iunit, y_sky_col, i, 1, 1, inull, y,
     &                anyf, status)

c     for each photon, make image:

                 i_index = min(max(( x-1)/rebin+1,1),
     &                idim)
                 j_index = min(max(( y-1)/rebin+1,1),
     &                jdim)

                 if (dx .ne. 1) then
                    sky_image(i_index, j_index) =
     &                   sky_image(i_index, j_index)+1.0
                 end if

                 count = count + 1

              end if

           end do

           j = j + 1

        end do

        end
