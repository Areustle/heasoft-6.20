C*****************************************************************************
C SELECTOR TASK:
C
C     gis_rti
C
C FILE:
C
C     gis_rti.f
C
C DESCRIPTION:
C
C     Program to fill the RTI column of the GIS PH science files.
C     
C     GIS_RTI reads an input Science file and a Telescope Definition file
C     and uses the postion dependent rise time calibration map to compute 
c     the correction to the RT value.
C
C AUTHOR/DATE:
C
C     Eric Gotthelf,    April 1994
C     ASCA GOF, NASA/GSFC
C     
C     MODIFICATION HISTORY:
C        PDW 8/11/99: Fix misspelled CALDB string and replace gtcal with gtcalf
C     
C NOTES:
C
C     This version of GIS_RTI is compatible with FRFREAD version 2.995 
C     and higher.
C
C MAKE:
C
C      HOST: make -f mkhgis_rti
C      IRAF: xc -c xgis_rti.x gis_rti.f gis_ele2det.f
C            xc -p stsdas xgis_rti.o gis_rti.o 
C                  -lmisc -lfitsio -liraf77 -o gis_rti.x
C
C USAGE:
C
C      HOST: hgis_rti
C      IRAF: gis_rti
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
C      subroutine gis_rti_params - gets parameters from environment.
C      subroutine gis_rti     - run main program.
C     
C******************************************************************************

        subroutine GISRTI

        implicit none

        integer status, i, ninfile

        character(160) data_name, cal_name
        character(160) defSISfile, defGISfile, defS0file, defS1file
        character(160) defS2Ffile, defS3Ffile, defS2Pfile
        character(160) defS3Pfile, defATTpath
        character(160) info, vinfile(999)
        character(40) timecol, phacol, picol, skyxcol, skyycol
        character(40) rawxcol, rawycol, chipcol, detxcol, detycol
        character(40) taskname

        logical histry, verbose, negflg

C     SET UP MAIN PROGRAM COMMON BLOCKS:

        include 'gisrti_defs.inc'
        include 'gisrti_comm.inc'

        common /task/ taskname

C     INITIALIZE VARIABLES:

        taskname = 'GISRTI_V1.0.1'
        
C     START:
        
C     ECHO TASK NAME AND VERSION NUMBER: 
       
        call fcecho (taskname) 

C     GET PARAMETERS FROM PARAM FILE:
        
        call gis_rti_params (data_name, cal_name,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile,defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, 
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol, 
     &       skyxcol, skyycol, phacol, picol, histry, verbose, status)
        
        if (status .eq. 0) then
           
           if (verbose .and. data_name(1:1) .eq. '@') then 
              call fcecho (' ') 
              write(info, 222) 'reading list file: ', 
     &             data_name(1:132)
              call fcecho (info) 
           end if
           
C     Parse the infile:
           
           call fcgcls(data_name, vinfile, ninfile, negflg)
           
           if (ninfile .gt. 0) then
              
C     loop through input files and call fixedasp:
              
              do i = 1, ninfile
                 
                 status = 0
                 
C     RUN MAIN PROGRAM:
                 
                 call  gis_rtiear(vinfile(i), cal_name,
     &                defS0file, defS1file, defS2Ffile, defS3Ffile, 
     &                defATTpath, defS2Pfile, defS3Pfile, defSISfile, 
     &                defGISfile, timecol, rawxcol, rawycol, chipcol, 
     &                detxcol, detycol, skyxcol, skyycol, phacol, 
     &                picol, histry, verbose, status)
                 
                 if (status .ne. 0) then
                    write(info,'(a15,a80)')'Error for file: ',cal_name
                    if (status.lt.100.and..not.verbose)call fcerr(info)
                    if (status .gt. 99) call  fcerrm(status)
                 end if

              end do
           
           else
              
              call fcerr('Error parsing list file')
              
           end if
           
        else
           
           call fcerr('Error parsing list file')
           
        end if
        
 222    format(a23, a132)

        end
        
C******************************************************************************
C SUBROUTINE:
C     
C      gis_rti_params
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
C       gis_rti_params uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C
C      call gis_rti_params(infile,aspect, rawxcol, rawycol, phacol
C               timcol, detxcol, detycol, gis_rticol, histry, verbose, status)
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

        subroutine  gis_rti_params (datafile, calfile,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile, defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, 
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol, 
     &       skyxcol, skyycol, phacol, picol, histry, verbose, status)
       
        implicit none 

        character*(*) datafile, calfile, timecol
        character*(*) defS0file, defS1file, defS2Ffile, defS3Ffile
        character*(*) defS2Pfile, defS3Pfile, defSISfile
        character*(*) defGISfile, defATTpath
        character*(*) detxcol, detycol, rawxcol, rawycol
        character*(*) chipcol, phacol, picol, skyxcol, skyycol
        logical histry, verbose
        integer      status

        character(80) contxt

C Initialize variables 

        status = 0

C Get the name of the input FITS file

        call uclgst('datafile', datafile, status)
        contxt = 'Could not get DATAFILE parameter'
        if (status .ne. 0) go to 999    
        
C     Get the name of the input filter CALIBRATION file
        
        call uclgst('calfile', calfile, status)
        contxt = 'Could not get CALFILE parameter'
        if (status .ne. 0) go to 999    
                
C     Get the input TIME column name - hidden parameter
        
        call uclgst ('timecol', timecol, status)
        contxt = ' Could not get TIMECOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the input ELECTRONIC X intermediate column name - hidden parameter
        
        call uclgst ('rawxcol', rawxcol, status)
        contxt = ' Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the input ELECTRONIC X intermediate column name - hidden parameter

        call uclgst ('rawycol', rawycol, status)
        contxt = ' Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output SKY X column - hidden parameter
        
        call uclgst ('skyxcol', skyxcol, status)
        contxt = ' Could not get SKYXCOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the name of the output DETECTOR X column - hidden parameter
        
        call uclgst ('detxcol', detxcol, status)
        contxt = ' Could not get DETXCOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the name of the ouput DETECTOR Y column - hidden parameter
        
        call uclgst ('detycol', detycol, status)
        contxt = ' Could not get DETYCOL parameter'
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

C     Get the name of the ouput RT column - hidden parameter
        
        call uclgst ('rtcol', phacol, status)
        contxt = ' Could not get RTCOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the name of the ouput RTI column - hidden parameter
        
        call uclgst ('rticol', picol, status)
        contxt = ' Could not get RTICOL parameter'
        if (status .ne. 0) go to 999
        
C     Get whether to write information to screen - hidden paramter
        
        call uclgsb ('verbose', verbose, status)
        contxt = ' Could not get VERBOSE parameter'
        if (status .ne. 0) go to 999
        
C     Get whether to add history record - hidden paramter
        
        call uclgsb ('history', histry, status)
        contxt = ' Could not get HISTORY parameter'
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

 999    continue
        
        if (status .ne. 0) call fcerr (contxt)

        end
        
C******************************************************************************
C     SUBROUTINE:
C     
C     gis_rtiear
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
C        PDW 8/11/99: Fix misspelled CALDB string and replace gtcal with gtcalf
C     
C     NOTES:
C     
C     USAGE:
C     
C     call gis_rtiear(data_name, cal_name, detxcol, detycol, 
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
        
        subroutine gis_rtiear(data_name, cal_name,
     &       defS0file, defS1file, defS2Ffile, defS3Ffile, defATTpath,
     &       defS2Pfile, defS3Pfile, defSISfile, defGISfile, 
     &       timecol, rawxcol, rawycol, chipcol, detxcol, detycol,
     &       skyxcol, skyycol, phacol, picol, histry, verbose, status)
        implicit none
        character*(*) data_name, cal_name
        character*(*) defS0file, defS1file, defS2Ffile, defS3Ffile
        character*(*) defS2Pfile, defS3Pfile, defSISfile 
        character*(*) defGISfile, defATTpath 
        character*(*) timecol, rawxcol, rawycol, chipcol
        character*(*) detxcol, detycol, skyxcol, skyycol, phacol, picol
        logical histry, verbose, oldfrf
        integer status, chatter
        include 'gisrti_defs.inc'
        integer ii, jj, i, row, last_det, last_pos
        integer data_unit, cal_unit, cccc, rt_bin, rti_bin
        integer stat, calxo, calyo, inull, ival
        integer extno, day, month, year, hour, mins, count
        integer maxret, nfound, nret
        real x, y, sx, sy, tx, ty, x_flip, y_flip
        real det_x_offset, det_y_offset, xyoffset
        real sec, fx, fy, random, deg_to_rad
        logical file_open, cal_open, anyf,caldb,ftools,default,new_cal
        character(160) info, cal_time, gain_time, creator
        character(160) codename, timestr, datestr, last_cal
        character(160) asp_time, data_file, frf_name, detect, pos_det
        character(40) taskname, keywd
        character(160) teldefF2, teldefF3, teldefP2, teldefP3, online
        parameter(deg_to_rad = 0.01745329) 
        include 'gisrti_comm.inc'
        common /task/ taskname        
        data last_cal / ' ' /
        data teldefF2 / 'gis2_ano_on_flf_042094.fits' /
        data teldefP2 / 'gis2_ano_on_pow2_042094.fits' /
        data teldefF3 / 'gis3_phnew_tbl_flf_200494.fits' /
        data teldefP3 / 'gis3_ano_on_pow2_042094.fits' /
        data default, caldb, ftools, file_open, cal_open, new_cal
     &       /.FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE./
        data last_det, last_pos / -1, -1 / 
        save default, caldb, ftools, file_open, cal_open, last_cal
        count = 0
        cal_unit = 3
        data_unit = 1
        oldfrf = .FALSE.
        if (status .eq. 0) then
           write(info, 222) 'reading data file: ', data_name(1:132)
           if (verbose) then 
              call fcecho (' ') 
              call fcecho (info) 
           end if
           write(data_file, '(a)') data_name
           call oooo(data_name, frf_name, creator, data_unit, 
     &          detect, pos_det, timecol, rawxcol, rawycol, chipcol, 
     &          detxcol, detycol, skyxcol, skyycol, phacol, picol,
     &          histry, verbose,status)
           if ((n_events.eq.0.or.rise_size.le.1).and.status.eq.0) then
              if (verbose) then
                 if (n_events.eq.0)
     &                call fcerr('File contains zero events, ignoring')
                 if (rise_size.le.1)
     &           call fcerr('File contains no risetime info, ignoring')
              end if
              file_open = .TRUE.
              status = 1
           end if
           if (status .eq. 0) then
              file_open = .TRUE.
              if (cal_name .eq. 'CALDB' .or. caldb .or.
     &             cal_name .eq. 'caldb' ) then 
                 caldb = .TRUE.
                 call tttt(tstart, year, month,day,hour,mins,sec)
                 write(datestr, 333) year, month, day
                 write(timestr, 444) hour, mins, int(sec)
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
                 if ( verbose ) then
                    chatter = 15
                 else
                    chatter = 5
                 endif
                 call gtcalf(chatter,'ASCA',detect,'-','-',codename,
     &                datestr,timestr,datestr,timestr,'-',maxret,
     &                cal_name,extno,online,nret,nfound,status)
              else if (cal_name .eq. 'FTOOLS' .or. ftools .or.
     &                cal_name .eq. 'ftools' ) then 
                 ftools = .TRUE.
                 if (detector .eq. GIS2) then
                    if (pos_meth .eq. FLF) then
                       call fgfcal(cal_name, teldefF2, status)
                    else if (pos_meth .eq. POW2) then
                       call fgfcal(cal_name, teldefP2, status)
                    end if
                 else if (detector .eq. GIS3) then
                    if (pos_meth .eq. FLF) then
                       call fgfcal(cal_name, teldefF3, status)
                    else if (pos_meth .eq. POW2) then
                       call fgfcal(cal_name, teldefP3, status)
                    end if
                 end if
              else if (cal_name .eq. 'DEFAULT' .or. 
     &                cal_name .eq. 'default' .or. default) then
                 default = .TRUE.
                 if (detector .eq. GIS2) then
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
                 if (last_cal .ne. cal_name .or. last_det .ne. 
     &                detector .or. last_pos .ne. pos_meth .or.
     &                new_cal) then
                    last_cal = cal_name
                    last_det = detector
                    last_pos = pos_meth
                    write(info, 222) 
     &                   'reading cal  file: ', cal_name(1:132)
                    if (verbose) call fcecho (info)
                    call rrrr (cal_name, cal_unit, cal_time, 
     &                   calxo, calyo, verbose, status)
                    if (status .ne. 0) then
                       new_cal = .TRUE.
                       call fcerr
     &                      ('Cannot read teldef file, skipping...')
                       status = 1
                    else 
                       new_cal = .FALSE.
                    end if
                 end if
                 if (status .eq. 0) then
                    if (verbose) call fcecho(' ')                       
                    if (verbose) 
     &                   call fcecho('    processing data...')
                    det_x_offset = det_x_center * det_x_scale
                    det_y_offset = det_y_center * det_y_scale
                    xyscale =  float(gis_size) / float(pos_size)
                    do row = 1, n_events
                       call ftgcvj(data_unit, rt_col, row, 1, 1, 
     &                      inull, ival, anyf, status)
                       rt_bin=max(min(ival, rise_size-1),0)
                       call ftgcvj(data_unit, x_raw_col, row, 1, 1,
     &                      inull, ival, anyf, status)
                       ii=max(min(ival,gis_size-1),0)
                       call ftgcvj(data_unit, y_raw_col, row, 1, 1,
     &                      inull, ival, anyf, status)
                       jj=max(min(ival,gis_size-1),0)
                       x=(float(ii)+random(iseed)) * xyscale
                       y=(float(jj)+random(iseed)) * xyscale
                       tx=tr(1)+tr(2)*x+tr(3)*y
                       ty=tr(4)+tr(5)*x+tr(6)*y
                       sx=(tx+det_x_offset)/det_x_scale
                       sy=(ty+det_y_offset)/det_y_scale
                       if(rti_size.gt.1) rti_bin = max(min(int(
     &                      cccc (sx,sy,rt_bin)),rti_size-1),0)
                       call ftpclj(data_unit, rti_col, row, 1, 1, 
     &                      rti_bin, status)
                       count = count + 1
                    end do
                    if (verbose .and. status .eq. 0) then
                       call fcecho (' ')
                       write(info, 1000) count
                       call fcecho (info)
                    end if
                    if (status .eq. 0) then
                       if (verbose) call fcecho(' ')
                       if (verbose) 
     &                      call fcecho(
     &                      '    updating data file det keywords...')
                       call kkkk(keywd, 'TLMIN',rti_col)
                       call ftmkyj(data_unit, keywd, 0, 
     &                      'First RTI channel enumeration', status)
                       if (status .eq. 202) then
                          status = 0
                          call ftpkyj(data_unit, keywd, 0, 
     &                         'First RTI channel enumeration', status)
                       end if
                       call kkkk(keywd, 'TLMAX',rti_col)
                       call ftmkyj(data_unit, keywd, rti_size-1, 
     &                      'Last  RTI channel enumeration', status)
                       if (status .eq. 202) then
                          status = 0
                          call ftpkyj(data_unit, keywd, rti_size-1, 
     &                         'Last RTI channel enumeration', status)
                       end if
                       write(info,'(a24,a)') 'RTI column updated with ',
     &                      taskname
                       if (histry) call ftphis (data_unit, info, 
     &                      status)
                       write(info,'(a18,a140)') 'Using TELDEF file: ', 
     &                      cal_name
                       if (histry) call ftphis (data_unit, info, 
     &                      status)
                       write(info,'(a7,a30)') 'Dated: ', 
     &                      cal_time
                       if (histry) call ftphis (data_unit, info, 
     &                      status)
                       if (status .ne. 0) then
                          call fcerr('Error updating RTI keywords')
                       end if
                    else
                       call fcerr('Error processing data')   
                    end if
                 else
                    call fcerr
     &                   ('Error reading telescope definition file')
                 end if
              else
                 call fcerr('Error getting filename from CALDB')
              end if
           else
              if (status.gt.99)call fcerr('Error reading data file')
           end if
        end if
        if (cal_open) then 
           stat = 0
           if (verbose) call fcecho('    closing cal  file...')
           call  ftclos(cal_unit, stat)
           cal_open = .FALSE.
        end if
        if (file_open) then 
           stat = 0
           if (verbose) call fcecho('    closing data file...')
           call  ftclos(data_unit, stat)
           file_open = .FALSE.
        end if
 222    format(a23, a132)
 333    format(i4.4,'-',i2.2,'-',i2.2)
 444    format(i2.2,':',i2.2,':',i2.2)
 1000   format (' Number of EVENT records processed  = ', I10)
        end
        subroutine rrrr (filename,iunit,cal_time,calxo,calyo,verbose,
     &       status)
        implicit none
        include 'gisrti_defs.inc'
        integer maxdims
        parameter (maxdims = 99)
        integer iunit, calxo, calyo, status
        character*(*) filename, cal_time
        integer matrix_size, cal_inst, cal_meth, ip, jp
        real x, y, dx, dy, tx, ty, sx, sy, det_x_offset, det_y_offset
        character(80) contxt, cal_det, cal_pos
        integer stat
        integer bitpix, naxis, naxes(maxdims), pcount
        integer rwmode, blocksize, gcount, ghversion
        logical simple, extend, verbose
        include 'gisrti_comm.inc'
        stat = 0
        rwmode = 0
        cal_meth = -1
        cal_inst = -1
        matrix_size = gis_size
        call ftopen(iunit, filename, rwmode, blocksize, status)
        if (status .eq. 0) then
           call ftghpr(iunit, maxdims, simple, bitpix, naxis, naxes,
     &          pcount, gcount, extend, status)
           if (status .eq. 0) then
              call ftgkys(iunit, 'INSTRUME', cal_det, contxt, status)
              call ftgkys(iunit, 'POS_DET', cal_pos, contxt, status)
              if (status .eq. 0) then
                 if (index(cal_pos, 'FLF' ) .ne. 0) cal_meth = FLF
                 if (index(cal_pos, 'POW2') .ne. 0) cal_meth = POW2
                 if (index(cal_det, 'GIS2') .ne. 0) cal_inst = GIS2
                 if (index(cal_det, 'GIS3') .ne. 0) cal_inst = GIS3
                 if (cal_meth .ne. pos_meth) then 
                    write(contxt,'(a45, 1x, a32)') 
     &                'Error: Cal and data file POS_DET differ; CAL:',
     &                   cal_pos
                    call fcerr (contxt)
                    status = 1
                 end if
                 if (cal_inst .ne. detector) then 
                    write(contxt,'(a46, 1x, a31)') 
     &                'Error: Cal and data file INSTRUME differ; CAL:',
     &                   cal_det
                    call fcerr (contxt)
                    status = 1
                 end if
              else
                 call fcerr('Unable to get POS_DET or INSTRUME keyword')
              end if              
              if (status .eq. 0) call pppp (
     &             iunit, cal_time, calxo, calyo, status)
              if (status .eq. 0) then
                 if (status .eq. 0) then
                    call bbbb(iunit, 4, matrix_size, 
     &                   deltax, status)
                    if (status .eq. 0) then
                       call bbbb(iunit, 6, matrix_size,
     &                      deltay, status)
                       if (status .eq. 0) then
                          call bbbb(iunit, 7,
     &                         matrix_size, rt_map, status)
                          if (status .eq. 0) then
                             if (verbose) 
     &                         call fcecho('    closing cal  file...')
                             call ftclos(iunit, status)
                             if (status .eq. 0) then
                                return
                             else
                                contxt = 'Error closing GIS CAL file '
                             end if
                          else
                             contxt = 
     &                            'Error reading GIS CAL file RT map'
                          end if 
                       else
                          contxt = 
     &                         'Error reading GIS CAL file Y error map'
                       end if 
                    else
                       contxt = 
     &                      'Error reading GIS CAL file X error map'
                    end if 
                 else
                    contxt = 'Error reading GIS CAL file gain map '
                 end if 
              else
                 contxt = 'Error parsing GIS CAL file header '
              end if 
           else
              contxt = 'Unable read GIS CAL file primary header'
           end if 
           if (verbose) call fcecho('    closing cal  file...')
           call ftclos(iunit, stat)
        else
           contxt = 'Unable to open GIS CAL file: '//filename
        end if 
        call fcerr(contxt)
        end
        subroutine pppp (iunit, cal_time, calxo, calyo,
     &       status)
        implicit none
        include 'gisrti_defs.inc'
        integer calxo, calyo, iunit, status
        character*(*) cal_time
        integer i, j
        real det_x_offset, det_y_offset
        real num
        character(80) comment, key
        include 'gisrti_comm.inc'
        call ftgkys(iunit, 'DATE', cal_time, comment, status)
        if (status .eq. 202) then
           status = 0
           cal_time = 'NOT AVAILABLE'
        end if
        call ftgkye(iunit, 'DET_SCAL', xyscale, comment, status)
        call ftgkye(iunit, 'DET_XSIZ', det_x_size, comment, status)
        call ftgkye(iunit, 'DET_XCEN', det_x_center, comment, status)
        call ftgkye(iunit, 'DETXPIX1', det_x_pix1, comment, status)
        call ftgkye(iunit, 'DET_XSCL', det_x_scale, comment, status)
        call ftgkye(iunit, 'DET_YSIZ', det_y_size, comment, status)
        call ftgkye(iunit, 'DET_YCEN', det_y_center, comment, status)
        call ftgkye(iunit, 'DETYPIX1', det_y_pix1, comment, status)
        call ftgkye(iunit, 'DET_YSCL', det_y_scale, comment, status)
        call ftgkye(iunit, 'COE_X1_A', tr1(1), comment, status)
        call ftgkye(iunit, 'COE_X1_B', tr1(2), comment, status)
        call ftgkye(iunit, 'COE_X1_C', tr1(3), comment, status)
        call ftgkye(iunit, 'COE_Y1_A', tr1(4), comment, status)
        call ftgkye(iunit, 'COE_Y1_B', tr1(5), comment, status)
        call ftgkye(iunit, 'COE_Y1_C', tr1(6), comment, status)
        call ftgkye(iunit, 'COE_X2_A', tr2(1), comment, status)
        call ftgkye(iunit, 'COE_X2_B', tr2(2), comment, status)
        call ftgkye(iunit, 'COE_X2_C', tr2(3), comment, status)
        call ftgkye(iunit, 'COE_Y2_A', tr2(4), comment, status)
        call ftgkye(iunit, 'COE_Y2_B', tr2(5), comment, status)
        call ftgkye(iunit, 'COE_Y2_C', tr2(6), comment, status)
        call ftgkye(iunit, 'RAN_SEED', ran_seed, comment, status)
        iseed = int(ran_seed)
        rti_off = 0.0
        rti_size = 0
        if (status .eq. 0) then
           rti_size = 256
           call ftgkye(iunit, 'RTI_OFF', rti_off, comment, status)
           if (status .eq. 202) then 
              status = 0
              rti_off = 119.0
           end if
        end if
        det_x_offset = det_x_center * det_x_scale
        det_y_offset = det_y_center * det_y_scale
        if (status .eq. 0) then
           if (pos_meth .eq. FLF) then
              do i= 1, 6
                 tr(i) = tr2(i)
              end do
           else
              do i= 1, 6
                 tr(i) = tr1(i)
              end do
           end if
           num = tr(6)*tr(2) - tr(3)*tr(5)
           if (num .ne. 0.0) then
              itr(1) = (tr(4)*tr(3) - tr(1)*tr(6)) / num
              itr(2) = tr(6) / num
              itr(3) = -tr(3) / num
              itr(4) = (tr(1)*tr(5) - tr(4)*tr(2)) / num
              itr(5) = -tr(5) / num
              itr(6) = tr(2) / num
           else
              call fcerr (
     &             ' ERROR: PARSE_CAL_DATA: Determinant is singular')
              status = 1
              itr(1) = det_x_offset
              itr(2) = det_x_scale
              itr(3) = 0.0
              itr(4) = det_y_offset
              itr(5) = 0.0
              itr(6) = det_y_scale
           end if
        end if
        end
        subroutine oooo (data_name, frf_name, creator, iunit, 
     &       detect, pos_det, timecol, rawxcol, rawycol, chipcol, 
     &       detxcol, detycol, skyxcol, skyycol, phacol, picol, histry, 
     &       verbose,status)
        implicit none
        character*(*) data_name, frf_name, creator, detect, pos_det
        character*(*) rawxcol, rawycol, chipcol, detxcol, detycol
        character*(*) skyxcol, skyycol, phacol, picol, timecol
        integer iunit, status
        logical histry, verbose
        include 'gisrti_defs.inc'
        integer xsize, ival, i, stat
        integer maxcl
        parameter (maxcl = 512)
        real fval
        double precision tscale, tzero
        integer hdtype, nrecords, xtensn, rwmode, block, rowlen
        integer vardat, fcstln, tfield, tbcol(maxcl)
        character(80) filenm, ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam, contxt, comment
        character(40) taskname
        character(15) keywd, dmode, bitrate, telescope
        logical exact
        include 'gisrti_comm.inc'
        common /task/ taskname
        tzero  = 0.0
        tscale = 1.0
        exact = .false.
        rwmode = 1
        call fcpars (data_name, filenm, xtensn, status)
        contxt = 'Unable to parse GIS data file name and extension '
        if (status .ne. 0) goto 999
        call ftopen (iunit, filenm, rwmode, block, status)
        contxt = 'Unable to open GIS data file: '//filenm
        if (status .ne. 0) goto 999
        if (xtensn .eq. -99) xtensn = 1
        contxt = 'Primary array not supported'
        if (xtensn .eq. 0) then
           status = 1
           goto 998
        end if
        call ftgkys (iunit, 'TELESCOP', telescope, contxt, status)
        contxt = 'Cannot determine telescope'// telescope
        if (status .ne. 0) goto 998
        call ftgkys (iunit, 'INSTRUME', detect, contxt, status)
        contxt = 'Cannot determine detector'
        if (status .ne. 0) goto 998
        detector = -1
        if (index(detect,'SIS0') .ne. 0) then
           dettype = SIS
           detector = SIS0
        else if (index(detect,'SIS1') .ne. 0) then
           dettype = SIS
           detector = SIS1
        else if (index(detect,'SIS2') .ne. 0) then
           dettype = SIS
           detector = SIS2
        else if (index(detect,'GIS2') .ne. 0) then
           dettype = GIS
           detector = GIS2
        else if (index(detect,'GIS3') .ne. 0) then
           dettype = GIS
           detector = GIS3
        else 
           contxt = 'INSTRUME not ASCA : ' // detect
           status = 1
           goto 998
        endif
        if (.not.(detector .eq. GIS2 .or. detector .eq. 
     &       GIS3).or.index(telescope, 'ASCA').eq.0) then
           contxt='Not a GIS PH Science File, skipping....'
           status = 1
           goto 998
        end if
        call ftgkyd(iunit, 'TSTART', tstart, comment, status)
        contxt = 'Cannot get TSTART'
        if (status .ne. 0) goto 998
        call ftgkyd(iunit, 'TSTOP', tstop, comment, status)
        contxt = 'Cannot get TSTOP'
        if (status .ne. 0) goto 998
        if (dettype .eq. GIS) then
           call ftgkyj(iunit, 'RT_LD', ival, comment, status)
           rt_off = ival
           contxt = 'Cannot get RT_LD'
           if (status .ne. 0) goto 998
           call ftgkyj(iunit, 'RT_B_CD', ival, comment, status)
           rt_scale = 1.0
           if (ival .gt. 0) rt_scale = float(2**ival)
           contxt = 'Cannot get RT_B_CD'
           if (status .ne. 0) goto 998
        end if
        call ftmrhd (iunit, xtensn, hdtype, status)
        contxt = 'Error moving to requested extension'
        if (status .ne. 0) goto 998
        if (hdtype .eq. 1) then
           call ftghtb (iunit, maxcl, rowlen, nrecords, tfield, 
     &          ttype, tbcol, tform, tunit, extnam, status)
        else if (hdtype .eq. 2) then
           call ftghbn (iunit, maxcl, nrecords, tfield, ttype,
     &          tform, tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           status = 1
           goto 998
        endif
        contxt = 'Error getting extension header info'
        if (status .ne. 0) goto 999
        call ftgkys (iunit, 'DATAMODE', dmode, contxt, status)
        contxt = 'Cannot determine DATAMODE'
        if (status .ne. 0) goto 998
        datamode = -1
        if (index(dmode,'NA') .ne. 0) datamode = NA_mode
        if (index(dmode,'PH') .ne. 0) datamode = PH_mode
        if (index(dmode,'PH2') .ne. 0) datamode = PH2_mode
        if (index(dmode,'MPC') .ne. 0) datamode = MPC_mode
        if (index(dmode,'MEMORY_CHECK') .ne. 0) datamode =MEMORY_mode
        if (index(dmode,'PCAL') .ne. 0) datamode = PCAL_mode
        if (index(dmode,'PCAL') .ne. 0) datamode = PCAL_mode
        if (index(dmode,'FAINT') .ne. 0)  datamode = FAINT_mode
        if (index(dmode,'BRIGHT') .ne. 0)  datamode = BRIGHT_mode
        if (index(dmode,'BRIGHT2') .ne. 0) datamode = BRIGHT2_mode
        if (index(dmode,'FAST') .ne. 0) datamode = FAST_mode
        if (index(dmode,'FAST2') .ne. 0) datamode = FAST2_mode
        if (index(dmode,'FRAME') .ne. 0) datamode = FRAME_mode
        if (index(dmode,'HISTOGRAM') .ne. 0) datamode = HISTOGRAM_mode
        if (index(dmode,'DARK_IMAGE') .ne. 0) datamode=DARK_IMAGE_mode
        if (index(dmode,'INTEGRATION') .ne.0)datamode=INTEGRATION_mode
        if (index(dmode,'NONOBSERVATION') .ne.0) datamode=NONOBS_mode
        if (datamode .eq. -1) then
           contxt = 'Unknown DATAMODE: ' // dmode
           status = 1
           goto 998
        endif
        if (.not.(datamode.eq.PH_mode.or.datamode.eq.PH2_mode)) then
           call fcerr('Non-imaging GIS data modes not supported')
           status = 1
           goto 998
        end if
        do i=1, tfield
           call fttscl(iunit, i, tscale, tzero, status)
        end do
        contxt = 'Error reseting TSCALE/TZERO values'
        if (status .ne. 0) goto 999
        n_events = nrecords
        call ftgcno (iunit, exact, 'RISE_TIME', rt_col, status)
        contxt = 'SP, or RT column do not exist'
        if (status .ne. 0) goto 998
        call ftgcno (iunit, exact, rawxcol, x_raw_col, status)
        call ftgcno (iunit, exact, rawycol, y_raw_col, status)
        contxt = 'ELECTRONIC X or Y column do not exist'
        if (status .ne. 0) goto 998
        call ftgcno (iunit, exact, detxcol, x_det_col, status)
        call ftgcno (iunit, exact, detycol, y_det_col, status)
        contxt = 'DETECTOR X or Y column do not exist'
        if (status .ne. 0) goto 998
        call ftgcno (iunit, exact, 'RTI', rti_col, status)
        contxt = 'RTI column do not exist'
        if (status .ne. 0) goto 998
        call ftgkyj(iunit, 'RISEBINS', ival, comment, status)
        contxt = 'Cannot get RISE_BIN or RISEBINS'
        if (status .eq. 202) then
           status = 0
           call ftgkyj(iunit, 'RISE_BIN', ival, comment, status)
        end if
        rise_size = ival
        if (status .ne. 0) goto 998
        call ftgkyj(iunit, 'RAWXBINS', ival, comment, status)
        xsize = ival
        contxt = 'Cannot get RAWXBINS'
        if (status .ne. 0) goto 998
        call ftgkyj(iunit, 'RAWYBINS', ival, comment, status)
        pos_size = ival
        contxt = 'Cannot get RAWYBINS'
        if (status .ne. 0) goto 998
        if (xsize .ne. pos_size) then
           contxt = 'WARNING: X and Y scales differ !!!'
           call fcecho (contxt)
        end if
        if (pos_size .lt. xsize) pos_size = xsize
        contxt = 'Cannot determine POS_DET'
        call ftgkys (iunit, 'POS_DET', pos_det, contxt, status)
        if (status .ne. 0) goto 998
        pos_meth = -1
        if (index(pos_det,'FLF') .ne. 0)  pos_meth = FLF
        if (index(pos_det,'POW2') .ne. 0) pos_meth = POW2
        contxt = 'Unknown POS_DET keyword walue'
        if (pos_meth .eq. -1) then
           status = 1
           goto 998
        end if
 999    continue
        return
 998    if(verbose) then
           call fcerr(contxt)
           call fcecho('    closing data file...')           
        end if
        call  ftclos (iunit, stat)
        end
        integer function cccc (x, y, rtime)
        implicit none
        include 'gisrti_defs.inc'
        real x, y
        integer rtime
        include 'gisrti_comm.inc'
        integer x0, y0
        real xp, yp, rt_inv, s, t
        if (rtime .eq. 0) then
           cccc = 0
        else
           xp = x + 0.5
           yp = y + 0.5
           x0 = max(min(int(xp), int(det_x_size-1)), 1) 
           y0 = max(min(int(yp), int(det_y_size-1)), 1) 
           s = xp - real(x0)
           t = yp - real(y0)
           rt_inv = rti_off - rt_map(x0  , y0  ) * (1-s) * (1-t)
           rt_inv = rt_inv  - rt_map(x0+1, y0  ) * s     * (1-t)
           rt_inv = rt_inv  - rt_map(x0  , y0+1) * (1-s) * t
           rt_inv = rt_inv  - rt_map(x0+1, y0+1) * s     * t
           cccc = int(float(rtime) * rt_scale + rt_off + rt_inv)         
        end if
        end
        subroutine tttt (t,year,month,day,hour,mins,sec)
        double precision t
        integer month,day,year,hour,mins
        integer i, na, nb, nc, nd, ne, ng
        real sec
        double precision jd, fd, fh, fm, f, j
        parameter (j = 2448987.5d0)
        jd = t/86400.0d0 + j
        i = int(jd+0.5D0)
        f = (jd+0.5D0)-dble(i)
        nb = i
        if (i.gt.2299160) then
           na = int((float(i)-1867216.25)/36524.25)
           nb = i + 1 + na - int(na/4)
        else
           na = i
        end if
        nc = nb + 1524
        nd = int((real(nc)-122.1)/365.25)
        ne = int(365.25*real(nd))
        ng = int(real(nc-ne)/30.6001)
        fd = f + dble(nc - ne - int(30.6001*real(ng)))
        day = int(fd)
        fh = (fd - dble(day)) * 24.0D0
        hour = int(fh)
        fm = (fh - dble(hour)) * 60.0D0
        mins = int(fm)
        sec = (fm - dble(mins)) * 60.0
        if (real(ng) .gt. 13.5) then
           month = ng - 13
        else
           month = ng - 1
        end if
        if (real(month) .gt. 2.5) then
           year = nd - 4716
        else
           year = nd - 4715
        end if
        end
        subroutine bbbb (iunit, iext, isize, matrix, status)
        implicit none
        integer maxdims
        parameter(maxdims=99)
        integer iunit, iext, isize, status
        real matrix(isize, isize)
        real nullval
        integer hdutyp, bitpix,naxis,naxes(maxdims),pcount,gcount
        integer group
        logical simple,extend,anyf
        group = 0
        call ftmahd(iunit,iext+1,hdutyp,status)
        call ftghpr(iunit,99,simple,bitpix,naxis,naxes,pcount,gcount,
     &       extend,status)
        if (naxis .eq. 2 .and. naxes(1) .eq.
     &       isize .and. naxes(2) .eq. isize) then
           call ftg2de(iunit,group,nullval,isize,isize,isize,
     &          matrix,anyf,status)
        end if
        end
        subroutine kkkk(catname,name,n)
        character*(*) catname, name
        integer n
        integer i, j, k, p
        character(80) temp
        i = len(name)
        k = 1
        do while (i.gt. 0 .and. name(i:i) .eq. ' ') 
           i = i - 1
        end do
        j = i
        call yoddle(n,k,temp)
        p=len(temp)
        do i=1,j
           catname(i:i) = name(i:i)
        end do
        do i=1,k
           catname(j+i:j+i) = temp(i:i)
        end do
        do i=j+k+1,len(catname)
           catname(i:i) = ' '
        end do
        end
        subroutine yoddle(n,k,temp)
        integer k, n        
        character*(*) temp
        if (n .eq. 0) then
           k = 1
           temp = '0'
        else
           k = int(log10(real(abs(n))))+1
           if (n .lt. 0) k = k + 1
           if (k.eq.1) then
              write(temp, '(i1)') n
           else if (k .eq. 2) then
              write(temp, '(i2)') n
           else if (k .eq. 3) then
              write(temp, '(i3)') n
           else if (k .eq. 4) then
              write(temp, '(i4)') n
           else if (k .eq. 5) then
              write(temp, '(i5)') n
           else if (k .eq. 6) then
              write(temp, '(i6)') n
           else if (k .eq. 7) then
              write(temp, '(i7)') n
           else if (k .eq. 8) then
              write(temp, '(i8)') n
           else if (k .eq. 9) then
              write(temp, '(i9)') n
           else if (k .eq. 10) then
              write(temp, '(i10)') n
           else
              write(temp, '(i12)') n
           end if        
        end if
        end

