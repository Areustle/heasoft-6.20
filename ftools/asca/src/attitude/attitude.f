C*****************************************************************************
C SELECTOR TASK:
C
C     attide
C
C FILE:
C
C     attitude.f
C
C DESCRIPTION:
C
C     Program to compute summary information from attitude file. 
C     
C AUTHOR/DATE:
C
C     Eric Gotthelf,    July 1993
C     ASTRO-D GOF, GSFC
C     
C MAKE:
C
C      HOST: make -f mkhattitude
C      IRAF: xc -c xattitude.x attitude.f
C            xc -p stsdas xattitude.o attitude.o -lascalin
C                  -lmisc -lfitsio -liraf77 -o attitude.x
C
C USAGE:
C
C      HOST: hattitude
C      IRAF: attitude
C
C ARGUMENTS:
C     
C PRIMARY LOCAL VARIABLES:
C
C      asp_name - input FITS file and extension number
C      verbose   - whether to write information to screen
C      status    - FITSIO status
C
C CALLED ROUTINES:
C
C      subroutine attitude_params - gets parameters from environment.
C      subroutine attitude        - run main program.
C     
C******************************************************************************

        subroutine attite

        implicit none

        integer status

        character(160) asp_name, attpath
        character(160) defATTpath, outfile
        character(160) qcol, qstat, atimecol, pointing
        character taskname*40

        real ranom_euler_phi, decnom_euler_theta, euler_psi, slewmax
        logical histry, verbose, summary, acmflag

C     SET UP MAIN PROGRAM COMMON BLOCKS:

        include 'asca_defs.inc'
        include 'asca_common.inc'

        common /task/ taskname

C     INITIALIZE VARIABLES:

        taskname = 'ATTITUDE_V0.9j'
        
C     START:
        
C     ECHO TASK NAME AND VERSION NUMBER: 
       
        call fcecho (taskname) 

C     GET PARAMETERS FROM PARAM FILE:
        
        call attitude_params (asp_name, attpath, defATTpath, outfile, 
     &       qcol, qstat, atimecol, pointing, ranom_euler_phi, 
     &       decnom_euler_theta, euler_psi, slewmax, histry, verbose, 
     &       summary, acmflag, status)

        if (status .eq. 0) then
           
C     RUN MAIN PROGRAM:
           
           call  attitudeear(asp_name, attpath, defATTpath, outfile, 
     &          qcol, qstat, atimecol, pointing, ranom_euler_phi, 
     &          decnom_euler_theta, euler_psi, slewmax, histry, 
     &          verbose, summary, acmflag, status)
           
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
C      attitude_params
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
C       attitude_params uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C
C      call attitude_params (aspfile, attpath, defATTpath,
C     &       qcol, atimecol, pointing, ranom_euler_phi,
C     &       decnom_euler_theta, euler_psi, histry, verbose, summary, 
C     &       status)
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
C      subroutine fcecho - echo message to teslewmaxal
C      subroutine fcerrm - write error message to teslewmaxal
C
C******************************************************************************

        subroutine attitude_params (aspfile, attpath, defATTpath,
     &       outfile, qcol, qstat, atimecol, pointing, ranom_euler_phi,
     &       decnom_euler_theta, euler_psi, slewmax, histry, verbose, 
     &       summary, acmflag, status)
       
        implicit none 

        character*(*) aspfile, attpath, defATTpath
        character*(*) qcol, qstat, outfile
        character*(*) pointing, atimecol
        real ranom_euler_phi, decnom_euler_theta, euler_psi, slewmax
        logical histry, verbose, summary, acmflag
        integer      status

        character(80) contxt

C Initialize variables 

        status = 0

C     Get the name of the input ATTITUDE file
        
        call uclgst('attitude', aspfile, status)
        contxt = 'Could not get GAINFILE parameter'
        if (status .ne. 0) go to 999
        
C     Get the input ATTITUDE file path
        
        call uclgst('attpath', attpath, status)
        contxt = 'Could not get attpath parameter'
        if (status .ne. 0) go to 999    

C     Get whether to write information to screen - hidden paramter
        
        call uclgst ('outfile', outfile, status)
        contxt = ' Could not get OUTFILE parameter'
        if (status .ne. 0) go to 999
        
C     Get the value of the pointing parameter
        
        call uclgst('pointing', pointing, status)
        contxt = 'Could not get POINTING parameter'
        if (status .ne. 0) go to 999
        
        if (pointing .eq. 'user' .or. pointing .eq. 'USER') then 
           
C     Get the nominal ra (or Euler phi) , phi parameter
           
           call uclgsr('ranom', ranom_euler_phi, status)
           contxt = 'Could not get PHI parameter'
           if (status .ne. 0) go to 999
           
C     Get the nominal dec (or Euler theta) , phi parameter
           
           call uclgsr('decnom', decnom_euler_theta, status)
           contxt = 'Could not get THETA parameter'
           if (status .ne. 0) go to 999
           
        else if (.not.(pointing .eq. 'att' .or. 
     &          pointing .eq. 'ATT')) then 
           
           status = 1
           contxt = 
     &          'Error parsing POINTING parameter, not recognized'
           go to 999
           
        end if
        
C     Get whether to use ACM flag to compute mean - hidden parameter
        
        call uclgsb ('acmflag', acmflag, status)
        contxt = ' Could not get ACMFLAG parameter'
        if (status .ne. 0) go to 999
        
C     Get the attitude time column name - hidden parameter
        
        call uclgst ('atimecol', atimecol, status)
        contxt = ' Could not get ATIMECOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the attitude q column name - hidden parameter
        
	call uclgst ('qcol', qcol, status)
        contxt = ' Could not get QCOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the attitude qstat column name - hidden parameter
        
	call uclgst ('qstat', qstat, status)
        contxt = ' Could not get QSTAT parameter'
        if (status .ne. 0) go to 999
        
C     Get whether to write information to screen - hidden paramter
        
        call uclgsb ('verbose', verbose, status)
        contxt = ' Could not get VERBOSE parameter'
        if (status .ne. 0) go to 999
        
C     Get whether to write information to screen - hidden paramter
        
        call uclgsb ('summary', summary, status)
        contxt = ' Could not get SUMMARY parameter'
        if (status .ne. 0) go to 999
        
C     Get the name of the default input ATTITUDE file path
        
        call uclgst('defATTpath', defATTpath, status)
        contxt = 'Could not get defATTpath parameter'
        if (status .ne. 0) go to 999    

C     Get the SLEWMIN parameter
           
           call uclgsr('slewmax', slewmax, status)
           contxt = 'Could not get SLEWMAX parameter'
           if (status .ne. 0) go to 999
           
 999    continue
        
        if (status .ne. 0) call fcerr (contxt)

        end
        
C******************************************************************************
C     SUBROUTINE:
C     
C     attitudeear
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
C     call attitudeear(data_name, cal_name, detxcol, detycol, 
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
C     subroutine fcecho - echo message to teslewmaxal
C     function   fcstln - returns index of last non-blank character
C     subroutine ftxxxx - FITSIO calls
C     
C******************************************************************************
        
        subroutine attitudeear(asp_name, att_path, defATTpath,
     &       outfile, qcol, qstat, atimecol, pointing, ra_phi_user, 
     &       dec_theta_user, psi_user, slewmax, histry, verbose, 
     &       summary, acmflag, status)
        
        implicit none

        character*(*) asp_name, att_path, defATTpath
        character*(*) qcol, qstat, outfile
        character*(*) pointing, atimecol
        real ra_phi_user, dec_theta_user, psi_user, slewmax
        logical histry, verbose, summary, acmflag
        integer status
        
C     LOCAL VARIABLES:
        
        include 'asca_defs.inc'

        integer asp_unit, out_unit, i, j
        integer stat, fcstln
        logical asp_open, out_open, fixed_asp, true, false
        character(160) info
        character(160) asp_time, att_name
        character(164) ascfile

        include 'asca_common.inc'
        
C     INITIALIZE VARIABLES:
        
        asp_unit = 4
        out_unit = 3
        
        misalign(1,1) =  0.99999732d0
        misalign(1,2) =  0.00000507d0
        misalign(1,3) =  0.00231137d0
        misalign(2,1) =  0.00000507d0
        misalign(2,2) =  0.99999034d0
        misalign(2,3) = -0.00438987d0
        misalign(3,1) = -0.00231137d0
        misalign(3,2) =  0.00438987d0
        misalign(3,3) =  0.99998772d0

        do i = 1, 3
           do j = 1, 3
              imisalign(j, i) = misalign(i, j)
           end do
        end do

        ea_phi = ra_phi_user
        ea_psi = psi_user
        ea_theta = dec_theta_user
       
        out_open = .FALSE.
        asp_open = .FALSE.
        fixed_asp = .FALSE.
        false = .FALSE.
        true = .TRUE.

C     START:

C     INITIALIZE .PAR FILE

        call uclpsb('valid', false, status)

        if (status .eq. 0) then
           
C     OPEN ATTITUDE FILE:

           if (att_path .eq. 'default' .or. att_path .eq. 'DEFAULT')  
     &          write(att_path, '(a160)') defATTpath
            
           call pathcat(att_name, att_path, asp_name)           

           if (verbose) then

              call fcecho(' ')
              write(info, 222) 
     &             '   reading attitude file: ', att_name(1:135)
              call fcecho (info) 
           
           end if

           call open_asp_file (att_name, asp_unit, asp_time, atimecol, 
     &          qcol, qstat, status)
           
           if (status .eq. 0) then
              
              asp_open = .TRUE.

C     OPEN OUTPUT FILE:

              if (.not.(outfile .eq. 'none' .or. 
     &             outfile .eq. 'NONE') ) then
                 if (outfile .eq. 'default' .or.
     &                outfile .eq. 'DEFAULT') then
                    j = fcstln(asp_name)
                    write(ascfile, '(a,a4)') asp_name(1:j),'.asc'
                 else
                    write(ascfile, '(a160)') outfile
                 end if
                 open (unit=out_unit, file=ascfile, status='unknown', 
     &                err=100)
                 out_open = .TRUE.

                 write(out_unit, '(a)') 
     &   '!       QDP file containing aspect info from attitude file:'
                 write(out_unit, '(a6,a)') 'LA OT ', asp_name(1:j)

 100             continue

                 if (out_open) then
                    if (verbose) then
                       write(info, 222) 
     &                      '   open asc  output file: ', ascfile(1:135)
                       call fcecho (info) 
                    end if
                 else
                    info = 'Error opening ascii output file. ' //
     &                   'Proceed without...'                    
                    call fcecho (info) 
                 end if
                 
              end if

C     COMPUTE MEAN ASPECT:

              if (verbose) then
                 call fcecho(' ')
                 call fcecho(
     &            ' AVERAGE ASPECT AND OFFSET FOR THIS ATTITUDE FILE:')
              end if

              tstop = astop 
              tstart = astart

              call nominal_aspect (asp_unit, out_unit, ra_phi_user, 
     &             dec_theta_user, slewmax, pointing, verbose, 
     &             out_open, acmflag, status)
              
              if (status .eq. 0) then
                 
C     WRITE SUMMARY LINE AND RESULTS TO .PAR FILE:

                 if (summary) then 
                    
                    call  fcecho(' ')
                    write (info, 1014)
                    call fcecho (info)
                    write (info, 1015) asp_name, nattitude, 
     &                   euler(1)/deg_to_rad,euler(2)/deg_to_rad,
     &                   euler(3)/deg_to_rad, raavg, decavg, 
     &                   rollavg, offsetavg, rasig, 
     &                   decsig, rollsig, offsetsig, 
     &                   sun_source/deg_to_rad
                    call fcecho (info)
                    call fcecho (' ')              
                    
                 end if

                 call uclpsr('euler1',real(euler(1)/deg_to_rad),status)
                 call uclpsr('euler2',real(euler(2)/deg_to_rad),status)
                 call uclpsr('euler3',real(euler(3)/deg_to_rad),status)
                 call uclpsr('ra_avg', raavg, status)
                 call uclpsr('dec_avg', decavg, status)
                 call uclpsr('roll_avg', rollavg, status)
                 call uclpsr('offset_avg', offsetavg, status)
                 call uclpsr('ra_sig', rasig, status)
                 call uclpsr('dec_sig', decsig, status)
                 call uclpsr('roll_sig', rollsig, status)
                 call uclpsr('offset_sig', offsetsig, status)
                 
                 if (status .eq. 0) then
                    
                    call uclpsb('valid', true, status)
                    
                 else
                    
                    call fcerr('Error writing to .par file')
                 
                 end if

              else

                 call fcerr('Error computing mean attitude')
                 
              end if
              
           else
              
              call fcerr('Error reading attitude file')
              
           end if
           
        else
           
           call fcerr('Error writing to .par file')
           
        end if
        
C     CLOSE ATTITUDE FILE:
        
        if (out_open) then 
           if (verbose) call fcecho('    closing output   file...')
           close(out_unit)
        end if

        if (asp_open) then 
           stat = 0
           if (verbose) call fcecho('    closing attitude file...')
           call  ftclos(asp_unit, stat)
        end if

 222    format(a25, a55) 

 1014   format(
     &       '       FILE         .RECORDS. EULER1 . EULER2 . EULER3 .',
     &       '    RA  .   DEC  .  ROLL  . OFFSET .  RA_SIG  .',
     &       '  DEC_SIG . ROLL_SIG . OFF_SIG . SUN-SRC< .')
 1015   format (1x,a18,1x,i8,7(1x,f8.3),4(1x,e10.3),1x,f8.3)

        end

        subroutine pathcat(fullname, pathname, filename)

        implicit none

        character*(*) fullname, pathname, filename
 
        integer i, j, k, len

        k = min (len(pathname), len(fullname)) 
        do j=1,k
           fullname(j:j) = pathname(j:j)
        end do                       
        
        i = k
        do while (i.gt. 0 .and. pathname(i:i) .eq. ' ') 
           i = i - 1
        end do
        j = i + 1
        
        if (j .gt. 1) then 

c           fullname(j:j) = '/'
c           k = index(filename,' ') - 1
           k = min(len(fullname)-j-1, len(filename))
           do i = 1, k
              fullname(i+j-1:i+j-1) = filename(i:i)
           end do

        else
           
           k = min(len(fullname), len(filename))
           do i = 1, k
              fullname(i:i) = filename(i:i)
           end do

        end if
        
        end


