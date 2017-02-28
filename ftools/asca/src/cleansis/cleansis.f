C*****************************************************************************c
C SELECTOR TASK:
C
C     cleansis
C
C FILE:
C
C     cleansis.f
C
C DESCRIPTION:
C
C     Program to remove single pixel artifacts from a SIS image. 
C     
C     inputs: ASCA SIS science file, clean parameters.
C           
C     output: cleaned ASCA SIS science file with a hot pixel list extension
C             appended.
C
C AUTHOR/DATE:
C
C       Eric V. Gotthelf,    Aug 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C     From the original FORTRAN version (called SISCLEAN) of April 26 1993.
C     Developed so I could analyse ASCA SIS images. Donated to NASA's 
C     Guest Observer Facility. Turned into an FTOOL and modified to run on 
C     event files instead of images, and to append a 'HOT PIXEL' extension, 
C     July 1993. 
C
C     Added code to take into account edge of chips, EVG, Oct 10 1993.
C     Added pre-clean algorithms, EVG, Oct 11 1993.
C     Added dynamically allocated memory, EVG, April 21 1994.
C     Fixed PHA cut bug, EVG, June 15 1994.
C     Added facility to read and append multi-pass hot pixel extensions,
C           additional multi-pass info, and dirtysis option (write out
C           counts from dirty pixels only), EVG, June 28 1994.
C     Modify Probability error exit status to work better with Xselect,
C           EVG, Oct 14 1994.
C     Increased file name strings to 255 characters, EVG, April 6 1995.
C     Adding grade selection..., EVG, ???
C     Reset file name strings to 180 characters (FTOOLS), EVG, Dec 12 1995.
C 
C NOTES:
C
C     Program to remove non-poissonian artifacts from a SIS image.
C     
C     There is no right way to do this without compromising the 
C     statistical quality of the original data. 
C     
C     RESULTS DEPEND ON CHARACTERISTICS OF INPUT FIELD. 
C
C     ABSOLUTLY NO GUARANTIES, WHETHER EXPRESSED OR IMPLIED COMES 
C     WITH THIS PROGRAM!
C
C MAKE:
C
C      HOST: make -f mkhcleansis
C      IRAF: xc -c xcleansis.x cleansis.f
C            xc -p stsdas xcleansis.o cleansis.o
C                  -lmisc -lfitsio -liraf77 -o cleansis.x
C
C USAGE:
C
C      HOST: hcleansis
C      IRAF: cleansis
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
C      subroutine cleansis_params - gets parameters from environment.
C      subroutine cleansis     - run main program.
C     
C******************************************************************************

        subroutine cleans      
      
        implicit none
        
        character(180) data_name, out_name
        character(80) timecol, phacol, grdcol, skyxcol, skyycol, gflags
        character(80) rawxcol, rawycol, chipcol, detxcol, detycol
        character(40) taskname
        character(255) contxt
        
        integer bthresh, cellsize, phamin, phamax, status
        integer imsize, good, bad, chip, bgd, flag
        integer firstchip, numchips, rawx0, rawy0, MEMI(100)
        real logprob, MEMR(100)
        logical histry, verbose, summary, zero_edge, iterate, dirtysis
        double precision MEMD(100)

        equivalence (memd, memr, memi)
        
C     SET UP MAIN PROGRAM COMMON BLOCKS:
        
        common /task/ taskname
        common /mem/ memd

C     INITIALIZE VARIABLES:
        
        taskname = 'CLEANSIS_V1.8'
        status = 0
        
C     START:
        
C     ECHO TASK NAME AND VERSION NUMBER: 
       
        call fcecho (taskname) 

        dirtysis = .FALSE.

C     GET PARAMETERS FROM PARAM FILE:
        
        call cleansis_params (data_name, out_name, timecol, rawxcol, 
     &       rawycol, chipcol, detxcol, detycol, skyxcol, skyycol, 
     &       phacol, grdcol, cellsize, logprob, bthresh, phamin, phamax,
     &       gflags, histry, verbose, summary, dirtysis, zero_edge, 
     &       iterate, status)
        contxt = 'Failed to get parameters'
        if (status .ne. 0) GOTO 999

        if (verbose) 
     &       call fcecho('    allocating image arrays...')

C     GET INFORMATION ON CHIPS AND SIZES FROM DATA FILE

        call get_chip_info(data_name, rawxcol, rawycol, chipcol, 
     &                     verbose, firstchip, numchips, imsize, 
     &                     rawx0, rawy0, status)
        contxt = 'Failed to get chip info'
        if (status .ne. 0) GOTO 999
                 
C     ALLOCATE THE IMAGE ARRAYS DYNAMICALLY:

        good = 0
        bad = 0
        chip = 0
        bgd = 0
        flag = 0
           
        call udmget (numchips*imsize*imsize, 4, good, status)
        call udmget (numchips*imsize*imsize, 4, bad, status)
        call udmget (imsize*imsize, 4, chip, status)
        call udmget (imsize*imsize, 6, bgd, status)
        call udmget (imsize*imsize, 4, flag, status)

        contxt = 'Failed to get dynamic memory'
        if (status .ne. 0) GOTO 999
           
C     RUN MAIN PROGRAM:
              
        call cleansiser(data_name, out_name, timecol, rawxcol, 
     &          rawycol, chipcol, detxcol, detycol, skyxcol, skyycol,
     &          phacol, grdcol, cellsize, logprob, bthresh, phamin, 
     &          phamax, gflags, histry, verbose, summary, dirtysis, 
     &          zero_edge, iterate, imsize, rawx0, rawy0, firstchip, 
     &          numchips, memi(good), memi(bad), memi(chip), memr(bgd), 
     &          memi(flag), status)

        contxt = 'Failure in main program'
        if (status .ne. 0) GOTO 999
              
C     DEALLOCATE MEMORY:

        call udmfre (bgd, 4, status)
        call udmfre (chip, 4, status) 
        call udmfre (bad, 4, status) 
        call udmfre (good, 4, status) 

        contxt = 'Failed to release dynamic memory'
        if (status .ne. 0) GOTO 999

C     CHECK FOR ERROR:

 999    CONTINUE
           
        if (status .ne. 0) then
           call fcerr(contxt)
           if (status .ne. 1) call  fcerrm(status)
           call  fcerr('Program terminated prematurely')
        end if
        
       end
          
C******************************************************************************
C SUBROUTINE:
C     
C      cleansis_params
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
C       cleansis_params uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C
C      call cleansis_params(infile,aspect, rawxcol, rawycol, phacol
C               timcol, detxcol, detycol, cleansiscol, histry, verbose, status)
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

       subroutine cleansis_params (datafile, outfile, timecol, rawxcol, 
     &      rawycol, chipcol, detxcol, detycol, skyxcol, skyycol, 
     &      phacol, grdcol, cellsize, log_poisson, bgd_thresh, phamin, 
     &      phamax, grades, histry, verbose, summary, dirtysis, 
     &      zero_edge, iterate, status)
       
        implicit none 

        character*(*) datafile, outfile, timecol, phacol, grdcol
        character*(*) detxcol, detycol,  rawxcol, rawycol
        character*(*) chipcol, skyxcol, skyycol, grades
        real log_poisson
        integer cellsize, bgd_thresh, phamin, phamax, status
        logical histry, verbose, summary, dirtysis, zero_edge, iterate

        character(80) contxt

C     Initialize variables 

        status = 0

C     Get the name of the input FITS file
        
        call uclgst('datafile', datafile, status)
        contxt = 'Could not get DATAFILE parameter'
        if (status .ne. 0) go to 999            

C     Get the name of the input FITS file
        
        call uclgst('outfile', outfile, status)
        contxt = 'Could not get OUTFILE parameter'
        if (status .ne. 0) go to 999    
        
C     Get the cellsize parameter
        
        call uclgsi('cellsize', cellsize, status)
        contxt = 'Could not get CELLSIZE parameter'
        if (status .ne. 0) go to 999
        
C     Get the probalitity parameter
        
        call uclgsr('logprob', log_poisson, status)
        contxt = 'Could not get LOGPROB parameter'
        if (status .ne. 0) go to 999
        
C     Get the background theshold parameter
        
        call uclgsi('bthresh', bgd_thresh, status)
        contxt = 'Could not get BTHRESH parameter'
        if (status .ne. 0) go to 999

C     Get the minimum pha parameter
        
        call uclgsi('phamin', phamin, status)
        contxt = 'Could not get PHAMIN parameter'
        if (status .ne. 0) go to 999

C     Get the maximum  parameter
        
        call uclgsi('phamax', phamax, status)
        contxt = 'Could not get PHAMAX parameter'
        if (status .ne. 0) go to 999

C     Get the grade parameter
        
c        call uclgsi('grades', grades, status)
c        contxt = 'Could not get GRADES parameter'
c        if (status .ne. 0) go to 999

C     Iterate poisson clean - hidden paramter

        call uclgsb ('iterate', iterate, status)
        contxt = 'Could not get ITERATE parameter'
        if (status .ne. 0) go to 999

C     Do zero edge clean - hidden paramter

        call uclgsb ('zeroedge', zero_edge, status)
        contxt = 'Could not get ZEROEDGE parameter'
        if (status .ne. 0) go to 999

C     Do zero edge clean - hidden paramter

        call uclgsb ('dirtysis', dirtysis, status)
        contxt = 'Could not get DIRTYSIS parameter'
        if (status .ne. 0) go to 999

C     Get whether to write information to screen - hidden paramter

        call uclgsb ('verbose', verbose, status)
        contxt = 'Could not get VERBOSE parameter'
        if (status .ne. 0) go to 999

C     Get whether to write summary to screen - hidden paramter

        call uclgsb ('summary', summary, status)
        contxt = 'Could not get SUMMARY parameter'
        if (status .ne. 0) go to 999

C     Get the input ELECTRONIC X intermediate column name - hidden parameter

        call uclgst ('rawxcol', rawxcol, status)
        contxt = 'Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the input ELECTRONIC X intermediate column name - hidden parameter

        call uclgst ('rawycol', rawycol, status)
        contxt = 'Could not get RAWXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the input CHIP column name - hidden parameter
      
      call uclgst ('chipcol', chipcol, status)
      contxt = 'Could not get CHIPCOL parameter'
      if (status .ne. 0) go to 999

      if ( chipcol .eq. 'none' ) chipcol = 'NONE'
      
C     Get the input TIME column name - hidden parameter

        call uclgst ('timecol', timecol, status)
        contxt = 'Could not get TIMECOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output DETECTOR X column - hidden parameter

        call uclgst ('detxcol', detxcol, status)
        contxt = 'Could not get DETXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput DETECTOR Y column - hidden parameter

        call uclgst ('detycol', detycol, status)
        contxt = 'Could not get DETYCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the output SKY X column - hidden parameter

        call uclgst ('skyxcol', skyxcol, status)
        contxt = 'Could not get SKYXCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput SKY Y column - hidden parameter

        call uclgst ('skyycol', skyycol, status)
        contxt = 'Could not get SKYYCOL parameter'
        if (status .ne. 0) go to 999

C     Get the name of the ouput PHABIN column - hidden parameter

        call uclgst ('phacol', phacol, status)
        contxt = 'Could not get PHACOL parameter'
        if (status .ne. 0) go to 999
        
C     Get the name of the ouput GRADE column - hidden parameter

c        call uclgst ('grdcol', grdcol, status)
c        contxt = 'Could not get GRDCOL parameter'
c        if (status .ne. 0) go to 999
        
 999    continue
        
        if (status .ne. 0) call fcerr (contxt)

        end
        
C******************************************************************************
C     SUBROUTINE:
C     
C     cleansiser
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
C     AM   (March, 1999) - Updated to include checking if the total number 
C                          of events in a processed chip is not equal to 1, 
C                          to avoid division by 0.
C                            
C     
C     NOTES:
C     
C     USAGE:
C     
C     call cleansisear(data_name, cal_name, detxcol, detycol, 
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
        
        subroutine cleansiser(data_name, out_name, timecol, rawxcol, 
     &       rawycol, chipcol, detxcol, detycol, skyxcol, skyycol, 
     &       phacol, grdcol, cell_size, log_poisson, bgd_thresh, phamin, 
     &       phamax, gradestr, histry, verbose, summary, dirtysis, 
     &       zero_edge, iterate, imsize, rawx0, rawy0, firstchip, 
     &       numchips, image, bad_image, chip_image, bgd, flag_image, 
     &       status)
        
        implicit none

        character*(*) data_name, out_name, phacol, grdcol, gradestr
        character*(*) timecol, rawxcol, rawycol, chipcol
        character*(*) detxcol, detycol, skyxcol, skyycol
        real log_poisson
	integer imsize, rawx0, rawy0, firstchip, numchips
	real bgd(imsize, imsize)
        integer image(numchips, imsize, imsize)
	integer bad_image(numchips, imsize, imsize)
	integer chip_image(imsize, imsize)
        integer flag_image(imsize, imsize)
        integer cell_size, bgd_thresh, phamin, phamax, status
        logical histry, verbose
        logical zero_edge, iterate, summary, dirtysis
        
C     LOCAL VARIABLES:
        
	integer isize, maxhdr, maxcols, maxchips
	parameter (maxhdr = 100)        ! maximum cards 
	parameter (maxcols = 40)        ! maximum column size
        parameter (maxchips = 50)       ! maximum number of chips

	integer i_index, j_index, k_index, i, j, k, chip, chip_size
        integer cx_col, cy_col, ci_col, dx_col, dy_col, x_col, y_col
        integer pha_col, fix_counts, all_counts, bad_counts, hot_counts
        integer nevents, bytlen, nrows, naxis1
        integer nhdr, inull, anyf, htype, count
        integer nhotpix, cx, cy, ci
        integer ipmin, ipmax, jpmin, jpmax
        integer n, all, bad, fix, hot, rowlen, ncols, pixel
        integer cg_col, pixels(maxchips), counts(maxchips)
        integer accept(maxchips), total(maxchips), reject(maxchips)
        integer cx_a(maxcols), cy_a(maxcols), ci_a(maxcols) 
        integer cp_a(maxcols), NUM_EVENTS
        integer data_unit, out_unit
        integer stat, error, ftstat, event(maxcols)
        real    pmax, bmax, cmin
        real    badrej, percent(maxchips), totrej(maxchips)
        logical file_open, out_open, even, zero_fish, zero_improb
        logical cleansis
        character(80) contxt, ban(20)
        character(255) info

C     INITIALIZE VARIABLES:
        
        stat = 0

        bad = 0
        all = 0
        fix = 0
        hot = 0
        chip = 1
        ipmin = 7
        ipmax = 424
        jpmin = 2
        jpmax = 421
        count = 0
        error = 0
        isize = imsize
        out_unit = 2
        data_unit = 1

        out_open  = .FALSE.
        file_open = .FALSE.

        nhdr = maxhdr
        chip_size = isize

        n = cell_size
        
        cleansis = .not. dirtysis
        zero_fish = .TRUE.
        zero_improb = .TRUE.

        ban(1) = ' '
        write(ban(2),'(a44,a16)') 
     &       ' * Anomalous pixels may consist of at least ',
     &       'two populations.'
        ban(3) = ' '
        write(ban(4),'(a38,a30)') 
     &       ' 1. Persistent HOT pixels are removed ',
     &       'by comparing to the chip mean.'
        ban(5) = ' '     
        write(ban(6),'(a31,a38)') ' 2. Flickering WARM pixels are ',
     &       'removed by comparing to the cell mean.'
        ban(7) = ' '
        write(ban(8),'(a31,a36)') ' 3. In faint areas (zero bgd), ',
     &       'flickering pixels are removed with a'
        write(ban(9),'(a21)')     '    cutoff threshold.'
        ban(10) = ' '
        ban(11) = ' Hints:'
        write(ban(12),'(a33,a45)') ' Choose cell size and thresholds ',
     &       'based on the expected background and the PSF.'
        write(ban(13),'(a36,a28)')
     &       ' Use the DIRTYSIS option to examine ',
     &       'the anomalous pixel spectra.'
        ban(14) =' Try a multiple pass clean: '
        write(ban(15),'(a38,a29)') 
     &       '     Choose a PHA cut to optimize the ',
     &       'S/N of the flickering pixels,'
        ban(16) = '     then a broad band clean.'
        ban(17) = ' Be suspicious of extended source cleans.'
        write(ban(18),'(a33,a38)') ' For very bright sources you may ',
     &       'need to turn off the iteration option.'
        ban(19) = ' '
        write(ban(20),'(a36,a16)') 
     &       ' See the help page for further info ',
     &       '(fhelp cleansis)'

C     START:
        
        if (status .eq. 0) then
        
C     CHECK FOR SENSIBLE ARGUMENTS:
           
c           gradesmask=bin2int(gradestr, error)

           if (n .ne. 0) then
              if (log_poisson .ge. 0.0 .or. n .le. 1 .or. even(n)) then
                 call fcerr (' Bad clean parameter input value(s)') 
                 status = 1
                 return
              end if
              pmax = 10 ** log_poisson
              cmin = 1.0 / real(n*n-1)
              if (bgd_thresh .lt. 1) then
                 bgd_thresh  = 1
                 bmax = cmin
                 do while (bmax .gt. pmax)
                    bgd_thresh = bgd_thresh  + 1
                    bmax = (cmin * bmax)
         end do
                 bgd_thresh = max(bgd_thresh-1, 1)
              end if
           else
              if (bgd_thresh .le. 0) then
                 call fcerr (' Bad clean parameter input value(s)') 
                 status = 1
                 return
              end if
           end if
           
           if (summary) then 
              
              call fcecho(' ')
              if (cleansis) then
                 write(info,'(42A,23A)')
     &              ' PROGRAM TO MAKE AN SIS SCIENCE FILE CLEANED OF',
     &                ' ANOMALOUS PIXELS.'
              else
                 write(info,'(46A,30A)')
     &          ' PROGRAM TO MAKE A DIRTY SIS SCIENCE FILE CONTAINING',
     &                ' ANOMALOUS PIXEL COUNTS.'
              end if
              call fcecho(info)

              if (n. eq. 0) then
                 
c                 if (summary) then
                    call fcecho(' ')
                    if (cleansis) then
                       write(info, '(a,i12)') 
     &              ' Removing pixels whose value exceed (counts) : ', 
     &                      bgd_thresh
                    else
                       write(info, '(a,i12)') 
     &             ' Accepting pixels whose value exceed (counts) : ', 
     &                      bgd_thresh
                    end if
                    call fcecho(info)
c                 end if

              else

                 if (verbose) then
                    do i = 1, 20
                       info = ban(i)
c                      if (i.ne.13) call fcecho(info(1:80))
                       call fcecho(info(1:80))
                    end do
                 end if

c     intialize and echo probabilities:   

c                 if (summary) then

                    call fcecho(' ')
                    write(info, '(a,i12)') 
     &                   ' Poisson clean cell size         : ',
     &                   cell_size
                    call fcecho(info)
                    write(info, '(a,e12.3)') 
     &                   ' Poisson probability threshold   : ', 
     &                   pmax
                    call fcecho(info)
                    write(info, '(a,i12)') 
     &                   ' Zero Bgd Cutoff threshold (>)   : ',
     &                   bgd_thresh
                    call fcecho(info)
                    write(info, '(a,l12)') 
     &                   ' Iterate                         : ',
     &                   iterate
                    call fcecho(info)
                    write(info, '(a,l12)') 
     &                   ' Dirtysis                        : ',
     &                   dirtysis
                    call fcecho(info)
                    call fcecho(' ')
                    write(info, '(a,i12)') 
     &                   ' Minimum PHA value (inclusive)   : ',
     &                   phamin
                    call fcecho(info)
                    write(info, '(a,i12)') 
     &                   ' Maximum PHA value (inclusive)   : ',
     &                   phamax
                    call fcecho(info)
c                    write(info, '(a,z)') 
c     &                   ' Grade mask (hex)                : ',
c     &                   gradesmask
c                    call fcecho(info)

                    
c                 end if

              end if
              
           end if
           
C     OPEN THE OUTPUT FITS FILES:
           
           if (verbose) then
              call fcecho(' ')
              write(info, 222) 'open output  file: ', out_name(1:137)
              call fcecho (info) 
           end if

           call ffinit(out_unit, out_name, status)

C     MAKE AND CLEAN THE CHIP IMAGES:
           
           if (status .eq. 0) then 
              
              out_open = .TRUE.
              
C     OPEN DATA FILE:
              
              if (verbose) then
                 write(info, 222) 'reading data file: ', data_name(1:57)
                 call fcecho (info)
              end if
              
              call open_data_files (data_name, data_unit, out_unit, 
     &             nevents, rawxcol, rawycol, chipcol, detxcol,
     &             detycol, skyxcol, skyycol, phacol, grdcol, cx_col, 
     &             cy_col, ci_col, dx_col, dy_col, x_col, y_col, 
     &             pha_col, cg_col, rowlen, verbose, status)
              
              if (status .eq. 0) then
                 
                 file_open = .TRUE.

c     make chip image:

                 call zero_int_array(image,    
     &                               numchips*chip_size*chip_size) 
                 call zero_int_array(bad_image,
     &                               numchips*chip_size*chip_size) 
                    
                 if (verbose) call fcecho('    making chip image...')

                 i = 1
                 nrows = 1
                 ncols = maxcols
                                 
                 do while (i .lt. nevents .and. status .eq. 0)
                    
                    if (i + ncols .gt. nevents) then
                       ncols=mod(nevents,maxcols)
                       if (ncols.eq.0) ncols = maxcols
                    else
                       ncols = maxcols
                    end if

                    if ( ci_col .gt. 0 ) then
                       call ftgcvj(data_unit, ci_col, i, 1, ncols, 
     &                      inull, ci_a, anyf, status)
                    else
                       do j = 1, ncols
                          ci_a(j) = 1
                       enddo
                    endif
                    
                    call ftgcvj(data_unit, cx_col, i, 1, ncols, 
     &                   inull, cx_a,  anyf, status)
                    
                    call ftgcvj(data_unit, cy_col, i, 1, ncols,
     &                   inull, cy_a, anyf, status)

                    call ftgcvj(data_unit, pha_col, i, 1, ncols,
     &                   inull, cp_a, anyf, status)

c                    call ftgcvj(data_unit, grade_col, i, 1, ncols,
c     &                   inull, cg_a, anyf, status)
                    
                    do j = 1, ncols
                       k_index = ci_a(j) + (1-firstchip)
                       if (cp_a(j) .ge. phamin .and. 
     &                      cp_a(j) .le. phamax .and.
     &                      k_index .ge. 1 .and. 
     &                      k_index .le. numchips) then
                          i_index = min(max(cx_a(j)+(1-rawx0), 1), 
     &                                  chip_size)
                          j_index = min(max(cy_a(j)+(1-rawy0), 1), 
     &                                  chip_size)
                          image(k_index, i_index, j_index) = 
     &                         image(k_index, i_index, j_index) + 1
                          nrows = nrows + 1
                       end if

                    end do

                    i = i + ncols

                 end do

                 if (summary) then
                    call fcecho(' ')                     
                    write(info, '(a,i12)') 
     &                   ' File NEVENTS keyword value  : ',
     &                   nevents
                    if (nrows-1 .ne. nevents) call fcecho(info)
                    write(info, '(a,i12)') 
     &                   ' Total counts in chip images : ',
     &                   nrows - 1
                    call fcecho(info)
                    call fcecho(' ')                     
                 end if
                 
                 if (status .eq. 0) then 
                    
                    if (verbose) call 
     &                   fcecho('    copy bad pix array...')
                    
                    do cy=1, chip_size
                       do cx=1, chip_size                          
                          do ci=1, numchips
                             bad_image(ci, cx, cy) = image(ci, cx, cy) 
                          end do
                       end do
                    end do
                    
                    do while (chip .le. numchips .and. status .eq. 0 )
c     &                  .and. error .eq. 0)
                       
                       write(info, '(a,i2)') 'cleaning chip #', 
     &                                       (chip-(1-firstchip))
                       if (summary .or. verbose) call fcecho(info)
                       
                       ipmin = chip_size
                       ipmax = 1
                       jpmin = chip_size
                       jpmax = 1
                       do j = 1 , chip_size
                          do i = 1, chip_size
                             chip_image(i, j) = image(chip, i, j)
                             if ( chip_image(i,j) .gt. 0 ) then
                                ipmin = min(ipmin, i)
                                ipmax = max(ipmax, i)
                                jpmin = min(jpmin, j)
                                jpmax = max(jpmax, j)
                             endif
                          end do
                       end do

                       if (n. eq. 0) then
                          
                          fix_counts = 0
                          all_counts = 0
                          bad_counts = 0
                          
                          do j = 1 , chip_size
                             do i = 1, chip_size

                                all_counts=all_counts+chip_image(i, j)
                                if (chip_image(i, j) .gt. 
     &                               bgd_thresh) then
                                   bad_counts = bad_counts + 
     &                                  chip_image(i, j)
                                   chip_image(i, j) = 0
                                   fix_counts = fix_counts + 1
                                end if
                                
                             end do
                          end do
                          
                       else
                          
c                          error = 0

             NUM_EVENTS = (IPMAX-IPMIN+1)*(JPMAX-JPMIN+1)

             IF (NUM_EVENTS .EQ. 1) THEN
             WRITE(INFO, '(A,i2)' ) 
     &          'Number of events:',NUM_EVENTS
             CALL FCECHO(INFO)
             GOTO 13
             END IF

                          call clean_chip(isize, chip_size, 
     &                         chip_size, chip_image, ipmin, ipmax,
     &                         jpmin, jpmax, pmax, cell_size, summary, 
     &                         zero_fish, zero_edge, zero_improb, 
     &                         iterate, bgd_thresh, bgd, flag_image,
     &                         hot_counts, fix_counts, all_counts, 
     &                         bad_counts, error)
                          
                       end if
                       
                       do i = 1 , chip_size
                          do j = 1, chip_size
                             image(chip, i, j) = chip_image(i, j)
                          end do
                       end do
                       
                       pixels(chip) = fix_counts
                       counts(chip) = all_counts
                       reject(chip) = bad_counts
                       
                       percent(chip) = 0
                       if (counts(chip) .ne. 0) then
                          percent(chip) = 100.0 * 
     &                         real(reject(chip))/real(counts(chip))
                       end if
                       
                       bad = bad + bad_counts
                       all = all + all_counts
                       fix = fix + fix_counts

   13                  CONTINUE                        
                       chip = chip + 1
                       
                    end do

                    badrej = 0
                    if (all .gt. 0) badrej = 100.0*real(bad)/real(all)

                 else

                    call fcerr('error making chip image')
                    
                 end if
                    
                 if (error .ne. 0) then
                    info = 
     &   'NOTE: Underflow during cummulative Poisson Prob. computation'
                    call fcecho(info)
                 end if
                 
                 if (status .eq. 0) then
                    
                    if (summary) then
                       
                       call fcecho(' ')                       
                       write(info, '(a,i12)') 
     &                      ' Number of pixels rejected           : ', 
     &                      fix
                       call fcecho(info) 
                       write(info, '(a,i12)') 
     &                      ' Number of (internal) image counts   : ', 
     &                      all
                       call fcecho(info) 
                       write(info, '(a,i12,1x,f6.2)') 
     &                      ' Number of image cts rejected (N, %) : ',
     &                      bad, badrej
                       call fcecho(info) 
                       call fcecho(' ') 
                       write(info, 4) 
     &         '         By chip   : ', (k-(1-firstchip), k=1,numchips)
                       call fcecho(info) 
                       call fcecho(' ')
                       write(info, 4) 
     &         ' Pixels rejected   : ', (pixels(k), k=1,numchips)
                       call fcecho(info) 
                       call fcecho(' ')
                       write(info, 4) 
     &         ' Image counts      : ', (counts(k), k=1,numchips)
                       call fcecho(info) 
                       write(info, 4) 
     &         ' Image cts rejected: ', (reject(k), k=1,numchips)
                       call fcecho(info) 
c                       call fcecho(' ')
                       write(info, 5) 
     &         ' Image cts rej (%) : ', (percent(k), k=1,numchips)
                       call fcecho(info)                        
 4                     format(a, 17(1x, i12))
 5                     format(a, 17(1x, f12.2))
                       
                    end if

                    if (status .eq. 0) then 
                       
                       if (verbose) then
                          call fcecho(' ')
                          call fcecho('    filtering data...')
                       end if
                       
                       do j = 1, numchips
                          accept(j) =0
                          total(j) = 0
                       end do
                          
                       i = 0
                       nrows = 1
                       do while (nrows .le. nevents .and. 
     &                      status.eq.0)
                          
                          if ( ci_col .gt. 0 ) then
                             call ftgcvj(data_unit, ci_col, nrows, 
     &                            1, 1, inull, ci, anyf, status)
                             ci = ci + (1-firstchip)
                          else
                             ci = 1
                          endif
                          
                          call ftgcvj(data_unit, cx_col, nrows,
     &                         1, 1, inull, cx,  anyf, status)
                          cx = cx + (1-rawx0)
                          
                          call ftgcvj(data_unit, cy_col, nrows,
     &                         1, 1, inull, cy, anyf, status)
                          cy = cy + (1-rawy0)
                          
                          if (ci .ge. 1 .and. ci .le. numchips .and.
c     &                         iand(cg,grademask) .ne. 0 .and.
     &                         cx .gt. 0 .and. cx .le. chip_size .and. 
     &                         cy .gt. 0 .and. cy .le. chip_size ) then
                             
                             total(ci) = total(ci) + 1
                             pixel = bad_image(ci,cx,cy)
                             
                             if (.not.((pixel.ne.image(ci,cx,cy) 
     &                            .or. image(ci,cx,cy) .eq. -1)) 
     &                            .and. status .eq. 0) then
                                
                                if (cleansis) then
                                   
c     if (image(ci, cx, cy) .ne. 0 .and. status .eq. 0) then
                                   
                                   accept(ci) = accept(ci) + 1
                                   
                                   call ftgtbb(data_unit, nrows, 1,
     &                                  rowlen, event, status)
                                   call ftptbb(out_unit, i+1, 1, 
     &                                  rowlen, event, status)
                                   
                                   i = i + 1
                                
                                end if

                             else 
                                
                                if (dirtysis) then
                                   
                                   accept(ci) = accept(ci) + 1
                                   
                                   call ftgtbb(data_unit, nrows, 1,
     &                                  rowlen, event, status)
                                   call ftptbb(out_unit, i+1, 1, 
     &                                  rowlen, event, status)
                                   
                                   i = i + 1
                                   
                                end if
                                
                             end if
                             
                          end if
                          
                          nrows = nrows + 1
                          
                       end do
                       
                       do k = 1, numchips
                          totrej(k) = 0.0
                          if (total(k) .gt. 0) 
     &                         totrej(k) = 100.0*
     &                         (1.0-(real(accept(k))/real(total(k))))
                       end do

                       if (status .eq. 0) then 
                          
                          if (summary) then
                             
                             call fcecho(' ')
                             write(info, 4)
     &                            ' Total counts      : ',
     &                            (total(k), k=1,numchips)
                             call fcecho(info)
                             write(info, 4)
     &                            ' Total cts rejected: ',
     &                            (total(k)-accept(k), k=1,numchips)
                             call fcecho(info)
c     call fcecho(' ')
                             write(info, 5)
     &                            ' Total cts rej (%) : ',
     &                            (totrej(k), k=1,numchips)
                             call fcecho(info)
                             call fcecho(' ')
                             if (cleansis) then
                                write(info, '(a,i12)')
     &                       ' Number of clean counts accepted  : ', i
                             else
                                write(info, '(a,i12)')
     &                       ' Number of dirty counts accepted  : ', i
                             end if
                             call fcecho(info)
                             
                          end if
                          
                          if (verbose) then 
                             call fcecho(' ')
                             call fcecho('    writing history cards...')
                          end if
                          
                          call write_history(out_unit, n, nevents, 
     &                         log_poisson, bgd_thresh, phamin, phamax, 
     &                         iterate, zero_edge, firstchip, numchips,
     &                         counts, pixels, reject, percent, total, 
     &                         accept, dirtysis,status)
                          
                          if (status .eq. 0) then 
                             
                             if (verbose) then
                                call fcecho('    copying extensions...')
                             end if
                             
C     reset the length of the extension to the proper length:

                             call ftgkyj (out_unit, 'NAXIS1', naxis1, 
     &                            contxt, status)
                             call ftmkyj (out_unit, 'NAXIS2', i, 
     &                            contxt, status)
                             bytlen = naxis1 * i
                             call ftddef (out_unit, bytlen, status)
                             
                             if (status .eq. 0) then
                                
C     copy the remaining extensions, update hotpix ext if it exist:
   
                                ftstat = 0
                                nhotpix = 0
                                do while (ftstat .eq. 0 .and. 
     &                               status .eq. 0)
                                   call ftmrhd (data_unit, 1, htype, 
     &                                  ftstat)
                                   if (ftstat .eq. 0) then
                                      call ftgkys(data_unit,'EXTNAME ', 
     &                                     info, contxt, status)
                                      if (info(1:10).eq.'HOT_PIXELS')
     &                                     then
                                         call ftgkyj (data_unit, 
     &                                        'NAXIS2', nhotpix,
     &                                        contxt, status)
                                         call ftcrhd (out_unit, 
     &                                        status)
                                         call ftcopy (data_unit, 
     &                                        out_unit, 0, status)
                                         if (verbose) call fcecho
     &                             ('    writing out hot pixs...')
                                         call write_badpix_data 
     &                                        (out_unit, firstchip, 
     &                                        numchips, chip_size, 
     &                                        rawx0, rawy0, bad_image, 
     &                                        image, nhotpix, nrows, 
     &                                        status)
                                         call reset_badpix_ext(out_unit, 
     &                                        nrows, status)
                                         call write_history(out_unit, n, 
     &                                        nevents, log_poisson, 
     &                                        bgd_thresh, phamin,phamax, 
     &                                        iterate, zero_edge,
     &                                        firstchip,numchips,
     &                                        counts,pixels,reject,
     &                                        percent,total, accept, 
     &                                        dirtysis, status)
                                      else
                                         call ftcrhd (out_unit, status)
                                         call ftcopy (data_unit, 
     &                                        out_unit, 0, status)
                                      end if
                                   end if
                                end do
                
                                if (nhotpix .eq. 0) then
                                   if (verbose) then
                                      call  fcecho
     &                                ('    writing out hot pixs...')
                                      call fcecho(' ')
                                   end if
                                   call open_badpix_ext (out_unit, 
     &                                  chipcol, status)
                                   call write_badpix_data (out_unit,
     &                                  firstchip, numchips, chip_size,
     &                                  rawx0, rawy0, bad_image, image, 
     &                                  nhotpix, nrows, status)
                                   call reset_badpix_ext(out_unit, 
     &                                  nrows, status)
                                   call write_history(out_unit, n, 
     &                                  nevents, log_poisson, 
     &                                  bgd_thresh, phamin, phamax, 
     &                                  iterate, zero_edge, firstchip, 
     &                                  numchips, counts, pixels, 
     &                                  reject, percent, total, accept, 
     &                                  dirtysis, status)
                                   if (summary) then   
                                      write(info, '(a,i12)') 
     &                         ' Number of rejected pixels        : ', 
     &                                     nrows
                                      call fcecho(info)
                                      call fcecho(' ')
                                   end if
                                else
                                   if (summary) then
                                      write(info, '(a,i12)') 
     &                         ' Total file rejected pixels       : ', 
     &                                     nrows
                                      call fcecho(info) 
                                      call fcecho(' ')
                                   end if
                                end if
                                                          
                                if (status .eq. 0) then 

C     update header NEVENTS keyword:
 

                                  if (verbose) call 
     &                      fcecho('    updating NEVENTS keywords...')
                                  
                                   call ftmahd (out_unit, 1, htype, 
     &                                  status)
                                   call ftukyj (out_unit, 'NEVENTS', 
     &                                  i, '&', status)
                                   
                                   if (status .ne. 0) then 
                                      
                                      call fcerr
     &              ('Error updating primary header NEVENTS keyword')
                                      
                                   end if
                                   
                                else
                                   
                                   call fcerr
     &                              ('Error writing hot pix extension')
                                   
                                end if
                                
                             else
                                
                                call fcerr
     &                           ('Error copying remaining extensions')
                                
                             end if
                             
                          else
                             
                             call fcerr
     &                            ('Error writing HISTORY keywords')
                             
                          end if
                          
                       else
                          
                          call  fcerr('Error resetting axis size')
                          
                       end if
                       
                    else
                       
                       call  fcerr('Error filtering data')
                       
                    end if
                    
                 else
                    
                    call  fcerr('Error processing data')
                    
                 end if
              
              else
                 
                 call fcerr('Error reading data file')
                 
              end if
              
           else
              
              call fcerr('Error opening output file (exists already?)')
              
           end if
           
        end if

C     CLOSE OPEN FILES:
        
        if (file_open) then 
           stat = 0
           if (verbose) call fcecho('    closing data file...')
           call  ftclos(data_unit, stat)
        end if
        
        if (out_open) then 
           stat = 0
           if (verbose) call fcecho('    closing clean file...')
           call  ftclos(out_unit, stat)
        end if
        
 222    format(a23, a57)
        
        end
      
C******************************************************************************
C SUBROUTINE:
C
C      open_data
C
C DESCRIPTION:
C
C      opens and gets information from the event file
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  April 1993
C       ASTRO-D GOF, GSFC
C
C******************************************************************************

	subroutine open_data_files (data_name, iunit, ounit, 
     &       nrecords, cx_name, cy_name, ci_name, dx_name, dy_name, 
     &       x_name, y_name, pha_name, grd_name, cx_col, cy_col, 
     &       ci_col, dx_col, dy_col, x_col, y_col, pha_col, cg_col,
     &       rowlen, verbose, status)

        implicit none

        character*(*) data_name
        character*(*) dx_name, dy_name, x_name, y_name
        character*(*) cx_name, cy_name, ci_name, pha_name, grd_name
        integer cx_col, cy_col, ci_col, dx_col, dy_col, x_col, y_col
	integer pha_col, cg_col, iunit, ounit, rowlen, status
        logical verbose

	integer maxcl
	parameter (maxcl = 512)

	integer hdtype, nrecords, xtensn, rwmode, block
	integer decimal, vardat, tfield, tbcol(maxcl)
	character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
	character(80) extnam, contxt, info
	logical exact

	rwmode = 0
	exact = .false.
        decimal = 2
        xtensn = 1

C     get the input file name and extension
        
	call ftopen (iunit, data_name, rwmode, block, status)
        contxt = 'Unable to open INFILE'
	if (status .ne. 0) goto 999
        
        if (verbose) then
           info = '    copying primary header to output file...'
           call fcecho (info)
        end if
        
        call ftcopy (iunit, ounit, 0, status)
        contxt = 'Error copying primary header to output file'
	if (status .ne. 0) goto 998

C     move to the correct extension

        extnam = 'EVENTS'
        hdtype = 1
        CALL ftmnhd(iunit, hdtype, extnam, 0, status)
        IF ( status .NE. 0 ) THEN
           status = 0
           hdtype = 2
           CALL ftmnhd(iunit, hdtype, extnam, 0, status)
           contxt = 'Error moving to EVENTS extension'   
	   if (status .ne. 0) goto 998
        ENDIF

C     get header depending on extension type

	if (hdtype .eq. 1) then
           call ftghtb (iunit, maxcl, rowlen, nrecords, tfield, 
     &          ttype, tbcol, tform, tunit, extnam, status)
        else if (hdtype .eq. 2) then
           call ftghbn (iunit, maxcl, nrecords, tfield, ttype, 
     &          tform, tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           goto 998
        endif


        call ftgkyj (iunit, 'NAXIS1', rowlen, contxt, status)
        
	call ftgcno (iunit, exact, x_name,   x_col, status)
	call ftgcno (iunit, exact, y_name,   y_col, status)
        call ftgcno (iunit, exact, cx_name, cx_col, status)
	call ftgcno (iunit, exact, cy_name, cy_col, status)
        call ftgcno (iunit, exact, pha_name, pha_col, status)
c        if (datamode .ne. 'FAINT') then
c     &       call ftgcno (iunit, exact, grd_name, cg_col, status)
        call ftgcno (iunit, exact, pha_name, pha_col, status)
	call ftgcno (iunit, exact, dx_name, dx_col, status)
	call ftgcno (iunit, exact, dy_name, dy_col, status)
        if ( ci_name .NE. 'NONE' ) then
   	   call ftgcno (iunit, exact, ci_name, ci_col, status)
        else
           ci_col = 0
        endif

        if (status .ne. 0) then
           contxt = 'Cannot find named coordinate columns:'
c           write(contxt, '(7(1x,a8))') cx_name, cy_name, ci_name,
c     &          dx_name, dy_name, x_name, y_name
           goto 998
        endif

c        if (verbose) call fcecho('    copying event header...')
        
        call fhcopy(iunit, ounit, status)

        return

 998    call ftclos (iunit, status)

 999    continue
        call fcerr(contxt)

	end

C******************************************************************************
C SUBROUTINE:
C
C      clean_chip
C
C DESCRIPTION:
C
C     Search and destroy statistical anomolies in an 2D image field.
C     Written so I could analyse ASCA SIS data. Donated to NASA's ASCA 
C     GOF.
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  April 1993
C       ASTRO-D GOF, GSFC
C
C******************************************************************************

        subroutine clean_chip(isize, idim, jdim, image,
     &       ipmin, ipmax, jpmin, jpmax, pmax, n, verbose, zero_fish,
     &       zero_edge, zero_improb, iterate, cutoff, bgd, flag, 
     &       nhotpix, fix, all_counts, bad_counts, error)
        
        implicit none
        
        integer isize, idim, jdim, n, cutoff, error
	real    bgd(isize, isize)
        integer ipmin, ipmax, jpmin, jpmax
        integer prefix, fix, all_counts, prebad, bad_counts, nhotpix
        integer image(isize, isize)
        integer flag(isize, isize)
        logical zero_fish, zero_edge, zero_improb, iterate, verbose

        integer i, j, ii, jj, iii, jjj
        integer pixels, iter, max_count, npix
        integer z
        real    img, bac, pmax, pixel_prob, ft_gammq
        real    source, impfac, zmax
        character(80) info

c     start:
        
c     get total counts in image:
        
        fix = 0
        zmax = 0
        prefix = 0
        impfac = 320.0
        max_count = 0
        all_counts = 0
        bad_counts = 0

        do j = 1 , idim
           do i = 1, jdim
              if (i .lt. isize .or. j .lt. isize) then
                 z = image(i, j)
                 all_counts =  all_counts + z
                 if (z .gt. zmax) zmax = z 
              end if
	      flag(i, j) = 0	
           end do
        end do

        if (all_counts .eq. 0) return

        if (zero_improb) then
           prebad = 0
           prefix = 0

           pixels = (ipmax-ipmin+1)*(jpmax-jpmin+1)

c     remove hot pixels

           do j = 1 , idim
              do i = 1, jdim
                 img = real(image(i,j))
                 bac = impfac * real(all_counts)/real(pixels-1)
                 pixel_prob = 1.0 - ft_gammq(img+1.0, bac, error)
                 if (pixel_prob .lt. pmax) then
                    image(i,j) = 0
                    flag(i,j) = -1
                    prefix = prefix + 1
                    prebad = prebad + nint(img)           
                 end if
              end do
           end do

           fix = fix + prefix
           bad_counts = bad_counts + prebad
           if (verbose) then 
              write(info, '(a41,3x,i12,i12)') 
     &             ' Hot pixels & counts                   : ', 
     &             prefix, prebad
              call fcecho(info)
           end if


        end if

        if (zero_fish) then
           iter = 1
           prefix = -1
           do while(prefix .ne. 0 .and. error .eq. 0)
              prebad = 0
              prefix = 0
              do j = jpmin, jpmax
                 do i = ipmin, ipmax
                    img = 0
                    npix = 0
                    do jj=1,n
                       do ii=1,n
                          iii=min(max(i+ii-n/2-1,ipmin),ipmax)
                          jjj=min(max(j+jj-n/2-1,jpmin),jpmax)
	                  if ( flag(iii,jjj) .GE. 0 ) then  
                             img=img+real(image(iii,jjj))
                             npix = npix + 1
                          endif
                       end do
                    end do
                    bgd(i,j) = real(img-image(i,j))/real(npix-1)
                 end do
              end do
c     search and remove statistical anomolies

              do j = jpmin, jpmax
                 do i = ipmin, ipmax
                    img = real(image(i,j))
                    bac = bgd(i,j)
                    if (img .gt. bac) then
                       if (.not.(bac.eq.0.0.and.img.lt.cutoff)) then
                          pixel_prob = 1.0 - 
     &                         ft_gammq(img+1.0, bac, error)
                          if (pixel_prob .lt. pmax) then   
                             image(i,j) = 0
                             flag(i,j) = -1
                             prefix = prefix + 1
                             prebad = prebad + nint(img)
                          end if
                       end if
                    end if                    
                 end do
              end do 
              if (prefix .ne. 0) then
                 fix = fix + prefix
                 bad_counts = bad_counts + prebad
                 if (verbose) then 
                    write(info, '(a41,i3,i12,i12)') 
     &                   ' Flickering pixels iter, pixels & cnts : ', 
     &                   iter, prefix, prebad
                    call fcecho(info)
                 end if
              end if
              if (.not. iterate) prefix = 0
              iter = iter + 1
           end do
        end if
             
        if (zero_edge) then           
           prefix = 0
           prebad = 0
c     zero edge pixels (optional)

           do j = jpmin, jpmax
              do i = ipmin, ipmax
                 if (j .lt. jpmin+n/2 .or. j .gt. jpmax-n/2 .or. 
     &                i .lt. ipmin+n/2 .or. i .gt. ipmax-n/2) then
                    prebad = prebad + image(i, j)
                    image(i, j) = 0
                    prefix = prefix + 1
                 end if
              end do
           end do
           fix = fix + prefix
           bad_counts = bad_counts + prebad
           if (verbose) then 
              write(info, '(a41,3x,i12,i12)') 
     &             ' Edge pixels & counts (zero edge)      : ', 
     &             prefix, prebad
              call fcecho(info)
           end if
        end if
        
c     search and remove statistically significant bad columns
c        prefix = 0
c        prebad = 0
c        do j = jpmin+n/2, jpmax-n/2
c           do i = ipmin+n/2, ipmax-n/2
c              img = real(image(i,j))
c              bac = real(bgd(i,j)) / real(n*n-1)
c              pixel_prob = 1.0 - 
c     &             ft_gammq(img+1.0, bac, error)
c              if (pixel_prob .lt. pmax) then   
c                 image(i,j) = -1
c                 prefix = prefix + 1
c                 prebad = prebad + nint(img)
c              end if
c           end do
c        end do
c        if (prefix .ne. 0) then
c           fix = fix + prefix
c           bad_counts = bad_counts + prebad
c           if (verbose) then 
c              write(info, '(a,i,i)') 
c     &             ' Significant under populated pixels   : ', 
c     &             prefix, prebad
c              call fcecho(info)
c           end if
c        end if

c 10     format(4(1x,i4), 3(2x,f))
c 20     format(4(5x,a), 3x, 3(5x,a))

        end

        subroutine zero_int_array(array, nint) 
        integer nint, array(nint)

        integer i

        do i = 1, nint
           array(i) = 0
        end do
        
        end

        logical function even(n) 
        integer n

        if (mod(n, 2) .eq. 0) then
           even = .TRUE.
        else
           even = .FALSE.
        end if
        
        end

	subroutine open_badpix_ext (ounit, chipcol, status) 
 
C	SUBROUTINE ARGUMENTS: 
 
	implicit none 
 
	integer ounit, status

        character chipcol*80

C	LOCAL VARIABLES: 
        
C	FITS DECLARATIONS: 

        integer nrec, tfield
        integer frow, vardat, pcount, gcount
        character(20) ttype(20), tform(20), tunit(20)
        character(80) extname
        
        frow = 1
        nrec = 1

        extname = 'HOT_PIXELS'
        vardat = 0
        pcount = 0
        gcount = 0

        tfield = 0
        IF ( chipcol .NE. 'NONE' ) THEN
           tfield = tfield + 1
           ttype(tfield) = chipcol(1:20)
           tform(tfield) = '1I'
           tunit(tfield) = 'NUMBER'
        ENDIF

        tfield = tfield + 1
        ttype(tfield) = 'RAWX'
        tform(tfield) = '1I'
        tunit(tfield) = 'ADC_UNIT'

        tfield = tfield + 1
        ttype(tfield) = 'RAWY'
        tform(tfield) = '1I'
        tunit(tfield) = 'ADC_UNIT'

        tfield = tfield + 1
        ttype(tfield) = 'COUNTS'
        tform(tfield) = '1J'
        tunit(tfield) = 'COUNTS'

c     create new xtension, write out header and data table definition:
        
        call ftcrhd (ounit, status)
        call ftpbnh (ounit, nrec, tfield, ttype, tform, tunit, extname, 
     &       vardat, status)
        call ftbdef (ounit, tfield, tform, vardat, nrec, status)

 	return 
 
	end 
        
        subroutine fhcopy(iunit, ounit, status)
        
C     Routine to replicate a binary extension header from an existing file 
C     to a new extension. The binary extension is defined and made ready for
C     copying data records.
C     
C     Eric Gotthelf (ASCA GOF). 
C     
C     iunit, ounit: input and output FITS extension file units.
C     status:       FITSIO status.
C     
        
C     SUBROUTINE ARGUMENTS: 
        
	implicit none 
        
	integer iunit, ounit, status
        
C     LOCAL VARIABLES: 
        
C     FITS DECLARATIONS: 
        
        integer i, nrec, tfield
        integer frow, vardat
        character(20) tform(20)
        character(80) card
        
        frow = 1
        nrec = 1
        vardat = 0
        
C     create a new extension in the output FITS extension and copy header

        
        call ftgkns(iunit, 'TFORM',1, 20, tform, tfield, status)
        
        i = 1
        call ftcrhd(ounit, status)
        call ftgrec(iunit, i, card, status)
        do while (card(1:8) .ne. 'END     ' .and. status .eq. 0)
           i = i + 1
           call ftprec(ounit, card, status)
           call ftgrec(iunit, i, card, status)
        end do

        call ftbdef (ounit, tfield, tform, vardat, nrec, status)
        
        end 
        
        subroutine write_history(iunit, n, nevents, logprob, bthresh, 
     &       phamin, phamax, iterate, zero_edge, firstchip, numchips, 
     &       counts, pixels, reject, percent, total, accept, dirtysis, 
     &       status)

        implicit none

        integer iunit, n, nevents, bthresh, phamin, phamax, status
        integer firstchip, numchips
        integer counts(*), pixels(*), reject(*), total(*), accept(*)
        real logprob, percent(*)
        logical iterate, zero_edge, dirtysis

        integer k, all, fix, tot, rej, acc
        real irej, trej, totrej(4)
        character taskname*40, text1*8, text2*8, text3*8
        character info*255
        
        common /task/ taskname

        write(info,'(a40,a40)') 
     &       '----------------------------------------',
     &       '----------------------------------------'
        call ftphis (iunit, info(1:70), status)

        if (n .eq. 0) then
           
           write(info,'(a14, a19,i12)') taskname, '  : COUNT CUTOFF = ', 
     &          bthresh
           
        else
           
           write(info,'(a14, a14, i5, a13, i5, a12, f6.2)') taskname, 
     &          '  : CELLSIZE = ', n, ', BTHRESH   = ', bthresh, 
     &          ', LOG PROB = ', logprob
           call ftphis (iunit, info(1:70), status)
           
           text1 = '   NO'
           text2 = '   NO'
           text3 = '   NO'
           if (iterate)   text1 = '  YES'
           if (zero_edge) text2 = '  YES'
           if (dirtysis)  text3 = '  YES'
           write(info,'(a14, a14, a5, 2(a13, a5))') taskname, 
     &          '  : ITERATE  = ', text1,  ', ZERO_EDGE = ', text2,
     &          ', DIRTYSIS  = ', text3
        end if
        call ftphis (iunit, info(1:70), status)
        write(info,'(a14, a14, i5, a13, i5)') taskname, 
     &       '  : PHAMIN   = ', phamin, ', PHAMAX    = ', phamax 
        call ftphis (iunit, info(1:70), status)
        
        all = 0
        rej = 0
        acc = 0
        tot = 0
        fix = 0
        do k = 1, numchips
           fix = fix + pixels(k)
           all = all + counts(k)
           rej = rej + reject(k)
           tot = tot + total(k)
           acc = acc + accept(k)
           totrej(k) = 0
           if (total(k) .gt. 0) totrej(k) = 
     &       100.0 * ( 1.0 - (real(accept(k))/real(total(k))) )
        end do
        irej = 0.0
        trej = 0.0
        if (all .gt. 0) irej = 100.0 * real(rej) / real(all)
        if (tot .gt. 0) trej = 100.0 * (1.0-real(acc)/real(tot))

        info = ' '
        call ftphis (iunit, info(1:70), status)
        write(info, '(a,i12)') 
     &       'Number of counts in file           : ', nevents
        call ftphis (iunit, info(1:70), status)

        write(info, '(a,i12)') 
     &       'Number of counts in image          : ', all
        call ftphis (iunit, info(1:70), status)
        
        write(info, '(a,i12)') 
     &       'Number of pixels rejected          : ', fix
        call ftphis (iunit, info(1:70), status)
        
        write(info, '(a,i12,f10.2)') 
     &       'Number of image cnts rej. (N, %)   : ', rej, irej
        call ftphis (iunit, info(1:70), status)
        
        write(info, '(a,i12,f10.2)') 
     &       'Number of total cnts rej. (N, %)   : ', tot-acc, trej
        call ftphis (iunit, info(1:70), status)

        info = ' '
        call ftphis (iunit, info(1:70), status)        
        write(info, 4) 
     &       '        By chip      : ', (k-(1-firstchip), k=1,numchips)
        call ftphis (iunit, info(1:70), status)

        info = ' '
        call ftphis (iunit, info(1:70), status)        
        write(info, 4) 
     &       'Pixels rejected      : ', (pixels(k), k=1,numchips)
        call ftphis (iunit, info(1:70), status)
        write(info, 4) 
     &       'Counts in image      : ', (counts(k), k=1,numchips)
        call ftphis (iunit, info(1:70), status)
        write(info, 4) 
     &       'Image cnts rejected  : ', (reject(k), k=1,numchips)
        call ftphis (iunit, info(1:70), status)
        info = ' '
        call ftphis (iunit, info(1:70), status)
        write(info, 5) 
     &       'Reject image cts %   : ', (percent(k), k=1,numchips)
        call ftphis (iunit, info(1:70), status)
        write(info, 5) 
     &       'Reject total cts %   : ', (totrej(k),  k=1,numchips)
        call ftphis (iunit, info(1:70), status)
        
 4      format(a, 17(1x, i12))
 5      format(a, 17(1x, f10.2))

        end

        subroutine write_badpix_data (unit, firstchip, numchips, 
     &       chip_size, rawx0, rawy0, bad_image, image, startrow, 
     &       nrows, status)
        
        implicit none
        
        integer unit, firstchip, numchips, chip_size, startrow
        integer rawx0, rawy0
        integer nrows, status
        integer bad_image(numchips, chip_size, chip_size)
        integer image(numchips, chip_size, chip_size)
        
        integer cy, cx, ci, badpix, count
        
        nrows = startrow
        

        do cy=1, chip_size
           do cx=1, chip_size
              do ci=1, numchips
                 
                 count  = image(ci,cx,cy)
                 badpix = bad_image(ci,cx,cy) 
                 if ( (badpix.ne.count .or. count.eq.-1) .and. 
     &                status .eq. 0) then
                    
                    nrows = nrows + 1

                    if ( numchips .eq. 1 ) then
                       call ftpclj(unit, 1, nrows, 1, 1, cx-(1-rawx0), 
     &                             status)
                       call ftpclj(unit, 2, nrows, 1, 1, cy-(1-rawy0), 
     &                             status)
                       call ftpclj(unit, 3, nrows, 1, 1, badpix, status)
                    else
                       call ftpclj(unit, 1, nrows, 1, 1, 
     &                          ci-(1-firstchip), status)
                       call ftpclj(unit, 2, nrows, 1, 1, cx-(1-rawx0),
     &                             status)
                       call ftpclj(unit, 3, nrows, 1, 1, cy-(1-rawy0),
     &                             status)
                       call ftpclj(unit, 4, nrows, 1, 1, badpix, status)
                    endif

                 end if
                 
              end do
           end do
        end do

        end

        subroutine reset_badpix_ext(unit, nrows, status)
        implicit none
        integer unit, nrows, status

        integer naxis1, bytlen
        character(80) contxt


        call ftgkyj (unit, 'NAXIS1', naxis1, contxt, status)
        call ftmkyj (unit, 'NAXIS2', nrows, '&', status)
        bytlen = naxis1 * nrows
        call ftddef (unit, bytlen, status)
        
        end

        subroutine get_chip_info(data_name, rawxcol, rawycol, chipcol, 
     &                           verbose, firstchip, numchips, imsize, 
     &                           rawx0, rawy0, status)
        implicit none
        integer firstchip, numchips, imsize, rawx0, rawy0, status
        character data_name*180, chipcol*80, rawxcol*80, rawycol*80
        character keynam*8, comment*80, contxt*255
        logical verbose

        integer unit, block, hdutype, extver, icol, ierr
        integer lastchip, imlim(2)

        integer fcstln
        external fcstln

c Open the input file

        call ftgiou(unit, status)
	call ftopen (unit, data_name, 0, block, status)
        contxt = 'Unable to open INFILE'
	if (status .ne. 0) goto 999

c Move to the events extension

        hdutype = 2
        extver = 0
        call ftmnhd(unit, hdutype, 'EVENTS', extver, status)
        contxt = 'Unable to move to EVENTS extension'
	if (status .ne. 0) goto 998

c Find the column number for RAWXCOL

        call ftgcno(unit, .FALSE., rawxcol, icol, status)
        contxt = 'Unable to find column for rawx'
	if (status .ne. 0) goto 998

        if (verbose) then
           write(contxt,'(a,a,a,i4)') 'Column for ', 
     &       rawxcol(:fcstln(rawxcol)), ' is ', icol
           call fcecho(contxt)
        ENDIF

c Get the TLMIN and TLMAX for this column

        call ftkeyn('tlmin', icol, keynam, status)
        call ftgkyj(unit, keynam, imlim(1), comment, status)
        contxt = 'Unable to find TLMIN for rawx column'
        if (status .ne. 0) goto 998
        call ftkeyn('tlmax', icol, keynam, status)
        call ftgkyj(unit, keynam, imlim(2), comment, status)
        contxt = 'Unable to find TLMAX for rawx column'
        if (status .ne. 0) goto 998

        if (verbose) then
           write(contxt,'(a,i6,1x,i6)') 'TLMIN/MAX are ', 
     &       imlim(1), imlim(2)
           call fcecho(contxt)
        ENDIF

c calculate the image size and save the first x pixel number

        imsize = imlim(2) - imlim(1) + 1
        rawx0 = imlim(1)

c Find the column number for RAWYCOL

        call ftgcno(unit, .FALSE., rawycol, icol, status)
        contxt = 'Unable to find column for rawy'
	if (status .ne. 0) goto 998

        if (verbose) then
           write(contxt,'(a,a,a,i4)') 'Column for ', 
     &       rawycol(:fcstln(rawycol)), ' is ', icol
           call fcecho(contxt)
        ENDIF

c Get the TLMIN and MAX for this column 

        call ftkeyn('tlmin', icol, keynam, status)
        call ftgkyj(unit, keynam, imlim(1), comment, status)
        contxt = 'Unable to find TLMIN for rawy column'
        if (status .ne. 0) goto 998
        call ftkeyn('tlmax', icol, keynam, status)
        call ftgkyj(unit, keynam, imlim(2), comment, status)
        contxt = 'Unable to find TLMAX for rawy column'
        if (status .ne. 0) goto 998

        if (verbose) then
           write(contxt,'(a,i6,1x,i6)') 'TLMIN/MAX are ', 
     &       imlim(1), imlim(2)
           call fcecho(contxt)
        ENDIF

c set the image size to the larger of the X and Y sizes and save the first
c y pixel number

        imsize = MAX(imsize, imlim(2) - imlim(1) + 1)
        rawy0 = imlim(1)

c If the chipcol has been set to none then assume that there is only one
c chip

        IF ( chipcol .EQ. 'NONE' .OR. chipcol .EQ. 'none' ) THEN

           firstchip = 1
           numchips = 1

           IF(verbose) THEN
              call fcecho('No chip name given so assuming single chip')
           ENDIF

        ELSE

c Find the column number for CHIPCOL

           call ftgcno(unit, .FALSE., chipcol, icol, status)
           contxt = 'Unable to find column for chip name'
	   if (status .ne. 0) goto 998

           if (verbose) then
              write(contxt,'(a,a,a,i4)') 'Column for ', 
     &          chipcol(:fcstln(chipcol)), ' is ', icol
              call fcecho(contxt)
           ENDIF

c Get the TLMIN and TLMAX for this column

           call ftkeyn('tlmin', icol, keynam, status)
           call ftgkyj(unit, keynam, firstchip, comment, status)
           contxt = 'Unable to find TLMIN for chip column'
           if (status .ne. 0) goto 998
           call ftkeyn('tlmax', icol, keynam, status)
           call ftgkyj(unit, keynam, lastchip, comment, status)
           contxt = 'Unable to find TLMAX for chip column'
           if (status .ne. 0) goto 998

           if (verbose) then
              write(contxt,'(a,i6,1x,i6)') 'TLMIN/MAX are ', 
     &          firstchip, lastchip
              call fcecho(contxt)
           ENDIF

c Calculate the number of chips

           numchips = lastchip - firstchip + 1

        ENDIF

 998    CONTINUE
        ierr = 0
        CALL ftclos(unit, ierr)

 999    CONTINUE
        IF ( status .NE. 0 ) CALL fcerr(contxt)
           
        RETURN
        END
