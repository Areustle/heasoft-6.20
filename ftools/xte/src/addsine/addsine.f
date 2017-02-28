C ***************************************************************************
C SELECTOR TASK
C      addsine
C
C FILE:
C      addsine.f
C
C DESCRIPTION:      
C      adds a sine curve to an input light curve
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      5/26/94 - replaced UNIX FORTRAN ran routine with ft-ran2 function
C                for uniform random numbers
C      6/13/94 - implemented dynamic memory allocation
C      7/15/94 - added FPROFILE routine to add a periodic user-defined
C                 pulse profile.  Uses an ascii input.
C      10/3/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C      10/7/94 - implemented abort option for non-fitsio errors
C      10/7/94 - corrected output file to give correct start and stop
C                 times when only portion of the light curve is modified
C      4/18/96 - changed call to xftgkyd for MJDREF so as to catch
C                 MJDREFI/MJDREFF if these exist instead of MJDREF
C      
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      Intervals - time intervals to which sine is to be added
C      profile   - name of file containing pulse profile
C      period	 - period of sine curve
C      perunit   - units of the period (s=sec, d=days)
C      amplitude - amplitude of sine curve
C      zerophase - time of zero phase
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                  2 = Guassian noise)
C      sigma     - sigma value for Gaussian noise
C      iseed     - seed for random number generator
C      timename  - alternate name for time column in infile
C      ratename  - alternate name for intensity column in infile
C      errname   - alternate name for error column in infile
C      abort     - logical for non-fitsio errors
C      
C CALLED ROUTINES:
C      subroutine gsine - gets parameters from environment
C      subroutine fsine - adds sine curve to input light curve
C ************************************************************************** 

	Subroutine ADDSIE

c start with initial declarations
	character(160) infile, outfile, intervals, profile
        character(20) timename, ratename, errname
        character(1) perunit
	double precision zerophase, period
	real amplitude, sigma
	integer noise, iseed
        logical abort
        
        character(40) taskname
        common /task/ taskname

        taskname = 'addsine3.3'
        abort = .false.
        
c get the parameters from the par file
	call gsine(infile,outfile,intervals,profile,period,
     &       perunit,amplitude,zerophase,noise,sigma,iseed,
     &       timename,ratename,errname)

c Perform the Algorithm:
c   read in the FITS file, 
c   Add the sine curve to the input 
c   write the resulting light curve into a FITS file of same format as 
c	the input 
c
	call fsine(infile,outfile,intervals,profile,period,
     &       perunit,amplitude,zerophase,noise,sigma,iseed,
     &       timename,ratename,errname,abort)


c  Exit subroutine

	return
	end
C **************************************************************************
C SUBROUTINE:
C      gsine
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/15/94 - gets file name for user specified pulse profile
C      10/3/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C      
C NOTES:
C      gsine uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gsine(infile,outfile,intervals,profile,period,perunit
C                   amplitude, zerophase,noise,sigma,iseed,timename,
C                    ratename,errname)
C      
C ARGUMENTS:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      intervals - intervals of times to be analyzed (in MJD)
C      profile   - name of file containing pulse profile
C      period 	 - period of sine curve
C      perunit   - units of the period (s or d)
C      amplitude - amplitude of sine curve
C      zerophase - time of zero phase
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                   2 = Guassian Noise)
C      sigma	 - sigma for Gaussian noise (hidden)
C      iseed     - seed for random number generator
C      timename  - alternate name for time column in infile
C      ratename  - alternate name for intensity column in infile
C      errname   - alternate name for error column in infile
C      
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real*4 parameter
C      subroutine uclgst - get string parameter
C      
C ************************************************************************** 

	subroutine gsine(infile,outfile,intervals,profile,period,
     &     perunit,amplitude,zerophase,noise,sigma,iseed, timename,
     &     ratename, errname)
        

c start with the declarations
	character*(*) infile, outfile, perunit, intervals
        character*(*) profile, timename, ratename, errname
        real rzerophase, rperiod
	double precision  zerophase, period
	real amplitude, sigma
	integer noise, iseed
	character(80) context
	integer status

	status=0
c get the name of the input FITS file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the output FITS file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the min and maxt times for intervals to be analyzed
	call uclgst('intervals',intervals,status)
	if (status .ne. 0) then
	    context = 'could not get INTERVALS parameter'
	    call fcerr(context)
	    go to 999
         endif

C get the name of the file containing the pulse profile (OPTIONAL) 
        call uclgst('profile',profile,status)
	if (status .ne. 0) then
	    context = 'could not get PROFILE parameter'
	    call fcerr(context)
	    go to 999
         endif

c get the period of the sine curve 
	call uclgsr('period',rperiod,status)
	if (status .ne. 0) then
	    context = 'could not get PERIOD parameter'
	    call fcerr(context)
	    go to 999
         endif
         period = rperiod         

c      get the units of the period
         call uclgst('perunit',perunit,status)
	if (status .ne. 0) then
	    context = 'could not get PERUNIT parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the amplitude of the sine curve 
	call uclgsr('amplitude',amplitude,status)
	if (status .ne. 0) then
	    context = 'could not get AMPLITUDE parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the time of zero phase for the sine curve 
	call uclgsr('zerophase',rzerophase,status)
	if (status .ne. 0) then
	    context = 'could not get ZERO PHASE parameter'
	    call fcerr(context)
	    go to 999
         endif
         zerophase = dble(rzerophase)

c get the noise option for the sine curve 
	call uclgsi('noise',noise,status)
	if (status .ne. 0) then
	    context = 'could not get NOISE OPTION parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the SIGMA for guassian noise 
	call uclgsr('sigma',sigma,status)
	if (status .ne. 0) then
	  context = 'could not get SIGMA parameter'
	  call fcerr(context)
	  go to 999
         endif
       
c get the seed for the random number generator 
	call uclgsi('seed',iseed,status)
	if (status .ne. 0) then
	    context = 'could not get SEED parameter'
	    call fcerr(context)
	    go to 999
	endif

C get the name of the time column 
        call uclgst('timename',timename,status)
	if (status .ne. 0) then
	    context = 'could not get TIMENAME parameter'
	    call fcerr(context)
	    go to 999
         endif
C get the name of the rate column 
        call uclgst('ratename',ratename,status)
	if (status .ne. 0) then
	    context = 'could not get RATENAME parameter'
	    call fcerr(context)
	    go to 999
         endif
C get the name of the error column 
        call uclgst('errname',errname,status)
	if (status .ne. 0) then
	    context = 'could not get ERRNAME parameter'
	    call fcerr(context)
	    go to 999
         endif
        
c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end


C ***************************************************************************
C SUBROUTINE:
C      fsine 
C
C DESCRIPTION:      
C      organized subroutines which add a sine curve to an input light curve
C       and write result to a FITS file      
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/15/94 - added FPROFILE routine to add a periodic pulse profile
C      10/3/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C      10/7/94 - implemented abort option for when RATE (and ERROR) 
C                 not found
C
C NOTES:
C
C USEAGE:
C      call fsine(infile,outfile,intervals,profile,period,perunit,
C                 amplitude,zerophase,noise,sigma,iseed,timename,
C                 ratename,errname,abort)
C      
C ARGUMENTS:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      intervals - time intervals to which sine is to be added
C      profile   - name of file containing pulse profile
C      period	 - period of sine curve
C      perunit   - units of the period (s=sec, d=days)
C      amplitude - amplitude of sine curve
C      zerophase - time of zero phase
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                  2 = Guassian noise)
C      sigma     - sigma value for Gaussian noise
C      iseed     - seed for random number generator
C      timename  - alternate name for time column in infile
C      ratename  - alternate name for intensity column in infile
C      errname   - alternate name for error column in infile
C      abort     - status flag for non-fitsio errors
C      
C PRIMARY LOCAL VARIABLES:
C      absmint  - absolute minimum time in the data
C      absmaxt  - absolute maximum time in the data
C      timecol   - flag indicating presence of a time column in input file
C      tunit     - units of the time array
C      timedel   - bin size of the data
C      tzero     - TZERO value
c      runit     - units associated with the RATE column
c      time      - array of input time values
c      rate      - array of input rate (intensity) values
c      error     - array of input error values
c      time2     - array of output time values
c      rate2     - array of output rate (intensity) values
c      error2    - array of output error values
C      nintervals- number of time intervals to which sine is added
C      mintime   - array of the start times of the intervals
C      maxtime   - array of the stop times of the intervals
C      per       - sine period in units of input data TIME column
C      zphase    - time of zero phase in units of input data TIME column
C      ftstatus  - fitsio status flag
C      
C CALLED ROUTINES:
C      subroutine udmget - allocate dynamic memory
C      subroutine udmfre - free the dynamic memory
C      subroutine ftgiou - get an available logical unit for i/o
C      subroutine ftfiou - release a logical unit for i/o	
C      subroutine readlc - reads light curve from FITS file
C      subroutine wrtprim - writes primary extension of output FITS file
C      subroutine fadd   - construct and add sine curve
C      subroutine fprofile- add a periodic user-defined pulse
C      subroutine fswrite- write result to FITS extension
C      subroutine fcgrgd - gets the number of real*8 ranges and their limits

C ************************************************************************** 

	SUBROUTINE fsine(infile,outfile,intervals,profile,period,perunit,
     &     amplitude,zerophase,noise,sigma,iseed,timename,ratename,
     &     errname,abort)

	integer maxsize

	character*(*) infile, outfile, perunit, intervals, profile
        character*(*) timename, ratename, errname
        double precision zerophase, period
	real amplitude, sigma
	integer noise, iseed
        logical abort

        character(8) runit, tunit, author
        character(80) context
	double precision timedel, tzero
        double precision zphase, per, absmint, absmaxt, mint, maxt
        double precision mintime(15), maxtime(15)
	integer nintervls
	integer j, ftstatus, iunit, ounit, block, nrows, jrows
	logical inopen, outopen, negflag, timecol 

C      declare the pointers for dynamic memory arrays
        integer time, rate, error, time2, rate2, error2
        
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
C       datatype        value
C       logical         1
C       integer*2       3
C       Integer         4
C       Long Integer    5
C       Real            6
C       Double          7
C       Complex         8

c  Initialize variables
	ftstatus = 0
	call ftgiou(iunit,ftstatus)
	call ftgiou(ounit,ftstatus)
c	iunit = 15
c	ounit = 16 
	inopen = .false.
	outopen = .false.
	negflag = .false.
        maxsize = 86400

      time = 0
      time2 = 0
      rate = 0
      rate2 = 0
      error = 0
      error2 = 0
        
C      allocate the dynamic memory for the TIME array
      call udmget(maxsize,7,time,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for TIME'
         call fcerr(context)
         go to 998
      endif

      call udmget(maxsize,7,time2,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for TIME2'
         call fcerr(context)
         go to 998
      endif

      call udmget(maxsize,6,rate,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for RATE'
         call fcerr(context)
         go to 998
      endif

      call udmget(maxsize,6,rate2,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for RATE2'
         call fcerr(context)
         go to 998
      endif

      call udmget(maxsize,6,error,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for ERROR'
         call fcerr(context)
         go to 998
      endif

      call udmget(maxsize,6,error2,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for ERROR2'
         call fcerr(context)
         go to 998
      endif
	

c  Read in the data
	call readlc(iunit, infile, inopen, block, nrows, absmint, 
     &       absmaxt, timename, ratename, errname, timecol,
     &       MEMD(time), MEMR(rate), MEMR(error), timedel, runit,
     &       tunit, tzero, maxsize, abort)
        if (abort) go to 999
        
c        Parse the interval times into pairs of min & max times
        if (intervals .eq. '-') then
           nintervls = 1
           mintime(1) = absmint 
           maxtime(1) = absmaxt 
        else
           call fcgrgd(intervals, absmint, absmaxt, nintervls,
     &          mintime, maxtime)
        endif

c      Reckon times to the TIME column units
        zphase = zerophase - tzero
        if (tunit .eq. 's') zphase = 86400. * zphase
        per = period
        if (tunit .eq. 's' .and. perunit .eq. 'd')
     &       per = 86400. * per
        if (tunit .eq. 'd' .and. perunit .eq. 's')
     &       per = per / 86400.

c      Reckon bin size into seconds, if necessary
c      (re: see treatment of timedel in readlc)
        if (tunit .eq. 'd') timedel = 86400. * timedel
        
c      Open the output file and write the primary header
        author = 'ADDSINE'
        call wrtprim(iunit, ounit, inopen, outopen, outfile, author)
        
c      Go through the intervals
        do j = 1, nintervls
           mint = mintime(j) - tzero
           maxt = maxtime(j) - tzero
           if (tunit .eq. 's') then
              mint = 86400. * mint
              maxt = 86400. * maxt
           endif
        
c  Construct & add the sine curve or pulse profile
           if (profile .eq. ' ') then
              call fadd(MEMD(time), MEMR(rate), runit, MEMR(error),
     &          nrows, amplitude, per, zphase, noise, sigma, iseed,
     &          mint, maxt, timedel, jrows, MEMD(time2), MEMR(rate2),
     &             MEMR(error2))
           else
              call fprofile(MEMD(time), MEMR(rate), runit, MEMR(error),
     &          nrows, profile, amplitude, per, zphase, noise, sigma,
     &          iseed, mint, maxt, timedel, jrows, MEMD(time2),
     &          MEMR(rate2), MEMR(error2)) 
           endif
              
                 
c      Write out the results to a new FITS extension 
           call fswrite(iunit, ounit, inopen, outopen, intervals, jrows,
     &          profile, amplitude, period, perunit, zerophase, noise,
     &          sigma, timename, ratename, errname,
     &          MEMD(time2), MEMR(rate2), MEMR(error2), abort) 

        end do

c      close the input and output files
        call ftclos(iunit,ftstatus)
        call ftclos(ounit,ftstatus)
	call ftfiou(iunit,ftstatus)
	call ftfiou(ounit,ftstatus)
	
C      Dynamic Memory failures
        go to 999
998     context = '  When memory allocation fails you can:'
        call fcecho(context)
        context = '   1) try again when system load is lighter'
        call fcecho(context)
        context = '   2) try again requesting a smaller light curve'
        call fcecho(context)
        
c      General Error Handling 
999     continue
        if (ftstatus .ne. 0) call fcerrm(ftstatus)

c      Free the memory
        call udmfre(time,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for TIME'
           call fcerr(context)
        endif
        call udmfre(time2,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for TIME2'
           call fcerr(context)
        endif

        call udmfre(rate,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for RATE'
           call fcerr(context)
        endif
        call udmfre(rate2,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for RATE2'
           call fcerr(context)
        endif

        call udmfre(error,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for ERROR'
           call fcerr(context)
        endif
        call udmfre(error2,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for ERROR2'
           call fcerr(context)
        endif

	return
	end



C ***************************************************************************
C SUBROUTINE:
C      fadd
C
C DESCRIPTION:      
C      adds sine curve to input light curve, utilizing a noise option
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C      call fadd(time, rate, runit, error, nrows, amplitude,
C                period, zerophase, noise, sigma, iseed, mintime,
C                maxtime, bin, jpoints, time2, rate2, error2)
C      
C ARGUMENTS:
C      time      - array of input time values
C      rate      - array of input rate (intensity) values
C      runit     - units associated with the RATE values
C      error     - array of input error values
C      nrows     - number of points in input series
C      amplitude - amplitude of sine curve
C      period	 - period of sine curve
C      zerophase - time of zero phase
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                  2 = Guassian noise)
C      sigma     - sigma value for Gaussian noise
C      iseed     - seed for random number generator
C      mintime   - start time 
C      maxtime   - stop time
c      bin       - bin size of the data
c      jpoints   - number of points in output time series
c      time2     - array of output time values
c      rate2     - array of output rate (intensity) values
c      error2    - array of output error values
C
C PRIMARY LOCAL VARIABLES:
C      comp      - noise-free value to be added
C      
C CALLED ROUTINES:
C      function ft_ran2    - random number generator for uniform deviates
C      subroutine poisnois - adds poisson noise to input value
C      subroutine gausnois - adds gaussian noise to input value
C      
C *********************************************************************
      
	subroutine fadd(time, rate, runit, error, nrows,
     &    amplitude, period, zerophase, noise, sigma, iseed, mintime,
     &     maxtime, bin, jpoints, time2, rate2, error2)

        integer noise, nrows, iseed
	double precision time(nrows), time2(nrows), period, zerophase
	double precision mintime, maxtime, bin
	real rate(nrows), rate2(nrows), error(nrows), error2(nrows)
	real amplitude, sigma
        character*(*) runit
        
	integer i, j, jpoints
	real comp, pi, r, ft_ran2

	pi = 3.14159
        r = ft_ran2(iseed)

c        write(20,*) 'in fadd: mintime, maxtime:',mintime,maxtime
        
c      Choosing data from the desired time range,
c    compute the sine component,
c     add noise if desired,
c      and add component to original light curve
c      NB mintime and maxtime must be in same units as time(i) array
	
	i = 1
	j = 1
	do while (i .le. nrows)
 	  if (time(i) .ge. mintime .and. time(i) .le. maxtime) then
	    time2(j) = time(i)
	    comp = amplitude * dsin(2*pi*(time(i) - zerophase)/period)
	    if (noise .eq. 0 ) then
	      rate2(j) = rate(i) + comp
	      error2(j) = error(i)
            else if (noise .eq. 1) then
	      call poisnois(bin, comp, rate(i), runit, rate2(j),
     &              error(i), error2(j), iseed)
	    else if (noise .eq. 2) then
	      call gausnois(sigma, comp, rate(i), rate2(j),
     &				error(i), error2(j), iseed)
	    endif
	    j = j + 1
	  end if
	  i = i + 1
	end do
	jpoints = j - 1

        
c      write results to temporary data file
c        write(20,*) 'at end of fadd, jpoints = ',jpoints
c	write(20,100) (j,time2(j),rate2(j),error2(j), j=1,10)
c100	format(' ',i4,2x,f9.3,2x,f6.2,2x,f6.2)

	return
	end
C ***************************************************************************
C SUBROUTINE:
C      fprofile
C
C DESCRIPTION:      
C      adds a periodic pulse profile to an input light curve, utilizing
c        a noise option
C      
C AUTHOR:
C      Dr. James Lochner  7/14/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C      call fprofile(time, rate, runit, error, nrows, profile, amplitude,
C                 period, zerophase, noise, sigma, iseed, mintime,
C                maxtime, bin, jpoints, time2, rate2, error2)
C      
C ARGUMENTS:
C      time      - array of input time values
C      rate      - array of input rate (intensity) values
C      runit     - units associated with the RATE values
C      error     - array of input error values
C      nrows     - number of points in input series
C      profile   - name of file containing pulse profile
C      amplitude - amplitude of sine curve
C      period	 - period of sine curve
C      zerophase - time of zero phase
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                  2 = Guassian noise)
C      sigma     - sigma value for Gaussian noise
C      iseed     - seed for random number generator
C      mintime   - start time 
C      maxtime   - stop time
c      bin       - bin size of the data
c      jpoints   - number of points in output time series
c      time2     - array of output time values
c      rate2     - array of output rate (intensity) values
c      error2    - array of output error values
C
C PRIMARY LOCAL VARIABLES:
C      comp      - noise-free value to be added
C      nphase    - number of phase points in the pulse profile
C      phase     - input phase values for pulse profile
C      pamp      - input normalized amplitude values for pulse profile
C      ph        - calculated phase value
C      
C CALLED ROUTINES:
C      function ft_ran2    - random number generator for uniform deviates
C      subroutine poisnois - adds poisson noise to input value
C      subroutine gausnois - adds gaussian noise to input value
C      
C *********************************************************************
      
	subroutine fprofile(time, rate, runit, error, nrows, profile,
     &    amplitude, period, zerophase, noise, sigma, iseed, mintime,
     &     maxtime, bin, jpoints, time2, rate2, error2)

        integer noise, nrows, iseed
	double precision time(nrows), time2(nrows), period, zerophase
	double precision mintime, maxtime, bin
	real rate(nrows), rate2(nrows), error(nrows), error2(nrows)
	real amplitude, sigma
        character*(*) runit, profile
        
        character*(80) context
	integer i, j, jpoints, k, maxphase, punit, nphase
	real comp, r, ft_ran2, phase(50), pamp(50), ph
        logical popen

        maxphase = 50 
        punit = 18
        popen = .false.
        r = ft_ran2(iseed)

        do k = 1,maxphase
           phase(k) = -99.99
           pamp(k) = -99.99
        end do
        

c      Read the file containing the pulse profile
c      (assume it's an ascii file)
        open(unit=punit,file=profile,err=30,status='old')
        popen = .true.
        j = 1
10      do while (j .lt. maxphase) 
           read(punit,*,end=40) phase(j), pamp(j)
           j = j + 1
        end do
        if (j .ge. maxphase) then
           context = 'Number of Points in Profile exceeds maximum'
           call fcerr(context)
           go to 999
        endif
        go to 40
30      context = 'Unable to open pulse file: '//profile
        call fcerr(context)
        go to 999
40      close(punit)
        nphase = j - 1
        if (pamp(nphase) .eq. -99.99) then
           context = 'last amplitude value not read '
     &         // '- add line feed to file'
           call fcerr(context)
           go to 999
        endif
        
        if (phase(nphase) .lt. 1.0) then
           phase(nphase+1) = 1.0 + phase(1)
           pamp(nphase+1) = pamp(1)
        endif
        nphase = nphase + 1
           
c      Choosing data from the desired time range,
c    compute the sine component,
c     add noise if desired,
c      and add component to original light curve
c      NB mintime and maxtime must be in same units as time(i) array
	
	i = 1
	j = 1
	do while (i .le. nrows)
 	  if (time(i) .ge. mintime .and. time(i) .le. maxtime) then
	    time2(j) = time(i)
            ph = (time(i) - zerophase)/period
     &           - int((time(i) - zerophase)/period)
            do k = 1, nphase
               if (ph .ge. phase(k) .and. ph .lt. phase(k+1))
     &            comp = amplitude *
     &              (pamp(k) + (ph - phase(k))/(phase(k+1)-phase(k)) *
     &              (pamp(k+1) - pamp(k)))
            end do

	    if (noise .eq. 0 ) then
	      rate2(j) = rate(i) + comp
	      error2(j) = error(i)
            else if (noise .eq. 1) then
	      call poisnois(bin, comp, rate(i), runit, rate2(j),
     &              error(i), error2(j), iseed)
	    else if (noise .eq. 2) then
	      call gausnois(sigma, comp, rate(i), rate2(j),
     &				error(i), error2(j), iseed)
	    endif
	    j = j + 1
	  end if
	  i = i + 1
	end do
	jpoints = j - 1

        go to 1000
999     if (popen) close(punit)
        
1000	return
	end
C *******************************************************************************
C SUBROUTINE:
C      fswrite
C
C DESCRIPTION:      
C      write the result from ADDSINE into FITS file with same format
C        as input file.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/18/94 - Added notation for user-defined pulse profile
C      10/3/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C      10/7/94 - corrected output file to give correct start and stop
C                 times when only portion of the light curve is modified
C      
C NOTES:
C
C USEAGE:      
C      call fswrite(iunit, ounit, inopen, outopen, nrows, profile,
C                   amp, period, punit, tzeroph, noise, sigma, timename,
C                   ratename, errname, time, rate, error, abort) 
C
C ARGUMENTS:
C      iunit    - logical unit for input file
C      ounit    - logical unit for output file
C      inopen   - logical variable indicating open status of input file
C      outopen  - logical variable indicating open status of output file
C      intervals- time intervals to which sine is to be added
C      nrows    - total number of data points in output light curve
C      profile  - name of file containing pulse profile
C      amp      - amplitude of sine curve
C      period	- period of sine curve
C      punit    - units of the period (s=sec, d=days)
C      zerophase- time of zero phase
C      noise 	- noise option (0 = no noise, 1 = Poisson noise,
C                 2 = Guassian noise)
C      sigma    - sigma value for Gaussian noise
C      timename - alternate name for time column in infile
C      ratename - alternate name for intensity column in infile
C      errname  - alternate name for error column in infile
C      time     - array of output time assignments
C      rate     - array of output intensity values
C      error    - array of output uncertainties on intensity values
C      abort    - logical to abort for non-fitsio errors
C
C PRIMARY LOCAL VARIABLES:
C      ncol     - number of columns in FITS table
C      history  - string for history keywords
C      ttype    - array of column names
C      tform    - array of column data types
C      tunit    - array of column units
C      timecol  - logical indicating presence of time column
C      ratecol  - logical indicating presence of rate column
C      errcol   - logical indicating presence of error column
C      timepos  - column number of the time column
C      ratepos  - column number of the rate column
C      errpos   - column number of the error column
C      context  - run time error messages
C      ftstatus - fitsio status number
C
C CALLED ROUTINES:
C      subroutine ftmahd - absolute move to extension
C      subroutine ftgkns - get a sequence of numbered keyword values
C      subroutine ftcrhd - create a new extension at end of file
C      subroutine ftphbn - put the required keywords into a binary extension
C      subroutine xcopynoscale - copy non-required keywords
C      subroutine ftphis - put a HISTORY keyword into a header
C      subroutine ftpdat - put the DATE keyword into a header
C      subroutine ftbdef - define the structure of a binary table extension
C      subroutine ftpcld - put real*8 values into a table column
C      subroutine ftpcle - put real*4 values into a table column
C      subroutine ftclos - close a FITS file opened with ftinit or ftopen
C      subroutine ftfiou - release a logical unit for i/o	
C      subroutine fcerr  - prints message to stderror device
C      subroutine fcerrm - prints fitsio error number and message
C      function comnum   - append a numerial value to a character string
C      
C ******************************************************************************

      
      subroutine fswrite(iunit, ounit, inopen, outopen,
     &     intervals, nrows, profile, amp, period, punit, tzeroph,
     &     noise, sigma, timename, ratename, errname, time, rate,
     &     error, abort) 
      
c      declarations
      
      integer iunit, ounit, frow, felem, nrows, noise 
      double precision time(nrows), period, tzeroph
      real rate(nrows), error(nrows), amp, sigma
      character*(*) punit, profile, intervals
      character*(*) timename, ratename, errname
      
      character(80) context, history, comnum
      character(8) tform(10), ttype(10), tunit(10), extname, runit
      character(21) comment
      character(1) tcolunit, timeunit
      double precision mjdref, tstart, startf, tstop, stopf, ontime
      integer starti, stopi
      integer ftstatus, htype, extnum, pc
      integer j, varidat, timepos, ratepos, errpos
      integer startno, maxkeys, ncol, mcol
      logical inopen, outopen, timecol, ratecol, errcol
      logical abort

      extnum = 1
      pc = 0

c      write(20,100) (j,time(j),rate(j), error(j), j = 1,10)
c100   format(' ',i4,2x,f9.3,2x,f6.2,2x,f6.2)

       ftstatus=0
c      Move to the extension in the input file
      call ftmahd(iunit,2,htype,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to move to extension' 
         call fcerr(context)
         goto 999
      endif 

c      Get the column names (TTYPE) and # columns from the input file
      startno = 1
      maxkeys = 10
      call ftgkns(iunit,'TTYPE',startno,maxkeys,ttype,ncol,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get column names (TTYPE values)' 
         call fcerr(context)
         goto 999
      endif 

c      Get the column data types (TFORM) info from the input file
      call ftgkns(iunit,'TFORM',startno,maxkeys,tform,mcol,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get column data types (TFORM values)' 
         call fcerr(context)
         goto 999
      endif 

c      get the units for the columns (TUNIT) 
      call ftgkns(iunit,'TUNIT',startno,maxkeys,tunit,mcol,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get column units (TUNIT) values' 
         call fcerr(context)
         goto 999
      endif 

c      get the TIMEUNIT keyword (unit for header keywords)
      call ftgkys(iunit,'TIMEUNIT',timeunit,comment,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get timeunit value' 
         call fcerr(context)
         goto 999
      endif 
      
c      initialize the new extension
      call ftcrhd (ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to initialize new extension' 
         call fcerr(context)
         goto 999
      endif 

c      Determine column numbers in the input file of the TIME, RATE and ERROR
c        columns, if present.  For RATE, get the units on the RATE column as well
      timecol = .false.
      ratecol = .false.
      errcol = .false.
      errpos = 0
      do j = 1,ncol
            if (TTYPE(j) .eq. timename) then
               timecol = .true.
               timepos = j
               tcolunit = TUNIT(j)
            end if
            if (TTYPE(j) .eq. ratename) then
               ratecol = .true.
               ratepos = j
               runit = TUNIT(j)
            endif
            if (TTYPE(j) .eq. errname) then
               errcol = .true.
               errpos = j
            endif
         end do
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
         if (.not. ratecol) then
            abort = .true.
            context = 'Requested Intensity column not found'
            call fcerr(context)
            go to 999
         endif
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
         if ((errname .ne. 'ERROR') .and. (.not. errcol)) then
            abort = .true.
            context = 'Requested Error column not found'
            call fcerr(context)
            go to 999
         endif

c      Add a column for error, if not present and if
c      needed (noise > 0).
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
      if ((noise .gt. 0) .and. (.not. errcol)) then
         ncol = ncol + 1
         errpos = ncol
         ttype(ncol) = 'ERROR'
         tform(ncol) = 'E'
         tunit(ncol) = runit
      endif

c      write the required keywords in the new extension
      extname = 'rate'
      call ftphbn(ounit,nrows,ncol,ttype,tform,tunit,extname,pc,
     &     ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write required extension keywords'
         call fcerr(context)
         go to 999
      endif

c      copy the non-required keywords from the old file to the new
c      extension
      call xcopynoscale(iunit,ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'call to xcopynoscale unsuccessful'
         call fcerr(context)
         go to 999
      endif
      
c      add HISTORY, giving period, amplitude, zerophase and noise option
c      of the added periodic function
c      (At a future time, add the units, getting units from the TIMEUNIT
c        and RATE units keywords) 
      history = 'This Data has had a periodic function added to it'
      call ftphis(ounit,history,ftstatus)
      if (profile .eq. ' ') then
         history = ' Periodic Function: Sine Curve'
         call ftphis(ounit,history,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to sine curve notation'
            call fcerr(context)
            goto 999
         endif
      else
         history = ' Pulse Profile from input file: '//profile
         call ftphis(ounit,history,ftstatus)
      endif
      
      history = comnum(' Amplitude = ',dble(amp))
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to add Amplitude HISTORY'
         call fcerr(context)
         goto 999
      endif
      comment = comnum(' Period = ',period)
      history = comment//' '//punit
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to add Period HISTORY'
         call fcerr(context)
         goto 999
      endif
      history = comnum(' Time of Zero Phase = ',tzeroph) 
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to add Zero Phase HISTORY'
         call fcerr(context)
         goto 999
      endif

      if (noise .eq. 0) then
         history = ' No noise added to sine curve'
      else if (noise .eq. 1) then
         history = ' Poisson noise added to sine curve'
      else if (noise .eq. 2) then
         history = comnum(' Guassian noise with sigma = ',dble(sigma))
      endif
      call ftphis(ounit,history,ftstatus)
      
c      Modify the TSTART and TSTOP times, if necessary
      if (intervals .ne. '-') then
         tstart = time(1)
         tstop = time(nrows) + (time(nrows) - time(nrows-1))
         ontime = nrows * (time(2) - time(1))
         if (tcolunit .ne. timeunit) then
            if (tcolunit .eq. 's') then
               tstart = tstart / 86400.
               tstop = tstop / 86400.
               ontime = ontime / 86400.
            endif
            if (tcolunit .eq. 'd') then
               tstart = 86400. * tstart
               tstop = 86400. * tstop
               ontime = 86400. * ontime
            endif
         endif
         starti = int(tstart)
         startf = tstart - starti
         stopi = int(tstop)
         stopf = tstop - stopi

         call ftmkyd(ounit,'ONTIME',ontime,8,'&',ftstatus)
         if (ftstatus .ne .0) then
            context = 'unable to modify ONTIME value'
            call fcerr(context)
            go to 999
         endif
         call ftmkyj(ounit,'TSTARTI',starti,'&',ftstatus)
         if (ftstatus .eq. 202) then
            call ftmkyd(ounit,'TSTART',tstart,13,'&',ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to modify TSTART value'
               call fcerr(context)
               go to 999
            endif
            ftstatus = 0
         else
            if (ftstatus .ne .0) then
               context = 'unable to modify TSTARTI value'
               call fcerr(context)
               go to 999
            endif
            call ftmkyd(ounit,'TSTARTF',startf,13,'&',ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to modify TSTARTF value'
               call fcerr(context)
               go to 999
            endif
         endif
      
         call ftmkyj(ounit,'TSTOPI',stopi,'&',ftstatus)
         if (ftstatus .eq. 202) then
            call ftmkyd(ounit,'TSTOP',tstop,13,'&',ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to modify TSTOP value'
               call fcerr(context)
               go to 999
            endif
            ftstatus = 0
         else
            if (ftstatus .ne .0) then
               context = 'unable to modify TSTOPI value'
               call fcerr(context)
               go to 999
            endif
            call ftmkyd(ounit,'TSTOPF',stopf,13,'&',ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to modify TSTOPF value'
               call fcerr(context)
               go to 999
            endif
         endif
      endif
      
c      Modify the DATE keyword (FITS file creation date)
      call ftpdat(ounit,ftstatus)

c      Define data structure of new extension
      varidat = 0
      call ftbdef(ounit,ncol,tform,varidat,nrows,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to define structure of new extension' 
         call fcerr(context)
         goto 999
      endif 
      
c      write the new data
      felem = 1
      frow = 1
c      write(20,*) frow,felem,nrows
c      write(20,100) (j,time(j),rate(j),error(j), j = 1,10)
      
c      If implicit TIME, then write RATE and ERROR (if present)
      if (.not. timecol) then
         call ftpcle(ounit,ratepos,frow,felem,nrows,rate,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write rate array'
            call fcerr(context)
            goto 999
         endif
         if (errpos .gt. 0) then 
            call ftpcle(ounit,errpos,frow,felem,nrows,error,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to write error array'
               call fcerr(context)
               goto 999
            endif
         endif
         
      else
         
c      IF explicit TIME, write TIME, RATE, ERROR
         call ftpcld(ounit,timepos,frow,felem,nrows,time,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write time array' 
            call fcerr(context)
            goto 999
         endif
         call ftpcle(ounit,ratepos,frow,felem,nrows,rate,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write rate array' 
            call fcerr(context)
            goto 999
         endif
         if (errpos .gt. 0) then 
            call ftpcle(ounit,errpos,frow,felem,nrows,error,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to write error array' 
               call fcerr(context)
               goto 999
            endif
         endif
      
      endif
      
c      Finally, if necessary, return to the primary header
c        and change the TSTART and TSTOP there
      if (intervals .ne. '-') then
         call ftmahd(ounit,1,htype,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to move back to primary extension'//
     &           'in output file'
            call fcerr(context)
            go to 999
         endif

c      Get mjdref value from keyword.  If not there, leave it as zero
c      MJT 4/18/96 - using xftgkyd to find I/F pair if MJDREF not found
         mjdref = 0.0
         call xftgkyd(ounit,'MJDREF',mjdref,comment,ftstatus)
         if (ftstatus .eq. 202) ftstatus = 0
         
c      Convert TSTART and TSTOP into days, if necessary
c        Re - we've previous converted tstart and tstop to units of TIMEUNIT
         if (timeunit .eq. 's') then
            tstart = tstart / 86400.
            tstop = tstop / 86400.
         endif
         tstart = mjdref + tstart
         tstop = mjdref + tstop
         call ftmkyd(ounit,'TSTART',tstart,13,'&',ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to modify TSTART value in primary header'
            call fcerr(context)
            go to 999
         endif
         call ftmkyd(ounit,'TSTOP',tstop,13,'&',ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to modify TSTOP value in primary header'
            call fcerr(context)
            go to 999
         endif
      endif
         
c      error response
999   continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
         if (inopen) then
	    call ftclos(iunit,ftstatus)
	    call ftfiou(iunit,ftstatus)
	 endif
         if (outopen) then
	    call ftclos(ounit,ftstatus)
	    call ftfiou(ounit,ftstatus)
	 endif
      endif
      
      
c      end subroutine
      return
      end
