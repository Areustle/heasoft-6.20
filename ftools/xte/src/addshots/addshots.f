C ***************************************************************************
C SELECTOR TASK
C      addshots
C
C FILE:
C      addshots.f
C
C DESCRIPTION:      
C      adds a shot noise light curve to an input light curve
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      5/26/94 - replaced UNIX Fortran rand with ft_ran2 for generating
C                uniformly distributed random numbers
C      6/14/94 - implemented dynamic memory allocation
C      7/7/94  - added new shot shape: exponential shots with sinusoidal
C                 modulation      
C      10/11/94 - allow user to specify names of time, rate and error
C                  columns.  Defaults remain TIME, RATE, ERROR.  Also
C                  implemented abort option for non-fitsio errors.
C                  These modifications based on those in ADDSINE
C     08/23/97 - made case-independent the parameters for the shape and
C                the names of the time, rate and error columns (in gshot)
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
C      shape     - shot shape (SQUARE, EXP, EXPSIN)
C      parm      - array of shot parameter values:
C       parm(1) = srate     - shot rate
C       parm(2) = amplitude - shot amplitude
C       parm(3) = alpha     - power law dependence of ampl on duration
C       parm(4) = duration  - shot duration/decay time
C       parm(5) = eps       - lower limit to range in duration
C       parm(6) = zeta      - upper limit to range in duration
C       parm(7) = delta     - power law for resulting power density spetrum
C       parm(8) = amod      - amplitude for sinusoidal modulation (EXPSIN)      
C       parm(9) = per       - period for sinusoidal modulation (EXPSIN)
C       parm(10)= psig      - sigma for deviation of period
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                  2 = Guassian noise)
C      sigma     - sigma value for Gaussian noise
C      iseed     - seed for random number generator
C      abort     - logical to abort for non-fitsio errors
C
C CALLED ROUTINES:
C      subroutine gshot - gets parameters from environment
C      subroutine shotmem - allocate memory and adds shot light curve to input
C                             light curve
C ************************************************************************** 

      Subroutine ADDSHS

c start with initial declarations
      character(160) infile, outfile, intervals
      character(20) timename, ratename, errname
        double precision parm(12)
        real sigma
        integer noise, iseed
        character(10) shape
        logical abort
        
        character(40) taskname
        common /task/ taskname

        taskname = 'addshots3.3'
        abort = .false.
        
c get the parameters from the par file
        call gshot(infile, outfile, intervals, shape,
     &              parm, noise, sigma, iseed, timename,
     &              ratename, errname)

c Perform the Algorithm:   
        call shotmem(infile, outfile, intervals, shape,
     &                parm, noise, sigma, iseed, timename,
     &                ratename, errname, abort)

c  Exit subroutine

	return
	end

C*****************************************************************
C SUBROUTINE:
C      gshot
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/7/94 - added new shot shape: exponential shots with sinusoidal
C                 modulation 
C      10/11/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C     08/23/97 - made case-independent the parameters for the shape and
C                the names of the time, rate and error columns (in gshot)
C      
C NOTES:
C      gshot uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gshot(infile, outfile, intervals, shape, parm,
C                    noise, sigma, iseed, timename, ratename, errname)
C      
C ARGUMENTS:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      intervals - intervals of times to be analyzed (in MJD)
C      shape     - shot shape (SQUARE, EXP, EXPSIN)
C      parm      - array of shot parameter values:
C       parm(1) = srate     - shot rate
C       parm(2) = amplitude - shot amplitude
C       parm(3) = alpha     - power law dependence of ampl on duration
C       parm(4) = duration  - shot duration/decay time
C       parm(5) = eps       - lower limit to range in duration
C       parm(6) = zeta      - upper limit to range in duration
C       parm(7) = delta     - power law for distribution in duration
C       parm(8) = amod      - amplitude for sinusoidal modulation (EXPSIN)      
C       parm(9) = per       - period for sinusoidal modulation (EXPSIN)
C       parm(10)= psig      - sigma for deviation in period (EXPSIN)
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
C      subroutine uclgsd - get real*8 parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real*4 parameter
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

        SUBROUTINE gshot(infile, outfile, intervals, shape,
     &       parm, noise, sigma, iseed, timename, ratename, errname)


c start with the declarations
	character*(*) infile, outfile, intervals, shape
	character*(*) timename, ratename, errname
        double precision parm(*)
        real  sigma
        integer noise, iseed

	character(80) context
	integer status
        
	status = 0
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

c get the min & max times for intervals to be analyzed
        call uclgst('intervals',intervals,status)
        if (status .ne. 0) then
           context = 'could not get INTERVALS parameter'
           call fcerr(context)
           go to 999
        endif

c get the shot rate 
        call uclgsd('rate',parm(1),status)
        if (status .ne. 0) then
           context = 'could not get RATE parameter'
           call fcerr(context)
           go to 999
        endif

c get the shot amplitude
        call uclgsd('amplitude',parm(2),status)
        if (status .ne. 0) then
           context = 'could not get AMPLITUDE parameter'
           call fcerr(context)
           go to 999
        endif
         
c get the power law dependence of the shot amplitude on the duration
        call uclgsd('alpha',parm(3),status)
        if (status .ne. 0) then
           context = 'could not get ALPHA parameter'
           call fcerr(context)
           go to 999
        endif
         
c get the shot duration/decay time  
	call uclgsd('duration',parm(4),status)
	if (status .ne. 0) then
	  context = 'could not get DURATION parameter'
	  call fcerr(context)
	  go to 999
       endif
       
c get epsilon, the lower limit to the range of durations  
       call uclgsd('epsilon',parm(5),status)
       if (status .ne. 0) then
	  context = 'could not get EPSILON parameter'
	  call fcerr(context)
	  go to 999
       endif
       
c get zeta, the upper limit to the range of durations  
       call uclgsd('zeta',parm(6),status)
       if (status .ne. 0) then
	  context = 'could not get ZETA parameter'
	  call fcerr(context)
	  go to 999
       endif
       
c get delta, the power law for the distribution in shot duration  
       call uclgsd('delta',parm(7),status)
       if (status .ne. 0) then
	  context = 'could not get DELTA parameter'
	  call fcerr(context)
	  go to 999
       endif

C get amplitude of sinusoidal modulation (for EXPSIN shots)       
       call uclgsd('amod',parm(8),status)
       if (status .ne. 0) then
	  context = 'could not get AMOD parameter'
	  call fcerr(context)
	  go to 999
       endif

C get period of sinusoidal modulation (for EXPSIN shots)       
       call uclgsd('period',parm(9),status)
       if (status .ne. 0) then
	  context = 'could not get PERIOD parameter'
	  call fcerr(context)
	  go to 999
       endif

c get the deviation in the period of sinusoidal modulation (for EXPSIN shots)       
       call uclgsd('psig',parm(10),status)
       if (status .ne. 0) then
	  context = 'could not get PSIG parameter'
	  call fcerr(context)
	  go to 999
       endif

C get the shot shape 
        call uclgst('shape',shape,status)
        if (status .ne. 0) then
           context = 'cold not get SHAPE parameter'
           call fcerr(context)
           go to 999
        endif
        call FTUPCH(shape)
        
C get the noise option 
        call uclgsi('noise',noise,status)
        if (status .ne. 0) then
           context = 'cold not get NOISE parameter'
           call fcerr(context)
           go to 999
        endif

C get the sigma for Gaussian noise  
        call uclgsr('sigma',sigma,status)
        if (status .ne. 0) then
           context = 'cold not get SIGMA parameter'
           call fcerr(context)
           go to 999
        endif

C get the seed for the random number generator 
        call uclgsi('seed',iseed,status)
        if (status .ne. 0) then
           context = 'cold not get SEED parameter'
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
         call FTUPCH(timename)
C get the name of the rate column 
        call uclgst('ratename',ratename,status)
	if (status .ne. 0) then
	    context = 'could not get RATENAME parameter'
	    call fcerr(context)
	    go to 999
         endif
         call FTUPCH(ratename)
C get the name of the error column 
        call uclgst('errname',errname,status)
	if (status .ne. 0) then
	    context = 'could not get ERRNAME parameter'
	    call fcerr(context)
	    go to 999
         endif
         call FTUPCH(errname)
         
c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end

C **************************************************************************
C SUBROUTINE:
C      shotmem
C
C DESCRIPTION:      
C      Allocate dynamic memory and call shot noise routine
C      
C AUTHOR:
C      James Lochner  6/14/94
C
C MODIFICATION HISTORY:
C      10/12/94 - implemented abort option for non-fitsio errors
C      
C NOTES:
C
C USEAGE      
C      call shotmem(infile, outfile, intervals, shape, parm, noise,
C                    sigma, iseed, abort)
C
C ARGUMENTS:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      intervals - intervals of times to be analyzed (in MJD)
C      shape     - shot shape (SQUARE, EXP, EXPSIN)
C      parm      - array of shot parameter values
C                   (see main for list)      
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                   2 = Guassian Noise)
C      sigma	 - sigma for Gaussian noise (hidden)
C      iseed     - seed for random number generator
C      timename  - alternate name for time column in infile
C      ratename  - alternate name for rate column in infile
C      errname   - alternate name for error column in infile
C      abort     - logical to abort for non-fitsio errors
C
C PRIMARY LOCAL VARIABLES:
C      maxsize  - largest allowed array size
C      time     - pointer to array of input time values
C      rate     - pointer to array of input intensity values
C      error    - pointer to array of input uncertainty values
C      ratetmp  - pointer to array of working rate values
C      time2    - pointer to array of output time values 
C      rate2    - pointer to array of output intensity values
C      error2   - pointer to array of output uncertainty values
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine udmget - allocate dynamic memory
C      subroutine udmfre - free the dynamic memory
C      subroutine shots  - add a shot light curve to an input light curve
C
C ***************************************************************************
      subroutine shotmem(infile, outfile, intervals, shape, parm, noise,
     &                     sigma, iseed, timename, ratename, errname,
     &                     abort)

      character*(*) infile, outfile, intervals, shape
      character*(*) timename, ratename, errname
      double precision parm(*)
      real sigma
      integer noise, iseed
      logical abort

      integer maxsize, status
      character(80) context
      
C      declare the pointers for dynamic memory arrays
      integer time, rate, error, ratetmp, time2, rate2, error2
        
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

C      allocate the dynamic memory 
      maxsize = 86400

      status = 0

      time = 0
      time2 = 0
      rate = 0
      rate2 = 0
      ratetmp = 0
      error = 0
      error2 = 0
      
      call udmget(maxsize,7,time,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for TIME'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,7,time2,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for TIME2'
         call fcerr(context)
         go to 998
      endif
      
      call udmget(maxsize,6,rate,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for RATE'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,6,rate2,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for RATE2'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,6,ratetmp,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for RATETMP'
         call fcerr(context)
         go to 998
      endif
      
      call udmget(maxsize,6,error,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for ERROR'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,6,error2,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for ERROR2'
         call fcerr(context)
         go to 998
      endif

C      Now call the shot noise routine
      call shots(infile, outfile, intervals, shape, parm, noise, sigma,
     &     iseed, maxsize, timename, ratename, errname, MEMD(time),
     &     MEMR(rate), MEMR(error), MEMR(ratetmp), MEMD(time2),
     &     MEMR(rate2), MEMR(error2), abort)
      
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
        if (status .ne. 0) call fcerrm(status)

c      Free the memory
        call udmfre(time,7,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for TIME'
           call fcerr(context)
        endif
        call udmfre(time2,7,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for TIME2'
           call fcerr(context)
        endif

        call udmfre(rate,6,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for RATE'
           call fcerr(context)
        endif
        call udmfre(rate2,6,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for RATE2'
           call fcerr(context)
        endif
        call udmfre(ratetmp,6,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for RATETMP'
           call fcerr(context)
        endif
        
        call udmfre(error,6,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for ERROR'
           call fcerr(context)
        endif
        call udmfre(error2,6,status)
        if (status .ne. 0) then
           context = 'Error freeing dynamic memory for ERROR2'
           call fcerr(context)
        endif

        return
        end

C******************************************************************************
C SUBROUTINE:
C      shots
C
C DESCRIPTION:      
C      organize subroutines to carry out shot noise algorithms
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      10/12/94 - implemented abort option for non-fitsio errors
C      
C NOTES:
C
C USEAGE:      
C      call shots(infile, outfile, intervals, shape, parm,
C                  noise, sigma, iseed, maxsize, timename, ratename,
C                  errname, time, rate, error, ratetmp, time2, rate2
C                  error2, abort)
C      
C ARGUMENTS:
C      infile	 - name of input FITS file
C      outfile	 - name of output FITS file
C      intervals - intervals of times to be analyzed (in MJD)
C      shape     - shot shape (SQUARE, EXP, EXPSIN)
C      parm      - array of shot parameter values:
C                    (see main for list)
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                   2 = Guassian Noise)
C      sigma	 - sigma for Gaussian noise (hidden)
C      iseed     - seed for random number generator
C      maxsize   - largest allowed array size
C      timename  - alternate name for time column in infile
C      ratename  - alternate name for rate column in infile
C      errnmame  - alternate name for error column in infile
C      time      - array of input time values
C      rate      - array of input intensity values
C      error     - array of input uncertainty values
C      ratetmp   - shot noise light curve
C      time2     - array of output time values 
C      rate2     - array of output intensity values
C      error2    - array of output uncertainty values
C      abort     - logical to abort for non-fitsio errors
C      
C PRIMARY LOCAL VARIABLES:
C      timecol   - flag indicating presence of a time column in input file
C      bin       - bin size of the data
C      sbin      - bin size of the data in seconds
C      tunit     - units of the time array 
C      runit     - units of the rate array
C      tzero     - TZERO value
C      absmint   - absolute minimum time in the data 
C      absmaxt   - absolute maximum time in the data
C      nintervals- number of time intervals to which shots are added 
C      mintime   - array of the start times of the desired intervals
C      maxtime   - array of the stop times of the desired intervals
C      nsegs     - number of contiguous data segments in the interval
C      istart    - array of time array indices of beginning of each data segment
C      imax      - array index of the last point in the desired interval
C      obs       - duration of the shot light curve
C      segbins   - number of time bins in the shot light curve 
C      inopen    - logical indicating open status of input file
C      outopen   - logical indicating open status of output file
C      context   - error message
C      status    - error number
C
C CALLED ROUTINES:
C      subroutine readlc    - read light curve from FITS file
C      subroutine fcgrgd    - get the number of real*8 ranges and their limits
C      subroutine wrtprim    - write the primary extension of the output FITS file
C      subroutine segments  - determine the start times of segments of
C                            contiguous data
C      subroutine shotnoise - construct a shot noise light curve
C      subroutine addshotlc - add shot light curve to input light curve
C      subroutine riteshot  - write resulting light curve to FITS file
C      subroutine ftgiou    - get an available logical unit for i/o
C      subroutine ftfiou    - free a logical unit for i/o
C      subroutine fcerr     - echo message to terminal
C      subroutine fcerrm    - echo fitsio error message to terminal
C      function ft_ran2     - uniform random number generator
C      
C ********************************************************************************
      
      subroutine shots(infile, outfile, intervals, shape,
     &     parm, noise, sigma, iseed, maxsize, timename, ratename,
     &     errname, time, rate, error, ratetmp, time2, rate2, error2,
     &     abort)
      
C      Start with initial declarations
      integer maxsize
      
      character*(*) infile, outfile, intervals, shape
      character*(*) timename, ratename, errname
      double precision parm(*) 
      real sigma
      integer noise, iseed
      logical abort

      character(8) runit, tunit, author
      double precision time(maxsize), time2(maxsize), obs, bin, sbin
      double precision mintime(15), maxtime(15)
      double precision tzero, absmint, absmaxt, minT, maxT
      real rate(maxsize), error(maxsize)
      real rate2(maxsize), error2(maxsize)
      real ratetmp(0:maxsize-1)
      real r, ft_ran2
      integer istart(15), imax, nsegs, segbins, nintervls
      integer i, j 
      integer iunit, ounit, block, nrows, fstatus
      logical inopen, outopen, timecol

c      Initialize variables
      fstatus = 0
c      iunit = 15
c      ounit = 16
      inopen = .false.
      outopen = .false.
      call ftgiou(iunit,fstatus)
      call ftgiou(ounit,fstatus)
      
c      initialize the random number generator
      r = ft_ran2(iseed)
      
c      Read the input FITS file
      call readlc(iunit, infile, inopen, block, nrows, absmint,
     &     absmaxt, timename, ratename, errname, timecol, time, rate,
     &     error, bin, runit, tunit, tzero, maxsize, abort)
      if (abort) go to 999

c      obtain the bin size in secs (bin is in units of tunit)
      sbin = bin
      if (tunit .eq. 'd') sbin = 86400. * sbin
      
c      Parse the interval times into pairs of min & max times
      if (intervals .eq. '-') then
         nintervls = 1
         mintime(1) = absmint
         maxtime(1) = absmaxt
      else
         call fcgrgd(intervals, absmint, absmaxt, nintervls,
     &        mintime, maxtime)
      endif

c      Open the output file and write the primary header unit
      author = 'ADDSHOTS'
      call wrtprim(iunit, ounit, inopen, outopen, outfile, author)

c      call the shot routines for NINTERVLS
      do j = 1,nintervls
         minT = mintime(j) - tzero
         maxT = maxtime(j) - tzero
         if (tunit .eq. 's') then
            mint = 86400. * minT
            maxt = 86400. * maxT
         endif
         
c      Determine the start times of the contiguous data segments
         call segments(time, maxsize, nrows, mint, maxt, bin,
     &        nsegs, istart, imax)

c      Initialize the output arrays
         do i = 1,nrows
            time2(i) = time(i)
            rate2(i) = rate(i)
            error2(i) = error(i)
         end do
         
c      For each data segment, construct a shot light curve
c      and add it to the input light curve.
c       All inputs to shotnoise are in sec.
         do i = 1, nsegs-1
            obs = time(istart(i+1)-1) - time(istart(i)) + bin
            segbins = obs / bin
            if (tunit .eq. 'd') obs = 86400. * obs
            call shotnoise(shape, parm, obs,
     &           iseed, sbin, maxsize, ratetmp, runit, tunit)
            call addshotlc(maxsize, time, rate, error, runit, segbins,
     &       istart(i), noise, sigma, iseed, sbin, ratetmp,
     &       time2, rate2, error2)
         end do
c      Now do the last segment
         obs = time(imax) - time(istart(nsegs)) + bin
         segbins = obs / bin
         if (tunit .eq. 'd') obs = 86400. * obs
         call shotnoise(shape, parm, obs,
     &        iseed, sbin, maxsize, ratetmp, runit, tunit)
         call addshotlc(maxsize, time, rate, error, runit, segbins,
     &        istart(nsegs), noise, sigma, iseed, sbin, ratetmp,
     &        time2, rate2, error2)

c      Now output the light curve for this interval
         call riteshot(iunit, ounit, inopen, outopen,
     &        nrows, shape, parm, noise, sigma, timename, ratename,
     &        errname, time2, rate2, error2, abort)
         if (abort) go to 999
         
      end do

c      Close the input and output files
999   call ftclos(iunit,fstatus)
      call ftclos(ounit,fstatus)
      call ftfiou(iunit,fstatus)
      call ftfiou(ounit,fstatus)
      inopen = .false.
      outopen = .false.
      
      return
      end

C **********************************************************************
C SUBROUTINE:
C      segments
C
C DESCRIPTION:      
C      determine the start times of segments of contiguous data in
C       the input light curve
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:      
C      call segments(time, maxsize, nrows, mint, maxt, bin,
C                    nsegs, istart, imax)
C      
C ARGUMENTS:
C      maxsize - largest allowed array size
C      time    - array of input time values
C      nrows   - total number of points in time array
C      mint    - input minimum time in which to search for segments
C      maxt    - input maximum time in which to search for segments
C      bin     - bin size of the data
C      nsegs   - number of contiguous data segments found
C      istart  - array of time array indices of beginning of each segment
C      imax    - array index of the time meeting the maxt value
C      
C PRIMARY LOCAL VARIABLES:
C      flag    - indicates the finding of the first point in the requested
C                 time range
C
C CALLED ROUTINES:
C      none
C      
C ***********************************************************************      
      subroutine segments(time, maxsize, nrows, mint, maxt, bin,
     &     nsegs, istart, imax)

      integer istart(15), nsegs, maxsize, nrows, imax
      double precision  time(maxsize), mint, maxt, bin

      integer i, j, flag

      flag = 0

c      Find the first point in the requested time range
      i = 1
      do while (flag .eq. 0 .and. i .le. nrows)
         if (time(i) .ge. mint) then
            flag = 1
            istart(1) = i
         endif
         i = i + 1
      end do

c      Now find the rest of the segment boundaries
      j = 2
      do while (i .le. nrows .and. time(i) .le. maxt
     &     .and. j .le. 15)
         if (nint((time(i)-time(i-1))/bin) .ne. 1) then
            istart(j) = i
            j = j + 1
         endif
         i = i + 1
      end do
      nsegs = j - 1
      imax = i - 1
      
c      write(6,6000) (j, istart(j), time(istart(j)), j = 1,nsegs)
c6000  format(' ',i2,2x,i4,2x,f10.4)
c      write(6,*) 'imax = ',imax
      return
      end
      
C ******************************************************************************
C SUBROUTINE:
C      shotnoise
C
C DESCRIPTION:      
C      construct a shot noise light curve
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/7/94 - added new shot shape: exponential shots with sinusoidal
C                modulation      
C      
C NOTES:
C
C USEAGE:      
C      call shotnoise(shape, parm, obs, iseed, bin, maxsize,
C                     rate, runit, tunit)
C      
C ARGUMENTS:
C      shape   - shot profile: EXPON, SQUARE, or EXPSIN
C      parm      - array of shot parameter values:
C       parm(1) = srate  - shot rate (in #/sec)
C       parm(2) = amp    - shot amplitude (in cts/s)
C       parm(3) = alpha  - power law dependence of ampl on duration (le 0)
C       parm(4) = tau2   - shot duration/decay time (in sec)
C       parm(5) = eps    - lower limit to range in duration (in sec)
C       parm(6) = zeta   - upper limit to range in duration (in sec)
C       parm(7) = delta  - power law for resulting power density spectrum
C                           (-2 < delta < 0)
C       parm(8) = amod   - amplitude for sinusoidal modulation (EXPSIN)
C       parm(9) = per    - period for sinusoidal modulation (EXPSIN)
C       parm(10)= psig   - sigma for deviation of period (EXPSIN)
C      obs     - total observation time (in sec)
C      iseed   - seed for random number generator
C      bin     - bin size of the light curve (in sec)
C      maxsize - parameter for max size of time array
C      rate    - output intensity array
C      runit   - units of the input light curve (count or count/s)
C      tunit   - units of the time array
C      
C PRIMARY LOCAL VARIABLES:
C      time1   - time of the first shot
C      slength - duration of the first shot
C      tshot   - time of the shot relative to the previous shot
C      time    - absolute time of the shot
C      ibin    - array index corresponding to the shot time
C      tb      - time remaining in bin after the shot time
C      nshot   - total number of shots generated
C
C CALLED ROUTINES:
C      subroutine expon  - compute binned intensity for an exponential shot
C      subroutine square - compute binned intensity for a square shot
C      function ft_ran2  - uniform random number generator
C
C **************************************************************************      

      subroutine shotnoise(shape, parm, obs, iseed, 
     &     bin, maxsize, rate, runit, tunit)
      
      integer maxsize, iseed
      double precision parm(*), obs, bin
      double precision srate, tau2, eps, zeta, period, psig
      real rate(0:maxsize-1), delta, alpha, amp, amod
      character(10) shape
      character*(*) runit, tunit
      
      integer i, ibin, nshot
      real ft_ran2
      double precision zetafact, epsfact, per
      double precision time, time1, tshot, slength, tb

      slength=0.0D0

C      Set rate array to zero
      do i = 0,min(maxsize,int(obs/bin))-1
         rate(i) = 0.
      end do

C      unpack the parameter array
      srate = parm(1)
      amp   = parm(2)
      alpha = parm(3)
      tau2  = parm(4)
      eps   = parm(5)
      zeta  = parm(6)
      delta = parm(7)
      amod  = parm(8)
      period= parm(9)
      psig  = parm(10)
      if (delta .lt. 0 .and. alpha + delta .ne. -2) then
         epsfact = 1.0 / eps**(alpha + delta + 2.)
         zetafact = 1.0 / zeta**(alpha + delta + 2.)
      endif
      
C      Assign the time of the first shot according to its length
      i = 1
      time1 = -alog(1-ft_ran2(iseed))/srate
      if (shape .eq. 'EXPON') slength = -tau2 * alog(1.-0.99)
      if (shape .eq. 'SQUARE') slength = tau2
      if (shape .eq. 'EXPSIN') slength = -tau2 * alog(1.-0.99)
      tshot = time1
      
c      Calculate the time of the shots, and compute their contribution
c      to the intensity
100   i = i + 1
         tshot = tshot - alog(1 - ft_ran2(iseed)) / srate
         time = tshot - time1 - slength
         if (time .gt. obs) go to 200
         ibin = int(time/bin)
         if (time .lt. 0.) ibin = ibin - 1
         tb = bin*(ibin+1) - time
         call charac(parm,epsfact,zetafact,tau2,amp,per,bin,iseed)
         if (shape .eq. 'EXPON')
     &        call EXPON(tau2,amp,ibin,time,tb,bin,obs,rate,maxsize)
         if (shape .eq. 'SQUARE')
     &        call SQUARE(tau2,amp,ibin,time,tb,bin,obs,rate,maxsize)
         if (shape .eq. 'EXPSIN')
     &        call EXPSIN(tau2,amp,per,amod,ibin,time,tb,bin,obs,rate,
     &        maxsize)
      go to 100 

c      All done
200   nshot = i - 2

c      Re - shot light curve is in units of counts.  If input light curve
c      is not in counts, then convert shot light curve to count/s (or what
c      ever)
      if (runit .ne. 'count') then
         do i = 0,min(maxsize,int(obs/bin))-1
            rate(i) = rate(i) / bin
         end do
      end if

c      write(6,6000) obs, nshot
c6000  format( ' obs = ',f7.4,'  nshots = ',i3)
      return
      end

C*****************************************************************
C SUBROUTINE:
C      charac
C
C DESCRIPTION:      
C      Determine the characteristics of a given shot
C      
C AUTHOR:
C      James Lochner  5/27/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:      
C      call charac(
C      
C ARGUMENTS:
C      parm      - array of shot parameter values:
C       parm(1) = srate    - shot rate
C       parm(2) = amp      - shot amplitude
C       parm(3) = alpha    - power law dependence of ampl on duration
C       parm(4) = duration - shot duration/decay time
C       parm(5) = eps      - lower limit to range in duration
C       parm(6) = zeta     - upper limit to range in duration
C       parm(7) = delta    - power law for resulting power density spetrum
C       parm(8) = amod     - amplitude for sinusoidal modulation (EXPSIN)
C       parm(9) = per      - period for sinusoidal modulation (EXPSIN)
C       parm(10)= psig     - sigma for deviation in period (EXPSIN)
C      iseed     - seed for random number generator
C      
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C	function ft_ran2  - uniform random number generator
C
C *************************************************************************

	subroutine charac(parm,epsfact,zetafact,tc2,amp,per,bin,iseed)

	double precision parm(*), epsfact, zetafact, tc2, bin, per
        real amp
        integer iseed

	double precision srate, duration, eps, zeta
	real alpha, delta, amplit, period, psig
        real ft_ran2, gauss
        
C	Unpack the parameter array
      srate = parm(1)
      amplit   = parm(2)
      alpha = parm(3)
      duration  = parm(4)
      eps   = parm(5)
      zeta  = parm(6)
      delta = parm(7)
      period= parm(9)
      psig  = parm(10)

      if (delta .lt. 0) then
         IF ( alpha + delta .ne. -2.0) THEN                
            TC2 = 1.0 /                                                           
     *           (EPSFACT + FT_RAN2(ISEED) * (ZETAFACT - EPSFACT))                               
     *           ** (1./(2.0 + ALPHA + DELTA))
         ELSE
            TC2 = EPS * (ZETA/EPS) ** FT_RAN2(ISEED)
         ENDIF                                                                 
      endif
      
      if (alpha .lt. 0.)
     &     amp = amplit * (tc2/bin) ** (alpha/2.)
      
      if (psig .gt. 0.0) then
         per = gauss(period, psig, iseed)
      else
         per = period
      endif
      
      return
      end




C ******************************************************************************
C SUBROUTINE:
C      expon
C
C DESCRIPTION:      
C      generate intensities from exponentially shaped shots
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      adapted from IBM routines
C      
C USEAGE:      
C      call expon(TC,A,IBIN,TIME,TB,bnsize,obs,FLUX,maxsize)
C      
C ARGUMENTS:
C      tc    - decay time/duration of the shot (in sec)
C      a     - shot amplitude (in cts/s)
C      ibin    - array index corresponding to the shot time
C      time    - absolute time of the shot
C      tb      - time remaining in bin after the shot time
C      bnsize  - bin size of the light curve (in sec)
C      obs     - total observation time (in sec)
C      flux    - output intensity array
C      maxsize - parameter for max size of time array
C      
C PRIMARY LOCAL VARIABLES:
C      fshot   - intensity for fraction of first bin
C      nbins   - number of whole bins to be filled
C      fract   - time to be filled in the last bin
C
C CALLED ROUTINES:
C      none
C
C **************************************************************************      
      SUBROUTINE EXPON(TC,A,IBIN,TIME,TB,bnsize,obs,FLUX,maxsize)

c      implicit none
      integer maxsize, ibin
      real*4 a, FLUX(0:maxsize-1)
      real*8 tc, time, tb, bnsize, obs

      real*4 fract, fshot
      integer j, jinit, nbins

C--   following line is to shut up compiler warnings                
      jinit = 1
      IF (IBIN .LT. 0) GO TO 5                                                  
C--                                                                             
C--  FILL THE FIRST BIN                                                         
C--                                                                             
      FSHOT = A * TC * (1 - EXP(-TB/TC))                                        
      FLUX(IBIN) = FLUX(IBIN) + FSHOT                                           
      IF ( FSHOT / (A*TC) .GT. 0.99) GO TO 100                                  
C--                                                                             
C--  FILL THE WHOLE BINS WHICH LIE WITHIN THE OBSERVATION TIME                  
C--                                                                             
      JINIT = 1                                                                 
5     NBINS = ( -TC*ALOG(1-0.99) - TB) / BNSIZE                                 
      IF (IBIN .LT. 0) JINIT = -IBIN
C--  
      if (jinit-nbins-1 .lt. 0) then
7        DO 10 J = JINIT,NBINS                                                     
            IF ( BNSIZE*(IBIN+J) .GE. OBS) GO TO 100                                
            FLUX(IBIN+J) = FLUX(IBIN+J)                                             
     *           + A*TC * EXP(-(TB+J*BNSIZE)/TC)                                  
     *           * (EXP(BNSIZE / TC) - 1)                             
10       continue
      end if
C--                                                                             
C--  NOW FILL THE FRACTION OF THE LAST BIN                                      
C--
      if (jinit-nbins-1 .le. 0) then
15       IF ((IBIN+NBINS+1)*BNSIZE .GE. OBS) GO TO 100                             
         FRACT = (-TC*ALOG(1-0.99) - TB) / BNSIZE - NBINS                          
         FLUX(IBIN+NBINS+1) = FLUX(IBIN+NBINS+1)                                   
     *        + A*TC * EXP(-(TB + NBINS*BNSIZE ) / TC)                     
     *        * (1 - EXP(-FRACT * BNSIZE / TC))
      end if
         
100   RETURN                                                                    
      END                                                                       
C ******************************************************************************
C SUBROUTINE:
C      square
C
C DESCRIPTION:      
C      generate intensities from square shaped shots
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      adapted from IBM routines
C      
C USEAGE:      
C      call square(TC,A,IBIN,TIME,TB,bnsize,obs,FLUX,maxsize)
C      
C ARGUMENTS:
C      tc    - decay time/duration of the shot (in sec)
C      a     - shot amplitude (in cts/s)
C      ibin    - array index corresponding to the shot time
C      time    - absolute time of the shot
C      tb      - time remaining in bin after the shot time
C      bnsize  - bin size of the light curve (in sec)
C      obs     - total observation time (in sec)
C      flux    - output intensity array
C      maxsize - parameter for max size of time array
C      
C PRIMARY LOCAL VARIABLES:
C      nbins   - number of whole bins to be filled
C
C CALLED ROUTINES:
C      none
C
C **************************************************************************

      SUBROUTINE SQUARE(TC,A,IBIN,TIME,TB,bnsize,obs,FLUX,maxsize)

c      implicit none
      integer maxsize, ibin
      real*4 a, FLUX(0:maxsize-1)
      real*8 tc, time, tb, bnsize, obs

      integer j, jinit, nbins

C--   the following line is to shut up compiler warnings
      jinit = 1
      IF (IBIN .LT. 0) GO TO 5                                                  
C--                                                                             
C--  FILL THE FIRST BIN                                                         
C--                                                                             
      IF (TC .LE. TB) FLUX(IBIN) = FLUX(IBIN) + A*TC                            
      IF (TC .LE. TB) GO TO 100                                                 
      FLUX(IBIN) = FLUX(IBIN) + A*TB
C--                                                                             
C--  FILL THE WHOLE BINS WHICH LIE WITHIN THE OBSERVATION TIME                  
C--                                                                             
       JINIT = 1                                                                
5      NBINS = (TC - TB)/BNSIZE                                                 
C--  WHOLE NO. OF BINS REMAINING                                                
       IF (IBIN .LT. 0) JINIT = -IBIN

c      fill whole bins
       if (jinit-nbins-1 .lt. 0) then
7         DO  J=JINIT,NBINS                                                      
            IF ((IBIN+J)*BNSIZE .GE. OBS) GO TO 100                                 
            FLUX(IBIN+J) = FLUX(IBIN+J) + A*BNSIZE
         end do         
      endif
      
c      fill fraction of the last bin
       if (jinit-nbins-1 .le. 0) then
15       IF ((IBIN+NBINS+1)*BNSIZE .GE. OBS) GO TO 100                           
         FLUX(IBIN+NBINS+1) = FLUX(IBIN+NBINS+1)
     *           + A * (TC-TB-BNSIZE*NBINS)                      
         end if
         
100      RETURN                                                                    
      END                                                                       
C ******************************************************************************
C SUBROUTINE:
C      expsin
C
C DESCRIPTION:      
C      generate intensities from exponentially shaped shots having a 
C        sinusoidal modulation
C      
C AUTHOR:
C      James Lochner  7/7/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      adapted from IBM routines
C      
C USEAGE:      
C      call expon(TC,A,IBIN,TIME,TB,bnsize,obs,FLUX,maxsize)
C      
C ARGUMENTS:
C      tc    - decay time/duration of the shot (in sec)
C      a     - shot amplitude (in cts/s)
C      per   - period for sinusoidal modulation
C      c     - amplitude of the sinusoidal modulation 
C      ibin    - array index corresponding to the shot time
C      time    - absolute time of the shot
C      tb      - time remaining in bin after the shot time
C      bnsize  - bin size of the light curve (in sec)
C      obs     - total observation time (in sec)
C      flux    - output intensity array
C      maxsize - parameter for max size of time array
C      
C PRIMARY LOCAL VARIABLES:
C      fshot   - intensity for fraction of first bin
C      nbins   - number of whole bins to be filled
C      fract   - time to be filled in the last bin
C
C CALLED ROUTINES:
C      none
C
C **************************************************************************

      SUBROUTINE EXPSIN(TC,A,PER,C,IBIN,TIME,TB,bnsize,obs,
     &     FLUX,maxsize)                           

      integer maxsize, ibin
      real*4 a, c, FLUX(0:maxsize-1)
      double precision tc, time, tb, bnsize, obs, per

      real*4  twopi, t1, t2, t3, t4
      real*4 om,f
      integer j, jinit, nbins, nflux
      DATA TWOPI,T1,T2,T3,T4/6.2831853072,4*0.0/                                

C--                                                                             
      OM = TWOPI / PER                                                          
      IF (IBIN .LT. 0) GO TO 5                                                  
C--                                                                             
C--  FILL THE FIRST BIN                                                         
C--                                                                             
      FLUX(IBIN) = FLUX(IBIN) + A * TC * ( 1 - EXP(-TB/TC) )                    
     *                  +  C / ( (1/TC)**2 + OM**2) *                        
     *  ( OM - EXP(-TB/TC) * ( SIN(OM*TB) / TC + OM * COS(OM*TB) ) )            
C--                                                                             
C-- FILL THE REMAINING BINS IN THE OBSERVATION TIME                             
C--                                                                             
       JINIT = 1                                                                
5      IF (IBIN .LT. 0) JINIT = -IBIN                                           
       NFLUX = OBS / BNSIZE                                                     
       NBINS = MIN(NFLUX-IBIN,NFLUX)
       DO 10 J = JINIT,NBINS                                                    
         F = TB + J*BNSIZE                                                      
         IF (F/TC .GT. 45.0) GO TO 100                                          
         IF (C .EQ. 0.0) GO TO 7                                                
         T1 = C * EXP(-F/TC) / ( (1/TC)**2 + OM**2 )                        
         T2 = SIN(OM*F) / TC + OM * COS(OM*F)                                   
         T3 = EXP(BNSIZE/TC) * (SIN(OM*(F - BNSIZE) ) / TC  +                   
     *             OM * COS(OM*(F - BNSIZE) ) )                                 
7        T4 = A * TC * EXP(-F/TC) * (EXP(BNSIZE/TC) - 1)                        
         FLUX(IBIN+J) = FLUX(IBIN+J) + T1 * (T3 - T2) + T4                      
10    CONTINUE                                                                 
100    RETURN                                                                   
      END                                                                       
C--                                                                             
C **********    END SUBROUTINE EXPSIN       ***********                         

C ******************************************************************************
C SUBROUTINE:
C      addshotlc
C
C DESCRIPTION:      
C      add a shot noise light curve to an input light curve      
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:      
C      call addshotlc(maxsize, time, rate, error, runit, jpoints, jstart,
C                 noise, sigma, iseed, bin, ratetmp, time2, rate2, error2)
C      
C ARGUMENTS:
C      maxsize   - maximum size of the arrays
C      time      - array of input time values
C      rate      - array of input intensity values
C      error     - array of input uncertainty values
C      runit     - units of the rate array
C      jpoints   - number of points in the shot light curve
C      jstart    - time array index of beginning of each data segment 
C      noise 	 - noise option (0 = no noise, 1 = Poisson noise,
C                   2 = Guassian Noise)
C      sigma	 - sigma for Gaussian noise (hidden)
C      iseed     - seed for random number generator
C      bin       - bin size of the data 
C      ratetmp   - shot noise light curve
C      time2     - array of output time values 
C      rate2     - array of output intensity values
C      error2    - array of output uncertainty values
C    
C PRIMARY LOCAL VARIABLES:
C      none
C
C CALLED ROUTINES:
C      subroutine poisnois - add Poisson noise to an input value
C      subroutine gausnois - add Gaussian noise to an input value
C
C ******************************************************************************

      subroutine addshotlc(maxsize, time, rate, error, runit, jpoints,
     &     jstart, noise, sigma, iseed, bin, ratetmp,
     &     time2, rate2, error2)

      integer maxsize
      
      double precision time(maxsize), time2(maxsize), bin
      real rate(maxsize), rate2(maxsize), ratetmp(0:maxsize-1)
      real error(maxsize), error2(maxsize)
      real sigma
      integer noise, jpoints, jstart, iseed
      character*(*) runit

      integer j

c      write(22,*) 'ADDSHOTLC:  jstart,jpoints',jstart,jpoints
c      write(22,*) 'noise, sigma',noise, sigma
      
      do j = 0,jpoints-1
         time2(jstart+j) = time(jstart+j)
         if (noise .eq. 0) then
            rate2(jstart+j) = ratetmp(j) + rate(jstart+j)
            error2(jstart+j) = error(jstart+j)
         else if (noise. eq. 1) then
            call poisnois(bin,ratetmp(j),rate(jstart+j),runit,
     &       rate2(jstart+j),error(jstart+j),error2(jstart+j),iseed)
         else if (noise. eq. 2) then
            call gausnois(sigma,ratetmp(j),rate(jstart+j),
     &       rate2(jstart+j),error(jstart+j),error2(jstart+j),iseed)
         endif
      end do

c      do j = 0, jpoints-1
c         write(22,2200) j,time(jstart+j),rate(jstart+j),error(jstart+j),
c     &        time2(jstart+j),rate2(jstart+j),error2(jstart+j)
c2200     format(' ',i3,2(2x,f9.3,2x,f5.2,2x,f5.2,2x))
c      end do
      
      return
      end
C*******************************************************************************
C SUBROUTINE:
C      riteshot
C
C DESCRIPTION:      
C      write the result from ADDSHOT into FITS file with same format
C        as input file.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      10/11/94 - allow user to specify names of time, rate and error
C                  columns.  Defaults remain TIME, RATE, ERROR. These
C                  changes taken from FSWRITE in ADDSINE.
C NOTES:
C
C USEAGE:      
C      subroutine riteshot(iunit, ounit, inopen, outopen, timecol,
C          nrows, shape, parm, noise, sigma, timename, ratename, errname,
C          time, rate, error) 
C
C ARGUMENTS:
C      iunit    - logical unit for input file
C      ounit    - logical unit for output file
C      inopen   - logical variable indicating open status of input file
C      outopen  - logical variable indicating open status of output file
C      timcol   - logical variable indicating presence of time column
C                  in input file
C      nrows    - total number of data points in output light curve
C      shape	- shot shape
C      parm      - array of shot parameter values:
C       parm(1) = srate     - shot rate
C       parm(2) = amp       - shot amplitude
C       parm(3) = alpha     - power law dependence of ampl on duration
C       parm(4) = duration  - shot duration/decay time
C       parm(5) = eps       - lower limit to range in duration
C       parm(6) = zeta      - upper limit to range in duration
C       parm(7) = delta     - power law for distribution in duration
C       parm(8) = amod      - amplitude for sinusoidal modulation (EXPSIN)
C       parm(9) = per       - period for sinusoidal modulation (EXPSIN)
C       parm(10)= psig      - sigma for deviation in period (EXPSIN)
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
c      context  - error messages
c      fstatus  - fitsio status number
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
C      subroutine ftfiou - free a logical unit for i/o
C      subroutine fcerr  - prints message to stderror device
C      subroutine fcerrm - prints fitsio error number and message
C      function comnum   - append a numerial value to a character string
C      
C ******************************************************************************
      
      subroutine riteshot(iunit, ounit, inopen, outopen,
     &     nrows, shape, parm, noise, sigma, timename, ratename, 
     &     errname, time, rate, error, abort) 
      
c      declarations
      
      character *(*)  shape, timename, ratename, errname
      integer iunit, ounit, frow, felem, nrows, noise 
      double precision time(nrows), parm(*) 
      real rate(nrows), error(nrows), sigma
      logical abort

      character(80) context, history, comnum
      character(30) comment
      character(55) com55
      character(8) tform(10), ttype(10), tunit(10), extname
      character(8) runit
      character(1) tcolunit, timeunit
      integer ftstatus, htype, extnum, j, pc, varidat
      integer timepos, ratepos, errpos
      integer startno, maxkeys, ncol, mcol
      double precision srate, duration, eps, zeta, per, psig
      real amp, delta, alpha, amod
      logical inopen, outopen, timecol, ratecol, errcol
      
      
      extnum = 1
      pc = 0
      ftstatus = 0
      
C      unpack the parameter array
      srate = parm(1)
      amp   = parm(2)
      alpha = parm(3)
      duration  = parm(4)
      eps   = parm(5)
      zeta  = parm(6)
      delta = parm(7)
      amod  = parm(8)
      per   = parm(9)
      psig  = parm(10)
      
c      Move to the data extension in the input file
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

c      Get the column data types (TFORM) from the input file
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
      
c      add HISTORY giving the shot rate, shape, amplitude, duration
      history = 'This Data has had shot noise added to it:'
      call ftphis(ounit,history,ftstatus)
      
      history = ' Shot Shape = '//shape
      call ftphis(ounit,history,ftstatus)

      comment = comnum(' Shot Rate = ',srate)
      history = comment//' shots/s'
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to add HISTORY'
         call fcerr(context)
         goto 999
      endif

      comment = comnum(' Shot Amplitude = ',dble(amp))
      history = comment//' count/s'
      call ftphis(ounit,history,ftstatus)
      
      if (delta .lt. 0.) then
         history = ' Power law distribution in durations:'
         call ftphis(ounit,history,ftstatus)
         comment = comnum('  min duration = ',eps)
         history = comment//' s'
         call ftphis(ounit,history,ftstatus)
         comment = comnum('  max duration = ',zeta)
         history = comment//' s'
         call ftphis(ounit,history,ftstatus)
         history = comnum('  power law = ',dble(delta))
         call ftphis(ounit,history,ftstatus)
         if (alpha .lt. 0) then
            history = ' Power law dependence of amplitude on duration'
            call ftphis(ounit,history,ftstatus)
            history = comnum('  power law = ',dble(alpha))
            call ftphis(ounit,history,ftstatus)
         endif
         
      else
         comment = comnum(' Shot Duration = ',duration)
         history = comment//' s'
         call ftphis(ounit,history,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to add HISTORY'
            call fcerr(context)
            goto 999
         endif
      endif
         
      if (shape .eq. 'EXPSIN') then
         com55 = comnum(' Amplitude for Sinusoidal Modulation = ',
     &        dble(amod))
         history = com55//' count/s'
         call ftphis(ounit,history,ftstatus)
         com55 = comnum(' Period for Sinusoidal Modulation = ',per)
         history = com55//' s'
         call ftphis(ounit,history,ftstatus)
         com55 =
     &      comnum(' Sigma for Guassian Deviations in Period = ', psig)
         history = com55//' s'
         call ftphis(ounit,history,ftstatus)
      endif

      if (noise .eq. 0) then
         history = ' No noise added to shot light curve'
      else if (noise .eq. 1) then
         history = ' Poisson noise added to shot light curve'
      else if (noise .eq. 2) then
         history = comnum(' Guassian noise with sigma = ',dble(sigma))
      endif
      call ftphis(ounit,history,ftstatus)

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
      
c      If implicit TIME, then write RATE and ERROR (if present)
      if (.not. timecol) then
         call ftpcle(ounit,ratepos,frow,felem,nrows,rate,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write rate array'
            call fcerr(context)
            goto 999
         endif
         if (ncol .eq. 2 .or. noise .gt. 0) then
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
         if (ncol .eq. 3 .or. noise .gt. 0) then
            call ftpcle(ounit,errpos,frow,felem,nrows,error,ftstatus)
            if (ftstatus .ne. 0) then
               context = 'unable to write error array' 
               call fcerr(context)
               goto 999
            endif
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
