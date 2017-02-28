C ***************************************************************************
C SELECTOR TASK
C      fakelc
C
C FILE:
C      fakelc.f
C
C DESCRIPTION:      
C      creates a fake light curve with a given mean count rate.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      5/26/94 - utilized ft_ran2 as the random number generator
C      6/10/94 - implemented dynamic memory allocation
C      7/28/94 - added option to give std HEXTE 16-s on, 16-s off
C                 sampling of a single cluster. (later changed to 32-s)
C      8/18/94 - allow user to specify the units for time span of
C                 the observation.  Also, write MJDREF to output file,
C                 making TSTART a hidden parameter to override the XTE
C                 default.
C      9/19/94 - implemented low-earth orbit option for data sampling
C      9/28/94 - Changed definition of SPAN to be requested live time
C                 on source.  Accordingly, replace SEGLENGTH with GAPLENGTH,
C                 the average size of data gaps.
C      7/30/96 - fixed floating exception bug which appears on alpha
C                 platform when noise=0.  In compute.f assigned error=0.0
C                 in such cases
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      type     - type of light curve: Event or Binned
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      cts      - average intensity
C      ctsunit  - units for cts (in Counts or Count/s)
C      noise    - noise option
C      sigma    - sigma for Gaussian noise
C      span     - requested total live time
C      spanunit - units for span (s=sec, d=days)
C      tstart   - observation start time (in MJD)
C      hexte    - flag for using HEXTE standard 32-s dwell pattern
C      nsegs    - number of data segments
C      gaplenth - average length of each data gap (units of spanunit)
C      orbit    - option to use typical XTE orbit for time sampling
C      iseed    - seed for random number generator
C      outfile  - name of output FITS file
C
C CALLED ROUTINES:
C      subroutine gfakelc - gets parameters from environment
C      subroutine fakemem - create the fake light curve
C
C ************************************************************************** 

      subroutine fakelc
      
C      start with initial declarations
	character(180) outfile
        character(8) binunit, ctsunit, spanunit
        character(1) type, orbit
	double precision bin, tstart, span, gaplenth
	real cts, sigma
	integer noise, nsegs, iseed
        logical hexte

        character(40) taskname
        common /task/ taskname

        taskname = 'fakelc3.6'
        
C get the parameters from the par file
	call gfakelc(type,bin,binunit,cts,ctsunit,noise,sigma,span,
     &       spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,outfile)

C Create the fake light curve
	call fakemem(type,bin,binunit,cts,ctsunit,noise,sigma,span,
     &       spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,outfile)
        
C  Exit subroutine

	return
	end
C **************************************************************************
C SUBROUTINE:
C      gfakelc
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      8/18/94 - read parameter for units on time span of observation.
C      9/28/94 - replaced SEGLENTH with GAPLENTH
C
C NOTES:
C      gfakelc uses F77/VOS like calls to read parameters from .par file
C
C USEAGE      
C      call gfakelc(type,bin,binunit,cts,ctsunit,noise,sigma,span,
C                spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,outfile)
C      
C ARGUMENTS:
C      type     - type of light curve: Event or Binned
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      cts      - average intensity
C      ctsunit  - units for cts (in Counts or Count/s)
C      noise    - noise option
C      sigma    - sigma for Gaussian noise
C      span     - requested total live time
C      spanunit - units for span (s=sec, d=days)
C      tstart   - observation start time (in MJD)
C      hexte    - flag for using HEXTE std 32-s dwell pattern
C      nsegs    - number of data segments
C      gaplenth - average length of each data segment (units of spanunit)
C      orbit    - option to use typical XTE orbit for time sampling
C      iseed    - seed for random number generator
C      outfile  - name of output FITS file
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
C ************************************************************************** 

      SUBROUTINE gfakelc(type,bin,binunit,cts,ctsunit,noise,sigma,span,
     &         spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,outfile)

C start with the declarations
	character*(*) outfile, binunit, ctsunit, spanunit, type, orbit
        real cts, sigma
	double precision bin, tstart, span, gaplenth
	integer noise, nsegs, iseed
        logical hexte
        
	character(80) context
	integer status

	status=0
C      get the name of the output FITS file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcerr(context)
	    go to 999
	endif

C      get the type of light curve desired: Event or Binned
	call uclgst('type',type,status)
	if (status .ne. 0) then
	    context = 'could not get TYPE parameter'
	    call fcerr(context)
	    go to 999
         endif

C      get the observation start time
	call uclgsd('tstart',tstart,status)
	if (status .ne. 0) then
	    context = 'could not get TSTART parameter'
	    call fcerr(context)
	    go to 999
         endif

C      get the bin size  
	call uclgsd('binsize',bin,status)
	if (status .ne. 0) then
	    context = 'could not get BINSIZE parameter'
	    call fcerr(context)
	    go to 999
         endif

C      get the units of bin  
	call uclgst('binunit',binunit,status)
	if (status .ne. 0) then
	    context = 'could not get BIN UNIT parameter'
	    call fcerr(context)
	    go to 999
         endif
         
C      get the mean count rate of the light curve
	call uclgsr('cts',cts,status)
	if (status .ne. 0) then
	    context = 'could not get CTS parameter'
	    call fcerr(context)
	    go to 999
	endif

C      get the units of cts  
	call uclgst('ctsunit',ctsunit,status)
	if (status .ne. 0) then
	    context = 'could not get CTS UNIT parameter'
	    call fcerr(context)
	    go to 999
         endif

C      get the noise option 
        call uclgsi('noise',noise,status)
	if (status .ne. 0) then
	    context = 'could not get NOISE OPTION parameter'
	    call fcerr(context)
	    go to 999
	endif

C      get the SIGMA for guassian noise 
	call uclgsr('sigma',sigma,status)
	if (status .ne. 0) then
	  context = 'could not get SIGMA parameter'
	  call fcerr(context)
	  go to 999
	endif

C      get the total time span of the light curve 
	call uclgsd('span',span,status)
	if (status .ne. 0) then
	  context = 'could not get SPAN  parameter'
	  call fcerr(context)
	  go to 999
       endif

C      get the units of time span  
	call uclgst('spanunit',spanunit,status)
	if (status .ne. 0) then
	    context = 'could not get SPAN UNIT parameter'
	    call fcerr(context)
	    go to 999
         endif

C      get the HEXTE flag
        call uclgsb('hexte',hexte,status)
        if (status .ne. 0) then
	  context = 'could not get HEXTE parameter'
	  call fcerr(context)
	  go to 999
	endif
       
C      get the number of data segments in the light curve  
        call uclgsi('nsegs',nsegs,status)
	if (status .ne. 0) then
	  context = 'could not get NSEGS parameter'
	  call fcerr(context)
	  go to 999
	endif

C      get the average length of each data segment  
	call uclgsd('gaplength',gaplenth,status)
	if (status .ne. 0) then
	  context = 'could not get GAPLENTH parameter'
	  call fcerr(context)
	  go to 999
	endif

C      get the orbit option  
	call uclgst('orbit',orbit,status)
	if (status .ne. 0) then
	  context = 'could not get ORBIT  parameter'
	  call fcerr(context)
	  go to 999
	endif

C      get the seed for the random number generator
	call uclgsi('seed',iseed,status)
	if (status .ne. 0) then
	  context = 'could not get SEED parameter'
	  call fcerr(context)
	  go to 999
	endif

C      Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end

C **************************************************************************
C SUBROUTINE:
C      fakemem
C
C DESCRIPTION:      
C      Allocate dynamic memory and call routines to create the fake data
C      
C AUTHOR:
C      James Lochner  6/10/94
C
C MODIFICATION HISTORY:
C      8/18/94 - update so it passes the receives and passes the SPANUNIT
C                 parameter
C      3/28/95 - increased maxsize from 86,400 to 500,000
C NOTES:
C
C USEAGE      
C      call fakemem(type,bin,binunit,cts,ctsunit,noise,sigma,span,
C              spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,outfile)
C      
C ARGUMENTS:
C      type     - type of light curve: Event or Binned
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      cts      - average intensity
C      ctsunit  - units for cts (in Counts or Count/s)
C      noise    - noise option
C      sigma    - sigma for Gaussian noise
C      span     - requested total live time
C      spanunit - units for span (s=sec, d=days)
C      tstart   - observation start time (in MJD)
C      hexte    - flag for using HEXTE standard 32-s dwell pattern
C      nsegs    - number of data segments
C      gaplenth - average length of each data gap (units of spanunit)
C      orbit    - option to use typical XTE orbit for time sampling
C      iseed    - seed for random number generator
C      outfile  - name of output FITS file
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C      maxsize  - array size limit for time, rate & error arrays
C      
C CALLED ROUTINES:
C      subroutine udmget  - allocate dynamic memory
C      subroutine udmfre  - free the dynamic memory
C      subroutine fevent  - create an event list and write the result
C      subroutine fbinned - create a binned light curve and write the result 
C
C ***************************************************************************
      SUBROUTINE fakemem(type,bin,binunit,cts,ctsunit,noise,sigma,span,
     &             spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,
     &             outfile)

C declarations of the arguments
	character*(*) outfile, binunit, ctsunit, type, orbit, spanunit
        real cts, sigma
	double precision bin, tstart, span, gaplenth
	integer noise, nsegs, iseed
        logical hexte

	character(80) context
	integer status
        integer maxsize
        
C      declare the pointers for dynamic memory arrays
        integer time, rate, error
        
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

      status=0
      time = 0
      rate = 0
      error = 0
C      allocate memory for the TIME array
      maxsize = 500000
      call udmget(maxsize,7,time,status)
      if (status .ne. 0) then
         context = ' Error allocating dynamic memory for TIME'
         call fcerr(context)
         go to 998
      endif
      
c      Call appropriate subroutine, allocating RATE and ERROR if necessary 
      if (type .eq. 'E') then
           call fevent(bin,binunit,cts,ctsunit,span,spanunit,tstart,
     &        hexte,nsegs,gaplenth,orbit,iseed,outfile,maxsize,
     &        MEMD(time))
        else
           call udmget(maxsize,6,rate,status)
           if (status .ne. 0) then
              context = ' Error allocating dynamic memory for RATE'
              call fcerr(context)
              go to 998
           endif

           call udmget(maxsize,6,error,status)
           if (status .ne. 0) then
              context = ' Error allocating dynamic memory for ERROR'
              call fcerr(context)
              go to 998
           endif
           call fbinned(bin,binunit,cts,ctsunit,noise,sigma,span,
     &          spanunit,tstart,hexte,nsegs,gaplenth,orbit,iseed,
     &          outfile,maxsize,MEMD(time),MEMR(rate),MEMR(error))
        endif

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

        if (type . eq. 'B') then
           call udmfre(rate,6,status)
           if (status .ne. 0) then
              context = 'Error freeing dynamic memory for RATE'
              call fcerr(context)
           endif
           call udmfre(error,6,status)
           if (status .ne. 0) then
              context = 'Error freeing dynamic memory for ERROR'
              call fcerr(context)
           endif
        endif
        


        return
        end
C **************************************************************************
C SUBROUTINE:
C      fevent
C
C DESCRIPTION:      
C      Creates an event list with a given mean count rate, using a Poisson
C       distribution of times between the events
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/28/94 - Added option to give HEXTE modulation of 32-s on, 32-s off
C      9/12/94 - utilize the SPANUNIT parameter as the units for both the
C                 total time span of the observation (SPAN), and of the
C                 average length of the data segments (SEGLENGTH).
C      9/13/94 - corrected SUMLENGTH to give correct value when
C                 number of data points exceed MAXPTS
C      9/19/94 - implemented low-earth orbit option for data sampling
C      9/28/94 - Changed definition of SPAN to be requested live time
C                 on source.  Accordingly, replace SEGLENGTH with GAPLENGTH,
C                 the average size of data gaps.
C
C NOTES:
C
C USEAGE:      
C      call fevent(bin,binunit,rate,rateunit,span,spanunit,tstart,nsegs,
C                   gaplength,orbit,outfile,maxsize,time)
C      
C ARGUMENTS:
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      rate     - average count rate
C      rateunit - units for cts (in Counts or Count/s)
C      span     - total exposure time
c      spanunit - units for span (s=sec, d=days)
C      tstart   - observation start time (in MJD)
C      hexte    - flag for using HEXTE std 32-s dwell pattern
C      nsegs    - number of data segments
C      gaplength- average length of each data gap (in units of spanunit)
C      orbit    - option to use typical XTE orbit for time sampling
C      iseed    - seed for random number generator
C      outfile  - name of output FITS file
C      maxsize  - the maximum allowed number of data points 
C      time     - array of time assignments
C
C PRIMARY LOCAL VARIABLES:
C      npts     - total number of data points in light curve
C      seglength- average length of each data segment
C      length   - array of lengths of each data segment
C      start    - array of start times of each data segment
C      window   - average length of the data gaps
C      sumlength- total live time
C      context  - error message
C
C CALLED ROUTINES:
C      subroutine xorbit   - compute start times and durations of data segments
C                               according to a typical XTE orbit
C      subroutine ritevent - writes the event list 
C      function   ft_ran2 - returns a  uniformly distributed random variable
C      subroutine fcecho  - prints a string to the stdio device
C      function comnum    - converts a real*8 to a string, concatenating to input
C      
C **************************************************************************

      subroutine fevent(bin,binunit,rate,rateunit,span,spanunit,tstart,
     &     hexte,nsegs,gaplength,orbit,iseed,outfile,maxsize,time)
      
C start with initial declarations
      integer maxsize
 
      character*(*) outfile
      character(1) orbit
      character*(*) binunit, rateunit, spanunit
      double precision bin,tstart, span, gaplength
      real rate
      integer nsegs, iseed
      logical hexte
      
      double precision time(maxsize), start(100), length(100)
      double precision sumlength, seglength, tstop
      integer i, j, k, npts
      real r, ft_ran2
      real modulate
      character(80) context, comnum
      character(67) comment

C     shut up compiler warnings
      j=0
      modulate=0.0
      
C      Set up random number generator
      r = ft_ran2(iseed)

C      Convert span and gaplength to same units as bin, if necessary
      if (spanunit .ne. binunit) then         
         if (binunit .eq. 's') then
            span = 86400. * span
            gaplength = 86400. * gaplength
         else
            span = span / 86400.
            gaplength = gaplength / 86400.
         endif
      endif
      
C      If necessary, convert "rate" into a true rate, count/s or count/d
C      according to binunit.
      if (rateunit .eq. 'count') rate = rate / bin

C      if necessary, alter the units on the rate
      if (rateunit. eq. 'count/s' .and. binunit .eq. 'd')
     &     rateunit = 'count/d'      

C      check for HEXTE mode, putting in modulation (in sec) if necessary
      if (HEXTE) modulate = 64.
            
C      Assign lengths and start times of each segment.  Start times are
C      configured to match time bin boundaries.
      
      if (orbit .eq. 'y') then
         call xorbit(binunit,span,nsegs,start,length,iseed)
         sumlength = 0.0
         do j = 1,nsegs
            sumlength = sumlength + length(j)
         end do
      else if (nsegs .eq. 1) then
         length(1) = span
         start(1) = 0.0
         sumlength = span
      else
         sumlength = 0.
         seglength = span / nsegs
         do j = 1,nsegs
            length(j) = (ft_ran2(iseed) + 0.5) * seglength
            sumlength = sumlength + length(j)
         end do
         if (gaplength .le. 0.) then
            context = 'WARNING: no data gaps'
            call fcerr(context)
            gaplength = 0.0
         endif
         start(1) = 0.0
         do j = 2,nsegs
            start(j) = start(j-1) + length(j-1)
     &           + (ft_ran2(iseed) + 0.5) * gaplength
            start(j) = aint(start(j)/bin) * bin
         end do
      endif

c      Now compute the times and the data
c        (For HEXTE, recompute the live time)
      if (hexte) then
         i = 1
         sumlength = 0.
         do 100 k = 1, nsegs
            if (dmod(length(k),64.0D+00) .le. 32) then
               sumlength = sumlength + int(length(k)/64.0D+00) * 32
     &              + dmod(length(k),64.0D+00) 
            else
               sumlength = sumlength + (int(length(k)/64.) + 1)*32
            endif
            time(i) = start(k) - alog(1.-ft_ran2(iseed)) / rate
            i = i + 1
            j = 0
            do while (
     &           time(i-1) .le. start(k) + length(k) .and.
     &           i .le. maxsize)
               do while (time(i-1) .le. start(k) + (j+0.5)*modulate
     &              .and. time(i-1) .le. start(k) + length(k)
     &              .and. i .le. maxsize)
                  time(i) = time(i-1) - alog(1.-ft_ran2(iseed)) / rate
                  time(i-1) = aint(time(i-1)/bin) * bin
                  i = i + 1
               end do
               j = j + 1
               time(i-1) = start(k) + j*modulate
     &              - alog(1.-ft_ran2(iseed)) / rate
            end do
            i = i - 1
100      continue
         npts = i - 1

      else
         i = 1
         j = 1
         do while (j .le. nsegs .and. i .lt. maxsize)
            time(i) = start(j) - alog(1.-ft_ran2(iseed)) / rate
            i = i + 1
            do while (time(i-1) .le. start(j) + length(j) .and.
     &           i .le. maxsize)
               time(i) = time(i-1) - alog(1.-ft_ran2(iseed)) / rate
               time(i-1) = aint(time(i-1)/bin) * bin

               i = i + 1
            end do
            i = i - 1
            j = j + 1
200      end do
         npts = i - 1
      endif

      tstop = start(nsegs) + length(nsegs)
      
C       If NPTS is at maxsize, Write out a warning message and correct the
C      live timeif npts is at maxsize
       if (i .ge. maxsize) then 
          comment = 'number of pts too large for this program '
     &        //'- truncated to max pts of '
          context = comnum(comment,dble(maxsize))
          call fcerr(context)
          time(i) = aint(time(i)/bin) * bin
          npts = i
          do 250 k = nsegs,j-1,-1
             sumlength = sumlength - length(k)
250       continue
          sumlength = sumlength + (time(npts) - start(j-1))
       endif
               
C      convert span, sumlength, tstop, and gaplength back into days,
C        if necessary
      if (spanunit .ne. binunit) then
         if (binunit .eq. 's') then
            span = span / 86400.
            sumlength = sumlength / 86400.
            tstop = tstop / 86400.
            gaplength = gaplength / 86400.
         else
            span = 86400. * span
            sumlength = 86400. * sumlength
            tstop = 86400. * tstop
            gaplength = 86400. * gaplength
         endif
      endif
         
C      If necessary, convert "rate" back into count
      if (rateunit .eq. 'count') rate = rate * bin

C      now write out the data 
      call ritevent(outfile,npts,sumlength,tstart,tstop,spanunit,
     &     binunit,rate,rateunit,time)

      return
      end
C **************************************************************************
C SUBROUTINE:
C      fbinned
C
C DESCRIPTION:      
C      Creates a binned fake light curve with a given mean
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      5/16/94 - Fill the time array regardless of whether there are data
C                gaps
C      7/19/94 - Added option to give HEXTE modulation of 16-s on, 16-s off
C                 (Later changed to 32-s on, 32-s off)
C      8/18/94 - utilize the SPANUNIT parameter as the units for both the
C                 total time span of the observation (SPAN), and of the
C                 average length of the data segments (SEGLENGTH)
C      8/30/94 - implemented low-earth orbit option for data sampling
C      9/28/94 - Changed definition of SPAN to be requested live time
C                 on source.  Accordingly, replace SEGLENGTH with GAPLENGTH,
C                 the average size of data gaps.
C     
C NOTES:
C
C USEAGE:      
C      call fbinned(bin,binunit,cts,ctsunit,noise,sigma,span,spanunit,
C                   tstart,hexte,nsegs,gaplength,orbit,iseed,outfile
C                   maxsize,time,rate,error)
C      
C ARGUMENTS:
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      cts      - average intensity
C      ctsunit  - units for cts (in Counts or Count/s)
C      noise    - noise option
C      sigma    - sigma for Gaussian noise
C      span     - requested total live time 
C      spanunit - units for span (s=sec, d=days)
C      tstart   - observation start time (in MJD)
C      hexte    - flag for using HEXTE std 32-s dwell pattern
C      nsegs    - number of data segments
C      gaplength- average length of each data gap (units of spanunit)
C      orbit    - option to use typical XTE orbit for time sampling
C      iseed    - seed for random number generator
C      outfile  - name of output FITS file
C      maxsize  - the maximum allowed number of data points 
C      time     - array of time assignments
C      rate     - array of intensity values
C      error    - array of uncertainties on intensity values
C
C PRIMARY LOCAL VARIABLES:
C      sbin     - bin size in seconds
C      npts     - total number of data points in light curve
C      seglength- average length of each data segment
C      length   - array of lengths of each data segment
C      start    - array of start times of each data segment
C      window   - average length of the data gaps
C      context  - error message
C
C CALLED ROUTINES:
C      subroutine xorbit  - compute start times and durations of data segments
C                             according to a typical XTE orbit
C      subroutine compute - computes the intensities, with noise option
C      subroutine ritebin - writes the binned light curve
C      function   rt_ran2 - returns a  uniformly distributed random variable
C      subroutine fcecho  - prints a string to the stdio device
C      function comnum    - converts a real*8 to a string, concatenating to input
C      
C **************************************************************************

      subroutine fbinned(bin,binunit,cts,ctsunit,noise,sigma,span,
     &     spanunit,tstart,hexte,nsegs,gaplength,orbit,iseed,outfile,
     &     maxsize,time,rate,error)
      
C      start with initial declarations
      integer maxsize


      character*(*) outfile, ctsunit, binunit, spanunit, orbit
      double precision bin, tstart, span, gaplength
      real cts, sigma
      integer noise, nsegs, iseed
      logical hexte

      real rate(maxsize), error(maxsize), modulate
      double precision time(maxsize), start(100), length(100)
      double precision sbin, sumlength, seglength
      integer i, j, k, m, npts(100), tpts, nmod, n2mod, psegs
      real r, ft_ran2
      character(80) context, comnum
      character(68) comment
      logical trunc
      
C      Set up random number generator
      r = ft_ran2(iseed)
      
C      compute the bin size in seconds, converting from days if necessary
      if (binunit .eq. 's') then
         sbin = bin
      else
         sbin = 86400. * bin 
      endif
      
C      if intensity is in count/s, convert it to counts
       if (ctsunit .eq. 'count/s') then
         cts = cts * sbin
         sigma = sigma * sbin
      end if

C      check for HEXTE mode, putting in modulation (in sec) if necessary
      if (HEXTE) then
         modulate = 64.
         nmod = nint(modulate/sbin)
         n2mod = nint(0.5*modulate/sbin)
      else
         modulate = sbin
         nmod = 1
         n2mod = 1
      endif

      
C      Convert span and gaplength to same units as bin, if necessary
      if (spanunit .ne. binunit) then         
         if (binunit .eq. 's') then
            span = 86400. * span
            gaplength = 86400. * gaplength
         else
            span = span / 86400.
            gaplength = gaplength / 86400.
         endif
      endif
      
C      Compute start times and lengths for the data segments
      if (orbit .eq. 'y') then
         call xorbit(binunit,span,nsegs,start,length,iseed)
         do j = 1,nsegs
            start(j) = aint(start(j)/bin) * bin
         end do
      else if (nsegs .eq. 1) then
         npts(1) = span / bin
         length(1) = span
         start(1) = 0.0
      else
         sumlength = 0.
         seglength = span / nsegs
         do j = 1,nsegs
            length(j) = (ft_ran2(iseed) + 0.5) * seglength
            sumlength = sumlength + length(j)
         end do
         if (gaplength .le. 0.) then
            context = 'WARNING: no data gaps'
            call fcerr(context)
            gaplength = 0.0
         endif
         start(1) = 0.0
         do j = 2,nsegs
            start(j) = start(j-1) + length(j-1)
     &           + (ft_ran2(iseed) + 0.5) * gaplength
            start(j) = aint(start(j)/bin) * bin
         end do
      endif

C      Now check for the total number of data points
      tpts = 0
      psegs = 0
      if (hexte) maxsize = 2 * maxsize
      do k = 1,nsegs
         psegs = psegs + 1
         if (mod(length(k),bin) .eq. 0) then
            npts(k) = int(length(k)/bin)
         else
            npts(k) = int(length(k)/bin) + 1
         endif
         tpts = tpts + npts(k)
         if (tpts .ge. maxsize) then
            comment = 'number of pts too large for this program '
     &           //'- truncated to max pts of  '
            context = comnum(comment,dble(maxsize))
            call fcerr(context)
            length(k) = length(k) - (tpts - maxsize)*bin
            npts(k) = npts(k) - (tpts - maxsize)
            tpts = maxsize
            trunc = .true.
            go to 20
         endif
      end do

c      Reset the number of segments
20    nsegs = psegs
      
c      Now compute the times and the data
      if (hexte) then
         i = 0
         do 100 k = 1, nsegs
            do 50 j = 0,npts(k)-1,nmod
               i = i + 1
               time(i) = start(k) + j * bin
               call compute(i,iseed,noise,cts,sigma,rate,error)
               m = 1
               do while (m .le. n2mod-1
     &              .and. time(i) .lt. start(k) + length(k)
     &              .and. i .lt. maxsize)
                  m = m + 1
                  i = i + 1 
                  time(i) = time(i-1) + bin
                  call compute(i,iseed,noise,cts,sigma,rate,error)
               end do
50          continue
100      continue
         tpts = i - 1

      else
         i = 1
         do 200 j = 1,nsegs
            time(i) = start(j)
            call compute(i,iseed,noise,cts,sigma,rate,error)
            do while (time(i) .lt. start(j) + length(j)
     &           .and. i .lt. maxsize)
               i = i + 1
               time(i) = time(i-1) + bin
               call compute(i,iseed,noise,cts,sigma,rate,error)
            end do
200      continue
         tpts = i - 1
      endif

C      convert span and gaplength back into days, if necessary
      if (spanunit .ne. binunit) then
         if (binunit .eq. 's') then
            span = span / 86400.
            gaplength = gaplength / 86400.
         else
            span = 86400. * span
            gaplength = 86400. * gaplength
         endif
      endif
      
C      Convert result to count/s, if necessary
      if (ctsunit .eq. 'count/s') then
         do j = 1,tpts
            rate(j) = rate(j) / sbin
            error(j) = error(j) / sbin
         end do
      endif

C      Restore Maxsize value
      if (hexte) maxsize = maxsize / 2
      
C      Now write the data
C      Convert input mean back into its original units, if necessary
      if (ctsunit .eq. 'count/s') then
         cts = cts / sbin
         sigma = sigma / sbin
      endif
      
      call ritebin(outfile,tpts,bin,binunit,nsegs,noise,sigma,
     &      cts,ctsunit,tstart,span,spanunit,time,rate,error)
      
      return
      end
C *************************************************************************
C SUBROUTINE:
C      xorbit
C
C DESCRIPTION:      
C      computes sampling according to a typical XTE orbit, taking account of
C       earth occultations and SAA passages
C      
C AUTHOR:
C      Dr. James Lochner  8/30/94
C
C MODIFICATION HISTORY:
C      9/28/94 - Changed definition of SPAN to be requested live time
C                 on source.
C      10/15/94- Modified routine to calculate SAA passages for observations
C                 spanning more than 1 day.
C      
C NOTES:
C
C USEAGE:      
C      call xorbit(binunit,span,nsegs,start,length,iseed)
C
C ARGUMENTS:
C      binunit  - units for the bin size (s=sec, d=days)
C      span     - requested total live time
C      nsegs    - number of data segments
C      length   - array of lengths of each data segment
C      start    - array of start times of each data segment
C      iseed    - seed for random number generator
C
C      PRIMARY LOCAL VARIABLES:
C      period   - orbital period of the satellite
C      occlength- duration of earth occultation
C      occult1  - time of the first earth occultation
C      occsig   - sigma to use for determining time of first earth occultation
C      saaorbit - orbit number in which 1st SAA passage occurs
C      sigstart - sigma used for determining time of 1st SAA passage
C      midsaa   - array of the mid times of SAA passage
C      begsaa   - start time of particular SAA passage
C      endsaa   - end time of particular SAA passage
C      saalength- array of median lengths of SAA passages.
C      sumlength- sum of durations of data segments
C      ndays    - number of days spanned by the observation
C      context  - error message
C
C CALLED ROUTINES:
C      function   rt_ran2 - returns a  uniformly distributed random variable
C      subroutine fcecho  - prints a string to the stdio device
C      function comnum    - converts a real*8 to a string, concatenating to input
C      
C **************************************************************************

      subroutine xorbit(binunit,span,nsegs,start,length,iseed)

      double precision span, start(100), length(100)
      character*(*) binunit
      integer nsegs, iseed

      double precision period, occlength
      real midsaa(10), saalength(9), begsaa, endsaa, lengthsaa
      real saastart, sigstart, sigsaa, sumlength
      integer saaorbit, i, j, k, ndays
      real occult1, occsig, gauss
      real ft_ran2
      character(80) context
      data saalength/720.,1440.,1740.,1860.,1800.,1500.,1260.,
     $     1140.,780./
      
      if (span .lt. 4000) then
         context='Observation may be done without an earth occultation'
         call fcerr(context)
         return
      endif

c      put constants in unit of the bin size
      if (binunit .eq. 's') then
         period = 6000.
         occlength = 1800.
         occult1 = 4000.
         occsig = 200.
         sigsaa = 30.
      else
         period = 6000. / 86400.
         occlength = 1800. / 86400.
         occult1 = 4000. / 86400.
         occsig = 200. / 86400.
         sigsaa = 30. / 86400.
         do i = 1,9
            saalength(i) = saalength(i) / 86400.
         end do
      endif
      
c      First compute times of the earth occultations
c      occult1 = time of the first earth occultation
      length(1) = gauss(occult1,occsig,iseed)
      start(1) = 0.0
      sumlength = length(1)
      i = 1
      do while (sumlength .lt. span)
         i = i + 1
         start(i) = start(i-1) + length(i-1) + occlength
         length(i) = period - occlength
         sumlength = sumlength + length(i)
      end do
      nsegs = i
      
C      Now include SAA passages:
C       Choose the orbit containing the first SAA passage
C       Compute number of days spanned by observation (use 15 orbits/day)
C       For each day, randomize start time of initial SAA passage
C       Subsequent SAA passages occur every orbit, but length varies
      saaorbit = ft_ran2(iseed) * min(15,nsegs) + 1
      ndays = nsegs / 15
      do k = 0,ndays
         j = k*15 + saaorbit
         if (j .le. nsegs) then
            i = 1
            saastart = start(j) + length(j) + 0.5 * occlength
            sigstart = 0.1 * occlength
            midsaa(i) = gauss(saastart,sigstart,iseed)
            do while (j .le. nsegs .and. i .le. 9)
               if (i .gt. 1) midsaa(i) = midsaa(i-1) + period
               lengthsaa = gauss(saalength(i),sigsaa,iseed)
               begsaa = midsaa(i) - 0.5 * lengthsaa
               endsaa = midsaa(i) + 0.5 * lengthsaa
               if (begsaa .lt. start(j) + length(j))
     &           length(j) = length(j) - (start(j) + length(j) - begsaa)
               if (endsaa .gt. start(j+1))
     &           start(j+1) = endsaa
               i = i + 1
               j = j + 1
            end do
         end if
      end do

      return
      end
C **************************************************************************
C SUBROUTINE:
C      compute
C
C DESCRIPTION:      
C      Computes intensities, using noise option
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      7/30/96 - When noise=0, assign error=0.0 explicitly.
C                 Also, increase maxsize to 500000 for consistency
C                 with rest of code.
C NOTES:
C
C USEAGE:      
C      call compute(j,iseed,noise,cts,sigma,rate,error)
C      
C ARGUMENTS:
C      j        - array index
C      iseed    - seed for random number generator
C      noise    - noise option
C      cts      - avg intensity
C      sigma    - sigma for Gaussian noise
C      rate     - array of intensity values
C      error    - array of uncertainty values
C      
C PRIMARY LOCAL VARIABLES:
C      maxsize  - maximum allowed size of rate and error arrays
C      
C CALLED ROUTINES:
C      function poiss - returns poisson distributed random number
C      function gauss - returns Guassian distributed random number
C *******************************************************************************
      
      subroutine compute(j,iseed,noise,cts,sigma,rate,error)

      integer maxsize
      parameter (maxsize = 500000)
      
      integer j, noise, poiss, iseed
      real cts, sigma, gauss, rate(maxsize), error(maxsize)
      
      if (noise .eq. 0) then
         rate(j) = cts
         error(j) = 0.0
      else if (noise .eq. 1) then
         rate(j) = poiss(cts,iseed)
         error(j) = sqrt(rate(j))
      else if (noise .eq. 2) then
         rate(j) = gauss(cts,sigma,iseed)
         error(j) = sigma
      endif

      return
      end





C ***********************************************************************      
C SUBROUTINE:
C      ritevent
C
C DESCRIPTION:      
C      Writes an event list to a FITS file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      9/12/94 - writes value of MDJREF to the header, and sets TIMESYS = 'TT'
C                 in header.  TSTART set to 0.
C
C NOTES:
C
C USEAGE:      
C      call ritevent(outfile,npts,ontime,tstart,span,spanunit,binunit,
C     &     rate,rateunit,time)
C
C ARGUMENTS:
C      outfile  - name of output FITS file
C      npts     - total number of data points in light curve
C      ontime   - total live time
C      tstart   - observation start time (in MJD)
C      tstop    - observation stop time  
C      spanunit - units for span/tstop (s=sec, d=days)
C      binunit  - units of the bin size
C      rate     - average intensity
C      rateunit - units for rate (in Counts or Count/s)
C      time     - array of time assignments
C
C PRIMARY LOCAL VARIABLES:
C      ounit    - output unit number
c      fstatus  - fitsio status number
c      bitpix   - number of bits per pixel
c      naxis    - number of dimensions in the FITS array
c      naxes    - size of each dimension in the FITS array
c      tfields  - number of columns in FITS table
c      rowlen   - length of the table row, in bytes
c      frow     - beginning row number
c      felem    - beginning pixel of the element vector
C      mjdrefi  - integer part of mjd reference date
C      mjdreff  - fractional part of mjd reference date
c      istarti  - integer portion of start time
c      startf   - fractional portion of start time
c      istopi   - integer portion of stop time
c      stopf    - fractional portion of stop time
c      context  - error messages
c      history  - string for history keywords
C      ttype    - array of column names
C      tform    - array of column data types
C      tunit    - array of column units
C      
C CALLED ROUTINES:
C      subroutine ftgiou - get an available logical unit for i/o
C      subroutine ftfiou - free a logical unit for i/o
C      subroutine ftinit - open and initialize a new FITS file
C      subroutine ftphpr - put the required keywords into the primary header
C      subroutine ftpkyd - put a keyword with a real*8 value int a header
C      subroutine ftpkyj - put a keyword with an interter value into a header
C      subroutine ftpkyl - put a keyword with a logical value into a header
C      subroutine ftpkys - put a keyword with a string value into a header
C      subroutine ftphis - put a HISTORY keyword into a header
C      subroutine ftpdat - put the DATE keyword into a header
C      subroutine ftpdef - define the structure of the primary array
C      subroutine ftbdef - define the structure of a binary table extension
C      subroutine ftcrhd - create a new extension at end of file
C      subroutine ftphbn - put the required keywords into a binary extension 
C      subroutine ftpcld - put real*8 values into a table column
C      subroutine ftclos - close a FITS file opened with ftinit or ftopen
C      subroutine fcerr  - prints message to stderror device
C      subroutine fcerrm - prints fitsio error number and message
C      function comnum    - converts a real*8 to a string, concatenating to input
      
C**************************************************************************

      Subroutine ritevent(outfile,npts,ontime,tstart,tstop,spanunit,
     &     binunit,rate,rateunit,time)

      integer npts
      double precision time(npts), tstart, tstop, ontime
      character*(*) outfile, rateunit, binunit, spanunit
      real rate
      
      integer ounit, fstatus, bitpix, naxis, pc, gc, blocksize
      integer naxes, tfields, rowlen, frow, felem
      integer istarti, istopi, mjdrefi
      double precision mjdref, mjdreff, startf, stopf, tstopm
      logical simple,extend, ex
      character(80) context, history, comnum
      character(24) comment
      character(8) ttype(10), tform(10), tunit(10), extname

      fstatus=0
      call ftgiou(ounit,fstatus)
c      ounit = 16

C      Set MJDREF value
C      default to xte value 
      if (tstart .ge. 0) then
         mjdref = tstart
      else
         mjdref = 49352.000696574074
      endif
      mjdrefi = int(mjdref)
      mjdreff = mjdref - mjdrefi

c      Create the new file (FTINIT)
      inquire(FILE=outfile,EXIST=ex)
      if (ex) then
         open(UNIT=ounit,FILE=outfile,STATUS='unknown')
         close(UNIT=ounit,STATUS='delete')
      endif
      blocksize = 1
      call ftinit(ounit,outfile,blocksize,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not initialize new FITS file'
         call fcerr(context)
         go to 999
      endif
      
c      Write the Required Primary Header Keywords 
      simple = .true.
      extend = .true.
      bitpix = 8
      naxis = 0
      naxes = 0 
      pc = 0 
      gc = 0
      call ftphpr(ounit,simple,bitpix,naxis,naxes,pc,gc,extend,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write required primary keywords'
         call fcerr(context)
         go to 999
      endif
      
c      Write the additional Primary Header Keywords 
      call ftpkys(ounit,'CONTENT','LIGHT CURVE','light curve file',
     &             fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write CONTENT primary keywords'
         call fcerr(context)
         go to 999
      endif
      call ftpkys(ounit,'ORIGIN','NASA/GSFC/XTE/GOF',
     &     'origin of fits file',fstatus)
      call ftpdat(ounit,fstatus)
      call ftpkys(ounit,'TELESCOP','XTE',
     &  'Telescope (Mission) name',fstatus)
      call ftpkys(ounit,'INSTRUME','PCA',
     &  'Instrument used for observation',fstatus)
      call ftpkys(ounit,'TIMVERSN','OGIP/93-003',
     &     'OGIP memo number for file format',fstatus)
      call ftpkys(ounit,'CREATOR','FAKELC/XTE/GOF',
     & 'Program which produced this file',fstatus)
      call ftpkyd(ounit,'MJDREF',mjdref,16,'Reference MJD',fstatus)
      call ftpkyd(ounit,'TSTART',mjdref,13,
     &        'Observation start time',fstatus)

      if (spanunit .eq. 's') then
         tstopm = mjdref + tstop / 86400.
      else
         tstopm = mjdref + tstop
      endif
      call ftpkyd(ounit,'TSTOP',tstopm,13,
     &        'Observation stop time',fstatus)

      call ftpkys(ounit,'OBJECT','FAKE',
     &        'Object name',fstatus)
 
c      Define structure of primary array (FTPDEF)
      call ftpdef(ounit,bitpix,naxis,naxes,pc,gc,fstatus)
      
c      Create Extension (FTCRHD)
      call ftcrhd(ounit,fstatus)
      tfields = 1
      rowlen = 8
      ttype(1) = 'TIME'
      tform(1) = 'D'
      tunit(1) = 's'
      
c      Write Required Header Keywords (FTPHTB or FTPHBN)
      extname = 'EVENTS'
      call ftphbn(ounit,npts,tfields,ttype,tform,tunit,extname,
     &     pc,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write required extension keywords'
         call fcerr(context)
         go to 999
      endif
      
c      Write additional keywords (FTPKYx)
      call ftpkys(ounit,'TIMESYS','TT','Time system is TT',fstatus)

      call ftpkyj(ounit,'MJDREFI',mjdrefi,
     &     'Integer portion of Reference MJD',fstatus)
      call ftpkyd(ounit,'MJDREFF',mjdreff,8,
     &     'Fractional portion of Reference MJD',fstatus)

      call ftpkys(ounit,'TIMEUNIT',spanunit,
     &        'unit for TSTARTI/F and TSTOPI/F, TIMEZERO',fstatus)

      call ftpkys(ounit,'CLOCKCOR','UNKNOWN',
     &        'Has the time been correct to UT',fstatus)
         
      call ftpkys(ounit,'TIMEREF','SATELLITE',
     &        'barycentric correction applied to times',fstatus)

c    This is calculation of integer and fractional parts of 
c      the observation start time
        istarti = 0.0
	startf = 0.0
        
      call ftpkyj(ounit,'TSTARTI',istarti,
     &        'integer portion of start time',fstatus)
         
      call ftpkyd(ounit,'TSTARTF',startf,8,
     &        'fractional observation start time',fstatus)

      istopi= int(tstop)
      stopf = tstop - istopi
        
      call ftpkyj(ounit,'TSTOPI',istopi,
     &        'integer observation stop time',fstatus)
        
      call ftpkyd(ounit,'TSTOPF',stopf,8,
     &        'fractional observation stop time',fstatus)

      call ftpkyd(ounit,'TIERRELA',0.0d0,8,
     &        'relative time error',fstatus)
         
      call ftpkyd(ounit,'TIERABSO',0.0d0,8,
     &        'absolute time error',fstatus)
         
      call ftpkyd(ounit,'ONTIME',ontime,8,
     &        'time on source',fstatus)
         
      call ftpkyl(ounit,'BACKAPP',.FALSE.,
     &        'background is subtracted',fstatus)
         
      Call ftpkyl(ounit,'DEADAPP',.FALSE.,
     &        'deadtime correction applied',fstatus)
         
      call ftpkyl(ounit,'VIGNAPP',.FALSE.,
     &        'vignetting or collimator applied',fstatus)
         
      call ftpkyj(ounit,'MINCHAN',0,
     &        'minimum channel included in bin',fstatus)
         
      call ftpkyj(ounit,'MAXCHAN',255,
     &        'maximum channel include in bin',fstatus)
         
c      Write out the mission name.
      call ftpkys(ounit,'TELESCOP','XTE',
     &       'Telescope (mission) name',fstatus)
      
c      Write out the name of the instrument
      call ftpkys(ounit,'INSTRUME','PCA',
     &       'Instrument name',fstatus)

      call ftpkys(ounit,'OBJECT','FAKE',
     &        'OBJECT from the FIRST input file',fstatus)

c         call ftpkye(ounit,'RA',ra,8,
c     &        'RA of First input object',fstatus)
c         print*,'wrote RA-NOM'

c         call ftpkye(ounit,'DEC',dec,8,
c     &        'DEC of First input object',fstatus)
c         print*,'wrote DEC-NOM'
         
c         call ftpkyf(ounit,'EQUINOX',equino,2,
c     &        'Equinox of the FIRST object',fstatus)
c         print*,'wrote EQUINOX'

c         call ftpkys(ounit,'RADECSYS',radecsys,
c     &        'Co-ordinate frame used for equinox',fstatus)
c        print*,'wrote RADECSYS'

c         call ftpkys(ounit,'DATE-OBS',dateobs(1),
c    &        'EARLIEST observation date of files',fstatus)
c         print*,'wrote DATE-OBS'

c         call ftpkys(ounit,'TIME-OBS',timeobs(1),
c     &        'EARLIEST time of all input files',fstatus)
c         print*,'wrote TIME-OBS'

c         call ftpkys(ounit,'DATE-END',dateend(no),
c     &        'LATEST observation date of files',fstatus)
c         print*,'wrote DATE-END'

c         call ftpkys(ounit,'TIME-END',timeend(no),
c     &        'LATEST time of all input files',fstatus)
c         print*,'wrote TIME-END'

c      Write out the origin of this file
      CALL FTPKYS(ounit,'origin','NASA/GSFC',
     &        'origin of fits file',fstatus)
c
c      Write out the creation date of this file...
      call ftpdat(ounit,fstatus)

c      Write HISTORY keywords
      history = 'This file contains a fake light curve'
      call ftphis(ounit,history,fstatus)
      comment = comnum('Input mean = ',dble(rate))
      history = comment // ' '// rateunit
      call ftphis(ounit,history,fstatus)
      if (fstatus .ne. 0) then
         context = 'unable to add HISTORY'
         call fcerr(context)
         goto 999
      endif
 
c      Define extension data structure (FTADEF or FTBDEF)
      call ftbdef(ounit,tfields,tform,pc,npts,fstatus)
      if (fstatus .ne. 0) then
         context = 'could define data structure for extension'
         call fcerr(context)
         go to 999
      endif
      
c      Write the Data
      frow = 1
      felem = 1
      call ftpcld(ounit,1,frow,felem,npts,time,fstatus)
      
c      Close file with FTCLOS
      call ftclos(ounit,fstatus)
      call ftfiou(ounit,fstatus)

c      Error handling
999   continue
      if (fstatus .ne. 0) call fcerrm(fstatus)

      return
      end
C ***************************************************************************
C SUBROUTINE:
C      ritebin
C
C DESCRIPTION:      
C      Writes a binned fake light curve to a FITS file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C      5/16/94 - let routine always output a TIME column, regardless of
C                whether the data has gaps or not.
C      8/18/94 - writes value of MDJREF to the header, and sets TIMESYS = 'TT'
C                 in header.  TSTART set to 0.
C      
C NOTES:
C
C USEAGE:      
C      Call ritebin(outfile,npts,bin,binunit,nseg,noise,sigma,
C                   cts,ctsunit,tstart,span,time,rate,error)
C
C ARGUMENTS:
C      outfile  - name of output FITS file
C      npts     - total number of data points in light curve
C      bin      - timing precision (for Event) or bin size (for Binned)
C      binunit  - units for the bin size (s=sec, d=days)
C      nseg     - number of data segments
C      noise    - noise option
C      sigma    - sigma for Gaussian noise
C      cts      - average intensity
C      ctsunit  - units for cts (in Counts or Count/s)
C      tstart   - observation start time (in MJD)
C      span     - requested total live time  
C      spanunit - units for span (s=sec, d=days)
C      time     - array of time assignments
C      rate     - array of intensity values
C      error    - array of uncertainties on intensity values
C
C PRIMARY LOCAL VARIABLES:
C      ounit    - output unit number
C      fstatus  - fitsio status number
C      bitpix   - number of bits per pixel
C      naxis    - number of dimensions in the FITS array
C      naxes    - size of each dimension in the FITS array
C      tfields  - number of columns in FITS table
C      rowlen   - length of the table row, in bytes
C      frow     - beginning row number
C      felem    - beginning pixel of the element vector
C      mjdrefi  - integer part of mjd reference date
C      mjdreff  - fractional part of mjd reference date
C      istarti  - integer portion of start time
C      startf   - fractional portion of start time
C      istopi   - integer portion of stop time
C      stopf    - fractional portion of stop time
C      context  - error messages
C      history  - string for history keywords
C      ttype    - array of column names
C      tform    - array of column data types
C      tunit    - array of column units
C      
C CALLED ROUTINES:
C      subroutine ftgiou - get an available logical unit for i/o
C      subroutine ftfiou - free a logical unit for i/o
C      subroutine ftinit - open and initialize a new FITS file
C      subroutine ftphpr - put the required keywords into the primary header
C      subroutine ftpkyd - put a keyword with a real*8 value int a header
C      subroutine ftpkyj - put a keyword with an interter value into a header
C      subroutine ftpkyl - put a keyword with a logical value into a header
C      subroutine ftpkys - put a keyword with a string value into a header
C      subroutine ftphis - put a HISTORY keyword into a header
C      subroutine ftpdat - put the DATE keyword into a header
C      subroutine ftpdef - define the structure of the primary array
C      subroutine ftbdef - define the structure of a binary table extension
C      subroutine ftcrhd - create a new extension at end of file
C      subroutine ftphbn - put the required keywords into a binary extension 
C      subroutine ftpcld - put real*8 values into a table column
C      subroutine ftpcle - put real*4 values into a table column
C      subroutine ftclos - close a FITS file opened with ftinit or ftopen
C      subroutine fcerr  - prints message to stderror device
C      subroutine fcerrm - prints fitsio error number and message
C      function comnum    - converts a real*8 to a string, concatenating to input
C        
C ***************************************************************************

      Subroutine ritebin(outfile,npts,bin,binunit,nseg,noise,sigma,
     &     cts,ctsunit,tstart,span,spanunit,time,rate,error)

      integer npts

      double precision time(npts), bin, tstart, span
      real cts, sigma, rate(npts), error(npts)
      character*(*)outfile, ctsunit, binunit, spanunit
      integer nseg, noise
      
      integer ounit, fstatus, bitpix, naxis, pc, gc, blocksize
      integer naxes, tfields, rowlen, frow, felem
      integer istarti, istopi, mjdrefi
      double precision mjdref, mjdreff, startf, stopf, tstop
      logical simple,extend, ex
      character(80) context, history, comnum
      character(24) comment
      character(8) ttype(10), tform(10), tunit(10), extname

      fstatus=0
      call ftgiou(ounit,fstatus)
c      ounit = 16

C      Set MJDREF value
C      default to xte value 
      if (tstart .ge. 0) then
         mjdref = tstart
         mjdrefi = int(mjdref)
         mjdreff = mjdref - mjdrefi
      else
         mjdref = 49352.000696574074
         mjdrefi = 49352
         mjdreff = 6.96574074e-04
      endif
      
C      Create the new file 
      inquire(FILE=outfile,EXIST=ex)
      if (ex) then
         open(UNIT=ounit,FILE=outfile,STATUS='unknown')
         close(UNIT=ounit,STATUS='delete')
      endif
      blocksize = 1
      call ftinit(ounit,outfile,blocksize,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not initialize new FITS file'
         call fcerr(context)
         go to 999
      endif
      
C      Write the Required Primary Header Keywords 
      simple = .true.
      extend = .true.
      bitpix = 8
      naxis = 0
      naxes = 0 
      pc = 0 
      gc = 0
      call ftphpr(ounit,simple,bitpix,naxis,naxes,pc,gc,extend,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write required primary keywords'
         call fcerr(context)
         go to 999
      endif
      
C      Write the additional Primary Header Keywords 
      call ftpkys(ounit,'CONTENT','LIGHT CURVE','light curve file',
     &             fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write CONTENT primary keywords'
         call fcerr(context)
         go to 999
      endif
      call ftpkys(ounit,'ORIGIN','NASA/GSFC/XTE/GOF',
     &     'origin of fits file',fstatus)
      call ftpdat(ounit,fstatus)
      call ftpkys(ounit,'TELESCOP','XTE',
     &  'Telescope (Mission) name',fstatus)
      call ftpkys(ounit,'INSTRUME','PCA',
     &  'Instrument used for observation',fstatus)
      call ftpkys(ounit,'TIMVERSN','OGIP/93-003',
     &     'OGIP memo number for file format',fstatus)
      call ftpkys(ounit,'CREATOR','FAKELC/XTE/GOF',
     & 'Program which produced this file',fstatus)
      call ftpkyd(ounit,'MJDREF',mjdref,16,'Reference MJD',fstatus)
      call ftpkyd(ounit,'TSTART',mjdref,13,
     &     'Observation start time',fstatus)

      if (binunit .eq. 's') then
         tstop = mjdref + time(npts)/86400.
      else
         tstop = mjdref + time(npts)
      endif
      call ftpkyd(ounit,'TSTOP',tstop,13,
     &        'Observation stop time',fstatus)

      call ftpkys(ounit,'OBJECT','FAKE',
     &        'Object name',fstatus)
      
C      Define structure of primary array (FTPDEF)
      call ftpdef(ounit,bitpix,naxis,naxes,pc,gc,fstatus)
      
C      Create Extension (FTCRHD)
      call ftcrhd(ounit,fstatus)

C      Determine number and type of columns from values of NOISE
      tfields = 2
      rowlen = 12
      ttype(1) = 'TIME'
      tform(1) = 'D'
      tunit(1) = binunit
      ttype(2) = 'RATE'
      tform(2) = 'E'
      tunit(2) = ctsunit
      if (noise .gt. 0) then
         tfields = tfields + 1
         rowlen = rowlen + 4
         ttype(3) = 'ERROR'
         tform(3) = 'E'
         tunit(3) = ctsunit
      endif

C      Write Required Header Keywords (FTPHTB or FTPHBN)
      extname = 'RATE'
      call ftphbn(ounit,npts,tfields,ttype,tform,tunit,extname,
     &     pc,fstatus)
      if (fstatus .ne. 0) then
         context = 'could not write required extension keywords'
         call fcerr(context)
         go to 999
      endif
      
C      Write additional keywords (FTPKYx)

      call ftpkys(ounit,'TIMESYS','TT','Time system is TT',fstatus)

      call ftpkyj(ounit,'MJDREFI',mjdrefi,
     &     'Integer portion of Reference MJD',fstatus)
      call ftpkyd(ounit,'MJDREFF',mjdreff,8,
     &     'Fractional portion of Reference MJD',fstatus)

      call ftpkys(ounit,'TIMEUNIT',spanunit,
     &        'unit for TSTARTI/F and TSTOPI/F, TIMEZERO',fstatus)

      call ftpkys(ounit,'CLOCKCOR','UNKNOWN',
     &        'Has the time been correct to UT',fstatus)
         
      call ftpkys(ounit,'TIMEREF','SATELLITE',
     &        'barycentric correction applied to times',fstatus)

C    This is calculation of integer and fractional parts of 
C      the observation start time
        istarti = 0
	startf = 0.0
        
      call ftpkyj(ounit,'TSTARTI',istarti,
     &        'integer portion of start time',fstatus)
         
      call ftpkyd(ounit,'TSTARTF',startf,8,
     &        'fractional observation start time',fstatus)

      if (binunit .eq. spanunit) then
         istopi = int(time(npts) + bin)
         stopf = time(npts) + bin - istopi
      else
         if (binunit .eq. 's') then
            istopi= int((time(npts) + bin)/86400.)
            stopf = (time(npts) + bin) / 86400 - istopi
         end if
         if (binunit .eq. 'd') then
            istopi = int((time(npts) + bin) * 86400.)
            stopf = (time(npts) + bin) * 86400  - istopi
         endif
      endif
      call ftpkyj(ounit,'TSTOPI',istopi,
     &        'integer observation stop time',fstatus)
      call ftpkyd(ounit,'TSTOPF',stopf,8,
     &        'fractional observation stop time',fstatus)

C      insert the TIMEZERI and TIMEZERF keywords
      call ftpkyj(ounit,'TIMEZERI',istarti,
     &     'integer zerotime to calculate t(n) bin',fstatus)
      call ftpkyd(ounit,'TIMEZERF',startf,8,
     &     'fractional zerotime to calculate t(n) bin',fstatus)
      
C      Convert bin to TIMEUNIT, if necessary
      if (binunit .ne. spanunit) then
         if (binunit .eq. 's') bin = bin / 86400.
         if (binunit .eq. 'd') bin = 86400. * bin
      endif
      call ftpkyd(ounit,'TIMEDEL',bin,8,
     &        'integration time',fstatus)
         
      call ftpkyd(ounit,'TIERRELA',0.0d0,8,
     &        'relative time error',fstatus)
         
      call ftpkyd(ounit,'TIERABSO',0.0d0,8,
     &        'absolute time error',fstatus)
         
      call ftpkyd(ounit,'ONTIME',npts*bin,8,
     &        'time on source',fstatus)
         
      call ftpkyl(ounit,'BACKAPP',.FALSE.,
     &        'background is subtracted',fstatus)
         
      Call ftpkyl(ounit,'DEADAPP',.FALSE.,
     &        'deadtime correction applied',fstatus)
         
      call ftpkyl(ounit,'VIGNAPP',.FALSE.,
     &        'vignetting or collimator applied',fstatus)
         
      call ftpkyj(ounit,'MINCHAN',0,
     &        'minimum channel included in bin',fstatus)
         
      call ftpkyj(ounit,'MAXCHAN',255,
     &        'maximum channel include in bin',fstatus)
         
C      Write out the mission name.
      call ftpkys(ounit,'TELESCOP','XTE',
     &       'Telescope (mission) name',fstatus)
      
C      Write out the name of the instrument
      call ftpkys(ounit,'INSTRUME','PCA',
     &       'Instrument name',fstatus)

      call ftpkys(ounit,'OBJECT','FAKE',
     &        'OBJECT from the FIRST input file',fstatus)

C      Write out the origin of this file
      CALL FTPKYS(ounit,'origin','NASA/GSFC',
     &        'origin of fits file',fstatus)

C      Write out the creation date of this file...
        call ftpdat(ounit,fstatus)

C      Write HISTORY keywords
      history = 'This file contains a fake light curve'
      call ftphis(ounit,history,fstatus)
      comment = comnum('Input mean = ',dble(cts))
      history = comment //' '// ctsunit
      call ftphis(ounit,history,fstatus)
      if (fstatus .ne. 0) then
         context = 'unable to add HISTORY'
         call fcerr(context)
         goto 999
      endif

      if (noise .eq. 0) then
         history = 'No noise added to light curve'
      else if (noise .eq. 1) then
         history = 'Poisson noise added to light curve'
      else if (noise .eq. 2) then
         history = comnum('Guassian noise with sigma = ',dble(sigma))
      endif
      call ftphis(ounit,history,fstatus)
 
C      Define extension data structure (FTADEF or FTBDEF)
      call ftbdef(ounit,tfields,tform,pc,npts,fstatus)
      if (fstatus .ne. 0) then
         context = 'could define data structure for extension'
         call fcerr(context)
         go to 999
      endif
      
C      Write the Data
      frow = 1
      felem = 1
      call ftpcld(ounit,1,frow,felem,npts,time,fstatus)
      call ftpcle(ounit,2,frow,felem,npts,rate,fstatus)
      if (noise .gt. 0)
     &     call ftpcle(ounit,tfields,frow,felem,npts,error,fstatus)
      
      
c      Close file with FTCLOS
      call ftclos(ounit,fstatus)
      call ftfiou(ounit,fstatus)

c      Error handling
999   continue
      if (fstatus .ne. 0) call fcerrm(fstatus)

      return
      end
