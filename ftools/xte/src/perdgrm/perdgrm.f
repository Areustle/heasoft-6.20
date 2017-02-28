C ***************************************************************************
C SELECTOR TASK
C      perdgrm
C
C FILE:
C      perdgrm.f
C
C DESCRIPTION:      
C      compute a periodogram using Scargle's (1982, ApJ, 263, 835) routine and
C        statistics.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C       June 24, 1996 - added chatter parameter & fixed reading of TIMEZERO
C (MJT)  Jan 09, 1997 - IRIX bug fixed: (tunit(2,3,[4]) not assigned
C	
C NOTES:
C      This routine is designed particularly to handle binned data having
C       uneven intervals between the bins, or binned data having gaps.
C      
C      Implementation is based on that developed by Koji Mukai and Alan Smale      
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile	- name of input FITS file
C      outfile	- name of output FITS file
C      timename - alternate name for time column in infile
C      ratename - alternate name for intensity column in infile
C      errname  - alternate name for error column in infile
C      intervals- min and max times for intervals to be analyzed (in MJD)
C      inres    - frequency resolution
C      rms      - RMS error of fit for subtracting polynomial
C                   (if < 0, max order of polynomial)
C      inminP   - lower period limit 
C      inmaxP   - upper period limit
C      window   - compute window function ?  
C      chatter  - how much to tell user
C      abort    - logical to abort for non-fitsio errors
C      
C CALLED ROUTINES:
C      subroutine gperdgrm - gets parameters from environment
C      subroutine fperdgrm - performs scargle algorithm
C      
C ************************************************************************** 

	Subroutine PERDGM

c start with initial declarations
	character(160) infile, outfile, intervals
        character(20) timename, ratename, errname
	real*8 inminP, inmaxP, rms
        integer inres, chatter
        logical window, inres_l, inmaxP_l
        logical abort
        
        character(40) taskname
        common /task/ taskname

        taskname = 'perdgrm3.6.1'
        abort = .false.
        
c get the parameters from the par file
        call gperdgrm(infile,outfile,timename,ratename,errname,
     &       intervals,inres,inres_l,rms,inminP,inmaxP,inmaxP_l,
     &	     window, chatter)

c Perform the Algorithm:  FPERDGRM
        call fperdgrm(infile,outfile,timename,ratename,errname,
     &       intervals,inres,inres_l,rms,inminP,inmaxP,inmaxP_l,
     &       window,chatter,abort)


c  Exit subroutine

	return
	end

C **************************************************************************
C SUBROUTINE:
C      gperdgrm
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C     June 24, 1996 - added chatter parameter
C     
C NOTES:
C      gperdgrm uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C        SUBROUTINE gperdgrm(infile,outfile,timename,ratename,errname,
C                    times,inres,inres_l,rms,inminP,inmaxP,inmaxP_l,window
C                    chatter)
C      
C ARGUMENTS:
C      infile	- name of input FITS file
C      outfile	- name of output FITS file
C      timename - alternate name for time column in infile
C      ratename - alternate name for intensity column in infile
C      errname  - alternate name for error column in infile
C      times    - min and max times for intervals to be analyzed (in MJD)
C      inres    - frequency resolution (INDEF)
C      inres_l  - logical indicating whether inres is at its INDEF value
C      rms      - RMS error of fit for subtracting polynomial
C                   (if < 0, max order of polynomial)
C      inminP   - lower period limit 
C      inmaxP   - upper period limit (INDEF)
C      inmaxP_l - logical indicating whether inmaxP is at its INDEF value
C      window   - compute window function ?
C      chatter  - how much to tell the user
C      
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsd - get real*8 parameter
C      subroutine uclgst - get string parameter
C      
C ************************************************************************** 

        SUBROUTINE gperdgrm(infile,outfile,timename,ratename,errname,
     &       times,inres,inres_l,rms,inminP,inmaxP,inmaxP_l,window,
     &       chatter)


c start with the declarations
	
	character*(*) infile, outfile, times
	character*(*) timename, ratename, errname
	real*8 inminP, inmaxP, rms 
	character(80) context
	integer chatter, status, inres
        logical window
        logical inres_l,inmaxP_l

        inres_l = .false.
        inmaxP_l = .false.
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
        call uclgst('times',times,status)
        if (status .ne. 0) then
           context = 'could not get TIMES parameter'
           call fcerr(context)
           go to 999
        endif

c get the frequency resolution, allowing for INDEF value
         call uclgsi('inres',inres,status)
         if (status .eq. 3) then
            status = 0
            inres_l = .true.
         endif
         if (status .ne. 0) then
	    context = 'could not get INRES parameter'
	    call fcerr(context)
	    go to 999
         endif
         
c get the RMS error of fit for subtracting polynomial 
	call uclgsd('rms',rms,status)
	if (status .ne. 0) then
	    context = 'could not get RMS parameter'
	    call fcerr(context)
	    go to 999
         endif

c get the lower limit to period range
         call uclgsd('inminP',inminP,status)
         if (status .ne. 0) then
	    context = 'could not get INMINP parameter'
	    call fcerr(context)
	    go to 999
         endif

c get the upper limit to period range, allowing for INDEF value 
         call uclgsd('inmaxP',inmaxP,status)
         if (status .eq. 3) then
            status = 0
            inmaxP_l = .true.
         endif
         if (status .ne. 0) then
	    context = 'could not get INMAXP parameter'
	    call fcerr(context)
	    go to 999
         endif
         
c get the WINDOW logical 
        call uclgsb('window',window,status)
	if (status .ne. 0) then
	  context = 'could not get WINDOW parameter'
	  call fcerr(context)
	  go to 999
        endif
       
C     get the chatter parameter
         call uclgsi('chatter',chatter,status)
         if (status .ne. 0) then
            context = 'could not get CHATTER parameter'
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
C ********************************************************************************
C SUBROUTINE:
C      fperdgrm
C
C DESCRIPTION:
C      organize the subroutines to carry out the Scagle algorithm
C
C AUTHOR
C      James Lochner  4/29/94
C      
C MODIFICATION HISTORY
C      6/16/94 - implemented dynamic memory allocation
C      6/24/96 - added chatter parameter (to pass to anlzinput in dfourier)
C     
C NOTES:
C
C USEAGE:
C      call fperdgrm(infile, outfile,  timename, ratename, errname,
C                  intervals, inres, inres_l, rms, inminP, inmaxP,
C                  inmaxP_l, window, chatter, abort)
C
C ARGUMENTS:      
C      infile   - name of input FITS file
C      outfile  - name of output FITS file
C      timename - alternate name for time column in infile
C      ratename - alternate name for intensity column in infile
C      errname  - alternate name for error column in infile
C      intervals- min and max times for intervals to be analyzed (in MJD)
C      inres    - frequency resolution  (INDEF)
C      inres_l  - logical indicating whether inres is at its INDEF value
C      rms      - RMS error of fit for subtracting polynomial
C                   (if < 0, max order of polynomial)
C      inminP   - input lower period limit
C      inmaxP   - input upper period limit  (INDEF)
C      inmaxP_l - logical indicating whether inmaxP_l is at its INDEF value
C      window   - compute window function ?
C      chatter  - how much to tell user
C      abort    - logical indicating non-fitsio error
C
C PRIMARY LOCAL VARIABLES:
C      inopen   - logical indicating open status of input file
C      outopen  - logical indicating open status of output file
C      nrows    - total number of data points in the input file
C      absmin_t - absolute minimum time in the input data
C      absmax_t - absolute maximum time in the input data
C      timecol  - logical indicating the presence of a time column in input file
c      maxsize  - largest allowed array size for input time series
C      time     - pointer to array of input time values
c      rate     - pointer to array of input intensity values
c      error    - pointer to array of input uncertainty values
c      tunit    - units of the time array
C      runit    - units of the rate array
c      drate    - pointer to array of real*8 values of the rate array
c      nintervals- number of time intervals to be analyzed
c      mintime  - array of start times of the desired intervals
c      maxtime  - array of stop times of the desired intervals
c      nofreq   - maximum number of frequencies which can be analyzed
c      nf       - number of frequencies
c      freq     - pointer to array of frequency values
c      power    - pointer to array of fourier power values
c      faps     - pointer to array of false alarm probability values
c      pwindow  - pointer to array of fourier power of the window function
c      minP     - actual minimum period used in analysis
C      maxP     - actual maximum period used in analysis
c      recper   - period having the maximum fourier power
c      recamp   - amplitude of period having the maximum fourier power
c      rect0    -
c      clevel   - false alarm probability for period having max fourier power
C      deg      - degree of polynomial subtracted from input light curve
C
C CALLED ROUTINES:
C      subroutine fxdread - read light curve from a FITS file
c      subroutine fcgrgd  - get the number of real*8 ranges and their limits
c      subroutine dfourier - perform the scargle algorithm
c      subroutine rtfourier - write scargle result to FITS file
C      
C ******************************************************************************

      subroutine fperdgrm(infile, outfile, timename, ratename,
     &     errname, intervals, inres, inres_l, rms, inminP, inmaxP,
     &     inmaxP_l, window, chatter, abort)
      
c      Start with initial declarations
      
      integer maxsize, noFreq
      
      character*(*) infile, outfile, intervals
      character*(*) timename, ratename, errname
      double precision inminP, inmaxP, rms
      integer inres, chatter
      logical window, inres_l, inmaxP_l, abort

      character(8) runit, tunit
      character(7) author
      character(80) context
      double precision timedel, tzero, minT, maxT
      double precision mintime(15), maxtime(15), absmin_t, absmax_t
      double precision minP, maxP, recper, recamp, rect0, clevel
      integer deg
      integer j, iunit, ounit, block, nrows, nf, nintervls
      integer ftstatus, flag
      logical inopen, outopen, timecol
      
C      declare the pointers for dynamic memory arrays
        integer time, rate, error, drate, freq, power, faps, pwindow
        
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

c      Initialize variables
      ftstatus = 0
      flag = 0
      iunit = 15
      ounit = 16
      inopen = .false.
      outopen = .false.
      maxsize = 86400
      nofreq = 172800
      author = 'PERDGRM'

      time = 0
      rate = 0
      error = 0
      drate = 0
      freq = 0
      power = 0
      faps = 0
      pwindow = 0
      
C      allocate the dynamic memory 
      call udmget(maxsize,7,time,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for TIME'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,6,rate,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for RATE'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,6,error,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for ERROR'
         call fcerr(context)
         go to 998
      endif
      call udmget(maxsize,7,drate,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for DRATE'
         call fcerr(context)
         go to 998
      endif
      call udmget(nofreq,7,freq,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for FREQ'
         call fcerr(context)
         go to 998
      endif
      call udmget(nofreq,7,power,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for POWER'
         call fcerr(context)
         go to 998
      endif
      call udmget(nofreq,7,faps,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for FAPS'
         call fcerr(context)
         go to 998
      endif
      call udmget(nofreq,7,pwindow,ftstatus)
      if (ftstatus .ne. 0) then
         context = ' Error allocating dynamic memory for PWINDOW'
         call fcerr(context)
         go to 998
      endif

c      Read the input FITS file
      call fxdread(iunit, infile, inopen, block, nrows, absmin_t,
     &     absmax_t, timename, ratename, errname, timecol, MEMD(time),
     &     MEMR(rate), MEMR(error), timedel, runit, tunit, tzero,
     &     maxsize, abort)

c      Parse the interval times into pairs of min & max times
      if (intervals .eq. '-') then
         nintervls = 1
         mintime(1) = absmin_t
         maxtime(1) = absmax_t
      else
         call fcgrgd(intervals, absmin_t, absmax_t, nintervls,
     &        mintime, maxtime)
      endif

c      Open the output file and write the primary header
         call wrtprim(iunit, ounit, inopen, outopen, outfile,
     $     author)
      
c      call the fourier routine for NINTERVLS
      do j = 1,nintervls
         minT = mintime(j) - tzero
         maxT = maxtime(j) - tzero
         if (tunit .eq. 's') then
            mint = 86400. * mint
            maxt = 86400. * maxt
         endif
         
         call dfourier(MEMD(time), MEMR(rate), MEMD(drate), nrows,
     &        mint, maxt, inres, inres_l, inminP, inmaxP, inmaxP_l,
     &        rms, window, nf, MEMD(freq), MEMD(power), MEMD(faps),
     &        MEMD(pwindow), deg, minP, maxP,recper, recamp, rect0,
     &        clevel, chatter, flag)
         if (flag .eq. 0)         
     &    call rtfourier(iunit, ounit, inopen, outopen, infile,
     &        outfile, mint, maxt, nf, MEMD(freq), MEMD(power),
     &        MEMD(faps), window, MEMD(pwindow), tunit, runit,
     &        deg, minP, maxP, recper, recamp, rect0, clevel)
      end do

c      Close the input and output files
      call ftclos(iunit,ftstatus)
      call ftclos(ounit,ftstatus)
      inopen = .false.
      outopen = .false.

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
        call udmfre(rate,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for RATE'
           call fcerr(context)
        endif  
        call udmfre(error,6,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for ERROR'
           call fcerr(context)
        endif  
        call udmfre(drate,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for DRATE'
           call fcerr(context)
        endif  
        call udmfre(freq,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for FREQ'
           call fcerr(context)
        endif  
        call udmfre(power,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for POWER'
           call fcerr(context)
        endif  
        call udmfre(faps,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for FAPS'
           call fcerr(context)
        endif  
        call udmfre(pwindow,7,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'Error freeing dynamic memory for PWINDOW'
           call fcerr(context)
        endif  
    
      return
      end
      
      
C ********************************************************************************
C SUBROUTINE:
C      dfourier
C
C DESCRIPTION:
C      organize the subroutines to carry out the Scagle algorithm
C
C AUTHOR
C      James Lochner  4/29/94
C      
C MODIFICATION HISTORY
C     June 24, 1996 - added chatter parameter
C     
C NOTES:
C     Routines are based on those developed by Koji Mukai and Alan Smale
C      
C USEAGE:
C     call DFOURIER( Xin, Yinreal, Yin, nrows, minT, maxT, inRes, inRes_l,
C          inminP, inmaxP, inmaxP_l, Dummy, window, nf, freq, power,
C          fap, pwindow, deg, minP, maxP, recper0, recamp0,
C          rect00, clevel, chatter, status)
C
C ARGUMENTS:      
C      Xin      - array of input time values
C      Yinreal  - array of input real*4 intensity values
C      Yin      - array of input real*8 intensity values
C      nrows    - total number of data points in the input file
C      minT     - start time of the desired interval to be analyzed
C      maxT     - array of stop times of the desired interval to be analyzed
C      inres    - frequency resolution  (INDEF)
C      inres_l  - logical indicating whether inres is at its INDEF
C      inminP   - input lower period limit
C      inmaxP   - input upper period limit  (INDEF)
C      inmaxP_l - logical indicating whether inmaxP_l is at its INDEF value
C      dummy    -
C      window   - compute window function ?
C      chatter  - how much to tell the user
C      nf       - number of frequencies
C      freq     - array of frequency values
C      power    - array of fourier power values
C      faps     - array of false alarm probability values
C      pwindow  - array of fourier power of the window function
C      deg      - degree of polynomial subtracted from input light curve
C      minP,maxP- actual minimum & maximum periods used in analysis
C      recper0  - period having the maximum fourier power
C      recamp0  - amplitude of period having the maximum fourier power
C      rect00   -
C      clevel   - false alarm probability for period having max fourier power
C      status   - error status
C      
C PRIMARY LOCAL VARIABLES:
C      notime   - maximum number of data points in light curve which can be
c                 handled
c      nofreq   - maximum number of frequency bins which can be handled
c      nodata   - number of data points in input stream
c      X        - array of input time values to be analyzed
c      Y        - array of input intensity values to be analyzed
c      avgy     - mean Y value
c      vary     - variance of the Y values
c      mint,maxt-
c      avnu     - mean frequency bin size
c      sigmay   -
c      maxf,minf- frequency range analyzed
c      minP,maxP- period range analyzed
c      res      - resolution factor used
c      df       -
c      indn     -
c      coeff    -
c      errc     -
c      cnorm    -
c      snorm    -
C      tau      -
c      cCos,cSin - 
c      
C CALLED ROUTINES:
C      subroutine anlzinput - obtain and analyze the input light curve
c      subroutine ktcheb- prewhiten by subtracting polynomia
c      subroutine mknorm- compute normalization for computing faps
c      subroutine slwft - compute the periodogram
c      subroutine scnorm- apply the normalization to the fourier result
c      subroutine convft0-
C      subroutine faps - compute false alarm probabilities
C
C **********************************************************************************
      
      Subroutine DFOURIER( Xin, Yinreal, Yin, nrows, minT, maxT, inRes,
     &     inRes_l, inminP, inmaxP, inmaxP_l, Dummy, window, nf,
     &     freq, power, fap, pwindow, deg, minP, maxP, recper0, recamp0,
     &     rect00, clevel, chatter, status)

      
C
      Integer noTime, noFreq, subNo, addNo, recNo
C
      parameter( notime = 86400, nofreq = 172800 )
      Parameter( subNo = 20, addNo = 5, recNo = 30 )
C
        real*4 Yinreal( noTime )
	Real*8 Xin( noTime ), Yin( noTime )
        Real*8 X( noTime ), Y( notime )
	Real*8 Freq( noFreq ), Power( noFreq ), Pwindow( noFreq )
	Real*8 cCos( noFreq ), cSin( noFreq )
	Real*8 Tau( noFreq ), cNorm( noFreq ), sNorm( noFreq )
	Real*8 TwoPi, fap(noFreq)
	Real*8 recPer( 0: recNo ), recCos( 0: recNo ), recSin( 0: recNo )
	Real*8 recPwr( 0: recNo ), recFrq( 0: recNo ), recT0( 0: recNo )
        real*8 recper0, recamp0, rect00
	Real*8 recAmp( 0: recNo ), recdP( 0: recNo )
	Real*8 avrgY, varY, sigmaY, minT, maxT, minF, maxF, minP, maxP
	Real*8 cLevel
	Real*8 Dummy, maxPwr, dF, avNu
	Real*8 Coeff( 0: 9 ), errC( 0: 9 )
        Real*8 inminP, inmaxP
	Integer I, indN, nF
	Integer maxIf, status, chatter
	Integer noData, Res, Deg, nrows
	Integer datSet, numAdd, numSub, numRec
        Integer inRes
	character(80) context
	Logical Verify, outP, outC, There, subPol, Normd, Anlzd
	Logical Scargl
        Logical inRes_l, inmaxP_l, window
C
	Real*8 SDPOLY
C
	TwoPi = dAtan( 1.0D0 ) * 8.0D0
	status = 0
	datSet = 1
	numAdd = 0
	numSub = 0
	numRec = 0
	Res = 8
	Verify = .True.
	outP = .False.
	outC = .False.
	subPol = .False.
	Normd = .False.
	Scargl = .False.
	Anlzd = .False.

c      Convert the input to real*8
        do i = 1,nrows
           Yin(i) = dble(Yinreal(i))
        end do

C      ANALYZE THE INPUT DATA STREAM
        call ANLZINPUT(Xin, Yin, X, Y, notime, Res, NoData, nrows,
     $       avrgY, varY, minT, maxT, avNu, sigmaY, maxF, minF,
     $       maxP, minP, nf, df, indN, chatter, status)
        if (status .ne. 0) go to 2000
C
C       SET RESOLUTION
C      (Change RES if it has been specified with new value in
C        parameter file.  In such a case, the INDEF logical inres_l is .false.)
        if (.not. inres_l) then
           dF = dF * Res
           Res = inRes
           If( datSet .ne. 0 ) Then
              dF = dF / Res
              nF = ( maxF - minF ) / dF
              indN = Min( nF, noData / 2 )
           End If
           Scargl = .False.
           Anlzd = .False.
        endif
        
C
C       CHANGE LIMIT OF PERIOD TO BE SEARCHED
C      minP and maxP are known from original READ
c      IF (minP, maxP) are not (0,INDEF), then use input values here

        if (.not. inmaxP_l .or. inminP .ne. 0) then
           minP = inminP
           maxP = inmaxP
           If( maxP .eq. 0.0 ) Then
              minF = 0.0
           Else
              minF = 1.0 / maxP
           End If
           If( minP .eq. 0.0 ) Then
              maxF = avNu * 0.5
              minP = 1.0 / maxF
           Else
              maxF = 1.0 / minP
           End If
           nF = ( maxF - minF ) / dF
           Scargl = .False.
           Anlzd = .False.
           numRec = 0
           indN = Min( nF, noData / 2 )
        endif
C       
C       PREWHITEN BY SUBTRACTING A POLYNOMIAL
C            (Need to input RMS error to be achieved by fit into Dummy)
        
	There = .False.
	Call KTCHEB( X, Y, noData, Coeff, errC, Deg, Dummy )
	avrgY = 0.0
	varY = 0.0
        Do I = 1, noData
	  Y( I ) = Y( I ) - SDPOLY( X( I ), Coeff, Deg )
	  avrgY = avrgY + Y( I )
	  varY = varY + Y( I ) * Y( I )
	End DO
	avrgY = avrgY / noData
	varY = varY / noData - avrgY * avrgY
	sigmaY = SQRT( varY )
	subPol = .True.
	Normd = .False.
	Anlzd = .False.
	numRec = 0
C
C       SCARGLE---FIND OUT THE CORRECT NORMALIZATION
C
	If( nF .gt. noFreq ) Then
	   context =  'too many frequencies for this program'
	   call fcerr(context)
	   status = 1
	End If
	if (status .ne. 0) go to 2000
	minF = minF * TwoPi
	dF = TwoPi * dF        
	Call MKNORM( X, noData, cNorm, sNorm, Tau, nF, minF, dF )
	minF = minF / TwoPi
	dF = dF / TwoPi
	Scargl = .True.
C
C       RUN
C
	If( nF .gt. noFreq ) Then
	   context =  'too many frequencies for this program'
	   call fcerr(context)
	   status = 1
       End If
       if (status .ne. 0) go to 2000
	minF = minF * TwoPi
	dF = TwoPi * dF
	Call SLWFT( X, Y, cCos, cSin, noData, nF, minF, dF )
	minF = minF / TwoPi
	dF = dF / TwoPi
	maxPwr = 0.0
	maxIf = 0.0
	If( Scargl ) Then
	  Call SCNORM( cCos, cSin, nF, Freq, minF, dF,
     1                         Tau, cNorm, sNorm, Power, maxPwr, maxIf )
	Else
	  Do I = 1, nF
	    Freq( I ) = minF + dF * ( I - 1 )
	    Power( I ) = cCos( I ) * cCos( I ) + cSin( I ) * cSin( I )
	    If( Power( I ) .gt. maxPwr ) Then
	      maxPwr = Power( I )
	      maxIf = I
	    End If
	  End Do
	End If
	maxPwr = maxPwr * 1.1
	recFrq( 0 ) = TwoPi * Freq( maxIf )
	recCos( 0 ) = cCos( maxIf )
	recSin( 0 ) = cSin( maxIf )
	recPwr( 0 ) = Power( maxIf )
	If( Freq( maxIf ) .eq. 0.0 ) Then
	  recPer( 0 ) = 0.0
	  recdP( 0 ) = 0.0
	  recT0( 0 ) = 0.0
	  recAmp( 0 ) = Sqrt( recCos( 0 ) * recCos( 0 )
     1                                     + recSin( 0 ) * recSin( 0 ) )
       Else
	  recPer( 0 ) = 1.0 / Freq( maxIf )
	  recdP( 0 ) = dF / ( Freq( maxIf ) * Freq( maxIf ) )
	  Call CNVRT0( recFrq( 0 ), recCos( 0 ), recSin( 0 ),
     1                                         recAmp( 0 ), recT0( 0 ) )
       End If
       recper0 = recper(0)
       recamp0 = recamp(0)
       rect00 = rect0(0)

       
       If( Normd ) Then
c           write(6,*) 'here where normd is true'
	  If( Scargl ) Then
	    cLevel = Exp( -recPwr( 0 ) )
	    If( cLevel .ge. 0.001 ) Then
	      cLevel = 1.0 - ( 1.0 - cLevel ) ** indN
	    Else
	      cLevel = cLevel * indN
	    End If
	  End If
	Else
          
c      Compute the false alarm probability for power spectrum
c          normalized via Scargle 
	  If( Scargl ) Then
	    cLevel = Exp( -recPwr( 0 ) / varY )
	    If( cLevel .ge. 0.001 ) Then
	      cLevel = 1.0 - ( 1.0 - cLevel ) ** indN
	    Else
	      cLevel = cLevel * indN
	    End If
            call FAPS(nf, varY, indN, power, fap)
	  End If
	End If
	Anlzd = .True.
C      
C       WINDOW
C
        if (window) then
           Do I = 1, noData
              Y( I ) = 1.0
           End Do
           sigmaY = 0.0
           varY = 0.0
           avrgY = 1.0
           datSet = 3
           numAdd = 0
           subPol = .False.
           numSub = 0
           Normd = .False.
           Anlzd = .False.
           Scargl = .False.
           numRec = 0
c      Run SCARGLE on this new data set
           minF = minF * TwoPi
           df = df * TwoPi
           call MKNORM( X, noData, cNorm, sNorm, Tau, nF, minF, dF)
c      Run SLWFT on the new data
           call SLWFT( X, Y, cCos, CSin, noData, nF, minF, dF)
           minF = minF / TwoPi
           df = df / TwoPi
c      Compute the power in the window transform
           call SCNORM( cCos, CSin, nf, Freq, minF, dF, Tau,
     &          cNorm, sNorm, Pwindow, maxPwr, MaxIf)
           
        endif

C      
5000    Format( A60 )
5010    Format( A1 )

2000    continue
        return
	End
C ********************************************************************************
C SUBROUTINE:
C      anlzinput
C
C DESCRIPTION:
C      analyze input time series of mean, variance, average frequency step
C
C AUTHOR
C      James Lochner  4/29/94
C      
C MODIFICATION HISTORY
C     7/19/95 - changed name to anlzinput  (Brian Elza had changed it
C                already for the Jan 95 FTOOLS delivery)
C     6/24/96 - recoded output msg for duplicat times
C                implemented chatter parameter 
C NOTES:
C     Routine is based on that developed by Koji Mukai and Alan Smale
C      
C USEAGE:
C      call anlzInput( X, Y, Xout, Yout, noTime, Res, noData,
C                 nrows, avrgY, varY, minT, maxT, avNu, sigmaY, maxF,
C                 minF, maxP, minP, nf, df, indN, chatter, Flag )
C
C ARGUMENTS:      
c      X        - array of input time values to be analyzed
c      Y        - array of input intensity values to be analyzed
c      Xout     - array of times within mint & maxt
c      Yout     - array of intensities at times within mint & maxt
C      notime   - maximum number of data points in light curve which can be
C                 handled
C      res      - resolution factor to be used
C      nodata   - number of accepted data points meeting mint & maxt
C      nrows    - number of data points in input X array
C      avgy     - mean Y value
C      vary     - variance of the Y values
C      mint,maxt- min & max times within which to accept data
C      avnu     - mean frequency bin size
C      sigmay   - sqrt(vary)
C      maxf,minf- frequency range inherent in accepted data
C      minP,maxP- period range inherent in accepted data
C      nf       - number of frequency steps inherent in the accepted data
C      df       - frequency bin size inherent in the accepted data
C      indn     -
C      chatter  - how much to tell user
C     
C PRIMARY LOCAL VARIABLES:
C      none
c      
C CALLED ROUTINES:
C      none
C
C **********************************************************************************

      Subroutine anlzInput( X, Y, Xout, Yout, noTime, Res, noData,
     &     nrows, avrgY, varY, minT, maxT, avNu, sigmaY, maxF,
     &     minF, maxP, minP, nf, df, indN, chatter, Flag )
      
c      Subroutine to analyze input time series for FSCARGLE
      
C
      Integer noTime, noData, Res, nrows
      Real*8 X( noTime ), Y( noTime ), Xout(noTime), Yout(noTime)
      Real*8 avrgY, varY, minT, maxT, avNu
      Real*8 sigmaY, maxF, minF, maxP, minP,df
      Integer indN, nF, chatter, flag

      Integer I, J
	character(80) context
C       
      Flag = 0
      avNu = 0.0
      avrgY = 0.0
      varY = 0.0
      
c      Find the 1st Point in requested time range
      I = 1
      do while (Flag .eq. 0 .and. I .le. nrows)
         if ( X(I) .ge. minT) then
            Xout(1) = X(I)
            Yout(1) = Y(I)
            avrgY = Y(I)
            varY = Y(I) * Y(I)
            Flag = 1
         endif
         I = I + 1
      end do
      Flag = 0
      
c      Now find rest of points in requested time range
      J = 2
      ndup = 0
      do while ( I .le. nrows)
         if ( X( I ) .le. maxT) then
            Xout(J) = X(I)
            Yout(J) = Y(I)
            avrgY = avrgY + Y( I )
            varY = varY + Y( I ) * Y( I )
            If (J .gt. 1) then               
               If( X( I ) .ne. X( I - 1 ) ) Then
                  avNu = avNu + 1.0 / ( X( I ) - X( I - 1 ) )
               Else
                  ndup = ndup + 1
                  if (chatter .ge. 20) then
                     write(context,'(a,i7,a)') 'Line ',i,
     $                    ' has same time as previous line'
                     call fcerr(context)
                  endif                  
               End If
            End If
            J = J + 1
         endif
         I = I + 1
      End Do
      noData = J - 1
      if (ndup .gt. 0) then
         write(context,'(i7,a)') ndup,' Instances of duplicate times'
         call fcerr(context)
      endif
      
C
C      Compute statistics and frequency properties
C      
      avrgy = avrgy / noData
      vary = vary / nodata - avrgy * avrgy
      avNu = avNu / (noData - 1)
      sigmaY = sqrt(varY)
      maxF = avNu * 0.5
      minF = 0.0
      maxP = 0.0
      minP = 1. / maxF
      nF = (maxT - minT) * avNu * Res
      dF = maxF / nF
      indN = min(noData/2,nF)
c
      if (nodata .gt. notime) then
         context= 'too many data points for this program'
	 call fcerr(context)
         flag = 1
      endif
c      

      return
      End





C ************************************************************************
	Subroutine KTCHEB( X, Y, Pix, Coef, errC, Order, RMS )
C
	Integer maxM
	Parameter( maxM = 9 )
C
	Integer Pix
	Real*8 X( Pix ), Y( Pix ), RMS
	Double Precision Coef( 0: maxM ), ErrC( 0: maxM )
C
	Real*8 minX, maxX, sRMS
	Double Precision Add, Div, Mul
	Double Precision C( 0: maxM, -1: maxM )
c       ...C( I, J ) is the factor of X**I in Jth Tchebyceff polynomial
	Double Precision Gamma( 0: maxM ), Bj( 0: maxM ), sBj( 0: maxM )
	Double Precision Ssq1( 0: maxM ), E( 0: 1 )
	Double Precision Xi, Sx, Pw, BetaJ, Sum, Diff
	Double Precision SigM
	Double Precision PPOLY
	Double Precision Alpha( 0: maxM, 0: maxM ), Temp, Work
	Real*8 Ftest( 9 ), Test, ftRat( maxM )
	Integer Limit, Order, I, J, J1, J2, K
	character(80) context, comnum
	character(33) comment
	Common / cheb / C
	Data Ftest / 39.9, 8.53, 5.54, 4.54, 4.06,
     1                                          3.78, 3.59, 3.46, 3.36 /
C
	minX = X( 1 )
	maxX = X( 1 )
	Do I = 2, Pix
	  minX = Min( minX, X( I ) )
	  maxX = Max( maxX, X( I ) )
	End Do
	Mul = maxX - minX
	If( Mul .eq. 0.0 ) Mul = 1.0
	Div = 1.0 / Mul
	Add = Mul - minX
	Limit = Min( maxM, Pix - 2 )
	If( Limit .le. 0 ) Then
	   comment = ' Number of pixels is too small: '
	   context = comnum(comment,dble(pix))
	   call fcerr(context)
	   Return
	End If
	sRMS = RMS
	If( RMS .lt. 0 ) Then
	  Limit = Min( Nint( -RMS ), Limit + 1 )
	  RMS = 0.0
	End If
	E( 1 ) = 1.0
	C( 0, -1 ) = 0.0
	C( 0, 0 ) = 1.0
C
C     Main
C
c       Section 1.  Calculate the polynomials for this data
c
	Do J = 0, Limit
	  Gamma( J ) = 0.0
	  Sx = 0.0
	  Do I = 1, Pix
	    Xi = ( X( I ) + Add ) * Div
	    Pw = PPOLY( J, Xi ) ** 2
	    Gamma( J ) = Gamma( J ) + Pw
	    Sx = Sx + Pw * Xi
	  End Do
	  If( J .le. 0 ) Then
	    BetaJ = 0.0
	  Else
	    BetaJ = Gamma( J ) / Gamma( J - 1 )
	  End If
	  E( 0 ) = -Sx / Gamma( J )
	  J1 = J - 1
	  J2 = J + 1
	  If( J2 .le. maxM ) Then
	    Call PRODCT( C( 0, J ), J, E, 1, C( 0, J2 ) )
	    Do K = 0, J1
	      C( K, J2 ) = C( K, J2 ) - C( K, J1 ) * BetaJ
	    End Do
	  End If
	End Do
c
c       Section 2.  Calculate the coefficients Bj
c
	Do J = 0, Limit
	  Sum = 0.0
	  Do I = 1, Pix
	    Xi = ( X( I ) + Add ) * Div
	    Sum = Sum + Y( I ) * PPOLY( J, Xi )
	  End Do
	  Bj( J ) = Sum / Gamma( J )
	End Do
c
c       Section 3.  Calculate residuals and test for best order fit
c
	Do J = 0, Limit
	  Ssq1( J ) = 0.0
	End Do
	Do I = 1, Pix
	  Xi = ( X( I ) + Add ) * Div
	  Sum = 0.0
	  Do J = 0, Limit
	    Sum = Sum + Bj( J ) * PPOLY( J, Xi )
	    Diff = Y( I ) - Sum
	    Ssq1( J ) = Ssq1( J ) + Diff * Diff
	  End Do
	End Do
	RMS = RMS * RMS
	Do J = 0, Limit
	  K = Pix - J - 1
	  If( K .eq. 0 ) Then
c	    Write( *, 331 )
c331         Format( ' @ The EXACT solution is calculated !!!' )
	    Order = J
	    Goto 350
	  Else
	    Ssq1( J ) = Ssq1( J ) / K
	    If( J .ge. 1 ) Then
	      Test = ( Ssq1( J - 1 ) - Ssq1( J ) ) / Ssq1( J ) * K
	      If( K .ge. 10 ) Then
		ftRat( J ) = Test / 2.71
	      Else
		ftRat( J ) = Test / Ftest( K )
	      End If
	    End If
	    If( Ssq1( J ) .le. RMS ) Then
	      Order = J
	      Goto 350
	    End If
	  End If
	End Do
	Do J = 1, Limit
	  If( ftRat( J ) .ge. 1.0 ) Order = J
	End Do
c	If( Order .ne. Limit ) Then
c	  comment = 'The fit does not improve with orders higher than '
c	  context = comnum(comment,dble(order))
c	  call fcerr(context)
c	Else
c	   context = 'Couldn''t find a good fit !!!'
c	   call fcerr(context)
c	End If
350     Continue
	RMS = sRMS
c
c       Section 4.  Calculate errors on polynomial coefficients
c
	SigM = Sqrt( Ssq1( Order ) )
	Do J = 0, Order
	  sBj( J ) = SigM / Sqrt( Gamma( J ) )
	End Do
c
c       Section 5. Calculate coefficients Coeff and associated errors
c               ErrC of equivalent straight polynomial fit
c
	Do J = 0, Order
	  Coef( J ) = 0.0
	  ErrC( J ) = 0.0
	  Do K = J, Order
	    Coef( J ) = Coef( J ) + Bj( K ) * C( J, K )
	    ErrC( J ) = ErrC( J ) + sBj( K ) * C( J, K )
	  End Do
	End Do
	Do J = Order + 1, Limit
	  Coef( J ) = 0.0
	  errC( J ) = 0.0
	End Do
C
C       convert back to the real data set
C
	Call CONVRT( Alpha, maxM, -Add, Mul )
	Do J = 0, Order
	  Temp = 0.0
	  Work = 0.0
	  Do K = J, Order
	    Temp = Temp + Alpha( K, J ) * Coef( K )
	    Work = Work + Alpha( K, J ) * errC( K )
	  End Do
	  Coef( J ) = Temp
	  errC( J ) = Work
	End Do
C
C     END
C
	return
	End

C *************************************************************************
	Subroutine PRODCT( A, M, B, N, P )
c
c       Makes the product of two polynomials
c       A( J ) * X ** J ( J = 0, M ) and B( K ) * X ** K ( K = 0, N )
c       and returns the result P( L ) * X ** L ( L = 0, M + N )
c                                       Koji, 10.1.84
	Integer J, K, L, M, N
	Integer MinA, MaxA
	Double Precision A( 0:M ), B( 0:N ), P( 0:M+N )
C
	Do L = 0, M + N
	  P( L ) = 0.0
	  MinA = Max( 0, L - N )
	  MaxA = Min( L, M )
	  Do J = MinA, MaxA
	    K = L - J
	    P( L ) = P( L ) + A( J ) * B( K )
	  End Do
	End Do
C
	End
C *******************************************************************
	Subroutine CONVRT( Alpha, Order, A0, A1 )
C
C       This subroutine constructs conversion matrix for polynomial
C       coefficients when the argument is linearly transformed.
C               Poly = Sum(0toOrder){C(i)*X**i}
C                       Y=A0+A1X
C               Poly = Sum(0toOrder){D(j)*Y**j}
C               where D(j) = Sum{Alpha(i,j)*C(i)}
C
C                               created by Koji Mukai 24/9/1984
C
	Integer Order
	Double Precision Alpha( 0:Order, 0:Order )
	Double Precision A0, A1
c
	Double Precision B0, B1
	Integer I, J
C
	Do I = 0, Order
	  Do J = 0, Order
	    Alpha( I, J ) = 0.0
	  End Do
	End Do
	B1 = 1.0 / A1
	B0 = -A0 * B1
	Alpha( 0, 0 ) = 1.0
	Do I = 1, Order
	  Do J = 0, I - 1
	    Alpha( I, J ) = Alpha( I, J ) + B0 * Alpha( I - 1, J )
	    Alpha( I, J + 1 ) = Alpha( I, J + 1 )
     1                                          + B1 * Alpha( I - 1, J )
	  End Do
	End Do
C
	End

C ***********************************************************************
	Double Precision Function PPOLY( J, X )
c
c       Tchebycheff polynomials with coefficient C
c
	Integer maxM
	Parameter( MaxM = 9 )
C
	Integer J, K
	Double Precision X, Dummy
	Double Precision C( 0: maxM, -1: maxM )
	Common / cheb / C
C
	If( J .le. -1 ) Then
	  PPOLY = 0.0
	Else If( J .eq. 0 ) Then
	  PPOLY = 1.0
	Else
	  If( X .eq. 0.0 ) Then
	    PPOLY = C( 0, J )
	  Else
	    Dummy = 0.0
	    Do K = J, 0, -1
	      Dummy = Dummy * X + C( K, J )
	    End Do
	    PPOLY = Dummy
	  End If
	End If
C
	Return
	End
	Double Precision Function SDPOLY( Argmnt, Coeff, Order )
C
	Integer Order
	Real*8 Argmnt
	Double Precision Coeff( 0: Order )
	Double Precision Dummy, Arg
	Integer I
C
	Arg = Argmnt
	Dummy = 0.0D0
	Do I = Order, 0, -1
	  Dummy = Dummy * Arg +  Coeff( I )
	End Do
C
	SDPOLY = Dummy
C
	End
	Subroutine MKNORM( X, nData, cNorm, sNorm, Tau, nF, Fmin, dF )
C
C       By Tau, I really mean Tau x Omega in Scargle's notation
C
c	Implicit Undefined(a-z)
C
	Integer nData, nF
	Real*8 X( nData ), cNorm( nF ), sNorm( nF ), Tau( nF ), Fmin, dF
C
	Real*8 Z0, dZ, cosK, sinK, adcos, adsin, Phase, Pi, Work, N2
	Integer J, K, K0, Interval
C
	If( nF .le. 100 ) Then
	  Interval = 500	
	Else If( nF .le. 200 ) Then
	  Interval = 500 ! 200	
	Else If( nF .le. 500 ) Then
	  Interval = 500 ! 100	
	Else
	  Interval = 500 ! 100
	End If
	Print *
	Pi = dAtan( 1.0D0 ) * 4.0D0
	Do K = 1, nF
	  cNorm( K ) = 0.0D0
	  sNorm( K ) = 0.0D0
	  Tau( K ) = 0.0D0
	End Do
	Fmin = Fmin * 2.0D0
	dF = dF * 2.0D0
	Do J = 1, nData
	  Z0 = Fmin * X( J )
	  cosK = dCOS( Z0 )
	  sinK = dSIN( Z0 )
	  dZ = dF * X( J )
	  adcos = dCOS( dZ )
	  adsin = dSIN( dZ )
	  cNorm( 1 ) = cNorm( 1 ) + cosK
	  sNorm( 1 ) = sNorm( 1 ) + sinK
	  Do K = 2, nF
	    Call SWING( cosK, sinK, adcos, adsin )
	    cNorm( K ) = cNorm( K ) + cosK
	    sNorm( K ) = sNorm( K ) + sinK
	  End Do
c	  If ( MOD( J, Interval ) .eq. 0 ) Then
c	    Write( *, 110 ) J, nData
c110	    Format( '+Processed ', I5, ' out of ', I6, ' records' )
c	  End If
	End Do
c	Write( *, 120 ) nData
c120	Format( '+* Now finished processing', I7, ' records.     ' )
	Fmin = Fmin * 0.5D0
	dF = dF * 0.5D0
	If( Fmin .eq. 0.0D0 ) Then
	  Tau( 1 ) = cNorm( 1 ) / nData
	  cNorm( 1 ) = 1.0D0 + Tau( 1 )
	  sNorm( 1 ) = 1.0D0 - Tau( 1 )
	  Phase = Tau( 1 ) * 2.0D0
	  K0 = 2
	Else
	  Phase = 0.0
	  K0 = 1
	End If
	N2 = Real( nData ) * 0.25D0
	Z0 = 1.0D0 / Real( nData )
	Do K = K0, nF
	  Tau( K ) = dAtan( sNorm( K ) / cNorm( K ) )
	  J = Nint( ( Tau( K ) - Phase ) / Pi )
	  Tau( K ) = Tau( K ) + Pi * J
	  Phase = Tau( K )
	  Work = ( cNorm( K ) * dCos( Phase )
     1            + sNorm( K ) * dSin( Phase ) ) * Z0
	  cNorm( K ) = N2 / ( 1.0D0 + Work )
	  sNorm( K ) = N2 / ( 1.0D0 - Work )
	  Tau( K ) = Tau( K ) * 0.5D0
	End Do
C
	End
	Subroutine SLWFT( X, Y, C, S, nData, nF, Fmin, dF )
C
C     Chebyshev recursion formulae used in Paul Murdin's version
C       accumulate numerical errors, and therefore now replaced
C       with a better routine taken from the Monro book.
C
C                                                       KM 11/7/85
C
c	Implicit Undefined(a-z)
C
	Integer nData, nF
	Real*8 X( nData ), Y( nData ), C( nF ), S( nF ), Fmin, dF
C
	Real*8 Z0, dZ, cosK, sinK, adcos, adsin
	Integer J, K, Interval
C
	Do K = 1, nF
	  S( K ) = 0.0
	  C( K ) = 0.0
	End Do
c	Print *
	If( nF .le. 100 ) Then
	  Interval = 500
	Else If( nF .le. 200 ) Then
	  Interval = 500 ! 200
	Else If( nF .le. 500 ) Then
	  Interval = 500 ! 100
	Else
	  Interval = 500 ! 200
c were 1000, 500, 200 and 500
	End If
	Do J = 1, nData
	  Z0 = Fmin * X( J )
	  cosK = dCOS( Z0 )
	  sinK = dSIN( Z0 )
	  dZ = dF * X( J )
	  adcos = dCOS( dZ )
	  adsin = dSIN( dZ )
	  C( 1 ) = C( 1 ) + cosK * Y( J )
	  S( 1 ) = S( 1 ) + sinK * Y( J )
	  Do K = 2, nF
	    Call SWING( cosK, sinK, adcos, adsin )
	    C( K ) = C( K ) + cosK * Y( J )
	    S( K ) = S( K ) + sinK * Y( J )
	  End Do
c	  If ( MOD( J, Interval ) .eq. 0 ) Then
c	    Write( *, 110 ) J, nData
c110	    Format( '+ Processed ', I5, ' out of ', I6, ' records' )
c	  End If
	End Do
c	Write( *, 120 ) nData
c120	Format( '+ *Now finished processing', I7, ' records.' )
	Z0 = 2.0 / REAL( nData )
	Do K = 1, nF
	  C( K ) = C( K ) * Z0
	  S( K ) = S( K ) * Z0
	End Do
C
	End
C
C
C
	Subroutine SWING( cosA, sinA, cosB, sinB )
C
	
C
	Real*8 half, one, one5
	Parameter( half = 0.5D0, one = 1.0D0, one5 = 1.5D0 )
C
	Real*8 cosA, sinA, cosB, sinB
C
C       cosA (out) = cos(A+B), sinA (out) = sin(A+B)
C
	Real*8 D, Z1, Z2, T
C
	D = cosB - one
	Z1 = cosA + D * cosA - sinA * sinB
	Z2 = sinA + D * sinA + cosA * sinB
	T = one5 - half * ( Z1 * Z1 + Z2 * Z2 )
	cosA = T * Z1
	sinA = T * Z2
C
	End
	Subroutine SCNORM( cCos, cSin, nF, Freq, minF, dF,
     1                         Tau, cNorm, sNorm, Power, maxPwr, maxIf )
C
C	Implicit Undefined(a-z)
C
	Integer nF, maxIf
	Real*8 cCos( nF ), cSin( nF ), Freq( nF )
	Real*8 Tau( nF ), cNorm( nF ), sNorm( nF ), Power( nF )
	Real*8 minF, dF, maxPwr
C
	Integer I
	Real*8 cosTau, sinTau, cTerm, sTerm
C
	maxPwr = 0.0
	Do I = 1, nF
	  Freq( I ) = minF + dF * ( I - 1 )
	  cosTau = Cos( Tau( I ) )
	  sinTau = Sin( Tau( I ) )
	  cTerm = cCos( I ) * cCos( I ) * cosTau * cosTau
     1            + cSin( I ) * cSin( I ) * sinTau * sinTau
     2                   + 2.0 * cCos( I ) * cSin( I ) * cosTau * sinTau
	  sTerm = cCos( I ) * cCos( I ) * sinTau * sinTau
     1            + cSin( I ) * cSin( I ) * cosTau * cosTau
     2                   - 2.0 * cCos( I ) * cSin( I ) * cosTau * sinTau
	  Power( I ) = cTerm * cNorm( I ) + sTerm * sNorm( I )
	  If( Power( I ) .gt. maxPwr ) Then
	    maxPwr = Power( I )
	    maxIf = I
	  End If
	End Do
C
	End
	Subroutine CNVRT0( Freq, cCos, cSin, Amp, T0 )
C
C	Implicit Undefined(a-z)
C
	Real*8 Freq, cCos, cSin, Amp, T0
C
	Real Angle
C
	Amp = Sqrt( cCos * cCos + cSin * cSin )
	If( cSin .eq. 0.0 ) Then
	  If( cCos .lt. 0.0 ) Then
	    Angle = dAtan( 1.0D0 ) * 2.0
	  Else
	    Angle = - dAtan( 1.0D0 ) * 2.0
	  End If
	Else
	  If( cSin .gt. 0.0 ) Then
	    Angle = dAtan( - cCos / cSin )
	  Else
	    Angle = dAtan( - cCos / cSin ) + dAtan( 1.0D0 ) * 4.0
	  End If
	End If
	T0 = Angle / Freq
C
	End

C **********************************************************************
C      SUBROUTINE:  FAPS.F
C      Purpose   :  to compute false alarm probabilities for power
C                    spectra having Scargle's normalization
C                     (Based on DFOURIER routine)

      subroutine faps(nf, varY, indN, power, fap)

      
      integer nf, indN
      real*8 varY
      real*8 power(nf), fap(nf)

      integer j

      do j =  1, nf
         fap(j) = exp( -power(j) / varY)
         if (fap(j) .ge. 0.001) then
            fap(j) = 1.0 - (1.0 - fap(j)) ** indN
         else
            fap(j) = fap(j) * indN
         endif
      end do

      return
      end
C ********************************************************************************
C SUBROUTINE:
C      rtfourier
C
C DESCRIPTION:
C      write the result of SCARGLE to a FITS binary extension
C
C AUTHOR
C      James Lochner  4/29/94
C      
C MODIFICATION HISTORY
C      10/14/95 - modify the TSTART and TSTOP keywords in extension header
C                  to be values for the analyzed data segment contained in
C                  the extension      
C      7/19/95 - cleaned up error checking for ONTIME & TSTARTI
C
C NOTES:
C      
C USEAGE:
C     call rtfourier(iunit, ounit, inopen, outopen, infile,
C          outfile, tstart, tstop, nf, freq, power, fap, window,
C          pwindow, tcolunit, runit, deg, minP, maxP, recper, recamp,
C          rect0, clevel)
C
C ARGUMENTS:      
C      iunit    - logical unit for input file
C      ounit    - logical unit for output file
C      inopen   - logical indicating open status of input file
C      outopen  - logical indicating open status of output file
C      infile   - name of input file
c      outfile  - name of output file
C      tstart   - start time for analyzed data segment
c      tstop    - stop time for analyzed data segment
C      nf       - number of frequencies
C      freq     - array of frequency values
C      power    - array of fourier power values
C      faps     - array of false alarm probability values
C      pwindow  - array of fourier power of the window function
C      tcolunit - units of the analyzed time array
C      runit    - units of the analyzed intensity array      
C      deg      - degree of polynomial subtracted from input light curve
C      minP,maxP- actual minimum & maximum periods used in analysis
C      recper   - period having the maximum fourier power
C      recamp   - amplitude of period having the maximum fourier power
C      rect0    -
C      clevel   - false alarm probability for period having max fourier power
C      
C PRIMARY LOCAL VARIABLES:
C      tform    - array of data types for output
c      ttype    - array of output column names
c      tunit    - array of output column units
c      tfields  - number of output columns
c      comment  - character comment statement for header
c      history  - character history comment statement for header
C      
C CALLED ROUTINES:
C      subroutine ftmahd - absolute move to extension
C      subroutine ftcrhd - create a new extension at end of file
C      subroutine ftphbn - put the required keywords into a binary extension
C      subroutine xcopynoscale - copy non-required keywords
C      subroutine ftpkys - put a string keyword into a header
c      subroutine ftpdey - delete an existing header keyword
C      subroutine ftphis - put a HISTORY keyword into a header
C      subroutine ftpdat - put the DATE keyword into a header
C      subroutine ftbdef - define the structure of a binary table extension
C      subroutine ftpcld - put real*8 values into a table column
C      subroutine ftclos - close a FITS file opened with ftinit or ftopen
C      subroutine fcerr  - prints message to stderror device
C      subroutine fcerrm - prints fitsio error number and message
C      function comnum   - append a numerial value to a character string
C
C **************************************************************************      
      
      subroutine rtfourier(iunit, ounit, inopen, outopen, infile,
     &     outfile, tstart, tstop, nf, freq, power, fap, window,
     &     pwindow, tcolunit, runit, deg, minP, maxP, recper, recamp,
     &     rect0, clevel)

      
      character*(*) outfile, infile, tcolunit, runit
      integer iunit, ounit, nf, deg
      real*8 tstart, tstop
      real*8 freq(nf), power(nf), fap(nf), pwindow(nf)
      real*8 minP, maxP, recper, recamp, rect0, clevel
      logical inopen, outopen, window
      
      character(80) context, history, comnum
      character(25) comment
      character(34) comment2
      character(48) comment3
      character(10) tform(10), ttype(10), tunit(10), extname
      character(10) timeunit
      integer starti, stopi
      integer htype, tfields, ftstatus, pc
      integer j, frow, felem, extnum
      real*8  startf, stopf
      extnum = 1
      ftstatus = 0
      pc=0

c      Move to the data extension in the input file
      call ftmahd(iunit,2,htype,ftstatus)

c      get the TIMEUNIT keyword (unit for header keywords)
      call ftgkys(iunit,'TIMEUNIT',timeunit,comment,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get timeunit value' 
         call fcerr(context)
         goto 999
      endif 

c      Initialize the new extension in the output file
      call ftcrhd(ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to initialize new extension'
         call fcerr(context)
         go to 999
      endif

c      Set the column names, types and units
      ttype(1) = 'FREQUENCY'
      tform(1) = 'D'
      tunit(1) = 'Hz'
      ttype(2) = 'POWER'
      tform(2) = 'D'
      tunit(2) = ' '
      ttype(3) = 'FAP'
      tform(3) = 'D'
      tunit(3) = ' '

      tfields = 3
      extname = 'PWR SPEC'

c      Check for Window funtion
      if (window) then
         tfields = tfields + 1
         ttype(4) = 'WINDOW'
         tform(4) = 'D'
         tunit(4) = ' '
      end if
      
c      Write required header keywords
      call ftphbn(ounit,nf,tfields,ttype,tform,tunit,extname,
     &     pc,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write required extension keywords'
         call fcerr(context)
         go to 999
      endif

c      Write the creation date of this file
      call ftpdat(ounit,ftstatus)

c      Write additional keywords
      call ftpkys(ounit,'CREATOR','fscargle','Name of FTOOL that'//
     &    ' created this file',ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to put additional keywords'
         call fcerr(context)
         goto 999
      endif

      call ftpkys(ounit,'HDUCLASS','OGIP',' ',ftstatus)
      call ftpkys(ounit,'HDUCLAS1','POWER SPECTRUM',' ',ftstatus)
      call ftpkys(ounit,'HDUCLAS2','NET',' ',ftstatus)
      call ftpkys(ounit,'HDUCLAS3','RESULTS',' ',ftstatus)
      call ftpkys(ounit,'CONTENT','SCARGLE OUTPUT',' ',ftstatus)
      call ftpkys(ounit,'ORIGIN','XTE/OGIP/GSFC',' ',ftstatus)
      
c      Copy the rest of the non-required keywords from the input
c      extension to the output extension
      call xcopynoscale(iunit,ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'call to xcopynoscale unsuccessful'
         call fcerr(context)
         goto 999
      endif

C     delete the ONTIME keyword if it exists
      call ftdkey(ounit,'ONTIME',ftstatus)
      if (ftstatus .ne. 0 ) then
         if (ftstatus .ne. 202) then 
            context = 'unable to delete ONTIME keyword'
            call fcerr(context)
            go to 999
         endif         
      endif
      ftstatus = 0

c      Modify the TSTART and TSTOP times, if necessary
c      RE TIMEUNIT is from TIMEUNIT keyword

      if (tcolunit .ne. timeunit) then
         if (tcolunit .eq. 's') then
            tstart = tstart / 86400.
            tstop = tstop / 86400.
c            ontime = ontime / 86400.
         endif
         if (tcolunit .eq. 'd') then
            tstart = 86400. * tstart
            tstop = 86400. * tstop
c            ontime = 86400. * ontime
         endif
      endif
      starti = int(tstart)
      startf = tstart - starti
      stopi = int(tstop)
      stopf = tstop - stopi
      call ftmkyj(ounit,'TSTARTI',starti,'&',ftstatus)
      if (ftstatus .eq. 202) then
         ftstatus = 0
         call ftmkyd(ounit,'TSTART',tstart,13,'&',ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to modify TSTART value'
            call fcerr(context)
            go to 999
         endif
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
         ftstatus = 0
         call ftmkyd(ounit,'TSTOP',tstop,13,'&',ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to modify TSTOP value'
            call fcerr(context)
            go to 999
         endif
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

c      Now delete the history keywords
      ftstatus = 0
      do while (ftstatus .eq. 0)
         call ftdkey(ounit,'HISTORY',ftstatus)
         if (ftstatus .ne. 0 .and. ftstatus .ne. 202) then
            context = 'unable to delete HISTORY keyword'
            call fcerr(context)
            goto 999
         endif
      end do
      ftstatus = 0

c      Insert new HISTORY keywords
      history = 'Input File: '//infile
      call ftphis(ounit,history,ftstatus)

      history = comnum('Degree of polynomial subtracted: ',dble(deg))
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write HISTORY keyword'
         call fcerr(context)
         goto 999
      endif

      if (maxp .eq. 0) maxp = 1. /  freq(2)
      comment2 = comnum('Period Range searched: ',minp)
      comment3 = comnum(comment2//' - ',maxp)
c      history = comnum(comment2//' - ',maxp)
      history = comment3 // ' ' // tcolunit
      call ftphis(ounit,history,ftstatus)
      
      comment = comnum('Best Period = ',recper)
      history = comment // ' '// tcolunit
      call ftphis(ounit,history,ftstatus)

      comment = comnum(' Amplitude = ',recamp)
      history = comment // ' ' // runit 
      call ftphis(ounit,history,ftstatus)

      history = comnum(' Time of Zero Phase = ',rect0)
      call ftphis(ounit,history,ftstatus)

      history = comnum(' False Alarm Prob. = ',clevel)
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write HISTORY keyword'
         call fcerr(context)
         goto 999
      endif
      
c      Define the output extension Data structure
      call ftbdef(ounit,tfields,tform,pc,nf,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to define data structure for extension'
         call fcerr(context)
         goto 999
      endif

c      If TIME unit is days, then convert the frequency from
c      cycles/day into cycles/s
      if (tcolunit .eq. 'd') then
         do j = 1,nf
            freq(j) = freq(j) / 86400.
         end do
      end if
      
c      Write the data
      frow = 1
      felem = 1
      call ftpcld(ounit,1,frow,felem,nf,freq,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write freq array'
         call fcerr(context)
         go to 999
      endif
      call ftpcld(ounit,2,frow,felem,nf,power,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write power array'
         call fcerr(context)
         go to 999
      endif
      call ftpcld(ounit,3,frow,felem,nf,fap,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write fap array'
         call fcerr(context)
         go to 999
      endif
      if (window) then
         call ftpcld(ounit,4,frow,felem,nf,pwindow,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write fap array'
            call fcerr(context)
            go to 999
         endif
      endif
      
      
c      Error handling
999   continue
      if (ftstatus .ne. 0) then         
         call fcerrm(ftstatus)
         ftstatus = 0
         if (inopen) call ftclos(iunit,ftstatus)
         if (outopen) call ftclos(ounit,ftstatus)
      endif
      
      return
      end





C **********************************************************************
C SUBROUTINE:
C      fxdread
C
C DESCRIPTION:      
C      Read a  Standard OGIP Timing FITS files.
C
C AUTHOR:
C      James Lochner 4/29/94
C      
C MODIFICATION HISTORY:
C      10/3/94 - allow user to specify names of time, rate and error
C                 columns.  Defaults remain TIME, RATE, ERROR
C      10/7/94 - implemented abort option when RATE (and ERROR) columns 
C                 not found
C      7/19/95 - cleaned up error checking for TIMEZERO, etc.  Also allowed
C                 for MJDREFI/F.
C      8/07/96 - If TIMEZERO not found, assume it's 0.0
C      4/18/96 - Search for MJDREF via xftgkyd - M. Tripicco
C      6/21/96 - Cleanup TIMEZERO bug, utlize xftgky for TIMEZERO
C	
C NOTES:
C
C USAGE:
C      subroutine fxdread(iunit, infile, inopen, block, nrows,
C                  mintime, maxtime, timename, ratename, errname, 
C                  timecol, time, rate, error, timedel, runit, 
C                  timeunit, tzero, maxsize)
C
C ARGUMENTS:
C      iunit    - logical unit for input file
C      infile   - name of input file
C      inopen   - logical indicating open status of input file
C      block    -
C      nrows    - number of rows in input file (= number of data points)
C      mintime  - absolute minimum time in the data (in same units as TIMEZERO)
C      maxtime  - absolute maximum time in the data (in same units as TIMEZERO)
C      timename - alternate name for time column in infile
C      ratename - alternate name for intensity column in infile
C      errname  - alternate name for error column in infile
C      timecol  - flag indicating presence of a time column in input file
C      time     - array of input time values (in units of timeunit)
C      rate     - array of input rate (intensity) values
C      error    - array of input error values
C      timedel  - bin size of the data (on output, in units of timeunit)
C      runit    - units associated with the RATE column
C      timeunit - units of the time array
C      tzero    - MJDREF + TIMEZERO value
C      maxsize  - maximum size for output arrays
C      abort    - status flag for non-fitsio errors
C      
C PRIMARY LOCAL VARIABLES:
C      ttype    - array of column names
C      tunit    - array of column units
C      spanunit - value of the TIMEUNIT keyword - input units for timedel
C                  and timezero
C      timecol  - logical indicating presence of time column
C      ratecol  - logical indicating presence of rate column
C      errcol   - logical indicating presence of error column
C      timepos  - column number of the time column
C      ratepos  - column number of the rate column
C      errpos   - column number of the error column
C      ftstatus - fitsio status flag
C      context  - error messages generated by this subroutine
C
C CALLED ROUTINES:
C      subroutine fcpars - parse the input file name and extension number
C      subroutine ftopen - opens the input FITS file
C      subroutine ftmrhd - relative move to FITS extension
C      subroutine ftgkns -
C      subroutine ftgkyd - get real*8 valued keyword
C      subroutine ftgkyj - getinteger valued keyword
C      subroutine ftgcvd - read real*8 values from table column
C      subroutine ftgcve - read real*4 values from table column
C
C *********************************************************************************
	subroutine fxdread(iunit, infile, inopen, block, nrows, 
     &     mintime, maxtime, timename, ratename, errname, timecol,
     &     time, rate, error, timedel, runit, timeunit, tzero, maxsize,
     &     abort)   


C data declarations
	integer maxsize
	integer iunit, block, nrows

	character *(*) infile, runit, timeunit
        character *(*) timename, ratename, errname
 	double precision time(maxsize), timedel, mintime, maxtime
	real rate(maxsize), error(maxsize)
	logical abort, inopen, timecol
	
	logical anynull, ratecol, errcol
	character(8) keyroot, ttype(10), tunit(10), spanunit
	character(160) filename
	character(80) errstr
 	character(70) context, comment 
	double precision nlvald, tzero, mjdref
        real nlvale
        integer timepos, ratepos, errpos
	integer startno, maxkeys, ncol, mcol
	integer j, ftstatus, frow, felem, extnum, htype, status

        startno = 1
        maxkeys = 10
        anynull = .false.
        nlvald = -100.
        nlvale = -100.
        frow = 1
        felem = 1
        timecol = .false.
        ratecol = .false.
        errcol = .false.
	ftstatus = 0
        
c get the filename and extension number
c  ? What does this do that the par file didn't ?
	call fcpars(infile,filename,extnum,ftstatus)

c open the input FITS file
	call ftopen(iunit,filename,0,block,ftstatus)
	if (ftstatus .ne. 0) then
	    context = 'unable to open infile'
	    call fcerr(context)
	    goto 999
	endif
 	inopen = .true.

c move to the extension   
	call ftmrhd(iunit,1,htype,ftstatus)
	if (ftstatus .ne. 0) then
	  errstr = 'error moving to extension number '
	  call fcerr(context)
	  goto 999
	endif

c      Assumed a BINNED light curve rather than an EVENT list
        
c      Get info on number of columns and their content
        keyroot = 'TTYPE'
        call ftgkns(iunit,keyroot,startno,maxkeys,ttype,ncol,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'error reading TTYPE keywords'
           call fcerr(context)
           goto 999
        endif

        keyroot = 'TUNIT'
        call ftgkns(iunit,keyroot,startno,maxkeys,tunit,mcol,ftstatus)
        if (ftstatus .ne. 0) then
           context = 'error reading TUNIT keywords'
           call fcerr(context)
           goto 999
        endif
        
c      get the number of rows from keyword
        call ftgkyj(iunit,'naxis2',nrows,comment,ftstatus)
        if (ftstatus .ne. 0) then
           errstr = 'error reading nrows keyword from FITS file'
           call fcerr(errstr)
           goto 999
        endif

C      get TIMEUNIT value (units for TIMEZER and TIMEDEL)
       call ftgkys(iunit,'TIMEUNIT',spanunit,comment,ftstatus)
       if (ftstatus .ne. 0) then
          errstr = 'error getting TIMEUNIT value'
          call fcerr(errstr)
          goto 999
       endif

C      get MJDREF value (if it doesn't exist (ftstatus = 202), simply
c      set it to 0.0)
c      Now using xftgkyd which will check for MJDREFI/MJDREFF if MJDREF
c      isn't found  MJT 4/18/96
	call xftgkyd(iunit,'MJDREF',mjdref,comment,ftstatus)
	if (ftstatus .ne. 0 .and. ftstatus .ne. 202) then
	   errstr = 'error getting MJDREF value'
	   call fcerr(errstr)
	   goto 999
	endif
	if (ftstatus .eq. 202) then
	   mjdref = 0.0
           ftstatus = 0
        endif
       
c      get the bin size of the data (in TIMEUNIT units)
c      if TIMEDEL does not exist, report this in status and continue
       call ftgkyd(iunit,'TIMEDEL',timedel,comment,ftstatus)
       if (ftstatus .eq. 202) then
          status = 1202
          ftstatus = 0
       else
          if (ftstatus .ne. 0) then
             errstr = 'error getting TIMEDEL value'
             call fcerr(errstr)
             goto 999
          endif
       endif
       
C      get TIMEZERO value (if it doesn't exist (ftstatus = 202), simply
c      set it to 0.0)
c      Now using xftgkyd (checks for TIMEZERI/TIMZERF if TIMEZERO itself
c       is not found JCL 6/21/96
       call xftgkyd(iunit,'TIMEZERO',tzero,comment,ftstatus)
       if (ftstatus .ne. 0 .and. ftstatus .ne. 202) then
          errstr = 'error getting TIMZERO value'
          call fcerr(errstr)
          goto 999
       endif
	if (ftstatus .eq. 202) then
	   errstr = 'Warning - TIMEZERO not found, using 0.0'
	   call fcerr(errstr)
           tzero = 0.0
	   ftstatus = 0
	endif
	

C      Reckon TZERO value to include MJDREF, converting TZERO to days
c      if necessary
         if (spanunit .eq. 's') then
            tzero = mjdref + tzero / 86400.
         else
            tzero = mjdref + tzero
         endif

c      Determine column numbers of TIME, RATE and ERROR columns, if present
c      For RATE, get the units on the RATE column as well
         do j = 1,ncol
            if (TTYPE(j) .eq. timename) then
               timecol = .true.
               timepos = j
               timeunit = TUNIT(j)
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
            errstr = 'Requested Intensity column not found'
            call fcerr(errstr)
            go to 999
         endif
c MJT 15July96 (g77/linux) change to .eqv./.neqv. from .eq./.ne.
c     16Aug  -- treating logicals in a more sensible way
         if ((errname .ne. 'ERROR') .and. (.not. errcol)) then
            abort = .true.
            errstr = 'Requested Error column not found'
            call fcerr(errstr)
            go to 999
         endif
         
c      Read file with TIME column present
         if (timecol) then

c      Reckon bin size into units of the time array
           if (spanunit .ne. timeunit) then
              if (timeunit .eq. 's') timedel = 86400. * timedel
              if (timeunit .eq. 'd') timedel = timedel / 86400.
           endif
           
c      read the time, counts and error
c      (assume TIME in column 1, RATE in column 2, and ERROR (if present)
c           in column 3)
           call ftgcvd(iunit,timepos,frow,felem,nrows,nlvald,time,
     +              anynull,ftstatus)
           call ftgcve(iunit,ratepos,frow,felem,nrows,nlvale,rate,
     +              anynull,ftstatus)
           if (errcol) call ftgcve(iunit,errpos,frow,felem,nrows,
     +        nlvale,error,anynull,ftstatus)
	
           
c      Read FITS file having implicit TIME column
        else

c      Construct the Time array
         do j = 1, nrows
            time(j) = (j-1) * timedel
         end do
         timeunit = spanunit
         
c      Read the RATE and ERROR columns
c      (Assume RATE is in column 1, ERROR (if present) in col 2)
         call ftgcve(iunit,ratepos,frow,felem,nrows,nlvale,rate,anynull,
     &        ftstatus)
         if (errcol)
     &        call ftgcve(iunit,errpos,frow,felem,nrows,nlvale,error,
     &        anynull,ftstatus)    
      endif

      
c      Assign absolute min & max times
      if (timeunit .eq. 's') then
         mintime = tzero + time(felem) / 86400.
         maxtime = tzero + time(nrows) / 86400.
      else
         mintime = tzero + time(felem)
         maxtime = tzero + time(nrows)
      endif

c  Exit subroutine
999	continue
	if (ftstatus .ne. 0) call fcerrm(ftstatus)

1000	format(A34,I3)
	return
	end
