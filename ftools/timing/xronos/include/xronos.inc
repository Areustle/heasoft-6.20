C
C Include file for all XRONOS programs
C
C Collected by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      February/March, 1995
C
C          maxseries = maximum number of time series allowed
C          maxbintype = maximum binning types: nbins, newbins, interval
C          maxopts = maximum number of file option suffixes
      integer maxseries, maxbintype,maxopts
      parameter (maxbintype = 3)
      parameter (maxseries = 4)
C     fix cser,cfile_pars,ncolpfm data statements in xronon5_init.inc
C     if you change maxseries

      character(1) cser(maxseries)
      character(10) cfile_pars(maxseries)

      integer nser,ncolpfm(maxseries)
      character(8) progname
      character(10) progtype

      REAL*8 dv, dtint, dtstat, dtstot,dtintt,
     &       dtstas(maxseries), dtstos(maxseries), dtstate, dtstote, 
     $     dtintte




c        copt = array of options to be decoded (strings)
c        iopt = array of flag options
c        mopt = array of math options
c        dopt = array of constants for math options

c  Options consist of 2 chars. and in some cases a numerical value up to
c  8 digits. The maximum no. of chars in one option is 10.
c
c  Note that the array of options copt*10(10) is decoded in
c  a) flag options iopt(10) (0= no= df, 1=yes)
c  b) matemathical options mopt(10) and constants dopt(10) to be executed
c     In the same order as the input string
c
c  List of supported options:
c
c  BT    => iopt(1) = 1    ; apply Barycentric Time correction (rate buff)
c  FEn   => iopt(1) = n    ; start loading counts at energy channel n in
c                            the FITS y-vector.
c  FRn   => iopt(2) = n    ; start reading infile from record n (First Rec)(rb)
c                          ;                     (from row n for FITS)
c  LRn   => iopt(3) = n    ; stop reading infile at record n (Last Rec)(rb)
c                          ;                     (at row n for FITS)
c  DN    => iopt(4) = 1    ; Not to do what is described below (which is done
c                            by d/f): (ME rate buff)
c                            apply Dead Time correction to counts and variance
c                            (to variance only if it is already applied to the
c                             counts) (for ME only) (this if all the relevant
c                            info is in the header otherwise see 'DV')
c                            (option 'MA' can be used to apply a dead time
c                            correction factor to the counts)
c  LEn   => iopt(4) = n    ; stop loading counts at energy channel n in
c                            the FITS y-vector.
c  VXn   => iopt(5) = n    ; use vector n as X-axis (time) (for qdp or FITS)
c                            (a neg. value means stepped X-axis according to
c                             file header line "!!H")
c  VYn   => iopt(6) = n    ; use vector n as Y-axis (for qdp and FITS infiles)
c  FSn   => iopt(7) = n    ; Find Subfile n (for z-files)
c
c  VCn   => iopt(7) = n    ; use column n for Energy Channel in FITS infiles
c
c  VEn   => iopt(8) = n    ; use vector n as Exposure-axis (for qdp)
c                                       or as Dead Time (FITS)
c  VE0   => iopt(8) = -99  ; turn off GTIs (FITS).
c  VSn   => iopt(9) = n    ; use vector n as Y-error-axis (for qdp and FITS)
c  RTn   => iopt(10)= n    ; use extension n for the rate table.
c                            (where the first extension is
c                            n = 1 and the primary array is irrelevant)
c
c  MUx   => mopt(nma) = 10 ; multiply data and errors by x (MUltiply)
c  MDx   => mopt(nma) = 11 ; multiply data by x (Multiply Data)
c  MEx   => mopt(nma) = 12 ; multiply errors by x (Multiply Errors)
c  MAx   => mopt(nma) = 13 ; as MU but divide exposure by x (Multiply All)
c                            (used to have possib. to reconstruct photon stat)
c  DIx   => mopt(nma) = 20 ; divide data and errors by x (DIvide)
c  DDx   => mopt(nma) = 21 ; divide data by x (Divide Data)
c  DEx   => mopt(nma) = 22 ; divide  errors by x (Divide  Errors)
c  DAx   => mopt(nma) = 23 ; as DI but multiply exposure by x (Divide All)
c                            (used to have possib. to reconstruct photon stat)
c  AAx   => mopt(nma) = 30 ; add data and errors with x (Add All)
c  ADx   => mopt(nma) = 31 ; add data with x (Add Data)
c  AEx   => mopt(nma) = 32 ; add errors with x (Add Errors)
c  SAx   => mopt(nma) = 40 ; subtract data and errors with x (Subtract All)
c  SDx   => mopt(nma) = 41 ; subtract  data with x (Subtract Data)
c  SEx   => mopt(nma) = 42 ; subtract  errors with x (Subtract Errors)
c  QAx   => mopt(nma) = 33 ; add to data the square of data multiplied by x
c                            and sum to errors the product of data and error
c                            multiplied by x. This is used to multiply data
c                            and errors by a linear interpolation constant
c                            y'=y(a+by)=ya+byaya/aa=ya+cyaya with c=b/aa
c                            sy'=sy(a+by)=sya+byasya/aa=sya+cyasya
c                            (sy = error). This can be done by using options:
c                            MUa QAc (where a and c are those used above).
c  QDx   => mopt(nma) = 34 ; as above but for data only
c  QEx   => mopt(nma) = 35 ; as above but for errors only
c
c  STx   => mopt(nma) = 50 ; shift times by x days (Shift Times)
c  SSx   => mopt(nma) = 51 ; shift times by x seconds (Shift Seconds)
c
c  DVx   => mopt(nma) = 60 ; apply Dead Time correction to variance
c                            by providing avg. count rate BEFORE
c                            dead time correction x, or AFTER dead time
c                            correction -x (i.e. negative) (for ME rate buff)

      parameter (maxopts = 15)
      character(10) copt(maxopts)
      integer iopt(maxopts),mopt(maxopts)
      double precision dopt(maxopts)

C parameter arrays:
C     These arrays are used to pass parameter values around.  Current
C     meanings are:
C
C   cpf:
C     1 - clobber/append - outfile status (NEW/UNKNOWN) needs reworking
C     2 -
C     3 - outfileroot - the root filename for output files (if DEFAULT use
C                       1st infile name
C     4 -
C     5 - progname - the (abreviated) program identifier
C
C   ipf:
C     1 - forcestart - force interval start at window start (boolean)
C     2 - errorbars - how to calculate error bars
C     3 - exposure - use exposure profile (boolean)
C     4 - normalization - how to normalize the data
C     5 - simultaneous - whether to force simultaneousness (boolean)
C     6 - gapfill - how to calculate running gap fill routine
C     7 -
C     8 -
C     9 -
C    10 -
C
C   rpf:
C     1 - spwinbefore - seconds to include in special window before start (double)
C     2 - spwinafter - seconds to include in special window after stop (double)
C     3 - rescale - value to use to rescale results at output (double)
C     4 - offset - value to use to offset results at output (double)
C     5 -
C
C   dpf:
C     1 -
C     2 -
C     3 -
C     4 -
C     5 -
C
C   iflags:
C     1 -
C     2 - tunits - time axis units {0=s from start,1=s,2=h,3=d}
C     3 -
C     4 -
C     5 - nwito - total number of all types of windows
C     6 - errorbars - how to calculate error bars = ipf(2)
C     7 - normalization - how to normalize the data = ipf(4)
C     8 - number of result columns/frame
C     9 - number of independant variable columns
C    10 - nse - number of time series
C    11 - default plot file type (currently 1 or 2)
C    12 -
C    13 - number of points after final rebinning
C    14 -
C    15 - nmaxa - maxiumum number of analysis points (obsol., dyn. alloc.)
C    15 - nfast - 0 use Slow FFT algorithm 1 use Fast FFT algorithm
C    16 - simultaneous - whether to force simultaneousness (boolean) = ipf(5)
C    17 - gapfill - how to calculate running gap fill routine = ipf(6)
C    18 -
C    19 - flatexpo - If yes bin exposure=1 for event data (efsearch and efold)
C    20 -
C
C   rflags:
C     1 -
C     2 - rescale - value to use to rescale results at output = rpf(3)
C     3 - offset - value to use to offset results at output = rpf(4)
C     4 - expected white noise level = rwn, based on ipf(4)=normalization
C     5 -
C     6 -
C     7 -
C     8 -
C     9 -
C    10 -
C
c  iflags(1) = file type (=0 data and errors, other types TBD)
c  iflags(2) = unit of time axis (if required) (=0 for secs from start
c              1=s of hour, 2=h of day, 3=d) (irrelevant for binary files)
c  iflags(3) = trend removal (power)
c  iflags(4) = trend removal mode
c  iflags(5) = tot. no of windows
c  iflags(6) = min for scatter estimate of sigma in frame and rebin
c  iflags(7) = analysis normalization flag
c  iflags(8) = no. of result columns/frame (from main) (2n+1)
c  iflags(9) = no. of independent variable columns (from main)(2n)
c  iflags(10) = no. of input series (from main)
c  iflags(11) = no. of boxes/plot (from main)
c  iflags(12) = max no. of frames expected (excluding frame 0) (from main)
c  iflags(13) = no. of result lines, after rebinning if any (from xrtyana)
c  iflags(14) = no. of analysis points (set internally)
c  iflags(15) = max. no. of analysis points (nmaxa) (obsol., dyn. alloc.)
c  iflags(16) = simoultaneousness flag
c  iflags(17) = no. of newbins in running mean for gap filling
c  iflags(18) = no. of freqs. for period search (only for dps)(xrgetdps) 
c
c  rflags(1) = rebin = rebinning constant (set internally)
c  rflags(2) = rescaling factor for results
c  rflags(3) = additive constant for results
c  rflags(4) = expected white noise level (only for power spectra)
c  rflags(5) = single freq.prob. for periodicity search(only for dps)(xrgetdps) 
c  rflags(6) = start freq. for periodicity search(only for dps)(xrgetdps) 
c  rflags(7) = stop freq. for periodicity search(only for dps)(xrgetdps) 
c

      integer maxcpf, maxipf, maxrpf, maxdpf, maxiflags, maxrflags
      parameter (maxcpf = 5)
      parameter (maxipf = 10)
      parameter (maxrpf = 5)
      parameter (maxdpf = 5)
      parameter (maxiflags = 20)
      parameter (maxrflags = 10)

      character(40) cpf(maxcpf)
      integer ipf(maxipf), iflags(maxiflags)
      real rpf(maxrpf), rflags(maxrflags)
      double precision dpf(maxdpf)

C
C     Basic general variables:
C
C          status flag
      integer status
C          task common
      character(40) taskname
      common /task/ taskname
      character(80) context
C
C     windows variables
C
C           nwi - number of each type of window:
C           nwito = iflags(5) - total number of all types of windows
C     1 - number of time windows
C     2 - number of phase windows
C     3 - number of intensity windows series 1 original bins
C     4 - number of intensity windows series 1 new bins
C     5 - number of intensity windows series 1 intervals
C     6 - number of intensity windows series 2 original bins
C     7 - number of intensity windows series 2 new bins
C     8 - number of intensity windows series 2 intervals
C     9 - number of intensity windows series 3 original bins
C    10 - number of intensity windows series 3 new bins
C    11 - number of intensity windows series 3 intervals
C    12 - number of intensity windows series 4 original bins
C    13 - number of intensity windows series 4 new bins
C    14 - number of intensity windows series 4 intervals
C    15 - number of exposure windows series 1 original bins
C    16 - number of exposure windows series 1 new bins
C    17 - number of exposure windows series 1 intervals
C    18 - number of exposure windows series 2 original bins
C    19 - number of exposure windows series 2 new bins
C    20 - number of exposure windows series 2 intervals
C    21 - number of exposure windows series 3 original bins
C    22 - number of exposure windows series 3 new bins
C    23 - number of exposure windows series 3 intervals
C    24 - number of exposure windows series 4 original bins
C    25 - number of exposure windows series 4 new bins
C    26 - number of exposure windows series 4 intervals
C
C           pwi(1) = phase epoch
C           pwi(2) = phase period
C           pwia, pwio - phase window start and stop
C
C           fwia, fwio - flux window start and stop
C           ewia, ewio - exposure window start, stop
C
C     maxwintype = time + phase + maxseries*maxbintype (exposure + flux) = 26

      integer maxwintype, maxphwin, maxfluxwin, maxexpwin, maxtimewin
      parameter (maxwintype = 26)
      parameter (maxphwin = 10)
      parameter (maxfluxwin = 10)
      parameter (maxexpwin = 1)
      parameter (maxtimewin = 1000)

      integer nwi(maxwintype)
      real pwia(maxphwin),pwio(maxphwin)
      real fwia(maxseries*maxbintype,maxfluxwin)
      real fwio(maxseries*maxbintype,maxfluxwin)
      real ewia(maxseries*maxbintype),ewio(maxseries*maxbintype)
      double precision twia(maxtimewin),twio(maxtimewin),pwi(2)


C input file information
C maximum number of files in one series: previously nfilma
      integer nfilemax
      parameter (nfilemax = 1000)


c default no. of newbins/intv.
      integer nbdf

c =-1 for programs with no intv average and rebinning
      integer nintfm
c =1 if no. of newbins/intv. must be a power of 2
      integer ipow2
c =1 to write all bins to terminal (and log)
      integer ilda
c severity code to control strictness of minimum newbin sizes (currently 0 for
C	"lc" tasks, nozero for others)
      integer isev

C expected number of intervals
      integer nintv

C actual number of newbins/interval
      integer nbint

C phase bins/period
      integer nbin

C size of newbin/phasebin array
      integer nkount

C number of analysis points
	integer nanal

C one line summary from all infiles
      character(80) csumm


C trend removal flag (iflags(3))
      integer itre

C trend removal mode (1=subtract, 2=divide, 3=return trend) (ipflags(4))
      integer itremo

C rebin result constant (>1 linear, <-1 log, otherw. none)
      real rebin

C start time of first infile
      double precision dtsta

C stop time of last infile
      double precision dtsto

c newbin duration in seconds
      double precision dtnb

C frequency of 1st newbin in Hz and step size
      double precision dxsta(maxseries), dxstep(maxseries)

C expected white noise level = rflags(4)
      real rwn

C defined white noise level to be subtracted, if any
      real rnoise

C input and output filenames
      character(160) cfile(nfilemax,maxseries)
      character(160) cfilo
C output file type 
      integer oftype

C number of input files
      integer nfil(maxseries)

C Epoch in decimal days
      double precision depoch
      
C Period in seconds
      double precision dper

C Period derivative
      double precision dpdot

C Period resolution for period search
      double precision dres

C Number of periods to search
      integer nper

C  Running total of number of input bins/events
      integer nobins(maxseries)
C  Running total of number of input gtis
      integer ngtis(maxseries)

C
C frame variables
C
c
c        (total means summed on all good intvs. and frames so far)
c
c        irtsta(1)= tot. no. of good data intvs.
c        irtsta(2)= no. of good newbins in frame
c        irtsta(3)= no. of good    bins in frame
c        irtsta(4)= no. of good   intvs in frame
c        irtsta(5)= tot. no. of good bins
c        irtsta(6)= tot. no. of good newbins
c        irtsta(9)= current frame no.
c        irtsta(10)= tot. no. of overflowing bins
c        irtsta(11)= tot. no. of bins excluded by time windows
c        irtsta(12)= tot. no. of bins excluded by phase windows
c        irtsta(13)= tot. no. of bins excluded by ints. windows
c        irtsta(14)= tot. no. of newbins excluded by ints. windows
c        irtsta(15)= tot. no. of intvs.  excluded by ints. windows
c        irtsta(16)= tot. no. of bins excluded by exps. windows
c        irtsta(17)= tot. no. of newbins excluded by exps. windows
c        irtsta(18)= tot. no. of intvs.  excluded by exps. windows
c        irtsta(19)= tot. no. of bins excluded because of neg. newbin index
c        irtsta(20)= tot. no. of gap bins
c
c        rrtsta(1)= avg cts/s in frame
c        rrtsta(2)= avg frac. expos in frame
c        rrtsta(3)= avg observed variance in frame
c        rrtsta(4)= avg expected variance in frame
c        rrtsta(5)= avg observed third moment in frame
c        rrtsta(6)= min cts/s in frame
c        rrtsta(7)= max cts/s in frame
c        rrtsta(8)= avg excess variance in frame
c        rrtsta(9)= avg chisquare in frame
c        rrtsta(10)= rms variability
c        rrtsta(11)= error on avg.
c        rrtsta(13)= error on observed variance
c        rrtsta(14)= error on expected variance
c        rrtsta(15)= error on third moment
c        rrtsta(18)= error on excess variance
c        rrtsta(19)= error on chisquare
c        rrtsta(20)= error on rms
c
c array of drtsta is now 20 as for the integer and real values. The first 
c position is now used only for the start time of the frame epoch is 
c stored in in position 6
c  
c        drtsta(1)= center time of 1st newbin in current frame or epoch for folding
c        drtsta(2)= time of baryc. of frame
c        drtsta(3)= center time of 1st good newbin in frame
c        drtsta(4)= center time of last good newbin in frame
c        drtsta(5)= period for folding
c        drtsta(6)= epoch for folding
c        drtsta(7)= start time of the first interval 

      integer maxistat, maxrstat, maxdstat
      parameter (maxistat = 20)
      parameter (maxrstat = 20)
      parameter (maxdstat = 20)

      integer irtsta(maxistat,maxseries)
      real rrtsta(maxrstat,maxseries)
      double precision drtsta(maxdstat,maxseries)
      integer iframe


C
C     QDP parameters:
C         iqdp
C
C     1 - 0 for no plot, 1 for plot
C     2 - 1 qdp file specified (this is ALWAYS true now in X5)
C     3 - number of boxes/plot = iflags(11)
C     4 - 
C     5 -
C     6 -
C     7 -
C     8 -
C     9 -
C    10 -
C
C         cqdp
C
C     1 - pgplot plot device
C     2 - QDP cmd file name (df: task_1.pco)
C     3 - 
C     4 - 
C     5 -
C     6 -
C     7 -
C     8 -
C     9 -
C    10 -
C
	integer maxqdp
	parameter (maxqdp = 10)
	integer iqdp(maxqdp)
	character(160) cqdp(maxqdp)
	real rqdp(maxqdp)

C	maximum chattiness present, for optimizing
	integer ichat

C	best period index for efsearch
	integer kmax(4)


C User defined parameters
	integer maxuserp
	parameter (maxuserp = 50)

	integer iuser(maxuserp)
	double precision duser(maxuserp)
	real ruser(maxuserp)
	logical buser(maxuserp)
	character(160) suser(maxuserp)
