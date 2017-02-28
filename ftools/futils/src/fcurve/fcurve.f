C******************************************************************************
C SELECTOR TASK:
C      fcurve
C
C FILE:
C      fcurve.f
C
C DESCRIPTION:
C      Makes a "light curve" or rates file from an input events list.  For
C      specified columns containing the time and the quantity to be binned, the
C      tool bins up the quantity with the specified time binning.  The binning
C      has five modes, in the first the output is total counts, in the second
C      the output is total counts/time,  in the third the output is the
C      mean value, in the fourth the total number of events falling in a bin
C      is counted and in the fifth the number of events/time is output.
C
C AUTHOR:
C      Janice Tarrant  3/11/92
C
C MODIFICATION HISTORY:
C       October, 1992 (EAG) - changed number of bins to bin size
C                           - allow for no GTI file
C                           - allow program to calculate low, high
C                             and bin size if INDEF is requested
C       2/19/93 (EAG)   - allow for double precision
C                         add copyprime parameter
C                         use fcerr for error output
C       3/15/93 (EAG)   - add copyall parameter
C       4/1/93  (EAG)   - add Event_rate and Event_sum output modes
C       4/30/93 (EAG)   - allow for infinite number of bins
C                         enforce binsize over min,max values
C                         some vector column support
C       7/7/93  (EAG)   - allow for multiple depenant variables
C       2/24/94 (EAG) 2.8a - fixed logic problem for no GTI file and
C                            calculation of offset values
C       3/14/94 (EAG) 2.8b - dynamic memory, rationalized character lengths
C       8/22/94 (EAG) 3.0a - clobber capability
C       6/27/95 MJT        - now accepts lower case 'none' (in fignkc)
C       1/2/1996 Jeff Guerber 3.0c - Changed all char*16 column-name variables
C          to char*40.  fcurve: timecol, outtimecol, outlive;  firfwc: ttype,
C          gtype, ntype;  fignkc: errlist, colist.
C       2/25/96 (Srilal) 3.0d - timestamp added
C
C       May, 1997 4.0 (Banashree Seifert)
C           . the calculation for the last bin was not right and is fixed in 
C             subroutine FILTCV
C       10/15/97 PDW 4.0a Replace old get header routines
C       6/30/98 Ning Gan. 4.0b Updated for the new format of Date
C                         keyword.
C       12/01/06 BKI 4.1 Allow user to input gtifile parameter as filename+n
C                or filename[NAME] instead of assuming GTI is the first
C                extension.  Error if specified extension is not GTI.
C
C NOTES:
C      fcurve supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile     - input FITS file and extension number
C      gtifile    - GTI file name
C      outfile    - output file name
C      timecol    - name of the output time column
C      columns    - column names for binned parameter(s)
C      binsz      - size of bins
C      lowval     - lower time limit
C      getmin     - whether program should find minimum
C      highval    - upper time limit
C      getmax     - whether program should find maximum
C      mode       - binned parameter mode
C      gticols    - column names for start and stop times
C      gtidate    - GTI observation start date keyword
C      gtitime    - GTI observation start time keyword
C      extname    - keyword for extension name
C      obsdate    - observation start date keyword
C      obstime    - observation start time keyword
C      outtimecol - column name for bin centers
C      outcol     - column name for binned parameter(s) values
C      outerr     - column name for statistical rms error(s)
C      outlive    - column name for fractional integration time
C      copyprime  - whether to copy primary array and important keywords
C      copyall    - whether to copy all other extensions to the output file
C      sensecase  - whether values should be case sensitive
C
C CALLED ROUTINES:
C      subroutine gcurve - gets parameters from environment
C      subroutine firfwc - read input FITS file and write light curve file
C
C******************************************************************************

      subroutine fcurve
      character(160) infile, outfile, gtifile, columns
      character(40) timecol, outtimecol, outlive
      character(80) obsdate, obstime, mode,
     &     gticols, gtidate, gtitime, extname, outcol, outerr
      double precision binsz
      double precision lowval, highval
      logical getmin, getmax, copyprime, copyall, sensecase

      integer status

      character(40) taskname
      common /task/ taskname

      taskname = 'fcurve4.1'

      call ftcmsg

      infile = ' '
      timecol = ' '
      columns = ' '
      obsdate = ' '
      obstime = ' '
      mode = ' '
      gtifile = ' '
      gticols = ' '
      gtidate = ' '
      gtitime = ' '
      outfile = ' '
      extname = ' '
      outtimecol = ' '
      outcol = ' '
      outerr = ' '
      outlive = ' '
      status = 0

C  get the parameters from the par file
      call gcurve(infile,columns,obsdate,obstime,mode,binsz,lowval,
     &     highval,gtifile,gticols,gtidate,gtitime,outfile,
     &     extname,outtimecol,outcol,outerr,outlive,getmin,
     &     getmax, copyprime, copyall, timecol, sensecase,
     &     status)
      if (status .ne. 0) goto 999

C  read in the FITS file and write out the light curve file
      call firfwc(infile,columns,obsdate,obstime,mode,binsz,lowval,
     &     highval,gtifile,gticols,gtidate,gtitime,outfile,
     &     extname,outtimecol,outcol,outerr,outlive,getmin,
     &     getmax, copyprime, copyall, timecol, sensecase)

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      gcurve
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Janice Tarrant  3/11/92
C
C MODIFICATION HISTORY:
C       10/23/92 (EAG) - changed number of bins to bin size
C                      - allow for INDEF input
C       2/19/93  EAG   - add copyprime parameter
C   3/11/93  JCI - Add the ability to subtract the starting time
C                  from all the times before binning, so QDP can
C                  plot correctly.
C       3/15/93 EAG    - added copyall parameter
C
C NOTES:
C      gcurve uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gcurve(infile,columns,obsdate,obstime,mode,binsz,lowval,
C                  highval,gtifile,gticols,gtidate,gtitime,outfile,
C                  extname,outtimecol,outcol,outerr,outlive,getmin,
C                  getmax,copyprime, copyall, timecol, sensecase, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      columns - column names for binned parameter and time
C      obsdate - observation start date keyword
C      obstime - observation start time keyword
C      mode    - binned parameter mode
C      binsz   - size of bins
C      lowval  - lower time limit
C      highval - upper time limit
C      gtifile - GTI file name
C      gticols - column names for start and stop times
C      gtidate - GTI observation start date keyword
C      gtitime - GTI observation start time keyword
C      outfile - output file name
C      extname - keyword for extension name
C      outtimecol - column name for bin centers
C      outcol - column name for binned parameter values
C      outerr - column name for statistical rms error
C      outlive - column name for fractional integration time
C      getmin  - whether program should find minimum
C      getmax  - whether program should find maximum
C      copyprime - whether to copy primary array and important keywords
C       copyall  - whether to copy all other extension to output
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real parameter
C      subroutine uclgst - get string parameter
C
C******************************************************************************
      subroutine gcurve(infile,columns,obsdate,obstime,mode,binsz,
     &     lowval,highval,gtifile,gticols,gtidate,
     &     gtitime,outfile,extname,outtimecol,outcol,
     &     outerr,outlive,getmin,getmax,
     &     copyprime, copyall, timecol, sensecase, status)
      character*(*) infile, outfile, gtifile
      character*(*) columns, obsdate, obstime, mode, timecol,
     &     gticols, gtidate, gtitime, extname,
     &     outtimecol, outcol, outerr, outlive
      double precision binsz
      double precision lowval, highval
      logical getmin, getmax, copyprime, copyall, sensecase

      character(80) context
      integer status

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         context = 'could not get infile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the GTI file
      call uclgst('gtifile',gtifile,status)
      if (status .ne. 0) then
         context = 'could not get gtifile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output ASCII file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
         context = 'could not get outfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the time column name
      call uclgst('timecol',timecol,status)
      if (status .ne. 0) then
         context = 'could not get timecol parameter'
         call fcerr(context)
         goto 999
      endif

C  get the column names for the binned parameter(s)
      call uclgst('columns',columns,status)
      if (status .ne. 0) then
         context = 'could not get columns parameter'
         call fcerr(context)
         goto 999
      endif

C  get the size of bins
      call uclgsd ('binsz',binsz,status)
      if (status .eq. 3) then
         status = 0
         binsz = -1
      else if (status .ne. 0) then
         context = 'could not get binsz parameter'
         call fcerr(context)
         goto 999
      endif

C  get the lower time limit
      getmin = .false.
      call uclgsd('lowval',lowval,status)
      if (status .eq. 3) then
         status = 0
         getmin = .true.
      else if (status .ne. 0) then
         context = 'could not get lowval parameter'
         call fcerr(context)
         goto 999
      endif

C  get the upper time limit
      getmax = .false.
      call uclgsd('highval',highval,status)
      if (status .eq. 3) then
         status = 0
         getmax = .true.
      else if (status .ne. 0) then
         context = 'could not get highval parameter'
         call fcerr(context)
         goto 999
      endif

C  get the binned parameter mode
      call uclgst('binmode',mode,status)
      if (status .ne. 0) then
         context = 'could not get binmode parameter'
         call fcerr(context)
         goto 999
      endif

C  get the column names for start and stop times
      call uclgst('gticols',gticols,status)
      if (status .ne. 0) then
         context = 'could not get gticols parameter'
         call fcerr(context)
         goto 999
      endif

C  get the GTI observation date keyword
      call uclgst('gtidate',gtidate,status)
      if (status .ne. 0) then
         context = 'could not get gtidate parameter'
         call fcerr(context)
         goto 999
      endif

C  get the GTI observation time keyword
      call uclgst('gtitime',gtitime,status)
      if (status .ne. 0) then
         context = 'could not get gtitime parameter'
         call fcerr(context)
         goto 999
      endif

C  get the extension name keyword
      call uclgst('extname',extname,status)
      if (status .ne. 0) then
         context = 'could not get extname parameter'
         call fcerr(context)
         goto 999
      endif

C  get the observation date keyword
      call uclgst('obsdate',obsdate,status)
      if (status .ne. 0) then
         context = 'could not get obsdate parameter'
         call fcerr(context)
         goto 999
      endif

C  get the observation time keyword
      call uclgst('obstime',obstime,status)
      if (status .ne. 0) then
         context = 'could not get obstime parameter'
         call fcerr(context)
         goto 999
      endif

C  get the bin centers column name
      call uclgst('outtimecol',outtimecol,status)
      if (status .ne. 0) then
         context = 'could not get outtimecol parameter'
         call fcerr(context)
         goto 999
      endif

C  get the parameter values column name
      call uclgst('outcol',outcol,status)
      if (status .ne. 0) then
         context = 'could not get outcol parameter'
         call fcerr(context)
         goto 999
      endif

C  get the error column name
      call uclgst('outerr',outerr,status)
      if (status .ne. 0) then
         context = 'could not get outerr parameter'
         call fcerr(context)
         goto 999
      endif

C  get the fractional integration time column name
      call uclgst('outlive',outlive,status)
      if (status .ne. 0) then
         context = 'could not get outlive parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to copy primary array and important keywords
      call uclgsb('copyprime',copyprime,status)
      if (status .ne. 0) then
         context = 'could not get copyprime parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to all other extensions to output file
      call uclgsb('copyall', copyall,status)
      if (status .ne. 0) then
         context = 'could not get copyall parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to be case sensitive
      call uclgsb('sensecase', sensecase,status)
      if (status .ne. 0) then
         context = 'could not get sensecase parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      firfwc
C
C DESCRIPTION:
C      Reads the FITS file column and writes the light curve data to a new
C      FITS file
C
C AUTHOR:
C      Janice Tarrant  3/11/92
C
C MODIFICATION HISTORY:
C       October, 1992 (EAG) as above
C       February, 1993 (EAG) as above
C       1/2/1996 JRG - changed ttype, gtype, ntype to char*40
C
C NOTES:
C      firfwh uses FITSIO calls to read FITS file
C      vector elements in tables not supported
C
C USAGE:
C      call firfwc(infile,columns,obsdate,obstime,mode,binsz,lowval,highval,
C                  gtifile,gticols,gtidate,gtitime,outfile,extname,outtimecol,
C                  outcol,outerr,outlive,getmin,getmax, copyprime,
C                  copyall, timecol, sensecase)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      columns - column names for binned parameter and time
C      obsdate - observation start date keyword
C      obstime - observation start time keyword
C      mode    - binned parameter mode
C      nbins   - number of bins
C      lowval  - lower time limit
C      highval - upper time limit
C      gtifile - GTI file name
C      gticols - column names for start and stop times
C      gtidate - GTI observation start date keyword
C      gtitime - GTI observation start time keyword
C      outfile - output file name
C      extname - keyword for extension name
C      outtimecol - column name for bin centers
C      outcol - column name for binned parameter values
C      outerr - column name for statistical rms error
C      outlive - column name for fractional integration time
C      getmin  - whether program should find minimum value
C      getmax  - whether program should find maximum value
C      copyprime - whether to copy primary array and important keywords
C       copyall  - whether to copy all other extentions to output
C       timecol  - input time column name
C       sensecase - whether values should be case sensitive
C
C PRIMARY LOCAL VARIABLES:
C      maxcl      - maximum number of columns
C      maxgti     - maximum number of good time intervals
C      filename   - name of FITS file
C      context    - error message
C      extnum     - FITS file extension number
C      status   - FITSIO error number
C      ndate      - new FITS file date
C      ntime      - New FITS file time
C      ntype      - new FITS file column label
C      nform      - new FITS file column format
C      nunit      - new FITS file column physical unit
C      nrowlen    - new FITS file row length
C      nnrows     - new FITS file number of rows
C      nfields    - new FITS file number of fields
C      nbcol      - new FITS file first character column number
C      ngti       - number of good time intervals
C      ooffs      - observation time offset from new date/time
C      goffs      - GTI time offset from new date/time
C      binll      - bin lower limit
C      binul      - bin upper limit
C      bin_value  - bin value
C      bin_error  - bin value error
C
C CALLED ROUTINES:
C      function   fcstln - return length of character string (integer)
C      subroutine fccmpl - compare two lists for a subset
C      subroutine fcerr  - report error to STDERR
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcgcls - get column list based on parameter
C      subroutine fcpars - parse off filename and extension number
C      subroutine fignkc - get keywords for new FITS file
C      subroutine filubl - get the lower and upper bin limits
C      subroutine filtcv - get the ligth curve values
C      subroutine fimimc - get minimum and maximum column values
C      subroutine fctofs- get the observation and GTI time offsets
C      subroutine ftadef - define ASCII extension
C      subroutine ftbdef - define binary extension
C      subroutine ftclos - close a FITS file
C      subroutine ftcrhd - create a new header
C      subroutine ftghbn - get BINARY table header
C      subroutine xftgkys - get string keyword value
C      subroutine ftghpr - get primary header keywords from CHU
C      subroutine ftghtb - get ASCII table header
C      subroutine ftinit - create a new FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine fttopn - open a FITS file and move to first table HDU
C      subroutine ftphbn - put binary table header keywords into CHU
C      subroutine ftpdef - define structure of primary array
C      subroutine ftphis - put history keyword record
C      subroutine ftphpr - put primary header keywords into CHU
C      subroutine ftphtb - put ASCII table header keywords into CHU
C
C******************************************************************************

      subroutine firfwc(infile,columns,obsdate,obstime,mode,binsz,
     &     lowval,highval,gtifile,gticols,gtidate,
     &     gtitime,outfile,nextname,outtimecol,outcol,
     &     outerr,outlive,getmin,getmax,
     &     copyprime, copyall, timecol, sensecase)
      character*(*) infile, columns, obsdate, obstime, mode, gtifile,
     &     gticols, gtidate, gtitime, outfile, nextname,
     &     outtimecol, outcol, outerr, outlive, timecol

      double precision binsz
      double precision lowval, highval
      logical sensecase

      integer nbins
      integer maxcl, maxgti

C The maximum number of GTIs/bin (maxgti) can't be dynamicall allocated
C because it is different for each bin

      parameter (maxcl= 999)
      parameter (maxgti = 64)
      character(160) filename, history
      character(80) context
      character(40) ttype(maxcl), gtype(maxcl), ntype(maxcl)
      character(16) tform(maxcl), gform(maxcl), nform(maxcl)
      character(25) tunit(maxcl), gunit(maxcl), nunit(maxcl)
      character(40) colist(maxcl), gtilist(2)
      character(80) extname, odate, otime, comment, gextname,
     &     gdate, gtime, ndate, ntime
      logical inopen, gtiopen, outopen, exact, negflag, goodlist,
     &     getmin, getmax, simple, extend, copyprime
      logical copyall, dates, julian

      integer fcstln, i, trepeat, vrepeat, width, dtype
      integer status, iunit, gtiunit, ounit, block, extnum, htype,
     &     nrows, rowlen, tfields, tbcol(maxcl), varidat, tcols,
     &     colnum(maxcl), ghtype, gnrows, growlen, gfields,
     &     gbcol(maxcl), gvaridat, gtinum(2),
     &     nrowlen, nnrows, nfields, nbcol(maxcl), bitpix, naxis,
     &     naxes(99), pcount, gcount, length, colout, tcolnum
      integer binll, binul, ngti, livetime, bin_value, bin_error,
     &     bin_center, bin_num, nallocated, dstatus

      double precision ooffs, goffs

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
C	datatype	value
C	logical		1
C	integer*2	3
C	Integer		4
C	Long Integer	5
C	Real		6
C	Double		7
C	Complex		8

C  initialize variables
      status = 0
      iunit = 15
      gtiunit = 16
      ounit = 17
      inopen = .false.
      gtiopen = .false.
      outopen = .false.
      negflag = .false.
      exact = sensecase

C  Allow the mode parameter to be case insensitive
      call fcrvmd (mode, status)

C  get the filename and extension number
      call fcpars(infile,filename,extnum,status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C  if extension is 0 then give error and exit
      if (extnum .eq. 0) then
         context = 'primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C  open the input FITS file
      call ftopen(iunit,filename,0,block,status)
      if (status .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C  move to the extension number
      call ftmrhd(iunit,extnum,htype,status)
      if (status .ne. 0) then
         context = 'error moving to extension number '
         write(context,1000) context, extnum
         call fcerr(context)
         goto 999
      endif

C  get the header depending on the extension type
      if (htype .eq. 1) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,status)
      else
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,status)
      endif


C check that the time column exists
      call ftgcno (iunit, exact, timecol, tcolnum, status)
      if (status .ne. 0) then
         context = ' Time column name does not exist:' // timecol
         call fcerr (context)
         goto 999
      endif

C  check that column names exist and get their position in the input file
      if (((columns .eq. ' ') .or. (columns .eq. '-')) .and.
     &     (index(mode,'Event') .gt. 0)) then
         tcols = 0
      else
         call fcgcls(columns,colist,tcols,negflag)
         call fccmpl(tcols,tfields,colist,ttype,negflag,goodlist)
         if (.not. goodlist) then
            context = 'error in input column names'
            call fcerr(context)
            goto 999
         endif
         do 10 i = 1, tcols
            call ftgcno(iunit,exact,colist(i),colnum(i),status)
 10      continue
      endif
      if (status .ne. 0) then
         context = ' Error determining column numbers'
         call fcerr (context)
         goto 999
      endif


C  get the observation date and time
      if ((obsdate .eq. ' ') .or. (obsdate .eq. '-')) then
         dates = .false.
      else
         dates = .true.
         call xftgkys(iunit,obsdate,odate,comment,status)
         if (status .ne. 0) then
            context = 'error getting observation date'
            call fcerr(context)
            goto 999
         endif
C check for type of date
         if (index(odate,'/') .gt. 0. or.index(odate,'-').gt.0) then
            julian = .false.
         else
            julian = .true.
         endif
         if (.not. julian) then
            call xftgkys(iunit,obstime,otime,comment,status)
            if (status .ne. 0) then
               context = 'error getting observation time'
               call fcerr(context)
               goto 999
            endif
         endif
      endif

C get the repeat counts for each column and check that they are OK
      call ftbnfm (tform(tcolnum), dtype, trepeat, width, status)
      if (status .ne. 0) then
C assume was ascii column
         trepeat = 1
         status = 0
      endif
      do 20 i = 1, tcols
         call ftbnfm (tform(colnum(i)), dtype, vrepeat, width, status)
         if (status .ne. 0) then
C assume was ascii column
            vrepeat = 1
            status = 0
         endif
         if ((vrepeat .ne. trepeat) .and. (trepeat .ne. 1)) then
            context = ' Cannot deal with these vector columns'
            call fcerr (context)
            status = 1
            goto 999
         endif
 20   continue

C  open the GTI FITS file
      if (fcstln(gtifile) .gt. 1.and.gtifile(1:1).ne.'-' ) then
         call fttopn(gtiunit,gtifile,0,status)
         if (status .ne. 0) then
C could not open GTI file - assume don't want one
C but mention this fact if filename is not blank!! (EAG 2/19/93)
            context = 'WARNING: cannot find GTI file, '
     &           // 'continuing without'
            call fcecho(context)
            status = 0
            gdate = odate
            gtime = otime
            gtiopen = .false.
            ooffs = 0.D0
            goffs = 0.D0
            ndate = odate
            goto 100
         else
            gtiopen = .true.
         endif
      else
         gdate = odate
         gtime = otime
         gtiopen = .false.
         ooffs = 0.D0
         goffs = 0.D0
         ndate = odate
         goto 100
      endif

C  get the header depending on the extension type
      call ftghdt(gtiunit,ghtype,status)
      if (ghtype .eq. 1) then
         call ftghtb(gtiunit,maxcl,growlen,gnrows,gfields,gtype,gbcol,
     &        gform,gunit,gextname,status)
      else
         call ftghbn(gtiunit,maxcl,gnrows,gfields,gtype,gform,gunit,
     &        gextname,gvaridat,status)
      endif
      call upc(gextname)
      if (gextname .NE. 'GTI' .AND. gextname .NE. 'STDGTI') then
         context = 'Extension specified is not GTI: ' // gextname
         call fcerr(context)
         goto 999
      endif

C  check that column names exist and get their position in the GTI file
      if (gticols .eq. ' ') then
         context = 'error in GTI column names'
         call fcerr(context)
         goto 999
      endif
      call fcgcls(gticols,gtilist,colout,negflag)
      call fccmpl(2,gfields,gtilist,gtype,negflag,goodlist)
      if (.not. goodlist) then
         context = 'error in GTI column names'
         call fcerr(context)
         goto 999
      endif
      call ftgcno(gtiunit,exact,gtilist(1),gtinum(1),status)
      call ftgcno(gtiunit,exact,gtilist(2),gtinum(2),status)

C  get the GTI observation date and time
      if (dates) then
         call xftgkys(gtiunit,gtidate,gdate,comment,status)
         if (status .ne. 0) then
            context = 'error getting GTI observation date'
            call fcerr(context)
            goto 999
         endif
         if ( ( (index(gdate,'/') .gt.0) .and. (julian)) .or. 
     *     ((index(gdate,'-') .gt.0) .and. (julian)) ) then  
            context = ' GTI file has different style reference'
            call fcerr (context)
            status = 1
            goto 999
         endif
         if (.not. julian) then
            call xftgkys(gtiunit,gtitime,gtime,comment,status)
            if (status .ne. 0) then
               context = 'error getting GTI observation time'
               call fcerr(context)
               goto 999
            endif
         endif

C  get the time offset for the observation and GTI file
         call fctofs(odate,otime,gdate,gtime,ndate,ntime,ooffs,goffs,
     &        julian, status)
      else
         ooffs = 0.D0
         goffs = 0.D0
         ndate = odate
      endif

 100  continue

C  get the upper and lower time limits
      if (getmin .or. getmax)
     &     call fimimc(iunit,tcolnum,nrows,getmin,getmax,ooffs,
     &     trepeat, lowval,highval)

C figure bin size
      if (binsz .le. 0.) then
         nbins = 100
         binsz = (highval - lowval) / (nbins - 1)
      else
         nbins = (highval - lowval) / binsz + 1
      endif

C  get the keywords for the new FITS file
      call fignkc(htype, tunit, mode, nbins, timecol, columns,
     &     outtimecol, outcol, outerr, outlive, nrowlen, nnrows,
     &     nfields, ntype, nbcol, nform, nunit, status)
      if (status .ne. 0) goto 999

      length = fcstln(filename)
      history(1:20) = 'TASK:FCURVE on file '
      history(21:20+length) = filename
      history(21+length:35+length) = ' with GTI file '
      history(36+length:) = gtifile

C  open the new FITS file
      call ffinit(ounit,outfile,status)
      if (status .ne. 0) then
         context = 'unable to open outfile, may exist? ' // outfile
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C copy primary array, if so requested
      if ((copyprime) .or. (copyall)) then
         call ftmrhd (iunit, -extnum, htype, status)
         call ftcopy (iunit, ounit, 0, status)
      else

C  construct a simple primary header for the new file
         call ftmahd(iunit,1,htype,status)
         call ftghpr(iunit,99,simple,bitpix,naxis,naxes,pcount,gcount,
     &        extend,status)
         simple = .true.
         naxis = 0
         pcount = 0
         gcount = 1
         call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &        extend,status)
         call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
      endif

C copyall extensions before this extension, if so requested
      if ((copyall) .and. (extnum .gt. 1)) then
         do 500 i = 1, extnum - 1
            call ftmrhd (iunit, 1, htype, status)
            call ftcrhd (ounit, status)
            call ftcopy (iunit, ounit, 0, status)
 500     continue
         if (status .ne. 0) then
            context = ' error copying extensions'
            call fcerr (context)
            goto 999
         endif
      endif
      call ftmahd (iunit, extnum+1, htype, status)

C  create a new new extension
      call ftcrhd(ounit,status)
      if (htype .eq. 1) then
         call ftphtb(ounit,nrowlen,nnrows,nfields,ntype,nbcol,
     &        nform,nunit,nextname,status)
         call ftadef(ounit,nrowlen,nfields,nbcol,nform,nnrows,
     &        status)
      else
         call ftphbn(ounit,nnrows,nfields,ntype,nform,nunit,
     &        nextname,varidat,status)
         call ftbdef(ounit,nfields,nform,varidat,nnrows,status)
      endif

C copy all keywords from this extension to the output file
      call xcopynoscale (iunit, ounit, status)
      if (dates) then
         call ftmkys (ounit, obsdate, ndate, '&', status)
         if (.not. julian) call ftmkys (ounit, obstime, ntime, '&',
     &        status)
      endif

      status=0
      call ftpdat(ounit,status)
      call ftphis(ounit,history,status)
      history = ' Binning mode used:  ' // mode
      call ftphis (ounit, history, status)
      call timestamp(ounit)

      if (status .ne. 0) goto 999

C allocate the needed dynamic memory
      binll = 0
      binul = 0
      ngti = 0 
      livetime = 0
      bin_value = 0
      bin_error = 0
      bin_center = 0
      bin_num = 0

      nallocated = 0
      call udmget (nbins*maxgti, 7, binll, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: binll')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins*maxgti, 7, binul, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: binul')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 4, ngti, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: ngti')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 7, livetime, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: livetime')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 7, bin_value, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: bin_value')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 7, bin_error, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: bin_error')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 7, bin_center, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: bin_error')
         goto 998
      endif
      nallocated = nallocated + 1

      call udmget (nbins, 4, bin_num, status)
      if (status .ne. 0) then
         call fcerr (' Error allocating dynamic memory: bin_error')
         goto 998
      endif
      nallocated = nallocated + 1

C  get the lower and upper bin limits based on the GTI values
 550  call filubl (gtiunit, gtinum, gnrows, goffs, lowval, highval,
     &     nbins,binsz,memd(binll),memd(binul),memi(ngti),gtiopen)

C do the calculating and writing that only needs to be done once
      call filonc (ounit, 1, nbins, outlive, gtiopen, memi(ngti),
     &     lowval, memd(binll), memd(binul), binsz, nfields,
     &     memd(livetime), memd(bin_center), status)
      if (status .ne. 0) goto 998

      do 575 i = 1, tcols

C  get the light curve bin centers, values, errors and fractional
C  integration times
         call filtcv (iunit, colnum(i), ooffs, nrows, mode, nbins,
     &        lowval, highval, binsz, memd(binll), memd(binul),
     &        memi(ngti), tcolnum, memd(bin_value), memd(bin_error),
     &        gtiopen, trepeat, memd(livetime), vrepeat, memi(bin_num))
C  write the light curve values
         colout = i+1
         if (outerr .ne. 'NONE') colout = i*2
         call fiwlcv(ounit, 1, nbins, outerr, colout,
     &        memd(bin_value), memd(bin_error))
 575  continue

C free dynamic memory
 998  dstatus = 0
      if (nallocated .ge. 1) call udmfre (binll, 7, dstatus)
      if (nallocated .ge. 2) call udmfre (binul, 7, dstatus)
      if (nallocated .ge. 3) call udmfre (ngti, 4, dstatus)
      if (nallocated .ge. 4) call udmfre (livetime, 7, dstatus)
      if (nallocated .ge. 5) call udmfre (bin_value, 7, dstatus)
      if (nallocated .ge. 6) call udmfre (bin_error, 7, dstatus)
      if (nallocated .ge. 7) call udmfre (bin_center, 7, dstatus)
      if (nallocated .ge. 8) call udmfre (bin_num, 4, dstatus)
      if (dstatus .ne. 0) then
         call fcerr (' Error deallocating dynamic memory')
         goto 999
      endif

      if (status .ne. 0) goto 999

C and copyall remaining extensions
      if (copyall) then
 600     call ftmrhd (iunit, 1, htype, status)
         call ftcrhd (ounit, status)
         call ftcopy (iunit, ounit, 0, status)
         if (status .eq. 0) goto 600
         status = 0
      endif

C  close the infile and the outfile
      call ftclos(iunit,status)
      if (gtiopen) call ftclos(gtiunit,status)
      call ftclos(ounit,status)

C  close files and return on error
 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         status = 0
         if (inopen)  call ftclos(iunit,status)
         status = 0
         if (gtiopen)  call ftclos(gtiunit,status)
         status = 0
C  delete output file if already created since was an error somewhere
         if (outopen)  call ftdelt(ounit,status)
      endif

 1000 format(A34,I3)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fimimc
C
C DESCRIPTION:
C      Gets the minimum and maximum column values
C
C AUTHOR/DATE:
C      Janice Tarrant  3/9/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call fimimc(iunit,colnum,nrows,getmin,getmax,ooffs,repeat,min,max)
C
C ARGUMENTS:
C      iunit  - input unit number
C      colnum - column number in input file
C      nrows  - number of rows
C      getmin - flag to get minimum
C      getmax - flag to get maximum
C      ooffs  - observation time offset
C      repeat - repeat count for column < 0 if variable
C      min    - minimum value
C      max    - maximum value
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      felem    - first pixel of element vector
C      frow     - beginning row number
C      remain   - number of rows remaining
C      nelem    - number of data elements
C      status - fitsio error number
C      evalues  - array of real values
C
C CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine ftgcfe - get real column values
C
C******************************************************************************
      subroutine fimimc(iunit,colnum,nrows,getmin,getmax,ooffs,repeat,
     &     mini,maxi)
      logical getmin, getmax
      integer iunit, colnum, nrows, repeat
      double precision ooffs, mini, maxi

      integer maxsize
      parameter (maxsize = 1000)

      character(80) context
      logical anyf, flagvals(maxsize), variable
      integer i, felem, frow, remain, nelem, status, offset
      integer oldrepeat
      double precision evalues(maxsize)

      felem = 1
      frow = 1
      remain = nrows * repeat
      status = 0

C check for variable length array
      variable = .false.
      oldrepeat = repeat
      if (repeat .lt. 0) then
         call ftgdes (iunit, colnum, frow, remain, offset, status)
         repeat = remain
         variable = .true.
      endif

 10   if (remain .ge. maxsize) then
         nelem = maxsize
      else
         nelem = remain
      endif
      anyf = .false.
      call ftgcfd(iunit,colnum,frow,felem,nelem,evalues,flagvals,
     &     anyf,status)
      if (getmin) then
         if (frow .eq. 1) then
            i = 1
 21         if (.not. flagvals(i)) then
               mini = evalues(i)
            else
               i = i + 1
               goto 21
            endif
         endif
         do 20 i = 1, nelem
            if (.not. flagvals(i)) then
               if (evalues(i) .lt. mini)  mini = evalues(i)
            endif
 20      continue
      endif
      if (getmax) then
         if (frow .eq. 1) then
            i = 1
 31         if (.not. flagvals(i)) then
               maxi = evalues(i)
            else
               i = i + 1
               goto 31
            endif
         endif
         do 30 i = 1, nelem
            if (.not. flagvals(i)) then
               if (evalues(i) .gt. maxi)  maxi = evalues(i)
            endif
 30      continue
      endif

C update the pointer and go get the next chunk
      remain = remain - nelem
      felem = felem + nelem
      if (variable) then
         if (felem .gt. repeat) then
            felem = 1
            frow = frow + 1
            if (frow .gt. nrows) goto 900
            call ftgdes (iunit, colnum, frow, remain,
     &           offset, status)
            repeat = remain
         endif
      else
 200     if (felem .gt. repeat) then
            felem = felem - repeat
            frow = frow + 1
            goto 200
         endif
      endif

      if (remain .gt. 0) goto 10

 900  if (status .ne. 0) then
         context = 'error getting min/max values'
         call fcerr(context)
      endif

      if (getmin)  mini = mini + ooffs
      if (getmax)  maxi = maxi + ooffs

      repeat = oldrepeat
      return
      end


C******************************************************************************
C SUBROUTINE:
C      filubl
C
C DESCRIPTION:
C      Gets the lower and upper bin limits based on the Good Time Interval
C      values
C
C AUTHOR/DATE:
C      Janice Tarrant  3/16/92
C
C MODIFICATION HISTORY:
C       4/30/93 (EAG) - made size of chunks taken = maxsize
C                       this routine is too complicated to allow
C                       for vector columns as currently written.
C                       probably should be re-written.
C
C NOTES:
C
C USAGE:
C      call filubl(gtiunit,gtinum,gnrows,goffs,lowval,highval,nbins,
C                  bin_size, binll, binul,ngti,gtiopen)
C
C ARGUMENTS:
C      gtiunit - GTI file input unit number
C      gtinum  - column number in GTI file
C      gnrows  - number of rows
C      goffs   - GTI time offset
C      lowval  - lower limit of histogram
C      highval - upper limit of histogram
C      nbins   - number of bins in histogram
C      binll   - lower limits for each GTI in each bin
C      binul   - upper limits for each GTI in each bin
C      ngti    - number of GTI in each bin
C      gtiopen - whether GTI file was specified
C
C PRIMARY LOCAL VARIABLES:
C      context    - error message
C      gllfound   - found GTI lower limit for bin flag
C      gulfound   - found GTI upper limit for bin flag
C      inbin      - number of bin, value is in
C      felem      - first pixel of element vector
C      frow       - beginning row number
C      remain     - number of rows remaining
C      nelem      - number of data elements
C      status   - fitsio error number
C      gll        - GTI which bin lower limit is in
C      gul        - GTI which bin upper limit is in
C      bin_size   - bin size
C      bll        - bin lower limit
C      bul        - bin upper limit
C      start_time - GTI start times
C      start_time - GTI stop times
C
C CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine ftgcfe - get real column values
C
C******************************************************************************
      subroutine filubl(gtiunit,gtinum,gnrows,goffs,lowval,highval,
     &     nbins,bin_size,binll,binul,ngti,gtiopen)
      integer maxgti, maxsize
      parameter (maxgti = 64)
      parameter (maxsize = 180)

      integer gtiunit, gtinum(2), gnrows, nbins, ngti(nbins)
      double precision goffs, lowval, highval, binll(nbins,maxgti),
     &     binul(nbins,maxgti)

      character(80) context
      logical anyf, gllfound, gulfound, gtiopen
      integer i, inbin, frow, felem, nelem, remain, status, gll,
     &     gul
      double precision bin_size, bll, bul
      double precision start_time(maxsize), stop_time(maxsize)

C  check the GTI values for each bin
      status = 0
      frow = 1
      felem = 1
      remain = gnrows
      gll = 1
      gul = 1
      do 10 inbin = 1, nbins

         bll = lowval + (inbin - 1) * bin_size
         bul = lowval + inbin * bin_size
         if (bul .gt. highval) bul = highval

C if no gti file
         if (.not. gtiopen) then
            ngti(inbin) = 1
            binll(inbin,1) = bll
            binul(inbin,1) = bul
            goto 10
         endif
 500     if (remain .ge. maxsize) then
            nelem = maxsize
         else
            nelem = remain
         endif
         ngti(inbin) = 0
C  get the GTI start and stop times
         call ftgcvd(gtiunit,gtinum(1),frow,felem,nelem,0.D0,
     &        start_time,anyf,status)
         call ftgcvd(gtiunit,gtinum(2),frow,felem,nelem,0.D0,
     &        stop_time,anyf,status)
         do 20 i = 1, nelem
            start_time(i) = start_time(i) + goffs
            stop_time(i) = stop_time(i) + goffs
 20      continue

C  get the bin lower and upper limits and determine if they are within the
C  GTI times

         gllfound = .false.
         gulfound = .false.
         do 30 i = 1, nelem
            if (.not. gllfound) then
               if ((start_time(i) .le. bll) .and.
     &              (stop_time(i) .ge. bll)) then
                  gll = i
                  gllfound = .true.
               endif
            endif
            if (.not. gulfound) then
               if ((start_time(i) .le. bul) .and.
     &              (stop_time(i) .ge. bul)) then
                  gul = i
                  gulfound = .true.
               endif
            endif
 30      continue

C  found GTI lower and upper limits, set the bin limits
         if (gllfound .and. gulfound) then
            if (gll .eq. gul) then
               ngti(inbin) = 1
               binll(inbin,1) = bll
               binul(inbin,1) = bul
            else
               ngti(inbin) = gul - gll + 1
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               binll(inbin,1) = bll
               binul(inbin,1) = stop_time(gll)
               do 40 i = 2, (ngti(inbin)-1)
                  binll(inbin,i) = start_time(gll+i-1)
                  binul(inbin,i) = stop_time(gll+i-1)
 40            continue
               binll(inbin,ngti(inbin)) = start_time(gul)
               binul(inbin,ngti(inbin)) = bul
            endif
C  found GTI lower limit, but not upper limit, set the bin limits
         else if (gllfound .and. (.not. gulfound)) then
            if (bul .lt. stop_time(nelem)) then
               binll(inbin,1) = bll
               binul(inbin,1) = stop_time(gll)
               do 50 i = 1, (nelem-gll)
                  if (.not. gulfound) then
                     if (bul .lt. start_time(gll+i)) then
                        ngti(inbin) = i
                        gulfound = .true.
                     endif
                  endif
 50            continue
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               do 60 i = 1, ngti(inbin)
                  binll(inbin,i+1) = start_time(gll+i)
                  binul(inbin,i+1) = stop_time(gll+i)
 60            continue
            else
C EAG gll -> nelem should fix infinite loop problem 2/19/93
               frow = frow + nelem - 1
               remain = gnrows - frow + 1
               if (remain .gt. nelem)  goto 500
               gul = nelem
               ngti(inbin) = gul - gll + 1
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               binll(inbin,1) = bll
               binul(inbin,1) = stop_time(gll)
               do 70 i = 2, ngti(inbin)
                  binll(inbin,i) = start_time(gll+i-1)
                  binul(inbin,i) = stop_time(gll+i-1)
 70            continue
            endif
C  found GTI upper limit, but not lower limit, set the bin limits
         else if ((.not. gllfound) .and. gulfound) then
            if (bll .gt. start_time(1)) then
               do 80 i = 1, (gul-1)
                  if (.not. gllfound) then
                     if (bll .gt. stop_time(gul-i)) then
                        ngti(inbin) = i
                        gllfound = .true.
                     endif
                  endif
 80            continue
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               do 90 i = 1, (ngti(inbin)-1)
                  binll(inbin,i) = start_time(gul-ngti(inbin)+i)
                  binul(inbin,i) = stop_time(gul-ngti(inbin)+i)
 90            continue
               binll(inbin,ngti(inbin)) = start_time(gul)
               binul(inbin,ngti(inbin)) = bul
            else
               gll = 1
               ngti(inbin) = gul
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               do 100 i = 1, (ngti(inbin)-1)
                  binll(inbin,i) = start_time(i)
                  binul(inbin,i) = stop_time(i)
 100           continue
               binll(inbin,ngti(inbin)) = start_time(ngti(inbin))
               binul(inbin,ngti(inbin)) = bul
            endif
C  found neither GTI lower or upper limit, determine if they are outside the
C  GTI times
         else
            if ((bll .gt. start_time(1)) .and.
     &           (bll .lt. stop_time(nelem))) then
               do 110 i = 1, (nelem-1)
                  if (.not. gllfound) then
                     if ((stop_time(i) .le. bll) .and.
     &                    (start_time(i+1) .ge. bll)) then
                        gll = i
                        gllfound = .true.
                     endif
                  endif
 110           continue
            endif
            if ((bul .gt. start_time(1)) .and.
     &           (bul .lt. stop_time(nelem))) then
               do 120 i = 1, (nelem-1)
                  if (.not. gulfound) then
                     if ((stop_time(i) .le. bul) .and.
     &                    (start_time(i+1) .ge. bul)) then
                        gul = i
                        gulfound = .true.
                     endif
                  endif
 120           continue
            endif
C  found GTI lower and upper limits outside the GTI times, set the bin limits
            if (gllfound .and. gulfound) then
               if (gll .eq. gul) then
                  ngti(inbin) = 0
               else
                  ngti(inbin) = gul - gll
                  if (ngti(inbin) .gt. maxgti) then
                     context = 'number of GTI too large - reduce '
     &                    //  'binning range'
                     call fcerr(context)
                     goto 999
                  endif
                  do 130 i = 1, ngti(inbin)
                     binll(inbin,i) = start_time(gll+i)
                     binul(inbin,i) = stop_time(gll+i)
 130              continue
               endif
C  found GTI lower limit, but not upper limit outside the GTI times, set the
C  bin limits
            else if (gllfound .and. (.not. gulfound)) then
C EAG 2/19/93 changed gll -> nelem
               frow = frow + nelem - 1
               remain = gnrows - frow + 1
               if (remain .gt. nelem)  goto 500
               gul = nelem
               ngti(inbin) = gul - gll
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               do 140 i = 1, ngti(inbin)
                  binll(inbin,i) = start_time(gll+i)
                  binul(inbin,i) = stop_time(gll+i)
 140           continue
C  found GTI upper limit, but not lower limit outside the GTI times, set the
C  bin limits
            else if ((.not. gllfound) .and. (gulfound)) then
               gll = 1
               ngti(inbin) = gul - gll + 1
               if (ngti(inbin) .gt. maxgti) then
                  context = 'number of GTI too large - reduce '
     &                 //  'binning range'
                  call fcerr(context)
                  goto 999
               endif
               do 150 i = 1, ngti(inbin)
                  binll(inbin,i) = start_time(i)
                  binul(inbin,i) = stop_time(i)
 150           continue
C  found neither GTI lower or upper limits outside the GTI times, get the next
C  set of GTI times
            else
               frow = frow + nelem
               remain = remain - nelem
               if (remain .gt. 0)  goto 500
               frow = 1
            endif
         endif
C  start next bin search from where previous bin was found
         frow = frow + gll - 1
         remain = gnrows - frow + 1
 10   continue

 999  continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      fignkc
C
C DESCRIPTION:
C      Gets the keywords for the new FITS file
C
C AUTHOR/DATE:
C      Janice Tarrant  3/13/92
C
C MODIFICATION HISTORY:
C      6/27/95 MJT - now accepts lower case 'none'
C      1/2/1996 JRG - errlist, colist changed to char*40
C
C NOTES:
C
C USAGE:
C       call fignkc(htype, tunit, mode, nbins, timecol, columns,
C                   outtimecol, outcol, outerr, outlive, nrowlen, nnrows,
C                   nfields, ntype, nbcol, nform, nunit)
C
C ARGUMENTS:
C      htype   - FITS header type
C      tunit   - physical unit for input column
C      mode    - specifies binned parameter: sum, rate, mean
C      nbins   - number of bins
C      outtimecol - bin centers column name
C      outcol - parameter values column name
C      outerr - parameter value errors column name
C      outlive - fractional integration time column name
C      nrowlen - row length for new FITS file
C      nnrows  - number of rows for new FITS file
C      nfields - number of fields for new FITS file
C      ntype   - column label for new FITS file
C      nbcol   - first character column number for new FITS file
C      nform   - column format for new FITS file
C      nunit   - column physical unit for new FITS file
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      function   fcafml - return length of ASCII format (integer)
C
C******************************************************************************
      subroutine fignkc(htype, tunit, mode, nbins, timecol, columns,
     &     outtimecol, outcol, outerr, outlive, nrowlen, nnrows,
     &     nfields, ntype, nbcol, nform, nunit, status)
      integer maxcl
      parameter (maxcl = 999)
      character*(*) mode, tunit(maxcl), outtimecol, outcol, outerr,
     &     outlive, ntype(maxcl), nform(maxcl), nunit(maxcl)
      character*(*) columns, timecol
      character(40) errlist(maxcl), colist(maxcl)
      character(80) context

      integer htype, nbins, nrowlen, nnrows, nfields, nbcol(maxcl)
      integer fcafml, fcstln
      integer i, errcols, status, newcols, tcols

      logical negflag

C  get new number of rows
      nnrows = nbins

C get the output time column name
      nfields = 1
      if ((outtimecol .eq. ' ') .or. (outtimecol .eq. '-'))
     &     outtimecol = timecol
      ntype(nfields) = outtimecol

C get whether the output columns match
      if ((outcol .eq. ' ') .or. (outcol .eq. '-'))
     &     outcol = columns
      call fcgcls (columns, colist, tcols, negflag)
      call fcgcls (outcol, colist, newcols, negflag)
      if (newcols .ne. tcols) then
         context = ' Number of output parameters does not match input'
         call fcerr (context)
         status = 1
         goto 999
      endif

C check out the error values
      if ((outerr .eq. ' ') .or. (outerr .eq. '-')) then
         errcols = -tcols
C extended to include lower case (6/27/95 -- MJT)
      else if (outerr .eq. 'NONE' .or. outerr .eq. 'none') then
         errcols = 0
      else
         call fcgcls (outerr, errlist, errcols, negflag)
         if (errcols .ne. tcols) then
            context = ' Number of error columns does not match input'
            call fcerr (context)
            status = 1
            goto 999
         endif
      endif

      do 5 i = 1, newcols
         nfields = nfields + 1
         ntype(nfields) = colist(i)

         if (errcols .lt. 0) then
            nfields = nfields + 1
            ntype(nfields) = colist(i)(1:fcstln(colist(i)))
     &           // '_Err'
         else if (errcols .gt. 0) then
            nfields = nfields + 1
            ntype(nfields) = errlist(i)
         else
            continue
         endif
 5    continue

C  get the livetime column information
      if ((outlive .ne. ' ') .and. (outlive .ne. '-')) then
         nfields = nfields + 1
         ntype(nfields) = outlive
      endif

C  get new column formats
      do 10 i = 1, nfields
         if (htype .eq. 1) then
            nform(i) = 'F23.15'
         else
            nform(i) = 'D'
         endif
 10   continue

C  get new row length and beginning column character
      if (htype .eq. 1) then
         nrowlen = 0
         do 20 i = 1, nfields
            nbcol(i) = 1 + nrowlen
            nrowlen = nrowlen + fcafml(nform(i)) + 1
 20      continue
      endif

C  get new column units
      nunit(1) = 's'

      if (mode .eq. 'Rate') then
         nunit(2) = 'counts/s'
      else if (mode .eq. 'Event_rate') then
         nunit(2) = 'events/s'
      else if (mode .eq. 'Event_sum') then
         nunit(2) = 'events'
      else
         nunit(2) = 'counts'
      endif

      do 30 i = 3, nfields-1
         nunit(i) = nunit(2)
 30   continue
      nunit(nfields) = ' '

 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      filtcv
C
C DESCRIPTION:
C      Makes a light curve of the time and quantity to be binned.  The binned
C      value is sum, rate, or mean depending on the mode.
C
C AUTHOR/DATE:
C      Janice Tarrant  3/15/92
C
C MODIFICATION HISTORY:
C
C     Banashree M Seifert (May 1997)
C         . report was that the last bin was not calculating right
C           so, an "ELSE" atatement was added and now it works fine
C
C NOTES:
C
C USAGE:
C      call filtcv(iunit,colnum,ooffs,nrows,mode,nbins,lowval,highval,
C                  binsz, binll, binul,ngti,bin_value,
C                  bin_error,gtiopen, trepeat, vrepeat)
C
C ARGUMENTS:
C      iunit      - input file unit number
C      colnum     - column number in input file
C      ooffs      - observation time offset
C      nrows      - number of rows
C      mode       - binned parameter mode
C      nbins      - number of bins in histogram
C      lowval     - lower limit of histogram
C      highval    - upper limit of histogram
C      binll      - bin lower limit for each GTI
C      binul      - bin upper limit for each GTI
C      ngti       - number of GTIs
C      bin_value  - bin value
C      bin_error  - bin value error
C      gtiopen    - whether using GTI file
C       trepeat   - repeat count for time column
C       vrepeat   - repeat count for values column
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      felem    - first pixel of element vector
C      frow     - beginning row number
C      remain   - number of rows remaining
C      nelem    - number of data elements
C      status - fitsio error number
C      inbin    - number of bin, value is in
C      bin_num  - number of values in bin
C      bin_size - bin width
C      evalues  - array of parameter values
C      etimes   - array of times
C      offstime - array of times + offset
C      time     - time for each bin based on GTI
C
C CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine ftgcfe - get real column values
C
C******************************************************************************
      subroutine filtcv(iunit,colnum,ooffs,nrows,mode,nbins,lowval,
     &     highval,bin_size,binll,binul,ngti, tcolnum,
     &     bin_value,bin_error,gtiopen, trepeat, time,
     &     vrepeat, bin_num)

      integer maxgti, maxsize
      parameter (maxgti = 64)
      parameter (maxsize = 1)

      character*(*) mode
      integer iunit, colnum, nrows, nbins, ngti(nbins), tcolnum
      double precision ooffs, lowval, highval, binll(nbins,maxgti),
     &     binul(nbins,maxgti), bin_value(nbins),
     &     bin_error(nbins),  bin_size

      character(80) context
      logical anyf, flagvals(maxsize), flagtms(maxsize), ingti
      logical gtiopen
      integer i, j, felem, frow, remain, nelem, status, inbin,
     &     bin_num(nbins), vrepeat, trepeat, nsize, itime
      double precision evalues(maxsize), etimes(maxsize)
      double precision offstime(maxsize),time(nbins)

C  get bin values
      status = 0
      do 20 i = 1, nbins
         bin_value(i) = 0
         bin_error(i) = 0
         bin_num(i) = 0
 20   continue
C determine the repeat count - already tested for good values
      felem = 1
      frow = 1
      remain = nrows * vrepeat
      if (vrepeat .ne. trepeat) then
         if (vrepeat .le. maxsize) then
            nsize = vrepeat
         else
            nsize = maxsize
         endif
      else
         nsize = maxsize
      endif

 60   if (remain .ge. nsize) then
         nelem = nsize
      else
         nelem = remain
      endif
      anyf = .false.
      call ftgcfd(iunit,colnum,frow,felem,nelem,evalues,flagvals,
     &     anyf,status)
      if (trepeat .eq. vrepeat) then
         call ftgcfd(iunit,tcolnum,frow,felem,nelem,etimes,flagtms,
     &        anyf,status)
      else
         call ftgcfd(iunit,tcolnum,frow,felem,1,etimes,flagtms,
     &        anyf,status)
      endif
      do 40 i = 1, nelem
         itime = i
         if (trepeat .ne. vrepeat) itime = 1
         if ((.not. flagvals(i)) .and. (.not. flagtms(itime))) then
            offstime(i) = etimes(itime) + ooffs
            if ((offstime(i) .ge. lowval) .and.
     &           (offstime(i) .le. highval)) then
               if (offstime(i) .eq. highval) then
                  inbin = nbins
               else
                  inbin = (offstime(i) - lowval) / bin_size + 1
               endif
               ingti = .false.
               if (ngti(inbin) .ne. 0) then
                  do 50 j = 1, ngti(inbin)
                     if (.not. ingti) then
                        if ((offstime(i) .ge. binll(inbin,j))
     &                       .and.
     &                       (offstime(i) .le. binul(inbin,j))) then
                           ingti = .true.
                        endif
                     endif
 50               continue
               else if (.not. gtiopen) then
                  ingti = .true.
               endif
               if (ingti) then
                  if (index(mode,'Event') .le. 0) then
C for Sum, Mean or Rate modes, use the value
                     bin_value(inbin) = bin_value(inbin) + evalues(i)
                  else
C for Event_rate, Event_sum modes, just count number of events
                     bin_value(inbin) = bin_value(inbin) + 1
                  endif
                  bin_num(inbin) = bin_num(inbin) + 1
               endif
            endif
         endif
 40   continue

      remain = remain - nelem
      felem = felem + nelem
 55   if (felem .gt. vrepeat) then
         frow = frow + nelem/vrepeat
         felem = felem - vrepeat * (nelem/vrepeat)
         goto 55
      endif
      if (remain .gt. 0) goto 60

C  get the normalized and error values
      if ((mode .eq. 'Rate') .or. (mode .eq. 'Event_rate')) then
         do 70 i = 1, nbins
C the time is fractional time
            if (time(i) .ne. 0.D0) then
               bin_error(i) = sqrt(abs(bin_value(i))) /
     &              (time(i) * bin_size)
               bin_value(i) = bin_value(i) /
     &              (time(i) * bin_size)
ccccccc these two lines with else are new addition by Banashree M Seifert
ccccccc since tha last bin was not doing right without this May, 1997
            else
               bin_value(i) = 0.d0
               bin_error(i) = 0.d0
            endif
 70      continue
      else if ((mode .eq. 'Sum') .or. (mode. eq. 'Event_sum')) then
         do 80 i = 1, nbins
            bin_error(i) = sqrt(abs(bin_value(i)))
 80      continue
      else if (mode .eq. 'Mean') then
         do 90 i = 1, nbins
            if (bin_num(i) .ne. 0) then
               bin_error(i) = sqrt(abs(bin_value(i))) / bin_num(i)
               bin_value(i) = bin_value(i) / bin_num(i)
            endif
 90      continue
      endif


      if (status .ne. 0) then
         context = 'error getting histogram values'
         call fcerr(context)
         call fcerrm(status)
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fiwlcv
C
C DESCRIPTION:
C      Writes the light curve bin centers, values, errors and fractional
C      integration times to a new FITS file
C
C AUTHOR/DATE:
C      Janice Tarrant  3/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call fiwlcv(ounit,nbins,frow, outerr,colnum,
C               bin_value,bin_error)
C
C ARGUMENTS:
C      ounit      - output unit number
C      nbins      - number of bins in histogram
C       frow      - starting row number
C      outerr    - column name for statistical rms error
C      bin_value  - bin value
C      bin_error  - bin value error
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      felem    - first pixel of element vector
C      frow     - beginning row number
C      remain   - number of rows remaining
C      nelem    - number of data elements
C      status - fitsio error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine ftpcle - put real column values
C
C******************************************************************************
      subroutine fiwlcv(ounit,frow,nbins,outerr,colnum,
     &     bin_value,bin_error)

      character*(*) outerr
      integer ounit, nbins, frow, colnum
      double precision bin_value(nbins)
      double precision bin_error(nbins)

      character(80) context
      integer status

      status = 0
      call ftpcld(ounit,colnum,frow,1,nbins,bin_value,status)
      if (outerr .ne. 'NONE')
     &     call ftpcld(ounit,colnum+1,frow,1,nbins,bin_error,status)

      if (status .ne. 0) then
         context = 'error writing light curve values'
         call fcerr(context)
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fcrvmd
C
C DESCRIPTION:
C       This routine checks for valid mode parameter, and allows it
C       to be case insensitive
C
C AUTHOR/DATE:
C
C      Emily A. Greene
C       5/6/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call fcrvmd (mode, status)
C
C ARGUMENTS:
C       mode   - requested mode of operation
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine fcrvmd (mode, status)

      character*(*) mode
      integer     status

      character(80) context

C make the string upper case
      call ftupch (mode)

      if (mode .eq. 'RATE') then
         mode = 'Rate'
      else if (mode .eq. 'MEAN') then
         mode = 'Mean'
      else if (mode .eq. 'SUM') then
         mode = 'Sum'
      else if (mode .eq. 'EVENT_RATE') then
         mode = 'Event_rate'
      else if (mode .eq. 'EVENT_SUM') then
         mode = 'Event_sum'
      else
         context = ' Unknown binmode value:' // mode
         call fcerr (context)
         status = 1
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      filonc
C
C DESCRIPTION:
C       This routine determines the center bin value and
C       live time and writes it to the output file
C
C AUTHOR/DATE:
C
C      Emily A. Greene
C       Hughes STX
C       7/7/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       subroutine filonc (ounit, frow, tbins, outlive, ngti,
C                          lowval, binll, binul, bin_size, nfields, status)
C
C ARGUMENTS:
C       ounit - output unit number
C       frow  - output starting row number
C       tbins - number of time bins
C       ngti       - number of gti in this time bin
C       binll      - bin loweer time limit
C       binul      - bin upper time limit
C       bin_size   - size of the bins
C       nfields    - total number of output fields
C       status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C       livetime    - the value of the live time for this bin
C       bin_center - the time value of the bin center
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine filonc (ounit, frow, nbins, outlive, gtiopen, ngti,
     &     lowval, binll, binul, bin_size, nfields, livetime,
     &     bin_center, status)


      integer maxgti
      parameter (maxgti = 64)

      integer ounit, frow, nbins, ngti(nbins), nfields, status, i, j

      double precision bin_center(nbins), livetime(nbins)
      double precision binll(nbins, maxgti), binul(nbins, maxgti)
      double precision bin_size, lowval

      logical gtiopen

      character*(*) outlive


C  get bin center values
      bin_center(1) = lowval + bin_size / 2
      do 10 i = 2, nbins
         bin_center(i) = bin_center(i-1) + bin_size
 10   continue
C write out the bin center
      call ftpcld(ounit,1,frow,1,nbins,bin_center,status)

C get the livetime information, if requested
      if ((outlive .ne. ' ') .and. (outlive .ne. '-')) then
C  get bin values
         do 20 i = 1, nbins
            livetime(i) = 0
            if (ngti(i) .ne. 0) then
               do 30 j = 1, ngti(i)
                  livetime(i) = livetime(i) + binul(i,j) - binll(i,j)
 30            continue
               livetime(i) = livetime(i) / bin_size
            else if (.not. gtiopen) then
               livetime(i) = 1.
            endif
 20      continue

         call ftpcld(ounit,nfields,frow,1,nbins,livetime,status)
      endif

 999  return
      end
