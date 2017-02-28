C******************************************************************************
C SELECTOR TASK:
C      seextrct
C
C FILE:
C      seextrct.f 
C
C DESCRIPTION: 
C      Makes a "light curve" output file from an SE input file.  For
C      specified columns containing the time and the quantity to be binned,
C      the tool sums over specified time intervals producing the specified
C      light curve
C     
C AUTHOR:  
C      Brian K. Elza 1/94
C
C MODIFICATION HISTORY:
C  26Jan99 (MJT) make checking for "INDEF" more flexible in string-valued
C                parameters by allowing "indef" as well. Mixed-case will
C                still fail and we can't simply convert the parameter to
C                uppercase at read time since they might contain column
C                names or filenames and those comparisons are still 
C                case-sensitive. Also changed a whole slew of calls to
c                fcerr with integer parameter to fcerrm (v4.2b)
c
c  04Jan01 (MJT) Change to gtiminfo() which improves precision of kludge
c                used to convert timemin/max to timeint internally (v4.2c)
c
c  29Jan02 (MJT) Initialize all "pointer" arguments to udmget to avoid
c                unwitting reallocs! (v4.2d)
c
c  01May09 (MJT) Increased default for ichan (lcbinarray parameter)
c  (v4.2e)       to 400000 and added suggestion to increase it on the
c                command line if a particular error condition is hit.
C
C  2014-04-29 (CBM) Increase maximum number of input files in SEEXTRCT
C                from 100 to 999.  This is to match the change in 
C                SAEXTRCT v4.3a about a year ago.  It turns out the
C                increase in size is *required* for SEEXTRCT as well,
C  (v4.3a)       otherwise you get memory weirdness.
C
C NOTES:
C      
C      
C ARGUMENTS:
C      
C
C PRIMARY LOCAL VARIABLES:
C      infile     - input FITS file and extension number
C      gofile     - input GTI file name that is to be OR'ed with infiles
C      gafile     - input GTI file name that is to be AND'ed with infiles
C      outfile    - output fil
C      phasefil   - phase file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
C      timemin    - lower time limit
C      timemax    - upper time limit
C      timeint    - Time intervals to include in summation
C      gticols    - Good Time Interval column start, stop names
C      gtidate    - GTI reference start date keyword
C      gtitime    - GTI reference start time keyword
C      extnam     - Extension name keyword
C      chmin      - Minimum bin to include in the output
C      chmax      - Maximum bin to include in the output
C      binsz      - Input energy binsz in seconds
C      ephem      - Ephemeris or starting time of phase information
C      period     - Periodicity
C      phaseint   - Phase interval(s) to be retained in summation
C      obsdate    - observation start date keyword
C      obstime    - observation start time keyword
C      outimecol  - column name for bin centers      
C      outcol     - column name for binned parameter(s) values
C      outerr     - column name for statistical rms error(s)
C      copyprime  - whether to copy primary array and important keywords
C      copyall    - whether to copy all other extensions to the output file
C      sensecase  - whether values should be case sensitive
C      gettmin    - get the time minumum from the FITS file
C      gettmax    - get the time maximum from the FITS file
C      getcmin    - get the energy bin minumum from the FITS file
C      getcmax    - get the energy bin maximum from the FITS file
C      getephem   - use the ephemeris and periodicity information
c      chkit      - Check the files for errors? (Default is yes)      
C      
C CALLED ROUTINES:
C      subroutine gsecrv - gets parameters from environment
C      subroutine fsecrv - read input FITS file and writes light curve file
C
C***************************************************************************
      subroutine seextt
      implicit none

      integer nb,ngti,isiz
      parameter (nb = 512)
      parameter (isiz=999)
      integer itmjds,itmjde,itims,itime, ichan, ichan_temp,
     &   ngti_temp, itr1, itr2, itr3, itr4, itrs, itre
      
      character(160) infile, outfile, gofile, gafile, phasefil,
     &   bitfil
      character(80) timecol, columns, timeint, gticols, gtidate,
     &   gtitime, extnam, phaseint, mode, lcmode,spmode,
     &   negative,accumulate,extenpha,extenlc,
     &   writesum,writemean,
     &   outimecol, outcol, outerr, obsdate, obstime,
     &   printmode
      character(3000) chint,chbin
      double precision timemin, timemax, ephem, period,
     &   mfracexp,mlcinten,mspinten, binsz
      logical gettmin, gettmax, copyprime, copyall, sensecase,
     &   getcmin, getcmax, getephem, chkit, ldryrun, clobber, abort
        
      integer chmin, chmax, status

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

      character(40) taskname
      common /task/ taskname

      common/input/infile,gofile,gafile,outfile,phasefil,
     &   bitfil,timecol,columns,
     &   lcmode,spmode,
     &   accumulate,negative,extenpha,extenlc,
     &   writesum,writemean,timemin,timemax,timeint,gticols,
     &   gtidate,gtitime,extnam,chmin,chmax,chint,
     &   chbin,binsz,ephem,
     &   period, mfracexp,
     &   mlcinten,mspinten,clobber,ldryrun,
     &   phaseint,copyprime,copyall,sensecase,mode,gettmin,gettmax,
     &   getcmin,getcmax,getephem,chkit,printmode,outcol,outimecol,
     &   obsdate,obstime

      common/gtiinfo/ichan_temp,ngti_temp
      
      taskname = 'SEEXTRCT version 4.3a'
      infile = ' '
      gofile= ' '
      gafile= ' '        
      outfile = ' '
      phasefil = ' '
      bitfil = ' '
      timecol = ' '
      columns = ' '
      lcmode = ' '
      spmode = ' '
      timeint = ' '
      gticols = ' '
      gtidate = ' '
      gtitime = ' '
      extnam = ' '
      binsz = 0.0d0
      phaseint = ' ' 
      obsdate = ' '
      obstime = ' '
      mode = ' '
      outimecol = ' '
      outcol = ' '
      outerr = ' '
      printmode = ' '
      abort=.FALSE.

      call fcecho(' ')
      call fcecho('Running SEEXTRCT version 4.2e')
      call fcecho('==============================================')

C     get the parameters from the par file
      call gsecrv(status)
      
      ichan=ichan_temp
      ngti=ngti_temp

      if (status .ne. 0) goto 999
c        print*,'binsz out of gsecrv is ',binsz
c        print*,'printmode is ',printmode
c        print*,'ngti and ichan are',ichan,ngti

C     Read in the FITS file and write out the light curve file.
C      Read in the FITS file and write out the light curve file.

c     Initialize udmget "pointer" arguments! MJT 29Jan02
      itmjds = 0
      itmjde = 0
      itr1 = 0
      itr2 = 0
      itr3 = 0
      itr4 = 0
      itrs = 0
      itre = 0
      itims = 0
      itime = 0
      
      call udmget(ngti,7,itmjds,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TMJDS')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itmjde,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TMJDE')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif

      call udmget(ngti,7,itr1,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE1')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itr2,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE2')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itr3,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE3')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itr4,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGE4')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itrs,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGEs')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(ngti,7,itre,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMERANGEE')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
      call udmget(isiz,7,itims,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIMS')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif

      call udmget(isiz,7,itime,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TIME')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        return
      endif
      
c      call fsecrv()

      call fsecrv(ichan,ngti,memd(itmjds),memd(itmjde),
     &   memd(itr1),memd(itr2),memd(itr3),memd(itr4),
     &   memd(itrs),memd(itre),
     &   isiz,memd(itims),memd(itime),abort)

      
      call udmfre(itmjds,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TMJDS')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itmjde,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TMJDE')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itr1,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE1')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itr2,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE2')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itr3,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE3')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itr4,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGE4')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itrs,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGES')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itre,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMERANGEE')
        call fcerrm(status)
        status=0
      endif
      
      call udmfre(itims,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIMS')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itime,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TIME')
        call fcerrm(status)
        status=0
      endif
      
999   return
      end

c**********************************************************************
c
c SUBROUTINE:
C      gsecrv
c
c DESCRIPTION:
C      Get parameters from parameter file
C      
c AUTHOR:
C      Brian K. Elza 6/94
c
c MODIFICATION HISTORY:
C      None, yet...
c
c NOTES:
C      gsecrv uses F77/VOS like calls to read parameters from the .par file
C      
c USAGE:
C      call gsecrv(status)
c
c ARGUMENTS:
C      infile     - input FITS file and extension number
C      gofile     - input GTI file name that is to be OR'ed with infiles
C      gafile     - input GTI file name that is to be AND'ed with infiles
C      outfile    - output file name
C      phasefil    - phase file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
c      lcmode     - mode of operation for LIGHTCURVE: SUM, RATE, or MEAN
c      spmode     - mode of operation for SPECTRUM: SUM, RATE, or MEAN
C      timemin    - lower time limit
C      timemax    - upper time limit
C      timeint    - Time intervals to include in summation
C      gticols    - Good Time Interval column start, stop names
C      gtidate    - GTI reference start date keyword
C      gtitime    - GTI reference start time keyword
C      extnam     - Extension name keyword
C      chmin       - Minimum bin to include in the output
C      chmax       - Maximum bin to include in the output
C      binsz    - Input energy binsz in seconds
C      ephem      - Ephemeris or starting time of phase information
C      period     - Periodicity
C      phaseint     - Phase interval(s) to be retained in summation
C      obsdate    - observation start date keyword
C      obstime    - observation start time keyword
C      outimecol  - column name for bin centers      
C      outcol     - column name for binned parameter(s) values
C      outerr     - column name for statistical rms error(s)
C      copyprime  - whether to copy primary array and important keywords
C      copyall    - whether to copy all other extensions to the output file
C      sensecase  - whether values should be case sensitive
C      gettmin    - get the time minumum from the FITS file
C      gettmax    - get the time maximum from the FITS file
C      getcmin    - get the energy bin minumum from the FITS file
C      getcmax    - get the energy bin maximum from the FITS file
C      getephem   - use the ephemeris and periodicity information
c      chkit      - Check the files for errors? (Default is yes)        
c
c PRIMARY LOCAL VARIABLES:
C      context - error messages
C      status - error number
c
c CALLED ROUTINES:
C      subroutine fcerr  - report error to STDERR
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsd - get double precision real parameter
C      subroutine uclgst - get string parameter
C
c**********************************************************************

        subroutine gsecrv(status)

        implicit none

        character(80) context
        character(20) cval
	character(160) infile, outfile, gofile,gafile, phasefil,
     &     bitfil
	character(80) timecol, columns, timeint, gticols, gtidate,
     &     gtitime, extnam, phaseint, mode, lcmode,spmode,
     &     negative,accumulate,extenpha,extenlc,writesum,writemean,
     &     outimecol, outcol, obsdate, obstime,
     &     printmode, ctimezero
        character(3000) chint,chbin
	double precision timemin, timemax, ephem, period,
     &     mfracexp,mlcinten,mspinten, binsz
        real tnull
        logical gettmin, gettmax, copyprime, copyall, sensecase,
     &     getcmin, getcmax, getephem, chkit, ldryrun, clobber,
     &     lmultiple, lbailout

	integer chmin, chmax, status, maxmiss, ichan, ngti
        
        common/input/infile,gofile,gafile,outfile,phasefil,
     &     bitfil,timecol,columns,
     &     lcmode,spmode,
     &     accumulate,negative,extenpha,extenlc,
     &     writesum,writemean,timemin,timemax,timeint,gticols,
     &     gtidate,gtitime,extnam,chmin,chmax,chint,
     &     chbin,binsz,ephem,
     &     period, mfracexp,
     &     mlcinten,mspinten,clobber,ldryrun,
     &     phaseint,copyprime,copyall,sensecase,mode,gettmin,gettmax,
     &     getcmin,getcmax,getephem,chkit,printmode,outcol,outimecol,
     &     obsdate,obstime

        common/multiple/lmultiple
        common/bail/lbailout
        common/nullval/tnull
        common/miss/maxmiss
        common/offset_value/ctimezero
        common/gtiinfo/ichan,ngti
        
C      initialize variables
        status=0
        writesum=' '
        writemean=' '
        lmultiple=.FALSE.
        lbailout=.FALSE.
        
C      get the name of the input FITS file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
           context = 'could not get INFILE parameter'
           call fcerr(context)
           goto 999
	endif

C  get the maximum number of misses to allow before skipping rest
c  of file that is being processed. - MAXMISS
	call uclgsi('maxmiss',maxmiss,status)
        if (status .eq. 3) then
          call uclpst('maxmiss','INDEF',status)
          status = 0
          maxmiss=99999999
	else if (status .ne. 0) then
          context = 'could not get MAXMISS parameter'
          call fcerr(context)
          goto 999
	endif


C  get the maximum number of light-curve bins that are to be used
c  to analyze the input file. - LCBINARRAY
	call uclgsi('lcbinarray',ichan,status)
        if (status .eq. 3) then
          call uclpst('lcbinarray','INDEF',status)
          status = 0
          ichan=400000
	else if (status .ne. 0) then
          call fcerr('could not get LCBINARRAY parameter')
          goto 999
	endif

C  get the maximum number of GTI bins that are to be used
c  to analyze the input file. - GTIARRAY
	call uclgsi('gtiarray',ngti,status)
        if (status .eq. 3) then
          call uclpst('gtiarray','INDEF',status)
          status = 0
          ngti=12800
	else if (status .ne. 0) then
          call fcerr('could not get GTIARRAY parameter')
          goto 999
	endif
        
C  get the name of the input gtifile to be OR'ed with the infiles
	call uclgst('gtiorfile',gofile,status)
	if (status .ne. 0) then
          context = 'could not get GTIORFILE parameter'
          call fcerr(context)
          goto 999
	endif
        if(gofile(1:5).eq.'apply')gofile(1:5)='APPLY'
        if(gofile(1:5).eq.'Apply')gofile(1:5)='APPLY'
        if(gofile.eq.'-')gofile=' '

C  get the name of the input gtifile to be AND ed with the infiles
	call uclgst('gtiandfile',gafile,status)
	if (status .ne. 0) then
          context = 'could not get GTIANDFILE parameter'
          call fcerr(context)
          goto 999
	endif
        if(gafile.eq.'-')gafile=' '

C  get the name of the output file
	call uclgst('outroot',outfile,status)
	if (status .ne. 0) then
           context = 'could not get OUTROOT parameter'
           call fcerr(context)
           goto 999
	endif

C  get the name of the EXTENPHA file
	call uclgst('extenpha',extenpha,status)
	if (status .ne. 0) then
          context = 'could not get EXTENPHA parameter'
          call fcerr(context)
          goto 999
	endif
        if(extenpha.eq.'-'.or.extenpha.eq.' ')extenpha='sp.fits'

C  get the name of the EXTENLC file
	call uclgst('extenlc',extenlc,status)
	if (status .ne. 0) then
          context = 'could not get EXTENLC parameter'
          call fcerr(context)
          goto 999
	endif
        if(extenlc.eq.'-'.or.extenlc.eq.' ')extenlc='lc.fits'
        
C  get the name of the PHASEFILE file
	call uclgst('phasefile',phasefil,status)
	if (status .ne. 0) then
           context = 'could not get PHASEFIL parameter'
           call fcerr(context)
           goto 999
	endif
        if(phasefil.eq.'-')phasefil=' '

C  get the name of the BITFILE file
	call uclgst('bitfile',bitfil,status)
	if (status .ne. 0) then
          context = 'could not get BITFILE parameter'
          call fcecho(context)
          call fcecho('Setting bitfile parameter to a -')
          bitfil='-'
          status = 0
	endif
        if(bitfil.eq.'-')bitfil=' '
        
C  get the time column string
	call uclgst('timecol',timecol,status)
	if (status .ne. 0) then
           context = 'could not get TIMECOL parameter'
           call fcerr(context)
           goto 999
	endif

C  get the columns
	call uclgst('columns',columns,status)
	if (status .ne. 0) then
           context = 'could not get COLUMNS parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get if the bin size in binsz is specifying an even multiple
c     of the smallest time-resolution of the first file. 
	call uclgsb('multiple', lmultiple,status)
	if (status .ne. 0) then
          context = 'could not get MULTIPLE  parameter'
          call fcerr(context)
          goto 999
	endif
        
C  get the size of the bin to be used in creating the light curve.
	call uclgsd('binsz',binsz,status)
        if (status .eq. 3) then
          call uclpst('binsz','INDEF',status)
           status = 0
           binsz=-1.0d0
	else if (status .ne. 0) then
           context = 'could not get BINSZ parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get minimum fractional exposure to be used in creating the light curve.
	call uclgsd('mfracexp',mfracexp,status)
        if (status .eq. 3) then
          call uclpst('mfracexp','INDEF',status)
          status = 0
          mfracexp=0.0d0
	else if (status .ne. 0) then
          context = 'could not get MFRACEXP parameter'
          call fcerr(context)
          goto 999
	endif

C  value to use as the TNULL value
	call uclgsr('tnull',tnull,status)
        if(status.eq.3)then
          call uclpst('tnull','INDEF',status)
          status=0
          tnull=0.0e0
	else if (status .ne. 0) then
          context = 'could not get TNULL parameter'
          call fcecho(context)
          call fcecho('Setting TNULL to 0.0')
          tnull=0.0e0
          status=0
	endif

c  get the TIMEZERO value from the parameter file
	call uclgst('timezero',ctimezero,status)
        call ftupch(ctimezero)
	if (status .ne. 0) then
          context = 'could not get TIMEZERO parameter'
          call fcerr(context)
          goto 999
        endif
        
C  get the printmode
	call uclgst('printmode',printmode,status)
        call ftupch(printmode)
	if (status .ne. 0) then
           context = 'could not get PRINTMODE parameter'
           call fcerr(context)
           goto 999
         endif

         if(printmode.ne.'LIGHTCURVE'.and.printmode(1:1).eq.'L')
     &      printmode='LIGHTCURVE'
         if(printmode.ne.'SPECTRUM'.and.printmode(1:1).eq.'S')
     &      printmode='SPECTRUM'
         if(printmode.ne.'BOTH'.and.printmode(1:1).eq.'B')
     &      printmode='BOTH'
          
         if(printmode.ne.'LIGHTCURVE'.and.
     &      printmode.ne.'SPECTRUM'.and.
     &      printmode.ne.'BOTH')printmode='BOTH'
          
C  get the lcmode
	call uclgst('lcmode',lcmode,status)
	if (status .ne. 0) then
          context = 'could not get LCMODE parameter'
          call fcerr(context)
          goto 999
         endif
                 
         if(lcmode.ne.'EVENT_SUM'.and.
     &      lcmode.ne.'EVENT_RATE'.and.lcmode.ne.'SUM'
     &      .and.lcmode.ne.'RATE'.and.lcmode.ne.'MEAN')then
           context = 'LCMODE must be EVENT_SUM, EVENT_RATE, '
           call fcerr(context)
           context = 'SUM, RATE, or MEAN'
           call fcerr(context)
           status=1
           goto 999
         endif
        
C  get the spmode
	call uclgst('spmode',spmode,status)
	if (status .ne. 0) then
          context = 'could not get SPMODE parameter'
          call fcerr(context)
          goto 999
        endif
        
        if(spmode.ne.'EVENT_SUM'.and.
     &     spmode.ne.'EVENT_RATE'.and.spmode.ne.'SUM'
     &     .and.spmode.ne.'RATE'.and.spmode.ne.'MEAN')then
          context = 'SPMODE must be EVENT_SUM, EVENT_RATE, '
          call fcerr(context)
          context = 'SUM, RATE, or MEAN'
          call fcerr(context)
          status=1
          goto 999
        endif

C  get maximum exposure intensity allowed in creating the light curve.
	call uclgsd('mlcinten',mlcinten,status)
        if (status .eq. 3) then
          call uclpst('mlcinten','INDEF',status)
          status = 0
          mlcinten=1.0d+18
	else if (status .ne. 0) then
          context = 'could not get MLCINTEN parameter'
          call fcerr(context)
          goto 999
	endif

C  get maximum exposure intensity allowed in creating the spectrun.
	call uclgsd('mspinten',mspinten,status)
        if (status .eq. 3) then
          call uclpst('mspinten','INDEF',status)
          status = 0
          mspinten=1.0d+18
	else if (status .ne. 0) then
          context = 'could not get MSPINTEN parameter'
          call fcerr(context)
          goto 999
	endif
        
C      get the minimum time parameter to include
        gettmin=.FALSE.         
	call uclgsd('timemin',timemin,status)
        if (status .eq. 3) then
          call uclpst('timemin','INDEF',status)
           status = 0
           timemin = -1.0d0
           gettmin=.TRUE.
	else if (status .ne. 0) then
           context = 'could not get TIMEMIN parameter'
           call fcerr(context)
           goto 999
	endif

C      get the maximum time parameter to include
        gettmax=.FALSE.
	call uclgsd('timemax',timemax,status)
        if (status .eq. 3) then
          call uclpst('timemax','INDEF',status)
           status = 0
           timemax=-1.0d0
           gettmax=.TRUE.
	else if (status .ne. 0) then
           context = 'could not get TIMEMAX parameter'
           call fcerr(context)
           goto 999
	endif

c        if(timemin.ge.timemax)then
c           context = '*ERROR* TIMEMIN .ge. TIMEMAX'
c           call fcerr(context)
c           goto 999
c        endif
        
C  get the time intervals to 
	call uclgst('timeint',timeint,status)
	if (status .ne. 0) then
           context = 'could not get TIMEINT parameter'
           call fcerr(context)
           goto 999
        endif

        if(timeint.eq.'INDEF'.or.timeint.eq.'-'
     &     .or.timeint.eq.'indef'.or.timeint.eq.' ')timeint='0.0-0.0'
        
C  get the GTI column names, i.e. STOP START etc...
	call uclgst('gticols',gticols,status)
	if (status .ne. 0) then
           context = 'could not get GTICOLS parameter'
           call fcerr(context)
           goto 999 
	endif

C  get the GTI reference date keyword
c	call uclgst('gtidate',gtidate,status)
c	if (status .ne. 0) then
c           context = 'could not get GTICOLS parameter'
c           call fcerr(context)
c           goto 999
c         endif
        gtitime='TSTART'
        gtidate='MJDREF'

C  get the filename extension to put on each file
c	call uclgst('extnam',extnam,status)
c	if (status .ne. 0) then
c           context = 'could not get EXTNAME parameter'
c           call fcerr(context)
c           goto 999
c       endif
        extnam='RATES'
         
C  get the lower bin number - CHMIN
        getcmin = .FALSE.
	call uclgsi('chmin',chmin,status)
        if (status .eq. 3) then
          call uclpst('chmin','INDEF',status)
           status = 0
           chmin=-1
           getcmin = .TRUE.
	else if (status .ne. 0) then
           context = 'could not get CHMIN parameter'
           call fcerr(context)
           goto 999
	endif

C  get the upper bin number - CHMAX
        getcmax = .FALSE.
	call uclgsi('chmax',chmax,status)
        if (status .eq. 3) then
          call uclpst('chmax','INDEF',status)
           status = 0
           chmax=-1
           getcmax = .TRUE.
	else if (status .ne. 0) then
           context = 'could not get CHMAX parameter'
           call fcerr(context)
           goto 999
	endif
         
C  get the energy channels that will be utilized in creating the light-curve
	call uclgst('chint',chint,status)
	if (status .ne. 0) then
           context = 'could not get CHINT parameter'
           call fcerr(context)
           goto 999
	endif
        if(chint.eq.'INDEF'.or.chint.eq.'indef')chint='0-255'
        if(chint.eq.'0-0')chint='0-255'
        if(chint.eq.' ')chint='0-255'
        if(chint.eq.' ')chint='0-255'
         
C  get the energy channels associate with each channel bin that
c      will be utilized in creating the spectrum.
	call uclgst('chbin',chbin,status)
	if (status .ne. 0) then
          context = 'could not get CHBIN parameter'
          call fcerr(context)
          goto 999
	endif

        if(chbin.eq.'INDEF'.or.chbin.eq.'indef')chbin='INDEF'
        if(chbin.eq.'0-0')chbin='INDEF'
        if(chbin.eq.' ')chbin='INDEF'
        if(chbin.eq.'-')chbin='INDEF'

C  get the ephemeris time start - EPHEM
        getephem = .FALSE.
	call uclgsd('ephem',ephem,status)
        if (status .eq. 3) then
          call uclpst('ephem','INDEF',status)
           status = 0
           ephem=-1.0d0
           getephem = .TRUE.
        else if (status .ne. 0) then
           context = 'could not get EPHEM parameter'
           call fcerr(context)
           goto 999
        endif

C      get the periodicity for creating the light curve
	call uclgsd('period',period,status)
        if (status .eq. 3) then
          call uclpst('period','INDEF',status)
           status = 0
           period=-1.0d0
	else if (status .ne. 0) then
           context = 'could not get PERIOD parameter'
           call fcerr(context)
           goto 999
	endif

C  get the input phase intervals to use in creating the light curve 
	call uclgst('phaseint',phaseint,status)
	if (status .ne. 0) then
           context = 'could not get PHASEINT parameter'
           call fcerr(context)
           goto 999
	endif

c        print*,'Read in PHASEINT and it is ',phaseint
c      if(phaseint.ne.' '.and.phaseint(1:1).ne.'[')then
c           context ='PHASEINT parameter not in correct form'
c           call fcerr(context)
c           context ='Please use [1-3,6-8], or [] for nothing'
c        endif
        
        
C  get the observation date - OBSDATE
	call uclgst('obsdate',obsdate,status)
	if (status .ne. 0) then
           context = 'could not get OBSDATE parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get the observation time - OBSTIME
	call uclgst('obstime',obstime,status)
	if (status .ne. 0) then
           context = 'could not get OBSTIME parameter'
           call fcerr(context)
           goto 999
	endif
        
C  get the title for the output bin column
c	call uclgst('outimecol',outimecol,status)
c	if (status .ne. 0) then
c           context = 'could not get OUTIMECOL parameter'
c           call fcerr(context)
c           goto 999
c       endif
        outimecol=' '
        
C  get the title for the output EVENT_SUM column
c	call uclgst('outcol',outcol,status)
c	if (status .ne. 0) then
c           context = 'could not get OUTCOL parameter'
c           call fcerr(context)
c           goto 999
c	endif
        outcol=' '
        
C  get whether to copy primary array and important keywords 
c	call uclgsb('copyprime',copyprime,status)
c	if (status .ne. 0) then
c           context = 'could not get copyprime parameter'
c           call fcerr(context)
c           goto 999
c       endif
        copyprime=.TRUE.

C  get whether to all other extensions to output file
c	call uclgsb('copyall', copyall,status)
c	if (status .ne. 0) then
c           context = 'could not get copyall parameter'
c           call fcerr(context)
c           goto 999
c	endif
        copyall=.FALSE.
        
C     get whether to be case sensitive
        sensecase=.FALSE.
	call uclgsb('sensecase', sensecase,status)
	if (status .ne. 0) then
           context = 'could not get SENSECASE parameter'
           call fcerr(context)
           goto 999
	endif

C  get whether to perform all checks
	call uclgsb('chkit', chkit,status)
	if (status .ne. 0) then
	    context = 'could not get CHKIT parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to CLOBBER files
	call uclgsb('clobber', clobber,status)
	if (status .ne. 0) then
          context = 'could not get CLOBBER parameter'
          call fcerr(context)
          goto 999
	endif
        
C  get the action to be performed with non-NULL negative values
	call uclgst('negative',negative,status)
	if (status .ne. 0) then
          context = 'could not get NEGATIVE parameter'
          call fcerr(context)
          goto 999
	endif

        call ftupch(negative)
        if(negative.ne.'IGNORE'.and.negative.ne.'SUM')then
          context='NEGATIVE defaulting to IGNORE'
          call fcecho(context)
          negative='IGNORE'
        endif

C  get if the users wants to abort on any problem -
c     i.e. make the code dumb... 
	call uclgsb('bailout', lbailout,status)
	if (status .ne. 0) then
          context = 'could not get BAILOUT parameter'
          call fcerr(context)
          goto 999
	endif
        
        accumulate='ONE'
        ldryrun=.TRUE.
         
        if(maxmiss.ne.99999999)then
          call fti2c(maxmiss,cval,status)
          context='The parameter MAXMISS was set to:'
          context(34:44)=cval(10:20)
          call fcecho(' ')
          call fcecho(context)
          call fcecho('This can cause data to be skipped!')
          call fcecho('No checks are done to detect this.')
          call fcecho('No warnings will be given!')
        endif

c     Let's do so sanity checks on the size that was input for
c     array allocation.
        if(ichan.lt.26000)then
          call fcecho(' ')
          call fcecho('You set the array size for light-bins to')
          call fcecho('less than 26000 elements. This is too small.')
          call fcecho('Setting lcbinarray to 100000')
          ichan=100000
        endif

        if(ngti.lt.5000)then
          call fcecho(' ')
          call fcecho('You set the array size for GTIs to')
          call fcecho('less than 5000 elements. This is too small.')
          call fcecho('Setting gtiarray to 12800.')
          ngti=12800
        endif
        
        
c     Since saextrct operates on 40 columns we have to check that the
c     number of specified elements is divisible by 40 exactly. 
        if(mod(ichan,40).ne.0)then
          call fcecho(' ')
          call fcecho('Your input lcbinarray value is being')
          call fcecho('resized for compatibility. The value will be')
          call fcecho('exactly divisible by 40.')
          ichan=(ichan/40)*40
        endif

999	continue

	if(status .ne. 0)then
          call fcerrm(status)
          call fcecho(' ')
          call fcecho('An error occurred in getting the parameters.')
          call fcecho('Cannot continue. Aborting...')
          status=0
          stop
        endif

	return
	end

c**********************************************************************
c
c SUBROUTINE:
C      fsecrv
c
c DESCRIPTION:
C      Parse parameter file information, create light curve, print info.
C      
c AUTHOR:
C      Brian K. Elza 12/93
c
c MODIFICATION HISTORY:
C      None, yet...
c
c NOTES:
C     
C      
c USAGE:
c        call fsecrv()
c
c
c ARGUMENTS:
C      infile     - input FITS file and extension number
C      gtifile    - GTI file name
C      outfile    - output file name
C      phasefil    - phase file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
c      binmode    - mode of operation EVENT_SUM or EVENT_RATE        
C      timemin    - lower time limit
C      timemax    - upper time limit
C      timeint    - Time intervals to include in summation
C      gticols    - Good Time Interval column start, stop names
C      gtidate    - GTI reference start date keyword
C      gtitime    - GTI reference start time keyword
C      extnam     - Extension name keyword
C      chmin       - Minimum bin to include in the output
C      chmax       - Maximum bin to include in the output
C      binsz    - Input energy binsz in seconds
C      ephem      - Ephemeris or starting time of phase information
C      period     - Periodicity
C      phaseint     - Phase interval(s) to be retained in summation
C      obsdate    - observation start date keyword
C      obstime    - observation start time keyword
C      outimecol  - column name for bin centers      
C      outcol     - column name for binned parameter(s) values
C      outerr     - column name for statistical rms error(s)
C      copyprime  - whether to copy primary array and important keywords
C      copyall    - whether to copy all other extensions to the output file
C      sensecase  - whether values should be case sensitive
C      gettmin    - get the time minumum from the FITS file
C      gettmax    - get the time maximum from the FITS file
C      getcmin    - get the energy bin minumum from the FITS file
C      getcmax    - get the energy bin maximum from the FITS file
C      getephem   - use the ephemeris and periodicity information
c      chkit      - Check the files for errors? (Default is yes)
c
c PRIMARY LOCAL VARIABLES:
C      files      - array with the name of all input files to scan.
C      no         - number of files that have been input
c      
c
c CALLED ROUTINES:
C      chkfile     - check input file for multiple files
C      chktime   - check the time intervals and sort them such that
c                   the files are time ordered into ascending order.
c                   Also writes out the Primary date unit to the
c                   outfile using information from the first input
c                   file to define the Keywords
C      gsecrvfil   - reads in the binary files, processes the
c                   information
C
C
C
c**********************************************************************
        subroutine fsecrv(ichan,ngti,tmjds,tmjde,timerange1,
     &   timerange2,timerange3,timerange4,timeranges,timerangee,
     &   iisiz,tims,time,
     &   abort)
        implicit none
        
        integer isiz,nb,nf,ne,ichan,ngti, iisiz
        parameter (isiz = 999)
        parameter (nb = 512)
        parameter (nf = 128)
        parameter (ne = 128)

        character(80) objects(isiz)
        character(80) obstart,obstop

	double precision timemin, timemax, ephem, period,
     &     mfracexp,mlcinten,mspinten,
     &     tmjds(*), tmjde(*), binsz
        logical gettmin, gettmax, copyprime, copyall, sensecase,
     &       getcmin, getcmax, getephem

        character(160) files(isiz),gapplyfiles(isiz)
        integer no,cints(nb),cinte(nb),iphaseno
        logical abort, chkit
        
        integer ichno,ipermno,itimno(2,nb),itimnoall,iphano,j,i
        logical fstchar, ldryrun, clobber, lmultiple, lbailout

	character(160) infile, outfile, gofile,gafile, phasefil,
     &     bitfil
	character(80) timecol, columns, timeint, gticols, gtidate,
     &     gtitime, extnam, phaseint, mode, lcmode,spmode,
     &     negative,accumulate,extenpha,extenlc,writesum,writemean,
     &     outimecol, outcol, obsdate, obstime,
     &     printmode, charall, commentlines(ne), chartemp
        character(3000) chint,chbin
        character(20) cval
	double precision tims(*),time(*),mjdref, dtruc,
     &     timeresolution, holdtime

        double precision timerange1(*),timerange2(*),
     &     timerange3(*),timerange4(*),timeranges(*),
     &     timerangee(*)
        
        real tnull
	integer chmin, chmax, status, imjdref, maxmiss,
     &     nogtiorfiles, noofcomments

        integer bincntpt, binpt, ipernopt,
     &     realpt, iarraypt, igtistrtsavlc, igtistpsavlc,
     &     igtistart, igtistop

        LOGICAL          MEMB(100)
        INTEGER*2        MEMS(100)
        INTEGER*4        MEMI(100)
        INTEGER*4        MEML(100)
        REAL             MEMR(100)
        DOUBLE PRECISION MEMD(100)
        COMPLEX          MEMX(100)
        EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
        COMMON /MEM/ MEMD
        
      common/input/infile,gofile,gafile,outfile,phasefil,
     &   bitfil,timecol,columns,
     &   lcmode,spmode,
     &   accumulate,negative,extenpha,extenlc,
     &   writesum,writemean,timemin,timemax,timeint,gticols,
     &   gtidate,gtitime,extnam,chmin,chmax,chint,
     &   chbin,binsz,ephem,
     &   period, mfracexp,
     &   mlcinten,mspinten,clobber,ldryrun,
     &   phaseint,copyprime,copyall,sensecase,mode,gettmin,gettmax,
     &   getcmin,getcmax,getephem,chkit,printmode,outcol,outimecol,
     &   obsdate,obstime

      common/nullval/tnull

      common/gtiorfiles/gapplyfiles,nogtiorfiles
      common/miss/maxmiss
      common/comments/commentlines,noofcomments
      common/multiple/lmultiple
      common/bail/lbailout
      common/timeres/timeresolution

      cval=' '
      charall=' '
      noofcomments=0
c      print*,'In FESERVC'
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c      We have to allocate dynamic memory which we will use in
c      this program

c      First initialize udmget "pointer" variables! MJT 29Jan2002
      bincntpt = 0
      ipernopt = 0
      binpt = 0
      realpt = 0
      iarraypt = 0
      igtistrtsavlc = 0
      igtistpsavlc = 0
      igtistart = 0
      igtistop = 0

c      Allocate memory for DBINCOUNT
        call udmget (ichan,7,bincntpt,status)
c        print*,'bincntpt is ',bincntpt,status,ichan

        if(status.ne.0)then
          call fcecho('Error allocating memory for DBINCOUNT')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

c      Allocate memory for DPERNOARRAY
        call udmget (ichan,7,ipernopt,status)

        if(status.ne.0)then
          call fcecho('Error allocating memory for DPERNOARRY')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

c      Allocate memory for DBIN
        call udmget(ichan,7,binpt,status)

c        print*,'bincntpt is ',bincntpt,status,ichan
        if(status.ne.0)then
          call fcecho('Error allocating memory for DBIN')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif
        
c      Allocate memory for REALBIN
c        print*,'bincntpt is ',bincntpt,status,ichan
        call udmget(ichan,7,realpt,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for REALBIN')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

c      Allocate memory for IARRAY
        call udmget(1000+ichan,4,iarraypt,status)

        if(status.ne.0)then
          call fcecho('Error allocating memory for IARRAY')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

        call dinitial(ngti,tmjde)
        call dinitial(ngti,tmjds)

        call udmget(ngti,7,igtistrtsavlc,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for DGTISTRTSAVLC')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

        call udmget(ngti,7,igtistpsavlc,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for DGTISTPSAVLC')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

        call udmget(ngti,7,igtistart,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for DGTISTART')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif
        
        call udmget(ngti,7,igtistop,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for DGTISTOP')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        

        
c      Check to see if the first character in the file is a "@" and
c      if so then put that information into the "files" array which
c      has a maximum of 100...

c        print*,'About to go into chkfile...'

        if(chkit)then
          call chkfile(infile,files,chkit,no,status,abort)
        else
          call fcgcls(infile,files,no,abort)
        endif

        if(no.gt.100)then
          call fcecho(' ')
          call fcecho('Too many files being input!')
          call fcecho('You will have to break up and input')
          call fcecho('no more than 100 files at a time.')
          call fcecho('Cannot continue... Aborting...')
          stop
        endif
        
c        print*,'Out of chkfile...',chkit
        
c      When checking the files we see if they all contain scientific
c      event data, which is the only type of FITS file that this TOOL
c      operates on. If any of the files are not the correct type
c      the program will abort.

        if(abort)then
           stop
        endif

c      Set up arrays which contain the MJD's for the timeintervals covered
c      in the input files.

        obstart=' '
        obstop=' '
        
c----------------------------------------------------------------------
c      
      
      j=0
      i=1
      fstchar = .FALSE.
      
550   if(obstime(i:i).eq.' '.and.(.not.fstchar))then
         i=i+1
         goto 550
      else if (obstime(i:i).ne.' ')then
         fstchar=.true.
      endif

551   if(obstime(i:i).ne.' ')then
         j=j+1
         obstart(j:j)=obstime(i:i)
         i=i+1
         goto 551
      endif

552   if(obstime(i:i).eq.' ')then
         i=i+1
         goto 552
      endif

      j=0

553   if(obstime(i:i).ne.' ')then
         j=j+1
         obstop(j:j)=obstime(i:i)
         i=i+1
         goto 553
      endif

c      Okay now we have the TIME stamp information so that we can
c      actually read in the start and stop times from the files that
c      we are processing.
c----------------------------------------------------------------------      
        
c      Set up arrays which contain the MJD's for the timeintervals covered
c      in the input files.

        
c        print*,'About to go into chktime...'
        call chktime(files,objects,obstart,obstop,obsdate,mjdref,
     &       imjdref,tmjds,tmjde,no,status)      

c       Check to see that all files passed BARYTIME check. If any failed
c choke and die... 
        if(status.eq.9999)then
          status=0
          abort=.TRUE.
          stop
        endif

        if(gofile(1:5).eq.'APPLY')then
          if(gofile.eq.'APPLY')then
            call fcecho(' ')
            call fcecho('GTIORFILE set to APPLY - the DEFAULT')
            call fcecho(' ')
            call fcecho('All input files will have their 2nd data')
            call fcecho('extension searched for GTI information.')
            call fcecho('If you do not wish to apply this information')
            call fcecho('you will have to enter "-" for the GTIORFILE.')
          else
            call fcecho(' ')
            call fcecho('GTIORFILE set to APPLYn')
            call fcecho(' ')
            call fcecho('All input files will have their Nth data')
            call fcecho('extension searched for GTI information.')
            call fcecho('If you do not wish to apply this information')
            call fcecho('you will have to enter "-" for the GTIORFILE.')
          endif
          
            nogtiorfiles=no

            do i=1,no
              gapplyfiles(i)=files(i)
            enddo
          
        endif
        
c        print*,'Out of CHKTIME'
        
c      Store the start and stop times associated with each file into
c      an array so that later we can determine the number of bins of
c      size binsz there are in each file. This will also allow us to
c      determine an offset value so that we do not assign array space to
c      account for time intervals which do not exist in any of the input
c      files. These offsets will be stored in the array DOFFSET
        
        do 10 i=1,no
          tims(i)=tmjds(i)
          time(i)=tmjde(i)
c          print*,'Files start and stop from chktime',i,
c     &       tmjds(i),tmjde(i)
10      continue
        
c      print*,'Out of chktime'
        
c        print*,'logicals are',gettmin, gettmax,
c     &       copyprime, copyall, sensecase,
c     &       getcmin, getcmax, getephem, getcmin, getcmax

c      Set timemin and timemax if they haven't been set in the par files.
c      Let's take care of what we can for erroneous input parameters
c      and those with INDEF supplied as the value.
        
c      If timemin is not set or wrong than set it to the earliest time.
c      Note that timemin and timemax are given in MJD
        if(gettmin.or.timemin.lt.0.0d0)then
           timemin=tmjds(1)
        endif
        
c      If timemax is not set or wrong than set it to the latest time.
        if(gettmax.or.timemax.lt.0.0d0.or.
     &       timemax.gt.1.0d+35)then
          timemax=tmjde(no)

        endif

c      If the ephemeris is not set or wrong set it to the earliest time.
        if(getephem.or.ephem.lt.0.0)then
           ephem=timemin
        endif

c        print*,'The binsize is ', binsz
        
c        print*,'timemin and timemax is '
c     &       ,timemin,timemax
c        print*,'totals... ',tmjds(1),tmjde(no),timemin,timemax

c      While TMJD "s" and "e" up to this point define the starting and
c      stopping point of the times included in each file, at this point
c      these arrays take on the GTI's and define the start and stop times
c      in each. For simplicity we will allow them to span all of the files
c      at this point... 

c        print*,'Into GTIMINFO'

        call gtiminfo(gettmin,gettmax,timemin,timemax,timeint,
     &     tmjds,tmjde,itimno,itimnoall,files,no,gofile,
     &     gafile,gticols,
     &     gtidate,gtitime,obstart,obstop,obsdate,ipermno)

c        print*,'Out of gtiminfo and files(1) is ',files(1)
        
        call fcecho(' ')
        call fcecho('All time-filtering criteria has been processed.')
        call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        call fcecho('GTIORFILE filtering time-ranges are:')

        do i=1,no
          call fcecho(' ')
          cval=' '
          charall=' '
          call fti2c(i,cval,status)
          charall(1:17)='For file number: '
          charall(18:38)=cval(1:20)
          call fcecho(charall)

          if(itimno(1,i).le.itimno(2,i))then
            call fcecho('Time-ranges allowed by GTIORFILE are:')
            do j=itimno(1,i),itimno(2,i)
              cval=' '
              charall=' '
              call fti2c(j,cval,status)
              charall(1:9)=cval(12:20)
              cval=' '
              call ftd2f(tmjds(j),8,cval,status)
              charall(11:30)=cval(1:20)
              cval=' '
              call ftd2f(tmjde(j),8,cval,status)
              charall(36:55)=cval(1:20)
              
              call fcecho(charall)
            enddo
          else
            call fcecho('No GTIORFILE information was given.')
          endif
        enddo

        call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

        if(ipermno.ge.1)then
          call fcecho(' ')
          call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
          call fcecho('Merging GTIANDFILEs and user-supplied times')
          call fcecho('(ie, either timeint or timemin/max) yielded')
          call fcecho('the following acceptable time-ranges:')

        
          do j=1,ipermno
            cval=' '
            charall=' '
            call fti2c(j,cval,status)
            charall(1:9)=cval(12:20)
            cval=' '
            call ftd2f(tmjds(itimnoall+j),8,cval,status)
            charall(11:30)=cval(1:20)
            cval=' '
            call ftd2f(tmjde(itimnoall+j),8,cval,status)
            charall(36:55)=cval(1:20)
            call fcecho(charall)
          enddo
          call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

        endif
        
c        print*,'OUT of GTIMINFO'
        
c        print*,'itimno and ipermno is ',itimnoall,ipermno
c        print*,'tmjds(1) and tmjde is ',tmjds(1),tmjde(1)     
c        do i=1,itimnoall+ipermno
c          print*,'TMJDS AND TMJDE IS ',i,tmjds(i),tmjde(i)
c        enddo
        
c        print*,'tmjde is ',(tmjde(i),i=1,itimno)      

c        print*,'period and phaseint are',period,phaseint
        if(((period.gt.0.0d0).or.
     &     (phasefil.ne.' '.and.phasefil.ne.'INDEF'
     &       .and.phasefil.ne.'indef')))then
c          call fcecho('Period value has been set.')
c          call fcecho('Processing information')

c     print*,'bincntpt is ',bincntpt,status,ichan
          call gphaseinfo(timemin,timemax,mjdref,imjdref,
     &       ephem,period,
     &       phaseint,binsz,phasefil,tmjds,tmjde,itimnoall,
     &       ipermno,iphaseno)
c          print*,'bincntpt is ',bincntpt,status,ichan          
        else
          ephem=timemin
          tmjds(itimnoall+ipermno+1)=0.0d0
          tmjde(itimnoall+ipermno+1)=1.0d0
          iphaseno=1
        endif
        
c      If the bin-size is either not set or set equal to zero
c      then set it to the default binsize of timemax-timemin/100.
c      This value may be changed if the bin-size is smaller than
c      the most accurate time resolution for the file being processed... 
        if(lmultiple)then
          if(binsz.le.0.0d0)then
            call fcecho(' ')
            call fcecho('****ERROR****')
            call fcecho('You specified that each light-bin')
            call fcecho('is to be an even multiple of')
            call fcecho('of the minimum time resolution')
            call fcecho('but you did not set BINSZ!!!')
            call fcecho(' ')
            call fcecho('Aborting... READ THE DOCUMENTATION!')
            abort=.true.
            goto 999
          else
            holdtime=binsz
            binsz=dtruc(binsz)
            if(holdtime.ne.binsz)then
              call fcecho(' ')
              call fcecho('You specified that each light-bin')
              call fcecho('is to be an even multiple of BINSZ,')
              call fcecho('But your input BINSZ was NOT an integer!')
              if(.not.lbailout)then
                call fcecho('Truncating BINSZ to integer!')
                call ftd2f(binsz,8,cval,status)
                call fcecho('Setting multiplication factor to:')
                call fcecho(cval)
                binsz=binsz*timeresolution
                call fcecho(' ')
                call ftd2f(binsz,16,cval,status)
                call fcecho('Binsz being set to:')
                call fcecho(cval)                      
              else
                call fcecho('ABORTING!!!')
                abort=.TRUE.
                goto 999
              endif
            else
              binsz=binsz*timeresolution
              call fcecho(' ')
              call ftd2f(binsz,16,cval,status)
              call fcecho('Binsz being set to:')
              call fcecho(cval)              
            endif
          endif
        else
          if(binsz.le.0.0d0)then
            binsz=((timemax-timemin)/100.0d0)

c         If there are large gaps in the data BINSZ may be very large
c so test it to see if it is larger than if you take the total time
c covered by all files divided by 100. If it is then redefine it.
c            if(binsz.gt.(holdtime/100.0d0))binsz=holdtime/100.0d0
            call fcecho(' ')
            call ftd2f(binsz,8,cval,status)
            call fcecho('Binsz was not specified!')
            call fcecho('Setting BINSZ parameter to ')
            call fcecho(cval)
          endif
        endif

c        print*,'The binsize is ', binsz

c        print*,'files(1),outfile,objects,no'
c        print*,files(1),outfile,objects(1),no
c        print*,'timemin and timemax is '
c     &       ,timemin,timemax
c        print*,'totals... ',tmjds(1),tmjde(no),timemin,timemax,binsz

c        print*,'IN GCHNINFO'
        
        call gchninfo(chmin,chmax,chint,cints,cinte,
     &     ichno,getcmin,getcmax,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error in getting CHANNEL information')
          call fcecho('Check your input and see the manual.')
          call fcecho('Cannot continue... Aborting')
          stop
        endif

        noofcomments=noofcomments+1
        chartemp='CHMIN for filtering was '
        cval=' '
        call fti2c(chmin,cval,status)
        chartemp(25:45)=cval(1:20)
        commentlines( noofcomments)=chartemp

        noofcomments=noofcomments+1
        chartemp='CHMAX for filtering was '
        cval=' '
        call fti2c(chmax,cval,status)
        chartemp(25:45)=cval(1:20)
        commentlines( noofcomments)=chartemp
        status=0
        
c        print*,'OUT of GCHNINFO'
c        print*,'ichno is ',ichno
c        print*,'cints is ',(cints(i),i=1,ichno)
c        print*,'cinte is ',(cinte(i),i=1,ichno)      
c        print*,'totals... ',tmjds(1),tmjde(no),timemin,timemax,binsz
c        print*,'timecol and columns is ',timecol, columns

        call gsecrvfil(files,outfile,bitfil,lcmode,spmode,
     &     negative,extenpha,extenlc,writesum,writemean,printmode,
     &     timecol,columns,no,timemin,timemax,itimno,itimnoall,
     &     tmjds,tmjde,ephem,period,mfracexp,tnull,
     &     mlcinten,mspinten,clobber,binsz,ichno,cints,
     &     cinte,chbin,
     &     ipermno,iphano,iphaseno,obstart,obstop,obsdate,
     &     time,tims,1000+ichan,meml(iarraypt),
     &     ichan,memd(bincntpt),memd(ipernopt),memd(binpt),memd(realpt),
     &     ngti,memd(igtistrtsavlc),memd(igtistpsavlc),
     &     memd(igtistart),memd(igtistop),
     &     timerange1,timerange2,timerange3,timerange4,timeranges,
     &     timerangee, 
     &     sensecase,status)        

        status=0

999     continue
        
        call udmfre(bincntpt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for IBINCOUNT')
           call fcerrm(status)
           status=0
        endif

        call udmfre(ipernopt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for DPERNOARRAY')
           call fcerrm(status)
           status=0
        endif

        call udmfre(binpt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for BIN')
           call fcerrm(status)
           status=0
        endif

        call udmfre(realpt,7,status)

        if(status.ne.0)then
           call fcecho('Error freeing memory for REALBIN')
           call fcerrm(status)
           status=0
        endif

        call udmfre(igtistrtsavlc,7,status)
        if(status.ne.0)then
          call fcecho('Error freeing memory for DGTISTRTSAVLC')
          call fcerrm(status)
          status=0
        endif

        call udmfre(igtistpsavlc,7,status)
        if(status.ne.0)then
          call fcecho('Error freeing memory for DGTISTPSAVLC')
          call fcerrm(status)
          status=0
        endif

        call udmfre(igtistart,7,status)
        if(status.ne.0)then
          call fcecho('Error freeing memory for DGTISTART')
          call fcerrm(status)
          status=0
        endif
        
        call udmfre(igtistop,7,status)
        if(status.ne.0)then
          call fcecho('Error freeing memory for DGTISTOP')
          call fcerrm(status)
          status=0
        endif
        
        call udmfre(iarraypt,4,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for IARRAY')
           call fcerrm(status)
           status=0
        endif
        
c        print*,'bincntpt,binpt,realpt,arraypt'
c        print*,bincntpt,binpt,realpt,arraypt
        
c     print*,'columns out is ',columns
        
        return
        end

c****************************************************************************
c
c      gsecrvfil
c
c DESCRIPTION:
c      Gets the information necessary to begin processing files.
c      We will need to acquire all of the information about how the
c      data is stored in the FITS files. This routine does not
c      actually process any of the files, but simply gets all of the
c      information stored in the header which will be used in reading
c      in and processing the information. 

C INPUT PARAMETERS
C      iunit   i  Fortran i/o unit number
c      outfile c  Outfile
c      binmode c  Mode to use in creating the out file
c                 (EVENT_SUM or EVENT_RATE)
c      timecol c  Character string used for TIME header.
c      columns c  Character string used for count header.
c      no      i  Number of input files read from *.TMP file.
c      timemin d  Minimum time of first observation in all files.
c      timemax d  Maximum time of last observation in all files.
c      
C MAJOR LOCAL VARIABLES:
C      ncols   i  number of columns in the table
C      nrows   i  number of rows in the table
C      nfield  i  number of fields in the table
C      ttype   c  name of each field (array)
C      tbcol   i  beginning column of each field (array)
C      tform   c  Fortran-77 format of each field (array)
C      tunit   c  units of each field (array)
C      extnam  c  name of table (optional)
C      status  i  returned error status (0=ok)
c      clbd    s  string giving lower boundary for each column (2D array)
c      cubd    s  string giving upper boundary for each column (2D array)
c      
C OUTPUT PARAMETERS:
c      colpost i  column number which has timecol as the header (array)
c      colpos  i  column number which has columns as the header (array)
c      nfield  i  number of columns in the table (table setup)
c      
c CALLED ROUTINES:
c      fcpars    - parses a string to extract a file name
c      fcerr     - print error message associated with status
c      ftopen    - assign a unit number to a filename and open it
c      ftmahd    - move to absolute header data unit
c      ftghbn    - read required header keywords from a binary table ext.
c      ftgtdm    - parse teh TDIMnnn keywork to get dimensionality of col.
c      ftgcno    - determine col. num. corresponding to column name
c      ftgkys    - get string value associated with keyword from header.
c      ftgclj    - read in an integer array of elements
c      ftclos    - close up the file that was read in.
c      
c**********************************************************************
        
        subroutine gsecrvfil(files,outfile,bitfil,lcmode,spmode,
     &   negative,extenpha,extenlc,writesum,writemean,printmode,
     &   timecol,columns,no,timemin,timemax,itimno,itimnoall,
     &   tmjds,tmjde,ephem,period,mfracexp, tnull,
     &   mlcinten,mspinten,clobber,binsz,ichno,cints,
     &   cinte,chbin,
     &   ipermno,iphano,iphaseno,obstart,obstop,obsdate,
     &   time,tims,jichan,iarray,
     &   jchan,dbincount,dpernoarray,dbin,realbin,
     &   ngti,dgstrtsavlc,dgstpsavlc,dgtistart,dgtistop,
     &   timerange1,timerange2,timerange3,timerange4,timeranges,
     &   timerangee, 
     &   sensecase,status)        
        
        implicit none
        integer nf,nb,ne,ngti,isiz,ichan,jnchan,
     &     isml,ipermno,itcycle,
     &     ikey2,ikey3,ikey4,ikey5,jchan,jichan,icols

c      Define all of the common parameters used in the arrays.
        parameter (isml=9)
        parameter (nb = 512)
        parameter (nf = 512)
        parameter (ne = 1024)
        parameter (isiz = 999)
        parameter (icols = 40)

        parameter (itcycle=10000)
        parameter (ikey2=19)
        parameter (ikey3=3)
        parameter (ikey4=8)
        parameter (ikey5=6)

        integer iunit,nrows,nfield,pcount,status,ifile,itimnoall,
     &     cints(nb),cinte(nb),ichno,itimno(2,*),iphano,
     &     ilbd(ne),iubd(ne)
        character*(*) files(isiz),timecol,columns,lcmode,spmode,
     &     chbin,
     &     negative,extenpha,extenlc,writesum,writemean,
     &     outfile,printmode,obstart,obstop,obsdate,bitfil
        character(1) cnum(10)
        character(8) keyword2(ikey2),keyword3(ikey3),keyword4(ikey4),
     &       keyword
        character(40) ttype(nb),tform(nb),tunit(nb),extnam
        character(80) cols(icols),bitmask(5*isiz)
        character(800) cpixold
        character(160) file1

        integer no,block,
     &     extnum,colpos,
     &     colpost,i,j,xtend,
     &     ounit,colval,igti,ibitmask,
     &     ichbins(2*ne),ichbine(2*ne),ichbinum,imin,imax

        integer itimecycle,icycles,
     &     irowsave

        double precision timerange1(*),timerange2(*),
     &     timerange3(*),timerange4(*),timeranges(*),
     &     timerangee(*)
        
        double precision dtotevents,devents,dtotgood,dgood,dtotbad,
     &     dbitmask,dtotbitmask,
     &     dbad,dttot,dtim,dtotphase,dphase,dtotpha,dpha,dthres,
     &     dlcinten,dspinten
        
        character(20) curvetype,accumulate,cval
        character(40) ctyp(isml),cuni(isml),comm,comn(3*isml),
     &     contxt,cwrite(1)
        character(80) charall
        character(2400) tddes,tddestemp
        real tnull,rstor(1)
        double precision dbincount(*),dbin(*),dchannel(nf)
        double precision keyval(isml),timedel,deltat,
     &     dgtistart(*),dgtistop(*),mfracexp,mlcinten,mspinten,
     &     realbin(*),realchan(nf),
     &     time(isiz),tims(isiz)
        integer iword,iperiod,ltemp,icount,iphaseno,
     &       ikeyval(ikey3)
        integer icol,iarray(*),icompare(5*isiz),
     &     iskipfirst(5*isiz),iopval(5*isiz),iskipend(5*isiz),
     &     iopnum(5*isiz),lcounit,spounit,iwrt,imjdref,inocols,
     &     inobd,icounttotal,icountfix,isave,isav,iprint,
     &     fcstln,outlin,ilin,jlin,klin,outlen
        
        double precision totsecs,totsecp,totsecst,
     &     timemin,timemax,ephem,period,binsz,
     &     tmjds(*),tmjde(*),dremain,djstart,
     &     conversion,mjdref,timelower(itcycle),timeupper(itcycle)
        double precision dlta,dgstrtsavlc(*),dgstpsavlc(*),
     &     small,large
        logical exact,lastf,timec,onedim,lhex,lpca,
     &     equibin,lbins,lwrite(1),lext,lcbin,lspbin,clobber,
     &     ljump,lwritegti,sensecase,abort,lfalse,ldata
        double precision doffset(2,isiz),doffsettotal(itcycle),
     &     dchanlower(itcycle),dchan,dchanall,dtempall,dtemp,
     &     dpernoarray(*),dpernosave,dtruc,dbegin,
     &     dcyclestart(isiz),dcycleend(isiz)

        integer maxmiss
        logical lmultiple, lbailout
        
        common/miss/maxmiss
        common/multiple/lmultiple
        common/bail/lbailout

        common/dimen/ichan,jnchan

c      Set up an array where an integer value is assigned an
c      ascii character value
        data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &       '6','7','8','9'/
        data (keyword2(i),i=1,ikey2)/'ORIGIN','CREATOR','HDUCLASS',
     &       'HDUCLAS1','HDUCLAS2','HDUCLAS3',
     &       'DATE','DATE-OBS','TIME-OBS','DATE-END',
     &       'TIME-END','TIMESYS','TIMEUNIT',
     &       'OBJECT','RADECSYS','OBSERVER','OBSID',
     &       'TELESCOP','INSTRUME'/
        data (keyword3(i),i=1,ikey3)/'EXTVER','EXTLEVEL','APPID'/

        data (keyword4(i),i=1,ikey4)/'TSTART','TSTOP','MJDREF',
     &       'RA_PNT','DEC_PNT','EQUINOX','TIMEDEL','DELTAT'/

c        print*,'timemin and timemax',timemin,timemax
c        print*,'outfile,lcmode,spmode,printmode'
c        print*,outfile,lcmode,spmode,printmode
c        print*,'ephem period are',ephem,period
c        print*,'timecol is ',timecol
c        print*,'columns is ',columns
c        print*,'itimnoall is ',itimnoall
c        print*,'ephem is ',ephem
c        print*,'timemin timemax binsz are',timemin,timemax,binsz

c        print*,'files(1),outfile,no'
c        print*,files(1),outfile,no

c       Since SEEXTRCT can ONLY process one column of data but
c we are using the generic subroutines to write LIGHTCURVES and
c SPECTRA we have to set isave=1 and cols(1) to equal the correct
c column name. We also have to set CPIX to the correct value (0:255).

        ichan=jchan
        jnchan=jichan
        if(sensecase)exact=.TRUE.
        if(.not.sensecase)exact=.FALSE.
        ichbinum=0
        imin=0
        imax=0
        lcounit=0
        spounit=0
        abort=.FALSE.
        dpernosave=0.0d0
        inobd=0
        isave=1
        ibitmask=0
        cols(1)=columns
        cpixold='(0:255)'
        tddestemp=' '
        
        keyword=' '
        accumulate='ONE'
        lwrite(1)=.FALSE.
        lwritegti=.FALSE.
        ldata=.FALSE.        
        rstor(1)=0.0e0
        cwrite(1)=' '
        timec=.FALSE.
        lastf=.FALSE.
        onedim=.FALSE.
        lhex=.FALSE.
        lpca=.FALSE.
        lbins=.FALSE.
        lfalse=.FALSE.

        dbegin=0.0d0
        icount=0
        iperiod=0

        dtotevents=0.0d0
        devents=0.0d0
        dtotbitmask=0.0d0
        dbitmask=0.0d0
        dtotgood=0.0d0
        dgood=0.0d0
        dtotbad=0.0d0
        dbad=0.0d0
        dttot=0.0d0
        dtim=0.0d0
        dtotphase=0.0d0
        dphase=0.0d0
        dtotpha=0.0d0
        dpha=0.0d0
        dthres=0.0d0
        dlcinten=0.0d0
        dspinten=0.0d0

        lspbin=.FALSE.
        igti=0
        lext=.FALSE.
        iwrt=0
        itimecycle=0
        icycles=1
        inocols=1

        dlta=1.0d-8
        dlta=0.0d0
        isav=0
        irowsave=0

        call dinitial(ngti,dgstrtsavlc)
        call dinitial(ngti,dgstpsavlc)
        call dinitial(ngti,dgtistart)
        call dinitial(ngti,dgtistop)

c----------------------------------------------------------------------
        
c      Assign a unit file number to the output file.
        
        call ftgiou(ounit,status)
        if(status.ne.0)then
          call fcecho('Error getting output unit number')
          call fcecho('Setting to logical unit 9')
          status=0
          ounit=9
        endif
        
c      Assign a unit file number used in inputting file.
        call ftgiou(iunit,status)
        if(status.ne.0)then
          call fcecho('Error getting input unit number')
          call fcecho('Setting to logical unit 10')
          status=0
          iunit=10
        endif
        
c**********************************************************************
c      initialize variables:

        do 9 j=1,itcycle
          timelower(j)=0.0d0
          timeupper(j)=0.0d0
9       continue

        do 17 j=1,itcycle
          dchanlower(j)=0.0d0
17      continue

        timelower(1)=timemin
        timeupper(1)=timemax
        
        do 12 j=1,nf
          dchannel(j)=0.0d0
          realchan(j)=0.0d0
12      continue

c----------------------------------------------------------------------
c      If no bins have been specified than we have to assume that
c      we want to accumulate each channel separately with no binning.
c      Since each column may have different channels we have to apply
c      to each a different criteria. We will do that at a later point
c      at this point we have to initialize everything. 
        
        if(chbin.ne.'INDEF')then
          
          outlen=fcstln(chbin)
          
          if(outlen.gt.2950)then
            call fcecho(' ')
            call fcecho('The string input for chbin is too long.')
            call fcecho('This will cause problems. You should either ')
            call fcecho('create a file containing the channel binning ')
            call fcecho('that you are interested in, or use the ')
            call fcecho('SHORT HAND notation as described in the XFF')
            call fcecho('document. See the fhelp saextrct and look at ')
            call fcecho('chbin parameter. ')
            call fcecho('Cannot continue... Aborting...')
            stop
          endif

          lfalse=.FALSE.
          imax=2*ne
          
          call gchninfo(imin,imax,chbin,ichbins,ichbine,
     &       ichbinum,lfalse,lfalse,status)
          
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error parsing CHBIN information')
            call fcecho('Cannot continue. Check input...')
            call fcecho('Aborting...')
            return
          endif

        else
          do 300 j=1,nf
            ichbins(j)=j
            ichbine(j)=j
300       continue
          ichbinum=nf
        endif

c**********************************************************************
c      initialize variables:

         do 115 j=1,isiz
           doffset(1,j)=0.0d0
           doffset(2,j)=0.0d0
          dcyclestart(j)=1.0d0
          dcycleend(j)=1.0d0
115       continue

c        print*,'nf and j and channel(j) are',nf,j,channel(j)
c        print*,'columns is ',columns

        totsecs=0.0d0
        totsecp=0.0d0
        icounttotal=0
        dtemp=1.0d0

c       Read in the bitmask from the bitfile if a 'bitfil' was provided.
        if(bitfil.ne.' '.and.bitfil.ne.'INDEF'.and.
     &       bitfil.ne.'indef')then
          call rstringfile(bitfil,ibitmask,bitmask,abort)
          if(abort)then
            call fcecho(' ')
            call fcecho('Bitfile provided in parameter file failed.')
            call fcecho('Cannot continue using this information.')
            call fcecho('Check your bitfile, or')
            call fcecho('run your data through FSELECT to filter.')
            call fcecho('Continuing without any bitmask filtering.')
            ibitmask=0
            abort=.FALSE.
          else
            if(no.gt.1)then
              call fcecho(' ')
              call fcecho('Number of files input is greater than one.')
              call fcecho('Note that the SAME bitmask will be applied')
              call fcecho('to EACH file!')
            endif
            call fcecho(' ')
            call fcecho('Read in bitmask patterns from bitfile:')
            do i=1,ibitmask
              call fcecho(bitmask(i))
            enddo

            if(ibitmask.le.1)then
              outlin=fcstln(bitmask(1))
              if(outlin.le.0)then
                call fcecho(' ')
                call fcecho('Your bitfile contains no filtering')
                call fcecho('information. Cannot continue.')
                call fcecho('Aborting...')
                abort=.TRUE.
                goto 999
              endif
            endif
            
            
            call bitmasksort(ibitmask,icompare,
     &         iskipfirst,iopval,iskipend,iopnum,
     &         bitmask,columns,abort)

c            do i=1,ibitmask
c              print*,'icompare,iskipfirst,iopval,iskipend,iopnum,',
c     $           icompare(i),iskipfirst(i),iopval(i),iskipend(i),
c     $           iopnum(i)
c            enddo
            
            if(abort)then
              call fcecho(' ')
              call fcecho('An error was detected in your BITFILE')
              call fcecho('This is a hidden parameter that should be')
              call fcecho('set to a blank.')
              
              call fcecho('You can use SEBITMASK, with FSELECT ')
              call fcecho('for pre-filtering of the data. Or run')
              call fcecho('SESELECT or SEFILTER. See HELP.')
              call fcecho('Aborting...')
              goto 999
            endif
            
          endif

        endif
        
1       continue

        djstart=-1.0d0
        djstart=0.0d0
        if(icycles.eq.1)djstart=0.0d0
        ljump=.FALSE.
        
        totsecst=0.0d0
        icountfix=0
        icount=0
        
        do 14 j=1,jchan
          realbin(j)=0.0d0
          dbin(j)=0.0d0
          dbincount(j)=0.0d0
          dpernoarray(j)=0.0d0
14      continue
        
        do 15 j=1,jichan
          iarray(j)=0
15      continue
        
        do 19 i=1,itcycle
          doffsettotal(i)=0.0d0
19      continue

1000    continue
        
        do 10 ifile=1,no

c          print*,'FILE and cycles is', ifile, icycles
c          print*,'files(ifile) is ',files(ifile),no
c          print*,'binsz is ',binsz
c          print*,'ICOUNT at this point is ',icount
c          if(icount.gt.ichan)then
c            print*,'Resetting icount'
c            icount=0
c          endif
          
          do 13 iword=1,isml
            ctyp(iword)='$'
            cuni(iword)='$'
13        continue

          if(i.eq.no)lastf=.TRUE.

c      Lets determine if we have enough array space allocated to
c      process these files and if so set up the number of bins that
c      we have to deal with.
        
        if(ifile.eq.1.and.icycles.eq.1)then
c      When we are processing the first file we do not want any offsets

c          PRINT*,'tims(1) and timemin is',tims(1),timemin
          
          doffset(1,1)=dtruc((tims(1)-timemin)/binsz)
          doffset(1,1)=0.0d0
          
          if(doffset(1,1).lt.0.0d0)then
            doffset(1,1)=dabs(doffset(1,1))
          else
            doffset(1,1)=0
          endif

          dchan=0
          dchanall=0
          dtempall=0
          itimecycle=itimecycle+1
          dchanlower(itimecycle)=0.0d0

          if(dchanlower(itimecycle).lt.0.0d0)then
            dchanlower(itimecycle)=dabs(dchanlower(itimecycle))
          else
            dchanlower(itimecycle)=0.0d0
          endif

          timelower(1)=timemin
c          print*,'timelower(1) is',timelower(1),timemin
          
c      If we are processing more than one input file then we have to
c      worry about how many bins there are which are excluded as not
c      falling in any of the possible times stored in any of the files.
          
          if(no.gt.1)then

c            print*,'tims(j) is ',(tims(j),j=1,no)
c            print*,'time(j) is ',(time(j),j=1,no)
c            print*,'binsz is ',binsz
            
c      Set the initial number of bins that there are in the first file.

            dchan=dtruc(((time(1)-tims(1))/binsz)+1.0d0)
c            print*,'dchan is ',dchan

            if(dmod((time(1)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dchan-1.0d0
            dchan=dtruc(dchan)
            dtemp=dchan
            doffset(2,1)=dchan
            dchanall=dtruc(dchanall)
            dchanall=dtruc(dchanall+dchan)

c            print*,'doffset(2,1) is AA',dchan,dchanall
            
c      Now deal with all of the subsequent files.
            do 1050 i=2,no

              dtempall=dtruc(dtempall+dtemp)
              doffset(1,i)=dtruc(((tims(i)-tims(1))/binsz))
     &           -dtempall
c              print*,'dtempall,dtemp,doffset(1,i)  A',dtempall,dtemp,
c     $           doffset(1,i)
              doffset(1,i)=dtruc(doffset(1,i))
c              print*,'Doffset (1,i) is  B',i,doffset(1,i)
              if(doffset(1,i).lt.0.0d0)doffset(1,i)=0.0d0
c              if(doffset(1,i).lt.0.0d0)
c     &           doffset(1,i)=dtruc((tims(1)-timemin)/binsz)
c              if(doffset(1,1).lt.0.0d0)then
c                doffset(1,1)=dabs(doffset(1,1))
c              else
                doffset(1,1)=0.0d0
c              endif
              
              dtempall=dtruc(dtempall+doffset(1,i))
              dchan=dchan+dtruc((((time(i)-tims(1))/binsz))+1.0d0)
     &           -dtempall
c              print*,'dtempall and dchan C',dtempall,dchan
              dchan=dtruc(dchan)
              dtemp=dtruc((((time(i)-tims(1))/binsz))+1.0d0)-dtempall
c              print*,'dtemp and dchan D',dtemp,dchan
              dtemp=dtruc(dtemp)
c              print*,'dchanm dtemp, and dtempall are E',
c     $           dchan,dtemp,dtempall
              
              if(dremain((time(i)-tims(1)),binsz).
     &           lt.1.0d-9)then
c                print*,'Reducing dchan by 1.0 F'
                dchan=dchan-1.0d0
                dchan=dtruc(dchan)
                dtemp=dtemp-1.0d0
                dtemp=dtruc(dtemp)
              endif

              doffset(2,i)=dtemp
              dchanall=dchanall+dtemp
c              print*,'Doffset(2,i) is G ',i,doffset(2,i),dchanall,dtemp

1050        continue

            if(dremain((time(no)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dchan-1.0d0
          else
            
            dchan=dtruc(((time(1)-tims(1))/binsz)+1.0d0)
            
            if(dremain((time(1)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dchan-1.0d0
            doffset(2,1)=dchan
            dchanall=dchanall+dchan
c            print*,'Doffset (2,1) IS H ',doffset(2,1),dchanall,dchan
          endif

          if(ifile.eq.1.and.icycles.eq.1)then

            charall=' '
            call fcecho(' ')
            call fcecho('File time-ranges are given by:')
            call fcecho('  File Number       Start Time value     Stop T
     &ime Value')
            do iprint=1,no
              cval=' '
              charall=' '
             
              call fti2c(iprint,cval,status)
              charall(1:10)=cval(11:20)
              cval=' '
              call ftd2f(tims(iprint),8,cval,status)
              charall(18:38)=cval
              cval=' '
              call ftd2f(time(iprint),8,cval,status)
              charall(40:58)=cval(2:20)
              call fcecho(charall)
            enddo
            
c            print*,'About to go into timeinterval'
            call timeinterval(jchan,itimecycle,
     &         dchanlower, timelower,
     &         timeupper, dchanall, doffset, inocols, time,
     &         tims, binsz, no)
c            print*,'Timelower(1) is',timelower(1),timemin,tims(1)

            timelower(1)=timemin
            timelower(1)=tims(1)
            timeupper(itimecycle)=timemax
            
          endif

          if(itimecycle.gt.1)then
            call fcecho(' ')
            call fcecho('Performing cyclic analysis of data due to numbe
     &r of light bins.')

            charall=' '
            charall='We will be performing '
            call fti2c(itimecycle,cval,status)
            charall(23:33)=cval(11:20)
            charall(35:69)='cycles to analyze this data.'
            call fcecho(charall)
            call fcecho(' ')
            call fcecho('Each cycle will cover a certain time-range. The
     &se time-ranges are:')
            call fcecho('  Cycle Number       Lower Time value    Upper
     &Time Value')
            do iprint=1,itimecycle
              cval=' '
              charall=' '
              call fti2c(iprint,cval,status)
              charall(1:10)=cval(11:20)
              cval=' '
              call ftd2f(timelower(iprint),8,cval,status)
              charall(18:38)=cval
              cval=' '
              call ftd2f(timeupper(iprint),8,cval,status)
              charall(40:58)=cval(2:20)
              call fcecho(charall)
            enddo
          endif
        
          
        endif

        doffsettotal(icycles)=
     &     dtruc(doffsettotal(icycles)+doffset(1,ifile))

c        print*,'DOFFSETTOTAL IS ',doffsettotal(icycles),
c     &     doffset(1,ifile),icycles,ifile
        
c**********************************************************************

           
c      Set up unit number for reading in values. This really isn't
c      necessary since we successively open and close each file, but 

c           iunit=iunit+1

c        print*,'ifile is ', ifile
c        print*,'Files(ifile) is    ', files(ifile)
c        print*,'extnum and status',extnum, status
c      Parse the character input file name 
           call fcpars(files(ifile),file1,extnum,status)
           call fcecho(' ')
           call fcecho('Processing file')
           call fcecho(file1)

c      Print out any error information about accessing the files
           if(status.ne.0)then
             call fcecho('Could not parse the first file name')
             call fcerrm(status)
             status=0
           endif

c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the "extnum" and if it is less than 2 we
c      force it to 2. 
          if (extnum.lt.1) extnum=1

c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
          call ftopen(iunit,file1,0,block,status)
          if(status.ne.0)then
            call fcecho('Failure to open input file - aborting')
            call fcerrm(status)
            status=0
            call ftclos(iunit,status)
            return
          endif

c      Write out the header information for the output file? This
c      is only applicable if we are into the first iteration of this
c      loop.
           equibin=.TRUE.
           
c      Print out any error information about accessing the files
c      26Jan99 (MJT) changed to fcerrm from fcerr
           if (status.ne.0)then
              call fcerrm(status)
           endif

c      Skip the primary header and go to the second (or extnum)
c      to read all pertinent processing information.
           call ftmahd(iunit,extnum+1,xtend,status)
           if(status.ne.0)then
             call fcecho('Error moving to extnum')
             call fcerrm(status)
             status=0
           endif

c         Let's do a check to verify that that data in this file
c is correct. 
           if(icycles.eq.1)then
             call xteverify(iunit,status)
             if(status.ne.0)then
               call fcecho(' ')
               call fcecho('A problem may exist in this data file.')
               call fcecho('If it is XTE data, run FVERIFY, FSTATISTIC')
               call fcecho('and/or FCHECKSUM and examine the output.')
               status=0
             endif
           endif
           
c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
           call ftghbn(iunit,nb,nrows,nfield,ttype,
     &          tform,tunit,extnam,pcount,status)

           if(.not.sensecase.and.icycles.eq.1)then

c             If the user specifies that the columns names aren't
c case sensitive we have to do some screwy stuff. So we are calling
c FITSIO to find matches to the input columns and then setting the
c input columns to be equal to the actual values in the file.
c Yes, this is very dangerous, and I do this under protest. If your
c codes bomb, then try inputting the column names as they appear
c in the file - this may or may not work since this requirement was
c added after testing and design and was "shoehorned" in, as usual...
             call ftgcno(iunit,exact,cols(1),colpos
     &          ,status)
             if(status.eq.0)then
               cols(1)=ttype(colpos)
               columns=ttype(colpos)
             endif
           endif
           
c      Print out any error information about accessing the files
          if (status.ne.0)then
            call fcecho('Could not get FIELDS information')
            call fcerrm(status)
            return
          endif

c      Store useful information for the processing of data
           
c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls

          call ftgcno(iunit,exact,timecol,colpost,status)
          if(status.ne.0)then
            contxt='Could not find TIME column number - check input'
            call fcecho(contxt)
            call fcecho('aborting... cannot continue')
            call fcerrm(status)
            return
          endif
          
          call ftgcno(iunit,exact,columns,colpos,status)
          if(status.ne.0)then
            call fcecho('Could not find COLUMNS column number')
            call fcecho('Check input data')
            call fcecho('aborting... cannot continue')
            call fcerrm(status)
            return
          endif

c           print*,'colpos,colpost,nfield',colpos,colpost,nfield

c----------------------------------------------------------------------
c      Now we will read the rest of the information
c      that is contained within this extension. Since some of the
c      KEYWORDS searched for may not exist we will have to reset status
c      after each attempt. However if the KEYWORD is found we will print
c      that information out to the output file that we are creating.

c        print*,'doffset(1,ifile is)',doffset(1,ifile)          

        do 1010 i=1,ikey2
          
           call ftgkys(iunit,keyword2(i),comn(i),comm,status)
c           print*,'doffset(1,ifile is)',i,doffset(1,ifile)
           
           if(keyword2(i).eq.'CREATOR')comm=
     &          'SEEXTRCT - Version 4.2e'
c      print*,keyword2(i),comm
           if(comn(i).eq.'HEXTE')lhex=.TRUE.
           if(comn(i).eq.'PCA')lhex=.TRUE.
           if(comn(i).eq.'TRANS2FITS')lpca=.TRUE.
           if(comn(i).eq.'TRANS2FITSX')lhex=.TRUE.
           if(comn(i).eq.'XENON2FITS')lpca=.TRUE.
           if(comn(i).eq.'XENON2FITSX')lhex=.TRUE.

           if(keyword2(i).eq.'TIMEUNIT')then
             if(comn(i).eq.'s')then
               conversion=1.0d0
             elseif(comn(i).eq.'m')then
               conversion=60.0d0
             elseif(comn(i).eq.'h')then
               conversion=360.0d0
             elseif(comn(i).eq.'d')then
               conversion=86400.0d0
             else
               conversion=1.0d0
             endif
           endif
           status=0
c           print*,'doffset(1,ifile is)',k,doffset(1,ifile)           
1010    continue
c        print*,'doffset(1,ifile is)',doffset(1,ifile),ifile
        
c        print*,'conversion is',conversion
        do 1011 i=1,ikey3
           call ftgkyj(iunit,keyword3(i),ikeyval(i),comm,status)
c           print*,keyword3(i),ikeyval
           status=0
1011    continue
c        print*,'doffset(1,ifile is)',doffset(1,ifile),ifile
        
        do 1012 i=1,ikey4
c           print*,'about to do real reads',i,keyword4(i)
           call ftgkyd(iunit,keyword4(i),keyval(i),comm,status)
c      print*,'in key4',keyword4(i),keyval,status
           
           if(keyword4(i).eq.'TSTART'.and.status.ne.0)then
             status=0
             call ftgkyd(iunit,obstart,keyval(i),comm,status)
           endif
           if(keyword4(i).eq.'TSTOP'.and.status.ne.0)then
             status=0
             call ftgkyd(iunit,obstop,keyval(i),comm,status)
           endif
           if(keyword4(i).eq.'MJDREF'.and.status.ne.0)then
             status=0
             call ftgkyd(iunit,obsdate,keyval(i),comm,status)
             if(status.ne.0)then
               status=0
               call ftgkyj(iunit,'MJDREFI',imjdref,comm,status)
               if(status.ne.0)then
                 call fcecho('Could not find reference for MJDREF')
                 call fcecho('Proceeding with MJDREFI set to 0')
                 imjdref=0
                 status=0
               endif
               call ftgkyd(iunit,'MJDREFF',mjdref,comm,status)
               if(status.ne.0)then
                 call fcecho('Could not find reference for MJDREF')
                 call fcecho('Proceeding with MJDREFF set to 0.0d0')
                 mjdref=0.0d0
                 status=0
               endif
             endif
           endif
           
c      print*,'in key4',keyword4(i),keyval,status
           
           if(keyword4(i).eq.'TIMEDEL')timedel=keyval(i)
           if(keyword4(i).eq.'DELTAT')deltat=keyval(i)
           
           status=0
1012    continue

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c       This is a cludge and we will have to fix it later, but it
c should work for all data files for quite a while. 

1015    continue

        if(binsz.lt.timedel)then
          call fcecho(' ')
          call fcecho(' ***** ERROR ***** ')
          call fcecho(' BINSZ was set to be smaller than the')
          call fcecho(' minimum time resolution of this file!')
          call fcecho(' Mimimum time resolution is given by TIMEDEL')
          call fcecho(' Cannot continue processing this file!')
          contxt=' '
          call ftd2f(timedel,6,contxt,status)
          call fcecho(' ')
          call fcecho(' Minimum time resolution for file:')
          call fcecho(file1)
          call fcecho('is:')
          call fcecho(contxt)
          call fcecho(' ')
          call fcecho('If you wish to process this file, BINSZ')
          call fcecho('must be set to be larger than TIMEDEL')
          call fcecho('BINSZ is presently set to:')
          contxt=' '
          call ftd2f(binsz,6,contxt,status)
          call fcecho(contxt)
          goto 10
        endif

c        print*,'doffset(1,ifile is)',doffset(1,ifile),ifile
        
c      Since it is possible for there to be up to 999 columns in a FITS
c      file we have to be able to account for this in the keyword. 
        icol=6   
        if(colpos.lt.10)then
           keyword(icol:icol)=cnum(colpos+1)
        elseif (colpos.lt.100)then
           keyword(icol:icol)=cnum(colpos/10+1)
           keyword(icol+1:icol+1)=cnum(mod(colpos,10)+1)
        elseif (colpos.lt.1000)then
           keyword(icol:icol)=cnum(colpos/100+1)
           keyword(icol+1:icol+1)=cnum((mod(colpos,100)/10)+1)
           keyword(icol+2:icol+2)=cnum(mod(mod(colpos,100),10)+1)
        elseif (colpos.ge.1000)then
           comm='ERROR column number greater than 999'
           call fcecho(comm)
        endif
        
        keyword(1:5)='TEVTB'
        call ftgkys(iunit,keyword,tddes,comm,status)
        call ftupch(tddes)
        
        if(status.ne.0)then
          status=0
          keyword(1:5)='TDDES'
          call ftgkys(iunit,keyword,tddes,comm,status)
          call ftupch(tddes)
        endif

c       This little cludge is necessary because the HEXTE IT added a
c space between the ] and the { in the DDL which XFF does not. So this
c code moves through the TDDES and removes any spaces between those
c two special characters before the rest of the code is executed.
        if(tddes.ne.' ')then
          outlin=fcstln(tddes)
          jlin=0
          klin=0
          do ilin=1,outlin
c            jlin=jlin+1
            klin=klin+1
            if(klin.le.outlin)then
              if(tddes(klin:klin).eq.' ')then

              else
                jlin=jlin+1
                tddestemp(jlin:jlin)=tddes(klin:klin)
              endif
            endif
          enddo

          outlin=fcstln(tddestemp)
          tddes=' '
          tddes=tddestemp
        endif
        
c        print*,'KEYWORD for tddes is',keyword,tddes
c        print*,'keyword is ',keyword,' and val is ',tddes
c        print*,'logicals are hexte',lhex,' PCA ',lpca
c        print*,'period is ',period
        status=0
c        print*,'Going into lhex',lhex,lpca
c        print*,'doffset(1,ifile is)',doffset(1,ifile)
c        print*,'ifile and doffsettotal is ',ifile,
c     &     doffset(1,ifile),doffsettotal(icycles)

c       TESTING, TESTING, TESTING
c       print*,'Testing '
c       print*,'Icycles is and ifile is',icycles,ifile
c       print*,'tims(ifile) and time(ifile) is',ifile,
c     &    tims(ifile),time(ifile)
c       print*,'timelower and timeupper is',icycles,
c     &    timelower(icycles),timeupper(icycles)

       if((tims(ifile).lt.timelower(icycles).and.
     &    time(ifile).lt.timelower(icycles)).or.
     &    (tims(ifile).gt.timeupper(icycles).and.
     &    time(ifile).gt.timeupper(icycles)))then
         
c         print*,'Icycles is and ifile is',icycles,ifile
c         print*,'tims(ifile) and time(ifile) is',ifile,
c     &      tims(ifile),time(ifile)
c         print*,'timelower and timeupper is',icycles,
c     &      timelower(icycles),timeupper(icycles)

c         print*,'ICYCLES is in 51',icycles,itimecycle
c         print*,'Going to 51'
         goto 51

       endif

c       TESTING, TESTING, TESTING       

c       if(icycles.gt.20)goto 1101
       if(lhex)then
c     print*,'About to go into HEXTEDATA'

c     Since we have taken so much care not to lose any bins when changing files, we have to
c     be sure that we are not erroneously assigning the same bin to two files separated by
c     by a time-gap. So we check to see if there is an "offset" between the files. If there
c     is and we are not starting at 0 then we increment the counter by one. 
         if(ifile.gt.1.and.doffset(1,ifile).gt.0.and.
     &      icount.ne.0)icount=icount+1
        
c         if(ifile.eq.1.and.icount.eq.0)icount=1
c         print*,'About to call hextedata and icount is ',icount
c         if(icount.gt.ichan)icount=1
         
         
         call hextedata(iunit,columns,bitmask,ibitmask,
     &      icompare,iskipfirst,iopval,iopnum,iskipend,
     &      colpost,colpos,tddes,cpixold,tform(colpos),
     &      nrows,ttype(colpos),tunit(colpos),
     &      tmjds,tmjde,iphano,itimnoall,itimno,ipermno,
     &      iphaseno,binsz,timemin,
     &      chbin,ichbins,ichbine,ichbinum,
     &      ichno,cints,cinte,ifile,dbincount,ephem,
     &      period,realbin,dbin,iarray,dpernoarray,
     &      icount,conversion,inobd,ilbd,iubd,
     &      timelower,timeupper,dchanlower,tims,time,no,
     &      icycles,itimecycle,timedel,
     &      igti,dgtistart,dgtistop,dchannel,realchan,small,large,
     &      devents,dgood,dtim,dphase,dpha,dbitmask,doffsettotal,ljump,
     &      djstart,dcyclestart,dcycleend,ngti,
     &      timerange1,timerange2,timerange3,timerange4,timeranges,
     &      timerangee) 

c1101     continue
         
c         print*,'Out of hextedata and icount is',icount

         dtotbitmask=dtotbitmask+dbitmask
         dtotevents=dtotevents+devents
         dtotgood=dtotgood+dgood
         dttot=dttot+dtim
         dtotphase=dtotphase+dphase
         dtotpha=dtotpha+dpha

c          print*,'ifile and doffsettotal is ',ifile,
c     &     doffset(1,ifile),doffsettotal(icycles)

c         print*,' '
c         do i=1,igti
c           print*,'dgtistart and dgtistop is',
c     &        i,dgtistart(i),dgtistop(i)
c         enddo
c         print*,' '
         
c      Since for the HEXTE instrument there is a possibility of 256
c      data channels. Colval is set equal to this value.
c         print*,'OUT of HEXTEDATA
         if(chbin.eq.'INDEF')then
           colval=inobd
         elseif(chbin.ne.'INDEF')then
           colval=ichbinum
         endif

c         if(icycles.eq.itimecycle)icount=icount-1
c         if(icycles.eq.itimecycle)icount=icount-1         
         iperiod=icount
         icountfix=icount
         icounttotal=icounttotal+icountfix
         
c         print*,'ICOUNTFIX is ',icountfix,icycles,
c     &      itimecycle,icounttotal
           
       elseif(lpca)then    

         call fcecho(' ')
         call fcecho('***** ERROR *****')
         call fcecho(' DATA is not in SE format ')
         call fcecho(' Check TFORM keyword for nnX format')
         call fcecho(' If this file was created using TRANS2FITS')
         call fcecho(' or XENON2FITS please recreate this file')
         call fcecho(' using nnX format')
         call fcecho(' ***** Aborting ***** ')
 
       endif

51     continue        

c      We have finished reading in and processing all of the
c      information that is in one input file. We can close it and
c      then move on with the processing.
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        
c      Close up each file after extracting the useful information.
          call ftclos(iunit,status)
          if(status.ne.0)then
            call fcecho('Error closing input file')
            call fcerrm(status)
            status=0
          endif

          call wrtupdate2scr(columns,devents,dgood,dtim,
     &       dphase,dpha,dbitmask,inocols)


c          print*,'PRINTING UPDATE',ifile
          
10      continue

c        print*,'OUT of FILE loop and ifile is',ifile,no

c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        
c&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c            print*,'iperiod ',iperiod,' colval ',colval

c            print*,'bin(i),i,channel(j),j'

c        print*,'iperiod is ',iperiod,icount
        
        do 60 ltemp=1,iperiod

          totsecs=totsecs+realbin(ltemp)
c          print*,'REALBIN is',ltemp,realbin(ltemp),totsecs
        
          if(realbin(ltemp).ne.0.0d0.and.
     &       (realbin(ltemp)/binsz).lt.mfracexp.and.
     &       (lcmode.eq.'RATE'.or.lcmode.eq.'EVENT_RATE'))then
            dthres=dthres+dbin(ltemp)
            dbin(ltemp)=dble(tnull)
          endif

          if(realbin(ltemp).ne.0.0d0.and.
     &       (lcmode.eq.'RATE'.or.lcmode.eq.'EVENT_RATE'))then
            if((dbin(ltemp)/realbin(ltemp)).gt.mlcinten)then
              dlcinten=dlcinten+dbin(ltemp)
              dbin(ltemp)=dble(tnull)
            endif

          elseif((lcmode.eq.'SUM'.or.lcmode.eq.'EVENT_SUM').and.
     &         realbin(ltemp).ne.0.0d0.and.
     &         dbin(ltemp).gt.mlcinten)then
            dlcinten=dlcinten+dbin(ltemp)
            dbin(ltemp)=dble(tnull)
            
          elseif((lcmode.eq.'MEAN').and.
     &         dbincount(ltemp).gt.0)then
            if(((dbin(ltemp)/
     &         (dbincount(ltemp))).gt.mlcinten))then
              dlcinten=dlcinten+dbin(ltemp)
              dbin(ltemp)=dble(tnull)
            endif

          endif
                  
60      continue
1115    continue

c      Print out any error information about accessing the files
c      26Jan99 (MJT) changed fcerr --> fcerrm
        if (status.ne.0)then
           call fcerrm(status)
        endif
        
c      Now that we have calculated the light curve we have to write
c      it out in the proper format. So do it with the following.

        if(icount.eq.0)then
          call fcecho(' ')
          call fcecho('Data selection resulted in no counts')
          call fcecho('on this iteration. Continuing...')
        else
          call fcecho(' ')
          call fcecho(' TOTALS FOR ALL FILES ARE AS FOLLOWS ')
          call fcecho(
     &         '==================================================')
          call wrtupdate2scr(columns,dtotevents,dtotgood,dttot,
     &       dtotphase,dtotpha,dtotbitmask,1)
          if(icount.gt.0)ldata=.TRUE.
        endif
      
      if(printmode.eq.'BOTH'.or.printmode.eq.'LIGHTCURVE')then

       if(icount.ne.0)then

        curvetype='Light Curve:'

        call wrtlast2scr(columns,dtotevents,dtotgood,dttot,dtotphase,
     &   dtotpha,1,accumulate,dthres,dlcinten,
     &     curvetype,totsecs)

        call fcecho(' ')
        call fcecho('Writing out Light Curve FITS file')

c      print*,'iarray contains',(i,iarray(i),i=1,kchanall)

c        print*,'About to go into wrtlcfile'
c        print*,'icycles and icountfix is ',icycles,icountfix
        
c        do i=1,icountfix
c          print*,i,realbin(i),dbincount(i)
c        enddo

        call wrtlcfile(files,outfile,lcounit,icountfix,
     &     no,lcmode,extenlc,tims(1),
     &     timemin,timemax,totsecs,binsz,
     &     1,0,lcbin,tnull,isave,cols,
     &     dbin,realbin,dbincount,dpernoarray,
     &     inobd,ilbd(1),iubd(inobd),cpixold,chbin,
     &     icounttotal,clobber,icycles,dbegin,irowsave,
     &     abort)
        
        call fcecho('Wrote Light Curve')
        call fcecho(' ')

c        print*,'irowsave,icycles,totsecs,totsecst',
c     &     irowsave,dbegin,
c     &     icycles,totsecs,totsecst

       endif

c        print*,'irowsave,icycles,totsecs,totsecst',
c     &     irowsave,dbegin,
c     &     icycles,totsecs,totsecst
        
        if(icycles.eq.itimecycle)then
          lwritegti=.TRUE.

          if(.not.ldata)then
            call fcecho(' ')
            call fcecho('Filtering resulted in having no data!')
            call fcecho('Cannot create light-curve, or spectrum!')
            call fcecho('Check your input and data!')
            call fcecho('Aborting...')
            abort=.TRUE.
            goto 999
          endif
          
          call fcecho(' ')
          call fcecho('Writing out GTI for Light Curve FITS file')
        endif
        
        call wrtlcgti(lcounit,realbin,
     &     timemin,timemax,tims(1),
     &     binsz,totsecs,doffset,icountfix,dpernoarray,
     &     icycles,
     &     lwritegti,dgstrtsavlc,dgstpsavlc,isav,
     &     dpernosave,abort)

c        print*,'Out of LC gti'
c        do i=1,isav
c          print*,'Start and stop are',dgstrtsavlc(i),dgstpsavlc(i)
c        enddo
                
        if(lwritegti)then
          call fcecho(' ')
          call fcecho('Wrote out GTI extension.')
          call fcecho('Closing Light Curve.')
        endif
        
c        print*,'Ipernosave is',ipernosave,tims(1)

c        print*,'icycles and itimecycle are',icycles,itimecycle

      endif

c**********************************************************************
c     This is added to get the GTI for the spectrum correct
c     if only SPECTRUM was selected.

      if(printmode.eq.'SPECTRUM')then
        call wrtlcgti(lcounit,realbin,
     &     timemin,timemax,tims(1),
     &     binsz,totsecs,doffset,icountfix,dpernoarray,
     &     icycles,
     &     lwritegti,dgstrtsavlc,dgstpsavlc,isav,
     &     dpernosave,abort)
c        print*,'DPERNOSAVE is ',dpernosave

      endif

c**********************************************************************     
        
      if(icycles.lt.itimecycle)then
c        print*,' '
c        print*,' '
c        print*,'****************************************'
c        print*,'INCREMENTING ICYCLES from ',icycles,itimecycle
        icycles=icycles+1
c        print*,'ICYCLES after addition is ',icycles,itimecycle
c        print*,'GOING BACK to FIRST LOOP'
c        print*,'****************************************'

        goto 1
      endif

c      print*,'CLOSING LIGHT CURVE'
c      call ftclos(lcounit,status)
c      print*,'CLOSED LIGHT CURVE',status

      
      if(printmode.eq.'BOTH'.or.printmode.eq.'SPECTRUM')then

        do 160 ltemp=1,colval

          realchan(ltemp)=(dchannel(ltemp))
          
          if((spmode.eq.'RATE'.or.spmode.eq.'EVENT_RATE').and.
     $       (totsecs.gt.0.0d0))then
            if((dchannel(ltemp)/totsecs).gt.mspinten)then
              dspinten=dspinten+1.0d0
              lspbin=.TRUE.
            endif
            
          elseif((spmode.eq.'SUM'.or.spmode.eq.'EVENT_SUM').and.
     &         dchannel(ltemp).gt.mspinten)then
            dspinten=dspinten+1.0d0
            lspbin=.TRUE.

          elseif(spmode.eq.'MEAN'.and.
     $         realchan(ltemp).gt.0.0d0)then
            if((dchannel(ltemp)/(realchan(ltemp)))
     &         .gt.mspinten)then
              dspinten=dspinten+1.0d0
              lspbin=.TRUE.
            endif
            
          endif

160     continue
        
        curvetype='Spectrum:'
        call wrtlast2scr(columns,dtotevents,dtotgood,dttot,dtotphase,
     &   dtotpha,1,accumulate,dthres,dspinten,
     &     curvetype,totsecs)

        call fcecho(' ')
        call fcecho('Writing out Spectrum FITS file')
        
c           print*,'spectrum is ',colval,(realchan(i),i=1,colval)
           
c        print*,'colval,no,lcmode,spmode,timemin,timemax
c     &     ,totsecs,extnam'
c      print*,colval,no,lcmode,spmode,timemin,timemax,totsecs,extnam

c     print*,'about to call spectrum write',totsecs
        
        call wrtphafile(files,outfile,spounit,colval,
     &       no,spmode,extenpha,lspbin,tnull,
     &       timemin,timemax,binsz,totsecs,dchannel,realchan,
     &       mspinten,0,isave,cols,chbin,cpixold,
     &       rstor,iwrt,cwrite,lwrite,clobber,
     &       lext)

        call fcecho(' ')
        call fcecho('Writing out GTI for Spectrum FITS file')

        do i=1,isav
          dgtistart(i)=dgstrtsavlc(i)
c     &       -(0.5D0*binsz)
          dgtistop(i)=dgstpsavlc(i)
c     &       -(0.5D0*binsz)
          if(i.eq.isav.and.printmode.eq.'SPECTRUM')
     &       dgtistop(i)=tims(1)+((dpernosave)*binsz)
c     &       dgtistop(i)=tims(1)+((dpernosave+1.0d0)*binsz)
c          print*,'dgtistart, dgtistop ',i,dgtistart(i),dgtistop(i)
          igti=isav
        enddo
         
        call wrtphagti(spounit,igti,dgtistart,dgtistop,
     &     timemin,timemax,totsecs)

c        print*,'Out of pha gti'
c        do i=1,igti
c          print*,'Start and stop are',dgtistart(i),dgtistop(i)
c        enddo
        
        call fcecho('Wrote Spectrum FITS file.')
        call fcecho('Closing Spectrum.')
        call fcecho(' ')
      endif

      call fcecho(' ')
      call fcecho('Everything is finished.')
      call fcecho('Cleaning up memory and exiting.')

999   continue
      
      return
      end


c**********************************************************************
      subroutine hextedata(iunit,columns,bitmask,ibitmask,
     &   icompare,iskipfirst,iopval,iopnum,iskipend,
     &   colpost,colpos,tddes,cpixold,tform,
     &   nrows,ttype,tunit,
     &   tmjds,tmjde,iphano,itimnoall,itimno,ipermno,
     &   iphaseno,binsz,timemin,
     &   chbin,ichbins,ichbine,ichbinum,
     &   ichno,cints,cinte,ifile,dbincount,ephem,
     &   period,realbin,dbin,iarray,dpernoarray,
     &   icount,conversion,inobd,ilbd,iubd,
     &   timelower,timeupper,dchanlower,tims,time,no,
     &   icycles,itimecycle,timedel,
     &   igti,dgtistart,dgtistop,dchannel,realchan,small,large,
     &   devents,dgood,dtim,dphase,dpha,dbitmask,doffsettotal,ljump,
     &   djstart,dcyclestart,dcycleend,ngti,
     &   timerange1,timerange2,timerange3,timerange4,timeranges,
     &   timerangee) 

        implicit none
        integer nf,nb,ne,ngti,isiz,ichan,jichan,isml,ifile,
     &       ikey2,ikey3,ikey4,ikey5
      
c      Define all of the common parameters used in the arrays.
        parameter (isml=9)
        parameter (nb = 512)
c        parameter (ngti = 12800)
c        parameter (ngti = 12800)
        parameter (nf = 512)
        parameter (ne = 1024)
        parameter (isiz = 999)
        
        parameter (ikey2=19)
        parameter (ikey3=4)
        parameter (ikey4=6)
        parameter (ikey5=6)
        
        logical array(ne)
        integer iunit,nrows,status,itimno(2,*),
     &     ichno,cints(nb),cinte(nb),icount,icycles,icountentry,
     &     itimecycle,ichbins(*),ibitmask,
     &     ichbine(*),ichbinum,inobd,ilbd(*),iubd(*),no,
     &     icompare(*),iskipfirst(*),iopval(*),iskipend(*),
     &     iopnum(*)

        character(1) cnum(10)
        character(8) keyword2(ikey2),keyword3(ikey3),keyword4(ikey4)
        character*(*) ttype,tform,tunit(nb),columns,chbin,
     &     bitmask(5*isiz)
        
        integer colpos,lrow,
     &     colpost,i,k

        character*(*) tddes,cpixold
        character(40) comm
        character(80) contxt
        character(20) cval
        character(80) charall
        double precision realbin(*),realchan(*),
     &     dgtistart(*),dgtistop(*),timedel,djstart,
     &     tims(*),time(*),timedeladd,
     &     dbitmask,dcyclestart(*),dcycleend(*)
        double precision dchannel(nf),dbin(*),
     &     dbincount(*)
        integer nulval,icyclestart,
     &     nultyp,iperiod,iarray(*)

        integer rcount,width,dtype,iskip
        integer fbit,incr,itotal,isize,jsize,itimnoall,colval,
     &     iperstart,ipermno,iphano,igti,ilbitmask,ibitval
        integer itimezero,maxmiss,imiss,
     &     iphaseno,iper,integermax,
     &     jskip,inum,jnum,ij

        double precision dtotevents,devents,dtotgood,dgood,
     &     dttot,dtim,dtotphase,dphase,dtotpha,dpha

        double precision timeval,pstart,pstop,
     &     nulvald,timemin,binsz,small,large,
     &     tmjds(*),tmjde(*),ephem,period,
     &     conversion,timeupper(*),timelower(*),dlta,
     &     dleft,dstart,drangesize,
     &     timezerof,timezero,doffsettotal(*),dchanlower(*),
     &     dpernoarray(*),dpernoval,dper,dtruc,timedelt

        logical anynul,flgval,foundm,mval,lor,land,cand,
     &     pand,lfile,ljump,lall,lbitmask,ltimebin

        double precision timerange1(*),timerange2(*),
     &     timerange3(*),timerange4(*),timeranges(*),
     &     timerangee(*),dbottom,dtop,dhold1,dhold2
        
        integer it1,it2,it3,itot,numranges,iranges,j,
     &     ibottom, itop
        

        common/miss/maxmiss
        common/dimen/ichan,jichan
        
c      Set up an array where an integer value is assigned an
c      ascii character value
        data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &     '6','7','8','9'/
        data (keyword2(i),i=1,ikey2)/'ORIGIN','CREATOR','HDUCLASS',
     &     'HDUCLAS1','HDUCLAS2','HDUCLAS3',
     &     'DATE','DATE-OBS','TIME-OBS','DATE-END',
     &     'TIME-END','TIMESYS','TIMEUNIT',
     &     'OBJECT','RADECSYS','OBSERVER','OBSID',
     &     'TELESCOP','INSTRUME'/
        data (keyword3(i),i=1,ikey3)/'EXTVER','EXTLEVEL','APPID',
     &     'TIMEDEL'/
        data (keyword4(i),i=1,ikey4)/'TSTART','TSTOP','MJDREF',
     &     'RA_PNT','DEC_PNT','EQUINOX'/

        icountentry=icount
        status=0
        lbitmask=.TRUE.
        ltimebin=.FALSE.
        ilbitmask=0
        ibitval=0
        land=.FALSE.
        lor=.FALSE.
        cand=.FALSE.
        lfile=.FALSE.
        lall=.FALSE.
        dleft=0.0d0
        dstart=0.0d0
        timedelt=0.0d0
        ibottom=0
        itop=0
        dbottom=0.0d0
        dtop=0.0d0

        devents=0.0d0
        dbitmask=0.0d0
        dpha=0.0d0
        dphase=0.0d0
        dgood=0.0d0
        dtim=0.0d0
        dhold2=0.0d0

        iskip=0
        timedeladd=0.0d0
        integermax=2000000000
        drangesize=0.0d0
        itimezero=0
        timezero=0.0d0
        timezerof=0.0d0
        jskip=0
        iranges=0

        call dinitial(ngti,timerange1)
        call dinitial(ngti,timerange2)                
        call dinitial(ngti,timerange3)
        call dinitial(ngti,timerange4)        
        call dinitial(ngti,timeranges)        
        call dinitial(ngti,timerangee)        
        
        dlta=0.0d0

c        print*,' '
c        print*,'itimno(1,ifile),itimno(2,ifile),ifile,doffset is',
c     &     itimno(1,ifile),itimno(2,ifile),ifile,
c     &     doffsettotal(icycles),icycles
        
        
c        print*,'In Hextedata'
c        print*,'itotpha is ',itotpha
c        print*,'small,large,itotevents,itotgood,doffsettotal',
c     &     small,large,itotevents,itotgood,doffsettotal
c        print*,'ittot,itotphase,itotpha',ittot,itotphase,itotpha
        
c      Parse the TFORM value to find out the type of data storage
c      and the number of 8 byte values that there are. The type
c      is returned in "dtype" where 11 or 1 = 'X', and the rcount value
c      is the number of bits/8. We have to parse the proper TDDES
c      field to find out where the bits which tell which channel detected
c      the photon resides at a later point.

        call ftbnfm(tform,dtype,rcount,width,status)

c        print*,'colpos and colpost are',colpos,colpost
c            print*,'tform,dtype,rcount,width,colpos'
c            print*,tform,dtype,rcount,width,colpos

c      Print out any error information about accessing the files
c      26Jan99 (MJT) changed fcerr --> fcerrm
        if (status.ne.0)then
          call fcerrm(status)
        endif

c        print*,'Going into parseddl',tddes
        isize=0
        call parseddl(tddes,dtype,iskip,jskip,cpixold,
     &     foundm,mval)

        call parsemacro(iunit,cpixold)
        call parsebd(cpixold,inobd,ne,ilbd,iubd,.FALSE.,status)

c        print*,'Out of parseddl and iskip and jskip are '
c     &     ,iskip,jskip
c            print*,'cints is ',(cints(i),i=1,ichno)
c            print*,'cinte is ',(cinte(i),i=1,ichno)
c            print*,'ilbd is ',(ilbd(i),i=1,inobd)
c            print*,'iubd is ',(iubd(i),i=1,inobd)            
c######################################################################

c          Sort over all of the time intervals to find the absolute
c minimum set of time intervals that will be allowed in this cycle for
c the file that is being processed.


        it1=1
        it2=0
        it3=0
        numranges=0
c        PRINT*,'ICOUNT AND ICYCLES IS ',ICOUNT,ICYCLES
c        print*,'ipermno is ',ipermno
c        print*,'OR FILES ARE',itimno(2,ifile),itimno(1,ifile)
        
        do j=1,4

c          print*,'THERE are FOUR comparisons doing ',j

          if(j.eq.1)then

            timerange3(1)=timelower(icycles)
            timerange4(1)=timeupper(icycles)
            timerange1(1)=0.0d0
            timerange2(1)=0.0d0
            iranges=1
c            print*,'CYCLES timerange3 and timerange4 A',iranges,
c     &         timerange3(it1),timerange4(it1)

          elseif(j.eq.2)then
               
            it2=1
            do it1=itimno(1,ifile),itimno(2,ifile)
              timerange1(it2)=tmjds(it1)
              timerange2(it2)=tmjde(it1)
              it2=it2+1
c              print*,'FILE number is B',ifile
c              print*,'OR timerange1 and 2',it2-1,timerange1(it2-1),
c     &           timerange2(it2-1)
            enddo
            
            numranges=(itimno(2,ifile)-itimno(1,ifile))+1

c            print*,'num',itimno(2,ifile),itimno(1,ifile), numranges
            
          elseif(j.eq.3)then
            
            it2=1
            do it1=1,ipermno
              timerange1(it2)=tmjds(itimnoall+it1)
              timerange2(it2)=tmjde(itimnoall+it1)
c              print*,'AND timerange1 and 2 C',it2,timerange1(it2),
c     &           timerange2(it2)
              
              it2=it2+1
            enddo
            numranges=ipermno
          endif

c          call fcecho(' ')
c          call fcecho('Processing Timeranges')
            
          itot=0

c          if(j.eq.4)print*,'J IS 4!!!'
c          print*,'numranges and iranges',numranges,iranges
             
          do it1=1,iranges

c            print*,'timerange3 and timerange4 D',
c     &         timerange3(it1),timerange4(it1)

            do it3=1,numranges
              
              small=timerange1(it3)
              large=timerange2(it3)
              
              if(small.gt.timerange4(it1))then
c                print*,'small gt timerange4'
                goto 219
              endif
              
              if(large.lt.timerange3(it1))then
c                print*,'large gt timerange3'
                goto 219
              endif
              if(small.lt.timerange3(it1).and.
     &           small.lt.timerange4(it1))then
                small=timerange3(it1)
c                print*,'SET small ',small
              endif
              
              if(large.gt.timerange4(it1))then
                large=timerange4(it1)
c                print*,'SET large',large
              endif
              
              itot=itot+1
              timeranges(itot)=small
              timerangee(itot)=large
              
219           continue

c              print*,'ITOT is ',itot,j
c              if(j.eq.2)then

c              endif
              
            enddo
            
          enddo
          
c          print*,'ITOT AFTER ALL loops is ',itot
          
          do it1=1,itot
            timerange3(it1)=timeranges(it1)
            timerange4(it1)=timerangee(it1)
            iranges=itot
c            print*,'TIMERANGE3 and TIMERANGE4 DD',timerange3(it1),
c     $         timerange4(it1)
           
          enddo

c     If there are NO overlapping time-ranges then skip out of this
c     loop. 
          if(itot.eq.0.and.j.gt.1)then
c     print*,'ITOT WAS ZERO!!!'
c            status=0
c            call ftclos(iunit,status)            
            return
          endif
          
        enddo
        
301     continue

c           numranges=itot
        call dinitial(ngti,timerange1)
        call dinitial(ngti,timerange2)

c        print*,'ITOT after sorting is ',itot

c       Let's look for time intervals that have the same start and
c stop times - this happens if several of the input times overlap.
c Ideally, this should never happen, but more than a few people have
c input several time-ranges which have the stop time of one time range
c equal to the start time of another and they have done it in several
c GTI's so that each will result in a separte time range with the start
c     time equal to the stop time.
c        do j=1,itot
c          print*,'timerange3 and timerange4 DDD',
c     &       timerange3(j),timerange4(j)
c        enddo
        
        it2=0
        do j=1,itot
          if(timerange3(j).ne.timerange4(j))then
            it2=it2+1
            timerange1(it2)=timerange3(j)
            timerange2(it2)=timerange4(j)
c               print*,'TIMERANGE 1 and 2 are E',
c     &            it2,timerange1(it2),timerange2(it2)
          endif
        enddo
           
        itot=it2
           
        call dinitial(ngti,timerange3)
        call dinitial(ngti,timerange4)

c        print*,'timerange3(1) is ',timerange3(1)
c        print*,'timerange4(1) is ',timerange4(1)
        it3=0

c        print*,'ITOT is ',itot,it2,it3
c        print*,' '

c          Now we have to check for overlapping time intervals...
        if(itot.gt.1)then
          do j=1,itot-1

            if(it3.gt.0)then
              if(timerange3(it3).eq.0.0d0)then
                it3=it3+1
                timerange3(it3)=timerange1(j)
c                print*,'Timerange3 is ',it3, timerange3(it3)
              endif
            else
              it3=it3+1
              timerange3(it3)=timerange1(j)
c              print*,'El Timerange3 is ',it3, timerange3(it3)
            endif
            
            if(timerange2(j).eq.timerange1(j+1))then
c              print*,'Found that end of  ',j,' = start of ',j+1
            else
              timerange4(it3)=timerange2(j)
c              print*,'Timerange4 is ',it3, timerange4(it3)
              it3=it3+1
              timerange3(it3)=timerange1(j+1)
c              print*,'2 Timerange3 is ',it3, timerange3(it3)

            endif
            
            if(j.eq.itot-1)then
              timerange4(it3)=timerange2(j+1)
c              print*,'2 Timerange4 is ',it3, timerange4(it3)
            endif
            
          enddo
          itot=it3
        else
c          print*,'IN ELSE'
          timerange3(1)=timerange1(1)
          timerange4(1)=timerange2(1)
        endif

c        print*,' '
c        do j=1,itot
c          print*,'The timeranges are',j,timerange3(j),timerange4(j)
c        enddo
        
        
c          We are finished sorting time bins looking for the
c resulting time intervals that are actually active in this cycle.
c The variable ITOT contains the total number of acceptable TIME
c intervals and the variables TIMERANGE3 and TIMERANGE4 contain the
c start and stop times of those intervals that passed.

c          If NO time ranges passed all of the criteria then
c we might as well skip doing any processing and go onto the next
c cycle after printing a warning.
        
        if(itot.eq.0)then
          call fcecho(' ')
          call fcecho('Skipping this iteration of 100,000 bins.')
             call fcecho('TIME FILTERING will remove all of these elemen
     &ts, so we skip over processing.')
          call fcecho(' ')
             call fcecho('** Note that the TOTAL number of elements will
     &          be incorrect.')
          return
        endif

c        print*,'ITOT is ',itot, ifile, icycles
        
c        do j=1,itot
c          print*,'TIMERANGE3 AND 4 ARE ',j,
c     &       timerange3(j),timerange4(j)
c        enddo
c        print*,' '
        
c        icount=0
           
c        print*,'TIMS(1) is ',tims(1),icount
c        print*,'DOFFSETTOTAL(ICYCLES) ',doffsettotal(icycles)
c        print*,'DCHANLOWER(ICYCLES) ',dchanlower(icycles)
c        
c          Okay, let's figure out how many time bins are filled - or allowed
c with the present acceptable time ranges. So we will fill up the appropriate
c number of light-bins
        do j=1,itot

c          print*,' '
c          print*,' '
c          print*,'We are GTI number ',j,itimno(1,1),j-1+itimno(1,1),
c     $       itimno(2,1),' And time is  ',
c     $       tmjds(j-1+itimno(1,1)),tmjde(j-1+itimno(1,1))
          
c     At this point we have to add some "special logic" to deal with
c     the case where we may have multiple GTI's per light-bin.
c     This is normally NOT an issue, but HEXTE data (unfortunately)
c     uses a "rocking" motion and hxtback removes teh off-source times
c     with GTI's and also sets the bin-size to a LARGE value. Thus
c     there can multiple GTI's per light-bin, which violates the
c     logic of what follows, so we have to add yet another
c     "special cases" to deal with this problem... 


c     The entire logic of the following is designed to be able
c     to hadle the case where therer are millions or more
c     light-bins per GTI. So we have to modify it to handle
c     the case where there are multiple GTI's per light-bin
c     as these are mutually exclusive states.

c     So first we have to figure out if a GTI is contained within
c     one light-bin, or the more normal case of multiple light-bins
c     per GTI. 

c     Here we are figuring which light bin the leading edge
c     of the GTI falls into. This value is set to be "ibottom".
          dper=dtruc(((timerange3(j)-tims(1))/binsz)+1.0d0)
          dpernoval=(((timerange3(j)-tims(1))/binsz)+1.0d0)
          dleft=dtruc(dpernoval)

c          print*,'DPER bottom is ',dper
          
c     This little holding variable is used to tell us if
c     the code has undergone a transition from one light-bin
c     to another when the light-bin for the GTI is the same...
          
          dhold1=dleft
          if(j.eq.1.and.ifile.eq.1)dhold2=dleft

c          print*,'Dhold1 and Dhold2 and j are',dhold1, dhold2
c          print*,'Dpernoval and dleft',dpernoval,dleft,binsz
c          print*,'Subtract',dpernoval-dleft

          dleft=(dpernoval-dleft)*binsz

c          print*,'From beginning 3 the leading edge of this cycle'
c          print*,'(DLEFT) is ',dleft
          
          dbottom=(dper-doffsettotal(icycles)
     &       -dchanlower(icycles))
          dbottom=dtruc(((timerange3(j)-tims(1))/binsz)+1.0d0)
          
          ibottom=idint(dper-doffsettotal(icycles)
     &       -dchanlower(icycles))

c     Since rounding errors can sometimes place us into the
c     wrong bin, so if that is the case move to the next bin...
          if(ibottom.eq.0)then
c            print*,'IBOTTOM was 0'
            ibottom=1
          endif

c          if(ibottom.le.2)then
c            print*,'Ibottom is ',ibottom,icount
c            print*,'dbottom and ibottom are ',dbottom,ibottom
c            print*,'timerange3(j), tims(1)',timerange3(j),tims(1)
c            print*,'dper doffsettotoal dchanlower',icycles,
c     &         dper, doffsettotal(icycles),dchanlower(icycles)
c          endif
          

c     Here we are figuring which light bin the trailing edge
c     of the GTI falls into. This value is set to be "itop".   
          dper=dtruc(((timerange4(j)-tims(1))/binsz)+1.0d0)
          dpernoval=(((timerange4(j)-tims(1))/binsz)+1.0d0)
          dstart=dtruc(dpernoval)

c          print*,'Dpernoval and dstart',dpernoval,dstart,binsz
c          print*,'Subtract',dpernoval-dstart

          dstart=(dpernoval-dstart)*binsz

c          print*,'From end of this range the trailing edge'
c          print*,'(DSTART) is ',dstart

          dtop=(dper-doffsettotal(icycles)
     &       -dchanlower(icycles))
          dtop=dtruc(((timerange4(j)-tims(1))/binsz)+1.0d0)
          itop=idint(dper-doffsettotal(icycles)
     &       -dchanlower(icycles))

          if(itop.eq.0)then
            itop=1
          endif
          
c          if(itop.gt.99995)then
c            print*,'dtop and itop are ',dtop,itop
c            print*,'timerange4(j),tims(1)',timerange4(j),tims(1)
c            print*,'dper doffsettotoal dchanlower',icycles, dper,
c     &         doffsettotal(icycles),dchanlower(icycles)
c          endif
          
          
c          print*,'IBOTTOM and ITOP are',ibottom,itop
c          print*,'DBOTTOM and DTOP are',dbottom,dtop
c          print*,'DLEFT and Dstart are',dleft,dstart
c          print*,'ICOUNT is ',icount

c          print*,'About to handle this GTI'

          if (ibottom.eq.itop) then
c            print*,'IBOTTOM and ITOP are equal',ibottom
c            print*,'timerange4 and timerange3 is ',timerange4(j),
c     $         timerange3(j)
            
c     We have to look to see if we have moved on to another light-bin
c     and if so, than increment icount by one before continuing. 
            if(dhold1.ne.dhold2)then
c              print*,'dhold1 and dhold2 NE',dhold1,dhold2
c              print*,'We are incrementing icount from  ', icount,
c     $           '  to ',icount+1
              dhold2=dhold1
              icount=icount+1
            endif

            if(icount.eq.0)then
              icount=1
            endif
            
            iarray(ibottom)=icount
            iperiod=iarray(ibottom)
            
            if(iperiod.ne.0)then
              realbin(iperiod)=realbin(iperiod)+
     $           (timerange4(j)-timerange3(j))
              dpernoarray(iperiod)=dbottom
c              print*,'REALBIN is ',realbin(iperiod),iperiod,
c     $           icount
              
            else
c              print*,'Tried to access zeroth element!!!'
            endif

c            print*,'Dstart is ',dstart
c            print*,'Dleft is ',dleft
c            print*,'Dpernoval is ',dpernoval
            dleft=0.0d0
            dstart=0.0d0
            
          else

c            print*,'IBOTTOM and ITOP NOT EQUAL'

            if(dleft.gt.0.0d0)then
c              print*,'In dleft gt 0.0d0'

              if((dleft+1.0d-6).ge.binsz)dleft=0.0d0
           
c              print*,'In iarray reset value ',icount, ibottom

              if(icount.eq.0)then
                icount=1
              endif
              
              iarray(ibottom)=icount
              iperiod=iarray(ibottom)
              if(iperiod.ne.0)then
                realbin(iperiod)=realbin(iperiod)+(binsz-dleft)
                dpernoarray(iperiod)=dbottom
              else
c                print*,'Tried to access zeroth element!!!'
              endif
                          
c              if(iperiod.ne.0)then
c                print*,'dpernoarray is ',dbottom
c                print*,'realbin(iperiod) is ',realbin(iperiod)
c              endif
              
            else

c                 Since we have incremented icount once before entering
c     we don't want to do it again and thus cause problems in the logic down stream.
c     So we only increment icount if we are at a GTI in the SAME file. 
              if(icount.ne.icountentry)then
c                print*,'In top else statement and icount is',icount+1
                icount=icount+1
              endif
              if(icount.eq.0)then
c                print*,'In top else statement and icount 0 is',icount+1
                icount=icount+1
              endif
              
              iarray(ibottom)=icount
              iperiod=iarray(ibottom)
              realbin(iperiod)=realbin(iperiod)+binsz
              dpernoarray(iperiod)=dbottom
            endif

c            print*,'About to do time calculation'
c            print*,'And ICOUNT starting is',icount

c            print*,'ibottom and TIME is ',ibottom,
c     $         (dchanlower(icycles)
c     &         +dfloat(ibottom)+doffsettotal(icycles)-1)*binsz+
c     $         tims(1)
c            print*,'itop and TIME is ',itop,
c     $         (dchanlower(icycles)
c     &         +dfloat(itop)+doffsettotal(icycles)-1)*binsz+
c     $         tims(1)

            if(ibottom+1.le.itop-1)then
          
              do k=ibottom+1,itop-1
                icount=icount+1
                iarray(k)=icount
                iperiod=iarray(k)
                realbin(iperiod)=realbin(iperiod)+binsz
                dpernoarray(iperiod)=dchanlower(icycles)
     &             +dfloat(k)+doffsettotal(icycles)
c            print*,'k_new and stuff',
c     $         iarray(k), icount, iperiod,
c     $         doffsettotal(icycles),
c     $         realbin(iperiod),dpernoarray(iperiod)

c     print*,'REALBIN(iperiod) primary is ',iperiod,
c     &         realbin(iperiod),
c     &         dpernoarray(iperiod),icount
              enddo

c              print*,'IPERIOD after loop is ',iperiod,icount
            else
c              print*,'Skipped ibottom+1 -- itop-1 loop'
          
            endif

            if(dstart.gt.0.0d0.and.itop.le.ichan+100)then
c              print*,'In dstart gt 0.0d0o and itop le ichan'
c              print*,'dstart and itop',dstart,itop,ichan
              if((dstart+1.0d-6).ge.binsz)dstart=binsz
c              print*,'DSTART is now',dstart
              icount=icount+1
c              print*,'In icount was incremented to',icount
c              print*,'Itop is ',itop
              iarray(itop)=icount
              iperiod=iarray(itop)
              realbin(iperiod)=realbin(iperiod)+dstart
              dpernoarray(iperiod)=dtop
            elseif(itop.lt.jichan)then
c              print*,'IN ELSE for top and icount is',icount
            
            endif

c            print*,'itop_new and stuff',
c     $         iarray(itop), icount, iperiod,
c     $         realbin(iperiod),dpernoarray(iperiod)

          endif

c          print*,'REALBIN(iperiod) is ',iperiod,realbin(iperiod),
c     &       dpernoarray(iperiod),icount
          
c          print*,'ICOUNT is ',icount
c          PRINT*,'ICOUNT AND ICYCLES at end IS ',ICOUNT,ICYCLES,
c     &       ibottom,itop

c          print*,'Dpernoarrary is ',dbottom,
c     &       float(ibottom+1),float(itop-1),dtop
          
        enddo

c        print*,'Out of timing routine and icount is ',icount
        
c        stop
c        return
          
c----------------------------------------------------------------------

c      Loop over all of the rows that are stored in each file
c

        call ftgkyd(iunit,'TIMEZERO',timezero,comm,status)
        if(status.ne.0)then
          status=0
          call ftgkyt(iunit,'TIMEZERO',itimezero,timezerof,
     &       comm,status)
          if(status.ne.0)then
            timezero=0.0d0
            status=0
          else
            timezero=dfloat(itimezero)+timezerof
         endif
        endif
        
        call fcecho(' ')
        call fcecho('Processing data from file:')

        if(itimecycle.gt.1)then
          cval=' '
          charall=' '
          call fti2c(icycles,cval,status)
          charall='Beginning to process cycle'
          charall(28:38)=cval(11:20)
          charall(40:70)='of             cycles.'
          call fti2c(itimecycle,cval,status)
          charall(43:53)=cval(11:20)
          call fcecho(' ')
          call fcecho(charall)
          status=0
        endif
       
c        print*,'ICOUNT is ',icount
        
        call fti2c(nrows,cval,status)
        status=0
        contxt=' '
        contxt='This file contains:            rows.'
        contxt(20:30)=cval(10:20)
        call fcecho(contxt)

        if(dcycleend(ifile).gt.1.0d0)then
          dcyclestart(ifile)=dcycleend(ifile)
          cval=' '
          icyclestart=idint(dcyclestart(ifile))
          call fti2c(icyclestart,cval,status)
          status=0
          contxt=' '
          contxt='Processing file beginning on row '
          contxt(34:44)=cval(10:20)
          call fcecho(contxt)
        endif

        icyclestart=idint(dcyclestart(ifile))
        imiss=0

c        print*,'icycstart,nrows',icyclestart,nrows

c        goto 51
        
        do 50 lrow=icyclestart,nrows
c          print*,'Reading row',lrow,nrows
c          goto 51
          if(mod(lrow,100000).eq.0)then
c            print*,'TIMEVAL is ',timeval
            call fti2c(lrow,cval,status)
            status=0
            contxt=' '
            contxt='Processed              rows, continuing.'
            contxt(12:22)=cval(10:20)
            call fcecho(contxt)
          endif
          
          fbit=1
          nulvald=0.0d0
          
c      Increment value used in performing the read of the array.
          incr=1

c      What to do with null values? And what is the increment?
          nultyp=1
          incr=1
           
c      What to do with null values?
          nulval=0

c      Read in the timestamp for each row - there is usually only
c      one timestamp per row.

c         call ftgcld(iunit,colpost,lrow,1,1,incr,
c    &       nultyp,nulvald,timeval,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
          call ftgcvd(iunit,colpost,lrow,1,1,nulvald,
     &         timeval,anynul,status)

c          print*,'timeval is ',timeval
          
          timeval=timeval+timezero
          timeval=timeval*conversion

c          print*,'timeval is ',timeval          
          
c      Print out any error information about accessing the files
c      26Jan99 (MJT) changed fcerr to fcerrm
          if (status.ne.0)then
            call fcerrm(status)
          endif


c            print*,'timelower(icycles) AND timeupper(icycles)',
c     &         icycles,timeval,timelower(icycles),timeupper(icycles)
          
          if(timeval.ge.(timelower(icycles)).and.
     &       timeval.lt.(timeupper(icycles)))then

            dcycleend(ifile)=lrow
            
          else

            if(timeval.gt.(timeupper(icycles)+dlta))then
              imiss=imiss+1
              if(imiss.gt.maxmiss)goto 51
            endif
            
            goto 771
          endif
          
          devents=devents+1.0d0
          dtotevents=dtotevents+1.0d0

c      Read in the telemetry string as if it were a string of logicals
c      of a certain length. We will make use of ISKIP (the number of elements
c      to skip to get to the beginning of the proper place) to
c      determine which channel received the photon.
          call ftgcx(iunit,colpos,lrow,fbit,rcount*8,array,status)

c      Print out any error information about accessing the files
c      26Jan99 (MJT) changed fcerr to fcerrm
          if (status.ne.0)then
            call fcerrm(status)
          endif
c----------------------------------------------------------------------
c This section is dedicated to performing all operations to compare
c the input logical string to the BITMASKS that were read in from the
c input "bitmask" data file. We have to perform these checks for EACH
c data element read in so this could slow things down considerably.

          lbitmask=.TRUE.

          if(ibitmask.gt.0)then

            do ij=1,ibitmask

              itotal=0
              isize=2**(iopval(ij)-1)
              if(isize.eq.0)isize=1
          
              do jsize=1,iopval(ij)
                if(array(iskipfirst(ij)+jsize))itotal=itotal+isize
                isize=isize/2
              enddo

              if(icompare(ij).eq.1)then
                if(itotal.eq.iopnum(ij))then
c                  print*,'IOPNUM(Ij) eq is',itotal,iopnum(ij)
                else
                  lbitmask=.FALSE.
                endif

              elseif(icompare(ij).eq.4)then
                if(itotal.le.iopnum(ij))then
c                  print*,'IOPNUM(Ij) le is',itotal,iopnum(ij)
                else
                  lbitmask=.FALSE.
                endif

              elseif(icompare(ij).eq.5)then
                if(itotal.ge.iopnum(ij))then
c                  print*,'IOPNUM(Ij) ge is',itotal,iopnum(ij)
                else
                  lbitmask=.FALSE.
                endif

              endif
              
            enddo

          else
            lbitmask=.TRUE.
          endif

c         IF any of the bitmask comparisons fail then we have to skip
c         this data element.

          if(.not.lbitmask)then
c            print*,'Skipping value', lbitmask
            dbitmask=dbitmask+1.0d0
            goto 50

          else
c            print*,'Accepting value', lbitmask
            
          endif
          

c----------------------------------------------------------------------
          
c----------------------------------------------------------------------
c      This is where we will determine the channel which received the
c      photon. That value will be stored in ITOTAL. ISIZE maintains 
c      the value of each bit as we move through the logical. 
          itotal=0

c          print*,'isize is',isize,jskip

c      Since we are dealing with 8 bit integers we will have to move
c      through 8 elements within the logical array. If that element
c      within the logical is true then we increment ITOTAL by the value
c      within the place holder ISIZE. Then we divide ISIZE by 2 and proceed
c      to the next element

          isize=2**(jskip-1)
          if(isize.eq.0)isize=1

c          print*,'iskip is',iskip,jsize,isize
          
          do 74 jsize=1,jskip
            if(array(iskip+jsize))itotal=itotal+isize
            isize=isize/2
74        continue

c          print*,'Channel receiving input is ',itotal

c      ITOTAL now contains the channel which detected the photon...
c      However since the channels run from 0 - 255 and array the they
c      are placed into runs from 1 - 256 we must increment itotal by
c      one if we are going to use it as a pointer
          itotal=itotal+1
          colval=itotal
c          print*,'Channel is ',itotal-1,iskip,jskip

c----------------------------------------------------------------------        
c      Since we now have all of the information necessary we will write
c      out the timestamp and channel.

c          print*,'timeval,channel is ',timeval,itotal
c      print*,'cints(l),cinte(l),itotal        '

          land=.FALSE.
          lor=.FALSE.
          cand=.FALSE.
          pand=.FALSE.

          do 75 k=itimno(1,ifile),itimno(2,ifile)

            if(timeval.ge.tmjds(k).and.
     &         timeval.lt.(tmjde(k)))then
              lor=.TRUE.
            endif
            
75        continue

          
          do 55 k=1,ipermno

            if(timeval.ge.tmjds(itimnoall+k).and.
     &         timeval.lt.(tmjde(itimnoall+k)))then
              land=.TRUE.
            endif

c            print*,'LAND is ',land
            
55        continue
                  
          do 82 k=1,ichno
            if(ilbd(colval).le.cints(k).and.
     &         iubd(colval).ge.cinte(k))then
              cand=.TRUE.
            elseif(ilbd(colval).ge.cints(k).and.
     &           ilbd(colval).le.cinte(k))then
              cand=.TRUE.
            elseif(iubd(colval).ge.cints(k).and.
     &           iubd(colval).le.cinte(k))then
              cand=.TRUE.
            endif
            
82        continue

          if(period.gt.0.0d0)then
            if((tmjds(itimnoall+ipermno+1).eq.0.0d0).and.
     &         (tmjde(itimnoall+ipermno+1).eq.1.0d0))then
              pand=.TRUE.
            else

              iperstart=int((timeval-ephem)/period)
              
              do 155 k=1,iphaseno
                  
                pstart=ephem+(float(iperstart)*period)+
     &             period*tmjds(itimnoall+ipermno+k)

                pstop=ephem+(float(iperstart)*period)+
     &             period*tmjde(itimnoall+ipermno+k)
                
                if((timeval.ge.pstart).and.
     &             (timeval.le.pstop))then
                  pand=.TRUE.
                  goto 154
                endif
                        
155           continue
154           continue
            endif
            
          else
            pand=.TRUE.
          endif

          if((.not.lor).or.(.not.land))then
            
            dttot=dttot+1.0d0
            dtim=dtim+1.0d0
          elseif(.not.cand)then
c            print*,'in cand and ',timeval
            dpha=dpha+1.0d0
            dtotpha=dtotpha+1.0d0
          elseif(.not.pand)then
c            print*,'in pand and ',timeval
            dphase=dphase+1.0d0
            dtotphase=dtotphase+1.0d0
          endif
            
          if(lor.and.land.and.pand.and.cand)then

c            print*,'colval is ',colval

            dtotgood=dtotgood+1.0d0
            dgood=dgood+1.0d0

            dper=dtruc(((timeval-tims(1))/binsz)+1.0d0)
            dpernoval=dper
            iper=idint(dper-doffsettotal(icycles)
     &         -dchanlower(icycles))

            if(iper.eq.0)then

c     Unfortunately the SE fits files are chock full of tiny errors when
c it comes to TSTART and TSTOP - they are off by values ranging from 1D-9
c to 1D-6 which makes it VERY hard to filter properly. So getting a
c timebin that is less than or equal to 0 only means that the SLOP in
c the file is bigger than it should be if the files had been written with
c checks. So we have to catch these occurances and deal with them
c as special cases. We do this by simply incrementing them by 1. It isn't
c perfect but such is life.
              dper=dper+1
              dpernoval=dper
              iper=iper+1
c              print*,'INCREMENTING IPER',iper,dper
            endif
            
            if(iarray(iper).eq.0.0d0)then
c              goto 50
c              dper=dtruc(((timeval-tims(1))/binsz)+1.0d0)
              
c              print*,'IPER initial is ',iper,timeval,tims(1)
c              print*,'TIMELOWER and UPPER are',
c     $           timelower(icycles), timeupper(icycles)
c              iper=idint(dper-doffsettotal(icycles)
c     &           -dchanlower(icycles))
c              print*,'IPER after modification is',dper,iper,
c     &           doffsettotal(icycles),dchanlower(icycles),
c     &           icycles,timezero
c              print*,'iper is ',iper,icycles

c              status = 999
c              return

              call fcecho(' ')
              call fcecho('A problem has been noted!')
              call fcecho('This problem is invariably caused by')
              call fcecho('machine accuracy.')
              call fcecho('Try using a larger lcbinarray (see fhelp)')
              call fcecho('Aborting')
              stop
              
c              icount=icount+1
c              iarray(iper)=icount 
c              realbin(icount)=binsz
c              dpernoarray(iperiod)=dpernoval              
              
            endif

            iperiod=iarray(iper)

            if(itotal.lt.0)then
              call fcecho('Negative value found in data')
              call fcecho('Check data - proceeding using data')
            endif

            dbin(iperiod)=dbin(iperiod)+1.0d0
            dbincount(iperiod)=dbincount(iperiod)+1.0d0

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if(chbin.ne.'INDEF')then
              do 275 jnum=1,ichbinum
                if(ilbd(colval).ge.ichbins(jnum).and.
     &             iubd(colval).le.ichbine(jnum))then
                  
                  inum=jnum
                  
                  dchannel(inum)=dchannel(inum)+
     &               1.0d0
                         
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
                  realchan(inum)=realchan(inum)+1.0d0
                  goto 276

                endif
275           continue
            else
                          
              inum=colval
                          
              dchannel(inum)=dchannel(inum)+1.0d0
                                
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
              realchan(inum)=realchan(inum)+1.0d0
              
            endif

276         continue

c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%            
            
          endif

771       continue
          
50      continue
        
51      continue
        
c        print*,'ICOUNT is ',icount
        
c        call fti2c(nrows,cval,status)
        call fti2c(lrow-1,cval,status)        
        status=0
        contxt=' '
        contxt='Processed              rows.'
        contxt(12:22)=cval(10:20)
        call fcecho(contxt)
 
c        print*,'lrow is ',lrow,nrows
        
c        print*,'columns,devents,dgood,dtim,dphase,dpha'
c        print*,devents,dtim,dphase,dpha
c        print*,columns,dtotevents
        
c      print*,'Dttot,dtotphase,dtotpha',dttot,dtotphase,dtotpha

        ljump=.TRUE.
c        print*,'FINISHED reading this file',ifile
5005    continue
        
c###################################################################### 
        return
        end


c**********************************************************************  
       subroutine parseddl(tddes,dtype,iskip,jskip,
     &   cpixold,foundm,mval)

c       implicit none
       character*(*) tddes,cpixold
       character(40) outval
       integer dtype,iskip,j,k,jsize,istart,istop,
     &      outvali,itotal,outlen,fcstln,status,jskip,jhold
       logical foundchn,foundm,mval

c      Check to see if the information is stored as a binary string.
c      i.e. that TFORM has the form of nnnX

c      Find out how long the string is that is in either TEVTBn or
c TDDESn and assign that to outlen.

       status=0
       iskip=0
       jskip=0
       outvali=0
       outlen=fcstln(tddes)
       istart=0
       jhold=0
       foundchn=.FALSE.
       
c       print*,'tddes is ',tddes
c       print*,'outlen is ',outlen
       
c       print*,'IN PARSEDDL',dtype

       if(dtype.eq.11.or.dtype.eq.1)then

c       print*,'dtype is 11 and tddes is',tddes
c----------------------------------------------------------------------        
c      Now that we have the information stored in the DDL we have to
c      make use of it to determine where the information that we want
c      is stored. 
          j=0
          k=0
          itotal=0

c      The purpose of the following loop is to skip all of the
c      telemetry information which doesn't interest us. 
          do 73 jsize=1,outlen
             j=j+1

c      Check to see that we have gone far enough such that we are
c      actually into the part of the string which tells us how the
c      information is stored...
             
             foundchn=.TRUE.
             
             if(foundchn.and.tddes(j:j+1).eq.'C[')then
               jhold=j
c               j=9999
               goto 74
             endif

c      Find the position of each { so that we can find out the
c      number of bits that are associated with each part of the DDL. We
c      find out this value and set istart equal to this point.
             
             if(foundchn.and.tddes(j:j).eq.'{'.and.j.ne.9999)then
                istart=j
             endif
c      Find the position of the } so that we can find out the
c      number of bits that are associated with each part of the DDL. We
c      find out this value and set istop equal to this point.

             if(foundchn.and.tddes(j:j).eq.'}'.and.j.ne.9999)then
                istop=j

c      Now we set the temporary sting equal to the string that is contained
c      within istart and istop and convert that to an integer.
                
                outval=tddes(istart+1:istop-1)
                call ftc2i(outval,outvali,status)
c                print*,'outval and outvali is ',outval,outvali

c      We sum up these values so that we know the total number of "bits"
c      to skip.
                
                iskip=iskip+outvali
             endif

c             print*,'j is ',j
             
73         continue
74         continue

c           print*,'ISKIP is ',iskip
           
c      We have stored the number of element to skip in ISKIP.
c----------------------------------------------------------------------        
          status=0

c      Test to see if the information is stored as Nbit
c      unsigned integers in the form nnnI
          k=jhold
c          print*,'jhold is ',jhold,iskip,outlen
          
          do 75 jsize=1,outlen-jhold
            k=k+1
            if(foundchn.and.tddes(k:k).eq.'{')then
              istart=k
            endif
c      Find the position of the } so that we can find out the
c      number of bits that are associated with each part of the DDL. We
c      find out this value and set istop equal to this point.

            if(foundchn.and.tddes(k:k).eq.'}')then
              istop=k

c      Now we set the temporary sting equal to the string that is contained
c      within istart and istop and convert that to an integer.
                
              outval=tddes(istart+1:istop-1)
              call ftc2i(outval,outvali,status)

c      We sum up these values so that we know the total number of "bits"
c      to skip.
                
              jskip=jskip+outvali

              cpixold(1:1)='('
              cpixold(2:(istart-jhold)-2)=
     &           tddes(jhold+2:istart-2)
              cpixold((istart-jhold)-1:(istart-jhold)-1)=')'
              
              goto 999
            endif

75        continue
          
       elseif(dtype.eq.21)then

         
c       print*,'dtype is 21 ,tddes is',tddes
c----------------------------------------------------------------------        
c      Now that we have the information stored in the DDL we have to
c      make use of it to determine where the information that we want
c      is stored. 
          j=0
          k=0
          itotal=0

c      The purpose of the following loop is to skip all of the
c      telemetry information which doesn't interest us. 
          do 173 jsize=1,outlen
             j=j+1
             if(foundchn.and.tddes(j:j).eq.'^')goto 174
             if(tddes(j:j+1).eq.'>>')foundchn=.TRUE.
             
             if(.not.foundchn.and.tddes(j:j).eq.'M'.and.j.ne.9999)then
                foundm=.TRUE.
                outval=tddes(j+2:j+3)
                call ftc2i(outval,outvali,status)
                if(outvali.eq.1)mval=.TRUE.
             endif
             
             if(foundchn.and.tddes(j:j+1).eq.'C[')then
               jhold=j
               j=9999
               goto 174
             endif
             if(foundchn.and.tddes(j:j).eq.'{'.and.j.ne.9999)then
               istart=j
             endif
             if(foundchn.and.tddes(j:j).eq.'}'.and.j.ne.9999)then
                istop=j
                outval=tddes(istart+1:istop-1)
                call ftc2i(outval,outvali,status)
c                print*,'outval and outvali is ',outval,outvali
                iskip=iskip+outvali
c                print*,'iskip is ',iskip,outvali,istart+1,istop-1,
c     &             ' ',outval
                
              endif
          
173        continue
174        continue
c           print*,'ISKIP is ',iskip
c      We have stored the number of element to skip in ISKIP.
c----------------------------------------------------------------------        
           status=0

           k=jhold
           do 175 jsize=1,outlen-jhold
             j=j+1
             if(foundchn.and.tddes(k:k).eq.'{')then
               istart=k
             endif
c      Find the position of the } so that we can find out the
c      number of bits that are associated with each part of the DDL. We
c      find out this value and set istop equal to this point.

             if(foundchn.and.tddes(k:k).eq.'}')then
               istop=k

c      Now we set the temporary sting equal to the string that is contained
c      within istart and istop and convert that to an integer.
                
               outval=tddes(istart+1:istop-1)
               call ftc2i(outval,outvali,status)

c      We sum up these values so that we know the total number of "bits"
c      to skip.
                
               jskip=jskip+outvali
               goto 999
             endif

175        continue
           
       endif

999    continue

       return
       end

