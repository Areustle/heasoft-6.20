****************************************************************************
C SELECTOR TASK:  
C      saextrct
C
C FILE:
C      saextrct.f 
C
C DESCRIPTION: 
C      Makes a 'light curve' and/or 'spectrum' output file from an SA
c      input file.  For specified columns containing the time and
c      the quantity to be binned, the tool sums over specified time
c      intervals producing the specified light curve and/or spectrum
C     
C AUTHOR:  
C      Brian K. Elza 4/96
C
C MODIFICATION HISTORY:
C
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
c  29Jan02 (MJT) Initialize "pointer" arguments to udmget to prevent
c                memory from unwittingly being realloc'd! (v4.2d)
c
c  18May06 (MJT) Changed calls to ftgtdm (get_tdim) which had maxdim arg
c     (v4.2e)    much too large (causing cfortran probs on 64-bit Linux)
c
c  01May09 (MJT) Increased lcbinarray (ichan) to 400000 to mirror change
c     (v4.2f)    in seextrct v4.2e
c
c  08Apr10 (MJT) Initialized inobd array and added kludge to prevent error
c                for Standard1 files (i.e., without CPIX) occurring on
c     (v4.2g)    64-bit platforms
C
C  2013-06-24 (CBM) Increase maximum number of input files in SAEXTRCT
C                and supporting library functions from 100 to 999.  This
C                is mostly controled by the ISIZ parameter which is 
C     (v4.3a)    scattered through various source code files.
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
c      chkit      - Check the files for errors? (Default is NO!)      
C      
C CALLED ROUTINES:
C      subroutine gsacrv - gets parameters from environment
C      subroutine fsacrv - read input FITS file and writes light curve file
C
C***************************************************************************
      subroutine saextt
      implicit none

      integer nb,isiz
      parameter (nb = 512)
c      parameter (ntime = 12800)
      parameter (isiz=999)
      integer itmjds,itmjde,itims,itime, ngti, ichan,
     &   ngti_temp, ichan_temp
      
      character(160) infile, outfile, gofile,gafile, phasefil,
     &   extenpha,extenlc
      character(80) timecol, columns, timeint, gticols, gtidate,
     &   gtitime, extnam, phaseint, mode, lcmode,spmode,
     &   accumulate,negative,
     &   writesum,writemean,
     &   outimecol, outcol, outerr, obsdate, obstime, printmode
        character(3000) chbin,chint      
      double precision timemin, timemax, ephem, period, mfracexp,
     &   mlcinten,mspinten,binsz
      logical gettmin, gettmax, copyprime,clobber,
     &   ldryrun, copyall, sensecase,
     &   getcmin, getcmax, getephem, chkit, abort
      
      integer chmin, chmax, status

        
      character(40) taskname

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
      common /task/ taskname

      common/input/infile,gofile,gafile,outfile,phasefil,
     &   timecol,columns,
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
      
      taskname = 'SAEXTRCT version 4.3a'
      infile = ' '
      gofile = ' '
      gafile = ' '
      outfile = ' '
      phasefil = ' '
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
      writesum= ' '
      writemean= ' '
      status=0
      abort=.FALSE.

      call fcecho(' ')
      call fcecho('Running SAEXTRCT version 4.3a')
      call fcecho('==============================================')
        
C     get the parameters from the par file
      call gsacrv(status)
      ichan=ichan_temp
      ngti=ngti_temp
      
      if (status .ne. 0) goto 999
      
C      Read in the FITS file and write out the light curve file.

c     But please, initialize "pointer" arguments to udmget / MJT 29Jan2002
c     so that it doesn't try to realloc memory you still need...
      itmjds = 0
      itmjde = 0
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

c        print*,'itmjds,itmjde,itims,itime'
c        print*,itmjds,itmjde,itims,itime
        
        call fsacrv(ichan,ngti,memd(itmjds),memd(itmjde),
     &     isiz,memd(itims),memd(itime),abort)

c        print*,'itmjds,itmjde,itims,itime'
c        print*,itmjds,itmjde,itims,itime
        
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

999     return
        
      end

c**********************************************************************
c
c SUBROUTINE:
C      gsacrv
c
c DESCRIPTION:
C      Get parameters from parameter file
C      
c AUTHOR:
C      Brian K. Elza 9/94
c
c MODIFICATION HISTORY:
C      None, yet...
c
c NOTES:
C      gsacrv uses F77/VOS like calls to read parameters from the .par file
C      
c USAGE:
C      call gsacrv(status)
c
c ARGUMENTS:
C      infile     - input FITS file and extension number
C      gofile     - input GTI file name that is to be OR'ed with infiles
C      gafile     - input GTI file name that is to be AND'ed with infiles
C      outfile    - output file name
C      phasefil   - phase file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
c      lcmode     - mode of operation SUM, RATE, or MEAN 
C      spmode     - mode of operation SUM, RATE, or MEAN 
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

        subroutine gsacrv(status)

        implicit none

        character(80) context
        character(20) cval

        character(160) infile, outfile, gofile,gafile, phasefil,
     &     extenpha,extenlc
        character(80) timecol, columns, timeint, gticols, gtidate,
     &     gtitime, extnam, phaseint, mode, lcmode,spmode,
     &     accumulate,negative,ctimezero,
     &     writesum,writemean,
     &     outimecol, outcol, obsdate, obstime, printmode
        character(3000) chbin,chint        
        double precision timemin, timemax, ephem, period, mfracexp,
     &     mlcinten,mspinten,binsz
        real tnull
        logical gettmin, gettmax, copyprime,clobber,
     &     ldryrun, copyall, sensecase,
     &     getcmin, getcmax, getephem, chkit,
     &     lmultiple, lbailout

        integer chmin, chmax, maxmiss, status, ichan, ngti
        
        common/input/infile,gofile,gafile,outfile,phasefil,
     &     timecol,columns,
     &     lcmode,spmode,
     &     accumulate,negative,extenpha,extenlc,
     &     writesum,writemean,timemin,timemax,timeint,gticols,
     &     gtidate,gtitime,extnam,chmin,chmax, chint,
     &     chbin,binsz,ephem,
     &     period, mfracexp,
     &     mlcinten,mspinten,clobber,ldryrun,
     &     phaseint,copyprime,copyall,sensecase,mode,gettmin,gettmax,
     &     getcmin,getcmax,getephem,chkit,printmode,outcol,outimecol,
     &     obsdate,obstime        

        common/multiple/lmultiple
        common/bail/lbailout
        common/miss/maxmiss
        common/nullval/tnull
        common/offset_value/ctimezero
        common/gtiinfo/ichan,ngti
        
C      initialize variables
        status=0
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
          ngti=30000
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
        if(gofile.eq.'-')gofile= ' '

C  get the name of the input gtifile to be AND ed with the infiles
        call uclgst('gtiandfile',gafile,status)
        if (status .ne. 0) then
          context = 'could not get GTIANDFILE parameter'
          call fcerr(context)
          goto 999
        endif
        if(gafile.eq.'-')gafile=' '
        
C  get the GTI column names, i.e. STOP START etc...
        call uclgst('gticols',gticols,status)
        if (status .ne. 0) then
          context = 'could not get GTICOLS parameter'
          call fcerr(context)
          goto 999 
        endif

C  get the GTI reference date keyword
C       call uclgst('gtidate',gtidate,status)
C       if (status .ne. 0) then
c          context = 'could not get GTIDATE parameter'
c          call fcerr(context)
c          goto 999
C       endif
        gtidate='MJDREF'

C  get the GTI reference date keyword
C       call uclgst('gtitime',gtitime,status)
c        print*,'gtitime ',gtitime 
C       if (status .ne. 0) then
c          context = 'could not get GTITIME parameter'
c          call fcerr(context)
c          goto 999
C       endif
        gtitime='TSTART'
        
C  get the name of the output root file
        call uclgst('outroot',outfile,status)
        if (status .ne. 0) then
          context = 'could not get OUTROOT prefix parameter'
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
          context = 'could not get PHASEFILE parameter'
          call fcerr(context)
          goto 999
        endif
        if(phasefil.eq.'-')phasefil=' '

C  get the name of the ACCUMULATE file
        call uclgst('accumulate',accumulate,status)
        if (status .ne. 0) then
          context = 'could not get ACCUMULATE parameter'
          call fcerr(context)
          goto 999
        endif
        call ftupch(accumulate)
        if(accumulate.ne.'MANY'.and.accumulate.ne.'ONE')then
          context='ACCUMLATE defaulting to ONE'
          call fcecho(context)
          accumulate='ONE'
        endif

C  get the time column string
        call uclgst('timecol',timecol,status)
        if (status .ne. 0) then
          context = 'could not get TIMECOL parameter'
          call fcerr(context)
          goto 999
        endif

C  get the columns
        call uclgst('columns',columns,status)
c        print*,'columns is ',columns
        if (status .ne. 0) then
          context = 'could not get COLUMNS parameter'
          call fcerr(context)
          goto 999
        endif
        if(columns.eq.'good')columns='GOOD'
        if(columns.eq.'error')columns='ERROR'
        if(columns.eq.'backest')columns='ERROR'
        if(columns.eq.'BACKEST')columns='ERROR'

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
     &     printmode='LIGHTCURVE'
        if(printmode.ne.'SPECTRUM'.and.printmode(1:1).eq.'S')
     &     printmode='SPECTRUM'
        if(printmode.ne.'BOTH'.and.printmode(1:1).eq.'B')
     &     printmode='BOTH'

        if(printmode.ne.'LIGHTCURVE'.and.
     &     printmode.ne.'SPECTRUM'.and.
     &     printmode.ne.'BOTH')printmode='BOTH'

C  get the lcmode
        call uclgst('lcmode',lcmode,status)
        call ftupch(lcmode)
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
        call ftupch(spmode)         
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
        
C  get the writesum
        call uclgst('writesum',writesum,status)
        if (status .ne. 0) then
          context = 'could not get WRITESUM parameter'
          call fcerr(context)
          goto 999
        endif
        if(writesum.eq.'INDEF'.or.
     &       writesum.eq.'indef'.or.writesum.eq.'-')writesum=' '
        
C  get the writemean
        call uclgst('writemean',writemean,status)
        if (status .ne. 0) then
          context = 'could not get WRITEMEAN parameter'
          call fcerr(context)
          goto 999
        endif
        if(writemean.eq.'INDEF'.or.
     &       writemean.eq.'indef'.or.writemean.eq.'-')writemean=' '
        
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

C  get the time intervals to 
        call uclgst('timeint',timeint,status)
        if (status .ne. 0) then
          context = 'could not get TIMEINT parameter'
          call fcerr(context)
          goto 999
        endif
        if(timeint.eq.'INDEF'.or.timeint.eq.'-'
     &     .or.timeint.eq.'indef'.or.timeint.eq.' ')timeint='0.0-0.0'
        
C  get the filename extension to put on each file
C       call uclgst('extnam',extnam,status)
C       if (status .ne. 0) then
c          context = 'could not get EXTNAME parameter'
c          call fcerr(context)
c          goto 999
C       endif
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
        if(chint.eq.'INDEF'.or.chint.eq.'indef')chint='0-2047'
        if(chint.eq.'-')chint='0-2047'
        if(chint.eq.'0-0')chint='0-2047'
        if(chint.eq.' ')chint='0-2047'
         
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

C get the ephemeris time start - EPHEM
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
C       call uclgst('outimecol',outimecol,status)
C       if (status .ne. 0) then
c          context = 'could not get OUTIMECOL parameter'
c          call fcerr(context)
c          goto 999
c       endif
        outimecol=' '
        
C  get the title for the output EVENT_SUM column
C       call uclgst('outcol',outcol,status)
C       if (status .ne. 0) then
c          context = 'could not get OUTCOL parameter'
c          call fcerr(context)
c          goto 999
C       endif
        outcol=' '

C  get whether to copy primary array and important keywords 
C       call uclgsb('copyprime',copyprime,status)
C       if (status .ne. 0) then
c          context = 'could not get copyprime parameter'
c          call fcerr(context)
c          goto 999
C       endif
        copyprime=.TRUE.
        
C  get whether to all other extensions to output file
C       call uclgsb('copyall', copyall,status)
C       if (status .ne. 0) then
c          context = 'could not get copyall parameter'
c          call fcerr(context)
c          goto 999
C       endif
        copyall=.FALSE.
        
C  get whether to be case sensitive
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
        if(negative.eq.'INCLUDE')negative='SUM'
        if(negative.eq.'EXCLUDE')negative='IGNORE'
        if(negative.ne.'IGNORE'.and.negative.ne.'SUM')then
          context='NEGATIVE defaulting to IGNORE'
          call fcecho(context)
          negative='IGNORE'
        endif

C  get whether to perform a DRYRUN on all files -
c  do not process any data but test all files and print a report
c  
        call uclgsb('dryrun', ldryrun,status)
        if (status .ne. 0) then
          context = 'could not get DRYRUN parameter'
          call fcerr(context)
          goto 999
        endif

C  get if the users wants to abort on any problem -
c     i.e. make the code dumb... 
        call uclgsb('bailout', lbailout,status)
        if (status .ne. 0) then
          context = 'could not get BAILOUT parameter'
          call fcerr(context)
          goto 999
        endif
        
c      Take care of all of the case sensitive issues... 

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
        
999     continue

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
C      fsacrv
c
c DESCRIPTION:
C      Parse parameter file information, create light curve, print info.
C      
c AUTHOR:
C      Brian K. Elza 4/96
c
c MODIFICATION HISTORY:
C      None, yet...
c
c NOTES:
C     
C      
c USAGE:
c        call fsacrv()
c
c
c ARGUMENTS:
C      infile     - input FITS file and extension number
C      gofile     - input GTI file name that is to be OR'ed with infiles
C      gafile     - input GTI file name that is to be AND'ed with infiles
C      outfile    - output file name
C      phasefil   - phase file name
C      timecol    - name of the time column
C      columns    - column names for binned parameter(s)
c      lcmode     - mode of operation for Lightcurve SUM, RATE, or MEAN
c      spmode     - mode of operation for Spectrum SUM, RATE, or MEAN
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
C      gsacrvfil   - reads in the binary files, processes the
c                   information
C
C
C
c**********************************************************************
        subroutine fsacrv(ichan,ngti,tmjds,tmjde,iisiz,tims,time,
     &   abort)
        implicit none
        integer isiz,nb,ngti,iisiz
        parameter (isiz = 999)
        parameter (nb = 512)

        character(80) objects(isiz)
        character(80) obstart,obstop

        double precision tmjds(*), tmjde(*), binsz,
     &     tims(isiz), time(isiz),mjdref

        character(160) files(isiz)
        integer no,cints(nb),cinte(nb),imjdref
        logical abort

        integer nnf,ne,ichan
        parameter (nnf = 10240)
        parameter (ne = 128)
c        parameter (naray=256*100)         

        integer itimno(2,nb),itimnoall,ichno,ipermno,iphaseno,i,j
        logical fstchar

        character(160) infile, outfile, gofile,gafile, phasefil,
     &     extenlc,extenpha
        character(80) timecol, columns, timeint, gticols, gtidate,
     &     gtitime, extnam, phaseint, mode, lcmode,spmode,
     &     accumulate,negative,commentlines(ne), chartemp,
     &     writesum,writemean,
     &     outimecol, outcol, obsdate, obstime,
     &     printmode, charall
        character(3000) chbin,chint
        character(20) cval
        double precision timemin, timemax, ephem, period, mfracexp,
     &     mlcinten,mspinten,holdtime, dtruc, timeresolution
        character(160) gapplyfiles(isiz)
        real tnull
        logical gettmin, gettmax, copyprime,clobber,
     &     ldryrun, copyall, sensecase, lmultiple, lbailout,
     &     getcmin, getcmax, getephem, chkit

        integer chmin, chmax, maxmiss, status, nogtiorfiles,
     &     noofcomments
C     &     outlin, fcstln
        integer bincntpt, binpt, ipernopt,
     &     realpt, arraypt, iarraypt, ichannel, irealchan,
     &     igtistrtsavlc, igtistpsavlc,
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
     &     timecol,columns,
     &     lcmode,spmode,
     &     accumulate,negative,extenpha,extenlc,
     &     writesum,writemean,timemin,timemax,timeint,gticols,
     &     gtidate,gtitime,extnam,chmin,chmax, chint,
     &     chbin,binsz,ephem,
     &     period, mfracexp,
     &     mlcinten,mspinten,clobber,ldryrun,
     &     phaseint,copyprime,copyall,sensecase,mode,gettmin,gettmax,
     &     getcmin,getcmax,getephem,chkit,printmode,outcol,outimecol,
     &     obsdate,obstime

        common/gtiorfiles/gapplyfiles,nogtiorfiles
        common/miss/maxmiss

        common/nullval/tnull
        common/comments/commentlines,noofcomments
        common/multiple/lmultiple
        common/bail/lbailout
        common/timeres/timeresolution
        
        holdtime=0.0d0
        status=0
        cval=' '
        charall=' '
        noofcomments=0
        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c      We have to allocate dynamic memory which we will use in
c      this program
c      Remembering, of course, to initialize the pointers... MJT 29Jan2002

        bincntpt = 0
        ipernopt = 0
        binpt = 0
        realpt = 0
        arraypt = 0
        iarraypt = 0
        irealchan = 0
        ichannel = 0
        igtistrtsavlc = 0
        igtistpsavlc = 0
        igtistart = 0
        igtistop = 0

c      Allocate memory for DBINCOUNT
        call udmget (ichan,7,bincntpt,status)

        if(status.ne.0)then
          call fcecho('Error allocating memory for DBINCOUNT.')
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

c      Allocate memory for DARRAY
        call udmget(ichan,7,arraypt,status)

        if(status.ne.0)then
          call fcecho('Error allocating memory for DARRAY')
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

        call udmget(nnf,7,irealchan,status)

        if(status.ne.0)then
          call fcecho('Error allocating memory for REALCHAN')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

        call udmget(nnf,7,ichannel,status)
        if(status.ne.0)then
          call fcecho('Error allocating memory for DCHANNEL')
          call fcecho('Free up some memory, by closing windows')
          call fcecho('or terminating background jobs.')
          call fcecho('Then try running this again.')
          call fcerrm(status)
          status=0
          return
        endif

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
        
c      Check to see if the first character in the file is a '@' and
c      if so then put that information into the 'files' array which
c      has a maximum of 100...

c        print*,'About to go into chkfile...'

        if(chkit)then
          call chkfile(infile,files,chkit,no,status,abort)
        else
          call fcgcls(infile,files,no,abort)
        endif

        if(no.gt.ISIZ)then
          call fcecho(' ')
          call fcecho('Too many files being input!')
          call fcecho('You will have to break up and input')
          call fcecho('no more than 999 files at a time.')
          call fcecho('Cannot continue... Aborting...')
          stop
        endif
        
c      When checking the files we see if they all contain scientific
c      array data, which is the only type of FITS file that this TOOL
c      operates on. If any of the files are not the correct type
c      the program will abort.
        
        if(abort)then
          call fcecho(' ')
          call fcecho('Out of CHKFILE or Fcfcls with abort code.')
          call fcecho('There is a problem with your input or codes.')
          call fcecho('Aborting...')
          goto 999
        endif
        
        if(ldryrun)then
          call fcecho(' ')
          call fcecho('Input file from the parameter file was:')
          call fcecho(infile)
          call fcecho('**************************************')
          call fti2c(no,cval,status)
          call fcecho('The total number of files found were:')
          call fcecho(cval)
          call fcecho('The names of the files to be processed:')
          call fcecho('--------------------------------------')
          do 104 i=1,no
            call fcecho(files(i))
104       continue
          call fcecho('--------------------------------------')        
        endif

c        print*,'Out of chkfile...',chkit

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

c      print*,'Going into CHKTIME'
c      print*,'NO is ',no
c      do i=1,no
c        print*,files(i)
c      enddo
      
        call chktime(files,objects,obstart,obstop,obsdate,
     &       mjdref,imjdref,tmjds,tmjde,no,status)

c       Check to see that all files passed BARYTIME check. If any failed
c choke and die... 
        if(status.eq.9999)then
          call fcecho(' ')
          call fcecho('An Error occurred in CHKTIME.')
          call fcecho('Cannot continue - aborting.')
          status=0
          abort=.TRUE.
          goto 999
        endif
        
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('An Error occurred in CHKTIME.')
          call fcecho('Cannot continue - aborting.')
          abort=.TRUE.
          goto 999
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
        
c      Store the start and stop times associated with each file into
c      an array so that later we can determine the number of bins of
c      size binsz there are in each file. This will also allow us to
c      determine an offset value so that we do not assign array space to
c      account for time intervals which do not exist in any of the input
c      files. These offsets will be stored in the array DOFFSET
        
        do 10 i=1,no
          tims(i)=tmjds(i)
          time(i)=tmjde(i)
          holdtime=holdtime+(time(i)-tims(i))
c          print*,'tims and time is',i,tims(i),time(i)
10      continue
        
c      print*,'Out of chktime'

c      While TMJD 's' and 'e' up to this point define the starting and
c      stopping point of the times included in each file, at this point
c      these arrays take on the GTI's and define the start and stop times
c      in each. For simplicity we will allow them to span all of the files
c      at this point... 

        call gtiminfo(gettmin,gettmax,timemin,timemax,timeint,
     &     tmjds,tmjde,itimno,itimnoall,files,no,gofile,
     &     gafile,gticols,
     &     gtidate,gtitime,obstart,obstop,obsdate,ipermno)

        call fcecho(' ')
        call fcecho('All time-filtering criteria has been processed.')
        call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
        call fcecho('GTIORFILE filtering time-ranges are:')

        do i=1,no
          call fcecho(' ')
          charall=' '
          cval=' '
          call fti2c(i,cval,status)
          charall(1:17)='For file number: '
          charall(18:38)=cval(1:20)
          call fcecho(charall)

c          print*,'itimno(1,i) and itimno(2,i)',i,itimno(1,i),itimno(2,i)
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
        
c        print*,'TIMEMIN and TIMEMAX are',timemin,timemax

c      Set timemin and timemax if they haven't been set in the par files.
c      Let's take care of what we can for erroneous input parameters
c      and those with INDEF supplied as the value.
        
c      If timemin is not set or wrong than set it to the earliest time.
c      Note that timemin and timemax are given in MJD
        if(gettmin.and.timemin.lt.0.0d0)then
           timemin=tmjds(1)
        endif
        
c      If timemax is not set or wrong than set it to the latest time.
        if(gettmax.and.timemax.lt.0.0d0.or.
     &       timemax.gt.1.0d+35)then
           timemax=tmjde(no)
        endif
        
c      If the ephemeris is not set or wrong set it to the earliest time.
        if(getephem.and.ephem.lt.0.0)then
           ephem=timemin
        endif
        
c        print*,'itimnoall and ipermno is ',itimnoall,ipermno
c        print*,'timemin and timemax',timemin,timemax,no,
c     &     tmjde(no),gettmax,gettmin
c        do i = 1,itimnoall+ipermno
c          print*,'Tmjds and tmjde is ',i,tmjds(i),tmjde(i)
c        enddo
        
c        print*,'tmjds is ',(tmjds(i),i=1,itimnoall)
c        print*,'tmjde is ',(tmjde(i),i=1,itimnoall)      
c        print*,'period and phaseint are',period,phaseint

        
        if(((period.gt.0.0d0).or.
     &     (phasefil.ne.' '.and.phasefil.ne.'INDEF'.and.
     &       phasefil.ne.'indef')))then
          call gphaseinfo(timemin,timemax,mjdref,imjdref,
     &       ephem,period,
     &       phaseint,binsz,phasefil,tmjds,tmjde,itimnoall,
     &       ipermno,iphaseno)
        else
          ephem=timemin
          tmjds(itimnoall+ipermno+1)=0.0d0
          tmjde(itimnoall+ipermno+1)=1.0d0
          iphaseno=1
        endif

c        print*,'ipermno and iphaseno is ',ipermno,iphaseno,
c     &     ephem,period
c        do k=1,iphaseno
c          print*,'tmjds and tmjde are',k,
c     &       tmjds(itimnoall+ipermno+k),tmjde(itimnoall+ipermno+k)
c        enddo
          
        
c      If the bin-size is either not set or set equal to zero
c      then set it to the default binsize of timemax-timemin/100.
c      This value may be changed if the bin-size is smaller than
c     the most accurate time resolution for the file being processed...
        if(lmultiple)then
          if(binsz.le.0.0d0)then
            call fcecho(' ')
            call fcecho('****ERROR****')
            call fcecho('You specified that each light-bin')
            call fcecho('is to be an even multiple of,')
            call fcecho('of the minimum time resolution')
            call fcecho('but you did not set a BINSZ!!!')
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
              else
                call fcecho('ABORTING!!!')
                abort=.TRUE.
                goto 999
              endif
            else

            endif
          endif
        else
          if(binsz.le.0.0d0)then
            binsz=((timemax-timemin)/100.0d0)

c         If there are large gaps in the data BINSZ may be very large
c so test it to see if it is larger than if you take the total time
c covered by all files divided by 100. If it is then redefine it.
            if(binsz.gt.(holdtime/100.0d0))binsz=holdtime/100.0d0
            call fcecho(' ')
            call ftd2f(binsz,8,cval,status)
            call fcecho('Binsz was not specified!')
            call fcecho('Setting BINSZ parameter to ')
            call fcecho(cval)
          endif
        endif

c        print*,'The binsize is ', binsz,timemin,timemax
c        print*,'files(1),outfile,objects,no'
c        print*,files(1),outfile,objects(1),no
c        print*,'timemin and timemax is '
c     &       ,timemin,timemax
c        print*,'totals... ',tmjds(1),tmjde(no),timemin,timemax,binsz
c        print*,'CHINT value is ',chint

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
        commentlines(noofcomments)=chartemp

        noofcomments=noofcomments+1
        chartemp='CHMAX for filtering was '
        cval=' '
        call fti2c(chmax,cval,status)
        chartemp(25:45)=cval(1:20)
        commentlines( noofcomments)=chartemp
        status=0
        
        
c        print*,'bincntpt is ',bincntpt,status,ichan
c        print*,'ichno is ',ichno
c        print*,'cints is ',(cints(i),i=1,ichno)
c        print*,'cinte is ',(cinte(i),i=1,ichno)      
c        print*,'totals... ',tmjds(1),tmjde(no),timemin,timemax,binsz

c        print*,'bincntpt,ipernopt,binpt,realpt,arraypt,
c     $     iarraypt, irealchan, ichannel, igtistrtsavlc,
c     $     igtistpsavlc, igtistart, igtistop'
        
c        print*,bincntpt,ipernopt,binpt,realpt,arraypt,
c     $     iarraypt, irealchan, ichannel, igtistrtsavlc,
c     $     igtistpsavlc, igtistart, igtistop
        
        call gsacrvfil(files,outfile,clobber,
     &     ldryrun,lcmode,spmode,
     &     accumulate,negative,extenpha,extenlc,
     &     writesum,writemean,printmode,
     &     timecol,columns,no,timemin,timemax,itimno,itimnoall,
     &     tmjds,tmjde,ephem,period, mfracexp, tnull,
     &     mlcinten,mspinten,binsz,ichno,cints,
     &     cinte,chbin,gettmin,gettmax,
     &     ipermno,iphaseno,obstart,obstop,obsdate,
     &     time,tims,1000+ichan,meml(iarraypt),
     &     ichan,memd(bincntpt),memd(ipernopt),
     &     memd(binpt),memd(realpt),memd(arraypt),
     &     nnf,memd(irealchan),memd(ichannel),
     &     ngti,memd(igtistrtsavlc),memd(igtistpsavlc),
     &     memd(igtistart),memd(igtistop),
     &     sensecase,status)

c        print*,'bincntpt,ipernopt,binpt,realpt,arraypt,
c     $     iarraypt, irealchan, ichannel, igtistrtsavlc,
c     $     igtistpsavlc, igtistart, igtistop'
        
c        print*,bincntpt,ipernopt,binpt,realpt,arraypt,
c     $     iarraypt, irealchan, ichannel, igtistrtsavlc,
c     $     igtistpsavlc, igtistart, igtistop
        
        
c        print*,'Out of gsacrvfil'
        status=0
        
        call udmfre(bincntpt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for DBINCOUNT')
           call fcerrm(status)
           status=0
        endif
c        print*,'Freed dbincount'
        
        call udmfre(ipernopt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for DPERNOARRAY')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed dpernoarray'
        
        call udmfre(binpt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for DBIN')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed binpt'
        
        call udmfre(realpt,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for REALBIN')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed realbin'
        
        call udmfre(arraypt,7,status)

        if(status.ne.0)then
           call fcecho('Error freeing memory for DARRAY')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed darray'
        
        call udmfre(iarraypt,4,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for IARRAY')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed iarray'
        
        call udmfre(irealchan,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for REALCHAN')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed realchan'

        call udmfre(ichannel,7,status)
        if(status.ne.0)then
           call fcecho('Error freeing memory for DCHANNEL')
           call fcerrm(status)
           status=0
        endif
c        print*,'freed dchannel'

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

        
999     continue
        
        return
        end


c****************************************************************************
c      
c      gsacrvfil
c      
c      DESCRIPTION:
c      Gets the information necessary to begin processing files.
c      We will need to acquire all of the information about how the
c      data is stored in the FITS files. This routine does not
c      actually process any of the files, but simply gets all of the
c      information stored in the header which will be used in reading
c      in and processing the information. 
        
C      INPUT PARAMETERS
C      iunit   i  Fortran i/o unit number
c      outfile c  Outfile
c      lcmode  c  Mode to use in creating the SPECTRUM out file
c      (SUM, RATE, MEAN)
c      spmode  c  Mode to use in creating the SPECTRUM out file
c      (SUM, RATE, MEAN)
c      timecol c  Character string used for TIME header.
c      columns c  Character string used for count header.
c      no      i  Number of input files read from *.TMP file.
c      timemin d  Minimum time of first observation in all files.
c      timemax d  Maximum time of last observation in all files.
c      
C      MAJOR LOCAL VARIABLES:
C      ncols   i  number of columns in the table
C      nrows   i  number of rows in the table
C      nfield  i  number of fields in the table
C      ttype   c  name of each field (array)
C      tbcol   i  beginning column of each field (array)
C      tform   c  Fortran-77 format of each field (array)
C      tunit   c  units of each field (array)
C      extnam  c  name of table (optional)
C      status  i  returned error status (0=ok)
c      cpix    s  string giving channel boundaries for each column 
c      
C      OUTPUT PARAMETERS:
c      colpost i  column number which has timecol as the header (array)
c      colpos  i  column number which has columns as the header (array)
c      nfield  i  number of columns in the table (table setup)
c      
c      CALLED ROUTINES:
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
c**********************************************************************
        
        subroutine gsacrvfil(files,outfile,clobber,
     &   ldryrun,lcmode,spmode,
     &   accumulate,negative,extenpha,extenlc,
     &   writesum,writemean,printmode,
     &   timecol,columns,no,timemin,timemax,itimno,itimnoall,
     &   tmjds,tmjde,ephem,period, mfracexp, tnull,
     &   mlcinten,mspinten,binsz,ichno,cints,
     &   cinte,chbin,gettmin,gettmax,
     &   ipermno,iphaseno,obstart,obstop,obsdate,
     &   time,tims,jichan,iarray,
     &   ichan,dbincount,dpernoarray,dbin,realbin,
     &   darray,jnf,realchan,dchannel,
     &   ngti,dgstrtsavlc,dgstpsavlc,dgtistart,dgtistop,
     &   sensecase,status)        
        
        implicit none
        integer nf,nb,ne,isiz,ichan,isml,ngti,
     &     naray,ikey2,ikey3,ikey4,ikey5,
     &     jichan,icols,jnf,itcycle
        
c      Define all of the common parameters used in the arrays.
        parameter (isml=9)
c        parameter (naray=256*100) 
        parameter (nb = 512)
c        parameter (ngti = 12800)
        parameter (nf = 10240)
        parameter (ne = 1024)
        parameter (isiz = 999)
c        parameter (ichan = 100000)
        parameter (icols=40)

        parameter (itcycle=10000)
        parameter (ikey2=19)
        parameter (ikey3=3)
        parameter (ikey4=7)
        parameter (ikey5=6)
        
        double precision darray(*)
        integer iunit,nrows,nfield,pcount,status,
     &     ifile,itimno(2,nb),itimnoall,ichno,inull(nb)
        character*(*) files(isiz),timecol,columns,
     &     outfile,lcmode,spmode,chbin,
     &     accumulate,negative,extenpha,extenlc,
     &     writesum,writemean,printmode,obstart,obstop,obsdate
        character(1) cnum(10)
        character(8) keyword2(ikey2),keyword3(ikey3),keyword4(ikey4),
     &     keyword
        character(80) cols(icols),cscols(icols),cwrite(2*ikey2),
     &     cstcols(icols), colstemp(2*icols)
        character(40) ttype(nb),tform(nb),tunit(nb),extnam,
     &     contxt,timeunit,cwrites(ikey2),cwritem(ikey2)

        character(160) file1
        
        double precision dchannel(nf),dbin(*),dbincount(*)
        integer no,block,naxis(icols),naxes(isml,icols),naxes1(isml),
     &     extnum,cints(nb),cinte(nb),jcolpos(icols),
     &     elem,lrow,inum,inum2,icount(icols),
     &     icountfix,icountfixtemp,iarray(*),icounttotal,
     &     iwrite(2*ikey2),iwrt,inul(nb),
     &     iwrites(ikey2),iwritem(ikey2),iwrts,iwrtm,
     &     colpost,i,j,k,l,xtend,itemp,ichn,ifelem,inum1,
     &     ounit,spounit,lcounit,colval,colvalhold,tdtype,trcount,
     &     ichbins(2*ne),ichbine(2*ne),ichbinum,
     &     icstore(icols),imin,imax

        integer dtype(nb),rcount(nb),width,ilbd(ne),iubd(ne),
     &     ilbds(ne,icols),iubds(ne,icols),
     &     islbd(ne,icols),isubd(ne,icols),inobd(icols),
     &     isnobd(icols)
        character(80) ctyp(isml),cuni(isml),comm,comn
        character(3000) cpix(isml),cspix(icols),cpixold

        character(80) tddes(isml)
        double precision realbin(*),
     &     realchan(nf), timepha(nf),
     &     cinc(isml,icols),tinc(isml,icols),
     &     cdlt(isml),timeval,tims(isiz),time(isiz),
     &     dcyclestart(isiz),dcycleend(isiz)
        
        integer nulval,felem,ielem(icols),nelem(icols),
     &     ileft(icols),nleft,eincr,irun(icols),
     &     icolfset,
     &     nultyp,iword,iperiod,iaccum,
     &     ikeyval(ikey3),itimpos(icols),icolpos(icols),
     &     iper,itimecycle,icycles,
     &     irowsave,icyclestart
        integer illimit(icols),iulimit(icols),ipernoval,
     &     itofst(icols),ifmfst(icols),itoend(icols),icol,
     &     ipermno,itime,iphaseno,igti,iconoffset,
     &     iconoffsettotal,icolfsettotal(itcycle)

        double precision dtotevents(icols),devents(icols),
     &     dtotgood(icols),dgood(icols),dtotbad(icols),dtotval,
     &     dbad(icols),dttot(icols),dtim(icols),dtotphase(icols),
     &     dphase(icols),dtotpha(icols),dpha(icols),dthres(icols),
     &     dlcinten(icols),dspinten(icols),dbitmask(icols)

        integer jnum,kchanall,
     &     inocols,isnocols
        
        character(80) charall
        character(20) cval,curvetype,cvalhold
        
        double precision timearay,totsecs,totsecsa(icols),totsecp,
     &     nulvald,timemin,timemax,binsz,pstart,pstop,totsecst,
     &     tmjds(*),tmjde(*),ephem,period,mfracexp,totsecsp(icols),
     &     mlcinten,mspinten,timelower(itcycle),timeupper(itcycle),
     &     keyval(isml),dremain,dlta,dgstrtsavlc(*),dgstpsavlc(*),
     &     conversion,small,large,dgtistart(*),dgtistop(*),mjdref,
     &     timezero,timezerof,dperstart,dtruc,dbegin
        logical anynul,flgval,equibin,exact,lastf,timec,onedim(icols),
     &     nonxte,lor,land,pand,cand,logi,lwrite(nb),chkit,abort,
     &     clobber,ldryrun,lext,lbins,lcbin,lspbin,lidentical,lmany,
     &     lstatus,llast,lwritegti,sensecase,gettmin,gettmax,lfalse
     &     ,ldata,lgoodcols,lerrorcols,lmultiple
        
        real rval, rstor(nb),tnull,nulvale
        integer j1,ic,ic1,ic2,kchannum,j2,isav,itimezero,
     &     idim
        double precision dpernoval, dper,
     &     doffsettotal(itcycle,icols),doffset(2,isiz),
     &     dpernoarray(*),dchanlower(itcycle),dchan,dchanall,
     &     dpernosave,dtemp,dtempall,dchansum

c      Variables that are associated with storing variables that are
c      used in dealing with with all of the files.

        integer icolfsetarray(icols),iconoffsetarray(icols),
     &     iswrite(2*ikey2),iwrtstore,isave,outlen,fcstln
        character(80) cswrite(2*ikey2)
        integer imjdref,iprint, imiss(icols), maxmiss

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
        data (keyword3(i),i=1,ikey3)/'EXTVER','EXTLEVEL','APPID'/
        data (keyword4(i),i=1,ikey4)/'TSTART','TSTOP','MJDREF',
     &     'RA_PNT','DEC_PNT','EQUINOX','TIMEDEL'/

        common/miss/maxmiss
        common/multiple/lmultiple
        
c      print*,'NNNtimemin and timemax are',timemin,timemax,binsz
c      print*,'lcmode,ichno,ipermno,itimnoall is',
c      &       lcmode,ichno,ipermno,itimnoall
c      print*,'jaray and naray is ',jaray,naray
c      print*,'ichan is ',ichan,ngti

c        print*,'TNULL is ',tnull

c        call dinitial(isiz,dcyclestart)
c        call dinitial(isiz,dcycleend)

c        call dinitial(nb,dgstrtsavlc)
c        call dinitial(nb,dgstpsavlc)
c        call dinitial(nb,dgtistart)
c        call dinitial(nb,dgtistop)

        naray=ichan
        imin=0
        imax=0
        lcounit=0
        spounit=0
        idim=1
        dpernosave=0.0d0
        ipernoval=0
        dchansum=0.0d0
        icountfix=0
        icountfixtemp=0
        icounttotal=0
        keyword=' '
        lwritegti=.FALSE.
        ldata=.FALSE.
        lgoodcols=.FALSE.
        lerrorcols=.FALSE.
        if(sensecase)exact=.TRUE.
        if(.not.sensecase)exact=.FALSE.
        lcbin=.FALSE.
        lmany=.FALSE.
        lspbin=.FALSE.
        lidentical=.FALSE.
        timec=.FALSE.
        lastf=.FALSE.
        nonxte=.FALSE.
        lor=.FALSE.
        land=.FALSE.
        pand=.FALSE.
        cand=.FALSE.
        chkit=.FALSE.
        abort=.FALSE.
        lext=.FALSE.
        lstatus=.FALSE.
        icolfset=0
        iconoffset=0
        iconoffsettotal=0
        igti=0
        status=0
        inocols=1
        iwrt=0
        iwrts=0
        iwrtm=0
        isave=1
        llast=.FALSE.
        itimecycle=0
        icycles=1
        dlta=1.0d-8
        dlta=0.0d0
        isav=0
        irowsave=0
        timezero=0.0d0
        timezerof=0.0d0
        itimezero=0
        itime=0
        small=0.0d0
        large=0.0d0
        dpernoval=0.0d0
        iwrtstore=0

        call dinitial(ngti,dgstrtsavlc)
        call dinitial(ngti,dgstpsavlc)
        call dinitial(ngti,dgtistart)
        call dinitial(ngti,dgtistop)
        
        do j=1,isiz
          doffset(1,j)=0.0d0
          doffset(2,j)=0.0d0
          dcyclestart(j)=1.0d0
          dcycleend(j)=1.0d0
        enddo
        
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
          
          do j=1,2*ne
            ichbins(j)=j-1
            ichbine(j)=j-1
          enddo
          
          ichbinum=2*ne
          
        endif
        
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
        do ic=1,2*icols
          colstemp(ic)=' '
        enddo

        call dinitial(itcycle,timelower)
        call dinitial(itcycle,timeupper)
        call dinitial(itcycle,dchanlower)
        
        timelower(1)=timemin
        timeupper(1)=timemax

        call dinitial(nf,dchannel)
        call dinitial(nf,realchan)
        call dinitial(nf,timepha)
        
c      print*,'writesum and writemean',writesum,writemean
        call fcgcls(writesum,cwrites,iwrts,logi)
        call fcgcls(writemean,cwritem,iwrtm,logi)
        
c      print*,'writemean,writesum',writemean,writesum
c      print*,'cwrites',iwrts,(cwrites(j),j=1,iwrts)
c      print*,'cwritem',iwrtm,(cwritem(j),j=1,iwrtm)


c1       continue        

        totsecs=0.0d0
        totsecp=0.0d0
        call dinitial(icols,totsecsa)
        call dinitial(icols,totsecsp)

c        print*,'totsecsp(1) is ',totsecsp(1)
        
        dtotval=0.0d0

        do ic=1,icols
            
c      Set up unit number for reading in values. This really isn't
c      necessary since we successively open and close each file, but
          icount(ic)=0
          icolfsetarray(ic)=0
          iconoffsetarray(ic)=0
          cspix(ic)=' '

          dtotevents(ic)=0.0d0
          devents(ic)=0.0d0
          dtotgood(ic)=0.0d0
          dgood(ic)=0.0d0
          dlcinten(ic)=0.0d0
          dspinten(ic)=0.0d0
          dtotbad(ic)=0.0d0
          dbad(ic)=0.0d0
          dttot(ic)=0.0d0
          dtim(ic)=0.0d0
          dtotphase(ic)=0.0d0
          dphase(ic)=0.0d0
          dbitmask(ic)=0.0d0
          dtotpha(ic)=0.0d0
          dpha(ic)=0.0d0
          dthres(ic)=0.0d0

          do i=1,itcycle
            icolfsettotal(i)=0
            doffsettotal(i,ic)=0.0d0
          enddo

c         MJT 08 Apr 2010
          inobd(ic)=0
        enddo
        
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      We are setting up the primary loop that will cycle over all of
c      the files that were given as input. The file number that is
c      being processed is in 'ifile' and in the 10 loop.
        
        itemp=0

        call fcecho(' ')
        call fcecho('###########################################')
        call fcecho(' ')

1       continue

        totsecst=0.0d0
        icountfix=0

        call dinitial(ichan,realbin)
        call dinitial(ichan,dbin)
        call dinitial(ichan,dbincount)
        call dinitial(ichan,dpernoarray)

        do j=1,jichan
          iarray(j)=0
        enddo
        
        do ic=1,icols
          icount(ic)=0
          icolfsetarray(ic)=0
c          iconoffsetarray(ic)=0
          do i=1,itcycle
            doffsettotal(i,ic)=0
          enddo
        enddo
        
1000    continue
        
        do 10 ifile=1,no

          charall=' '
          cpixold=' '
          
          do 13 iword=1,isml
            ctyp(iword)='$'
            cpix(iword)='$'
            cuni(iword)='$'
            cdlt(iword)=0.0d0

            do 113 ic=1,icols
              naxes(iword,ic)=0
              naxis(ic)=0
              icolpos(ic)=0
              itimpos(ic)=0
c              totsecsa(ic)=0.0d0
c              totsecsp(ic)=0.0d0
              cinc(iword,ic)=0.0d0
              tinc(iword,ic)=0.0d0
              jcolpos(ic)=0

              devents(ic)=0.0d0
              dgood(ic)=0.0d0
              dbad(ic)=0.0d0
              dtim(ic)=0.0d0
              dphase(ic)=0.0d0
              dpha(ic)=0.0d0
              dbitmask(ic)=0.0d0
113         continue

13        continue
            
          if(i.eq.no)lastf=.TRUE.
          
          do 11 k=1,nb
            inull(k)=-1
            inul(k)=-1
            lwrite(k)=.FALSE.
11        continue

c**********************************************************************
          
c      Parse the character input file name 
          call fcpars(files(ifile),file1,extnum,status)
            
c      Print out any error information about accessing the files
          if (status.ne.0)then
            call fcerrm(status)
            call fcecho(' ')
            call fcecho('Error in parsing input files.')
            call fcecho('Cannot continue... Aborting.')
            return
          endif

c          print*,'COlval and inocols is ',colval,inocols
c          print*,'iconoffsetarray is ',iconoffsetarray
c          print*,'lext is ',lext
            
          call fcecho(' ')
          call fcecho('Processing file ')
          call fcecho(file1)

c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 1 we
c      force it to 1. 
          if (extnum.lt.1) extnum=1
            
c      Open the file that is of interest. Note that files have
c      been sorted in hktime according to time of observation.
          call ftopen(iunit,file1,0,block,status)
          if(status.ne.0)then
            call fcerr('Failure to open input file - aborting')
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
          if (status.ne.0)then
            call fcerrm(status)
            call fcecho(' ')
            call fcecho('Error accessing input files ')
            call fcecho('Aborting...')
            status=0
            return
          endif
            
c      Skip the primary header and go to the second (or extnum)
c      to read all pertinent processing information.
          call ftmahd(iunit,extnum+1,xtend,status)
          if(status.ne.0)then
            call fcerr('Error moving to extnum')
            call fcerrm(status)
            call fcecho('Aborting...')
            status=0
            return
          endif
            
          call ftgkys(iunit,'TIMEUNIT',timeunit,contxt,status)
          if(status.ne.0)then
            call fcecho('Could not find keyword TIMEUNIT')
            call fcecho('Assuming all inputs in SECONDS')
            timeunit='s'
            status=0
          endif

          call timeconvert(timeunit,conversion,status)
          if(status.ne.0)then
            call fcecho('Could not determine TIMEUNIT conversion')
            call fcecho('Assuming all inputs are in seconds')
            conversion=1.0d0
            status=0
          endif

c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
          call ftghbn(iunit,nb,nrows,nfield,ttype,
     &       tform,tunit,extnam,pcount,status)

          if(nrows.eq.0)then
            call fcecho(' ')
            call fcecho('This file contains no information under')
            call fcecho('the COLUMNS name or file is empty')
            call fcecho('Aborting...')
            return
          endif
            
c      Print out any error information about accessing the files
          if (status.ne.0)then
            call fcecho('Could not get FIELDS information')
            call fcerrm(status)
            call fcecho('Aborting...')
            status=0
            return
          endif


c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c         Since the following section offset by %'s deals with accumulating
c       information that will be written into the header of the PHA file
c       we only need to accumulate it once and store it until it is needed.
c       So after the first cycle for storing the information we won't do
c       it again, so we jump around it. 
          
          if(icycles.gt.1)goto 8051
          
c      This section set apart by the use of % is were we search
c      for additional column names that are to be summed or the mean 
c      calculated and placed into the header as a keyword of the same
c      name in the PHA file.

          if(iwrts.ge.1)then
            do 800 j=1,iwrts
              call ftgcno(iunit,exact,cwrites(j),iwrites(j),status)
              if (status .ne. 0) then
                contxt = 'Could not find WRITESUM column'
                call fcecho(contxt)
                call fcecho(cwrites(j))
                call fcecho('Continuing without that parameter')
                status=0
              endif
800         continue
          
            do 802 j=1,iwrts
              cwrite(j)=cwrites(j)
              iwrite(j)=iwrites(j)
802         continue

          endif
          
          if(iwrtm.ge.1)then
            do 801 j=1,iwrtm
              call ftgcno(iunit,exact,cwritem(j),iwritem(j),status)
              if (status .ne. 0) then
                contxt = 'Could not find WRITEMEAN column'
                call fcecho(contxt)
                call fcecho(cwrites(j))
                call fcecho('Continuing without that parameter')
                status=0
              endif            
801         continue
            
            do 803 j=1,iwrtm
              cwrite(iwrts+j)=cwritem(j)
              iwrite(iwrts+j)=iwritem(j)
803         continue

          endif
          
          iwrt=iwrts+iwrtm

          if(iwrt.ge.1)then 
            if(ifile.eq.1.and.icycles.eq.1)then

              call sortit(cwrite,iwrite,iwrt)
              iwrtstore=iwrt

              do 804 j=1,iwrt
                cswrite(j)=' '
                cswrite(j)=cwrite(j)
                iswrite(j)=iwrite(j)
804           continue
            
            else

              if(iwrt.ne.iwrtstore)then
                call fcecho('Different number of keywords')
              endif
            
              if(iswrite(j).ne.iwrite(j))then
                call fcecho('Keywords are in different order')
              endif

            endif
          
            do 805 j=1,iwrt
              keyword(6:8)=' '
              icol=6   
              if(iwrite(j).lt.10)then
                keyword(icol:icol)=cnum(iwrite(j)+1)
              elseif (iwrite(j).lt.100)then
                keyword(icol:icol)=cnum(iwrite(j)/10+1)
                keyword(icol+1:icol+1)=cnum(mod(iwrite(j),10)+1)
              elseif (iwrite(j).lt.1000)then
                keyword(icol:icol)=cnum(iwrite(j)/100+1)
                keyword(icol+1:icol+1)=cnum((mod(iwrite(j),100)/10)+1)
                keyword(icol+2:icol+2)=cnum(mod(mod(iwrite(j),100),10)
     &             +1)
              elseif (iwrite(j).ge.1000)then
                comm='ERROR column number greater than 999'
                call fcerr(comm)
              endif
            
              call ftbnfm(tform(iwrite(j)),dtype(j),rcount(j)
     &           ,width,status)
              keyword(1:5)='TNULL'
              call ftgkyj(iunit,keyword,inul(j),comm,status)
              
              if(status.ne.0)inul(j)=-999
              status=0

805         continue

          endif
          
8051      continue
          
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c      Now that we have finished all of the extra information we
c      we begin our processing of the information about how the
c      data we are interested in is stored.
          
c      Find out the column number that is associated with the
c      time column (timecol) and counts column (columns) in the next
c      two subroutine calls


c      Find the postition of the TIMESTAMP column and store it in
c      colpost
          call ftgcno(iunit,exact,timecol,colpost,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not find TIME column number!')
            call fcecho('Try setting sensecase to NO and/or')
            call fcecho('the spelling of your time column name!')
            call fcecho('Check input data. Aborting.')
            status=0
            abort=.TRUE.
            goto 999
          endif

c     Let's check to see how long the COLUMNS input value is. If it
c     is 80 characters (the maximum) then we are probably missing some
c     characters in a column name.
          outlen=fcstln(columns)
          if(outlen.ge.80)then
            call fcecho(' ')
            call fcecho('***ERROR***')
            call fcecho('COLUMNS parameter may be too long !')
            call fcecho('Create a file containing the names of each')
            call fcecho('column with one name per line. Input that')
            call fcecho('file at the COLUMNS prompt via @filename,')
            call fcecho('where FILENAME is the name of the file')
            call fcecho('which contains the list of column names.')
            call fcecho(' ')
            call fcecho('****Aborting****')
            abort=.TRUE.
            goto 999
          endif

c      Let's see how many column names are stored in the COLUMN input
c      parament and store their names in the array 'cols' and the number
c      of them in 'inocols'.

          if(ifile.eq.1.and.icycles.eq.1)then
            call fcgcls(columns,cols,inocols,abort)

            if(.not.ldryrun.and.chbin.eq.'INDEF'
     &         .and.(inocols.gt.1.or.(cols(1).eq.'GOOD'.or.
     &         cols(1).eq.'ERROR')))then
              call fcecho(' ')
              call fcecho('DRYRUN set to FALSE and CHBIN set to INDEF')
              call fcecho('Errors in column formats or in files')
              call fcecho('can cause unexpected errors !!!')
            endif
            
          endif

          if(cols(1).eq.'GOOD')lgoodcols=.TRUE.
          if(cols(1).eq.'ERROR')lerrorcols=.TRUE.
          
          if(ifile.eq.1.and.icycles.eq.1.and.
     &       inocols.eq.1.and.(cols(1).eq.'GOOD'.or.
     &       cols(1).eq.'ERROR'))then
            
            call goodcols(iunit,nfield,ttype,colstemp,
     &         inocols,ldryrun,abort)

            if((colstemp(1).ne.'GOOD').and.
     &         (colstemp(1).ne.'ERROR'))then

              ic1=0
              ic2=0
              
              do ic=1,inocols
                cscols(ic)=' '
                cstcols(ic)=' '
                comm=' '
                comm=colstemp(ic)
                outlen=fcstln(comm)
                if(lerrorcols)then
                  if(outlen.gt.4)then
                    do j2=1,outlen-3
                      if(comm(j2:j2+3).eq.'Errs')then
                        ic1=ic1+1
                        cscols(ic1)=colstemp(ic)
                      endif
                    enddo
                  endif
                else
                  if(outlen.gt.4)then
                    do j2=1,outlen-3
                      if(comm(j2:j2+3).eq.'Errs')then
                        ic2=ic2+1
                        cstcols(ic2)=colstemp(ic)
                      endif
                    enddo
                  endif
                  if(cstcols(ic2).ne.colstemp(ic))then
                    ic1=ic1+1
                    cscols(ic1)=colstemp(ic)
                  endif
                endif
              enddo  

              inocols=ic1
              do ic=1,inocols
                cols(ic)=cscols(ic)
                cscols(ic)=' '
              enddo
              
              call fcecho(' ')
              call fcecho('The columns that passed acceptance')
              call fcecho('criteria were:')
              call fcecho(' ')

              do ic=1,inocols
                call fcecho(cols(ic))
              enddo
              
              if(inocols.gt.icols)then
                call fcecho(' ')
                call fcecho('    ****** ERROR ******      ')
                call fcecho('More than 40 columns were found!')
                call fcecho(' ')
                call fcecho('You much set icols=40 to a higher value')
                call fcecho('and recompile all codes, or specify the')
                call fcecho('the columns to operate on!')
              endif

            endif
            
          endif

c          print*,'out of goodcols',inocols,abort,nfield

          if((cols(1).eq.'GOOD'.and.inocols.eq.1))then
            call fcecho('ERROR - NO GOOD COLUMNS COULD BE DETERMINED')
            call fcecho('Enter COLUMNS to be processed by hand')
            call fcecho('***Aborting***')
            abort=.TRUE.
            goto 999
          endif

          if((cols(1).eq.'ERROR'.and.inocols.eq.1))then
            call fcecho('ERROR - NO ERROR COLUMNS COULD BE DETERMINED')
            call fcecho('Enter COLUMNS to be processed by hand')
            call fcecho('***Aborting***')
            abort=.TRUE.
            goto 999
          endif

          
          if(accumulate.eq.'MANY'.and.inocols.eq.1)then
            call fcecho(' ')
            call fcecho('Only ONE column being searched for but')
            call fcecho('ACCUMULATE equals MANY. Changing to ONE')
            accumulate='ONE'
          endif

          do 499 ic=1,inocols
            
            if(ifile.eq.1.and.icycles.eq.1)then

c             If the user specifies that the columns names aren't
c case sensitive we have to do some screwy stuff. So we are calling
c FITSIO to find matches to the input columns and then setting the
c input columns to be equal to the actual values in the file.
c Yes, this is very dangerous, and I do this under protest. If your
c codes bomb, then try inputting the column names as they appear
c in the file - this may or may not work since this requirement was
c added after testing and design and was "shoehorned" in, as usual...
              if(.not.sensecase)then
                call ftgcno(iunit,exact,cols(ic),jcolpos(ic)
     &             ,status)
                if(status.ne.0)then
                  call fcecho(' ')
                  call fcecho('Could not find COLUMN number')
                  call fcecho('for column name:')
                  call fcecho(cols(ic))
                  call fcecho('Try setting sensecase to NO and/or')
                  call fcecho('the spelling of your column names!')
                  call fcecho('Check input data. Aborting.')
                  status=0
                  abort=.TRUE.
                  goto 999
                else
                  cols(ic)=ttype(jcolpos(ic))
                endif
                
                if(columns.ne.'GOOD'.and.
     &             columns.ne.'ERROR')then
                  if(ic.eq.1)then
                    call fcecho(' ')
                    call fcecho('Column names in the data file which cor
     &respond to the input columns were:')
                  endif
                  call fcecho(cols(ic))
                endif
              endif

              isnocols=inocols
              cscols(ic)=' '
              cscols(ic)=cols(ic)
              
            endif

            do 498 ic1=1,isnocols
              if(ic.ne.ic1.and.(.not.lidentical))then

                if(cols(ic).eq.cols(ic1))then
                  call fcecho(' ')
                  call fcecho('****WARNING**** IDENTICAL COLUMN NAMES FO
     &UND!')
                  call fcecho(' ')
                  call fcecho('Possible ambiguity in columns when proces
     &sing multiple files.')
                  call fcecho('Possible errors - same column MAY be accu
     &mulated multiple times!')
                  call fcecho('Cannot uniquely indentify each column')
                  lidentical=.TRUE.
                  call fcecho('Aborting... ')
                  abort=.TRUE.
                  goto 999
                endif

              endif
              
498         continue

499       continue

          if(ifile.gt.1)then

c      If we do not have identical column names then we can find
c      out where each one is so that each file accumulates things properly. 

            do 351 j=1,isnocols

              if(.not.lidentical)then
                if(cols(ic).eq.cscols(j))then
                  icstore(ic)=j
                endif
              endif

351         continue

          endif

          if(ldryrun)then
            call fcecho(' ')
            call fcecho('Columns to be processed are')
            call fcecho(columns)
            call fti2c(inocols,cval,status)
            call fcecho('The number of columns extracted are')
            call fcecho(cval)
          endif

c      If there are multiple identical column names then we cannot
c      uniquely identify each column by its name. So we will assume that
c      each column is positioned consecutively. 
          
          do 500 ic=1,inocols

            if(ifile.eq.1.and.icycles.eq.1.or.lidentical)then
              icstore(ic)=ic
            endif

            call ftgcno(iunit,exact,cols(ic),jcolpos(ic),status)
            if(status.eq.0)then
              if(.not.sensecase)then
                cols(ic)=ttype(jcolpos(ic))
              endif
            endif

            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Could not find COLUMN number')
              call fcecho('for column name:')
              call fcecho(cols(ic))
              call fcecho('Try setting sensecase to NO and/or')
              call fcecho('the spelling of your column names!')
              call fcecho('Check input data. Aborting.')
              status=0
              abort=.TRUE.
              goto 999
            endif
            
500       continue

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

          if(inocols.gt.1.and.icycles.eq.1)then

c           This routine will check that the columns are all 100 percent
c compatible AND will sort them so that they are in ascending order
c so that the code will perform optimally.
            
            call chkcolumns(iunit,cols,jcolpos,inocols,tform,
     &         tunit,ttype,files(ifile),abort)

            if(abort)then
              call fcecho(' ')
              call fcecho('Error in CHKCOLUMNS subroutine')
              call fcecho('Your columns cannot be processed')
              call fcecho('together. You will have to analyze')
              call fcecho('them with separate runs!')
              call fcecho(' ')
              call fcecho('Columns not compatible - aborting')
              return
            else
              call fcecho(' ')
              call fcecho('File passed compatibility column checks.')
            endif

            if(ifile.eq.1)then
              if(.not.lidentical)then
                do ic1 = 1, isnocols
                  do ic=1, inocols
                    if(cols(ic).ne.cscols(ic1))then
                      lidentical=.TRUE.
                    endif
                  enddo
                enddo

                if(lidentical)then
                  call fcecho(' ')
                  call fcecho('Columns were reorganized to optimize')
                  call fcecho('processing. The resulting sequence was:')
                  do ic = 1,inocols
                    cscols(ic)=cols(ic)
                    call fcecho(cols(ic))
                  enddo
                  lidentical=.FALSE.
                endif
              
              endif
            endif
          endif
          
          
          do 501 ic=1,inocols
          
c      Parce the TDIMnnn keyword to get the dimensionality of the counts
c      column (colcnt(ifile)) and store that info in naxis(ic),
c      and naxes.
c            print*,'jcolpos is ic',ic,jcolpos(ic),ttype(jcolpos(ic))
          call ftgtdm(iunit,jcolpos(ic),isml,naxis(ic),
     &       naxes1,status)

          if(naxis(ic).eq.1)naxes1(2)=1
          
          do 550 j=1,isml
            naxes(j,ic)=naxes1(j)
550       continue
          
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not find TDIM# value')
            call fcecho('The format of this data violates SA data')
            call fcecho('Error may occur!!!')
            call fcecho('Attempting to continue...')
          endif

c      Since all of the columns stored in the 'COLUMNS' option input
c      parament MUST be stored in EXACTLY the same form we we check that
c      and if that is indeed the case then we can proceed using only one
c      set of values to figure out how to process the information stored
c      within each column.
           
c      Since there is a discrepancy in how XTE/SA files are written
c      when the array is (N,1) we have to compensate for this and make
c      our case function as if we have a 2D array in which the second
c      element is zero.
          onedim(ic)=.FALSE.

          if(naxis(ic).eq.1)then
            onedim(ic)=.TRUE.
          endif

          if(ldryrun.and.(naxis(ic).eq.1))then
            call fcecho(' ')
            call fcecho('NAXIS value yields only 1 axis')
          endif

c----------------------------------------------------------------------
c      Now we will read and write out the rest of the information
c      that is contained within this extension. Since some of the
c      KEYWORDS searched for may not exist we will have to reset status
c      after each attempt. However if the KEYWORD is found we will print
c      that information out to the output file that we are creating.

c      Here we will attempt to do all reads on various keywords that are
c      necessary for actually processing the files. An error in finding
c      something that is necessary may produce unexpected results and
c      since some of these keywords may or may not be present we cannot
c      do any type of generic check to call out the error..
           
          do 1010 i=1,ikey2

            call ftgkys(iunit,keyword2(i),comn,comm,status)
            if(keyword2(i).eq.'CREATOR')comm=
     &         'SAEXTRCT - Version 4.3a'
            
            if(keyword2(i).eq.'TELESCOP'.and.
     &         comn.ne.'XTE')nonxte=.TRUE.
            status=0
            
1010     continue

         do 1011 i=1,ikey3
           call ftgkyj(iunit,keyword3(i),ikeyval(i),comm,status)
           status=0
1011     continue

         do 1012 i=1,ikey4
           call ftgkyd(iunit,keyword4(i),keyval(i),comm,status)
           
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
           
           if(keyword4(i).eq.'TIMEDEL')then
             itime=i
             if(status.ne.0.and.nonxte)then
               call fcecho('In NON-XTE file and cannot find TIMEDEL')
               call fcecho('aborting....')
               call fcerrm(status)
               return
             endif
           endif
           status=0

1012     continue

c---------------------------------------------------------------------
c
c      Perform a check to find out what the upper and lower channel
c      bounds are on the counts column. We set the 1st character in
c      the keyword to the correct element of the array. From the TDIM
c      calls we know the dimension of the array which is stored in
c      the variable 'naxis(ic)'. Since the first column is time dependent
c      we are left with other columns as having possible
c      upper and lower channel limits which vary according to the
c      the number of channel analyzers.
c
c      Since modification to fitsio routines would be required to
c      generalize the following and the final form of the keywords
c      may/will change, I opted to create the exact string that will
c      exist in SA data based upon the returns from the TDIM calls.

c      'i' will take on the value of the dimension of the array.
c      i.e. we are dealing with 2D to ND arrays where NAXIS(IC) gives the
c      dimension. This is usually written as (X,Y) so in this instance
c      NAXIS(IC) = 2. For SA data we know that the first column is time so
c      all we have to deal with are the other elements. Each of these
c      elements have an upper and lower bound for the detector. 

c      Now we have to perform a check to see that the channel
c      boundaries are the same in all of the file. If the boundaries
c      are not correct in this file we will jump to the end of the
c      loop and continue processing all of the other files...
c      
c----------------------------------------------------------------------

         keyword(6:8)=' '
         do 1013 i=1,naxis(ic)
           icol=6   
           if(jcolpos(ic).lt.10)then
             keyword(icol:icol)=cnum(jcolpos(ic)+1)
           elseif (jcolpos(ic).lt.100)then
             keyword(icol:icol)=cnum(jcolpos(ic)/10+1)
             keyword(icol+1:icol+1)=cnum(mod(jcolpos(ic),10)+1)
           elseif (jcolpos(ic).lt.1000)then
             keyword(icol:icol)=cnum(jcolpos(ic)/100+1)
             keyword(icol+1:icol+1)=cnum((mod(jcolpos(ic),100)/10)+1)
             keyword(icol+2:icol+2)=cnum(mod(mod(jcolpos(ic),100),10)+1)
           elseif (jcolpos(ic).ge.1000)then
             comm='ERROR column number greater than 999'
             call fcerr(comm)
           endif

c           print*,'keyword is ',keyword
            
           keyword(1:5)='TDDES'
           call ftgkys(iunit,keyword,tddes(i),comm,status)

c          Convert the TDDES value read in to uppercase...
           call ftupch(tddes(i))
           status=0

c           print*,'keyword is ',keyword
           
           keyword(1:1)=cnum(i+1)
           keyword(2:5)='CTYP'
           call ftgkys(iunit,keyword,ctyp(i),comm,status)
           status=0        

c           print*,'keyword is ',keyword

           call ftupch(ctyp(i))
            
           if(ctyp(i).eq.'DELTA_TIME'.or.ctyp(i).eq.'TIME')then
             timec=.TRUE.
             itimpos(ic)=i
           endif

           if(ctyp(i).eq.'CHANNEL')icolpos(ic)=i

c           print*,'ctyp(i) value is ',i,ctyp(i)
c           print*,'ITIMPOS(IC),ICOLPOS(IC)',ic, itimpos(ic),icolpos(ic)

           if(onedim(ic))then
             if(itimpos(ic).ne.0)then
               icolpos(ic)=itimpos(ic)+1
               cdlt(icolpos(ic))=1.0d0
               cpixold='(0~255)'
               if(ldryrun)then
                 call fcecho('1D - found DELTA_TIME setting colpos')
                 call fcecho('Only 1 channel exists for this file')
                 call fcecho('Setting CPIXn = (0~255)')
               endif
             elseif(icolpos(ic).ne.0)then
               itimpos(ic)=icolpos(ic)+1
               cdlt(itimpos(ic))=keyval(itime)*conversion
               if(ldryrun)
     &            call fcecho('1D - found CHANNEL setting itimpos')
             endif
           endif

           if(ifile.eq.1.and.icycles.eq.1.and.
     &        naxes(icolpos(ic),ic).gt.iconoffset)
     &        iconoffset=naxes(icolpos(ic),ic)

c           if(ifile.eq.1.and.
c     &        naxes(icolpos(ic),ic).gt.iconoffset)
c     &        iconoffset=naxes(icolpos(ic),ic)

c           print*,'keyword is ',keyword,' and val is ',ctyp(i)
           status=0

           keyword(2:5)='CUNI'
           call ftgkys(iunit,keyword,cuni(i),comm,status)
           if(status.ne.0)cuni(i)=' '
c           print*,'keyword is ',keyword,' and val is ',cuni(i)
           status=0

           if(ctyp(i).eq.'TIME'.or.ctyp(i).eq.'DELTA_TIME')then
             call timeconvert(cuni(i),conversion,status)
             if(status.ne.0)then
               call fcecho('Could not determine TIMEUNIT conversion')
               call fcecho('Assuming all inputs are in seconds')
               conversion=1.0d0
               status=0
             endif
           endif

c           print*,'CPIX BEING CHECKED',ic,
c     &        i,icolpos(ic),naxes(icolpos(ic),ic)
           
           if(i.eq.icolpos(ic))then
c      CPIXi is the KEYWORD which describes the upper and lower energy
c      channels that are associated with each 'channel' written into the
c      science array file. 
             keyword(2:5)='CPIX'
             call ftgkys(iunit,keyword,cpix(i),comm,status)
             if(ldryrun.and.(status.ne.0))then
               call fcecho(' ')
               call fcecho('Unable to find mCPIXnnn KEYWORD!')
               call fcecho('Cannot uniquely identify energy channels to bi
     &n channel correspondence!')
               call fcecho('Assuming a 1-1 correspondence')
               cval=' '
             endif

             if(status.ne.0)then
               cval=' '
               status=0
               call fti2c((naxes(icolpos(ic),ic)-1),cvalhold,status)
               if(status.ne.0)then
                 call fcecho('Could not generate CPIX keyword.')
                 call fcecho('Setting CPIX to 0:1024.')
                 status=0
                 cpix(i)='0:1024'
               else
                 outlen=fcstln(cvalhold)
                 j1=outlen
                 k=outlen
222               continue
                 if(cvalhold(k:k).eq.' ')goto 223
                 k=k-1
                 goto 222
223               continue
                 cval(1:2)='0:'
                 cval(3:(j1-k+4))=cvalhold(k+1:j1)
                 if(cvalhold(k+1:j1).eq.' ')cpix(i)='0:1024'
                 cpix(i)=cval
c                 cpixold=cpix(i)
                 if(ldryrun)call fcecho(cval)
               endif
             endif
           
             if(ldryrun)then
               call fcecho('CPIX(I) value before MACRO checking is')
               call fcecho(cpix(i))
             endif
             
             call parsemacro(iunit,cpix(i))

             if(cpix(i).ne.cpixold)then

               if(ldryrun)then
                 call fcecho('CPIX(I) after MACRO checking is')
                 call fcecho(cpix(i))
               endif

               call parsebd (cpix(i),inobd(ic),ne,ilbd,iubd,
     &            ldryrun,status)
               cpixold=cpix(i)

               if(status.ne.0)then
                 if(ldryrun)then
                   call fcecho('Unable to PARSE CPIX, so assuming')
                   call fcecho('that number of channels are given by')
                   call fcecho('NAXES for this column')
                 endif
               else
                 inobd(ic)=inobd(icolpos(1))
               endif
                 
               inobd(ic)=naxes(icolpos(ic),ic)
               status=0
               lstatus=.TRUE.
             endif
             
             if(cpix(i).eq.' '.or.lstatus)
     &          inobd(ic)=naxes(icolpos(ic),ic)

             if(ifile.eq.1.and.icycles.eq.1.and.
     &          naxes(icolpos(ic),ic).gt.iconoffset)
     &          iconoffset=naxes(icolpos(ic),ic)

c             if(ifile.eq.1.and.
c     &          naxes(icolpos(ic),ic).gt.iconoffset)
c     &          iconoffset=naxes(icolpos(ic),ic)

             if(ldryrun)then
               if(naxes(icolpos(ic),ic).ne.inobd(ic))then
                 call fcecho('Error: The number of channels found')
                 call fcecho('does not equal the value in TDIMS')
               endif
             endif
             
             if(ldryrun)then
               if(cpix(i).eq.' ')then
                 call fcecho(' ')
                 call fcecho('No CPIX keyword found')
                 call fcecho('Assuming no channel binning in file')
                 call fti2c(naxes(icolpos(ic),ic),cval,status)
                 call fcecho('We have the following number of channels')
                 call fcecho(cval)
               else
                 call fcecho(' ')
                 call fcecho('CPIX keyword for this column is')
                 call fcecho(cpix(i))
                 call fcecho(' ')
                 call fti2c(inobd(ic),cval,status)
                 call fcecho('Therefore we have N channels, N =')
                 call fcecho(cval)
               
               endif
             endif

             if(ifile.eq.1.and.icycles.eq.1)then
               cspix(ic)=cpix(i)
               isnobd(ic)=inobd(ic)
             endif
             
             do 350 j=1,inobd(ic)
               if(ifile.eq.1.and.icycles.eq.1)then
                 islbd(j,ic)=ilbd(j)
                 isubd(j,ic)=iubd(j)
               endif
               ilbds(j,ic)=ilbd(j)
               iubds(j,ic)=iubd(j)
350          continue
             
             if(inobd(ic).ne.isnobd(icstore(ic)))then
               call fcecho('Error: Number of channels differs')
               call fcecho('and column name is the same.')
               call fcecho('Original channel breakdown is')
               call fcecho(cspix(icstore(ic)))
               call fcecho('channel breakdown for this file is')
               call fcecho(cpix(i))
             endif
               
           endif
           
c          MJT 08 Apr 2010
c          A desperate attempt to avoid out-of-range array indexing
c          which is causing nonsensical results and/or crashes on 64-bit
c          platforms. The code is too convoluted to fix it correctly...
           if (inobd(ic).eq.0) then
              inobd(ic)=1
              ilbd(1)=0
              iubd(inobd(ic))=0
           endif
c           print*,'ilbd is ',inobd(ic),(ilbd(j),j=1,inobd(ic))
c           print*,'iubd is ',inobd(ic),(iubd(j),j=1,inobd(ic))
           
           keyword(2:5)='CDLT'
           call ftgkyd(iunit,keyword,cdlt(i),comm,status)
           cdlt(i)=cdlt(i)*conversion
            
           if(status.ne.0)cdlt(i)=0.0e0
           status=0

1013     continue

         keyword(1:5)='TNULL'
         call ftgkyj(iunit,keyword,inull(jcolpos(ic)),comm,status)

         if(status.ne.0)inull(jcolpos(ic))=-999
         status=0
         
c      Since naxes contains the dimensioned size of the array according
c      to (time,channels) we can access the interval of increase for
c      both time and columns

c      If we are dealing with a non-XTE FITS file then we need to set
c      values that would normally be set by reading specific KEYWORDS.
        
         if(nonxte)then
           call ftbnfm(tform(jcolpos(ic)),tdtype,trcount,width,status)
            
           if(status.ne.0)then
             call fcecho('No TFORM and NON-XTE data - aborting')
             call fcerrm(status)
             return
           endif

           naxes(icolpos(ic),ic)=trcount
           naxes(itimpos(ic),ic)=1

c           print*,'tnull for jcolpos(ic) is',jcolpos(ic),
c     &        inull(jcolpos(ic))
c           print*,'icolpos is', naxes(icolpos(ic),ic),icolpos(ic)
c           print*,'itimpos is', naxes(itimpos(ic),ic),itimpos(ic)
           
           if(keyval(itime).eq.0.0d0.and.
     &         cdlt(itimpos(ic)).eq.0.0d0)then
             cdlt(itimpos(ic))=1.0d0
           elseif(cdlt(itimpos(ic)).eq.0.0d0)then
             cdlt(itimpos(ic))=keyval(itime)
           endif

c           print*,'cdlt is ',cdlt(itimpos(ic))

         else
           call ftbnfm(tform(jcolpos(ic)),tdtype,trcount,width,status)
           if(status.ne.0)then
             call fcecho(' ')
             call fcecho('Error in calling ftbnfm')
             call fcerrm(status)
             call fcecho('Cannot continue. Aborting.')
             status=0
             abort=.TRUE.
             goto 999

           endif
           
c           print*,'tform(jcolpos(ic),tdtype,trcount,width',
c     &        tform(jcolpos(ic)),tdtype,trcount,width
c           print*,'nb and naxis is',naxis(ic),nb
c           print*,'trcount is ',trcount
           j1=1
           do 1056 i = 1,naxis(ic)
             j1=j1*naxes1(i)
c             print*,'naxes1 is, ic, i',ic,i,naxis(ic),naxes1(i)
1056       continue

           idim = trcount

c        Since there has cropped up cases where valid XTE data contains
c no information about HOW the data is stored and it turns out that
c case is because we have a scalar and technically NOT VALID SA data, so
c we have to punch in the type of information that the code expects.
           if(idim.eq.1)then
             icolpos(ic)=2
             itimpos(ic)=1
             naxes(icolpos(ic),ic)=trcount
             naxes(itimpos(ic),ic)=1
             tinc(itimpos(ic),ic) = keyval(itime)*conversion
             cinc(icolpos(ic),ic) = 1.0d0
             cdlt(itimpos(ic))= keyval(itime)*conversion
             cdlt(icolpos(ic))= 1.0d0
           endif
             
c           print*,'idim is ',idim,trcount,j1
c           print*,'icolpos is ',ic,icolpos(ic),
c     &        naxes(icolpos(ic),ic)
c           print*,'itimpos is ',ic,itimpos(ic),
c     &        naxes(itimpos(ic),ic)
c           print*,'tinc is ',tinc(itimpos(ic),ic),conversion,
c     &        itime,keyval(itime)
c           print*,'cinc is ',cinc(icolpos(ic),ic)
           
           if(trcount.ne.j1)then
             call fcecho(' ')
             call fcecho('ERROR in input file!!!')
             call fcecho('Values of TDIMn multiplied together')
             call fcecho('do not agree with TFORM keyword!!!')
             call fcecho(' ')
             call fcecho('CHECK INPUT DATA FILES!')
             call fcecho(' ')
             call fcecho('This error will cause SAEXTRCT to fail!')
             call fcecho('You can change TDIMn with FPARKEY')
             call fcecho('and rerun SAEXTRCT with a correct TDIM.')
             call fcecho('ABORTING...')
             call fcecho(' ')
             abort=.TRUE.
             goto 999
           elseif(ldryrun)then
             call fcecho(' ')
             call fcecho('TDIM and TFORM keyword comparison agrees.')
           endif
           
         endif
         
c      Since the channel increment may not be consistent, we will set it
c      to 1.0 if none is specified in the input file.
        if(cdlt(icolpos(ic)).eq.0)cdlt(icolpos(ic))=1.0d0
           
c      Since there is a discrepancy in how XTE/SA files are written
c      when the array is (N,1) we have to compensate for this and make
c      our case function as if we have a 2D array in which the second
c      element is zero.
        if(naxis(ic).eq.1)then
          naxis(ic)=2
          naxes(naxis(ic),ic)=1
        endif

c      These values may be modified at a later point if it turns
c      out that the TDIM KEYWORD isn't present in the file. If
c      that is the case we are not processing XTE data and we will
c      have to modify a LOT of the above values such that this code
c      will preform as expected.

        if(nonxte)cdlt(itimpos(ic))=keyval(itime)*conversion

        tinc(itimpos(ic),ic) = (cdlt(itimpos(ic)))
        cinc(icolpos(ic),ic) = (cdlt(icolpos(ic)))

c        print*,'tinc(itimpos(ic),ic) and cinc(icolpos(ic),ic) are'
c        print*,ic,itimpos(ic),tinc(itimpos(ic),ic),
c     &     icolpos(ic),cinc(icolpos(ic),ic)

c     Since the bin-size was specified to be an even multiple of
c     the time resolution of the file, we have to set bin-size
c     at this point.
        if((ifile.eq.1).and.(icycles.eq.1).and.
     &     (lmultiple).and.(ic.eq.1))then
          if(tinc(itimpos(ic),ic).le.0.0d0)then
            call fcecho('You specified that each light-bin')
            call fcecho('is to be an even multiple')
            call fcecho('of the minimum time resolution')
            call fcecho('but this file does not contain this')
            call fcecho('information. Cannot continue!')
            call fcecho(' ')
            call fcecho('Aborting...Enter BINSIZE by hand!')
            abort=.TRUE.
            goto 999
          endif
          
          binsz=binsz*tinc(itimpos(ic),ic)
          call fcecho(' ')
          call ftd2f(binsz,16,cval,status)
          call fcecho('Binsz being set to:')
          call fcecho(cval)           
        endif
        
        
c      We have to check the bin-size against the cdlt for the chosen
c     column.

        if(ifile.eq.1.and.icycles.eq.1.and.
     &     binsz.lt.tinc(itimpos(ic),ic))then
          call fcecho(' ')
          call fcecho('Bin-size is smaller than DELTA_TIME.')
          call fcecho('Multiple files can be processed.')
          call fcecho('Setting BINSZ to equal minimum time')
          call fcecho('resolution of the first file:')
          call fcecho(file1)
          binsz=cdlt(itimpos(ic))
          contxt=' '
          call ftd2f(binsz,6,contxt,status)

          if(status.ne.0)then
            call fcecho('Error in converting BINSZ to characters')
            status=0
          else
            call fcecho('Bin-size is being set to:')
            call fcecho(contxt)
          endif
          
        elseif(ifile.gt.1.and.binsz.lt.tinc(itimpos(ic),ic))then
          call fcecho(' ')
          call fcecho('Bin-size is smaller than DELTA_TIME.')
          call fcecho('Multiple are being processed.')
          call fcecho('of variable bin-size. ')
          call fcecho('Unexpected errors may result')
          call fcecho('Skipping file:')
          call fcecho(file1)
          goto 10
        endif

c        print*,'columns timecol, jcolpos(ic) colpost is '
c     &     ,columns,cols(ic),timecol,jcolpos(ic),colpost

c        print*,'itimpos(ic).lt.icolpos(ic)',itimpos(ic),icolpos(ic)

c**********************************************************************
c      The following section sets up the number of elements that
c      we have to skip to get to the first column of interest. In
c      order to do this we have to do some checks...
        if(itimpos(ic).ne.0.and.icolpos(ic).ne.0)then

c          print*,'IN FIRST LOOP'
          if(itimpos(ic).lt.icolpos(ic))then
            illimit(ic)=itimpos(ic)
            iulimit(ic)=icolpos(ic)
          else
            illimit(ic)=icolpos(ic)
            iulimit(ic)=itimpos(ic)
          endif
          
        else

c          print*,'IN SECOND LOOP'
          if(itimpos(ic).eq.0)then
            illimit(ic)=icolpos(ic)
          else
            illimit(ic)=itimpos(ic)
          endif
          
          iulimit(ic)=illimit(ic)+1

        endif

c        print*,'itimpos(ic),icolpos(ic)',itimpos(ic),icolpos(ic)
c        print*,'iulimit(ic), and illimit(ic) is ',ic,illimit(ic),
c     &     iulimit(ic)
        
        itofst(ic)=0
        ifmfst(ic)=0
        itoend(ic)=0

        if(illimit(ic).gt.1)itofst(ic)=1
        if(illimit(ic)+1.ne.iulimit(ic).and.
     &     illimit(ic).ne.iulimit(ic))ifmfst(ic)=1
        if(iulimit(ic).lt.naxis(ic))itoend(ic)=1

c----------------------------------------------------------------------
c      Now that we have stored the lower limit in illimit(ic) and the
c      upper limit in iulimit(ic) we can figure out if we are going to
c      have to skip any elements to get to the first element (itofst(ic))
c      and to go from the last element of interest to the last
c      element in that cell (itoend(ic)).
            
        do 1014 i=1,illimit(ic)-1
          itofst(ic)=itofst(ic)*naxes(i,ic)
1014    continue
           
        do 1015 i=illimit(ic),iulimit(ic)
          ifmfst(ic)=ifmfst(ic)*naxes(i,ic)
1015    continue
        
        do 1016 i=iulimit(ic)+1,naxis(ic)
          itoend(ic)=itoend(ic)*naxes(i,ic)
1016    continue

c      Find out the total number of elements that are in a complete
c      data set column
            
        nelem(ic)=1
        do 1017 i=illimit(ic),iulimit(ic)
          nelem(ic)=nelem(ic)*naxes(i,ic)
1017    continue

c      Since there are a couple of possiblities when we are processing
c      these files we will deal with the cases differently for speed
c      of processing. The first case is that all of the elements are
c      together so that we do not have to skip any elements before
c      processing the files. Thus we can read in large blocks of data
c      and process it all at once. We may want to break these up into
c      separate subroutines at a later point. Also as this case if the
c      only one for which data exists at the moment we will deal with
c      other cases as they arrive.
           
c!        if((illimit(ic).eq.1.and.iulimit(ic).eq.2).or.
c!     &     (illimit(ic).eq.1.and.iulimit(ic).eq.1))then
        
c      Lets determine if we have enough array space allocated to
c      process these files and if so set up the number of bins that
c      we have to deal with.
        
        if(ifile.eq.1.and.icycles.eq.1.and.ic.eq.1)then
c      When we are processing the first file we do not want any offsets

          doffset(1,1)=dtruc((tims(1)-timemin)/binsz)
          doffset(1,1)=0.0d0
          
          if(doffset(1,1).lt.0.0d0)then
            doffset(1,1)=dabs(doffset(1,1))
          else
            doffset(1,1)=0.0d0
          endif

          kchanall=0
          dchan=0.0d0
          dchanall=0.0d0
          dtempall=0.0d0
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
c            print*,'For the first file dchan and dchanall is',
c     &         dchan,dchanall

            if(dmod((time(1)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dchan-1.0d0
            dchan=dtruc(dchan)
            dtemp=dchan
            doffset(2,1)=dchan
            dchanall=dtruc(dchanall)
            dchanall=dtruc(dchanall+dchan)

c            print*,'doffset(2,1) is ',doffset(2,1),dchanall
            
c      Now deal with all of the subsequent files.
            do 1050 i=2,no
              
              dtempall=dtruc(dtempall+dtemp)
c              print*,'DTEMPALL is ',dtempall,dtemp
              
              doffset(1,i)=dtruc(((tims(i)-tims(1))/binsz))
     &           -dtempall
              doffset(1,i)=dtruc(doffset(1,i))
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
              dchan=dtruc(dchan)
              dtemp=dtruc((((time(i)-tims(1))/binsz))+1.0d0)-dtempall
              dtemp=dtruc(dtemp)

c              print*,'DTEMP and DCHAN are',dtemp,dchan

              if(dremain((time(i)-tims(1)),binsz).
     &           lt.1.0d-9)then
                dchan=dchan-1.0d0
                dchan=dtruc(dchan)
                dtemp=dtemp-1.0d0
                dtemp=dtruc(dtemp)
              endif

              doffset(2,i)=dtemp
              dchanall=dtruc(dchanall+dtemp)

c              print*,'DOFFSET(2,i) is:',i,doffset(2,i),dchanall,dtemp
              
1050        continue

            if(dremain((time(no)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dtruc(dchan-1.0d0)

          else
            doffset(1,1)=0.0d0
            
            dchan=dtruc(((time(1)-tims(1))/binsz)+1.0d0)

            if(dremain((time(1)-tims(1)),binsz).lt.1.0d-9)
     &         dchan=dchan-1.0d0
            doffset(2,1)=dchan
            dchanall=dtruc(dchanall+dchan)
          endif

c          print*,'dtemp,dchan,dchanall is',dtemp,dchan,
c     &       dchanall,doffset(1,1),doffset(2,1)

          if(ifile.eq.1.and.icycles.eq.1)then
c             print*,'itimecycle is ',itimecycle,dchanall

            charall=' '
            call fcecho(' ')
            call fcecho('File time-ranges are given by:')
            call fcecho('  File Number       Start Time value     Stop T
     &ime Value')
            do iprint=1,no
c              print*,'doffset(1,i),doffset(2,1) is',iprint,
c     &           doffset(1,iprint),
c     &           doffset(2,iprint),dchanall


              
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

c            print*,' '
c            print*,'CALLING TIMEINTERVAL1'
c            print*,' '
            
            call timeinterval(ichan,itimecycle, dchanlower, timelower,
     &         timeupper, dchanall, doffset, inocols, time,
     &         tims, binsz, no)

c            print*,' '
c            print*,'CALLING TIMEINTERVAL2'
c            print*,' '

c            call timeinterval2(ichan,itimecycle, dchanlower, timelower,
c     &         timeupper, dchanall, doffset, inocols, time,
c     &         tims, binsz, no)


c            stop
            
c            print*,'itimecycle after is',itimecycle
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
c              print*,'iprint,timelower,timeupper',iprint,
c     &           timelower(iprint),timeupper(iprint)
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
        
          
          
c          print*,'timelower(1) is',timelower(1),timeupper(itimecycle)
c          print*,'timemin, timemax,itimecycle',
c     &       timemin,timemax,itimecycle
          
          if(inocols.gt.1)icolfset=dtruc(dchanall)

        endif

        doffsettotal(icycles,ic)=dtruc(doffsettotal(icycles,ic)+
     &     doffset(1,ifile))

c        print*,'DOFFSETTOTAL IS ',doffsettotal(icycles,ic),
c     &     doffset(1,ifile),ic
c        print*,'icolfsettotal(icycles) and dchanlower are',
c     &     icolfsettotal(icycles), dchanlower(icycles)
        
c**********************************************************************
c      Now that we have read in all of the information which is
c      necessary to process this file we have to rearrange it into a
c      form which is in agreement with XTE/SA data 

c      In order to make this code as efficient as possible on each
c      access to the input file we have to do some manipulation of the
c      information to determine the optimum size to read in for each
c      access of the input file. Since we have allocated space for a
c      maximum of naray elements we will optimize the reads to
c      the input file for maximum efficiency.

c      IELEM(IC) = the number of times we can read
c      naxes(illimit(ic),ic) elements
c      from the file before our allocated array space is filed.

c        print*,'ielem(ic) and stuff',naray, ic, illimit(ic),
c     &     naxes(illimit(ic),ic)
        ielem(ic)=naray/naxes(illimit(ic),ic)
c        print*,'ielem(ic)',ic,ielem(ic)

c      Check to see we can read in all of the elements contained within
c      one cell with one read statement, if so then set it to the number
c      of times that naxes(illimit(ic),ic) are written in the
c      file of interest. 
          if(ielem(ic).ge.naxes(iulimit(ic),ic))
     &       ielem(ic)=naxes(iulimit(ic),ic)
c          print*,' After if ielem(ic)',ic,ielem(ic)
          
c      IRUN(IC) = the number of loops in which we can read IELEM(IC)*
c      naxes(illimit(ic),ic)
c      elements.
          irun(ic)=naxes(iulimit(ic),ic)/ielem(ic)
c          print*,'irun(ic)',ic,irun(ic),naxes(iulimit(ic),ic),
c     &       iulimit(ic)

c      ILEFT = the number of naxes(illimit(ic),ic) reads we
c      are left to perform
c      before we have finished reading in all of the elements in the cell. 
          ileft(ic)=(naxes(iulimit(ic),ic)-(irun(ic)*ielem(ic)))
          if(ileft(ic).lt.0)then
            ileft(ic)=0
          endif
c          print*,'ileft(ic)',ic,ileft(ic)
          
c**********************************************************************
c      Okay, now that we have all of the book-keeping taken care of
c      as far as figuring out how many elements we can handle before
c      our array space is filled up on each read we are ready to
c      actually proceed to reading the data from the files.

c      All of the following is dedicated to making optimal use of the
c      array space that is allocated for reading the file in. If speed
c      is the primary considered, increasing the size of the array can
c      result in an increase of processing speed, with a corresponding
c      increase in memory requirements.

c      So the number of elements (nelem(ic)) that we will read are
c      ielem(ic)*naxes(illimit(ic),ic) so lets redefine
c      nelem(ic) to be equal to
c      this value for each of the loops.
          nelem(ic)=ielem(ic)*naxes(illimit(ic),ic)

c      Let's also define the number of elements that we will have to
c      read in order to process the entire cell of elements.
          nleft=ileft(ic)*naxes(illimit(ic),ic)
           
c      Let's check that everything is behaving as it should be and
c      that no errors have crept stealthily into our code...
          if((irun(ic)*ielem(ic))+ileft(ic)
     &       .ne.naxes(iulimit(ic),ic))then
            call fcecho('ERROR ERROR')
            call fcecho('IRUN, IELEM, and ILEFT should be checked')
          endif

501     continue

c        stop
c        print*,'After 501 continue'
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
c      Now that we have all of the information that is necessary to
c      process this input file, we will actually begin to perform
c      reads on the rows to get the necessary information.

           
c      Loop over all of the rows that are stored in each file
c
       if(ldryrun)goto 51

c       if((tims(ifile).lt.timelower(icycles).or.
c     &    tims(ifile).gt.timeupper(icycles)).and.
c     &    (time(ifile).le.timelower(icycles).or.
c     &    time(ifile).gt.timeupper(icycles)).and.
c     &    tims(ifile))then

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
c         if(icycles.lt.itimecycle)then
c           icycles=icycles+1
c           print*,'Going to 2'
c           goto 2
c         else
c         print*,'Going to 51'
           goto 51
c         endif

       endif

       call ftgkyd(iunit,'TIMEZERO',timezero,comm,status)
       if(status.ne.0)then
         call fcecho('Could not find TIMEZERO')
         status=0
         call ftgkyt(iunit,'TIMEZERO',itimezero,timezerof,
     &      comm,status)
         if(status.ne.0)then
           timezero=0.0d0
           status=0
         else
           timezero=dfloat(itimezero)+timezerof
         endif
       endif

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

       call fti2c(nrows,cval,status)
       status=0
       call fcecho(' ')
       comm=' '
       comm='This file contains:            rows.'
       comm(20:30)=cval(10:20)
       call fcecho(comm)
       
       call fti2c(trcount,cval,status)
       status=0
       comm=' '
       comm='Each row contains:             elements.'
       comm(20:30)=cval(10:20)
       call fcecho(comm)
       comm=' '

       if(dcycleend(ifile).gt.1.0d0)then
         dcyclestart(ifile)=dcycleend(ifile)
         cval=' '
         icyclestart=idint(dcyclestart(ifile))
         call fti2c(icyclestart,cval,status)
         status=0
         comm=' '
         comm='Processing file beginning on row '
         comm(34:44)=cval(10:20)
         call fcecho(comm)
       endif

       icyclestart=idint(dcyclestart(ifile))
       
       do i=1,icols
         imiss(i)=0
       enddo
       

       do 50 lrow=icyclestart,nrows

         idim = (1000000/(trcount*inocols))+1
         if(mod(lrow,idim).eq.0)then
           call fti2c(lrow,cval,status)
           status=0
           comm=' '
           comm='Processed              rows,'
           comm(12:22)=cval(10:20)
           call fti2c((lrow*trcount*inocols),cval,status)
           comm(29:39)=cval(10:20)
           comm(40:72)=' elements, continuing.'
           call fcecho(comm)
         endif

c         print*,'Processing row number',lrow
c      Accumulate the total number of seconds which has passed
c      in performing the observations so we can calculate
c      the EVENT_RATE
         nulvald=0.0d0

c      First element to begin reading.
         felem=1

c      Increment value used in performing the read of the array.
         eincr=1

c      What to do with null values?
         nultyp=0
c      Read in a double precision time element stored in
c      the input file into 'timearay' 
c        call ftgcld(iunit,colpost,lrow,felem,1,eincr,
c    &      nultyp,nulvald,timearay,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
         call ftgcvd(iunit,colpost,lrow,felem,1,nulvald,
     &      timearay,anynul,status)

c      Print out any error information about accessing the files
         if (status.ne.0)then
           call fcecho('Could not get timestamp')
           call fcerrm(status)
         endif

         timearay=(timearay+timezero)*conversion
         timeval=timearay
            
c      If we have been given a series of COLUMNS that we have to
c      carry with us this is where we do them. But this will slow the
c      code down as it is presently written.
         if(iwrt.gt.0)then

           lor=.FALSE.
           land=.FALSE.
           pand=.FALSE.
              
           timeval=timearay

c      Let's check to see if this time falls within a GTI.
           do 274 k=itimno(1,ifile),itimno(2,ifile)
             if(timeval.ge.tmjds(k).and.
     &          timeval.lt.(tmjde(k)+dlta))then
               lor=.TRUE.
             endif
274        continue

           do 64 k=1,ipermno
             if(timeval.ge.tmjds(itimnoall+k).and.
     &          timeval.lt.(tmjde(itimnoall+k)+dlta))then
               land=.TRUE.
             endif
64         continue

c      We test to see if any period information has been selected
c      for us to use as a criteria in creating the output file.

           if(period.gt.0.0d0)then
             if((tmjds(itimnoall+ipermno+1).eq.0.0d0).and.
     &          (tmjde(itimnoall+ipermno+1).eq.1.0d0))then
               pand=.TRUE.
             else

               dperstart=dtruc((timeval-ephem)/period)
               
               do 166 k=1,iphaseno

                 pstart=ephem+((dperstart)*period)+
     &              period*tmjds(itimnoall+ipermno+k)
                 pstop=ephem+((dperstart)*period)+
     &              period*tmjde(itimnoall+ipermno+k)
                    
                 if((timeval.ge.pstart).and.
     &              (timeval.lt.pstop))then
                   pand=.TRUE.
                   goto 167
                 endif
                      
166            continue
                      
167            continue
             endif
           else
             pand=.TRUE.
           endif

           if(lor.and.land.and.pand)then
             
             do 810 j1=1,iwrt
               do 820 j=1,rcount(j1)

c                call ftgcle(iunit,iwrite(j1),lrow,j,1,eincr    
c    &              ,nultyp,nulval,rval,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcve since ftgcle should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
                 nulvale=0.0
                 call ftgcve(iunit,iwrite(j1),lrow,j,1,nulvale,
     &              rval,anynul,status)

                 if(rval.lt.0.and.rval.ne.inul(j1))then
                   call fcecho('Skipping negative or INDEF value for')
                   call fcecho(cwrite(j1))
c                   print*,'rval is ',rval
                 elseif(rval.eq.inul(j1).or.
     &                negative.eq.'IGNORE')then
                   rval=0

                   if(ifile.eq.1.and.icycles.eq.1)then
                     rstor(j1)=rstor(j1)+rval
                   else
                     do 830 j2=1,iwrtstore
                       if(cwrite(j1).eq.cswrite(j2))then
                         rstor(j2)=rstor(j2)+rval
                       else
                              
                       endif
830                  continue
                   endif
                            
                 endif

820            continue
810          continue

           endif
         endif
            
c      This is the point at which we begin our regular loop
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

c      Reinitialize element number and channel number for each row.
c      Each row is a different channel.

         elem=0
         iaccum=0
         iperiod=0
         inum=0
         inum2=0

c      What to do with null values?
         nulval=0 

c        print*,'iconoffsetarray(ic is)',iconoffsetarray,
c     &      iconoffset

         icolfsettotal(icycles)=0
c         iconoffsettotal=0
         
         do 900 ic=1,inocols

           if(ifile.eq.1)then
             if(accumulate.eq.'MANY')then

               if(chbin.ne.'INDEF')iconoffset=ichbinum

               icolfsettotal(icycles)=((ic-1)*icolfset)
               iconoffsettotal=(ic-1)*iconoffset

             elseif(accumulate.eq.'ONE')then

               icolfsettotal(icycles)=0
               iconoffsettotal=0

             endif

             icolfsetarray(ic)=icolfsettotal(icycles)
             iconoffsetarray(ic)=iconoffsettotal
                
           elseif(ifile.gt.1)then

             if(accumulate.eq.'MANY')then
               icolfsettotal(icycles)=icolfsetarray(icstore(ic))
               iconoffsettotal=iconoffsetarray(icstore(ic))
c               print*,'iconoffsettotal and array are',
c     &            ic,icstore(ic),iconoffsetarray(icstore(ic)),
c     &            iconoffsettotal
               
               icolfsettotal(icycles)=
     &            (dfloat(icstore(ic)-1)*icolfset)
               iconoffsettotal=((icstore(ic)-1)*iconoffset)
             elseif(accumulate.eq.'ONE')then
               icolfsettotal(icycles)=0
               iconoffsettotal=0

             endif

           endif

c          print*,'ICOLFSETTOTAL, ICONOFFSETTOTAL',
c     &        icolfsettotal(icycles),iconoffsettotal,
c     &        icstore(ic),ic,
c     &        icolfsetarray(icstore(ic)),
c     &        iconoffsetarray(icstore(ic))
           
           do 80 ichn=1,irun(ic)

c      Read into 'array,' nelem(ic) elements of I*2 values.
c      If we do more than 1 loop each read will skip over all of the
c      values already read in.
             ifelem=((ichn-1)*(nelem(ic)))+1

c            call ftgcld(iunit,jcolpos(ic),lrow,ifelem,nelem(ic),eincr
c    &          ,nultyp,nulval,darray,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
             call ftgcvd(iunit,jcolpos(ic),lrow,ifelem,nelem(ic),
     &            nulvald,darray,anynul,status)

c             print*,'jcolpos(ic)',jcolpos(ic),lrow, ifelem,
c     &          nelem(ic),darray(1),nultyp,nulval,
c     &          flgval,anynul,status
              
c      Print out any error information about accessing the files
             if (status.ne.0)then
               comm='Trying to read in integer array'
               call fcecho(comm)
               call fcerrm(status)
             endif

             inum=0
             inum1=0
             iper=nint((timemin-timemax/period)+0.5)
 
c             print*,'Read a value',array(1),array(2),jcolpos(ic),ic,
c     &            nelem(ic),lrow,icstore(ic)
c      Sum all of the time values into the correct channel.
c      print*,'timeval,array(inum1),inum1,
c     &                bin,iperiod,channel,colval',

c             print*,'doing 70 and 72 loop',ielem(ic),
c     &          illimit(ic),ic,naxes(illimit(ic),ic)
             
             do 70 j = 1,ielem(ic)
               do 71 i=1,naxes(illimit(ic),ic)

c                 print*,'itimpos(ic) start is',j,i,itimpos(ic)
                 lor=.FALSE.
                 land=.FALSE.
                 pand=.FALSE.
                 cand=.FALSE.
                 
                 timeval=timearay+((j-1)*tinc(iulimit(ic),ic)
     &              +(i-1)*tinc(illimit(ic),ic))


c                 print*,'TIME is ',timearay,timeval,ic,j,i
                 inum1=inum1+1

                 if(timeval.ge.(timelower(icycles)).and.
     &              timeval.lt.(timeupper(icycles)+dlta))then

                   if(imiss(ic).eq.0)then
                     dcycleend(ifile)=lrow
                   endif
                   
                 else
                   
c                 print*,'itimpos(ic) 2 is',itimpos(ic)
c                   print*,'Going to 771', timeval,timelower(icycles)
c     &                ,timeupper(icycles)
                   
c                   if((timeval.gt.timeupper(itimecycle)).or.
c     &                (timeval.lt.timelower(1)))then

c                   endif

                   if(timeval.gt.(timeupper(icycles)+dlta))then
                     imiss(ic)=imiss(ic)+1
                     if(imiss(ic).gt.maxmiss)goto 900
                   endif
                   if(icountfix.lt.icount(ic))
     &                icountfix=icount(ic)
                   
                   goto 771
                 endif
                  
                 dtotevents(icstore(ic))=dtotevents(icstore(ic))
     &              +darray(inum1)
                 devents(icstore(ic))=devents(icstore(ic))
     &              +darray(inum1)
                 
                 colval=1+((j-1)*cinc(iulimit(ic),ic)
     &              +(i-1)*cinc(illimit(ic),ic))
                  
c      Let's check to see if this time falls within a GTI.
c                 print*,'Doing itimno ',
c     &              itimno(1,ifile),itimno(2,ifile)
c                 print*,'itimpos(ic) 3 is',itimpos(ic)
c           print*,'itimno(1,ifile), and itimno(2,ifile) are',
c     &        itimno(1,ifile), itimno(2,ifile),
c     &              tmjds(1),tmjde(1)

                 
                 do 75 k=itimno(1,ifile),itimno(2,ifile)
                   
                   if(timeval.ge.(tmjds(k)).and.
     &                timeval.lt.(tmjde(k)+dlta))then
                     lor=.TRUE.
                     small=tmjds(k)
                     large=tmjde(k)
                   endif

75               continue

c                 print*,'itimpos(ic) 4 is',itimpos(ic)                 
c                 print*,'Doing ipermno check'

c                 print*,'ipermno is ',ipermno
           
                 
                 do 55 k=1,ipermno
                   if(timeval.ge.(tmjds(itimnoall+k)).and.
     &                timeval.lt.(tmjde(itimnoall+k)+dlta))then
                     land=.TRUE.
                     if(tmjds(itimnoall+k).gt.small)
     &                  small=tmjds(itimnoall+k)
                     if(tmjde(itimnoall+k).lt.large)
     &                  large=tmjde(itimnoall+k)
                   endif
                   
55               continue

c                 print*,'itimpos(ic) 5 is',itimpos(ic)
                 
c                 print*,'Doing ichno',ichno
                 do 82 k=1,ichno

c                    print*,'ilbd(colval) and iubd(colval) is'
c                    print*,ilbd(colval),iubd(colval),
c     &                 cints(k),cinte(k),colval,k
                   if(ilbd(colval).le.cints(k).and.
     &                iubd(colval).ge.cinte(k))then
                     cand=.TRUE.
                   elseif(ilbd(colval).ge.cints(k).and.
     &                  ilbd(colval).le.cinte(k))then
                     cand=.TRUE.
                   elseif(iubd(colval).ge.cints(k).and.
     &                  iubd(colval).le.cinte(k))then
                     cand=.TRUE.
                   endif
                   
82               continue
                    
c      We test to see if any period information has been selected
c      for us to use as a criteria in creating the output file.
c                 print*,'period is ',lrow,i,j,period
                 if(period.gt.0.0d0)then
                   if((tmjds(itimnoall+ipermno+1).eq.0.0d0).and.
     &                (tmjde(itimnoall+ipermno+1).eq.1.0d0))then
                     pand=.TRUE.
                   else
                     dperstart=dtruc((timeval-ephem)/period)

c                     print*,'dperstart is ',dperstart,period,ephem,
c     &                  timeval

                     do 155 k=1,iphaseno
                        
                       pstart=ephem+((dperstart)*period)+
     &                    period*tmjds(itimnoall+ipermno+k)
                       pstop=ephem+((dperstart)*period)+
     &                    period*tmjde(itimnoall+ipermno+k)

c                       print*,'pstart pstop',pstart,pstop,k
                       
                       if((timeval.ge.(pstart)).and.
     &                    (timeval.lt.(pstop)))then
                         pand=.TRUE.
c                         print*,'PAND is true',timeval
                         if(pstart.gt.small)small=pstart
                         if(pstop.le.large)large=pstop
                         goto 158
                       endif
                        
155                  continue
158                  continue

                   endif
                  
                 else
                   pand=.TRUE.
                 endif
c                 print*,'itimpos(ic) 7 is',itimpos(ic)
                 
                 if((.not.lor).or.(.not.land))then
                   dttot(icstore(ic))=dttot(icstore(ic))
     &                +darray(inum1)
                   dtim(icstore(ic))=dtim(icstore(ic))
     &                +darray(inum1)
c                    print*,'tmjds(1),tmjde(1),timeval',
c     &                 tmjds(1),tmjde(1),timeval,lor,land
                 elseif(.not.cand)then
                   dpha(icstore(ic))=dpha(icstore(ic))
     &                +darray(inum1)
                   dtotpha(icstore(ic))=dtotpha(icstore(ic))
     &                +darray(inum1)
                   dtotval=dtotval+1.0d0
                 elseif(.not.pand)then
                   dphase(icstore(ic))=dphase(icstore(ic))
     &                +darray(inum1)
                   dtotphase(icstore(ic))=dtotphase(icstore(ic))
     &                +darray(inum1)
                 endif

c                 print*,'lor,land,pand',lor,land,pand
c                 print*,'itimpos(ic) 8 is',itimpos(ic)                 
                 if(lor.and.land.and.pand)then
c                   print*,'All are true'
                   inum=colval+iconoffsettotal

c                   print*,'inum is ',inum,ic,itimpos(ic)
c                   itemp=itemp+1

                   timepha(inum)=timepha(inum)
     &                +(tinc(itimpos(ic),ic)/
     &                dfloat(naxes(icolpos(ic),ic)))

                   dper=dtruc((((timeval-tims(1))/binsz))+1.0d0)
                   dpernoval=dper
                   iper=idint((dper-doffsettotal(icycles,ic))+
     &                dfloat(icolfsettotal(icycles))
     &                -dchanlower(icycles))


                   if(iarray(iper).eq.0.0d0)then
                     icount(icstore(ic))=icount(icstore(ic))+1
                     iarray(iper)=icount(icstore(ic))+
     &                  icolfsettotal(icycles)
                   endif
                   
                   iperiod=iarray(iper)
                   
                   realbin(iperiod)=realbin(iperiod)
     &                +(tinc(itimpos(ic),ic)/
     &                dfloat(naxes(icolpos(ic),ic)))

                   endif

c                 print*,'lor,land,pand,cand',lor,land,pand,cand
c                 print*,'itimpos(ic) 9 is',itimpos(ic)
                 if(lor.and.land.and.pand.and.cand)then
                   
                   dchansum=dchansum+darray(inum1)
                   dtotgood(icstore(ic))=dtotgood(icstore(ic))
     &                +darray(inum1)                   
                   dtotval=dtotval+1.0d0
                   dgood(icstore(ic))=dgood(icstore(ic))
     &                +darray(inum1)
                   
                   dper=dtruc((((timeval-tims(1))/binsz))+1.0d0)
                 
                   iper=idint((dper-doffsettotal(icycles,ic))+
     &                dfloat(icolfsettotal(icycles))
     &                -dchanlower(icycles))

c                   print*,'itimpos(ic) 100 is',itimpos(ic)
                   
                   if(iarray(iper).eq.0.0d0)then
                     icount(icstore(ic))=icount(icstore(ic))+1

                     iarray(iper)=icount(icstore(ic))+
     &                  icolfsettotal(icycles)

c                     print*,' '
c                     print*,'ic,icount',ic,icount(icstore(ic))
c                     print*,'icolfsettotal(icycles),iper,inum1',
c     &                  icolfsettotal(icycles),iper,inum1,icycles
c                     print*,'doffsettotal(icycles,ic)',
c     &                  doffsettotal(icycles,ic)
c                     print*,'ARAYkchanlower(icycles),icycles'
c     &                  ,dchanlower(icycles),icycles,
c     &                  iarray(iper)
                      
                     if((icount(icstore(ic))+icolfsettotal(icycles))
     &                  .gt.(ichan))then
                       contxt='Maximum bin number exceeded lcbinarray'
                       call fcecho(contxt)
                       call fcecho('Set lcbinarray to a larger value.')
                       call fcecho('Cannot continue - aborting')
                       abort=.TRUE.
                       goto 999
                     endif

c                     print*,'itimpos(ic) 11 is',itimpos(ic)
                     
                   endif

c                   print*,'itimpos(ic) 12 is',itimpos(ic)
                   
                   iperiod=iarray(iper)

c                   if(inum1.eq.127)print*,'IPERIOD is ',iperiod

                   if(darray(inum1).eq.inull(jcolpos(ic)))goto 71

                   if(darray(inum1).lt.0.and.
     &                negative.eq.'IGNORE')goto 71
                   if(darray(inum1).lt.0.and.
     &                negative.ne.'SUM')goto 71

                   dbin(iperiod)=dbin(iperiod)+(darray(inum1))
                   dpernoarray(iperiod)=dpernoval
                   
                   dbincount(iperiod)=dbincount(iperiod)
     &                +1.0d0

c                   print*,'dbin(iperiod is ',dbin(iperiod),
c     &                darray(inum1)
c                   print*,'dbincount is ',dbincount(iperiod)

c                   print*,'itimpos(ic) 13 is',itimpos(ic)
                   
                   if(chbin.ne.'INDEF')then
                     do 275 jnum=1,ichbinum
                       if(ilbd(colval).ge.ichbins(jnum).and.
     &                    iubd(colval).le.ichbine(jnum))then
                         
                         inum=jnum+iconoffsettotal
                         
                         dchannel(inum)=dchannel(inum)+
     &                      (darray(inum1))
                         
c                         timepha(inum)=timepha(inum)
c     &                      +(tinc(itimpos(ic),ic)/
c     &                      dfloat(naxes(icolpos(ic),ic)))

                         
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
                         if(spmode.eq.'MEAN')then
                           realchan(inum)=realchan(inum)+1.0d0
                         endif
                         goto 276

                       endif
275                  continue
                   else
                          
                     inum=colval+iconoffsettotal
                          
                     dchannel(inum)=dchannel(inum)+
     &                  (darray(inum1))
                                
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
                     if(spmode.eq.'MEAN')then
                       realchan(inum)=realchan(inum)+1.0d0
                     endif

                   endif

c                   print*,'itimpos(ic) 14 is',itimpos(ic)
                   
276                continue

c                   if(igti.ge.512)then
c                    if(.not.lerrorgti)then
c                     lerrorgti=.TRUE.
c                     print*,'dgtistart and stop'
c                     do iprint=1,512
c                       print*,dgtistart(iprint),dgtistop(iprint)
c                     enddo
                     
c                     call fcecho(' ')
c                     call fcecho('ERROR!!!!!')
c                     call fcecho('Your data filtering has resulted in')
c                     call fcecho('more than 512 GTIs, exhausting the')
c                     call fcecho('allocated space.')
c                     call fcecho('Continuing but not generating')
c                     call fcecho('GTIs for the rest of the file.')
c                    endif
c                    goto 771
c                   endif
                   
c                   if(igti.eq.0.and.small.ne.0.0d0
c     &                .and.large.ne.0.0d0)then
c                     igti=1
c                     dgtistart(igti)=small
c                     dgtistop(igti)=large
c                   endif

c                   if(dgtistart(igti).ne.small.and.
c     &                dgtistop(igti).ne.large)then
c                     igti=igti+1
c                     dgtistart(igti)=small
c                     dgtistop(igti)=large
c                   endif

                 endif

771              continue

71             continue

70           continue

80         continue

           colvalhold=colval+1

c           print*,'COLVALHOLD is after 80',colvalhold,colval

c      Okay now let's go back and pick up any elements that we were
c      unable to read into our array using the original number of elements.
           if(ileft(ic).ne.0)then

c      Read into 'array,' nelem(ic) elements of I*2 values.
c      We have to skip over all of the values already read in when we
c      were reading in nelem(ic) at a pass. 
             ifelem=((irun(ic))*(nelem(ic)))+1

c      Since we have processed irun(ic)*nelem(ic) elements in each cell we have
c      to go back and pick up what is left. Thus this number of elements
c      is ileft(ic)*naxes(illimit(ic),ic)
             nelem(ic)=ileft(ic)*naxes(illimit(ic),ic)
c             print*,'nelem(ic) is ',nelem(ic)

c      Read in an integer array of elements into 'array' 
c            call ftgcld(iunit,jcolpos(ic),lrow,ifelem,nelem(ic),eincr
c    &          ,nultyp,nulval,darray,flgval,anynul,status)
c      MJT -- 31Dec97:
c      Changing this to ftgcvd since ftgcld should really be using a
c      logical *array* for flgval; this is confusing the new wrappers 
             call ftgcvd(iunit,jcolpos(ic),lrow,ifelem,nelem(ic),
     &            nulvald,darray,anynul,status)
c             print*,'jcolpos(ic)',jcolpos(ic),lrow, ifelem,
c     &          nelem(ic),darray(1),flgval,anynul,status

c      Print out any error information about accessing the files
             if (status.ne.0)then
               comm='Trying to read in integer array'
               call fcecho(comm)
               call fcerrm(status)
             endif

             inum=0
             inum1=0

c      Sum all of the time values into the correct channel.

             do 72 j = 1,ileft(ic)
               do 73 i=1,naxes(illimit(ic),ic)
                 
                 lor=.FALSE.
                 land=.FALSE.
                 pand=.FALSE.
                 cand=.FALSE.
                  
                 inum1=inum1+1
                 
                 timeval=timearay+((j-1)*tinc(iulimit(ic),ic)
     &              +(i-1)*tinc(illimit(ic),ic))

                 if(timeval.ge.(timelower(icycles)).and.
     &              timeval.lt.(timeupper(icycles)+dlta))then

                   if(imiss(ic).eq.0)then
                     dcycleend(ifile)=lrow
                   endif
                   
                 else
                   if((timeval.gt.timeupper(itimecycle)).or.
     &                (timeval.lt.timelower(1)))then
                  
                     dtotevents(icstore(ic))=dtotevents(icstore(ic))
     &                  +darray(inum1)
                     devents(icstore(ic))=devents(icstore(ic))
     &                  +darray(inum1)
                     dttot(icstore(ic))=dttot(icstore(ic))
     &                  +darray(inum1)
                     dtim(icstore(ic))=dtim(icstore(ic))
     &                  +darray(inum1)

                   endif
 
                   if(timeval.gt.(timeupper(icycles)+dlta))then
                     imiss(ic)=imiss(ic)+1
                     if(imiss(ic).gt.maxmiss)goto 900
                   endif

                   if(icountfix.lt.icount(ic))
     &                icountfix=icount(ic)                   

                   goto 773
                 endif

                 colval=colvalhold+((j-1)*cinc(iulimit(ic),ic)
     &              +(i-1)*cinc(illimit(ic),ic))

                 dtotevents(icstore(ic))=dtotevents(icstore(ic))
     &              +darray(inum1)
                 devents(icstore(ic))=devents(icstore(ic))
     &              +darray(inum1)
                 
c      Let's check to see if this time falls within a GTI.
                 do 74 k=itimno(1,ifile),itimno(2,ifile)
                   if(timeval.ge.(tmjds(k)).and.
     &                timeval.lt.(tmjde(k)+dlta))then
                     lor=.TRUE.
                     small=tmjds(k)
                     large=tmjde(k)
                   endif
74               continue
                  
                 do 54 k=1,ipermno
                   if(timeval.ge.(tmjds(itimnoall+k)).and.
     &                timeval.lt.(tmjde(itimnoall+k)+dlta))then
                     land=.TRUE.
                     if(tmjds(itimnoall+k).gt.small)
     &                  small=tmjds(itimnoall+k)
                     if(tmjde(itimnoall+k).lt.large)
     &                  large=tmjde(itimnoall+k)
                   endif
54               continue

                 do 81 l=1,ichno

                   if(ilbd(colval).le.cints(k).and.
     &                iubd(colval).ge.cinte(k))then
                     cand=.TRUE.
                   elseif(ilbd(colval).ge.cints(k).and.
     &                  ilbd(colval).le.cinte(k))then
                     cand=.TRUE.
                   elseif(iubd(colval).ge.cints(k).and.
     &                  iubd(colval).le.cinte(k))then
                     cand=.TRUE.
                   endif
81               continue

c      We test to see if any period information has been selected
c      for us to use as a criteria in creating the output file.

                 if(period.gt.0.0d0)then
                   if((tmjds(itimnoall+ipermno+1).eq.0.0d0).and.
     &                (tmjde(itimnoall+ipermno+1).eq.1.0d0))then
                     pand=.TRUE.
                   else
                     
                     dperstart=dtruc((timeval-ephem)/period)
                     
                     do 156 k=1,iphaseno

                       pstart=ephem+((dperstart)*period)+
     &                    period*tmjds(itimnoall+ipermno+k)
                       pstop=ephem+((dperstart)*period)+
     &                    period*tmjde(itimnoall+ipermno+k)
                       
                       if((timeval.ge.(pstart)).and.
     &                    (timeval.le.(pstop)))then
                         pand=.TRUE.
                         if(pstart.gt.small)small=pstart
                         if(pstop.le.large)large=pstop
                         goto 157
                       endif
                      
156                  continue
157                  continue

                   endif

                 else
                   pand=.TRUE.
                 endif

                 if((.not.lor).or.(.not.land))then
                   dttot(icstore(ic))=dttot(icstore(ic))
     &                +darray(inum1)
                   dtim(icstore(ic))=dtim(icstore(ic))
     &                +darray(inum1)
                   dtotbad(icstore(ic))=dtotbad(icstore(ic))
     &                +darray(inum1)
                   dbad(icstore(ic))=dbad(icstore(ic))
     &                +darray(inum1)
                 elseif(.not.cand)then
                   dpha(icstore(ic))=dpha(icstore(ic))
     &                +darray(inum1)
                   dtotpha(icstore(ic))=dtotpha(icstore(ic))
     &                +darray(inum1)
                   dtotval=dtotval+1.0d0
                   dtotbad(icstore(ic))=dtotbad(icstore(ic))
     &                +darray(inum1)
                   dbad(icstore(ic))=dbad(icstore(ic))
     &                +darray(inum1)
                 elseif(.not.pand)then
                   dphase(icstore(ic))=dphase(icstore(ic))
     &                +darray(inum1)
                   dtotphase(icstore(ic))=dtotphase(icstore(ic))
     &                +darray(inum1)
                   dtotbad(icstore(ic))=dtotbad(icstore(ic))
     &                +darray(inum1)
                   dbad(icstore(ic))=dbad(icstore(ic))
     &                +darray(inum1)
                 endif

                 if(lor.and.land.and.pand)then
                   
                   inum=colval+iconoffsettotal
                   
                   timepha(inum)=timepha(inum)
     &                +(tinc(itimpos(ic),ic)/
     &                dfloat(naxes(icolpos(ic),ic)))
                   
                   dper=dtruc((((timeval-tims(1))/binsz))+1.0d0)
                   dpernoval=dper
                   iper=idint((dper-doffsettotal(icycles,ic))+
     &                dfloat(icolfsettotal(icycles))
     &                -dchanlower(icycles))

                   if(iarray(iper).eq.0.0d0)then
                     icount(icstore(ic))=icount(icstore(ic))+1
                     
                     iarray(iper)=icount(icstore(ic))+
     &                  icolfsettotal(icycles)
                   endif
                   iperiod=iarray(iper)
                   realbin(iperiod)=realbin(iperiod)
     &                +(tinc(itimpos(ic),ic)/
     &                dfloat(naxes(icolpos(ic),ic)))

                 endif

                 if(lor.and.land.and.pand.and.cand)then 

                   dtotgood(icstore(ic))=dtotgood(icstore(ic))
     &                +darray(inum1)
                   dtotval=dtotval+1.0d0
                   dgood(icstore(ic))=dgood(icstore(ic))+
     &                darray(inum1)

                   dper=dtruc((((timeval-tims(1))/binsz))+1.0d0)

                   iper=idint((dper-doffsettotal(icycles,ic))+
     &                dfloat(icolfsettotal(icycles))
     &                -dchanlower(icycles))
                    
                   if(iarray(iper).eq.0.0d0)then
                     icount(icstore(ic))=icount(icstore(ic))+1
                     
                     iarray(iper)=icount(icstore(ic))+
     &                  icolfsettotal(icycles)
                     
                     if((icount(icstore(ic))+icolfsettotal(icycles))
     &                  .gt.(ichan))then
                       contxt='Maximum bin number exceeded lcbinarray'
                       call fcecho(contxt)
                       call fcecho('Set lcbinarray to a larger value.')
                       call fcecho('Cannot continue - aborting')
                       abort=.TRUE.
                       goto 999
                     endif
                     
                   endif

                   iperiod=iarray(iper)
                   
                   if(darray(inum1).eq.inull(jcolpos(ic)))goto 73

                   if(darray(inum1).lt.0.and.
     &                negative.eq.'IGNORE')goto 73
                   if(darray(inum1).lt.0.and.
     &                negative.ne.'SUM')goto 73
                    
                   dbin(iperiod)=dbin(iperiod)+darray(inum1)
                   dpernoarray(iperiod)=dpernoval

                   dbincount(iperiod)=dbincount(iperiod)
     &                +1.0d0

                   if(chbin.ne.'INDEF')then
                     do 375 jnum=1,ichbinum
                       if(ilbd(colval).ge.ichbins(jnum).and.
     &                    iubd(colval).le.ichbine(jnum))then
                         
                         inum=jnum+iconoffsettotal
                          
                         dchannel(inum)=dchannel(inum)+
     &                      (darray(inum1))
                                
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
                         if(spmode.eq.'MEAN')then
                           realchan(inum)=realchan(inum)+1.0d0
                         endif
                         goto 376
                       endif
375                  continue
                   else
                          
                     inum=colval+iconoffsettotal
                     
                     dchannel(inum)=dchannel(inum)+
     &                  (darray(inum1))
                                
c      Since the MEAN binmode requires that we know how many elements
c      fall into each bin we have to keep track of that here.
                     if(spmode.eq.'MEAN')then
                       realchan(inum)=realchan(inum)+1.0d0
                     endif

                   endif

376                continue

c                   if(igti.ge.512)then
c                    if(.not.lerrorgti)then
c                     lerrorgti=.TRUE.
c                     print*,'dgtistart and stop'
c                     do iprint=1,512
c                       print*,dgtistart(iprint),dgtistop(iprint)
c                     enddo
                     
c                     call fcecho(' ')
c                     call fcecho('ERROR!!!!!')
c                     call fcecho('Your data filtering has resulted in')
c                     call fcecho('more than 512 GTIs, exhausting the')
c                     call fcecho('allocated space.')
c                     call fcecho('Continuing but not generating')
c                     call fcecho('GTIs for the rest of the file.')
c                    endif
c                    goto 773
c                   endif

c                   if(igti.eq.0.and.small.ne.0.0d0
c     &                .and.large.ne.0.0d0)then
c                     igti=1
c                     dgtistart(igti)=small
c                     dgtistop(igti)=large
c                   endif
                    
c                   if(dgtistart(igti).ne.small.and.
c     &                dgtistop(igti).ne.large)then
c                     igti=igti+1
c                     dgtistart(igti)=small
c                     dgtistop(igti)=large
c                   endif
                    
                 endif

773              continue
                 
73             continue
              
72           continue

           endif
 
          if(icountfix.lt.icount(ic))icountfix=icount(ic)

c          print*,'Values are',ic,lrow,dtotevents(ic),devents(ic),
c     &       dtotgood(ic),
c     &       dgood(ic),dtotbad(ic),dbad(ic),dttot(ic),dtim(ic),
c     &       dtotphase(ic),dphase(ic),dtotpha(ic),dpha(ic),dthres(ic)
          
900      continue

c         print*,'Finishing loop',lrow,nrows

50     continue

150    continue
       
       call fti2c(lrow-1,cval,status)       
       status=0
       comm=' '
       comm='Processed              rows.'
       comm(12:22)=cval(10:20)
       call fcecho(comm)
       
c       print*,'iconoffsetarray(ic is)',iconoffsetarray

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

          if(.not.ldryrun)then
            if(accumulate.eq.'MANY')then
              call wrtupdate2scr(cols,devents,dgood,dtim,
     &           dphase,dpha,dbitmask,inocols)
            elseif(accumulate.eq.'ONE')then
              call wrtupdate2scr(cols,devents,dgood,dtim,
     &           dphase,dpha,dbitmask,isnocols)
            endif
          endif
          
10    continue

      if(ldryrun)then
        call fcecho(' ')
        call fcecho('Completed DRYRUN through all files')
        call fcecho('*** See all messages ***')
        call fcecho('Set DRYRUN equal FALSE to process files')
        goto 999
      endif
        
c      We are finished cycling over all of the input files and we
c      can move onto performing the processing that is necessary in
c      order to output the necessary information.
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      

c      Set maximum values for each array. IPERIOD is the maximum number
c      of bins of BINSZ that can be created in the time interval from
c      TIMEMIN to TIMEMAX.


      iperiod=icountfix
      
c      print*,'Icount is ',icount
c      print*,'timearray is ',timeval
c      print*,'timeupper is ',timeupper(icycles)
c      print*,'DIFFERECE IS ',timeval-timeupper(icycles)

      if(accumulate.eq.'ONE'.and.inocols.gt.1)then
        lmany=.TRUE.
        isave=inocols
        inocols=1
      else
        isave=inocols
      endif
      
c      print*,'dchanall and icount are',dchanall,icount(ic),
c     &   icountfix,ic,colval,' inocols is ',inocols,icountfix

c      print*,'ICOUNTFIX is ',icountfix
      do 90 k=1,icountfix

c      Reinitialize LBINS to be FALSE and this will remain as long as NO VALID
c      time intervals are encounted, if one is encountered than REALBIN is
c      not set to zero. REALBIN will be set to zero ONLY if all of the
c      data associated with that time are NULLS, i.e., that they are not
c      to be processed.
        lbins=.FALSE.
        
c        print*,'ICOLFSET is ',icolfset
        
        do 990 j=1,inocols
          l=(j-1)*icolfset

          if(lmany)then
            realbin(k+l)=realbin(k+l)/dfloat(isave)
          endif

c          print*,'k,l realbin',j,k,l,(k+l),realbin(k+l),
c     $       totsecsa(j)
          totsecsa(j)=totsecsa(j)+(realbin(k+l))
          
          dbincount(k+l)=
     &       dbincount(k+l)/dfloat(naxes(icolpos(j),j))
        
          if(realbin(k+l).ne.0.0d0.and.
     &       (realbin(k+l)/binsz).lt.mfracexp.and.
     &       (lcmode.eq.'RATE'.or.lcmode.eq.'EVENT_RATE'))then
            dthres(j)=dthres(j)+dbin(k+l)
            dbin(k+l)=dble(tnull)
            lcbin=.TRUE.
          endif
          
          if(realbin(k+l).ne.0.0d0)then
            if((dbin(k+l)/realbin(k+l)).gt.mlcinten.and.
     &       (lcmode.eq.'RATE'.or.lcmode.eq.'EVENT_RATE'))then
              dlcinten(j)=dlcinten(j)+dbin(k+l)
              dbin(k+l)=dble(tnull)
              lcbin=.TRUE.
            endif

          elseif((lcmode.eq.'SUM'.or.lcmode.eq.'EVENT_SUM').and.
     &         realbin(k+l).ne.0.0d0.and.
     &         dbin(k+l).gt.mlcinten)then
            dlcinten(j)=dlcinten(j)+dbin(k+l)
            dbin(k+l)=dble(tnull)
            lcbin=.TRUE.
            
          elseif((lcmode.eq.'MEAN').and.
     &         (dbincount(k+l).gt.0))then
            if(((dbin(k+l)/
     &         (dbincount(k+l))).gt.mlcinten))then
              dlcinten(j)=dlcinten(j)+dbin(k+l)
              dbin(k+l)=dble(tnull)
              lcbin=.TRUE.
            endif

          endif
        
c        print*,'ibincount(k),naxes(icolpos(ic),ic)',ibincount(k),
c     &     naxes(icolpos(ic),ic)

c          print*,'TOTSECSA is',j,totsecsa(j)
990     continue

c        print*,'About to go into 991 loop',inocols,icolfset,lbins

        icountfixtemp=0
        do 991 j=1,inocols
          l=(j-1)*icolfset
          if(dbin(k+l).ne.tnull.or.tnull.ge.0.0e0)lbins=.TRUE.
          if(.not.lbins)icountfixtemp=icountfixtemp+1
          if(totsecsa(j).gt.totsecst)totsecst=totsecsa(j)
991     continue

        if(icountfixtemp.eq.inocols)then
          icountfix=icountfix-1
        else
          lbins=.TRUE.
        endif
          
c        print*,'TOTSECST is',totsecst,lbins

c        print*,'About to go into 992 loop',inocols,icolfset
        icountfixtemp=0
        do 992 j=1,inocols
          l=(j-1)*icolfset
          if(.not.lbins)realbin(k+l)=0.0d0
          if(.not.lbins)icountfixtemp=icountfixtemp+1
992     continue

        if(icountfixtemp.eq.inocols)then
          icountfix=icountfix-1
        endif
        
90    continue
c      print*,'OUT of 90 LOOP'

      if(lmany)then
        call fcecho(' ')
        call fcecho('Since ACCUMULATING ONE column from MANY')
        call fcecho('Total BIN time (used for FRACEXP and RATE) is being
     & set to')
        call fcecho('(TOTAL BIN TIME)/(NUMBER OF COLUMNS)')
        totsecst=totsecst/dfloat(inocols)
      endif

c      print*,'TOTAL SECONDS is ',kchanall,totsecst,
c     &   totsecs,icountfix   
        
c      Now that we have calculated the light curve we have to write
c      it out in the proper format. So do it with the following.

      if(icountfix.eq.0)then
        call fcecho('Data selection resulted in no counts')
c        call fcecho('Check this file ')
      endif

c      Let's write out
      charall=' '

      call fcecho(' ')
      call fcecho(' TOTALS FOR ALL COLUMNS AND FILES ARE AS FOLLOWS ')
      call fcecho('==================================================')

      if(accumulate.eq.'ONE'.and.isave.gt.1)then
        call wrtupdate2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &     dtotpha,dbitmask,isave)
      else
        call wrtupdate2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &     dtotpha,dbitmask,inocols)
      endif

      if(icountfix.gt.0)ldata=.TRUE.
      
      if(printmode.eq.'BOTH'.or.printmode.eq.'LIGHTCURVE')then

        curvetype='Light Curve:'

        if(accumulate.eq.'ONE'.and.isave.gt.1)then        
          call wrtlast2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &       dtotpha,isave,accumulate,dthres,dlcinten,
     &       curvetype,totsecsa)
        else
          call wrtlast2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &       dtotpha,inocols,accumulate,dthres,dlcinten,
     &       curvetype,totsecsa)
        endif
      
        call fcecho(' ')
        call fcecho('Writing out Light Curve FITS file')

c        print*,'Total seconds from GTIs calculated is',sum

c        print*,'iarray contains',(iarray(i),i=1,kchanall*inocols)
c        print*,'realbin contains',(realbin(i),i=1,120)
c        print*,'bin contains',(bin(i),i=1,120)

c        if(gettmin.and.icycles.eq.1)then
c          timemin=timemin+1.0d-8
c          timelower(1)=timelower(1)+1.0d-8
c        endif
        
c        if(gettmax.and.icycles.eq.1)timemax=timemax-1.0d-8

c        print*,'before totsecst is ',totsecs
        if(totsecst.gt.0.0d0)then
          totsecs=totsecst
        endif
c        print*,'after totsecs is ',totsecs, totsecst
        
        icounttotal=icounttotal+icountfix
c        PRINT*,'ICOUNTFIX is ',icountfix,icounttotal

        if(icountfix.lt.0)then
          call fcecho(' ')
          call fcecho('    ******ERROR*******    ')
          call fcecho('Number of elements to be added is negative!')
          call fcecho('Aborting....')
          abort=.TRUE.
          goto 999
        endif

        kchanall=idint(dchanall)

        if(accumulate.eq.'MANY')then

c          print*,'bin is',(dbin(i),i=1,20)
c          print*,'realbin is',(realbin(i),i=1,20)
c          print*,'dbincount is',(dbincount(i),i=1,20)

          if(icountfix.lt.0)then
            icountfix=0
c            print*,'ICOUNTFIX is less than 0!',icycles
          endif

c          print*,'Before call to wrtlcfile',totsecs
          if(icountfix.gt.0)then
            call wrtlcfile(files,outfile,lcounit,kchanall,
     &         no,lcmode,extenlc,tims(1),
     &         timemin,timemax,totsecs,binsz,
     &         inocols,icolfset,lcbin,tnull,isave,cols,
     &         dbin,realbin,dbincount,dpernoarray,
     &         inobd(1),ilbd(1),iubd(inobd(1)),cpixold,chbin,
     &         icounttotal,clobber,icycles,dbegin,irowsave,
     &         abort)
          endif
          

        elseif(accumulate.eq.'ONE')then
          if(icountfix.gt.0)then
            call wrtlcfile(files,outfile,lcounit,icountfix,
     &         no,lcmode,extenlc,tims(1),
     &         timemin,timemax,totsecs,binsz,
     &         1,icolfset,lcbin,tnull,isave,cols,
     &         dbin,realbin,dbincount,dpernoarray,
     &         inobd(1),ilbd(1),iubd(inobd(1)),cpixold,chbin,
     &         icounttotal,clobber,icycles,dbegin,irowsave,
     &         abort)
            endif

c          print*,'After LC call'
c          print*,'dbegin is ',dbegin
c          print*,'tfirst is ',tims(1)

        endif
c        print*,'AFTER call to wrtlcfile',totsecs
        
c        print*,'irowsave,ibegin,icycels,totsecs,totsecst',
c     &     irowsave,ibegin,
c     &     icycles,totsecs,totsecst
        
        call fcecho('Wrote Light Curve')
        call fcecho(' ')
        
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
          
c        print*,'About to got into GTI write',icycles
c        print*,'totsecs before wrtlcgti is',totsecs,icountfix
        
        call wrtlcgti(lcounit,realbin,
     &     timemin,timemax,tims(1),
     &     binsz,totsecs,doffset,icountfix,dpernoarray,
     &     icycles,
     &     lwritegti,dgstrtsavlc,dgstpsavlc,isav,
     &     dpernosave,abort)

c        print*,'Ipernosave is',ipernosave,tims(1)
c        print*,'totsecs after wrtlcgti is',totsecs

        if(lwritegti)then
          call fcecho(' ')
          call fcecho('Wrote out GTI extension.')
          call fcecho('Closing Light Curve.')
        endif
        
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
        icycles=icycles+1
        inocols=isnocols
        goto 1
      endif
      
      if(printmode.eq.'BOTH'.or.printmode.eq.'SPECTRUM')then

        if(iwrt.gt.0)then

          do 850 j=1,iwrtm
            do 860 j1=1,iwrt
              if(cwrite(j1).eq.cwritem(j))then
                rstor(j1)=rstor(j1)/(rcount(j1)*
     &             (dtotval)/trcount)
                lwrite(j1)=.TRUE.
              endif
860         continue
850       continue
          
        endif

c      COLVAL is the MAXIMUM channel value stored in the file. Note that
c      each 'channel' my contain the channels from several detectors.
c      There is no way around this ambiguity as the output from several
c      detectors can be summed into one vector. 

        do 950 ic=1,inocols
          if(naxes(icolpos(ic),ic).gt.colval)then
            colval=naxes(icolpos(ic),ic)
          endif
950     continue
      
        if(chbin.ne.'INDEF')colval=ichbinum

        if(accumulate.eq.'MANY')then
          kchannum=(inocols)*colval
        else
          kchannum=colval
        endif

c      print*,'colval and kchannum is',colval,inocols,kchannum
c      print*,'iconoffset, icolfset',iconoffset,icolfset
c      print*,'channels are',(channel(k),k=1,kchannum)

      do 191 ic=1,inocols
        
        do 192 j=1,colval
          if(accumulate.eq.'ONE'.and.isave.gt.1)
     &       timepha(j)=timepha(j)/dfloat(isave)
          totsecsp(ic)=totsecsp(ic)+timepha(j)

c          print*,'totsecsp and timepha is',timepha(j),j,
c     &       totsecsp(ic),ic

c          print*,'tinc(itimepos(ic),ic) is',itemp,
c     &       isave,itimpos(ic),ic,icolpos(ic),
c     &       tinc(itimpos(ic),ic),dfloat(naxes(icolpos(ic),ic)),
c     &       (tinc(itimpos(ic),ic)/
c     &       dfloat(naxes(icolpos(ic),ic)))
          
192     continue
        if(totsecsp(ic).gt.totsecp)totsecp=totsecsp(ic)

c        print*,'Summing up PHA time ic,totsecsp and totsecp',
c     &     ic,totsecsp(ic),totsecp
        
191   continue
        
        do 91 j=1,colval
          do 919 ic=1,inocols
            k=((ic-1)*colval)+j
          
            if((spmode.eq.'RATE'.or.spmode.eq.'EVENT_RATE').and.
     &         (totsecsa(ic).gt.0.0d0))then
              if((dchannel(k)/totsecsa(ic)).gt.mspinten)then
                dspinten(ic)=dspinten(ic)+1.0d0
                lspbin=.TRUE.
              endif

            elseif((spmode.eq.'SUM'.or.spmode.eq.'EVENT_SUM').and.
     &           dchannel(k).gt.mspinten)then
              dspinten(ic)=dspinten(ic)+1.0d0
              lspbin=.TRUE.

            elseif(spmode.eq.'MEAN'.and.
     &           realchan(k).gt.0.0d0)then
              if((dchannel(k)/(realchan(k)))
     &           .gt.mspinten)then
                dspinten(ic)=dspinten(ic)+1         
                lspbin=.TRUE.
              endif

            endif

919       continue
          
91      continue
        
        curvetype='Spectrum:'

        if(accumulate.eq.'ONE'.and.isave.gt.1)then                
          call wrtlast2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &       dtotpha,isave,accumulate,dthres,dspinten,
     &       curvetype,totsecsp)
        else
          call wrtlast2scr(cols,dtotevents,dtotgood,dttot,dtotphase,
     &       dtotpha,inocols,accumulate,dthres,dspinten,
     &       curvetype,totsecsa)
        endif
        
        call fcecho(' ')
        call fcecho('Writing out Spectrum FITS file')

c        print*,'DCHANSUM from inside of loop is',dchansum
        
        dchansum=0
c        print*,'dchannel is ',
c     &     (i,dchannel(i),j+dchannel(i),'   ',i=1,kchannum)

c        print*,'CHANNEL sum is'
        
        do 9951 j=1,kchannum
          dchansum=dchansum+dchannel(j)
9951    continue

c        print*,'Sum of values is',dchansum

c        print*,'about to call spectrum write',totsecs

        iconoffsettotal=0
        if(accumulate.eq.'MANY')then
          colval=iconoffset

          if(chbin.ne.'INDEF')colval=ichbinum

          
c          print*,'About to go into multipha',colval,
c     &       files(1),files(2),outfile,timemin,timemax
c          print*,'COlval and inocols is ',colval,inocols
c          print*,'iconoffsetarray is ',iconoffsetarray
c          print*,'lext is ',lext
          
          call wrtmultphafile(files,outfile,spounit,colval,inocols,
     &       no,spmode,extenpha,lspbin,tnull,
     &       timemin,timemax,binsz,totsecsp,dchannel,realchan,
     &       mspinten,naxes,icolpos,cols,iconoffsetarray,
     &       cints,cinte,ichno,chbin,cpixold,rstor,iwrt,cwrite,
     &       lwrite,clobber,lext)

c          print*,'lext is',lext,iconoffsettotal
          
        elseif(accumulate.eq.'ONE')then

          call wrtphafile(files,outfile,spounit,colval,
     &       no,spmode,extenpha,lspbin,tnull,
     &       timemin,timemax,binsz,totsecp,dchannel,realchan,
     &       mspinten,iconoffsettotal,
     &       isave,cols,chbin,cpixold,
     &       rstor,iwrt,cwrite,lwrite,clobber,
     &       lext)
        endif

c        if(gettmin)dgtistart(1)=dgtistart(1)+1.0d-8
        if(gettmin)dgtistart(1)=dgtistart(1)       
c        if(gettmax)dgtistop(igti)=dgtistop(igti)-1.0d-8        
        if(gettmax)dgtistop(igti)=dgtistop(igti)
        
        call fcecho(' ')
        call fcecho('Writing out GTI for Spectrum FITS file')

c        print*,'DPERNOSAVE is ',dpernosave,tims(1),binsz
        do i=1,isav
          dgtistart(i)=dgstrtsavlc(i)
c     &       -(1.0D0*binsz)
c     &       -(0.5D0*binsz)
          
          dgtistop(i)=dgstpsavlc(i)
c     &       -(1.0D0*binsz)          
c     &       -(0.5D0*binsz)
          if(i.eq.isav.and.printmode.eq.'SPECTRUM')
     &       dgtistop(i)=tims(1)+((dpernosave)*binsz)
c     &       dgtistop(i)=tims(1)+((dpernosave+1.0d0)*binsz)
c          print*,'dgtistart, dgtistop ',i,dgtistart(i),dgtistop(i)
          igti=isav
        enddo
        
        call wrtphagti(spounit,igti,dgtistart,dgtistop,
     &     timemin,timemax,totsecp)

        call fcecho('Wrote Spectrum FITS file')
        call fcecho('Closing Spectrum.')
        call fcecho(' ')
      endif

      call fcecho(' ')
      call fcecho('Everything is finished.')
      call fcecho('Cleaning up memory and exiting.')
      
999   continue

      if(iunit.ne.0)then
        call ftfiou(iunit,status)
        if(status.ne.0)then
          call fcecho('Error freeing input unit number')
          status=0
        endif
      endif
      
      if(ounit.ne.0)then
        call ftfiou(ounit,status)
        if(status.ne.0)then
          call fcecho('Error freeing output unit number')
          status=0
        endif
      endif
      
      return
      end
        
      subroutine chkcolumns(iunit,cols,jcolpos,inocols,tform,
     &   tunit,ttype,files,abort)
      implicit none
      integer icols,nb,isml
      parameter (isml = 9)
      parameter (icols = 40)
      parameter (nb = 512)        

      character(1) cnum(10)        
      character*(*) cols(*),tform(*),tunit(*),ttype(*)        
      integer jcolpos(*),inocols
      character(8) keyword
      character(40) ctyp(icols,isml),cuni(icols,isml),comm
      character(60) contxt
      character*(*) files

      integer iunit,naxis(icols),naxes2(isml,icols),
     &   naxes(isml),status
      integer i,j,icol
      logical abort

c      Set up an array where an integer value is assigned an
c      ascii character value
      data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &   '6','7','8','9'/

      keyword=' '
      abort=.FALSE.
      status=0
      
c      Parce the TDIMnnn keywork to get the dimensionality of the counts
c      column (colcnt(ifile)) and store that info in naxis, and naxes.

      call sortit(cols,jcolpos,inocols)

      do 10 i=1,inocols

        if(i.gt.1)then
          if(tform(jcolpos(1)).ne.tform(jcolpos(i)))then
c              print*,'tform(jcolpos)1',tform(jcolpos(1)),' 2 ',
c     &           tform(jcolpos(i))
            call fcecho(' ')
            call fcecho('Columns are NOT COMPATIBLE - TFORM differs')
            call fcecho('The COLUMNS being checked were')
            contxt=' '
            contxt(1:18)=cols(1)
            contxt(19:20)=', '
            contxt(21:38)=cols(i)
            call fcecho(contxt)
            abort=.TRUE.
            goto 999
c              return
          endif
          if(tunit(jcolpos(1)).ne.tunit(jcolpos(i)))then
c              print*,'tunit(jcolpos)1',tunit(jcolpos(1)),' 2 ',
c     &           tunit(jcolpos(i))
            call fcecho(' ')
            call fcecho('Columns are NOT COMPATIBLE - TUNIT differs')
            call fcecho('The COLUMNS being checked were')
            contxt=' '
            contxt(1:18)=cols(1)
            contxt(19:20)=', '
            contxt(21:38)=cols(i)
            call fcecho(contxt)
            abort=.TRUE.
            goto 999
c              return
          endif
        endif
          
        call ftgtdm(iunit,jcolpos(i),isml,naxis(i),
     &     naxes,status)
        if(status.ne.0)then
          call fcecho('Could not find TDIM# value')
        endif

        if(i.gt.1)then
          if(naxis(1).ne.naxis(i))then
            call fcecho(' ')
            call fcecho('Columns are NOT COMPATIBLE - NAXIS differs')
            call fcecho('The COLUMNS being checked were')
            contxt=' '
            contxt(1:18)=cols(1)
            contxt(19:20)=', '
            contxt(21:38)=cols(i)
            call fcecho(contxt)
            abort=.TRUE.
            goto 999
c              return
          endif
        endif
          
        do 20 j=1,naxis(i)
          naxes2(j,i)=naxes(j)
          naxes(j)=0

          if(i.gt.1)then
            if(naxes2(j,1).ne.naxes2(j,i))then
              call fcecho(' ')
              call fcecho('Columns are NOT COMPATIBLE - NAXES differs')
              call fcecho('The COLUMNS being checked were')
              contxt=' '
              contxt(1:18)=cols(1)
              contxt(19:20)=', '
              contxt(21:38)=cols(i)
              call fcecho(contxt)
              abort=.TRUE.
              goto 999
            endif
          endif
20      continue

10    continue

      do 100 j=1,inocols
        keyword(6:8)=' '
        icol=6   
        if(jcolpos(j).lt.10)then
          keyword(icol:icol)=cnum(jcolpos(j)+1)
        elseif (jcolpos(j).lt.100)then
          keyword(icol:icol)=cnum(jcolpos(j)/10+1)
          keyword(icol+1:icol+1)=cnum(mod(jcolpos(j),10)+1)
        elseif (jcolpos(j).lt.1000)then
          keyword(icol:icol)=cnum(jcolpos(j)/100+1)
          keyword(icol+1:icol+1)=cnum((mod(jcolpos(j),100)/10)+1)
          keyword(icol+2:icol+2)=cnum(mod(mod(jcolpos(j),100),10)+1)
        elseif (jcolpos(j).ge.1000)then
          comm='ERROR column number greater than 999'
          call fcerr(comm)
        endif

        do 200 i=1,naxis(j)
          
          keyword(1:1)=cnum(i+1)
          keyword(2:5)='CTYP'
          call ftgkys(iunit,keyword,ctyp(j,i),comm,status)
          if(status.ne.0)ctyp(j,i)=' '
          status=0        
          
          keyword(2:5)='CUNI'
          call ftgkys(iunit,keyword,cuni(j,i),comm,status)
          if(status.ne.0)cuni(j,i)=' '
          status=0

          if(j.gt.1)then
              
            if(ctyp(1,i).ne.ctyp(j,i))then
              call fcecho(' ')
              call fcecho('Columns are NOT COMPATIBLE - CTYP differs')
              call fcecho('The COLUMNS being checked were')
              contxt=' '
              contxt(1:18)=cols(1)
              contxt(19:20)=', '
              contxt(21:38)=cols(i)
              call fcecho(contxt)
              abort=.TRUE.
              goto 999
c                return
            endif

            if(cuni(1,i).ne.cuni(j,i))then
              call fcecho(' ')
              call fcecho('Columns are NOT COMPATIBLE - CUNI differs')
              call fcecho('The COLUMNS being checked were')
              contxt=' '
              contxt(1:18)=cols(1)
              contxt(19:20)=', '
              contxt(21:38)=cols(i)
              call fcecho(contxt)
              abort=.TRUE.
              goto 999
c                return
            endif
          endif
            
200     continue
100   continue

999   continue
      
      if(abort)then
        call fcecho(' ')
        call fcecho('File being processed was')
        call fcecho(files)
        call fcecho('Check COLUMNS input for compatibility')
        call fcecho('Aborting - cannot continue')
      endif
      
      return
      end

      subroutine goodcols(iunit,nfield,ttype,cols,inocols,
     &   ldryrun,labort)
      implicit none
      integer nfield,inocols,iunit,status,icol
      integer i,j,outlen,fcstln,icols,idfirst,igfirst
      character*(*) cols(*),ttype(*)
      character(1000) tddess,cdval,cgval
      character(80) comn,comm
      character(20) cval
      character(10) keyword
      character(1) cnum(10)      
      logical lxte,lhexte,lpca,labort,ldryrun,linclude,
     &   lo,li,ld,lg
      data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &   '6','7','8','9'/
      status=0
      ld=.FALSE.
      lg=.FALSE.
      li=.FALSE.
      lo=.FALSE.
      lxte=.FALSE.
      lhexte=.FALSE.
      lpca=.FALSE.
      labort=.FALSE.
      cdval=' '
      cgval=' '
      keyword=' '
      icols=0
      
c      print*,'ttype is ',(ttype(i),i=1,nfield)
      call ftgkys(iunit,'TELESCOP',comn,comm,status)
      if(status.ne.0)then
        call fcecho('COLUMNS=GOOD but TELESCOP keyword not found')
        call fcecho('Cannot continue - enter COLUMNS by name')
        labort=.TRUE.        
        status=0
      endif

      if(ldryrun)then
        call fcecho(' ')
        call fcecho('TELESCOPE is ')
        call fcecho(comn)
      endif
      
      if(comn.eq.'XTE')then
        lxte=.TRUE.
      else
        call fcecho(' ')
        call fcecho('COLUMNS=GOOD but TELESCOP not equal XTE')
        call fcecho('Cannot continue - enter COLUMNS by name')
        labort=.TRUE.
      endif

      call ftgkys(iunit,'INSTRUME',comn,comm,status)
      if(comn.ne.'HEXTE'.and.comn.ne.'PCA')then
        call fcecho(' ')
        call fcecho('INSTRUME is neither HEXTE nor PCA, it is')
        call fcecho(comn)
        call fcecho('Cannot continue - enter COLUMNS by name')
        labort=.TRUE.
      endif
      
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('COLUMNS=GOOD but INSTRUME keyword not found')
        call fcecho('Cannot continue - enter COLUMNS by name')
        labort=.TRUE.        
        status=0
      endif

      if(ldryrun)then
        call fcecho(' ')
        call fcecho('INSTRUMENT is')
        call fcecho(comn)
      endif
      
      if(labort)return

      call ftgkys(iunit,'TDDES',comn,comm,status)
      
      if(status.ne.0)then
        status=0
        call ftgkys(iunit,'TDDESC',comn,comm,status)
        status=0
      endif

c     Convert the TDDES to uppercase... 
      call ftupch(comn)
      
      outlen=fcstln(comn)

c      Since we have to read the TDDES keyword to determine all of
c      the common values that are appended to the TDDESi values
c      the first thing we need to find is the Observatory (O)- must be
c      XTE for this to work, and the Instrument (I) - either PCA or HEXTE
c      at this time. If those are found - they should be the first and
c      second items - then we will look for the Detector (D), or the
c      Group (G) depending on what the Instrument is.

c      So we will move through the entire TDDES string and set logicals
c      depending on what is found.
      do 10 i=1,outlen
        if(comn(i:i+1).eq.'O[')then
          lo=.TRUE.
          if(comn(i+1:i+1).eq.'['.and.comn(i+5:i+5).eq.']')then
            if(comn(i+2:i+4).eq.'XTE')lxte=.TRUE.
          endif
        endif

        if(comn(i:i+1).eq.'I[')then
          li=.TRUE.
          if(comn(i+1:i+1).eq.'['.and.comn(i+7:i+7).eq.']')then
            if(comn(i+2:i+6).eq.'HEXTE')lhexte=.TRUE.
          else if(comn(i+1:i+1).eq.'['.and.comn(i+5:i+5).eq.']')then
            if(comn(i+2:i+4).eq.'PCA')lpca=.TRUE.
          endif

        endif

        if(lxte.and.lpca)then
          if(comn(i:i+1).eq.'D[')then
            ld=.TRUE.
            idfirst=i+2
            if(lxte.and.lpca.and.ld.and.idfirst.ne.0)then
              if(comn(i:i).eq.']')then
                cdval=comn(idfirst:i-1)
                idfirst=0
              endif
            endif
          endif
          
        elseif(lxte.and.lhexte)then
          if(comn(i:i+1).eq.'G[')then
            lg=.TRUE.
            igfirst=i+2
            if(lxte.and.lpca.and.ld.and.igfirst.ne.0)then
              if(comn(i:i).eq.']')then
                cgval=comn(igfirst:i-1)
                igfirst=0
              endif
            endif
          endif
        endif

10    continue

      if(ldryrun)then
        call fcecho(' ')
        call fcecho('TDDES that was found was:')
        call fcecho(comn)
        if(lo)call fcecho('Found O[ in TDDES string')
        if(li)call fcecho('Found I[ in TDDES string')
        if(lg)call fcecho('Found G[ in TDDES string')
        if(ld)call fcecho('Found D[ in TDDES string')
      endif
      
      if(.not.lxte)then
        call fcecho(' ')
        call fcecho('TDDES does not have values for the XTE')
        call fcecho('observatory. Cannot continue!')
        call fcecho('Set COLUMNS to specific names.')
        labort=.TRUE.
        return
      elseif((.not.lpca).and.(.not.lhexte))then
        call fcecho(' ')
        call fcecho('TDDES does not have values for the PCA or')
        call fcecho('HEXTE instruments. Cannot continue!')
        call fcecho('Set COLUMNS to specific names.')
        labort=.TRUE.
        return
      endif        

      if(ldryrun)then
        call fcecho(' ')
        call fcecho('About to search TDDESi keywords')
        cval=' '
        call fti2c(nfield,cval,status)
        if(status.eq.0)then
          call fcecho('Number of Columns to search is:')
          call fcecho(cval)
        else
          status=0
        endif
      endif
      
c      Let's search through all FIELDS that exist in this file for
c      the Data Descriptor and compare that to formats that we know are
c      acceptable. 
      do 100 j=1,nfield

        keyword(6:8)=' '
        icol=6   
        if(j.lt.10)then
          keyword(icol:icol)=cnum(j+1)
        elseif (j.lt.100)then
          keyword(icol:icol)=cnum(j/10+1)
          keyword(icol+1:icol+1)=cnum(mod(j,10)+1)
        elseif (j.lt.1000)then
          keyword(icol:icol)=cnum(j/100+1)
          keyword(icol+1:icol+1)=cnum((mod(j,100)/10)+1)
          keyword(icol+2:icol+2)=cnum(mod(mod(j,100),10)+1)
        elseif (j.ge.1000)then
          comm='**ERROR** Column number greater than 999'
          call fcerr(comm)
        endif
          
        keyword(1:5)='TDDES'
c      Try to read TDDESi from the input file
        call ftgkys(iunit,keyword,tddess,comm,status)
        call ftupch(tddess)

        if(status.ne.0.and.ldryrun)then
          call fcecho(' ')
          call fcecho('Could not find TDDESi keyword for column')
          call fcecho(ttype(j))
        endif
        
        linclude=.FALSE.
        if(status.eq.0)then
          if(ldryrun)then
            call fcecho(' ')
            call fcecho('Found TDDESi calling parsetddes to parse')
            call fcecho(tddess)
          endif

c      Parse the TDDESi value to see if we should include it or not.
c      LINCLUDE = .TRUE. if this column is to be added, and .FALSE. if
c      this column is to be ignored.
          
          call parsetddes(tddess,linclude,ld,lg,lxte,
     &         lpca,lhexte,cdval,cgval,ldryrun)

c         if(ldryrun)call fcecho('Out of parsetddes')
          
          if(linclude)then
            icols=icols+1
            cols(icols)=ttype(j)
            if(ldryrun)then
              call fcecho(' ')
              call fcecho('Column passed acceptance criteria.')
              call fcecho('Column added was ')
              call fcecho(ttype(j))
            endif
          else
            if(ldryrun)then
              call fcecho(' ')
              call fcecho('Column failed acceptance criteria.')
              call fcecho('Column that failed was ')
              call fcecho(ttype(j))
            endif
          endif

        else

c      Reinitalize the status parameter for the next check.
          status=0
          
        endif
        
100   continue

      if(icols.ge.1)inocols=icols
      
      return
      end

c**********************************************************************  
       subroutine parsetddes(tddes,linclude,ld,lg,lxte,
     &   lpca,lhexte,cdval,cgval,ldryrun)

       implicit none
       character*(*) tddes,cdval,cgval
       character(1) csearch,cdelimits,cdelimite
       character(1000) ceval
       integer fcstln,idval,ieval,igval,iamp,ihat,ix
       logical linclude,
     &    ld,lxte,lpca,lhexte,le,lg,ldryrun,lamp,lhat,lx

       cdelimits='['
       cdelimite=']'
       lamp=.FALSE.
       lhat=.FALSE.
       lx=.FALSE.

       if(ldryrun)then
         if(lhexte)then
           call fcecho('This is HEXTE data')
         endif
         if(lpca)then
           call fcecho('This is PCA data')
         endif
       endif
       
c      The purpose of the following loop is to skip all of the
c      telemetry information which doesn't interest us.

       csearch='E'

c     Added to initialize scnstrforint call
       ceval=' '
       call scnstrforint(tddes,csearch,cdelimits,
     &    cdelimite,ceval,ieval,le)
       
       ieval=fcstln(ceval)
       if(ldryrun.and.(ieval.ge.1))then
         call fcecho('E string is')
         call fcecho(ceval(:ieval))
       endif
       
         
c       print*,'ld,le',ld,le
c       print*,'idval,ieval',idval,ieval
c       print*,'cdval,ceval',cdval,ceval

       csearch='&'
       call scnstrforchar(ceval,ieval,csearch,lamp,iamp)

       csearch='^'
       call scnstrforchar(ceval,ieval,csearch,lhat,ihat)

       csearch='X'
       call scnstrforchar(ceval,ieval,csearch,lx,ix)

       if(ldryrun)then
         call fcecho(' ')
         call fcecho('Checked TDDESi for X:')
         call fcecho(tddes)
         if(lx.and.ix.eq.1)
     &      call fcecho('Found X as first character in TDDESi')
         if(lamp)call fcecho('Found & in TDDESi string')
         if(lhat)call fcecho('Found ^ in TDDESi string')
         if(.not.lx)call fcecho('Did not find X')
       endif

       
c       print*,'lamp,lhat,lx',lamp,lhat,lx
c       print*,'iamp,ihat,ix',iamp,ihat,ix

c      Let's see if the information gotten from the TDDES defines a
c      GOOD column for PCA.
       
       if(lxte.and.lpca)then
         if(.not.ld)then
           csearch='D'

c     Added to initialize scnstrforint call           
           cdval=' '
           call scnstrforint(tddes,csearch,cdelimits,
     &        cdelimite,cdval,idval,ld)

           if(ldryrun)then
             call fcecho(' ')
             if(ld)call fcecho('Found D[ in TDDESi')
             if(.not.ld)call fcecho('Could not find D[ in TDDESi')
           endif
           
         endif

         idval=fcstln(cdval)
         if(ldryrun.and.(idval.ge.1))then
           call fcecho('D string is')
           call fcecho(cdval(:idval))
         endif

         if(ld.and.idval.eq.1)then
           if(le.and.lx.and.(ix.eq.1).and.(ieval.eq.3))then
             linclude=.TRUE.
           endif
           
           if(le.and.lx.and.(ix.eq.1).and.lhat)then
             linclude=.TRUE.
           endif
           
         elseif(ld.and.(idval.ne.1))then
           if(le.and.lx.and.(.not.lamp))then
             linclude=.TRUE.
           endif
           
         else
           linclude=.FALSE.
         endif
         
       elseif(lxte.and.lhexte)then
c         print*,'IN lxte,lhexte loop'
         
         if(.not.ld)then
           csearch='D'

c     Added to initialize scnstrforint call           
           cdval=' '
           call scnstrforint(tddes,csearch,cdelimits,
     &        cdelimite,cdval,idval,ld)

           if(ldryrun)then
             if(ld)then
               call fcecho(' ')
               call fcecho('Found D[ in TDDESi')
             else
               call fcecho(' ')
               call fcecho('Could not find D[ in TDDESi')
             endif
             
           endif

         endif

         idval=fcstln(cdval)
         if(ldryrun.and.(idval.ge.1))then
           call fcecho('D string is')           
           call fcecho(cdval(:idval))
         endif

         if(.not.lg)then
           csearch='G'

c     Added to initialize scnstrforint call
c     Added to initialize cgval before scn for int value. 
           cgval=' '
           call scnstrforint(tddes,csearch,cdelimits,
     &        cdelimite,cgval,igval,lg)
           if(ldryrun)then
             if(lg)then
               call fcecho(' ')
               call fcecho('Found G[ in TDDESi')
             else
               call fcecho(' ')
               call fcecho('Could not find G[ in TDDESi')
             endif
           endif
           
         endif
         
         igval=fcstln(cgval)
         if(ldryrun.and.(igval.ge.1))then
           call fcecho('G string is')
           call fcecho(cdval(:igval))
         endif
         
c         print*,'lg,le',lg,le
c         print*,'igval,ieval',igval,ieval
c         print*,'cgval,ceval',cgval,ceval

c         print*,'ld,le,lg are',ld,le,lg

         if(ldryrun)then
           if(ld)then
             call fcecho('Found D[ string')
           else
             call fcecho('Could not find D[ ')
           endif
           if(le)then
             call fcecho('Found E[ string')
           else
             call fcecho('Could not find E[ ')
           endif
           if(lg)then
             call fcecho('Found G[ string')
           else
             call fcecho('Could not find G[ ')
           endif
         endif
         
         if(ld.and.le.and.lg)then
           if((.not.lhat).and.(.not.lamp))linclude=.TRUE.
         else
           linclude=.FALSE.
         endif
       endif
       
c       print*,'LINCLUDE equals ',linclude
       
       return
       end

