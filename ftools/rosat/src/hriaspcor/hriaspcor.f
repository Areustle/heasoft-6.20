*+
C FTOOLS ROSAT TASK:
C      hriaspcor 
C
C FILE:
C      hriaspcor.f 
C
C DESCRIPTION: 
C     Reads an HRI events list and uses the algorithm of Jon Morse
C     to correct the standard aspect solution for detector "wobble". 
C     See (PASP, June 1994) for details of the algorithm.
C     Writes a new events list with the corrected values of X and Y 
C     (actual sky position).  If the copyall parameter is set to true,
C     the new file should be identical to the old one except for the 
C     X and Y columns in the "good" events table and the necessary 
C     keyword changes in the headers. Default values of parameters 
C     assume an RDF file.  Previous release data sets use FITS 
C     extension [3] and detector coordinate column names DX and DY.
C     It is recommended that "dewobbling" bin size should 
C     correspond to ~10", and the mincnts parameter should not be 
C     less than 100.  Note, this technique will fail for extended
C     (>~10") sources.   
C
C AUTHOR:  
C      Dr. Lawrence E. Brown/ May,June 1994
C
C MODIFICATION HISTORY:
C
C   1.1 06/01/1994      elwin Initial revision
C   1.2 03/18/1998    toliver Revised subroutine morsemake by replacing call
C                             to obsolete fitsio routine 'ftgbnh' with call to
C                             new cfitsio function 'ftghbn'
C
C NOTES:
C      hriaspcor is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input HRI event list file (rh*_bas.fits)
C    outfile       Output FITS file
C    iwobbin       Size of "dewobbling" bin (~ 10") in number of pixels
C    mincnts       Minimum number of counts in a bin (bin discard threshold)
C    xcol          Name of column for detector  x coordinates
C    ycol          Name of column for detector  y coordinates
C    xcolcor       Name of column for "deaspected"  x coordinates
C    ycolcor       Name of column for "deaspected"  y coordinates
C    xrange        X range of interest in DETECTOR coordinates
C    yrange        Y range of interest in DETECTOR coordinates
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C    jxmin,jxmax,  Min and max photon pixel locations in detector coordinates
C    jymin,jymax   (these determine the ranges for allowed "dewobbling" bins)
C    ifxmin,ifxmax, Do min and max range values not exist?  or Does program
C    ifymin,ifymax  have to find a given range parameter by itself?
C
C CALLED ROUTINES:
C      subroutine ghriaspcor - gets parameters from environment
C      subroutine dewobstack - perform the "dewobbling transformation"
C
*-
      subroutine hriasr
      implicit none

      character(160)  infile,outfile 


      logical  copyprime,copyall

      character(40) xcol,ycol,xcolcor,ycolcor
      
      integer  iwobbin,mincnts,status,chatter
      integer jxmin,jxmax,jymin,jymax
      double precision rxmin,rxmax,rymin,rymax
      logical ifxmin,ifxmax,ifymin,ifymax
      character(80) xrange,yrange

      character(40) taskname
      common /task/ taskname

      taskname = 'hriaspcor1.2'
      status = 0

C     * Get the parameters from the par file *
      call ghriaspcor(infile,outfile,iwobbin,mincnts,
     $     xcol,ycol,xcolcor,ycolcor,
     $     xrange,yrange,
     $     copyprime,copyall,chatter,status)

C     Get the x range, if present, and set logicals
      call fcgrelhri(xrange,rxmin,rxmax,ifxmin,ifxmax,status)
      if(status.ne.0) return
      jxmin=nint(rxmin)
      jxmax=nint(rxmax)

C     Get the y range, if present, and set logicals
      call fcgrelhri(yrange,rymin,rymax,ifymin,ifymax,status)
      if(status.ne.0) return
      jymin=nint(rymin)
      jymax=nint(rymax)


C     * Pass input parameters to the restacking software*
      call morsemake(infile,outfile,iwobbin,mincnts,
     $     xcol,ycol,xcolcor,ycolcor,
     $     jxmin,jxmax,jymin,jymax,ifxmin,ifxmax,ifymin,ifymax,
     $     copyprime,copyall,chatter,status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C     ghriaspcor
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:
C      Dr. Lawrence E. Brown/ May 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C      ghriaspcor uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call ghriaspcor(infile,outfile,iwobbin,mincnts,
C    $     xcol,ycol,xcolcor,ycolcor,
C    $     xrange,yrange,
C    $     copyprime,copyall,chatter,status)
C
C ARGUMENTS:
C    infile        Input HRI event list file (rh*_bas.fits)
C    outfile       Output FITS file
C    iwobbin       Size of "dewobbling" bin (~ 10") in pixels
C    mincnts       Minimum number of counts in a bin (bin discard threshold)
C    xcol          Name of column for detector  x coordinates
C    ycol          Name of column for detector  y coordinates
C    xcolcor       Name of column for "deaspected"  x coordinates
C    ycolcor       Name of column for "deaspected"  y coordinates
C    xrange        X range of interest in DETECTOR coordinates
C    yrange        Y range of interest in DETECTOR coordinates
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C    chatter       How much user info?
C
C PRIMARY LOCAL VARIABLES:
C    contxt        error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to std error
C      subroutine uclgs_ - get parameter from par file
C
C****************************************************************************** 
      subroutine ghriaspcor(infile,outfile,iwobbin,mincnts,
     $     xcol,ycol,xcolcor,ycolcor,
     $     xrange,yrange,
     $     copyprime,copyall,chatter,status)


      implicit none
      character*(*)  infile,outfile,xcol,ycol,xcolcor,ycolcor
      character*(*)  xrange,yrange
      logical  copyprime,copyall
      integer  iwobbin,mincnts,status,chatter

C  LOCAL variables
      character(80) contxt

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
        contxt = 'could not get infile parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
        contxt = 'could not get outfile parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of iwobbin for hriaspcor
      call uclgsi('iwobbin',iwobbin,status)
      if (status .ne. 0) then
        contxt = 'could not get iwobbin parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of mincnts for hriaspcor
      call uclgsi('mincnts',mincnts,status)
      if (status .ne. 0) then
        contxt = 'could not get mincnts parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of xcol for hriaspcor
      call uclgst('xcol',xcol,status)
      if (status .ne. 0) then
        contxt = 'could not get xcol parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ycol for hriaspcor
      call uclgst('ycol',ycol,status)
      if (status .ne. 0) then
        contxt = 'could not get ycol parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of xcolcor for hriaspcor
      call uclgst('xcolcor',xcolcor,status)
      if (status .ne. 0) then
        contxt = 'could not get xcolcor parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ycolcor for hriaspcor
      call uclgst('ycolcor',ycolcor,status)
      if (status .ne. 0) then
        contxt = 'could not get ycolcor parameter'
        call fcerr(contxt)
        goto 999
      endif


C  get xrange of interest 
      call uclgst('xrange',xrange,status)
      if (status .ne. 0) then
         contxt = 'could not get X_RANGE'
         call fcerr(contxt)
         goto 999
      endif

C  get yrange of interest
      call uclgst('yrange',yrange,status)
      if (status .ne. 0) then
         contxt = 'could not get Y_RANGE'
         call fcerr(contxt)
         goto 999
      endif


C  get whether to copy other keywords from primary array of infile
      call uclgsb('copyprime', copyprime, status)
      if (status .ne. 0) then
        contxt = 'could not get copyprime parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get whether to copy other extensions from the input file
      call uclgsb('copyall', copyall, status)
      if (status .ne. 0) then
        contxt = 'could not get copyall parameter'
        call fcerr(contxt)
        goto 999
      endif

c Get the chattiness flag
      call uclgsi('chatter',chatter, status)
      if(status.NE.0) then
         contxt = 'Error getting CHATTER parameter'
         call fcecho(contxt)
         status = 0 
         contxt = 'Setting CHATTER = 10'
         call fcecho(contxt)
         chatter = 10
      endif      

 999  continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C     morsemake
C
C DESCRIPTION: 
C     Main control routine to do Morse's algorithm to correct aspect 
C     solution errors in HRI photon events lists
C
C AUTHOR:
C      Lawrence E. Brown/ June 1994
C
C MODIFICATION HISTORY:
C
C 03/18/1998    toliver Replaced call to obsolete fitsio routine 'ftgbnh' 
C                       with call to new cfitsio function 'ftghbn'
C
C NOTES:
C
C USAGE:
C      call morsemake(infile,outfile,iwobbin,mincnts,
C     $     xcol,ycol,xcolcor,ycolcor,
C     $     jxmin,jxmax,jymin,jymax,ifxmin,ifxmax,ifymin,ifymax,
C     $     copyprime,copyall,chatter,status)
C
C ARGUMENTS:
C    infile        Input HRI event list file (rh*_bas.fits)
C    outfile       Output FITS file
C    iwobbin       Size of "dewobbling" bin (~ 10") in pixels
C    mincnts       Minimum number of counts in a bin (bin discard threshold)
C    xcol          Name of column for detector  x coordinates
C    ycol          Name of column for detector  y coordinates
C    xcolcor       Name of column for "deaspected"  x coordinates
C    ycolcor       Name of column for "deaspected"  y coordinates
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C PRIMARY LOCAL VARIABLES:
C    xcentroidpt,ycentroidpt      pointers to arrays for collecting the 
C                                 sums for the centroids of each "dewobbling"
C                                 bin
C    dewobcntpt                   pointer to the array for recording how many
C                                 photons were in each "dewobbling" bin
C    xmaincentroid,ymaincentroid  scalars which hold the centroid for all 
C                                 photons which fall in a valid "dewobbling"
C                                 bin (i.e. one with > mincnts counts)
C
C CALLED ROUTINES:
C     fcpars   parses the filenames
C     ffinit   initializes the outfile
C     fcerr    sends error messages
C     ftopen   opens the infile
C     ftmahd   moves CHDU to an absolute HDU
C     ftghbn   gets binary table header parameters
C     fccmpl   compares strings (checks to see if column names are valid)
C     ftgcno   gets column number given name
C     ftkeyn   makes numbered keyname
C     ftgkyj   get keyword value
C     ftgcfj   get a table entry
C     udmget   allocate dynamic memory
C     dewob    performs the binning and centroid calculations for the aspect
C              correction (first pass through infile)
C     wrdewob  corrects each valid photon and writes it to outfile (second
C              pass through infile)
C     udmfre   deallocates dynamic memory
C     ftclos   closes fits files
C
C****************************************************************************** 
      
      subroutine morsemake(infile,outfile,iwobbin,mincnts,
     $     xcol,ycol,xcolcor,ycolcor,
     $     jxmin,jxmax,jymin,jymax,ifxmin,ifxmax,ifymin,ifymax,
     $     copyprime,copyall,chatter,status)
      implicit none
      character(160)  infile,outfile
      logical  copyprime,copyall
      character(40) xcol,ycol,xcolcor,ycolcor
      integer  iwobbin,mincnts,status,chatter
      integer jxmin,jxmax,jymin,jymax
      logical ifxmin,ifxmax,ifymin,ifymax
C******************************************************************************
C    the following MEM common block definition is in the system iraf77.inc file
C    and is used for dynamic memory allocation 
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
C******************************************************************************

C     LOCAL Variables
      character(160)  filnam
      character(80) context,errstr,extnam,comment
      integer maxcl
      logical exact,negflag
      integer extnum,ftstat,iunit,ounit,block,htype
      integer nrows,tfields,varidat
      integer xcolno,ycolno,xcolcorno,ycolcorno
      integer dewobsize,jxsize,jysize
      integer datatype,xmaincentroid,ymaincentroid
      integer dewobcntpt,ycentroidpt,xcentroidpt,n
      integer irow,felem,nelem,rawx,rawy
      logical allocok,flagvals,anyf
      logical xstat,ystat
      integer tjxmin,tjxmax,tjymin,tjymax
      parameter (maxcl = 512)
      character(16) ttype(maxcl), tform(maxcl)
      character(25) tunit(maxcl)
      character(8) kyxmin,kyxmax,kyymin,kyymax
C     initialize
      exact = .false.
      ftstat = 0
      iunit = 15
      ounit = 16
      negflag = .false.
      allocok= .false.
      xmaincentroid=0
      ymaincentroid=0

C  get the output FITS filename and extension number
      call fcpars(outfile,filnam,extnum,ftstat)

C  2 is the default extension # for HRI data sets
      if (extnum .eq. -99) extnum = 2

C  initialize the output FITS file
      call ffinit(ounit,filnam,ftstat)
      if (ftstat .ne. 0) then
         context = 'unable to open output file'
         call fcerr(context)
         goto 999
      endif
C  get the input FITS filename and extension number
      call fcpars(infile,filnam,extnum,ftstat)

C  2 is the default extension # for HRI data sets
      if (extnum .eq. -99) extnum = 2

C  open the input FITS file
      call ftopen(iunit,filnam,0,block,ftstat)
      if (ftstat .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif

C  move to the extension number
      call ftmahd(iunit,extnum+1,htype,ftstat)
      if (ftstat .ne. 0) then
         errstr = 'error moving to extension number '
         write(context,1000) errstr, extnum
         call fcerr(context)
         goto 999
      endif

C  get the header depending on the extension type
      if (htype .ne. 2) then
         context = ' extension must be binary table'
         call fcerr (context)
         goto 999
      endif
      call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &     extnam,varidat,ftstat)


C  Check if xcol and ycol(columns) exist.
      if (xcol .eq. ' ' .or. ycol .eq. ' ') then
         errstr = 'error in column name'
         call fcerr(errstr)
         goto 999
      endif
      n=1
      call fccmpl(n,tfields,xcol,ttype,negflag,xstat)
      n=1
      call fccmpl(n,tfields,ycol,ttype,negflag,ystat) 
      if ((.not.xstat) .or. (.not. ystat)) then
         errstr = 'error in column names'
         call fcerr(errstr)
         goto 999
      endif

C get the column numbers

      call ftgcno(iunit,exact,xcol,xcolno,ftstat)
      call ftgcno(iunit,exact,ycol,ycolno,ftstat)
C Do we have to find ranges?
      if(ifxmin.or.ifxmax.or.ifymin.or.ifymax) then
C yes, get the ranges of pixel values from header keywords TLMINn,TLMAXn
         call ftkeyn('TLMIN',xcolno,kyxmin,ftstat)
         call ftgkyj(iunit,kyxmin,tjxmin,comment,ftstat)
         call ftkeyn('TLMAX',xcolno,kyxmax,ftstat)
         call ftgkyj(iunit,kyxmax,tjxmax,comment,ftstat)
         call ftkeyn('TLMIN',ycolno,kyymin,ftstat)
         call ftgkyj(iunit,kyymin,tjymin,comment,ftstat)
         call ftkeyn('TLMAX',ycolno,kyymax,ftstat)
         call ftgkyj(iunit,kyymax,tjymax,comment,ftstat)
         if (ftstat .ne. 0) then
c     have to go find ranges ourselves
            ftstat=0
            tjxmax=0
            tjymax=0
            call ftgcfj(iunit,xcolno,irow,felem,nelem,rawx,flagvals,
     &           anyf,ftstat)
            tjxmin=rawx
            call ftgcfj(iunit,ycolno,irow,felem,nelem,rawy,flagvals,
     &           anyf,ftstat)
            tjymin=rawy
            felem = 1
            nelem = 1
            do irow=1,nrows
               call ftgcfj(iunit,xcolno,irow,felem,nelem,rawx,flagvals,
     &              anyf,ftstat)
               tjxmin=min(rawx,tjxmin)
               tjxmax=max(rawx,tjxmax)
               call ftgcfj(iunit,ycolno,irow,felem,nelem,rawy,flagvals,
     &              anyf,ftstat)
               tjymin=min(rawy,tjymin)
               tjymax=max(rawy,tjymax)
            enddo
         endif
         if(ifxmin)jxmin=tjxmin
         if(ifxmax)jxmax=tjxmax
         if(ifymin)jymin=tjymin
         if(ifymax)jymax=tjymax
      endif
      jxsize=max((jxmax-jxmin)/iwobbin,1)
      jysize=max((jymax-jymin)/iwobbin,1)
      dewobsize=jxsize*jysize

C create dynamic arrays for x and y  centroid sums and count sums
C for the dewobbling bins
      xcentroidpt = 0
      ycentroidpt = 0
      dewobcntpt = 0
      datatype=4
      call udmget(dewobsize,datatype,xcentroidpt,ftstat)
      call udmget(dewobsize,datatype,ycentroidpt,ftstat)
      call udmget(dewobsize,datatype,dewobcntpt,ftstat)
      if (ftstat .ne. 0) then
         context = ' Error dynamically allocating arrays'
         call fcerr (context)
         goto 999
      endif
      allocok=.true.

C  Check if xcolcor and ycolcor(columns) exist.
      if (xcolcor .eq. ' ' .or. ycolcor .eq. ' ') then
         errstr = 'error in column name'
         call fcerr(errstr)
         goto 999
      endif
      n=1
      call fccmpl(n,tfields,xcolcor,ttype,negflag,xstat)
      n=1
      call fccmpl(n,tfields,ycolcor,ttype,negflag,ystat) 
      if ((.not.xstat) .or. (.not.ystat)) then
         errstr = 'error in column names'
         call fcerr(errstr)
         goto 999
      endif

C get the column numbers

      call ftgcno(iunit,exact,xcolcor,xcolcorno,ftstat)
      call ftgcno(iunit,exact,ycolcor,ycolcorno,ftstat)
      if(ftstat.ne.0) then
         errstr = 'error getting column numbers'
         call fcerr(errstr)
         goto 999
      endif

C find the x and y centroids and total counts for each dewobbling bin
      call dewob(iunit,iwobbin,
     $     jxsize,jysize,jxmin,jymin,jxmax,jymax,mincnts,
     $     memi(xcentroidpt),memi(ycentroidpt),memi(dewobcntpt),
     $     xmaincentroid,ymaincentroid,
     $     xcolno,ycolno,xcolcorno,ycolcorno,nrows,chatter,status)
      if(status.ne.0) then
         errstr = 'error doing the dewobbling calculation'
         call fcerr(errstr)
         call fcerrm(status)
         goto 999
      endif
C write a new table with the corrected positions in X and Y columns
      call wrdewob(iunit,ounit,iwobbin,mincnts,memi(xcentroidpt),
     $     memi(ycentroidpt),memi(dewobcntpt),
     $     jxsize,jysize,jxmin,jymin,jxmax,jymax,
     $     xmaincentroid,ymaincentroid,
     $     xcolno,ycolno,
     $     xcolcorno,ycolcorno,nrows,copyprime,copyall,extnum,
     $     infile,
     $     status)
      if(status.ne.0) then
         errstr = 'error doing the restacking or writing'
         call fcerr(errstr)
         call fcerrm(status)
         goto 999
      endif

 999  continue
      if(allocok) then
         call udmfre (xcentroidpt, datatype, ftstat)
         call udmfre (ycentroidpt, datatype, ftstat)
         call udmfre (dewobcntpt, datatype, ftstat)
         if(ftstat.ne.0) then
            errstr = 'error deallocating dynamic memory'
            call fcerr(errstr)
         endif
      endif
      call ftclos(iunit,ftstat)
      call ftclos(ounit,ftstat)
      if(ftstat.ne.0) then
         errstr = 'error closing FITS files'
         call fcerr(errstr)
      endif
 1000 format(A34,I3)
      return
      end
      
C******************************************************************************
C SUBROUTINE:
C     dewob
C
C DESCRIPTION: 
C     Calculates the binning and centroiding parameters for Morse's aspect
C     correction algorithm for HRI
C
C AUTHOR:
C      Lawrence E. Brown/ June 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call dewob(iunit,iwobbin,
C     $     jxsize,jysize,jxmin,jymin,jxmax,jymax,mincnts,
C     $     xcentroidpt),memi(ycentroidpt),memi(dewobcntpt),
C     $     xmaincentroid,ymaincentroid,
C     $     xcolno,ycolno,xcolcorno,ycolcorno,nrows,chatter,status)
C
C ARGUMENTS:
C     iunit         Input file unit
C     iwobbin       Size of "dewobbling" bin (~ 10") in pixels
C     jxsize,jysize Size of the arrays
C     jxmin,jxmax,  Min and max photon pixel locations in detector coordinates
C     jymin,jymax   (these determine the ranges for allowed "dewobbling" bins)
C     mincnts       Minimum number of counts in a bin (bin discard threshold)
C     xcentroid,    Arrays for collecting sums for finding the centroids of 
C     ycentroid     "dewobbling" bins
C     dewobcnt      Array for counting the number of photons in a "dewobbling" 
C                   bin
C     xmaincentroid,  scalars which hold the centroid for all 
C     ymaincentroid   photons which fall in a valid "dewobbling"
C                     bin (i.e. one with > mincnts counts)
C     xcolno        Number of column for detector  x coordinates
C     ycolno        Number of column for detector  y coordinates
C     xcolcorno     Number of column for "deaspected"  x coordinates
C     ycolcorno     Number of column for "deaspected"  y coordinates
C     nrows         Size of table
C     status        Returns wether routine succeeded
C
C PRIMARY LOCAL VARIABLES:
C     irow          Row counter for scanning through table
C
C CALLED ROUTINES:
C     ftgcfj   get a table entry
C
C******************************************************************************
      
      subroutine dewob(iunit,iwobbin,
     $     jxsize,jysize,jxmin,jymin,jxmax,jymax,mincnts,
     $     xcentroid,ycentroid,dewobcnt,
     $     xmaincentroid,ymaincentroid,
     $     xcolno,ycolno,xcolcorno,ycolcorno,nrows,chatter,status)
      implicit none
      integer iunit,iwobbin,chatter
      integer jxsize,jysize,jxmin,jymin,jxmax,jymax,mincnts
      integer xcentroid(jxsize,jysize),ycentroid(jxsize,jysize)
      integer dewobcnt(jxsize,jysize)
      integer xmaincentroid,ymaincentroid
      integer xcolno,ycolno,xcolcorno,ycolcorno,nrows,status
C     LOCAL variables
      integer ftstat,felem,irow,nelem,rawx,rawy,x,y,xwobin,ywobin
      integer n,ix,iy
      logical flagvals,anyf
      character(160) context
      
      do ix=1,jxsize
         do iy=1,jysize
            xcentroid(ix,iy)=0
            ycentroid(ix,iy)=0
            dewobcnt(ix,iy)=0
         enddo
      enddo

C Initialize status
      ftstat = 0

C Remember, for all the binning, the arithmetic should be integer
      felem = 1
      nelem = 1
      do 90 irow=1,nrows
C Read xraw  value with ftgcfj
         call ftgcfj(iunit,xcolno,irow,felem,nelem,rawx,flagvals,
     &        anyf,ftstat)
         if(rawx.lt.jxmin.or.rawx.gt.jxmax) goto 90
C Find out what "wobble" bin it's in
         xwobin=(rawx-jxmin)/iwobbin+1
C Read yraw  value with ftgcfj
         call ftgcfj(iunit,ycolno,irow,felem,nelem,rawy,flagvals,
     &        anyf,ftstat)
         if(rawy.lt.jymin.or.rawy.gt.jymax) goto 90
C Find out what "wobble" bin it's in
         ywobin=(rawy-jymin)/iwobbin+1
C Add standard aspect corrected position to centroid sums for that bin         
         call ftgcfj(iunit,xcolcorno,irow,felem,nelem,x,flagvals,
     &        anyf,ftstat)
         xcentroid(xwobin,ywobin)=xcentroid(xwobin,ywobin)+x
         call ftgcfj(iunit,ycolcorno,irow,felem,nelem,y,flagvals,
     &        anyf,ftstat)
         ycentroid(xwobin,ywobin)=ycentroid(xwobin,ywobin)+y
         dewobcnt(xwobin,ywobin)=dewobcnt(xwobin,ywobin)+1
 90   continue
      n=0
      do 10 ix=max(1,jxmin/iwobbin),jxmax/iwobbin
         do 20 iy=max(1,jymin/iwobbin),jymax/iwobbin
            if(dewobcnt(ix,iy).lt.mincnts) goto 20
            xmaincentroid=xmaincentroid+xcentroid(ix,iy)
            ymaincentroid=ymaincentroid+ycentroid(ix,iy)
            n=n+dewobcnt(ix,iy)
 20      continue
 10   continue
      if (n.ne.0) then
         xmaincentroid=nint(real(xmaincentroid)/real(n))
         ymaincentroid=nint(real(ymaincentroid)/real(n))
      else 
         if(chatter.ge.10) then
            write(context,'(a,i4,a)') 
     $           '0 output events.  No dewobbling bin contains'
     $           ,mincnts,' events.'
            call fcecho(context)
            context='Adjust mincnts or iwobbin parameter.'
            call fcecho(context)
         endif
      endif
      status=ftstat
      return
      end




      
C******************************************************************************
C SUBROUTINE:
C     wrdewob
C
C DESCRIPTION: 
C     Applies centroid shift to each photon and writes the event record
C     to the output file
C
C AUTHOR:
C      Lawrence E. Brown/ June 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call wrdewob(iunit,ounit,iwobbin,mincnts,xcentroid,
C     $     ycentroid,dewobcnt,
C     $     jxsize,jysize,jxmin,jymin,jxmax,jymax
C     $     xmaincentroid,ymaincentroid,
C     $     xcolno,ycolno,
C     $     xcolcorno,ycolcorno,nrows,copyprime,copyall,extnum,
C     $     infile,
C     $     status)
C ARGUMENTS:
C     iunit         Input file unit
C     ounit         Output file unit
C     iwobbin       Size of "dewobbling" bin (~ 10") in pixels
C     mincnts       Minimum number of counts in a bin (bin discard threshold)
C     xcentroid,    Arrays for collecting sums for finding the centroids of 
C     ycentroid     "dewobbling" bins
C     dewobcnt      Array for counting the number of photons in a "dewobbling" 
C                   bin
C     jxsize,jysize Size of the arrays
C     jxmin,jxmax,  Min and max photon pixel locations in detector coordinates
C     jymin,jymax   (these determine the ranges for allowed "dewobbling" bins)
C     xmaincentroid,  scalars which hold the centroid for all 
C     ymaincentroid   photons which fall in a valid "dewobbling"
C                     bin (i.e. one with > mincnts counts)
C     xcolno        Number of column for detector  x coordinates
C     ycolno        Number of column for detector  y coordinates
C     xcolcorno     Number of column for "deaspected"  x coordinates
C     ycolcorno     Number of column for "deaspected"  y coordinates
C     nrows         Size of table
C     copyprime     Copy keywords from primary array of infile?
C     copyall       Copy all other extensions in infile?
C     extnum        Extension number of the photon list in the infile (PHDU=0)
C     infile        Input file name (used for HISTORY record in outfile)
C     status        Returns wether routine succeeded
C
C PRIMARY LOCAL VARIABLES:
C     irow          Row counter for scanning through table
C     newrows       Counts valid photons and thus will contain the size of 
C                   the output table upon completion
C
C CALLED ROUTINES:
C     copyfirst  Copies all the HDUs before the specified extension
C     copylast   Copies all the HDUs after the CHDU
C     ftmahd     Moves CHDU to an absolute HDU
C     ftcrhd     Creates a new HDU
C     ftghbn     Get binary table header keywords
C     ftphbn     Put binary table header keywords
C     ftbdef     Define binary table header 
C     xcopyscale Copy all extra keywords in an HDU
C     ftgcfj     Get a table entry
C     ftgkyj     Get keyword value
C     fcopyr     Copy a table row from infile to outfile
C     ftpclj     Write an entry to output table
C     ftmkyj     Change the value for a keyword
C     ftkeyn     Make a numbered keyword
C     ftmcom     Change the comment for a keyword
C     ftphis     Write a history record
C     ftrdef     Redefine the CHDU
C
C******************************************************************************

      subroutine wrdewob(iunit,ounit,iwobbin,mincnts,xcentroid,
     $     ycentroid,dewobcnt,
     $     jxsize,jysize,jxmin,jymin,jxmax,jymax,
     $     xmaincentroid,ymaincentroid,
     $     xcolno,ycolno,
     $     xcolcorno,ycolcorno,nrows,copyprime,copyall,extnum,
     $     infile,
     $     status)
      implicit none
      character(160)  infile
      integer jxsize,jysize,jxmin,jymin,jxmax,jymax
      integer iunit,ounit,iwobbin,mincnts
      integer xcentroid(jxsize,jysize),ycentroid(jxsize,jysize)
      integer dewobcnt(jxsize,jysize)
      integer xmaincentroid,ymaincentroid
      integer xcolno,ycolno
      integer xcolcorno,ycolcorno,nrows,extnum
      logical copyprime,copyall
      integer status
C     LOCAL variables
      integer ftstat,felem,irow,nelem,morekeys,delta,htype
      integer rawx,rawy,x,y,xwobin,ywobin,newrows,width
      logical flagvals,anyf
      character(80) histrec(3),comment
      character(8) keynam
      integer maxcl
      parameter (maxcl = 512)
      character(16) ttype(maxcl), tform(maxcl)
      character(25) tunit(maxcl)
      character(80) extnam
      integer krows,tfields,varidat
      histrec(1)='Created by hriaspcor from '//infile
      histrec(2)='Aspect corrected positions in the event list have'
      histrec(3)='been modified using the algorithm of Jon Morse'
      ftstat = 0
C     Copy the primary hdu and all the others if specified
      call copyfirst(iunit,ounit,extnum,copyprime,copyall,
     $     .true.,3,histrec,ftstat)
      morekeys=3
C     copy the required and non-required keywords and define the 
C     HDU that we're gonna work on
      call ftmahd(iunit,extnum+1,htype,ftstat)
      call ftcrhd(ounit,ftstat)
      call ftghbn(iunit,maxcl,krows,tfields,ttype,tform,tunit,
     $     extnam,varidat,ftstat)
      call ftphbn(ounit,krows,tfields,ttype,tform,tunit,
     $     extnam,varidat,ftstat)
      call ftbdef(ounit,tfields,tform,varidat,krows,ftstat)
      call xcopyscale(iunit,ounit,ftstat)
C     get NAXIS1 keyword for copying rows
      call ftgkyj(iunit,'NAXIS1',width,comment,ftstat)
      felem = 1
      nelem = 1
      newrows=0
      do 10 irow=1,nrows
C Read xraw  value with ftgcfj
         call ftgcfj(iunit,xcolno,irow,felem,nelem,rawx,flagvals,
     &        anyf,ftstat)
         if(rawx.lt.jxmin.or.rawx.gt.jxmax) goto 10
C Find out what x "wobble" bin it's in
         xwobin=(rawx-jxmin)/iwobbin+1
C Read yraw  value with ftgcfj
         call ftgcfj(iunit,ycolno,irow,felem,nelem,rawy,flagvals,
     &        anyf,ftstat)
         if(rawy.lt.jymin.or.rawy.gt.jymax) goto 10
C Find out what y "wobble" bin it's in
         ywobin=(rawy-jymin)/iwobbin+1
C Don't use photons from very low population wobble bins
         if(dewobcnt(xwobin,ywobin).lt.mincnts) goto 10
         newrows=newrows+1
C Copy entire current row
         call fcopyr(iunit,irow,ounit,newrows,width,ftstat)
C Adjust x position
         call ftgcfj(iunit,xcolcorno,irow,felem,nelem,x,flagvals,
     &        anyf,ftstat)
         delta=(xcentroid(xwobin,ywobin)/dewobcnt(xwobin,ywobin)
     $        -xmaincentroid)
         x=x-delta
         call ftpclj(ounit,xcolcorno,newrows,felem,nelem,x,ftstat)
C Adjust y position
         call ftgcfj(iunit,ycolcorno,irow,felem,nelem,y,flagvals,
     &        anyf,ftstat)
         delta=(ycentroid(xwobin,ywobin)/dewobcnt(xwobin,ywobin)
     $        -ymaincentroid)
         y=y-delta
         call ftpclj(ounit,ycolcorno,newrows,felem,nelem,y,ftstat)
 10   continue
      call ftmkyj(ounit,'NAXIS2',newrows,'&',ftstat)
C     change the X and Y column comments in the output file
      call ftkeyn('TTYPE',xcolcorno,keynam,ftstat)
      comment='X position of photon calculated by hriaspcor'
      call ftmcom(ounit,keynam,comment,ftstat)
      call ftkeyn('TTYPE',ycolcorno,keynam,ftstat)
      comment='Y position of photon calculated by hriaspcor'
      call ftmcom(ounit,keynam,comment,ftstat)
      call ftphis(ounit,histrec(1),ftstat)
      call ftphis(ounit,histrec(2),ftstat)
      call ftphis(ounit,histrec(3),ftstat)
C      reinitialize the CHDU with its new length
      call ftrdef(ounit,ftstat)
      if (copyall) call copylast(iunit,ounit,ftstat)
      if(ftstat.ne.0) status=ftstat
      return
      end




C******************************************************************************
C SUBROUTINE:
C      fcgrelhri
C
C DESCRIPTION:
C      Gets the min and max from a range
C       
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C modified from:  fcgrgs
C      Janice Tarrant  12/24/91
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C       this routine takes a string containing 2 (or less) values
C       seperated by a ,.  For example 2.3,4.3 will return a minimum
C       of 2.3 and a maximum of 4.3.  Leaving one part of the range
C       blank will set the corresponding "if" flag.  For example,
C       ,5.34 will return a maximum of 5.34 and the ifmin flag will be
C       .true.  Just a , or a blank (or other non-numeric character,
C       such as a - ) will return both ifmin and ifmax 
C       equal to .true.
C
C USAGE:
C      call fcgrelhri (instring,range1,range2,ifmin,ifmax,status)
C
C ARGUMENTS:
C      instring  - string list of ranges to translate
C      range1    - array of minima for each range
C      range2    - array of maxima for each range
C      ifmin     - whether program has to find minimum
C      ifmax     - whether program has to find maximum
C      status    - status of the range reading
C
C PRIMARY LOCAL VARIABLES:
C      rowlist     - array of separate ranges
C      range       - range flag, indicates single or range
C      rangelen    - length of range string
C      rangei      - position of range delimiter in range
C
C CALLED ROUTINES:
C      function   fcstln - returns length of character string (integer)
C
C******************************************************************************
      subroutine fcgrelhri(instring,range1,range2,ifmin,ifmax,status)

      character*(*)   instring
      character(30)    context

      integer         j 
      integer         fcstln
      integer         rangei
      integer         rangelen
      integer         status

      double precision                range1  
      double precision                range2

      logical         ifmin
      logical         ifmax

      logical         range
C INIT
      rangei = 0

C remove any blank characters
      call frmblk(instring)

C  search for "," range separator
      rangelen = fcstln(instring)

C check for blank - find min, max from data
      range = .false.
      if (rangelen .eq. 0) goto 40

      do 30 j = 1, rangelen
         if (instring(j:j) .eq. ',') then
            rangei = j
            range = .true.
         endif
 30   continue

 40   if ((rangei .eq.  1) .and. (rangelen .eq. 1)) 
     +     range = .false.

C get the min and max out of the range
      ifmin = .false.
      ifmax = .false.

      if (range) then
         if (rangei .eq. 1) then
            ifmin = .true.
            if (instring(2:6) .eq. 'INDEF') then
               ifmax = .true.
            else
               read(instring(2:rangelen),1000,err=999)
     &              range2
            endif
         else if (rangei .eq. rangelen) then
            if (instring(1:5) .eq. 'INDEF') then
               ifmin = .true.
            else
               read(instring(1:rangelen-1),1000,err=999)
     &              range1
            endif
            ifmax = .true.
         else
            if (instring(1:5) .eq. 'INDEF') then
               ifmin = .true.
            else
               read(instring(1:rangei-1),1000,err=999)
     &              range1
            endif
            if (instring(rangei+1:rangei+5) .eq. 'INDEF') then
               ifmax = .true.
            else
               read(instring(rangei+1:rangelen),1000,err=999)
     &              range2
            endif
         endif
      else
         ifmin = .true.
         ifmax = .true.
      endif

      return

 999  status = 1
      context = ' error reading range'
      call fcerr (context)

 1000 format(f12.0)
      return
      end

C******************************************************************************
