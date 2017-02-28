C******************************************************************************
C SELECTOR TASK:
C      fstatistic
C
C FILE:
C      fstat.f
C
C DESCRIPTION:
C       Compute mean,standard deviation,median,min,max for a table column
C
C AUTHOR/DATE:
C       Vidya Sagar  Jun 1992
C
C MODIFICATION HISTORY:
C     7 Oct, 1992 - added writing output values to parameter file
C       (EAG)     - fixed min/max bug
C                 - added output file capability
C                 - made output file hidden parameter 
C     9 Oct, 1992 - allow for minimum and maximum sorting before calculation
C       (EAG)     - add number of points used to hidden parameters
C                 - add row selection option
C     9/9/94 EAG 3.0a - use FAOPEN for output file
C     12/12/94 EAG 3.3a - modify for multi-dimensional columns
C     11/21/95 - (Srilal) Check for NAXIS2=0 condition
C     10/15/97 PDW 3.3c Replace old get header calls (but not used here!)
C     5/21/98 NG 3.3d- Replace ftmrhd(...,nmove,...) with ftmahd(..,nmove+1,..) 
C
C NOTES:
C      fstat supported in IRAF and HOST environments
C
C MAKE:
C      HOST: make -f mkhfstat
C      IRAF: xc -c xfstat.x fstat.f
C            xc -p stsdas xfstat.o fstat.o -lmisc -lfitsio -liraf77 -o fstat.e
C
C USAGE:
C      HOST: hfstat
C      IRAF: fstat
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      INFILE  - input FITS file and extension number
C      COLNAME  - Column value 
C      ROWS    - the rows to include in the calculation
C      OUTFILE - output FITS file and extension number
C      MAXVAL  - maximum value to include in calculation
C      MINVAL  - minimum value to include in calculation
C      nomax   - whether to test for maximum value
C      nomin   - whether to test for minimum value
C
C CALLED ROUTINES:
C      subroutine gstat - gets parameters from environment
C      subroutine figcls - gets column names from FITS file
C
C******************************************************************************

      subroutine fstatc
      character(160) infile,outfile
      character(80) rows
      character * 40 colname 
      real maxval, minval
      logical nomax, nomin
      integer status
      character(40) taskname
      common /task/ taskname

      taskname = 'fstatistic3.3d'

      call ftcmsg

C get parameters from par file
      call gstat(infile,colname,rows,outfile,maxval,minval,
     &     nomax,nomin, status) 
      if (status .ne. 0) goto 999

C read in and display column names
      call figcls(infile,colname,rows,outfile,maxval,minval,
     &     nomax,nomin) 
 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      gstat
C
C DESCRIPTION:
C      gets parameters from the parameter file 
C
C AUTHOR/DATE:
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C       gstat uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gstat(infile,colname,rows,outfile,maxval,minval,nomax,nomin) 
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      colname   - verbose value
C      rows     - the rows to include in the calculation
C      outfile  - output ASCII file containing column names
C      maxval   - the maximum value to include in calculation
C      minval   - the minimum value to include in calculation
C      nomax    - whether a maximum is requested
C      nomin    - whether a minimum is requested
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      colname  - verbose value
C      outfile  - output ASCII file containing column names
C      context - error message
C      irafsts - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine gstat(infile,colname,rows,outfile,maxval,minval,
     &     nomax, nomin, irafsts) 

      character*(*) infile,outfile, colname, rows
      character(80) context
      integer      irafsts
      real    maxval, minval
      real    hold
      logical nomin, nomax

C ---   Initialize variables 
      irafsts = 0

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the COLUMN NAME from the par file.

      call uclgst('colname',colname,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get COLNAME parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the ROWS from the par file.

      call uclgst('rows',rows,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get ROWS parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the output FITS file ---

      call uclgst('outfile',outfile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the MAXVAL parameter ---

      nomax = .false.
      call uclgsr('maxval', maxval,irafsts)
      if ( irafsts .eq. 3 ) then
         nomax = .true.
         irafsts = 0
      else if ( irafsts .ne. 0) then
         context = 'Could not get MAXVAL parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the MINVAL parameter ---

      nomin = .false.
      call uclgsr('minval', minval,irafsts)
      if ( irafsts .eq. 3 ) then
         nomin = .true.
         irafsts = 0
      else if ( irafsts .ne. 0 ) then
         context = 'Could not get MINVAL parameter'
         call fcerr(context)
         goto 999
      endif

c     MJT 26Jun2001
c     Broke this test into two parts so comparison won't be
c     done unless both quantities are defined (FPE on
c     Linux/Alpha w/g77)
c      if ((maxval .lt. minval) .and. (.not. nomin) .and. 
c     &     (.not. nomax)) then
      if ((.not. nomin) .and. (.not. nomax)) then
         if (maxval .lt. minval) then
             context = ' maximum is less than minimum - switching'
             call fcecho (context)
             hold = maxval
             maxval = minval
             minval = hold
         endif
      endif

 999  continue
      return
      end


C******************************************************************************
C SUBROUTINE:
C      figcls
C
C DESCRIPTION:
C      This subroutine actually does the reading of column names 
C      helpful to compute mean and standard deviation.
C
C AUTHOR/DATE:
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C      1/7/92 JKB Testing of extensions added 
C
C NOTES:
C       
C
C USAGE:
C      call figcls(infile,colname,rows,outfile,maxval,minval,nomax,nomin) 
C
C ARGUMENTS:
C      INFILE  - input FITS file and extension number
C      COLNAME    - Column value 
C      ROWS    - string containing the rows to use in calculations
C      OUTFILE - output ASCII file containing column names
C      MAXVAL  - the maximum value to include in the calculation
C      MINVAL  - the minimum value to include in the calculation
C      nomax   - whether to test for maximum in calculation
C      nomin   - whether minimum for inclusion specified
C
C PRIMARY LOCAL VARIABLES:
C      INFILE   - input FITS file and extension number
C      COLNAME  - Column value 
C      OUTFILE  - output ASCII file containing column names
C      extnumb  - extension number 
C      filename - input FITS filename
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcpars - parse INFILE into a filename and extension
C      subroutine ftopen - open a FITS file
C      subroutine ftmrhd - move relative to a FITS header
C      subroutine ftclos - close a FITS file
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************

      subroutine figcls(infile,colname,rows,outfile,maxval,minval,
     &     nomax, nomin) 

      integer         status
      integer         iunit,extnumb
      integer         block,nmove,hdutype
      integer         nrows
      integer         maxlst
      parameter       (maxlst = 512)
      integer         maxdim
      parameter       (maxdim = 10)
      integer         colnum
      integer         frow
      integer         nelem
      integer         felem, repeat
      integer         i, j, k
      integer         ounit
      integer         numranges, rowmin(maxlst), rowmax(maxlst)
      integer         range, naxis, naxes(maxdim)
C ---
      character * 16  csum, cmean, cstd, cmin, cmax, cnumb
      character *(*)  colname , rows
      character *(*)  infile,outfile 
      character * 160 filename
      character * 80  context
      character * 40  subset
C ---
      logical         exact
      logical         anyf, flagval
      logical         outopen
      logical         goodrows
      logical         nomax, nomin
C ---
      real            maxval, minval
C ---
      double precision        sum
      double precision        dvalues(100)
      double precision        rimin,rimax,std,mean
      double precision        minimum, maximum
      double precision        sumsquare
C ---
C ---
C Initialize variables
C ---
      iunit = 15
      ounit = 16
      status = 0
      rimin = 0.0
      rimax = 0.0
      k = 0
      sum = 0.0
      sumsquare = 0.0

C ---   Parse the INFILE into a filename and extension number ---
      call fcpars(INFILE,filename,extnumb,status)
      if (status .ne. 0) goto 999

C EAG 8/25/93 default to 1st extension
      if (extnumb .eq. -99) extnumb = 1

C ---   If the extension is 0 the give error and exit
      if ( extnumb .eq. 0 ) then
         context = 'Primary Extension Not Supported'
         call fcerr(context)
         goto 999
      endif

C ---   Open Existing input FITS file ---
      call ftopen(iunit,filename,0,block,status)
      if ( status .ne. 0 ) then
         context = ' Unable to open INFILE'
         call fcerr(context)
         goto 999
      endif
      
C ---   Move to the extension to be read ---
      nmove = extnumb
      call ftmahd(iunit,nmove+1,hdutype,status)
C ---
C ---   If status not zero then extension does not exist so exit
C ---
      if ( status .ne. 0 ) then
         context = 'Extension Not Found'
         call fcerr(context)
         goto 999
      endif

C ---   If this is not an ASCII or BINARY extension then exit
      if (( hdutype .ne. 1) .and. 
     &     ( hdutype .ne. 2).and.
     &     ( status .eq. 0 )) then
         context = 'Extension is Not Ascii or Binary'
         call fcerr(context)
         goto 999
      endif

C remove array specification, if any
      call rmsubset (1, colname, subset, status)

C ---   Now attempt getting the column number.
      exact = .false.
      call ftgcno(iunit,exact,colname, colnum, status)
      if (colnum .eq. 0 .or. status .ne. 0) then
         context = ' The column selected is not found'
         call fcerr(context)
         goto 999
      endif

C find the total number of rows
      call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
      if (nrows .eq. 0 ) then
         context = ' Input FITS table contains zero rows'
         call fcerr(context)
         goto 999
      endif

C determine felem
C if no specific element was specified do statistics on whole vector
      if (subset .eq. ' ') then
         felem = 1
      else
C if specific element was requested, just do statistics on that
         call elemparse (subset, iunit, colnum, felem, status)
      endif

C calculate the total number of point in each row
      call ftgtdm (iunit, colnum, maxdim, naxis, naxes, status)
      repeat = 1
      do 500 i = 1, naxis
         repeat = repeat*naxes(i)
 500  continue
C if it's a scalar, set subset to '-' to use most efficient routines
      if (repeat .eq. 1) subset = '-'

C ---   Next, get the requested row ranges
      call fccmpr (rows, goodrows)
      if (.not. goodrows) goto 999

      if ((rows .eq. '-') .or. (rows .eq. ' ')) then
         numranges = 1
         rowmin(1) = 1
         rowmax(1) = nrows
      else
         call fcgrgs (rows,nrows,numranges,rowmin,rowmax)
      endif

C ---  Loop over all sets of ranges

      do 2000 range = 1, numranges
         do 1500 frow = rowmin(range), rowmax(range)

            anyf = .false.
C ---   Data type is double precision.
C ---
            if (subset .eq. ' ') then
               call fgetdval (iunit, colnum, repeat, frow, 1, dvalues,
     &              nelem, status)
            else
               call ftgcfd (iunit, colnum, frow, felem, 1, dvalues,
     &              flagval, anyf, status)
               if (.not. flagval) then
                  nelem = 1
               else
                  nelem = 0
               endif
            endif
            if (status .ne. 0) goto 999
            if (nelem .ge. 1) then
               j = 1
               if ((nomin) .or. (dvalues(j) .ge. minval)) then
                  if ((nomax) .or. (dvalues(j) .le. maxval)) then
                     k = k + 1
                     if (k .eq. 1) then
                        rimin = dvalues(j)
                        rimax = rimin
                     endif
                     sum = sum + dvalues(j)
                     minimum = dmin1(rimin,dvalues(j))
                     maximum = dmax1(rimax,dvalues(j))
                     rimin = minimum
                     rimax = maximum
                     sumsquare =  sumsquare + dvalues(j) * dvalues(j)
                  endif
               endif
            endif
 1500    continue
 2000 continue
      
      mean = sum/k
C ---
C ---------------------    
C ---  Compute the standard deviation and avoid dividing by zero.
C ---    
      if (k .ne. 1) then
         std = ((sumsquare - (sum*sum)/k))/(k-1)
         std = dsqrt(std)
      endif 
C ---
      if ((outfile .ne. ' ') .and. (outfile .ne. 'STDOUT')) then
         call faopen (ounit, outfile, 2, 0, status)
         if (status .ne. 0) then
            context = ' Error opening output file, may exist? '
     &           // outfile
            call fcerr (context)
            goto 999
         else
            outopen = .true.
         endif
      else
         outopen = .false.
      endif
C ---
      write(csum,2) sum
      write(cmean,2) mean
      if (k .ne. 1) write(cstd,2) std
      if (k .eq. 1) cstd = ' undefined'
      write(cmin,2) minimum
      write(cmax,2) maximum
      write(cnumb,3) k
 2    format(1pg16.8)
 3    format(i11)
      context = ' The sum of the selected column is                '
     &     // csum
      call fprint (ounit, outopen, context)
      context = ' The mean of the selected column is               '
     &     // cmean
      call fprint (ounit, outopen, context)
      context = ' The standard deviation of the selected column is '
     &     //cstd
      call fprint (ounit, outopen, context)
      context = ' The minimum of selected column is                '
     &     // cmin
      call fprint (ounit, outopen, context)
      context = ' The maximum of selected column is                '
     &     // cmax
      call fprint (ounit, outopen, context)
      context = ' The number of points used in calculation is      '
     &     // cnumb
      call fprint (ounit, outopen, context)

C update the values in the parameter file
      call uclpsd ('sum', sum, status)
      call uclpsd ('mean', mean, status)
      call uclpsd ('sigma', std, status)
      call uclpsd ('min', minimum, status)
      call uclpsd ('max', maximum, status)
      call uclpsi ('numb', k, status)

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         status = 0
      endif
      if (outopen) close (ounit)
      call ftclos(iunit,status)

      return
      end

C******************************************************************************

