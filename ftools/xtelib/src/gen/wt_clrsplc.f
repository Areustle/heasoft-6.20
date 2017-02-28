C *******************************************************************************
C SUBROUTINE:
C      wt_clrsplc
C
C DESCRIPTION:      
C      write collimator corrected intensities to an output file, using the
C        same format as the input light curve.  Also, add a column for the
C        collimator correction values
C      
C AUTHOR:
C      James Lochner  7/28/95
C
C MODIFICATION HISTORY:
C  Feb 27, 1996 - allow RATE or COUNTS for input intensity column
C      
C NOTES:
C
C USEAGE:      
C     call wt_clrsplc(iunit, infile, inopen, reffile, outfile, nrows,
C                    rate, error, nclrsp, collresp, abort) 
C
C ARGUMENTS:
C      iunit    - logical unit for input file
C      infile   - name of input file
C      inopen   - logical variable indicating open status of input file
C      reffile  - name of a reference file to output as HISTORY
C      outfile  - name for output file
C      nrows    - total number of data points in output light curve
C      rate     - array of output intensity values
C      error    - array of output uncertainties on intensity values
C      nclrsp   - number of points in the collresp array
C      collresp - array of collimator correction values
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

      
      subroutine wt_clrsplc(iunit, infile, inopen, reffile, outfile,
     $     nrows, rate, error, nclrsp, collresp, abort) 
      
c      declarations
      
      integer iunit, ounit, frow, felem, nrows, nclrsp
      real rate(nrows), error(nrows), collresp(nclrsp)
      character*(*) infile, outfile, reffile

      character(120) history
      character(80) context
      character(8) tform(10), ttype(10), tunit(10)
      character(21) comment, author
      real nullval
      integer ftstatus, htype, extnum, pc
      integer ratepos, errpos, morekeys
      integer startno, maxkeys, ncol
      logical inopen, outopen, errcol
      logical abort

      extnum = 1
      pc = 0
      ftstatus = 0


c     Move to the primary array of the input file
      call ftmahd(iunit,1,htype,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to move to primary array' 
         call fcerr(context)
         goto 999
      endif 

c     initialize output file and copy the primary array
C       Update the checksums
      author = 'XTECOL'
      call ftgiou(ounit,ftstatus)      
      call wrtprim(iunit,ounit,inopen,outopen,outfile,author)
      call ftpcks(ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write checksums in output primary' 
         call fcerr(context)
         goto 999
      endif 
      
c      Move to the extension in the input file
      call ftmahd(iunit,2,htype,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to move to extension' 
         call fcerr(context)
         goto 999
      endif 

c      Get the column names (TTYPE) and # columns from the input file
      startno = 1
      maxkeys = 99
      call ftgkns(iunit,'TTYPE',startno,maxkeys,ttype,ncol,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get column names (TTYPE values)' 
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
      
c      Determine column numbers in the input file of the RATE and ERROR
c     columns, if present.
      errcol = .false.
      errpos = 0
      call ftgcno(iunit,.false.,'RATE',ratepos,ftstatus)
      if (ftstatus .eq. 219) then
         ftstatus = 0
         call ftgcno(iunit,.false.,'COUNTS',ratepos,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to get column number for RATE or COUNTS' 
            call fcerr(context)
            goto 999
         endif 
      endif
      
      call ftgcno(iunit,.false.,'ERROR',errpos,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to get column number for ERROR' 
         call fcerr(context)
         goto 999
      endif 
      if (errpos .gt. 0) errcol = .true.
      
c      Add a column for error, if not present and if
c      needed (noise > 0).        
c      if (noise .gt. 0 .and. errcol .eq. .false.) then
c         ncol = ncol + 1
c         errpos = ncol
c         ttype(ncol) = 'ERROR'
c         tform(ncol) = 'E'
c         tunit(ncol) = runit
c      endif
         
c     copy the entire extension from the input file to the output file
      morekeys = 10
      call ftcopy(iunit,ounit,morekeys,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not copy the extension'
         call fcerr(context)
         go to 999
      endif
      call ftrdef(ounit,fstatus)
      if (ftstatus .ne. 0) then
         context = 'error redefining the extension'
         call fcerr(context)
         go to 999
      endif

c     Modify the collimator correction keyword from False to True
      call ftmkyl(ounit,'VIGNAPP',.true.,'&',ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to modify VIGNAPP value'
         call fcerr(context)
      endif
      
c     add HISTORY,
      history = 'Input file '//infile
      call ftphis(ounit,history,ftstatus)
      history = 'Collimator Cube file '//reffile
      call ftphis(ounit,history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'WARNING: cannot write HISTORY keyword'
         call fcerr(context)
         ftstatus = 0
      endif
      
c      Modify the DATE keyword (FITS file creation date)
      call ftpdat(ounit,ftstatus)

C replace the RATE and ERROR columns with their new values
      felem = 1
      frow = 1
      nullval = -99.0
      
      call ftpcne(ounit,ratepos,frow,felem,nrows,rate,nullval,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write rate array' 
         call fcerr(context)
         goto 999
      endif
      if (errpos .gt. 0) then
         call ftpcne(ounit,errpos,frow,felem,nrows,error,nullval,
     $        ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write error array' 
            call fcerr(context)
            goto 999
         endif
      endif

C     If nclrsp = 1, add header keyword giving coll. correction value
      if (nclrsp .eq. 1) then
         comment = 'Collimator Correction Value'
         call ftpkye(ounit,'COLL_CORR',collresp(1),4,comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write coll. correction header value' 
            call fcerr(context)
            goto 999
         endif
      endif
      
C     If nclrsp > 1, Add a column for the collimator correction values
      if (nclrsp .gt. 1) then
         ncol = ncol + 1
         ttype(ncol) = 'COLLIMATOR CORRECTION'
         tform(ncol) = 'E'
         tunit(ncol) = ' '
         
         call fticol(ounit,ncol,ttype(ncol),tform(ncol),ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to insert new column array' 
            call fcerr(context)
            goto 999
         endif
            
         call ftpcle(ounit,ncol,frow,felem,nrows,collresp,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'unable to write coll. resp. array' 
            call fcerr(context)
            goto 999
         endif
      endif
         
      call ftpcks(ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to write checksums in output extension' 
         call fcerr(context)
         goto 999
      endif 
         
c      error response
999   continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
      endif
      
      if (inopen) then
         call ftclos(iunit,ftstatus)
         call ftfiou(iunit,ftstatus)
      endif
      if (outopen) then
         call ftclos(ounit,ftstatus)
         call ftfiou(ounit,ftstatus)
      endif

      
c      end subroutine
      return
      end
