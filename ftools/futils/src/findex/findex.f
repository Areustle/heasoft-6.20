C******************************************************************************
C SELECTOR TASK:
C      findex
C
C FILE:
C      findex.f
C
C DESCRIPTION:
C      Creates a FITS index file for a particular FITS extension
C      column
C
C AUTHOR/DATE:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C      3/10/94 EAG 2.8a Add dynamic memory, rationalize character sizes
C      8/24/94 EAG 3.0a Add clobber
C      1/2/1996 Jeff Guerber 3.0b - findex: change column to char*40.
C          fibndx: change ttype and clname to char*40; change tform(3) to 16A.
C          figsps: added note about number of columns.
C       2/25/96 (Srilal) 3.0c - timestamp added
C       10/15/97 PDW 3.0d Replace old get header routines
C
C NOTES:
C      findex supported in IRAF and HOST environments
C      complex data types currently not supported
C      Only the first 16 characters of character-string column are sorted
C      and written to the output file.
C
C USAGE:
C      HOST: call findex
C      IRAF: task findex
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      column  - name of column
C
C CALLED ROUTINES:
C      subroutine gindex - gets parameters from parameter file
C      subroutine fibndx - build index FITS file
C
C******************************************************************************
      subroutine findex
      character(160) infile, outfile
      character(40)  column

      character(40) taskname
      common /task/ taskname

      taskname = 'findex3.0d'

      call ftcmsg

C  get parameters from parameter
      call gindex(infile,outfile,column)

C  extract data to new FITS file
      call fibndx(infile,outfile,column)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gindex
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gindex uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gindex(infile,outfile,column)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file
C      column  - name of column
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************
      subroutine gindex(infile,outfile,column)
      character*(*) infile, outfile, column

      character(80) context
      integer status

C  initialize variables
      status = 0

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the column name
      call uclgst('column',column,status)
      if (status .ne. 0) then
         context = 'could not get COLUMN parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fibndx
C
C DESCRIPTION:
C      Build an index FITS file for INFILE's selected column
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C      Jeff Guerber 1/2/1996. Changed clname and ttype to char*40.
C         Changed tform(3) from A to 16A.
C
C NOTES:
C       fibndx uses FITSIO calls to read and write to FITS file
C       vector elements in tables not supported
C
C USAGE:
C      call fibndx(infile,outfile,column)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file
C      column  - name of column
C
C PRIMARY LOCAL VARIABLES:
C      filename  - name of FITS file
C      context   - error message
C      errstr    - fitsio error message string
C      history   - HISTORY keyword comment string
C      simple    - flag for simple header
C      extend    - flag for extensions
C      extnum    - FITS file extension number
C      ftstatus  - fitsio error number
C      htype     - FITS header type
C      iunit     - input unit number
C      ounit     - output unit number
C      block     - blocksize
C      bitpix    - bits per pixel
C      naxis     - number of dimensions in primary array
C      naxes     - size of each dimension in primary array
C      pcount    - value of PCOUNT keyword
C      gcount    - value of GCOUNT keyword
C
C CALLED ROUTINES:
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftcrhd - create a new header
C      subroutine ftghpr - get primary header keywords from CHU
C      subroutine ftinit - create a new FITS file
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine ftpdef - define structure of primary array
C      subroutine ftphis - put history keyword record
C      subroutine ftphpr - put primary header keywords into CHU
C      subroutine ftghtb - get ascii table header keywords into CHU
C      subroutine ftghbn - get binary table header keywords into CHU
C      subroutine fccmpl - compare to lists of strings
C      subroutine ftgcno - get column number with by column name
C      subroutine fcasfm - determine data format of ascii column
C      subroutine ftbnfm - determine data format of binary column
C      subroutine ftphtb - put ascii table header keywords into CHU
C      subroutine ftphbn - put binary table header keywords into CHU
C      subroutine ftpkyx - put keywords into CHU
C      subroutine ftmkyx - modify keywords in CHU
C      subroutine figspj - get integer data, index, sort, put in INDEX file
C      subroutine figspl - get logical data, index, sort, put in INDEX file
C      subroutine figspd - get double data, index, sort, put in INDEX file
C
C******************************************************************************
      subroutine fibndx(infile,outfile,column)
      character*(*) infile, outfile, column
      integer maxcl, maxsz
      parameter ( maxcl = 512 )
      parameter ( maxsz = 100000)

      character(160) filename
      character(80) context, errstr, history, extname
      logical simple, extend, exact, negflg, goodlist
      integer extnum, ftstatus, iunit, block, ounit, htype, bitpix,
     &     naxis, naxes(99), pcount, gcount, rowlen, nrows,
     &     tfields, clnum, varidat, dattyp, repeat, width, nd
      character(40) ttype(maxcl), clname
      character(25) tunit(maxcl)
      character(16) tform(maxcl)
      integer tbcol(maxcl), pindex, pflags, pvalues
      logical inopen,outopen, gindex, gflags, gvalues

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
      ftstatus = 0
      iunit = 15
      ounit = 16
      exact = .false.
      inopen = .false.
      outopen = .false.
      gindex = .false.
      gflags = .false.
      gvalues = .false.

C  get the filename and extension number
      call fcpars(infile,filename,extnum,ftstatus)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C  if the extension is 0 the give error and exit
      if ( extnum .eq. 0 ) then
         context = 'Primary Extension Not Supported'
         call fcerr(context)
         goto 999
      endif

C  open the input FITS file
      call ftopen(iunit,filename,0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C  prepare a simple primary header for the new file
      call ftghpr(iunit,99,simple,bitpix,naxis,naxes,pcount,
     &     gcount,extend,ftstatus)

C  move to the extension in the input file
      call ftmrhd(iunit,extnum,htype,ftstatus)
      if (ftstatus .ne. 0) then
         errstr = 'error moving to extension number '
         write(context,1000) errstr,extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

C  Get header depending on the extension type

      if ( htype .eq. 1 ) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstatus)
         call fccmpl(1,tfields,column,ttype,negflg,goodlist)
      else if ( htype .eq. 2 ) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstatus)
         call fccmpl(1,tfields,column,ttype,negflg,goodlist)
      else
         context = 'Extension type not supported'
         call fcerr(context)
         goto 999
      endif

C  determine the column number and data type
      if ( goodlist ) then
         call ftgcno(iunit,exact,column,clnum,ftstatus)
         if ( htype .eq. 1 ) then
            call fcasfm(tform(clnum),dattyp,ftstatus)
         else
            call ftbnfm(tform(clnum),dattyp,repeat,width,ftstatus)
         endif
      else
         context = 'Column not found in FITS extension'
         call fcerr(context)
         goto 999
      endif

C  check that data type is supported
      if ((dattyp .eq. 83) .or. (dattyp .eq. 163)) then
         context = 'Complex Data Types Not Supported'
         call fcerr(context)
         goto 999
      endif

C  check for string array overflow
      if (( nrows .gt. maxsz ) .and. (dattyp .eq. 16)) then
         context = 'Number of records exceeds string array size'
         call fcerr(context)
         goto 999
      endif

C  open the output FITS file
      call ffinit(ounit,outfile,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open outfile, may exist? ' // outfile
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C  construct simple primary header for INDEX file
      simple = .true.
      naxis = 0
      pcount = 0
      gcount = 1
      call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &     gcount,extend,ftstatus)
      call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,ftstatus)

C  create a new extension in the index file
      call ftcrhd(ounit,ftstatus)

C  store the column name
      clname = ttype(clnum)

C  write extension keywords to FITS file
C  Note: the meanings of ttype, tunit, and tform change here, from describing
C  the columns of the input file to those of the output file.
C  "Tform(3)" matches the string length of array "svalue" in subroutine
C  figsps, below.

      tfields = 3
      tunit(1) = ' '
      tunit(2) = ' '
      tunit(3) = ' '
      ttype(1) = 'SORTING'
      ttype(2) = 'DISTINCT'
      ttype(3) = 'VALUE'
      tform(1) = 'J'
      tform(2) = 'J'
      if ( dattyp .eq. 11 ) then
         tform(3) = 'E'
      elseif ( dattyp .eq. 14 ) then
         tform(3) = 'L'
      elseif ( dattyp .eq. 16 ) then
         tform(3) = '16A'
      elseif ( dattyp .eq. 21 ) then
         tform(3) = 'E'
      elseif ( dattyp .eq. 41 ) then
         tform(3) = 'E'
      elseif ( dattyp .eq. 42 ) then
         tform(3) = 'E'
      elseif ( dattyp .eq. 82 ) then
         tform(3) = 'D'
      endif
      varidat = 0
      call ftphbn(ounit,nrows,tfields,ttype,tform,tunit,
     &     extname,varidat,ftstatus)
      call ftbdef(ounit,tfields,tform,varidat,nrows,
     &     ftstatus)

C  write extra keywords to header, update ND later
      call ftpkys(ounit,'FILENAME',filename,
     &     'Original FITS File Name',ftstatus)
      call ftpkyj(ounit,'EXTNUMB',extnum,
     &     'Original FITS File Extension',ftstatus)
      call ftpkyj(ounit,'CLNUMBER',clnum,
     &     'Indexed Column Number',ftstatus)
      call ftpkys(ounit,'CLNAME',clname,
     &     'Indexed Column Name',ftstatus)
      call ftpkyj(ounit,'NS',nrows,
     &     'Number of Records in Index',ftstatus)
      call ftpkyj(ounit,'ND',0,
     &     'Number of Distinct Records',ftstatus)
      call ftpkyj(ounit,'NIP',1,
     &     'Number of Indexed Parameters',ftstatus)
      history = 'TASK: FINDEX on FILENAME: '//filename
      call ftphis(ounit,history,ftstatus)
      call timestamp(ounit)

      if (ftstatus .ne. 0) goto 999

C  allocate dynamic memory
C  need nrows integers to hold the index
      pindex = 0
      call udmget (nrows, 4, pindex, ftstatus)
      if (ftstatus .ne. 0) then
         call fcerr (' Error allocating dynamic memory: index')
         goto 999
      endif
      gindex = .true.

C  need nrows logical to hold flag values
      pflags = 0
      call udmget (nrows, 1, pflags, ftstatus)
      if (ftstatus .ne. 0) then
         call fcerr (' Error allocating dynamic memory: flags')
         goto 999
      endif
      gflags = .true.

C  need nrows elements to hold values.  Use doubles.  Is extra space
C  but greatly simplifies code
      pvalues = 0
      call udmget (nrows, 7, pvalues, ftstatus)
      if (ftstatus .ne. 0) then
         call fcerr (' Error allocating dynamic memory: values')
         goto 999
      endif
      gvalues = .true.

C  move data into FITS file based on data type
      if ( dattyp .eq. 11 ) then
         call figspe (iunit, ounit, clnum, nrows, nd,
     &        memi(pindex), memr(pvalues), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 14 ) then
         call figspl(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memb(pvalues), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 16 ) then
         call figsps(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 21 ) then
         call figspe(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memr(pvalues), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 41 ) then
         call figspe(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memr(pvalues), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 42 ) then
         call figspe(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memr(pvalues), memb(pflags), ftstatus)
      elseif ( dattyp .eq. 82 ) then
         call figspd(iunit,ounit,clnum,nrows,nd,
     &        memi(pindex), memd(pvalues), memb(pflags), ftstatus)
      endif

C  update nd keyword
      call ftmkyj(ounit,'ND',nd,
     &     'Number of Distinct Records',ftstatus)

C  close the FITS files, exit on fitsio error
 999  continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0

C delete output file if fitsio error
         call ftdelt (ounit, ftstatus)
         ftstatus = 0
         outopen = .false.
      endif
      if ( inopen ) call ftclos(iunit,ftstatus)
      ftstatus = 0
      if ( outopen ) call ftclos(ounit,ftstatus)

C  deallocate the dynamic memory
      ftstatus = 0
      if (gindex) call udmfre (pindex, 4, ftstatus)
      if (ftstatus .ne. 0)
     &     call fcerr (' Error deallocating index memory')
      ftstatus = 0
      if (gvalues) call udmfre (pvalues, 7, ftstatus)
      if (ftstatus .ne. 0)
     &     call fcerr (' Error deallocating values memory')
      ftstatus = 0
      if (gflags) call udmfre (pflags, 1, ftstatus)
      if (ftstatus .ne. 0)
     &     call fcerr (' Error deallocating flag memory')

      return
      end


C******************************************************************************
C SUBROUTINE:
C      figspe
C
C DESCRIPTION:
C      Get a numerical column from the input file, forms a sorted
C      image of the data as an index, and creates an index file
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call figspe(iunit,ounit,clnum,nrows,nd,ftstatus)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
C      clnum - column number to index
C      nrows - number of records(rows) in the column
C      nd    - number of distinct records in the column
C      ftstatus  - fitsio error status
C
C PRIMARY LOCAL VARIABLES:
C      evalue - string array of column records
C      index  - integer array containing the indexing
C      maxsz  - maximum number of records for a column to be processed
C
C CALLED ROUTINES:
C      subroutine ftgcfe - get real column from FITS file
C      subroutine fcidxe - index an array of  real
C      subroutine fcsrte - sort an array of  real
C      subroutine ftpclj - put an integer column in a FITS file
C      subroutine ftpcle - put a real column in a FITS file
C
C******************************************************************************
      subroutine figspe (iunit, ounit, clnum, nrows, nd, index,
     &     evalue, flagvals, ftstatus)

      integer iunit,ounit,clnum,nrows,nd,ftstatus
      integer i,l
      integer index(nrows)
      real    evalue(nrows)
      logical anyf,flagvals(nrows)

      call ftgcfe(iunit,clnum,1,1,nrows,evalue,
     &     flagvals,anyf,ftstatus)
      if ( anyf ) then
         do 10 i = 1, nrows
            if ( flagvals(i) ) then
               evalue(i) = -1.23456789e30
            endif
 10      continue
      endif
      call fcidxe(nrows,evalue,index)
      l = 1
      call ftpclj(ounit,2,l,1,1,index(1),ftstatus)
      do 20 i = 2, nrows
         if ( evalue(index(i)) .ne. evalue(index(l))) then
            l = l + 1
            call ftpclj(ounit,2,l,1,1,index(i),ftstatus)
         endif
 20   continue
      call fcsrte(nrows,evalue)
      nd = l
      call ftpclj(ounit,1,1,1,nrows,index,ftstatus)
      call ftpcle(ounit,3,1,1,nrows,evalue,ftstatus)
      if ( anyf ) then
         do 30 i = 1, nrows
            if ( flagvals(i) ) then
               call ftpclu(ounit,3,index(i),1,1,ftstatus)
            endif
 30      continue
      endif
      return
      end


C******************************************************************************
C SUBROUTINE:
C      figspl
C
C DESCRIPTION:
C      Get a logical column from the input file, forms a sorted
C      image of the data as an index, and creates an index file
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call figspl(iunit,ounit,clnum,nrows,nd,ftstatus)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
C      clnum - column number to index
C      nrows - number of records(rows) in the column
C      nd    - number of distinct records in the column
C      ftstatus  - fitsio error status
C
C PRIMARY LOCAL VARIABLES:
C      lvalue - string array of column records
C      index  - integer array containing the indexing
C      maxsz  - maximum number of records for a column to be processed
C
C CALLED ROUTINES:
C      subroutine ftgcfl - get logical column from FITS file
C      subroutine fcidxl - index an array of logicals
C      subroutine fcsrtl - sort an array of logicals
C      subroutine ftpclj - put an integer column in a FITS file
C      subroutine ftpcll - put a logical column in a FITS file
C
C******************************************************************************
      subroutine figspl(iunit,ounit,clnum,nrows,nd,index,
     &     lvalue,flagvals,ftstatus)

      integer iunit,ounit,clnum,nrows,nd,ftstatus
      integer i,l
      logical lvalue(nrows)
      integer index(nrows)
      logical anyf,flagvals(nrows)

      call ftgcfl(iunit,clnum,1,1,nrows,lvalue,
     &     flagvals,anyf,ftstatus)
      if ( anyf ) then
         do 10 i = 1, nrows
            if ( flagvals(i) ) then
               lvalue(i) = .false.
            endif
 10      continue
      endif
      call fcidxl(nrows,lvalue,index)
      l = 1
      call ftpclj(ounit,2,l,1,1,index(1),ftstatus)
      do 20 i = 2, nrows
c MJT 03July96 changed to .neqv. from .ne.
         if ( lvalue(index(i)) .neqv. lvalue(index(l))) then
            l = l + 1
            call ftpclj(ounit,2,l,1,1,index(i),ftstatus)
         endif
 20   continue
      call fcsrtl(nrows,lvalue)
      nd = l
      call ftpclj(ounit,1,1,1,nrows,index,ftstatus)
      call ftpcll(ounit,3,1,1,nrows,lvalue,ftstatus)
      if ( anyf ) then
         do 30 i = 1, nrows
            if ( flagvals(i) ) then
               call ftpclu(ounit,3,index(i),1,1,ftstatus)
            endif
 30      continue
      endif
      return
      end


C******************************************************************************
C SUBROUTINE:
C      figspd
C
C DESCRIPTION:
C      Get a double column from the input file, forms a sorted
C      image of the data as an index, and creates an index file
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call figspd(iunit,ounit,clnum,nrows,nd,ftstatus)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
C      clnum - column number to index
C      nrows - number of records(rows) in the column
C      nd    - number of distinct records in the column
C      ftstatus  - fitsio error status
C
C PRIMARY LOCAL VARIABLES:
C      dvalue - double array of column records
C      index  - integer array containing the indexing
C      maxsz  - maximum number of records for a column to be processed
C
C CALLED ROUTINES:
C      subroutine ftgcfd - get double column from FITS file
C      subroutine fcidxd - index an array of doubles
C      subroutine fcsrtd - sort an array of doubles
C      subroutine ftpclj - put an integer column in a FITS file
C      subroutine ftpcld - put a double column in a FITS file
C
C******************************************************************************
      subroutine figspd(iunit,ounit,clnum,nrows,nd,index,
     &     dvalue,flagvals,ftstatus)

      integer iunit,ounit,clnum,nrows,nd,ftstatus
      integer i,l
      double precision dvalue(nrows)
      integer index(nrows)
      logical anyf,flagvals(nrows)

      call ftgcfd(iunit,clnum,1,1,nrows,dvalue,
     &     flagvals,anyf,ftstatus)
      if ( anyf ) then
         do 10 i = 1, nrows
            if ( flagvals(i) ) then
               dvalue(i) = -2.34567891e30
            endif
 10      continue
      endif
      call fcidxd(nrows,dvalue,index)
      l = 1
      call ftpclj(ounit,2,l,1,1,index(1),ftstatus)
      do 20 i = 2, nrows
         if ( dvalue(index(i)) .ne. dvalue(index(l))) then
            l = l + 1
            call ftpclj(ounit,2,l,1,1,index(i),ftstatus)
         endif
 20   continue
      call fcsrtd(nrows,dvalue)
      nd = l
      call ftpclj(ounit,1,1,1,nrows,index,ftstatus)
      call ftpcld(ounit,3,1,1,nrows,dvalue,ftstatus)
      if ( anyf ) then
         do 30 i = 1, nrows
            if ( flagvals(i) ) then
               call ftpclu(ounit,3,index(i),1,1,ftstatus)
            endif
 30      continue
      endif
      return
      end


C******************************************************************************
C SUBROUTINE:
C      figsps
C
C DESCRIPTION:
C      Get a string column from the input file, forms a sorted
C      image of the data as an index, and creates an index file
C
C AUTHOR:
C      Kent Blackburn  1/14/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C      Only the first 16 characters of the column are sorted and
C      written into the output file.  If this is changed, also change
C      tform(3) in fibndx, above.
C
C USAGE:
C      call figsps(iunit,ounit,clnum,nrows,nd,ftstatus)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
C      clnum - column number to index
C      nrows - number of records(rows) in the column
C      nd    - number of distinct records in the column
C      ftstatus  - fitsio error status
C
C PRIMARY LOCAL VARIABLES:
C      svalue - string array of column records
C      index  - integer array containing the indexing
C      maxsz  - maximum number of records for a column to be processed
C
C CALLED ROUTINES:
C      subroutine ftgcfs - get string column from FITS file
C      subroutine fcidxs - index an array of strings
C      subroutine fcsrts - sort an array of strings
C      subroutine ftpclj - put an integer column in a FITS file
C      subroutine ftpcls - put a string column in a FITS file
C
C******************************************************************************
      subroutine figsps(iunit,ounit,clnum,nrows,nd,
     &     index,flagvals,ftstatus)

      integer iunit,ounit,clnum,nrows,nd,ftstatus
      integer i,l,maxsz
      parameter ( maxsz = 100000 )
      character(16) svalue(maxsz)
      integer index(nrows)
      logical anyf,flagvals(nrows)

      call ftgcfs(iunit,clnum,1,1,nrows,svalue,
     &     flagvals,anyf,ftstatus)
      if ( anyf ) then
         do 10 i = 1, nrows
            if ( flagvals(i) ) then
               svalue(i) = 'NULL'
            endif
 10      continue
      endif
      call fcidxs(nrows,svalue,index)
      l = 1
      call ftpclj(ounit,2,l,1,1,index(1),ftstatus)
      do 20 i = 2, nrows
         if ( svalue(index(i)) .ne. svalue(index(l))) then
            l = l + 1
            call ftpclj(ounit,2,l,1,1,index(i),ftstatus)
         endif
 20   continue
      call fcsrts(nrows,svalue)
      nd = l
      call ftpclj(ounit,1,1,1,nrows,index,ftstatus)
      call ftpcls(ounit,3,1,1,nrows,svalue,ftstatus)
      if ( anyf ) then
         do 30 i = 1, nrows
            if ( flagvals(i) ) then
               call ftpclu(ounit,3,index(i),1,1,ftstatus)
            endif
 30      continue
      endif
      return
      end
