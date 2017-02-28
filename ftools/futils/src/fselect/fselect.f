C******************************************************************************
C SELECTOR TASK:
C      fselect
C
C FILE:
C      fselect.f
C
C DESCRIPTION:
C      Selects rows from a FITS file for a particular expression
C
C AUTHOR/DATE:
C      Kent Blackburn  1/31/92
C
C MODIFICATION HISTORY:
C       11 January 1993 (EAG) Added copyall parameter
C       9/9/94 EAG 3.0a - add FFINIT
C       February 14, 1995 (JKB) Added BIT support (version ?)
C       2/25/96 (Srilal) 3.3e - timestamp added
C
C       Banashree M Seifert (August, 1997)4.0
C          . introduced XCLOCK so that it writes on screen '% completed'
C            in subroutine FIMROW
C       10/15/97 PDW 4.0a Replace old get header routines
C       03/02/98 PDW 4.1  Support arbitrarily long expressions
C       05/20/98 PDW 4.2  Port to new CFITSIO parser
C       07/28/98 PDW 4.2a Change name of parser routine
C       07/28/98 PDW 4.3  Drop usage of findx and nrow parameters
C       09/17/98 PDW 4.3a Drop usage of other index variables
C       08/16/99 PDW 4.4  Remove xselet and us ftcphd instead
C
C NOTES:
C      fselect supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fselect
C      IRAF: task fselect
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      expr   - expression which evaluates to a boolean
C      histkw  - print history keyword flag
C      copyall - whether to copy all other extensions to the output file
C
C CALLED ROUTINES:
C      subroutine gselet - gets parameters from parameter file
C      subroutine fislct - select out rows 
C
C******************************************************************************

      subroutine fselet
      character(160)  infile, outfile
      character(2048) expr
      logical        histkw, copyall, cpkey
      character(40)   taskname
      common /task/ taskname

      call ftcmsg

      taskname = 'fselect4.4'
C  get parameters from parameter file
      call gselet(infile,outfile,expr,histkw,copyall,cpkey)

C  extract data to new FITS file
      call fislct(infile,outfile,expr,histkw,copyall,cpkey)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gselet
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Kent Blackburn  1/31/92
C
C MODIFICATION HISTORY:
C       8 January 1993 (EAG) added copyall parameter
C      27 July    1998 (PDW) Drop findx and nrow parameters
C
C NOTES:
C       gselet uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gselet(infile,outfile,expr,histkw,copyall)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file
C      expr    - expression which evaluates to a boolean
C      histkw  - print history keyword flag
C      copyall - whether to copy all other extensions to the output file
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

      subroutine gselet(infile,outfile,expr,histkw,copyall,cpkey)
      character*(*) infile, outfile, expr
      logical       histkw, copyall, cpkey

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

C  get the name of the output FITS file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the boolean expression 
      call uclgst('expr',expr,status)
      if (status .ne. 0) then
         context = 'could not get EXPR parameter'
         call fcerr(context)
         goto 999
      endif

C PDW 7/28/98: Drop pretense of ever adding index capabilities
C  get the name of the findx FITS file
C      call uclgst('index',findx,status)
C      if (status .ne. 0) then
C         context = 'could not get INDEX parameter'
C         call fcerr(context)
C         goto 999
C      endif

C  get the print history keyword flag
      call uclgsb('histkw',histkw,status)
      if (status .ne. 0) then
         context = 'could not get history keyword flag'
         call fcerr(context)
         goto 999
      endif

C  get the copyall extesion flag
      call uclgsb('copyall', copyall, status)
      if (status .ne. 0) then
         context = 'could not get copyall extension flag'
         call fcerr(context)
         goto 999
      endif

C  get the keycopy keyword flag
      call uclgsb('keycopy', cpkey, status)
      if (status .ne. 0) then
         context = 'could not get keycopy keyword flag'
         call fcerr(context)
         goto 999
      endif

C  get the value of the nrow variable
C PDW 7/28/98: No longer needed... CFITSIO optimizes nrows itself
C      call uclgsi('nrows',nrow,status)
C      if ( status .ne. 0 ) then
C         context = 'Could not get nrows parameter'
C         call fcerr(context)
C         goto 999
C      endif

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fislct
C
C DESCRIPTION:
C      select rows from a FITS file extension based on boolean expression 
C
C AUTHOR/DATE:
C      J. Kent Blackburn 2/3/92
C
C MODIFICATION HISTORY:
C       January, 1993 (EAG) added copyall parameter
C       March 2, 1998 (PDW) Change expr handling to allow for arbitrarily
C                           long expressions (most changes in lex/parser)
C       May 20, 1998 (PDW)  Port to new CFITSIO parser, eliminating several
C                           subroutines and simplifying data handling
C       July 28, 1998 (PDW) Change name of parser routine
C       July 28, 1998 (PDW) Drop usage of findx and nrow parameters
C       Sept 17, 1998 (PDW) Drop usage of other "index" variables
C       
C
C NOTES:
C       
C
C USAGE:
C      call fislct(infile,outfile,expr,histkw,copyall)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      findx   - optional findx FITS file
C      outfile - output FITS file 
C      expr    - boolean expression string
C      histkw  - print history keyword flag
C      copyall - whether to copy all other extensions to the output file
C      cpkey  - whether to copy all keywords in extension to output file
C      nrow    - number of rows to buffer from input file
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      history - history string 
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      outopen - output file open flag
C      eopen   - expression file open flag
C      bool    - expression evaluates to boolean flag
C      extnum  - extension number in input FITS file
C      ftstat  - fitsio library call return status
C      iunit   - input file unit number
C      ounit   - output file unit number
C      eunit   - optional expression file unit number
C      filexpr - optional filename which contains an expression
C      record  - string containing contents of one line from eunit file
C      temp    - temporary storage string
C      rlen    - length of record string
C      tlen    - length of temporary string
C      block   - fits file block size
C      htype   - fits file header type
C      bitpix  - number of bits per pixel in primary array
C      naxis   - number of dimensions of in array
C      naxes   - number of points along each dimension 
C      pcount  - value of pcount keyword
C      gcount  - value of gcount keyword
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area 
C      clnum   - column number
C      berror  - boolean error number
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ftopen - open a FITS file
C      subroutine ftinit - initialize a FITS file
C      subroutine ftmrhd - relative move to header in FITS file
C      subroutine ftghtb - get ascii table header keywords
C      subroutine ftghbn - get binary table header keywords
C      subroutine ftgkys - get character string keyword
C      subroutine ftgkyj - get integer keyword
C      subroutine ftghpr - get primary header in FITS file
C      subroutine ftphpr - put primary header in FITS file
C      subroutine ftcrhd - create header in FITS file
C      subroutine ftphtb - put ascii table header in FITS file
C      subroutine ftphbn - put binary table header in FITS file
C      subroutine ftphis - put history keyword in FITS file
C      subroutine ftclos - close a FITS file
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcstln - length of string once padded spaces removed
C
C******************************************************************************

      subroutine fislct(infile,outfile,expr,histkw,copyall,cpkey)

      character*(*) infile,outfile,expr
      logical       histkw, copyall, cpkey
      integer       maxcl
      parameter ( maxcl = 999 )
      character(160) fname,filexpr
      character(80) context,errstr
      character(512) record
      character(2048) temp,history
      logical simple,extend,exact,inopen,outopen
      logical eopen
      integer extnum,ftstat,iunit,ounit,block,htype,bitpix,rlen
      integer naxis,naxes(99),pcount,gcount,rowlen,nrows,tlen
      integer tfields,varidat,xunit,eunit
      integer elen,flen
      character(70) ttype(maxcl),tform(maxcl)
      character(70) tunit(maxcl)
      character(70) extname
      integer fcstln,tbcol(maxcl), i

C   initialize variables
      ftstat = 0
      iunit = 15
      ounit = 16
      xunit = 17
      eunit = 18
      block = 0
      exact = .true.
      inopen = .false.
      eopen = .false.
      outopen = .false.
      context = ' '
      errstr = ' '

C   get the filename and extension
      call fcpars(infile,fname,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      flen = fcstln(fname)

C   if the extension is 0 then give an error and exit
      if (extnum .lt. 1) then
         context = 'Primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C PDW 7/28/98: Delete findx section
C   check for findx file
C [... lines deleted ... ]

C   open the input FITS file
      call ftopen(iunit,fname,0,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Unable to open infile'
         call fcerr(context)
         goto 999
      endif 
      inopen = .true.

C   read in the primary array header required keywords
      call ftghpr(iunit,99,simple,bitpix,naxis,naxes,
     &     pcount,gcount,extend,ftstat)

C   open the output FITS file
      call ffinit(ounit,outfile,ftstat)
      if (ftstat .ne. 0) then
         context = 'unable to open outfile'
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

      if (copyall) then
         call ftcopy (iunit, ounit, 0, ftstat)
         if (extnum .gt. 1) then
            do 100 i = 1, extnum - 1
               call ftmrhd (iunit, 1, htype, ftstat)
               call ftcrhd (ounit, ftstat)
               call ftcopy (iunit, ounit, 0, ftstat)
 100        continue
            if (ftstat .ne. 0) then
               context = ' Error copying extensions'
               call fcerr (context)
               goto 999
            endif
         endif
      else
C   construct simple primary header for OUTPUT file
         simple = .true.
         naxis = 0
         pcount = 0
         gcount = 1
         call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &        gcount,extend,ftstat)
      endif

C   move to the extension in the input file
      call ftmahd(iunit,extnum+1,htype,ftstat)
      if ( ftstat .ne. 0 ) then
         errstr = 'Error moving to extension number '
         write(context,1000) errstr, extnum
 1000    format(A34,I3)
         call fcerr(context)
         goto 999
      endif

C   get extension header's keywords depending on extension type
      if ( htype .eq. 1 ) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstat)
      else if ( htype .eq. 2 ) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstat)
      else 
         context = 'Extension type not supported'
         call fcerr(context)
         goto 999
      endif

C   create a new extension in the output FITS file
      call ftcrhd(ounit,ftstat) 

C   write extension header's keywords depending on extension type
      if (cpkey) then
C        copy all the keywords
         call ftcphd(iunit,ounit,ftstat)
C        reset number of rows to zero
         call ftmkyj(ounit, 'NAXIS2', 0, '&', ftstat)
         call ftmkyj(ounit, 'PCOUNT', 0, '&', ftstat)
      else
         if ( htype .eq. 1 ) then
            call ftphtb(ounit,rowlen,0,tfields,ttype,tbcol,
     &           tform,tunit,extname,ftstat)
         else if ( htype .eq. 2 ) then
            call ftphbn(ounit,0,tfields,ttype,tform,tunit,
     &           extname,varidat,ftstat)
         endif
      endif

      if (histkw) then
         history = 'TASK: FSELECT on FILENAME: '//fname(1:flen)
         call ftphis(ounit,history,ftstat)
	 call timestamp(ounit)
         elen = fcstln(expr)
         history = 'Expression: '//expr(1:elen)
         call ftphis(ounit,history,ftstat)
         if ( expr(1:1).eq.'@' ) then
            filexpr = expr(2:)
            history = ' '
            open(unit=eunit,file=filexpr,err=30,status='old')
            eopen = .true.
 10         continue
              temp = history
              read(eunit,1001,end=20) record
              rlen = fcstln(record)
              tlen = fcstln(temp) + 1
              if ( tlen+rlen.gt.2048 ) then
                 call ftphis(ounit,history,ftstat)
                 history = record(1:rlen)
              else
                 history = temp(1:tlen) // record(1:rlen)
              endif
            goto 10
 1001       format(a)
 20         close(eunit)
            eopen = .false.
            call ftphis(ounit,history,ftstat)
            goto 40
 30         context = 'Unable to open expression file: '//filexpr
            call fcerr(context)
            goto 999
 40         continue
         endif
      endif

C   move rows of data which have expression evaluate to true
      call ftsrow(iunit,ounit,expr,ftstat)
      if (ftstat .ne. 0) then
         context = ' Error selecting and copying rows'
         call fcerr (context)
         goto 999
      endif

C copy the remaining extensions, if any, and if requested
      if (copyall) then
 200     call ftmrhd (iunit, 1, htype, ftstat)
         call ftcrhd (ounit, ftstat)
         call ftcopy (iunit, ounit, 0, ftstat)
         if (ftstat .ne. 0) goto 998
         goto 200
      endif

 998  ftstat = 0

 999  continue
      if ( ftstat .ne. 0 ) then
         call fcerrm(ftstat)
         ftstat = 0
         if (outopen) then
            call ftdelt (ounit, ftstat)
            outopen = .false.
            ftstat = 0
         endif
      endif
      if ( inopen ) then
         call ftclos(iunit,ftstat)
         ftstat = 0
      endif
      if ( outopen ) then
         call ftclos(ounit,ftstat)
         ftstat = 0
      endif
      if ( eopen ) then
         close(eunit)
      endif
      return
      end

