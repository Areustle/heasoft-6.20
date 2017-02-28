C******************************************************************************
C SELECTOR TASK:
C      fcalc
C
C FILE:
C      fcalc.f
C
C DESCRIPTION:
C      Selects rows from a FITS file for a particular expression
C
C AUTHOR/DATE:
C      Kent Blackburn  4/20/93
C
C MODIFICATION HISTORY:
C      4/6/94 EAG 2.8a Fix scaling bug, increase speed, slightly smaller
C                      (Note dynamic memory will have minimal impact
C                       some space can be saved by shortening strings, but
C                       this propagates through misc.for, fselect, etc.)
C      8/19/94 EAG 3.0a Add clobber capability for output file
C                       Delete output file if error
C      8/30/94 EAG 3.0b Add timestamp history record
C     10/15/97 PDW 3.0c Replace old get header routines
C      2/18/98 PDW 3.1  Add fixed-length vector column support
C                       Added inull and anull parameters
C      3/02/98 PDW 3.2  Support expression files of unlimited length...
C                       pass filename to parser
C      5/20/98 PDW 3.3  Ported to use new CFITSIO parser instead of FTOOLS
C      7/28/98 PDW 3.3a Update names of parser routines
C      8/04/98 PDW 3.4  Allow for TBIT and TSTRING expression results
C      3/30/99 PDW 3.4a Fix bug in fttexp interp... repeat<0 means constant
C      9/10/99 PDW 3.5  Add rowrange parameter
C      6/13/00 PDW 3.6  Copy TDIM keywords to output extension...
C                            xcopyscale filters them out
C      6/15/00 PDW 3.7  Add tform parameter for specifying new column type
C
C NOTES:
C      fcalc supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fcalc
C      IRAF: task fcalc
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      clname  - resultant column name
C      expr    - expression which evaluates to a boolean, integer, or real
C      anull   - null string for ASCII table columns
C      inull   - null value to be used for an integer output column
C      copycol - whether to copy all other clnames to the output file
C      histkw  - print history keyword flag
C      rowrange- row range on which to operate
C      usertform-User-specified TFORM for new output column
C      copyall - whether to copy all other extensions to the output file
C
C CALLED ROUTINES:
C      subroutine gcalc - gets parameters from parameter file
C      subroutine ficalc - select out rows 
C
C******************************************************************************

      subroutine fcalc
      character(160)  infile,outfile,clname,rowrange
      character(2048) expr
      integer        inull
      logical        copycol,histkw,copyall
      character(40)   taskname
      character(70)   anull,usertform
      common /task/ taskname

      taskname = 'fcalc3.7'

      call ftcmsg

C  get parameters from parameter file
      call gcalc(infile,outfile,clname,expr,anull,inull,copycol,
     &     histkw,copyall,rowrange,usertform)

C  extract data to new FITS file
      call ficalc(infile,outfile,clname,expr,anull,inull,
     &     copycol,histkw,copyall,rowrange,usertform)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gcalc
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Kent Blackburn  4/20/93
C
C MODIFICATION HISTORY:
C      2/18/98 PDW - Added inull and anull parameters
C
C NOTES:
C       gselet uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gselet(infile,outfile,clname,expr,anull,inull,
C                  copycol,histkw,copyall)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      clname  - resultant column name
C      expr    - expression which evaluates to a boolean
C      anull   - null string for ASCII table columns
C      inull   - null value to be used for a new integer output column
C      copycol - whether to copy all other clnames to the output file
C      histkw  - print history keyword flag
C      copyall - whether to copy all other extensions to the output file
C      rowrange- row range on which to operate
C      usertform-User-specified TFORM for new output column
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

      subroutine gcalc(infile,outfile,clname,expr,anull,inull,
     &     copycol,histkw,copyall,rowrange,usertform)
      character*(*) infile, outfile, clname, expr, anull
      character*(*) rowrange,usertform
      logical       copycol, histkw, copyall
      integer       inull

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

C  get the name of the clname 
      call uclgst('clname',clname,status)
      if (status .ne. 0) then
         context = 'could not get COLUMN parameter'
         call fcerr(context)
         goto 999
      endif

C  get the arithmetic expression 
      call uclgst('expr',expr,status)
      if (status .ne. 0) then
         context = 'could not get EXPR parameter'
         call fcerr(context)
         goto 999
      endif

C  get the ASCII table null value
      call uclgst('anull',anull,status)
      if (status .ne. 0) then
         context = 'could not get ANULL parameter'
         call fcerr(context)
         goto 999
      endif

C  get the integer null value
      call uclgsi('inull',inull,status)
      if (status .ne. 0) then
         context = 'could not get INULL parameter'
         call fcerr(context)
         goto 999
      endif

C  get the copycol keyword flag
      call uclgsb('copycol', copycol,status)
      if (status .ne. 0) then
         context = 'could not get copycol keyword flag'
         call fcerr(context)
         goto 999
      endif

C  get the print history keyword flag
      call uclgsb('histkw',histkw,status)
      if (status .ne. 0) then
         context = 'could not get history keyword flag'
         call fcerr(context)
         goto 999
      endif

C  get the copyall keyword flag
      call uclgsb('copyall', copyall,status)
      if (status .ne. 0) then
         context = 'could not get copyall keyword flag'
         call fcerr(context)
         goto 999
      endif

C  get the row range
      call uclgst('rowrange',rowrange,status)
      if (status .ne. 0) then
         context = 'could not get ROWRANGE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the tform value for a new column
      call uclgst('tform',usertform,status)
      if (status .ne. 0) then
         context = 'could not get TFORM parameter'
         call fcerr(context)
         goto 999
      endif
      call ftupch(usertform)

 999  continue
      if (status .ne. 0) then
         call fcerrm(status)
         stop
      endif

      return
      end


C******************************************************************************
C SUBROUTINE:
C      ficalc
C
C DESCRIPTION:
C      Calculate a column from a FITS file extension based on expression 
C
C AUTHOR/DATE:
C      J. Kent Blackburn 4/20/92
C
C MODIFICATION HISTORY:
C      EAG 4/6/94 1.1 Use xcopyscale/noscale for copying keywords
C                     copy from input to output using bytes
C      PDW 2/18/98    Support additional clctyp return codes indicating
C                     vector column result
C      PDW 3/02/98    Alter handling of expr to support expression files
C                     of unlimited length... pass filename to parser
C      PDW 5/20/98    Ported to new CFITSIO parser, eliminating several
C                     fcalc subroutines, and simplifying data handling.
C                     Also added parameter names for extension and data types
C      PDW 7/28/98    Change names of parser routines
C      PDW 8/04/98    Allow for TBIT and TSTRING expression results
C      PDW 9/10/99    Add rowrange parameter
C      PDW 6/13/00    Copy TDIM keywords to output extension
C      PDW 6/15/00    Allow user to specify output column TFORM
C
C NOTES:
C       
C
C USAGE:
C      call ficalc(infile,outfile,clname,expr,anull,inull,
C                  copycol,histkw,copyall,rowrange)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output FITS file
C      clname  - resultant column name
C      expr    - expression which evaluates to a boolean
C      anull   - null string for ASCII table columns
C      inull   - null value to be used for a new integer output column
C      copycol - whether to copy all other clnames to the output file
C      histkw  - print history keyword flag
C      copyall - whether to copy all other extensions to the output file
C      rowrange- row range on which to operate
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      history - history string 
C      maxcl   - maximum number of clnames supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      outopen - output file open flag
C      eopen   - expression file open flag
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
C      tfields - total number of clnames in FITS table
C      varidat - size in bytes of variable data area 
C      clnum   - clname number
C      ttype   - array of clname names
C      tform   - array of clname formats
C      tunit   - array of clname units
C      tbcol   - clname number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine ft____ - FITSIO subroutines
C      subroutine fttexp - evaluate expression to determine return data type
C      subroutine ftcalc - calculate expression and place in a column
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine fcerr  - echo error message to terminal
C      subroutine fcstln - length of string once padded spaces removed
C
C******************************************************************************

      subroutine ficalc(infile,outfile,clname,expr,anull,inull,
     &     copycol,histkw,copyall,rowrange,usertform)

      character*(*) infile,outfile,clname,expr,anull,rowrange
      character*(*) usertform
      logical       copycol,histkw,copyall
      integer       inull
      integer maxcl
      parameter ( maxcl = 999 )
      integer maxrng
      parameter ( maxrng = 15 )
      integer nranges, frows(maxrng), lrows(maxrng)
      character(160) fname,filexpr
      character(80) context,errstr,card
      character(512) record
      character(2048) temp,history
      logical simple,extend,exact,inopen,outopen,eopen,exist
      integer extnum,ftstat,iunit,ounit,block,htype,bitpix,rlen
      integer naxis,naxes(maxcl),pcount,gcount,rowlen,nrows,tlen
      integer repeat
      integer tfields,varidat,eunit,elen,flen,datatype,colnum
      character(70) ttype(maxcl),tform(maxcl),tunit(maxcl)
      character(70) extname,colform,nullkey,tdimkwd
      integer fcstln,tbcol(maxcl), i, clen
      integer coldtype,colrpt,colwidth,coldec

C     Codes for FITS extension types
      integer IMAGE_HDU, ASCII_TBL, BINARY_TBL
      parameter (
     &     IMAGE_HDU  = 0,
     &     ASCII_TBL  = 1,
     &     BINARY_TBL = 2  )

C     Codes for FITS table data types

      integer TBIT,TBYTE,TLOGICAL,TSTRING,TSHORT,TINT,TLONG
      integer TFLOAT,TDOUBLE,TCOMPLEX,TDBLCOMPLEX
      parameter (
     &     TBIT        =   1,
     &     TBYTE       =  11,
     &     TLOGICAL    =  14,
     &     TSTRING     =  16,
     &     TSHORT      =  21,
     &     TINT        =  31,
     &     TLONG       =  41,
     &     TFLOAT      =  42,
     &     TDOUBLE     =  82,
     &     TCOMPLEX    =  83,
     &     TDBLCOMPLEX = 163  )

C   initialize variables
      ftstat = 0
      iunit = 15
      ounit = 16
      eunit = 17
      block = 0
      repeat = 1
      rowlen = 1 
      exact = .false.
      inopen = .false.
      eopen = .false.
      outopen = .false.
      context = ' '
      errstr = ' '

C   get the filename and extension
      call fcpars(infile,fname,extnum,ftstat)
      flen = fcstln(fname)
      clen = fcstln(clname)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

C   if the extension is 0 then give an error and exit
      if (extnum .lt. 1) then
         context = 'Primary extension not supported'
         call fcerr(context)
         goto 999
      endif

C   open the input FITS file
      call ftopen(iunit,fname,0,block,ftstat)
      if ( ftstat .ne. 0 ) then
         context = 'Unable to open infile'
         call fcerr(context)
         goto 999
      endif 
      inopen = .true.

C   read in the primary array header required keywords
      call ftghpr(iunit,maxcl,simple,bitpix,naxis,naxes,
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
         call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,ftstat)
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
      if ( htype .eq. ASCII_TBL ) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstat)
      else if ( htype .eq. BINARY_TBL ) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstat)
      else 
         context = 'Extension type not supported'
         call fcerr(context)
         goto 999
      endif

C   verify expression evaluates to an arithmetic
      call fttexp(iunit,expr,99,datatype,repeat,naxis,naxes,ftstat)
      if ( ftstat.ne.0 ) then
         context = 'Error parsing expression'
         call fcerr(context)
         goto 999
      endif

C   test for constant expression, but ignore it
      if ( repeat.lt.0 ) repeat = - repeat

C**********************************************************************
C   Build up column TFORM based on user-specified TFORM and/or result

      if( usertform .eq. ' ' .or. usertform .eq. '-' ) then

C        Build TFORM solely from result

         if (datatype .eq. TLOGICAL) then
            if (htype .eq. BINARY_TBL) then
               write(colform,250) repeat,'L'
            else
               context =
     &              'Cannot create a logical column in an ASCII table'
               call fcerr(context)
               goto 999
            endif
         else if (datatype .eq. TLONG) then
            if (htype .eq. BINARY_TBL) then
               write(colform,250) repeat,'J'
            else
               colform = 'I11'
            endif
         else if (datatype .eq. TDOUBLE) then
            if (htype .eq. BINARY_TBL) then
               write(colform,250) repeat,'D'
            else
               colform = 'D23.15'
            endif
         else if (datatype .eq. TSTRING) then
            if (htype .eq. BINARY_TBL) then
               write(colform,250) repeat,'A'
            else
               write(colform,250) repeat,' '
               call frmblk(colform)
               colform = 'A' // colform
            endif
         else if (datatype .eq. TBIT) then
            if (htype .eq. BINARY_TBL) then
               write(colform,250) repeat,'X'
            else
               write(colform,250) repeat,' '
               call frmblk(colform)
               colform = 'A' // colform
            endif
         else
            context = ' illegal return type for arithmetic expression'
            call fcerr(context)
            goto 999
         endif

      else

C        Build TFORM based on user-value and result

         if( htype .eq. BINARY_TBL ) then
            if( usertform(1:1).lt.'0' .or. usertform(1:1).gt.'9' ) then
               write(colform,250) repeat,usertform(1:1)
            else
               colform = usertform
            endif
         else
            if( usertform(2:2).lt.'0' .or. usertform(2:2).gt.'9' ) then
               if(       usertform(1:1).eq.'A' ) then
                  write(colform,250) repeat,' '
                  call frmblk(colform)
                  colform = 'A' // colform
               else if ( usertform(1:1).eq.'I' ) then
                  colform = 'I11'
               else if ( usertform(1:1).eq.'F' ) then
                  colform = 'F15.7'
               else if ( usertform(1:1).eq.'E' ) then
                  colform = 'E15.7'
               else if ( usertform(1:1).eq.'D' ) then
                  colform = 'D23.15'
               else
                  context = ' illegal TFORM for ASCII table'
                  call fcerr(context)
                  goto 999
               endif
            else
               colform = usertform
            endif
         endif

      endif

 250  format(I15,A)
      call frmblk(colform)

C**********************************************************************
C   determine if the clname already exists, setup column arrays

      call ftgcno(iunit,exact,clname,colnum,ftstat)
      if (ftstat .ne. 0 ) then
         exist = .false.
         ftstat = 0
         call ftcmsg

         if (copycol) then
            tfields = tfields + 1
            tbcol(tfields) = rowlen + 1
         else
            tfields = 1
            tbcol(1) = 1
         endif
         ttype(tfields) = clname
         tform(tfields) = colform
         colnum = tfields

         if( htype.eq.ASCII_TBL ) then
            call ftasfm( colform, coldtype, colwidth, coldec, ftstat )
            if( ftstat.ne.0 ) then
               context = ' unable to parse TFORM of output column'
               call fcerr(context)
               goto 999
            endif
            if( copycol ) then
               rowlen = rowlen + colwidth
            else
               rowlen = colwidth
            endif
         endif

      else

         exist = .true.
         if (.not. copycol) then
            tfields = 1
            ttype(1) = clname
            tform(1) = tform(colnum)
            if( htype.eq.ASCII_TBL ) then
               tbcol(1) = 1
               call ftasfm(tform(1),coldtype,rowlen,coldec,ftstat)
               if( ftstat.ne.0 ) then
                  context = ' unable to parse TFORM of output column'
                  call fcerr(context)
                  goto 999
               endif
            endif
         endif

      endif

C**********************************************************************
C     Check whether vector length of result and output column match

      if( htype .eq. BINARY_TBL ) then
         call ftbnfm(tform(colnum),coldtype,colrpt,colwidth,ftstat)
         if( ftstat.ne.0 ) then
            context = ' unable to parse supplied TFORM'
            call fcerr(context)
            goto 999
         else if(colrpt.ne.repeat .and. repeat.ne.1) then
            context = 'Resultant vector incompatible with ' //
     &           'supplied TFORM'
            call fcerr(context)
            goto 999
         endif
      endif

C**********************************************************************
C   create a new extension in the output FITS file

      call ftcrhd(ounit,ftstat) 

C   write extension header's keywords depending on extension type

      if ( htype .eq. ASCII_TBL ) then

         call ftphtb(ounit,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstat)

         if (anull.ne.' ') then
            call ftkeyn('TNULL',colnum,nullkey,ftstat)
            call ftukys(ounit,nullkey,anull,
     &           'Null value for column',ftstat)
         endif

         if (copycol) then
            call xcopyscale (iunit, ounit, ftstat)
         else
            call xcopynoscale (iunit, ounit, ftstat)
         endif
         
      else if ( htype .eq. BINARY_TBL ) then

         call ftphbn(ounit,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstat)

         if (coldtype.eq.TLONG .or. coldtype.eq.TINT
     &        .or. coldtype.eq.TSHORT .or. coldtype.eq.TBYTE) then
            if(inull.ne.0) then
               call ftkeyn('TNULL',colnum,nullkey,ftstat)
               call ftukyj(ounit,nullkey,inull,
     &              'Null value for column',ftstat)
            endif
         endif

         if (copycol) then
            call xcopyscale (iunit, ounit, ftstat)
C
C           Copy any TDIM keywords which are present
C                Only applies to vector columns/binary tables
            do 5 i=1,tfields-1
               call ftkeyn('TDIM', i, tdimkwd, ftstat)
               call ftgcrd( iunit, tdimkwd, card, ftstat )
               if ( ftstat.eq.0 ) then
                  call ftucrd( ounit, tdimkwd, card, ftstat )
               else if (ftstat.eq.202) then
                  ftstat = 0
                  call ftcmsg
               endif
C                Break IF here, so that we can catch ftucrd errors, too
               if( ftstat.ne.0 ) then
                  context = 'Error copying TDIM keywords'
                  call fcerr(context)
                  goto 999
               endif
 5          continue
         else
            call xcopynoscale (iunit, ounit, ftstat)
         endif

      endif

C     Write history keywords, including calculation expression
      if (histkw) then
         history = 'TASK: fcalc on FILENAME: '//fname(1:flen)
         call ftphis(ounit,history,ftstat)
         call timestamp (ounit)
         history = 'Resultant column name: '//clname(1:clen)
         call ftphis(ounit,history,ftstat)
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

C   move each clname in the extension if supposed to copy columns
      if (copycol) then
         call ftgkyj (iunit, 'NAXIS1', rowlen, context, ftstat)
         do 200 i=1, nrows
            call fcopyr (iunit, i, ounit, i, rowlen, ftstat)
 200     continue
      endif

C Parses the rowrange parameter
      call fcgrgs(rowrange,nrows,nranges,frows,lrows)

C do the calculation
      call ftcalc_rng(iunit,expr,ounit,clname,' ',nranges,
     &     frows,lrows,ftstat)
      if (ftstat .ne. 0) then
         context = 'Error selecting and copying rows'
         call fcerr (context)
         goto 999
      endif

C copy the remaining extensions, if any, and if requested
      if (copyall) then
 300     call ftmrhd (iunit, 1, htype, ftstat)
         call ftcrhd (ounit, ftstat)
         call ftcopy (iunit, ounit, 0, ftstat)
         if (ftstat .ne. 0) goto 998
         goto 300
      endif

 998  ftstat = 0

 999  continue
      if ( ftstat .ne. 0 ) then
         call fcerrm(ftstat)
         ftstat = 0

C delete created output file
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

