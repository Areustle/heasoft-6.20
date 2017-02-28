C******************************************************************************
C SELECTOR TASK:
C      fdump
C
C FILE:
C      fdump.f
C
C DESCRIPTION:
C      Converts a FITS Image file extension, ASCII, or BINARY table
C      into a simple ASCII file.  Prints out the contents of a FITS
C      array.
C
C AUTHOR:
C      Janice Tarrant  1/15/92
C
C MODIFICATION HISTORY:
C      Tarrant  4/20/92  removed header and data files
C       1/7/93 (EAG)    ASCII tables now output with scaling
C                       add parameter whether to show scaled or actual values
C       Feb '93 (EAG)   added skip parameter to allow display of every nth row
C                       added support of TDISPn keywords, if requested
C       3/17/93 EAG     fixed problem displaying ASCII tables
C       3/23/93 EAG     fixed problems with TDISP
C       3/24/93 EAG     Added wrap capability
C       4/1/93  EAG     Improved vector output appearance
C       4/2/93  EAG     Fixed bug introduced yesterday
C       4/5/93  EAG     Still not happy with output appearance
C       7/16/93 EAG     paging and deal with naxis2=0 nicely
C       7/29/93 EAG     fixed bug with i*2 output
C       8/24/93 EAG     improved performance
C       4/28/94 EAG 2.9a fixed precision problem (in fcb2s)
C       8/9/84  EAG 3.0a use FAOPEN to write output text file, add clobber
C                        shortened strings
C       10/3/94 EAG 3.1a rename fprintemp to pgfout, enable q to quit paging
C       6/27/95 MJT      fixed problem when TDISPn format is lower case
C       8/25/95 Srilal   Added new parameter fldsep (Field Separator)
C      11/28/1995 Guerber 3.2b  firfwa: increased TTYPE length from 16 to 40
C       4/24/96 MJT      added sensecase parameter (also checked in fccmpls)
C     9/4/1996 Guerber 3.2d  Fitsio handles bit arrays by byte, but <repeat>
C     lines were written, most blank; so adjust <repeat> for this case.
C       2/09/98 PDW      Status variable initialized to zero in fgdisp
C      04/05/1999 toliver 3.3 Added ftoolslib library routine call to support
C                             specification of columns using column numbers
C                             and column names containing wildcard characters.
C      11/18/1999 Ning 3.4 Added a new parameter of xdisp to
C                          write the bit column in "bit string"
C                          instead. 
C      09/11/2000 peachey 3.5
C                         Remove calls to ftgcno to get array colpos
C                         (column numbers). Instead, pass this array in
C                         the call to fcecls (the ftoolslib routine
C                         added in version 3.3). Fcecls has been
C                         changed to return the column number array,
C                         since it already gets the column number when
C                         it calls ftgcnn. The reason for this change
C                         is that fcecls properly handles wildcards in
C                         the column names. Also, case sensitivity flag
C                         was added to fcecls arguments.
C      08/16/2005 Ziqin Pan 3.6
C                         Change the dimension of 'rows' from character(80) to
C                         character(255).
C
C NOTES:
C      fdump supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file for header and data
C      columns  - list of column names to translate
C      rows     - list of rows to tranlate
C      pgwidth  - maximum width of page in characters
C      prhead   - flag for header keywords
C      prdata   - flag for data
C      showcol  - flag for column names
C      showrow  - flag for row numbers
C      showunit - flag for units
C      showscale- flag whether to showed scaled or straight values
C      align    - flag for column width
C       skip    - only print every nth row
C       tdisp   - whether to use the TDISPn keywords for output format
C       wrap    - whether to display entire row at one time
C       page    - whether to page through terminal output
C       fldsep  - define a new field separator
C
C CALLED ROUTINES:
C      subroutine gdump - gets parameters from environment
C      subroutine firfwa - read from FITS file and write to ASCII file
C
C******************************************************************************
      subroutine fdump
      character(160) infile, outfile
      character(256) columns, rows
      character(1) fldsep
      integer pgwidth, skip, status
      logical prhead, prdata, showcol, showunit, showrow, align
      logical showscale, tdisp, wrap, page, sensecase
      integer xdisp
      character(40) taskname
      common /task/ taskname

      taskname = 'fdump3.5'
      outfile = ' '

      call ftcmsg

C  get the parameters from the par file
      call gdump(infile,outfile,columns,rows,pgwidth,prhead,prdata,
     &     showcol,showunit,showrow,showscale, align, skip, tdisp,
     &     wrap, page, fldsep, sensecase, xdisp,status)
      if (status .ne. 0) goto 999

C  read in the FITS file and write out the ASCII file
      call firfwa(infile,outfile,columns,rows,pgwidth,prhead, prdata,
     &     showcol,showunit,showrow, showscale, align, skip, tdisp,
     &     wrap, page,fldsep,sensecase,xdisp)

 999  return
      end



C******************************************************************************
C SUBROUTINE:
C      gdump
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Janice Tarrant  1/15/92
C
C MODIFICATION HISTORY:
C      Tarrant  4/20 92  removed header and data files
C       1/7/93 (EAG)     Added showscale parameter
C       2/12/93 (EAG)   Added skip and tdisp parameters
C       3/24/93 (EAG)   Added wrap parameter
C
C NOTES:
C      gdump uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gdump(infile,outfile,columns,rows,pgwidth,prhead,prdata,showcol,
C                  showunit,showrow, showscale, align, skip, tdisp, wrap,
C                  page, status,fldsep)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file
C      columns  - list of column names to translate
C      rows     - list of rows to tranlate
C      pgwidth  - maximum width of page in characters
C      prhead   - flag for header keywords
C      prdata   - flag for data
C      showcol  - flag for column names
C      showrow  - flag for row numbers
C      showunit - flag for units
C      showscale- flag whether to show scaled values
C      align    - flag for column width
C       skip    - output only every nth row
C       tdisp   - whether to use TDISPn keywords
C       wrap    - whether to display entire row at one time
C       page    - whether to page through terminal output
C       status  - status of operation
C       fldsep  - define a new field separator
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C
C******************************************************************************
      subroutine gdump(infile,outfile,columns,rows,pgwidth,prhead,
     &     prdata, showcol, showunit, showrow, showscale, align, skip,
     &     tdisp, wrap, page, fldsep, sensecase, xdisp,status)

      character*(*) infile, outfile, columns, rows, fldsep
      integer pgwidth, skip
      logical prhead,prdata,showcol,showunit,showrow,align, showscale
      logical tdisp, wrap, page, sensecase
      integer xdisp
      character(5) strxdisp

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

C  get the name of the output ASCII file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
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

C  get the rows
      call uclgst('rows',rows,status)
      if (status .ne. 0) then
         context = 'could not get ROWS parameter'
         call fcerr(context)
         goto 999
      endif

C  get the page width
      call uclgsi('pagewidth',pgwidth,status)
      if (status .ne. 0) then
         context = 'could not get PAGEWIDTH parameter'
         call fcerr(context)
         goto 999
      endif

C  get the header flag
      call uclgsb('prhead',prhead,status)
      if (status .ne. 0) then
         context = 'could not get PRHEAD flag'
         call fcerr(context)
         goto 999
      endif

C  get the data flag
      call uclgsb('prdata',prdata,status)
      if (status .ne. 0) then
         context = 'could not get PRDATA flag'
         call fcerr(context)
         goto 999
      endif

C  get the column names flag
      call uclgsb('showcol',showcol,status)
      if (status .ne. 0) then
         context = 'could not get SHOWCOL flag'
         call fcerr(context)
         goto 999
      endif

C  get the units flag
      call uclgsb('showunit',showunit,status)
      if (status .ne. 0) then
         context = 'could not get SHOWUNIT flag'
         call fcerr(context)
         goto 999
      endif

C  get the row numbers flag
      call uclgsb('showrow',showrow,status)
      if (status .ne. 0) then
         context = 'could not get SHOWROW flag'
         call fcerr(context)
         goto 999
      endif

C  get the scaling flag
      call uclgsb('showscale',showscale,status)
      if (status .ne. 0) then
         context = 'could not get SHOWSCALE flag'
         call fcerr(context)
         goto 999
      endif

C  get the column width flag
      call uclgsb('align',align,status)
      if (status .ne. 0) then
         context = 'could not get ALIGN flag'
         call fcerr(context)
         goto 999
      endif

C  get the frequency of output - skip - parameter
      call uclgsi('skip',skip,status)
      if (status .ne. 0) then
         context = 'could not get SKIP parameter'
         call fcerr(context)
         goto 999
      endif

C  get the display type flag
      call uclgsb('tdisp',tdisp,status)
      if (status .ne. 0) then
         context = 'could not get TDISP flag'
         call fcerr(context)
         goto 999
      endif

C  get the wrap flag
      call uclgsb('wrap', wrap, status)
      if (status .ne. 0) then
         context = 'could not get WRAP flag'
         call fcerr(context)
         goto 999
      endif

C  get the page flag
      call uclgsb('page', page, status)
      if (status .ne. 0) then
         context = 'could not get PAGE flag'
         call fcerr(context)
         goto 999
      endif

C  get the field separator flag
      call uclgst('fldsep', fldsep, status)
      if (status .ne. 0) then
         context = 'could not get FLDSEP parameter'
         call fcerr(context)
         goto 999
       endif
       
C  get the sensecase parameter
       call uclgsb('sensecase',sensecase,status)
       if(status .ne. 0)then
         context = 'could not get SENSECASE paramter'
         call fcerr(context)
         goto 999
       endif

C  get the xdisp parameter
       call uclgst('xdisp',strxdisp,status)
       if(status .ne. 0)then
         context = 'could not get xdisp paramter'
         call fcerr(context)
         goto 999
       endif

C  default(0): bit string with 8 bits long 
C  integer byte with 8 bits(1) 
C  bit string with length of vector length(2)

       xdisp = 0
       if(strxdisp(1:1).eq.'D'.or.strxdisp(1:1).eq.'d') xdisp = 1
       if(strxdisp(1:1).eq.'B'.or.strxdisp(1:1).eq.'b') xdisp = 2

 999  continue

      if (status .ne. 0)  call fcerrm(status)

      return
      end



C******************************************************************************
C SUBROUTINE:
C      firfwa
C
C DESCRIPTION:
C      Read requested columns in from FITS extension and writes the
C      data to the output in ASCII format
C
C AUTHOR:
C      Janice Tarrant  1/15/92
C
C MODIFICATION HISTORY:
C      Tarrant  1/24/92  changed fcclwd and column width determination
C      Tarrant  4/16/92  added vector elements
C      Tarrant  4/20/92  removed header and data files
C      Pence    7/23/92  fixed problem with vector support
C                        added option to dump all extensions
C      Greene  12/22/92  truncate ASCII output is greater than pagewidth
C                        allow for output of variable length columns
C      Greene   1/7/93  add showscale option.
C      Greene  2/12/93  add skip and TDISPn support
C      Greene  3/24/93  add wrap support
C      Greene  7/16/93  add more support
C      Srilal  8/29/95  modified maxlst and maxcl from 512 to 999
C      Guerber 11/28/1995  increased TTYPE length from 16 to 40
C      MJT     4/24/96  added "exact" as argument (from sensecase)
C      Guerber 9/4/1996  Adjust repeat for case of bit arrays
C      toliver 03/03/1998 moved location of line separator between header
C                         and data to beginning of loop, this avoids
C                         extra new line after the end of data (per request
C                         in thread ideas/fdump_971231)
C      toliver 04/05/1999 Added ftoolslib library routine call to support
C                         specification of columns using column numbers
C                         and column names containing wildcard characters.
C      Ning    11/18/1999  Added a new parameter of xdisp to
C                          write the bit column in bit string
C                          instead 
C      peachey 09/11/2000 Remove calls to ftgcno to get array colpos
C                         (column numbers). Instead, pass this array in
C                         the call to fcecls (the ftoolslib routine
C                         added in version 3.3). Fcecls has been
C                         changed to return the column number array,
C                         since it already gets the column number when
C                         it calls ftgcnn. The reason for this change
C                         is that fcecls properly handles wildcards in
C                         the column names. Also, case sensitivity flag
C                         was added to fcecls arguments.
C
C NOTES:
C      firfwa uses FITSIO calls to read FITS file
C
C USAGE:
C      call firfwa(infile,outfile,columns,rows,pgwidth,prhead,prdata,showcol,
C                  showunit,showrow,showscale,align, skip, tdisp, wrap, page,fldsep)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file
C      columns  - list of column names to translate
C      rows     - list of rows to tranlate
C      pgwidth  - maximum width of page in characters
C      prhead   - flag for header keywords
C      prdata   - flag for data
C      showcol  - flag for column names
C      showrow  - flag for row numbers
C      showunit - flag for units
C      showscale- flag whether to show scaling
C      align    - flag for column width
C       skip    - display every nth row
C       tdisp   - whether to use TDISPn keyword for output format
C       wrap    - whether to display entire row at one time
C       fldsep  - define a new field separator
C
C PRIMARY LOCAL VARIABLES:
C      filename  - name of FITS file
C      extnum    - FITS file extension number
C      htype     - FITS header type
C      nkeys     - number of keywords
C      numranges - number of row ranges
C      rowstring - concat. string of column values
C      context   - error message
C      ftstatus  - FITSIO error number
C
C CALLED ROUTINES:
C      subroutine fcbtos - get a value from a BINARY element
C      subroutine fcclwd - get width of column
C      subroutine fccmpl - compare two lists for a subset
C      subroutine fccmpr - check for allowable rows list
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcgcls - get column list based on parameter
C      subroutine fcecls - ftoolslib routine that expands the column list by
C                          translating column numbers into corresponding names
C                          and column names containing wild card characters
C                          into matching column names
C      subroutine fcgrgs - get ranges of rows to translate
C      subroutine fcpars - parse off filename and extension number
C      subroutine fcstln - find index of last non-blank character
C      FITSIO subroutine library - all subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine firfwa(infile,outfile,columns,rows,pgwidth,prhead,
     &     prdata, showcol, showunit, showrow, showscale, align, skip,
     &     tdisp, wrap, page,fldsep,exact,xdisp)

      integer maxcl, maxlst
      parameter (maxlst = 999)
      parameter (maxcl = 999)
      character*(*) infile, outfile, columns, rows, fldsep
      integer pgwidth, skip
      logical prhead, prdata, showcol, showunit, showrow, align
      logical showscale, tdisp, wrap, page
      integer xdisp
      character(160) filename
      character(80) context, record, errstr, extname
      character(16) disp(maxcl)
      character(256) rowstring
      character(40) ttype(maxcl)
      character(25) tunit(maxcl)
      character(40) colist(maxlst)
      character(40) colist2(maxlst)
      character(16) tform(maxcl)
      logical outopen,exact,negflag,goodlist,goodrows,inopen,allext
      integer i, j, k, l, m, ftstatus, iunit, ounit, block,
     &     extnum, htype, nkeys, nmore, nrows, rowlen, tfields,
     &     tbcol(maxcl), varidat, lname(maxcl), lunit(maxcl),
     &     lform(maxcl), nchars, numfields, tcols, colpos(maxlst),
     &     numranges, rowrange1(maxlst), rowrange2(maxlst), nnames,
     &     nvals, repeat, dtype, width,maxrep,el,col1, status
      logical vector
      integer numvec, veccol(maxcl), offset, ii, oldk
      double precision tscal, tzero
      logical ctemp
      integer repeat1,width1
      integer ki, bitsinbyte

C  initialize variables
      ftstatus = 0
      status = 0
      iunit = 15
      ounit = 16
      inopen = .false.
      outopen = .false.
      negflag = .false.
C      exact = .false.
      allext=.false.
      vector = .false.
      numvec = 0
      tscal = 1.D0
      tzero = 0.D0

C  get the filename and extension number
      call fcpars(infile,filename,extnum,ftstatus)

C EAG 8/25/93 default to dumping all extensions
      if (extnum .lt. 0)then
         allext=.true.
         extnum=0
      end if

C  open the FITS file
      call ftopen(iunit,filename,0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to open infile'
         call fcerr(context)
         goto 999
      endif
      inopen = .true.

C  open the ASCII files
      if (outfile .ne. 'STDOUT') then
C recordlength is only used for VMS machines
         i = max(pgwidth, 80)
         call faopen (ounit, outfile, 2, i, status)
         if (status .ne. 0) then

 110        context = 'error: output file already exists? ' // outfile
            call fcerr(context)
            goto 999
         else
            outopen = .true.
         endif
 120     continue
      else
C enable paging in pgfout
         if (page) ounit = - ounit
      endif

 125  htype=0
C  move to the extension number
      if (extnum .ne. 0) then
         call ftmahd(iunit,extnum+1,htype,ftstatus)
         if (ftstatus .ne. 0) then
            if (.not. allext)then
               errstr = 'error moving to extension number '
               write(context,1000) errstr,extnum
 1000          format(A34,I3)
               call fcerr(context)
            end if
            goto 999
         else
C           separate the header and data
            call pgfout (ounit, outopen, ' ', status)
         endif
      endif

C  write out each header keyword if requested to do so
C  format should be controlled by page width
      if (prhead) then

C  find out the number of keywords in the header
         call ftghsp(iunit,nkeys,nmore,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'error getting number of keywords'
            call fcerr(context)
            goto 999
         endif
         do 10 i = 1, nkeys
            call ftgrec(iunit,i,record,ftstatus)
            call pgfout (ounit, outopen, record, status)
            if (status .ne. 0) goto 999
 10      continue
         if (ftstatus .ne. 0) then
            context = 'error reading header keywords'
            call fcerr(context)
            goto 999
         endif

         record='END'
         call pgfout (ounit, outopen, record, status)
         if (status .ne. 0) goto 999

      endif

      if (htype .eq. 0) then
C            currently don't support writing data from primary array or image
         go to 999
      end if

C  get the column names and the range of rows for an ASCII extension
      if (.not. prdata)  goto 999
      if (htype .eq. 1) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,ftstatus)
C  get the column names and range of rows for a BINARY extension
      else if (htype .eq. 2) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,ftstatus)
      endif

C initialize the lforms to 0 and disp to blank
      do 15 i = 1, tfields
         lform(i) = 0
         disp(i) = ' '
 15   continue

C  determine which columns are to be included and get the list of names
      if ((columns .eq. ' ').or.(columns .eq. '-')) then
         do 30 i = 1, tfields
            colist(i) = ttype(i)
            colpos(i) = i
 30      continue
         tcols = tfields
      else
         call fcgcls(columns,colist2,tcols,negflag)
C
C        Take the list of column specifiers from the parameter list and:
C           - replace any columns specified by number to the corresponding
C             column name
C           - replace any column specifiers containing wildcard characters
C             with all matching column names
C
         call fcecls (iunit, exact, colist2, colist, colpos,
     &                tcols, maxcl)
         call fccmpl(tcols,tfields,colist,ttype,negflag,goodlist)
         if (.not. goodlist) then
            errstr = 'error in column list names: extension'
            write(context,1001) errstr, extnum
            call fcerr(context)
            goto 999
         endif
      endif

      if (htype .eq. 2) then
C for binary header, turn off scaling if requested
         if (.not. showscale) then
            do 33 i = 1, tcols
               call fttscl (iunit, colpos(i), tscal, tzero, ftstatus)
 33         continue
         else
C check for byte, short or integer columns with scaling
            do 35 i = 1, tcols
               call fdmpsl (iunit, colpos(i), tform(colpos(i)),
     &              disp(colpos(i)), lform(colpos(i)), ftstatus)
 35         continue
         endif
      endif

C if we are supposed to look for TDISPn values
      if (tdisp) call fgdisp (iunit, tfields, disp, lform)

C  get all the field widths for the columns
      do 20 i = 1, tcols
         call fcclwd (ttype(colpos(i)), tunit(colpos(i)),
     &        tform(colpos(i)), htype, showcol, showunit, align,
     &        lname(colpos(i)), lunit(colpos(i)), lform(colpos(i)))
         if ((lform(colpos(i)) .gt. pgwidth-9) .or.
     &        (lname(colpos(i)) .gt. pgwidth-9) .or.
     &        (lunit(colpos(i)) .gt. pgwidth-9)) then
            context = ' Column too wide for page width - truncating'
            call fcecho (context)
         endif
C   Reset the width of the Bit column if the xdisp us setus set
         if ((index(tform(colpos(i)), 'X') .ne. 0)) then
              ftstatus = 0
              call ftbnfm(tform(colpos(i)),dtype,repeat1,
     &                  width1, ftstatus)
              if(xdisp.eq.2.or.xdisp.eq.0) then 
                  if(xdisp.eq.2) lform(colpos(i)) = repeat1+1 
                  if(xdisp.eq.0) lform(colpos(i)) = 9 
                  if(lname(colpos(i)).lt.lform(colpos(i))) then 
                      lname(colpos(i)) = lform(colpos(i)) 
                      lunit(colpos(i)) = lform(colpos(i))
                  endif
              endif
         endif
         if (lform(colpos(i)) .gt. pgwidth-9)
     &        lform(colpos(i)) = pgwidth - 9
         if (lname(colpos(i)) .gt. pgwidth-9)
     &        lname(colpos(i)) = pgwidth - 9
         if (lunit(colpos(i)) .gt. pgwidth-9)
     &        lunit(colpos(i)) = pgwidth - 9
 20   continue

C  check that rows list is allowable
      call fccmpr(rows,goodrows)
      if (.not. goodrows)  goto 999

C  determine which rows are to be included and get the row ranges
      if (rows .eq. '-') then
         numranges = 1
         rowrange1(1) = 1
         rowrange2(1) = nrows
      else
         call fcgrgs(rows,nrows,numranges,rowrange1,rowrange2)
      endif

C  initialize starting column
      col1=1

C  loop back to here if all the columns could not fit in the output
 200  continue

C  separate the header and data
      if ((.not. wrap) .or. (col1 .eq. 1))
     &     call pgfout (ounit, outopen, ' ', status)
      if (status .ne. 0) goto 999

C  build the row string for the column names
      numfields = tcols
      if (showcol) then
         rowstring = ' '
         nnames = 0
         if (showrow) then
            j = 9
            k = 8
         else
            j = 1
            k = 0
         endif
         do 40 i = col1, numfields
            nnames = nnames + 1
            k = k + lname(colpos(i)) + 1
            if (k .gt. pgwidth) then
               numfields = i - 1
               nnames = nnames - 1
               goto 41
            endif
            rowstring(j:k) = ttype(colpos(i))
            j = k + 1
 40      continue
 41      continue

C  print out the column names row string
         call pgfout (ounit, outopen, rowstring, status)
         if (status .ne. 0) goto 999
      endif

C  build the row string for the column units
      if (showunit) then
         rowstring = ' '
         if (showrow) then
            j = 9
            k = 8
         else
            j = 1
            k = 0
         endif
         do 50 i = col1, numfields
            k = k + lunit(colpos(i)) + 1
            rowstring(j:k) = tunit(colpos(i))
            if (k .gt. pgwidth) then
               numfields = i - 1
               nnames = nnames - 1
               goto 51
            endif
            j = k + 1
 50      continue
 51      continue

C  print out the column units row string
         call pgfout (ounit, outopen, rowstring, status)
         if (status .ne. 0) goto 999
      endif

C  if printing entire row at a time, loop back for additional
C       column headers, if necessary
      if ((wrap) .and. (numfields .lt. tcols)) then
         col1 = numfields + 1
         goto 200
      endif

C  determine maximum repeat count for all the columns
C       when NOT wrapping, only look at the correct columns for this "Page"
C     For bit arrays (dtype=1), fitsio reads as bytes so adjust repeat
      if (.not. wrap) then
         maxrep=1
         vector = .false.
         numvec = 0
         if (htype .eq. 2) then
            do 85 i=col1, numfields
               call ftbnfm(tform(colpos(i)),dtype,repeat,width,ftstatus)
               if (abs(dtype) .eq. 16)repeat=repeat/width
               if (abs(dtype) .eq. 1 .and.xdisp.lt.2) 
     &              repeat = (repeat + 7) / 8
               if (abs(dtype) .eq. 1 .and. xdisp.eq.2) 
     &              repeat = 1
               maxrep=max(maxrep,repeat)
               if (index(tform(colpos(i)),'P') .ne. 0) then
                  vector = .true.
                  numvec = numvec + 1
                  veccol(numvec) = colpos(i)
               endif
 85         continue
         endif
         repeat = maxrep
      endif

C  print out each row of column data
      nvals = 0
      do 100 m = 1, numranges

C deal with naxis2=0 gracefully
         if (rowrange1(m) .le. 0) goto 100
         do 60 l = rowrange1(m), rowrange2(m), skip
            if (wrap) then
               numfields = tcols
               col1 = 1
            endif
 300        maxrep = repeat

C for wrapped output, need to find the correct number of rows for vectors each
C       time, just like variable arrays
            if (wrap) then

C so lets go through and find out exactly which columns to print this time
C       around
               if (showrow) then
                  k = 8
               else
                  k = 0
               endif
               do 87 i = col1, numfields
                  if (.not. align) then
                     k = k + lform(colpos(i)) + 1
                  else
                     k = k + lname(colpos(i)) + 1
                  endif
                  if (k .gt. pgwidth) then
                     numfields = i - 1
                     goto 88
                  endif
 87            continue
C  determine maximum repeat count for all the columns
 88            maxrep=1
               vector = .false.
               numvec = 0
               if (htype .eq. 2) then
                  do 86 i=col1, numfields
                     call ftbnfm(tform(colpos(i)),dtype,repeat,width,
     &                    ftstatus)
                     if (abs(dtype) .eq. 16)repeat=repeat/width
                     if (abs(dtype) .eq. 1 .and. xdisp.lt.2) 
     &                   repeat = (repeat + 7) / 8
                     if (abs(dtype) .eq. 1 .and. xdisp.eq.2) 
     &                   repeat = 1
                     maxrep=max(maxrep,repeat)
                     if (index(tform(colpos(i)),'P') .ne. 0) then
                        vector = .true.
                        numvec = numvec + 1
                        veccol(numvec) = colpos(i)
                     endif
 86               continue
               endif
               repeat = maxrep
            endif

C for variable columns this is different for each row
            if (vector) then
               do 44 ii = 1, numvec
                  call ftgdes (iunit, veccol(ii), l, maxrep, offset,
     &                 ftstatus)
                  maxrep = max(maxrep, repeat)
 44            continue
            endif
            do 55 el=1,maxrep
               rowstring=' '
               if (showrow) then
                  if ((el .eq. 1) .and. ((col1 .eq. 1) .or.
     &                 (.not. wrap))) write(rowstring(1:7),1003) l
                  rowstring(8:8)=fldsep
                  j = 9
                  k = 8
               else
                  j = 1
                  k = 0
               endif
               do 70 i = col1, numfields
                  nvals = nvals + 1

                  oldk = k
                  if (.not. align) then
                     k = k + lform(colpos(i)) + 1
                  else
                     k = k + lname(colpos(i)) + 1
                  endif

                  if (k .gt. pgwidth) then
                     numfields = i - 1
                     nvals = nvals - 1
                     goto 71
                  endif
                  if (htype .eq. 1) then
                     nchars = lform(colpos(i))
                     call ftgtbs(iunit,l,tbcol(colpos(i)),nchars,
     &                    rowstring(j:),ftstatus)

C check for null and scaling definition
                     call fafix (iunit, colpos(i), l, nchars,
     &                    showscale, rowstring(j:), ftstatus)

                  else if ( htype .eq. 2 ) then
C
C                 check the bit columns and the bitpatern parameter
C                  
                     if ((xdisp.eq.1).or. 
     &                   (index(tform(colpos(i)), 'X') .eq. 0))
     &               then
                        call fcb2s(iunit,colpos(i),l,el,
     &                    tform(colpos(i)),disp(colpos(i)),
     &                    rowstring(j:))  
C                    combine 8 bits as a group.
                     else if(xdisp.eq.0) then 
                        call ftbnfm(tform(colpos(i)),dtype,repeat1,
     &                        width1, ftstatus)
                        bitsinbyte = repeat1 - (el-1)*8  
                        if(bitsinbyte .gt. 8) bitsinbyte = 8 
                        if(bitsinbyte .gt. 0) rowstring(j:j) = 'b'
                        ki = (el-1)*8
                        do ii = 1, bitsinbyte 
                           call ftgcx (iunit, colpos(i),l,ki+ii,1,
     &                            ctemp,ftstatus)
                           if(ctemp) then 
                             rowstring(j+ii:j+ii) = '1'
                           else
                             rowstring(j+ii:j+ii) = '0'
                           endif 
                        enddo
C                    bit string format
                     else if(xdisp.eq.2) then   
                         if(el.eq.1) then 
                            ftstatus = 0
                            call ftbnfm(tform(colpos(i)),dtype,repeat1,
     &                        width1, ftstatus)
                            ftstatus = 0
                            rowstring(j:j) = 'b'
                            do ii = 1, repeat1
                               call ftgcx(iunit,colpos(i),l,ii,1,
     &                           ctemp,ftstatus)
                               if(ctemp) then 
                                 rowstring(j+ii:j+ii) = '1'
                               else
                                 rowstring(j+ii:j+ii) = '0'
                               endif 
                             enddo
                         endif
                     endif
                  endif
CEAG                do 69 ii = oldk+1+lform(colpos(i)),fcstln(rowstring)
CEAG                    rowstring(ii:ii) = ' '
CEAG69              continue
                  rowstring(oldk+1+lform(colpos(i)):) = fldsep

                  j = k + 1

 70            continue
 71            continue
               call pgfout (ounit, outopen, rowstring, status)
               if (status .ne. 0) goto 999
 55         continue

C if are printing entire row at a time, loop back for next row, as
C       needed
            if ((wrap) .and. (numfields .lt. tcols)) then
               col1 = numfields + 1
               numfields = tcols
               goto 300
            endif

 60      continue
 100  continue
      if (numfields .lt. tcols)then
         col1=numfields+1
         go to 200
      end if

C  close files and return on error
 999  continue


      if (ftstatus .ne. 0) then
         if (.not. allext)call fcerrm(ftstatus)
      else if (status .ne. 0) then
         continue
      else if (allext)then
         if (status .ne. 0) goto 999
         extnum=extnum+1
         go to 125
      endif

C  close the infile and the outfile
      ftstatus = 0
      if (inopen)  call ftclos(iunit,ftstatus)
      if (outopen)  close(ounit)

 1001 format(A38, i4)
 1002 format(A36,I3,A22,I3)
 1003 format(I7)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      fafix
C
C DESCRIPTION:
C       fix up an ascii string copied from an ascii table to take
C       into account the TNULL, TZERO and TSCAL keywords
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       6 January, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      fafix uses FITSIO calls to read FITS file and one lower level
C       FITSIO routine to format a string
C
C USAGE:
C      call fafix (iunit, colnum, frow, nchars, showscale, outstr, status)
C
C ARGUMENTS:
C   input:
C       iunit    - input unit number
C       colnum   - column number
C       frow     - the current row in the file
C       nchars   - number of characters inwhich to format
C       showscale- whether to show scaling or not
C   output:
C       outstr   - the finalized output string
C       status  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C       FTR2E    - lower level FITSIO routine
C function fcstln - index of the last non-blank character
C
C******************************************************************************
      subroutine fafix (iunit, colnum, frow, nchars, showscale,
     &     outstr, status)

      integer iunit, frow, nchars, colnum, status
      character*(*) outstr
      logical showscale

      integer decimals
      logical flagvals, anyf
      character(80) context, keyword, keyroot, snull
      character(256) rowstring
      character(80) tempstr
      real value

      rowstring = outstr
      tempstr = ' '

C check for null definition
      keyroot = 'TNULL'
      snull = ' '
      call ftkeyn (keyroot, colnum, keyword, status)
      call ftgkys (iunit, keyword, snull, context, status)
      if (status .ne. 0) then
         snull = 'INDEF'
         status = 0
      endif
      if (rowstring .eq. snull)
CEAG    if (rowstring(1:fcstln(rowstring)) .eq. snull(1:fcstln(snull)))
     &     write(rowstring, 1001) 'INDEF'

C check for TZERO and TSCAL definition
      if (showscale) then
         keyroot = 'TZERO'
         snull = ' '
         call ftkeyn (keyroot, colnum, keyword, status)
         call ftgkys (iunit, keyword, snull, context, status)
         if (status .ne. 0) then
            call ftcmsg
            status = 0
            keyroot = 'TSCAL'
            call ftkeyn (keyroot, colnum, keyword, status)
            call ftgkys (iunit, keyword, snull, context, status)
         endif
         if (status .eq. 0) then
            call ftgcfe (iunit, colnum, frow, 1, 1, value,
     &           flagvals, anyf, status)
            if (anyf) goto 100
            decimals = nchars - 7
            if (decimals .lt. 0) then
c not enough room in space, fill with ***s
               rowstring = '***'
               goto 100
            endif
            call ftr2e (value, decimals, tempstr, status)

C remove leading/trailing blanks to make the string the proper length
            call frmblk (tempstr)
            rowstring = tempstr
         else
            status = 0
            call ftcmsg
         endif

 100     continue
      endif

      outstr = rowstring

      return
 1001 format(A)
      end

C******************************************************************************
C SUBROUTINE:
C      fgdisp
C
C DESCRIPTION:
C       find the TDISPn (if any) for all columns, and calculate
C       the width they represent
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       12 February, 1993
C
C MODIFICATION HISTORY:
C      added call to ftupch to force TDISP strings to uppercase (MJT 6/95)
C      initialize status variable to zero... undefined under IRIX (PDW 2/9/98)
C
C NOTES:
C      fgdisp uses FITSIO calls to read FITS file
C
C USAGE:
C       call fgdisp (iunit, tfields, disp, lform)
C
C ARGUMENTS:
C   input:
C       iunit    - input unit number
C       tfields  - the number of columns
C   output:
C       disp     - array of output TDISPns
C       lform    - the corresponding width
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message
C       status   - error number
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C       fcstln   - function returning index of last non-blank character
C
C******************************************************************************
      subroutine fgdisp (iunit, tfields, disp, lform)

      integer iunit, tfields
      character*(*) disp(tfields)
      integer lform(tfields)

      character(8) keyroot
      integer nfound, status, i, fcstln, endpos
      character(80) context

      status = 0
C get the TDISPn keywords
      keyroot = 'TDISP'
c       call ftkeyn (keyroot, i, keyword, status)
      call ftgkns (iunit, keyroot, 1, tfields, disp, nfound, status)
      if (status .ne. 0) call fcerrm(status)

C check for found values
      do 10 i = 1, tfields
         if (fcstln(disp(i)) .gt. 0) then

C convert TDISPn string to uppercase (MJT 6/27/95)
            call ftupch(disp(i))

C figure out the width ...
            endpos = index(disp(i), '.')
            if (endpos .le. 0) endpos = fcstln(disp(i)) + 1
            read (disp(i)(2:endpos-1),'(BN,I3)',err=999) lform(i)
         endif
         goto 10

C error during read, just use default
 999     context = ' strange TDISP: '//disp(i)
         call fcecho(context)
         context = ' using default format'
         call fcecho(context)
         lform(i) = 0
         disp(i) = ' '
 10   continue

      return
      end

