C******************************************************************************
C SELECTOR TASK:
C      flist
C
C FILE:
C      flist.f
C
C DESCRIPTION:
C       This routine prints a FITS file to a screen in a manner similar to
C       DUMPFITS
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C       2/15/94 EAG 1.1 Allow XTE style specification of array elements
C                       Output array index number
C       4/5/94  EAG 1.2 Allow Hex and Octal output
C       9/8/94  EAG 1.3 Use FAOPEN to open output text file, add clobber
C       10/3/94 EAG 1.4 rename fprintemp to pgfout, enable q to quit paging
C   11/30/1995  JRG 1.5 flstpd: fix TTYPE length
C
C NOTES:
C      flist supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine flist
      character(256) infile, outfile
      character(80) columns, rows
      logical prhead, prdata, showscale, tdisp, page, sensecase
      integer skip

      integer status

      character(40) taskname
      common /task/ taskname

      taskname = 'FLIST v1.6'

      outfile = ' '
      infile = ' '
      columns = ' '
      rows = ' '
      status = 0

C  zero out the FITSIO error stack
      call ftcmsg

C  get the parameters from the par file
      call glist (infile, outfile, columns, rows, prhead, prdata,
     &     showscale, skip, tdisp, page, sensecase, status)
      if (status .ne. 0) goto 999

C  print out the requested information
      call fplist (infile, outfile, columns, rows, prhead, prdata,
     &     showscale, skip, tdisp, page, sensecase, status)

 999  if (status .ne. 0) call fcerrm (status)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      glist
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      glist uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C       call glist (infile, outfile, columns, rows, prhead, prdata,
C                   showscale, skip, tdisp, page, sensecase, status)
C
C ARGUMENTS:
C      infile     - input FITS file and extension number
C      outfile    - output ASCII file
C       columns   - the columns to output
C       row       - the rows to output
C       prhead    - whether to print the header or not
C       prdata    - whether to print the data or not
C       showscale - show the scaled values
C       skip      - only output every nth row of data
C       tdisp     - display using TDISP keyword format
C       page      - page the output every 24 lines
C       sensecase - whether to be case sensitive about column names
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C
C******************************************************************************

      subroutine glist (infile, outfile, columns, rows, prhead,
     &     prdata, showscale, skip, tdisp, page, sensecase, status)

      character*(*) infile, outfile, columns, rows
      logical prhead, prdata, showscale, tdisp, page, sensecase
      integer skip, status

      character(512) context

C  get the name of the input FITS file
      call uclgst ('infile', infile, status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the output ASCII file
      call uclgst ('outfile', outfile, status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the columns to output
      call uclgst ('columns', columns, status)
      if (status .ne. 0) then
         context = 'could not get COLUMNS parameter'
         call fcerr(context)
         goto 999
      endif

C  get the rows to output
      call uclgst ('rows', rows, status)
      if (status .ne. 0) then
         context = 'could not get ROWS parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to print the header
      call uclgsb ('prhead', prhead, status)
      if (status .ne. 0) then
         context = 'could not get PRHEAD parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to print the data
      call uclgsb ('prdata', prdata, status)
      if (status .ne. 0) then
         context = 'could not get PRDATA parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to show scaling or not
      call uclgsb ('showscale', showscale, status)
      if (status .ne. 0) then
         context = 'could not get SHOWSCALE parameter'
         call fcerr(context)
         goto 999
      endif

C  get how frequently to output data
      call uclgsi ('skip', skip, status)
      if (status .ne. 0) then
         context = 'could not get SKIP parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to print the data using TDISP keywords
      call uclgsb ('tdisp', tdisp, status)
      if (status .ne. 0) then
         context = 'could not get TDISP parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to page the output
      call uclgsb ('page', page, status)
      if (status .ne. 0) then
         context = 'could not get PAGE parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to be case sensistive about column names
C       call uclgsb ('sensecase', sensecase, status)
C       if (status .ne. 0) then
C           context = 'could not get SENSECASE parameter'
C           call fcerr(context)
C           goto 999
C       endif

C NOTE case sensitivity MUST be on because fccmpl is case sensitive
C Not anymore LEB
      sensecase = .false.

 999  continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      fplist
C
C DESCRIPTION:
C      Print the requested information
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call fplist (infile, outfile, columns, rows, prhead, prdata,
C                   showscale, skip, tdisp, page, sensecase, status)
C
C ARGUMENTS:
C      infile     - input FITS file and extension number
C      outfile    - output ASCII file
C       columns   - the columns to output
C       row       - the rows to output
C       prhead    - whether to print the header or not
C       prdata    - whether to print the data or not
C       showscale - show the scaled values
C       skip      - only output every nth row of data
C       tdisp     - display using TDISP keyword format
C       page      - page the output every 24 lines
C       sensecase - whether to be case sensitive about column names
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C**************************************************************************

      subroutine fplist (infile, outfile, columns, rows, prhead,
     &     prdata, showscale, skip, tdisp, page, sensecase, status)

      character*(*) infile, outfile, columns, rows
      logical prhead, prdata, showscale, tdisp, page, sensecase
      integer skip, status

      integer iunit, ounit, htype, block, extnum, fstatus
      logical outopen, printall
      character(512) context
      character(256) filename

C initialize variables
      iunit = 15
      ounit = 16
      outopen = .false.
      printall = .false.

C get the filename and extension number
      call fcpars (infile, filename, extnum, status)
      if (extnum .lt. 0) then
         extnum = 0
         printall = .true.
      endif

C open the input file
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Error opening file ' // filename
         call fcerr (context)
         goto 999
      endif

C if a specific extension was requested, go to it
      call ftmahd (iunit, extnum+1, htype, status)

C open the output file, if one was requested
      if (outfile .ne. 'STDOUT') then
         call faopen (ounit, outfile, 2, 0, status)
         if (status .ne. 0) then
 10         context = ' Error opening output file, may exist? ' //
     &           outfile
            call fcerr (context)
            goto 998
         else
 20         outopen = .true.
         endif
      else

C if paging was requested, enable it
         if (page) ounit = -ounit
      endif

C print the header of this extension, if requested
 100  if (prhead) call flstph (iunit, ounit, outopen, status)
      if (status .ne. 0) then
         context = ' Error printing header information'
         call fcerr (context)
         goto 998
      endif

C print the data of this extension, if requested
C if this is an image extension, can't print the data
      if ((prdata) .and. (htype .ne. 0)) call flstpd (iunit, ounit,
     &     outopen, htype, columns, rows, showscale, skip,
     &     tdisp, sensecase, status)
      if (status .ne. 0) then
         context = ' Error printing data information'
         call fcerr (context)
         goto 998
      endif

C if all extensions were requested, move to the next one, and print it
      if (printall) then
         call ftmrhd (iunit, 1, htype, status)
         if (status .eq. 0) goto 100
         status = 0
      endif

 998  fstatus = 0
      call ftclos (iunit, fstatus)
      if (outopen) close (ounit)

 999  return
      end


C**************************************************************************
C SUBROUTINE:
C      flstph
C
C DESCRIPTION:
C      Print the header of this file to the appropriate location
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call flstph (iunit, ounit, outopen, status)
C
C ARGUMENTS:
C       iunit     - input unit number
C       ounit     - output unit number
C       outopen   - whether to output to file or screen
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C**************************************************************************
      subroutine flstph (iunit, ounit, outopen, status)

      integer iunit, ounit, status
      logical outopen

      integer i, nkeys, keysadd, pstatus
      character(512) context

      pstatus = 0

C find the number of lines in the header
      call ftghsp (iunit, nkeys, keysadd, status)
      if (status .ne. 0) then
         context = ' Error getting number of lines in header'
         call fcerr (context)
         goto 999
      endif

      call pgfout (ounit, outopen, ' ', pstatus)
      if (pstatus .ne. 0) goto 999
      do 100 i = 1, nkeys
         call ftgrec (iunit, i, context, status)
         call pgfout (ounit, outopen, context, pstatus)
C if pstatus is returned non-0, stop printing the header
         if (pstatus .ne. 0) goto 999
 100  continue

      if (status .ne. 0) then
         context = ' Error reading header information'
         call fcerr (context)
         goto 999
      endif

C END record is not returned by FITSIO, so put it in by hand
      context = 'END'
      call pgfout (ounit, outopen, context, pstatus)
      call pgfout (ounit, outopen, ' ', pstatus)

 999  return
      end


C**************************************************************************
C SUBROUTINE:
C      flstpd
C
C DESCRIPTION:
C      Print the current extension's data information
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C
C  11/30/1995 Jeff Guerber - changed TTYPE from char*16 to char*40
C
C NOTES:
C
C USAGE:
C       call flstpd (iunit, ounit, outopen, htype, columns,
C                     rows, showscale, skip, tdisp, sensecase, status)
C
C ARGUMENTS:
C       iunit     - input unit number
C       ounit     - output unit number
C       outopen   - whether output is to a file
C       htype     - header type of this extension
C       columns   - the columns to output
C       rows      - the rows to output
C       showscale - show the scaled values
C       skip      - only output every nth row of data
C       tdisp     - display using TDISP keyword format
C       sensecase - whether to be case sensitive about column names
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C**************************************************************************
      subroutine flstpd (iunit, ounit, outopen, htype, columns,
     &     rows, showscale, skip, tdisp, sensecase, status)

      integer iunit, ounit, htype, skip, status
      character*(*) columns, rows
      logical outopen, showscale, tdisp, sensecase

      integer maxcl, maxranges, maxdim
      parameter (maxcl = 999)
      parameter (maxranges = 15)
      parameter (maxdim = 10)

      character(25) tunit(maxcl)
      character(40) colist(maxcl), ttype(maxcl)
      character(80) extname, context, value, indices
      character(40) subset(maxcl)
      character(16) tform(maxcl), disp(maxcl)

      integer colpos(maxcl), repeat(maxcl), felem, offset, k, j
      integer startrow(maxranges), stoprow(maxranges), irow, i, ij
      integer numranges, numlen, collen, fcstln, ncols, varidat
      integer tbcol(maxcl), tfields, nrows, rowlen, pstatus
      integer ranges(maxdim, maxcl), naxis(maxcl), arrlen
      integer naxes(maxdim, maxcl), elements(maxdim)
      integer startrange(maxranges, maxdim, maxcl)
      integer stoprange(maxranges, maxdim, maxcl)
      integer incrange(maxranges, maxdim, maxcl)

      INTEGER BINARY_TBL
      PARAMETER (BINARY_TBL = 2)

      logical varflag(maxcl), good, negflag, done

      pstatus = 0
      do 1 i = 1, maxcl
         subset(i) = ' '
 1    continue

C Get the column information
      if (htype .eq. 1) then
         call ftghtb (iunit, maxcl, rowlen, nrows, tfields, ttype,
     &        tbcol, tform, tunit, extname, status)
      else
         call ftghbn (iunit, maxcl, nrows, tfields, ttype, tform,
     &        tunit, extname, varidat, status)
      endif
      if (status .ne. 0) then
         context = ' Error getting table data information'
         call fcerr (context)
         goto 999
      endif

C check which columns were requested for output
      if ((columns .eq. ' ') .or. (columns .eq. '-')) then
         ncols = tfields
         do 10 i = 1, ncols
            colist(i) = ttype(i)
            colpos(i) = i
C determine the number of dimensions for each column
            call ftgtdm (iunit, colpos(i), maxdim, naxis(i),
     &           naxes(1,i), status)
 10      continue
      else
         call fcgcls (columns, colist, ncols, negflag)

C remove the subset specification from the end of colist and put into subset
         call rmsubset (ncols, colist, subset, status)
         call fccmpl (ncols, tfields, colist, ttype, negflag, good)
         if (.not. good) then
            context = ' Column names not found in this extension '
     &           // columns
            call fcerr (context)
            goto 999
         endif
         do 20 i = 1, ncols
            call ftgcno (iunit, sensecase, colist(i), colpos(i),
     &           status)
C determine the number of dimensions for each column
            call ftgtdm (iunit, colpos(i), maxdim, naxis(i),
     &           naxes(1,i), status)
 20      continue
         if (status .ne. 0) then
            context = ' Error determining column position or dim'
            call fcerr (context)
            goto 999
         endif
      endif

C determine the repeat and tdisp output format (if needed) for each column
      call flstfm (iunit, ncols, colpos, tform, tdisp, htype,
     &     showscale, disp, numlen, varflag, repeat, status)
      if (status .ne. 0) goto 999

C find the longest column name string and dimensionality string
      collen = 0
      do 25 i = 1, ncols

C
C        If this is an ASCII string column in a binary table, reset the
C           naxes value to one to avoid trying to read non-existant
C           elements.  This needs to be done because of differences in
C           how TFORM keyword values for string columns in binary tables
C           are interpreted.  For example, whereas this program interprets
C           a TFORM value of 12A as a column containing 12 one-character
C           elements, cfitsio interprets it as one 12-character string.
C
         IF ((htype .EQ. BINARY_TBL) .AND.
     &       (INDEX (tform (i), 'A') .GT. 0)) THEN
            naxes (1, i) = 1
         ENDIF

         arrlen = fcstln(colist(i))
C if we need to specify the dimensionality
C or for variable length arrays
         if ((naxis(i) .gt. 1) .or. (naxes(1,i) .gt. 1)
     &        .or. (varflag(i))) then
C add the length of the punctuation
            arrlen = arrlen + 1 + naxis(i)
C get the length of the maximum of each array dimension
            do 23 j = 1, naxis(i)
               arrlen = arrlen + log10(real(naxes(j,i))) + 1
               if (varflag(i)) arrlen = arrlen + 4
 23         continue
         endif
         collen = max(collen, arrlen)
 25   continue

C turn off scaling, if so requested
      if (.not. showscale) then
         do 30 i = 1, ncols
            call fttscl (iunit, colpos(i), 1.0D0, 0.D0, status)
 30      continue
      endif

C check which rows were requested
      if ((rows .eq. ' ') .or. (rows .eq. '-')) then
         numranges = 1
         startrow(1) = 1
         stoprow(1) = nrows
      else
         call fcgrgs (rows, nrows, numranges, startrow, stoprow)
      endif

C check which elements of each column were requested
      do 100 i = 1, ncols
         call colparse (subset(i), naxis(i), naxes(1,i), ranges(1,i),
     &        startrange(1,1,i), stoprange(1,1,i),
     &        incrange(1,1,i), status)
 100  continue
      if (status .ne. 0) goto 999

C loop over requested rows
      do 500 i = 1, numranges
         do 400 irow = startrow(i), stoprow(i), skip

C print the row number
            call pgfout (ounit, outopen, ' ', pstatus)
            if (pstatus .ne. 0) goto 999
            write (context, 1000) irow
 1000       format ('Row ',i9)
            call pgfout (ounit, outopen, context, pstatus)
            if (pstatus .ne. 0) goto 999

C loop over the requested columns
            do 300 k = 1, ncols

C for variable length columns
               if (varflag(k)) then
                  call ftgdes (iunit, colpos(k), irow,
     &                 repeat(k), offset, status)
                  do 200 felem = 1, repeat(k)
                     if (repeat(k) .gt. 1) then
                        call idxfmt (1, felem, indices, status)
                     else
                        indices = ' '
                     endif
                     call flstvl (iunit, colpos(k), irow, felem,
     &                    disp(k), value, status)
                     if (status .ne. 0) goto 999
                     context = '  ' // colist(k)(1:fcstln(colist(k)))
     &                    // indices
                     context(collen+3:) = ' = ' // value
                     context(collen+numlen+7:) = tunit(colpos(k))
                     call pgfout (ounit, outopen, context, pstatus)
                     if (pstatus .ne. 0) goto 999
 200              continue
               else
C for non-variable length columns

C initialization for doloops routine
                  done = .false.
                  do 125 ij = 1, maxdim
                     elements(ij) = 0
 125              continue

C a subroutine rather than do loops is needed to account for the
C       unknown number of dimensions
 150              call doloops (naxis(k), ranges(1,k),
     &                 startrange(1,1,k), stoprange(1,1,k),
     &                 incrange(1,1,k), elements, done, status)
                  if (.not. done) then
                     call idxfmt (naxis(k), elements, indices, status)
                     call elemparse (indices, iunit, colpos(k), felem,
     &                    status)
                     if (status .ne. 0) goto 999
                     if ((indices .eq. '[1]') .and. (naxes(1,k) .le. 1))
     &                    indices = ' '

                     call flstvl (iunit, colpos(k), irow, felem,
     &                    disp(k), value, status)
                     if (status .ne. 0) goto 999
                     context = '  ' // colist(k)(1:fcstln(colist(k)))
     &                    // indices
                     context(collen+3:) = ' = ' // value
                     context(collen+numlen+7:) = tunit(colpos(k))
                     call pgfout (ounit, outopen, context, pstatus)
                     if (pstatus .ne. 0) goto 999

C go back to doloops subroutine
                     goto 150

C if the doloops subroutine is finished, we end up here
                  endif

C end if statement of whether this was a variable column or not
               endif
 300        continue
 400     continue
 500  continue
      call pgfout (ounit, outopen, ' ', pstatus)
      if (pstatus .ne. 0) goto 999

 999  return
      end

C***************************************************************************
C SUBROUTINE:
C      flstfm
C
C DESCRIPTION:
C      Figure out the correct formats for output
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call flstfm (iunit, ncols, colpos, tform, tdisp, htype, showscale,
C                    disp, numlen, varflag, repeat, status)
C
C ARGUMENTS:
C       iunit     - input unit number
C       htype     - header type of this extension
C       showscale - show the scaled values
C       skip      - only output every nth row of data
C       tdisp     - display using TDISP keyword format
C       htype     - extension type
C       sensecase - whether to be case sensitive about column names
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C**************************************************************************
      subroutine flstfm (iunit, ncols, colpos, tform, tdisp, htype,
     &     showscale, disp, numlen, varflag, repeat, status)

      integer iunit, ncols, colpos(ncols), numlen, repeat(ncols)
      integer htype, status

      integer maxcl
      parameter (maxcl = 999)

      character*(*) tform(maxcl), disp(ncols)
      logical tdisp, showscale, varflag(ncols)
      integer fcafml, i, dattyp, width, lform
      character(80) dummy, context
      character(8) keyword

C loop over all columns
      do 100 i = 1, ncols

C determine if there are any vector or variable length columns
         if (htype .eq. 1) then
            call fcasfm (tform(colpos(i)), dattyp, status)
            repeat(i) = 1
            varflag(i) = .false.
            if (dattyp .eq. 16) width = fcafml(tform(colpos(i)))
         else
            call ftbnfm (tform(colpos(i)), dattyp, repeat(i), width,
     &           status)
            if (dattyp .lt. 0) then
               varflag(i) = .true.
               dattyp = -dattyp
            else
               varflag(i) = .false.
            endif
            if (dattyp .eq. 16) repeat(i) = repeat(i)/width
         endif
         if (status .ne. 0) then
            context = ' Error parsing tform keyword' //
     &           tform(colpos(i))
            call fcerr (context)
            goto 999
         endif

C generate a DISP based on the datatype
         if (dattyp .eq. 11) then
            disp(i) = 'I6'
            if (showscale) call fdmpsl (iunit, colpos(i),
     &           tform(colpos(i)), disp(i), lform, status)
         else if (dattyp .eq. 14) then
            disp(i) = 'A5'
         else if (dattyp .eq. 16) then
            call ftkeyn ('A', width, disp(i), status)
         else if (dattyp .eq. 21) then
            disp(i) = 'I6'
            if (showscale) call fdmpsl (iunit, colpos(i),
     &           tform(colpos(i)), disp(i), lform, status)
         else if (dattyp .eq. 41) then
            disp(i) = 'I11'
            if (showscale) call fdmpsl (iunit, colpos(i),
     &           tform(colpos(i)), disp(i), lform, status)
         else if (dattyp .eq. 42) then
            disp(i) = '1PE15.7'
         else if (dattyp .eq. 82) then
            disp(i) = '1PE23.15'
         else if (dattyp .eq. 83) then
            context = ' Sorry, Complex data cannot be printed'
            call fcerr (context)
            status = 83
            goto 999
C                disp(i) = '(',1PE15.7,',',1PE15.7,')'
         else if (dattyp .eq. 163) then
            context = 'Sorry, Double Complex data cannot be printed'
            call fcerr (context)
            status = 163
            goto 999
C               disp(i) = '(',1PE23.15,',',1PE23.15,')'
         else
            write (context, 1001) dattyp
 1001       format (' Unknown data type: ', i5)
            call fcerr (context)
            status = dattyp
            goto 999
         endif
 100  continue
      if (status .ne. 0) goto 999

C check for TDISPs
      if (tdisp) then
         do 200 i = 1, ncols
            call ftkeyn ('TDISP', colpos(i), keyword, status)
            call ftgkys (iunit, keyword, dummy, context, status)
            if (status .eq. 0) then
               disp(i) = dummy
            else
               status = 0
            endif
 200     continue
      endif

C determine the width of the number print, based on DISP
      numlen = 0
      do 300 i = 1, ncols
         numlen = max(numlen, fcafml(disp(i)))
 300  continue

 999  return
      end


C**************************************************************************
C SUBROUTINE:
C      flstvl
C
C DESCRIPTION:
C      Read in a value from the input file and convert it to a string
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       September, 1993
C
C MODIFICATION HISTORY:
C       4/5/94 EAG Added octal and hex output
C
C NOTES:
C
C USAGE:
C       call flstvl (iunit, colpos, irow, felem, disp,
C                                value, status)
C
C ARGUMENTS:
C       iunit     - input unit number
C       colpos    - column number
C       irow      - input row number
C       felem     - element of the vector
C       disp      - display format for this value
C       value     - returned number as a string
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C**************************************************************************
      subroutine flstvl (iunit, colpos, irow, felem, disp,
     &     value, status)

      integer iunit, colpos, irow, felem, status
      character*(*) disp, value

      character(80) context, tmpstr
      logical anyf, flag, lvalue
      double precision dvalue

c read in the value
      if (index(disp,'A') .le. 0) then

C is a number, read with a real
         call ftgcfd (iunit, colpos, irow, felem, 1, dvalue, flag,
     &        anyf, status)
      else

C is a string, just read directly into value
         call ftgcfs (iunit, colpos, irow, felem, 1, value, flag,
     &        anyf, status)

C for logical columns ...
         if (status .eq. 309) then
            status = 0
            call ftgcfl (iunit, colpos, irow, felem, 1, lvalue,
     &           flag, anyf, status)
            if (lvalue) then
               value = 'T'
            else
               value = 'F'
            endif
         endif
      endif

C check for nulls
      if (flag) then
         value = 'INDEF'
      else if ((index(disp, 'I') .gt. 0) .or.
     &         (index(disp, 'Z') .gt. 0) .or.
     &         (index(disp, 'O') .gt. 0)) then

C convert the element to a string using disp
         tmpstr = disp
         write (value, '('//tmpstr//')', err=900) nint(dvalue)
      else if (index(disp, 'A') .le. 0) then
         tmpstr = disp
         write (value, '('//tmpstr//')', err=900) dvalue
      endif

      goto 999

 900  write (context, 1001) dvalue
 1001 format (' Error converting value to string', f15.7)
      call fcerr (context)

 999  return
      end

C*************************************************************************
