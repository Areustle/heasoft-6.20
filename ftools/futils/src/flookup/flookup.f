C******************************************************************************
C SELECTOR TASK:
C      flookup
C
C FILE:
C      flookup.f
C
C DESCRIPTION:
C       select rows from a file based on a lookup table
C
C AUTHOR/DATE:
C       Emily A. Greene 6/7/93
C
C MODIFICATION HISTORY:
C        Jim Ingham 6/10/94: Added output message on # of in, out rows
C        8/25/94 EAG 3.0a - add clobber, shorten strings, etc
C  	 9/29/95 (Srilal) Timestamp added
C  	11/21/95 (Srilal) Increased the size of the CONTEXT variable
C       11/30/1995 JRG 3.0c - lkupvl: fix ttype and ltype
C       09/08/1997 Banashree M Seifert 4.0
C             . In the subroutine lkupvl, the NEVENTS keyword, if exists,
C               is updated as to the no. of rows processed and then closed
C               the output file. NEVENTS keyword is for ASCA events file 
C               only.
C
C NOTES:
C
C USAGE:
C      flookup
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing column names
C      lookup  - the name of the lookup table
C      tcolumn - name of the table key column
C      lcolumn - name of the lookup key column
C      lbcol   - lower bound column name
C      ubcol   - upper bound column name
C      column  - name of the comparison column
C      bounds  - the bounds of the comparison
C      history - whether to output a history record
C      copyall - whether to copy all other extensions
C      copyprime - whether to copy only the primary array
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine glookup - gets parameters from environment
C
C******************************************************************************

      subroutine flookp

      character(160) infile,outfile,lookup
      character(80) tcolumn, lcolumn, lbcol, ubcol, column, bounds
      logical history, copyall, copyprime

      integer status

ccc      character(40) taskname
ccc      common /task/ taskname
ccc      taskname = 'flookup4.0'


      status = 0
      call ftcmsg

C get parameters from par file
      call glookup (infile, outfile, lookup, tcolumn, lcolumn, lbcol,
     &     ubcol, column, bounds, history,
     &     copyall, copyprime, status)
      if (status .ne. 0) goto 999

C do the work
      call lkupvl (infile, outfile, lookup, tcolumn, lcolumn, lbcol,
     &     ubcol, column, bounds, history,
     &     copyall, copyprime, status)
      if (status .ne. 0) goto 999

 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      glookup
C
C DESCRIPTION:
C      gets parameters from the parameter file
C
C AUTHOR/DATE:
C      Emily A. Greene  6/7/93
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C       glcol uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call glookup (infile, outfile, lookup, tcolumn, lcolumn, lbcol,
C                     ubcol, column, bounds, history, copyall,
C                     copyprime, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing column names
C      lookup  - the name of the lookup table
C      tcolumn - name of the table key column
C      lcolumn - name of the lookup key column
C      lbcol   - lower bound column name
C      ubcol   - upper bound column name
C      column  - name of the comparison column
C      bounds  - the boundary of the comparison
C      history - whether to output a history record
C      copyall - whether to copy all other extensions
C      copyprime - whether to copy only the primary array
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine glookup (infile, outfile, lookup, tcolumn, lcolumn,
     &     lbcol, ubcol, column, bounds, history,
     &     copyall, copyprime, status)

      character*(*) infile, outfile, lookup
      character*(*) tcolumn, lcolumn, lbcol, ubcol, column, bounds
      logical history, copyall, copyprime
      integer      status

      character(240) context

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the output FITS file ---

      call uclgst('outfile',outfile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the lookup FITS file ---

      call uclgst ('lookup', lookup,status)
      if ( status .ne. 0 ) then
         context = 'Could not get LOOKUP parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the tcolumn input key column ---

      call uclgst('tcolumn', tcolumn, status)
      if ( status .ne. 0 ) then
         context = 'Could not get TCOLUMN parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the lcolumn lookup key column ---

      call uclgst('lcolumn', lcolumn,status)
      if ( status .ne. 0 ) then
         context = 'Could not get LCOLUMN parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the lbcol lower bound column ---

      call uclgst('lbcol', lbcol,status)
      if ( status .ne. 0 ) then
         context = 'Could not get LBCOL parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the ubcol upper bound column ---

      call uclgst('ubcol', ubcol, status)
      if ( status .ne. 0 ) then
         context = 'Could not get UBCOL parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the column input comparision column ---

      call uclgst('column', column, status)
      if ( status .ne. 0 ) then
         context = 'Could not get COLUMN parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the bounary description of the comparision ---

      call uclgst('bounds', bounds, status)
      if ( status .ne. 0 ) then
         context = 'Could not get BOUNDS parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the history boolean ---

      call uclgsb ('history', history, status)
      if ( status .ne. 0 ) then
         context = 'Could not get HISTORY parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the copyall boolean ---

      call uclgsb ('copyall', copyall, status)
      if ( status .ne. 0 ) then
         context = 'Could not get COPYALL parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the copyprime boolean ---

      call uclgsb ('copyprime', copyprime, status)
      if ( status .ne. 0 ) then
         context = 'Could not get COPYPRIME parameter'
         call fcerr(context)
         goto 999
      endif

 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      lkupvl
C
C DESCRIPTION:
C
C AUTHOR/DATE:
C      Emily A. Greene 6/7/93
C
C MODIFICATION HISTORY:
C
C  11/30/1995 Jeff Guerber - Changed TTYPE and LTYPE to char*40.
C
C  09/08/1997 Banashree M Seifert -
C             . Before closing the output file, it modifies the
C               NEVENTS keyword to be same as the no. of data processed.
C               This was needed by ASCA processing team as only ASCA
C               events file have that keyword and that too only in the
C               primary extension and not in any other extensions.
C               Case when NEVENTS keywords is not in the primary header
C               that is, when returned error status = 202, it just sets
C               status=0 and the closes file. In short, if there is NEVENTS
C               keyword, then it modifies otherwise not. It will not 
C               introduce this NEVENTS keyword.
C
C NOTES:
C
C
C USAGE:
C      call lkupvl (infile, outfile, lookup, tcolumn, lcolumn, lbcol,
C                     ubcol, column, bounds, history, copyall,
C                     copyprime, status)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing column names
C      lookup  - the name of the lookup table
C      tcolumn - name of the table key column
C      lcolumn - name of the lookup key column
C      lbcol   - lower bound column name
C      ubcol   - upper bound column name
C      column  - name of the comparison column
C      bounds  - the bounds of the comparison
C      history - whether to output a history record
C      copyall - whether to copy all other extensions
C      copyprime - whether to copy only the primary array
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      extnumb  - extension number
C      filename - input FITS filename
C      context  - error message
C
C CALLED ROUTINES:
C      subroutine fcpars - parse infile into a filename and extension
C      subroutine fcecho - echo message to terminal
C      the FITSIO library - all subroutines beginning with ftxxxx
C
C******************************************************************************

      subroutine lkupvl (infile, outfile, lookup, tcolumn, lcolumn,
     &     lbcol, ubcol, column, bounds, history,
     &     copyall, copyprime, status)

      character*(*) infile, outfile, lookup
      character*(*) tcolumn, lcolumn, lbcol, ubcol, column, bounds
      logical history, copyall, copyprime
      integer      status

      integer maxcl
      parameter (maxcl = 999)
      integer maxrow
      parameter (maxrow = 1024)
      integer maxkey
      parameter (maxkey = 1024)

      character(160) filename
      character(40) ttype(maxcl), ltype(maxcl)
      character(25) tunit(maxcl)
      character(16) tform(maxcl), lform(maxcl)
      character(25) ltunit(maxcl)
      character(80) extname, context, lextname

      integer iunit, ounit, condition, bitpix, naxis, pcount
      integer extnum, htype, width, nrows, varidat, tfields, gcount
      integer tcolno, colno, remain, irow, rowlen
      integer block, tbcol(maxcl), lextnum, lhtype, lfields
      integer orow, nelem, count, i, goodirow, fstatus, naxes

      logical exact, single, simple, extend, tflag(maxrow)
      logical tvalflag(maxrow), anyf, good

      integer tkeyvals(maxrow)
      double precision tvals(maxrow)

      double precision lb(maxkey), ub(maxkey)
      integer key(maxkey), lkeymin, lkeymax, offset
      integer lunit, lrows, lcolno, lbcolno, ubcolno, bad

      common / lkupcm / lb, ub, key, lkeymin, lkeymax, offset,
     &     lunit, lrows, lcolno, lbcolno, ubcolno


C Initialize variables
      iunit = 15
      ounit = 16
      lunit = 17
      status = 0
      exact = .false.
      lkeymax = 1

C open the input file
      call fcpars (infile, filename, extnum, status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Unable to open input file'
         call fcerr (context)
         goto 999
      endif

C check that tcolumn and column exist in this file
      call ftmrhd (iunit, extnum, htype, status)
      if (htype .eq. 1) then
         call ftghtb (iunit, maxcl, width, nrows, tfields,
     &        ttype, tbcol, tform, tunit, extname, status)
      else if (htype .eq. 2) then
         call ftghbn (iunit, maxcl, nrows, tfields, ttype, tform,
     &        tunit, extname, varidat, status)
         call ftgkyj (iunit, 'NAXIS1', width, context, status)
      else
         context = ' Extension type not supported'
         call fcerr (context)
         goto 998
      endif

      call ftgcno (iunit, exact, tcolumn, tcolno, status)
      if (status .ne. 0) then
         context = ' specified tcolumn does not exist'
         call fcerr (context)
         goto 998
      endif
      call ftgcno (iunit, exact, column, colno, status)
      if (status .ne. 0) then
         context = ' specified column does not exist'
         call fcerr (context)
         goto 998
      endif

C go back to the primary array of the input file
      call ftmrhd (iunit, -extnum, htype, status)

C open the lookup file and check that requested columns exist
      call fcpars (lookup, filename, lextnum, status)

C EAG 8/25/93 default to first extension
      if (lextnum .eq. -99) lextnum = 1

      call ftopen (lunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Unable to open lookup file'
         call fcerr (context)
         goto 998
      endif

C check that lcolumn, lbcol and ubcol exist in this file
      call ftmrhd (lunit, lextnum, lhtype, status)
      if (lhtype .eq. 1) then
         call ftghtb (lunit, maxcl, rowlen, lrows, lfields,
     &        ltype, tbcol, lform, ltunit, lextname, status)
      else if (lhtype .eq. 2) then
         call ftghbn (lunit, maxcl, lrows, lfields, ltype, lform,
     &        ltunit, lextname, varidat, status)
      else
         context = ' Extension type not supported'
         call fcerr (context)
         goto 997
      endif

      call ftgcno (lunit, exact, lcolumn, lcolno, status)
      if (status .ne. 0) then
         context = ' specified lcolumn does not exist'
         call fcerr (context)
         goto 997
      endif
      call ftgcno (lunit, exact, lbcol, lbcolno, status)
      if (status .ne. 0) then
         context = ' specified lbcol does not exist'
         call fcerr (context)
         goto 997
      endif
      if ((ubcol .eq. ' ') .or. (ubcol .eq. '-')) then
C this is a single lookup condition
         single = .true.
      else
         single = .false.
         call ftgcno (lunit, exact, ubcol, ubcolno, status)
         if (status .ne. 0) then
            context = ' specified ubcol does not exist'
            call fcerr (context)
            goto 997
         endif
      endif

C determine the condition to check
      call lkupdc (bounds, single, condition, status)
      if (status .ne. 0) then
         context = ' unknown condition requested by bounds'
         call fcerr (context)
         goto 997
      endif

C do an initial read of the lookup table
      call lkupgt (single, status)

C input parameters all appear OK, so now create output file
      call ffinit (ounit, outfile, status)
      if (status .ne. 0) then
         context = ' Unable to open outfile - may exist? ' // outfile
         call fcerr (context)
         goto 998
      endif

C copy the primary array, if so requested
      if ((copyprime) .or. (copyall)) then
         call ftcopy (iunit, ounit, 0, status)
      else
         simple = .true.
         bitpix = 16
         naxis = 0
         pcount = 0
         gcount = 1
         extend = .true.
         call ftphpr (ounit, simple, bitpix, naxis, naxes,
     &        pcount, gcount, extend, status)
         call ftpdef (ounit, bitpix, naxis, naxes, pcount,
     &        gcount, status)
      endif
      if (history) then
         context = 'TASK:FLOOKUP on filename:'//infile
         call ftphis (ounit, context, status)
         call timestamp(ounit)
         context = '   using lookup table:'//lookup
         call ftphis (ounit, context, status)
C         call fptime (ounit, status)
         if (status .ne. 0) then
            context = ' Error creating primary array'
            call fcerr (context)
            goto 997
         endif
      endif

C copy other extension, if so requested
      if ((copyall) .and. (extnum .gt. 1)) then
         do 100 i = 1, extnum-1
            call ftmrhd (iunit, 1, htype, status)
            call ftcrhd (ounit, status)
            call ftcopy (iunit, ounit, 0, status)
 100     continue
         if (status .ne. 0) then
            context = ' Error copying extensions'
            call fcerr (context)
            goto 997
         endif
      endif

C move to the correct extension in the input file
      call ftmahd (iunit, extnum+1, htype, status)

C create the new extension in the output file
      call ftcrhd (ounit, status)
      if (htype .eq. 1) then
         call ftphtb (ounit, width, 0, tfields, ttype, tbcol,
     &        tform, tunit, extname, status)
         call ftadef (ounit, width, tfields, tbcol, tform, 0,
     &        status)
      else
         call ftphbn (ounit, 0, tfields, ttype, tform, tunit,
     &        extname, varidat, status)
         call ftbdef (ounit, tfields, tform, varidat, 0, status)
      endif
      call xcopyscale (iunit, ounit, status)
      if (history) then
         context = 'TASK:FLOOKUP on filename:'//infile
         call ftphis (ounit, context, status)
         context = '   using lookup table:'//lookup
         call ftphis (ounit, context, status)
C         call fptime (ounit, status)
         if (status .ne. 0) then
            context = ' Error initializing output extension'
            call fcerr (context)
            goto 996
         endif
      endif

C loop over the input file in maxrow chunks
      remain = nrows
      irow = 1
      orow = 1
      bad = 0
 200  if (remain .gt. maxrow) then
         nelem = maxrow
      else
         nelem = remain
      endif

C get the key column and comparison column
      call ftgcfj (iunit, tcolno, irow, 1, nelem, tkeyvals, tflag,
     &     anyf, status)
      call ftgcfd (iunit, colno, irow, 1, nelem, tvals, tvalflag,
     &     anyf, status)

C check whether to keep each row
      count = 0
      do 130 i = 1, nelem
         call lkupck (tkeyvals(i), tflag(i), tvals(i),
     &        tvalflag(i), condition, single, good)
         if (good) then
            goodirow = irow + i - 1
            call fcopyr (iunit, goodirow,
     &           ounit, orow, width, status)
            orow = orow + 1
         else
            bad = bad + 1
         endif
 130  continue
      irow = irow + nelem

      remain = remain - nelem
      if (remain .gt. 0) goto 200

C define the size of the exension correctly
      orow = orow - 1
      call ftddef (ounit, width*orow, status)
      call ftmkyj (ounit, 'NAXIS2', orow, '&', status)

C JCI
C Write out a results message:
      call fcecho('Infile # of rows    Outfile # of rows    # filtered')
      call fcecho('----------------    -----------------    ----------')
      write(context,'(4x,I8,12x,I8,10x,I8)') nrows,orow,nrows-orow
      call fcecho(context)
C JCI

C copy all additional extension, if so requested
      if (copyall) then
 900     call ftmrhd (iunit, 1, htype, status)
         call ftcrhd (ounit, status)
         call ftcopy (iunit, ounit, 0, status)
         if (status .eq. 0) goto 900
         status = 0
      endif

 996  fstatus = 0
      if (status .eq. 0) then
C         define NEVENTS for output file in the primary extension
C         before closing it
          call ftmahd (ounit, 1, htype, status) 
          call ftmkyj (ounit, 'NEVENTS', orow, '&', status)
          if(status .eq. 202) then
             status = 0
          endif
          call ftclos (ounit, fstatus)
      else
C        delete output file if an error occurred
         call ftdelt (ounit, fstatus)
      endif

 997  fstatus = 0
      call ftclos (lunit, fstatus)
 998  fstatus = 0
      call ftclos (iunit, fstatus)

 999  if (status .ne. 0) call fcerrm (status)
      return
      end

C******************************************************************************
C SUBROUTINE:
C      lkupck
C
C DESCRIPTION:
C      Check to see if this row satisfies the specified condition
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       8 June, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call lkupck (tkeyvals, tflag, tvals, tvalflag, condition, single, good)
C
C ARGUMENTS:
C   input:
C      tkeyval - table key value
C      tflag   - null flag for key value
C      tval    - table condition value
C      tvalflag - null flag for condition value
C      condition - the number of the condition to be tested
C         double conditions:
C            1 () lbcol < column < ubcol
C            2 [) lbcol <= column < ubcol
C            3 (] lbcol < column <= ubcol
C            4 [] lbcol <= column <= ubcol
C            5 )( column < lbcol or column > ubcol
C            6 ]( column <= lbcol or column > ubcol
C            7 )[ column < lbcol or column >= ubcol
C            8 ][ column <= lbcol or column >= ubcol
C          single conditions:
C            10 ( lbcol < column
C            11 [ lbcol <= column
C            12 ) column < lbcol
C            13 ] column <= lbcol
C            14 [] lbcol = column
C            15 )( column <> lbcol
C         single  - whether this is a single or double condition
C      output:
C         good - whether this is a good row or not
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message
C       status  - FITSIO error number
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C
C******************************************************************************
      subroutine lkupck (tkeyval, tflag, tval, tvalflag, condition,
     &     single, good)

      integer tkeyval
      double precision tval
      logical tflag, tvalflag, single, good
      integer condition

      integer maxkey
      parameter (maxkey = 1024)

      character(80) context

      integer lindex, status

      double precision lb(maxkey), ub(maxkey)
      integer key(maxkey), lkeymin, lkeymax, offset
      integer lunit, lrows, lcolno, lbcolno, ubcolno

      common / lkupcm / lb, ub, key, lkeymin, lkeymax, offset,
     &     lunit, lrows, lcolno, lbcolno, ubcolno


      status = 0
      good = .false.

C if one of the table values is null, this is a bad row
      if ((tflag) .or. (tvalflag)) return

C get the index of the requested key
      call lkupgk (tkeyval, single, lindex, status)
      if (status .ne. 0) then
         write (context, 1001) tkeyval
 1001    format (' WARNING: key not in lookup table', i10)
         call fcecho (context)
         return
      endif

C check the condition
      if (condition .eq. 1) then
         if ((lb(lindex) .lt. tval) .and. (ub(lindex) .gt. tval))
     &        good = .true.
      else if (condition .eq. 2) then
         if ((lb(lindex) .le. tval) .and. (ub(lindex) .gt. tval))
     &        good = .true.
      else if (condition .eq. 3) then
         if ((lb(lindex) .lt. tval) .and. (ub(lindex) .ge. tval))
     &        good = .true.
      else if (condition .eq. 4) then
         if ((lb(lindex) .le. tval) .and. (ub(lindex) .ge. tval))
     &        good = .true.
      else if (condition .eq. 5) then
         if ((lb(lindex) .gt. tval) .or. (ub(lindex) .lt. tval))
     &        good = .true.
      else if (condition .eq. 6) then
         if ((lb(lindex) .ge. tval) .or. (ub(lindex) .lt. tval))
     &        good = .true.
      else if (condition .eq. 7) then
         if ((lb(lindex) .gt. tval) .or. (ub(lindex) .le. tval))
     &        good = .true.
      else if (condition .eq. 8) then
         if ((lb(lindex) .ge. tval) .or. (ub(lindex) .le. tval))
     &        good = .true.
      else if (condition .eq. 10) then
         if (lb(lindex) .lt. tval) good = .true.
      else if (condition .eq. 11) then
         if (lb(lindex) .le. tval) good = .true.
      else if (condition .eq. 12) then
         if (lb(lindex) .gt. tval) good = .true.
      else if (condition .eq. 13) then
         if (lb(lindex) .ge. tval) good = .true.
      else if (condition .eq. 14) then
         if (lb(lindex) .eq. tval) good = .true.
      else if (condition .eq. 15) then
         if (lb(lindex) .ne. tval) good = .true.
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      lkupdc
C
C DESCRIPTION:
C      Determine the condition to be tested
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       8 June, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call lkupdc (bounds, single, condition, status)
C
C ARGUMENTS:
C   input:
C      bounds - string containing information on condition
C      single - whether this is a single lookup or bound
C   output:
C      condition - the number of the condition to be tested
C         double conditions:
C            1 () lbcol < column < ubcol
C            2 [) lbcol <= column < ubcol
C            3 (] lbcol < column <= ubcol
C            4 [] lbcol <= column <= ubcol
C            5 )( column < lbcol or column > ubcol
C            6 ]( column <= lbcol or column > ubcol
C            7 )[ column < lbcol or column >= ubcol
C            8 ][ column <= lbcol or column >= ubcol
C          single conditions:
C            10 ( lbcol < column
C            11 [ lbcol <= column
C            12 ) column < lbcol
C            13 ] column <= lbcol
C            14 [] lbcol = column
C            15 )( column <> lbcol
C       status  - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C
C******************************************************************************
      subroutine lkupdc (bounds, single, condition, status)

      character*(*) bounds
      logical single
      integer condition, status

      character(80) context
      integer fcstln

      if (bounds .eq. '()') then
         condition = 1
      else if (bounds .eq. '[)') then
         condition = 2
      else if (bounds .eq. '(]') then
         condition = 3
      else if (bounds .eq. '[]') then
         if (single) then
            condition = 14
         else
            condition = 4
         endif
      else if (bounds .eq. ')(') then
         if (single) then
            condition = 15
         else
            condition = 5
         endif
      else if (bounds .eq. '](') then
         condition = 6
      else if (bounds .eq. ')[') then
         condition = 7
      else if (bounds .eq. '][') then
         condition = 8
      else if (bounds .eq. '(') then
         condition = 10
      else if (bounds .eq. '[') then
         condition = 11
      else if (bounds .eq. ')') then
         condition = 12
      else if (bounds .eq. ']') then
         condition = 13
      else
         context = ' unknown bounds: ' // bounds(1:fcstln(bounds))
         call fcerr (context)
         status = 1
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      lkupgt
C
C DESCRIPTION:
C      Read in the appropriate portion of the lookup table
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       8 June, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C       the lookup table is passed via a common
C
C USAGE:
C      call lkupgt (single, status)
C
C ARGUMENTS:
C   input:
C      single - whether this is a single or double condition
C   output:
C       status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C
C******************************************************************************
      subroutine lkupgt (single, status)

      logical single
      integer status

      integer maxkey
      parameter (maxkey = 1024)

      integer nelem, frow

      logical flags(maxkey), anyf

      double precision lb(maxkey), ub(maxkey)
      integer key(maxkey), lkeymin, lkeymax, offset
      integer lunit, lrows, lcolno, lbcolno, ubcolno

      common / lkupcm / lb, ub, key, lkeymin, lkeymax, offset,
     &     lunit, lrows, lcolno, lbcolno, ubcolno

C decide what portion of the lookup table to read
      if (maxkey .ge. lrows) then
         nelem = lrows
         frow = 1
      else
C can't have all of table in memory at once.
C read in the next section
         nelem = maxkey
         frow = lkeymax
         if (frow .gt. lrows) frow = lrows - maxkey
         if (frow .le. 0) frow = 1
      endif

      call ftgcfj (lunit, lcolno, frow, 1, nelem, key, flags, anyf,
     &     status)
      call ftgcfd (lunit, lbcolno, frow, 1, nelem, lb, flags, anyf,
     &     status)
      if (.not. single) call ftgcfd (lunit, ubcolno, frow, 1, nelem,
     &     ub, flags, anyf, status)

      offset = key(1) - 1
      lkeymin = key(1)
      lkeymax = key(nelem)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      lkupgk
C
C DESCRIPTION:
C      Determine the index of the appropriate entry in the lookup table
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       9 June, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C       the lookup table is passed via a common
C
C USAGE:
C      call lkupgk (tkeyval, single, lindex, status)
C
C ARGUMENTS:
C   input:
C      tkeyval - the input key value
C      single  - whether this is a single or a double condition
C   output:
C      lindex  - the index of the key value
C       status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C
C******************************************************************************
      subroutine lkupgk (tkeyval, single, lindex, status)

      integer tkeyval
      logical single
      integer lindex, status

      integer maxkey
      parameter (maxkey = 1024)

      double precision lb(maxkey), ub(maxkey)
      integer key(maxkey), lkeymin, lkeymax, offset
      integer lunit, lrows, lcolno, lbcolno, ubcolno

      common / lkupcm / lb, ub, key, lkeymin, lkeymax, offset,
     &     lunit, lrows, lcolno, lbcolno, ubcolno

C find if the requested key is in the current lookup table in memory
 10   if ((lkeymin .gt. tkeyval) .or. (lkeymax .lt. tkeyval)) then
C read in the correct portion of the lookup table
         call lkupgt (single, status)
         goto 10
      endif

C check if we can just use tkeyval as the index
      lindex = tkeyval - offset
      if ((lindex .gt. 0) .and. (lindex .le. maxkey)) then
         if (key(lindex) .eq. tkeyval) return
      endif

C must search for the index
      call lkupgi (tkeyval, lindex, status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      lkupgi
C
C DESCRIPTION:
C      Determine the index of the appropriate entry in the lookup table
C      for non-direct lookups
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       9 June, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C       the lookup table is passed via a common
C
C USAGE:
C      call lkupgi (tkeyval, lindex, status)
C
C ARGUMENTS:
C   input:
C      tkeyval - the input key value
C   output:
C      lindex  - the index of the key value
C       status  - error number
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C       FTxxxx   - FITSIO routines
C
C******************************************************************************
      subroutine lkupgi (tkeyval, lindex, status)

      integer tkeyval
      integer lindex, status

      integer maxkey
      parameter (maxkey = 1024)

      integer step

      double precision lb(maxkey), ub(maxkey)
      integer key(maxkey), lkeymin, lkeymax, offset
      integer lunit, lrows, lcolno, lbcolno, ubcolno

      common / lkupcm / lb, ub, key, lkeymin, lkeymax, offset,
     &     lunit, lrows, lcolno, lbcolno, ubcolno

C do a cut-in-half type search through key to determine index
      lindex = maxkey / 2
      step = maxkey / 4

 10   if (key(lindex) .eq. tkeyval) then
         return
      else if (key(lindex) .gt. tkeyval) then
         lindex = lindex - step
      else
         lindex = lindex + step
      endif
      step = step / 2
      if (step .gt. 0) go to 10

C the key was not found
      status = 1

      return
      end

C******************************************************************************
