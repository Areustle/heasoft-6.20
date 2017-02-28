C******************************************************************************
C SELECTOR TASK:
C      fmerge
C
C FILE:
C      fmerge.f
C
C DESCRIPTION:
C      Merges the common columns from two input FITS files and
C      creates a new output file from these.
C
C AUTHOR/DATE:
C      Janice Tarrant  1/31/92
C
C MODIFICATION HISTORY:
C       August, 1992 - added hidden parameter of merged extension name, uses
C           EAG        1st input file name as extension name as default
C
C                      Allow for any number of input files to be merged
C
C                      If the hidden parameter copyprime is true, MOST of the
C                      keywords from the primary header, and the primary array
C                      (if there is one), are copied to the output file.  If
C                      false, only a minimal primary header is created.
C
C                      All additional keywords from 1st file extension
C                      header are copied to the extension header of the new
C                      FITS file.  Additional keyowrds from the 2nd and
C                      subsequent files are NOT copied.
C
C       10/22/92 (EAG) - added history parameter: whether to write history
C                        record
C
C       10/28/92 (EAG) - added lastkey parameter to copy keywords from the
C                        last input file
C       12/2/92 (EAG)  - fixed columns="-" problem
C       2/4/93  (EAG)  - added variable length array capability
C       3/2/93  (EAG)  - fixed problem with vector ASCII columns
C       4/16/93 (EAG)  - fixed core dump when only one input file
C       8/23/93 (EAG)  - allow lastkey = "-"
C       11/19/93 (EAG) - fixed problem with vectors longer than 1024
C       3/15/94 EAG 2.8a - rationalized character sizes
C       8/29/94 EAG 3.0a - scaling isn't passed on
C       11/30/1995 JRG 3.0b - fimgex: Fixed TTYPE1, TTYPE2, MTYPE
C       2/25/96 (Srilal) 3.0c - timestamp added
C       12/10/1996 Jeff Guerber - Major improvements in error reporting.
C          Inlined & removed tiny routines figmtt, figmtu.  Removed unused
C          routine xmerge. Removed case sensitivity. Increased number of files.
C       3/31/1997 Jeff Guerber - figman, figmbn calls had not been updated;
C          g77 didn't like "call fcecho('...'//filename)".  3.1b.
C      10/17/97 PDW 3.1c - Fixed bad gcount value
C       3/24/98 PDW 3.1d - Wasn't handling bit columns
C      1998-10-13 Jeff Guerber 3.1e - In figmbf, parse tforms with ftbnfm
C          and compare results instead of directly, so eg. D matches 1D.
C          In fimgex, if exact is false, ignore case of column names.
C      1998-10-14 Jeff Guerber - ...but preserve case of output column name.
C      2000-05-19 3.1f Ning Gan  - increased the string length of columns
C                              and lastkey from 80 to 255.
C      2000-12-21      MJT - initialized ftstatus variable in fimgcl (caused 
C                            bogus error messages when columns=-)
C
C NOTES:
C      fmerge supported in IRAF and HOST environments
C      complex data types not supported
C
C USAGE:
C      HOST: call fmerge
C      IRAF: task fmerge
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infiles - input FITS files and extension numbers
C      outfile - output FITS file
C      columns - list of columns to be merged
C      mextname - merged extension name
C      copyprime - whether to copy primary header and array
C      lastkey - list of keywords to copy from last input file
C      history - whether to add a history record to outfile
C
C CALLED ROUTINES:
C      subroutine gmerge - get parameters from parameter file
C      subroutine fimgex - merge the FITS extensions
C
C******************************************************************************
      subroutine fmerge
      character(256) infiles, outfile
      character(255) columns, lastkey
      character(80) mextname
C      character(80) columns, mextname, lastkey
      logical copyprime, history

      character(40) taskname
      common /task/ taskname

      taskname = 'fmerge4.1'

      call ftcmsg

C  get parameters from parameter file
      call gmerge(infiles,outfile,columns,mextname,
     &     copyprime,lastkey,history)

C  merge columns to new FITS file
      call fimgex(infiles,outfile,columns,mextname,
     &     copyprime,lastkey,history)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      gmerge
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Janice Tarrant  1/31/92
C
C MODIFICATION HISTORY:
C       18 August, 1992 - added MEXTNAME parameter (EAG)
C       26 August, 1992 - added COPYPRIME parameter (EAG)
C       22 October, 1992 - added HISTORY parameter (EAG)
C       26 October, 1992 - added LASTKEY parameter (EAG)
C
C NOTES:
C       gmerge uses F77/VOS like calls to read parameters from .par file
C
C USAGE:
C      call gmerge(infiles,outfile,columns,mextname,lastkey,copyprime)
C
C ARGUMENTS:
C      infiles - input FITS files and extension numbers
C      outfile - output FITS file
C      columns - list of columns to be merged
C      mextname - merged extension name
C      copyprime - whether to copy primary header and array
C      lastkey - list of keywords to update from the last input file
C      history - whether to add a history record to outfile
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      ftstatus - fitsio error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get boulean parameter
C
C******************************************************************************

      subroutine gmerge(infiles,outfile,columns,mextname, copyprime,
     &     lastkey, history)
      character*(*) infiles, outfile, columns, mextname,lastkey
      logical copyprime, history

      character(80) context
      integer ftstatus

C  initialize variables
      ftstatus = 0

C  get the name of the input FITS files
      call uclgst('infiles',infiles,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get INFILES parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfile,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  get the columns list
      call uclgst('columns',columns,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get COLUMNS parameter'
         call fcerr(context)
         goto 999
      endif

C get the merged extension name
      call uclgst('mextname',mextname,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get MEXTNAME parameter'
         call fcerr(context)
         goto 999
      endif

C get whether to copy primary header and array
      call uclgsb('copyprime',copyprime,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get COPYPRIME parameter'
         call fcerr(context)
         goto 999
      endif

C get the list of keyword to copy from the last input file
      call uclgst ('lastkey', lastkey, ftstatus)
      if (ftstatus .ne. 0) then
         context = ' could not get LASTKEY parameter'
         call fcerr (context)
         goto 999
      endif
C get whether to add a history record to outfile
      call uclgsb('history', history,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not get HISTORY parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         stop
      endif

      return
      end



C******************************************************************************
C SUBROUTINE:
C      fimgex
C
C DESCRIPTION:
C      Merge the columns of two FITS files' extensions
C
C AUTHOR:
C      Janice Tarrant  1/31/92
C
C MODIFICATION HISTORY:
C       18 August, 1992 - added MEXTNAME parameter, multiple input files,
C                         header keywords copied (EAG)
C                         changed get header calls to new FITSIO routines
C       22 October, 1992 - added HISTORY parameter: whether to write history
C                          record to outfile
C       28 October, 1992 - added LASTKEY parameter
C       Jeff Guerber 11/30/1995 - Changed TTYPE1, TTYPE2, MTYPE to char*40
C       Jeff Guerber 12/10/1996 - Major improvements in error handling.
C           Inlined figmtt, figmtu.  Increased size of file lists.
C
C NOTES:
C
C USAGE:
C      call fimgex(infiles,outfile,columns,mextname,copyprime,lastkey,history)
C
C ARGUMENTS:
C      infiles - input FITS files and extension numbers
C      outfile - output FITS file
C      columns - list of columns to be merged
C      mextname - merged extension name
C      copyprime - whether to copy primary array and header
C       lastkey - list of keywords to copy from last input file
C      history - whether to write history record to outfile
C
C PRIMARY LOCAL VARIABLES:
C      filename1/2  - name of first/second input FITS file
C      context      - error message
C      errstr       - fitsio error message string
C      inopen1/2    - first/second input file open flag
C      outopen      - output file open flag
C      negflag      - exclude name flag
C      goodlist     - good name list flag
C      scaling      - no scales in output file flag
C      newscale     - new scale for output file column flag
C      maxcl        - maximum number of columns
C      extnum1/2    - first/second FITS file extension number
C      ftstatus     - fitsio error number
C      tcols        - total number of columns in output FITS file
C      colpos1/2    - column name position in first/second FITS file
C      mbcol        - merged file first character column number
C      mrowlen      - merged file row length
C      mfields      - merged file number of fields
C      fistatus     - error number
C
C CALLED ROUTINES:
C      subroutine fccmpl - compare to lists of strings
C      subroutine fcgcls - get column list based on parameter
C      subroutine fcpars - parse off filename and extension number
C      subroutine figmaf - get merged ASCII tform keyword
C      subroutine figman - get merged ASCII null values
C      subroutine figmbf - get merged binary format
C      subroutine figmbn - get merged binary null values
C      subroutine figmsf - get merged scale factors
C      subroutine figmtt - get merged ttype keyword
C      subroutine figmtu - get merged tunit keyword
C      subroutine figmxn - get merged extname keyword
C      subroutine fimgcl - merge columns
C      subroutine ftadef - define ASCII extension
C      subroutine ftbdef - define binary extension
C      subroutine ftclos - close a FITS file
C      subroutine ftcrhd - create a new header
C      subroutine ftghbn - get binary table header keywords into CHU
C      subroutine ftgcno - get column number with by column name
C      subroutine ftghtb - get ascii table header keywords into CHU
C      subroutine ftinit - create a new FITS file
C      subroutine ftmrhd - relative move to FITS header
C      subroutine ftopen - open a FITS file
C      subroutine ftphbn - put binary table header keywords into CHU
C      subroutine ftpdef - define structure of primary array
C      subroutine ftphis - put history keyword record
C      subroutine ftphpr - put primary header keywords into CHU
C      subroutine ftphtb - put ASCII table header keywords into CHU
C
C******************************************************************************
      subroutine fimgex(infiles,outfile,columns,mextname,copyprime,
     &     lastkey, history)
      implicit none
      character*(*) infiles, outfile, columns, mextname, lastkey
      logical copyprime, history

      integer maxcl
      parameter (maxcl = 999)
      integer maxf
      parameter (maxf = 1024)

      character(512) context
      character(256) filename1, filename2
      character(256) filelist(maxf)
      character(8)  keylist(maxf)
      character(80) card, extname1, extname2, comment
      character(40) ttype1(maxcl), ttype2(maxcl), mtype(maxcl)
      character(16) tform1(maxcl), tform2(maxcl), mform(maxcl)
      character(25) tunit1(maxcl),tunit2(maxcl), munit(maxcl)
      character(40) colist(maxcl)

      logical inopen1, inopen2, outopen, simple,extend, exact,
     &     negflag, goodlist, scaling(maxcl),newscale(maxcl)
      integer fcafml, fcstln, mcount
      integer numfiles, j, status, dtype, width,repeat, nkeys
      integer i, extnum1,extnum2,ftstatus,iunit1,iunit2, block,
     &     ounit, htype1, htype2, rowlen1, rowlen2, nrows1,
     &     nrows2, tfields1, tfields2, tbcol1(maxcl),mrows,
     &     tbcol2(maxcl), varidat, bitpix, naxis,naxes(99),
     &     pcount,gcount,tcols,colpos1(maxcl),colpos2(maxcl),
     &     mbcol(maxcl), mrowlen, mfields, fistatus, mgstatus
      character(8) keyroot, keyword

      double precision scale

C  initialize variables
      ftstatus = 0
      fistatus = 0
      iunit1 = 15
      iunit2 = 16
      ounit = 18
      exact = .false.
      inopen1 = .false.
      inopen2 = .false.
      outopen = .false.

C get the file names using the get columns routine
      call fcgcls (infiles, filelist, numfiles, negflag)

C find out the total number of rows in all the files
      call fmgfil (iunit1, filelist, numfiles, mrows, mcount, ftstatus)
      if (ftstatus .ne. 0) goto 999

C  get the filename and extension number
      call fcpars(filelist(1),filename1,extnum1,ftstatus)

C EAG 8/25/93 default to 1st extension
      if (extnum1 .eq. -99) extnum1 = 1

C  if extension is 0 then give error and exit
      if (extnum1 .eq. 0) then
         context = 'Primary Extension Not Supported, file ' // filename1
         call fcerr(context)
         goto 999
      endif

C  open the 1st input FITS file
      call ftopen(iunit1,filename1,0,block,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'Unable to open file1: ' // filename1
         call fcerr(context)
         goto 999
      endif
      inopen1 = .true.

C read the primary header
      call ftghpr (iunit1, maxcl, simple, bitpix, naxis, naxes,
     &     pcount, gcount, extend, ftstatus)

C  open the output FITS file (with clobber processing)
      call ffinit(ounit,outfile,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'Unable to open outfile, may exist? ' // outfile
         call fcerr(context)
         goto 999
      endif
      outopen = .true.

C  construct simple primary header for the merged file using input file info
      if (.not. copyprime) then
         bitpix = 16
         naxis = 0
         naxes(1) = 0
         call ftiimg(ounit, bitpix, naxis, naxes, ftstatus)
      else
C copy all keywords from the input primary header to output file
         call ftcopy (iunit1, ounit, 0, ftstatus)
      endif
      if (ftstatus .ne. 0) then
         context = ' Unable to write primary header'
         call fcerr(context)
         goto 999
      endif

      if (history) then
         context = ' TASK:FMERGE on file '//filename1
         call ftphis(ounit,context,ftstatus)
	 call timestamp(ounit)
      endif

C  move to the extension in the 1st input file
      call ftmrhd(iunit1,extnum1,htype1,ftstatus)
      if (ftstatus .ne. 0) then
         write(context,'(a,i4,a,a)') 'Error moving to extension number',
     &       extnum1, ' in file ',
     &       filename1(:MIN(fcstln(filename1),len(context)-45))
         call fcerr(context)
         goto 999
      endif

C  get header depending on the extension type
      if ( htype1 .eq. 1 ) then
         call ftghtb(iunit1,maxcl,rowlen1,nrows1,tfields1,ttype1,tbcol1,
     &        tform1,tunit1,extname1,ftstatus)
      else if ( htype1 .eq. 2 ) then
         call ftghbn(iunit1,maxcl,nrows1,tfields1,ttype1,tform1,tunit1,
     &        extname1,varidat,ftstatus)
      else
          write (context, '(a,i3,a,a)')
     &        'Unsupported file extension type: ', htype1, ' in file ',
     &        filelist(1)(:MIN(fcstln(filelist(1)),len(context)-45))
          call fcerr(context)
          goto 999
      endif

C  create a new extension in the merged file
      call ftcrhd(ounit,ftstatus)

C check for input MEXTNAME.  If blank, use EXTNAME1
      if (mextname .eq. ' ') mextname = extname1

C  get the list of column names to be merged, and their positions
C  in the input file
      if ((columns .eq. ' ') .or. (columns .eq. '-')) then
         do 10 i = 1, tfields1
            colist(i) = ttype1(i)
            colpos1(i) = i
 10      continue

C use values from 1st input file for output file
         tcols = tfields1
         mrowlen = rowlen1
         mfields = tfields1

         do 20 i = 1, tcols
            mtype(i) = ttype1(i)
            mbcol(i) = tbcol1(i)
            mform(i) = tform1(i)
            munit(i) = tunit1(i)
            if (.not. exact) call ftupch( ttype1(i) )
 20      continue
      else
         call fcgcls(columns,colist,tcols,negflag)
         call fccmpl(tcols,tfields1,colist,ttype1,negflag,goodlist)
         if (.not. goodlist) then
            context = 'Error, missing column list name, file '
     &           // filelist(1)
            call fcerr(context)
            goto 999
         endif
         do 30 i = 1, tcols
            call ftgcno(iunit1,exact,colist(i),colpos1(i),ftstatus)
 30      continue
         mrowlen = 0
         mfields = tcols
         do 40 i = 1, tcols
            mtype(i) = ttype1(colpos1(i))
            munit(i) = tunit1(colpos1(i))
            mform(i) = tform1(colpos1(i))
            if (.not. exact) call ftupch( ttype1(colpos1(i)) )

C  find out mrowlen for ASCII table
            if (htype1 .eq. 1) then
               mbcol(i) = 1 + mrowlen
               mrowlen = mrowlen + fcafml(tform1(colpos1(i))) + 1
            endif
 40      continue
         mrowlen = mrowlen - 1
      endif

C check for any scaling in the first file
      if (ftstatus .ne. 0) goto 999
      do 50 i = 1,  tcols
         scaling(i) = .false.
         status = 0
         call ftcmsg
         if (htype1 .eq. 1) then
            if ((mform(i)(1:1) .eq. 'I') .or.
     &           (mform(i)(1:1) .eq. 'E') .or.
     &           (mform(i)(1:1) .eq. 'D')) then
               keyroot = 'TSCAL'
               call ftkeyn(keyroot, colpos1(i), keyword, status)
               call ftgkyd(iunit1, keyword, scale, comment, status)
               if (status .eq. 0) then
                  scaling(i) = .true.
               else

C check for TZERO
                  status = 0
                  keyroot = 'TZERO'
                  call ftkeyn (keyroot, colpos1(i), keyword, status)
                  call ftgkyd (iunit1, keyword, scale, comment, status)
                  if (status .eq. 0) scaling(i) = .true.
               endif
            endif
         else
            call ftbnfm(mform(i),dtype,repeat,width,ftstatus)
            if ((dtype .eq. 11) .or. (dtype .eq. 21) .or.
     &           (dtype .eq. 41) .or. (dtype .eq. 42) .or.
     &           (dtype .eq. 82)) then
               keyroot = 'TSCAL'
               call ftkeyn(keyroot,colpos1(i),keyword,status)
               call ftgkyd(iunit1,keyword,scale,comment,status)
               if (status .eq. 0) then
                  scaling(i) = .true.
               else

C check for TZERO
                  status = 0
                  keyroot = 'TZERO'
                  call ftkeyn (keyroot, colpos1(i), keyword, status)
                  call ftgkyd (iunit1, keyword, scale, comment, status)
                  if (status .eq. 0) scaling(i) = .true.
               endif
            endif
         endif
 50   continue

      if (htype1 .eq. 1) then
C  write a merged header
         call ftphtb(ounit,mrowlen,mrows,mfields,mtype,mbcol,
     &        mform,munit,mextname,ftstatus)
      else
C  write a merged header (initial PCOUNT = 0)
         mcount = 0
         call ftphbn(ounit,mrows,mfields,mtype,mform,munit,
     &        mextname,mcount,ftstatus)
      endif

C copy all other keywords from the input extension header to output file
      call xcopynoscale (iunit1, ounit, ftstatus)
c      call xmerge (iunit1, ounit, filename1, ftstatus)
      if (ftstatus .ne. 0) then
         context = 'Unable to write extension header'
         call fcerr(context)
         goto 999
      endif

C write the merged scale factors
      call figmsf (scaling, tcols, iunit1, colpos1, ounit)

C write the merged null values
      if (htype1 .eq. 1) then
         call figman (iunit1, colpos1, tcols, ounit, 1, colist,
     &        filelist(1))
      else
         call figmbn (iunit1, colpos1, tcols, ounit, 1, colist,
     &        filelist(1))
      endif

C write the merged data
      mrows = 1
      call fimgcl(iunit1,htype1,tform1,colpos1,colist,mrows,nrows1,
     &    scaling, tcols, ounit, mgstatus)
      if (mgstatus .ne. 0) then
          context = 'Problem merging data from file '//filelist(1)
          call fcerr(context)
          call fcerr('Continuing with next...')
      endif

C  write a history header keyword
      if (history) then
         write (context, 1110) nrows1, filename1
 1110    format (' TASK:FMERGE copied ',i7,' rows from file ',a30)
         call ftphis(ounit,context,ftstatus)
      endif

C if only one file specified, just copy from input to output and quit
      if (numfiles .le. 1) goto 995

C LOOP ON SUBSEQUENT INPUT FILES
C open the 2nd (and all additional) input fits files

      do 500 j = 2, numfiles
         call fcpars(filelist(j),filename2,extnum2,ftstatus)

C EAG 8/25/93 default to 1st extension
         if (extnum2 .eq. -99) extnum2 = 1

C  if extension is 0 then give error and exit
         if (extnum2 .eq. 0) then
            context = 'Primary Extension Not Supported, file ' //
     &           filename2
            call fcerr(context)
            goto 999
         endif

         call ftopen(iunit2,filename2,0,block,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'Unable to open input file '//filename2
            call fcerr(context)
            goto 999
         endif
         inopen2 = .true.

         call ftmrhd(iunit2,extnum2,htype2,ftstatus)
         if (ftstatus .ne. 0) then
             write(context,'(a,i4,a,a)')
     &           'Error moving to extension number', extnum1,
     &           ' in file ',
     &           filename1(:MIN(fcstln(filename1),len(context)-45))
             call fcerr(context)
             goto 999
         endif

C  check that extension types are the same
         if (htype1 .ne. htype2) then
            context = 'Header type differs from first file''s in '
     &           // filelist(j)
            call fcerr(context)
            goto 999
         endif

C  get header depending on the extension type
         if ( htype2 .eq. 1 ) then
            call ftghtb (iunit2, maxcl, rowlen2, nrows2, tfields2,
     &           ttype2, tbcol2, tform2, tunit2, extname2, ftstatus)
         else if ( htype2 .eq. 2 ) then
            call ftghbn (iunit2, maxcl, nrows2, tfields2, ttype2,
     &           tform2, tunit2, extname2, varidat, ftstatus)
         else
          write (context, '(a,i3,a,a)')
     &        'Unsupported file extension type: ', htype2, ' in file ',
     &        filelist(j)(:MIN(fcstln(filelist(j)),len(context)-45))
             call fcerr(context)
            goto 999
         endif

C check that extension names are the same, if not give warning
         if (extname1 .ne. extname2) then
            context = ' Warning: extension name differs from first '
     &           // 'file''s in ' // filelist(j)
            call fcecho (context)
         endif

C  get the list of column names to be merged, and their positions
C  in the input files
         negflag = .false.
         call fccmpl(tcols,tfields2,colist,ttype2,negflag,goodlist)
         if (.not. goodlist) then
             context = 'Error, missing column list names, file '
     &           // filelist(j)
             call fcerr(context)
             goto 999
         endif
         do 120 i = 1, tcols
             call ftgcno(iunit2,exact,colist(i),colpos2(i),ftstatus)
  120    continue

C  compare the header keywords for the merged extension
         do 140 i = 1, tcols

C  check that column labels match
             if (.not. exact) call ftupch( ttype2(colpos2(i)) )
             if (ttype1(colpos1(i)) .ne. ttype2(colpos2(i))) then
                 context = 'Warning: different ttypes for column ' //
     &               colist(i)
                 call fcecho(context)
                 context = filelist(1)(1:fcstln(filelist(1))) //
     &               ': ' // ttype1(colpos1(i))
                 call fcecho(context)
                 context = filelist(j)(1:fcstln(filelist(j))) //
     &               ': ' // ttype2(colpos1(i))
                 call fcecho(context)
             endif

C  check that column units match
             if (tunit1(colpos1(i)) .ne. tunit2(colpos2(i))) then
                 context = 'Warning: different tunits for column ' //
     &               colist(i)
                 call fcecho(context)
                 context = filelist(1)(1:fcstln(filelist(1))) //
     &               ': ' // tunit1(colpos1(i))
                 call fcecho(context)
                 context = filelist(j)(1:fcstln(filelist(j))) //
     &               ': ' // tunit2(colpos1(i))
                 call fcecho(context)
             endif

C  compare the tform header keyword, and find out if any values are scaled
            if (htype1 .eq. 1) then
               call figmaf(iunit2,tform1(colpos1(i)),
     &              tform2(colpos2(i)),
     &              colpos2(i),colist(i), newscale(i), fistatus)
            else
               call figmbf(iunit2,tform1(colpos1(i)),
     &              tform2(colpos2(i)),colpos2(i),colist(i),
     &              newscale(i),fistatus)
            endif
            if (fistatus .ne. 0) then
                call fcerr( 'file ' // filelist(j) )
                goto 999
            endif
 140     continue

C write the merged null values
         if (htype1 .eq. 1) then
            call figman (iunit2, colpos2, tcols, ounit, 2, colist,
     &           filelist(j))
         else
            call figmbn (iunit2, colpos2, tcols, ounit, 2, colist,
     &           filelist(j))
         endif

C write the merged data; try to continue on error
         call fimgcl(iunit2, htype2, tform2, colpos2, colist, mrows,
     &        nrows2, newscale, tcols, ounit, mgstatus)
         if (mgstatus .ne. 0) then
             context = 'Problem merging data from file '//filelist(j)
             call fcerr(context)
             call fcerr('Continuing with next...')
         endif

C  write a history header keyword
         if (history) then
            write (context, 1111) nrows2, filename2
 1111       format (' TASK:FMERGE appended ',i7,' rows from file ',a30)
            call ftphis(ounit,context,ftstatus)
         endif

C loop back for next input file
         if (j .ne. numfiles) then
            call ftclos(iunit2, ftstatus)
            inopen2 = .false.
         endif
 500  continue

C copy additional requested keywords
      if ((fcstln(lastkey) .le. 0) .or. (lastkey .eq. '-')) goto 995
      call fcgcls (lastkey, keylist, nkeys, negflag)
      do 600 i = 1, nkeys
         call ftupch (keylist(i))
         call fttkey (keylist(i), ftstatus)
         if (ftstatus .ne. 0) then
            context = 'Key contains illegal character -'//keylist(i)
            call fcecho (context)
            ftstatus = 0
            goto 600
         endif
         call ftgcrd (iunit2, keylist(i), card, ftstatus)
         if (ftstatus .ne. 0) then
            context = 'Keyword not found - '//keylist(i)
            call fcecho (context)
            ftstatus = 0
            goto 600
         endif

C update keyword card, add it if non-existent
         call ftmcrd (ounit, keylist(i), card, ftstatus)
         if (ftstatus .eq. 202) then
            ftstatus = 0
            call ftprec (ounit, card, ftstatus)
         endif
         if (ftstatus .ne. 0) then
            context = 'Problem writing key - '//keylist(i)
            call fcecho (context)
            ftstatus = 0
            goto 600
         endif
 600  continue
      goto 995

C  close the FITS files, exit on fitsio error
 999  if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ftstatus = 0
         if (outopen) call ftdelt (ounit, ftstatus)
         outopen = .false.
      endif
      call fcerr('Fatal error!')

  995 if (inopen1)  call ftclos(iunit1,ftstatus)
      if (inopen2)  call ftclos(iunit2,ftstatus)
      if (outopen)  call ftclos(ounit,ftstatus)

 1000 format(A45,I3)
 1001 format(A21,A,A5,A)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      figmaf
C
C DESCRIPTION:
C      gets the tform and scaling for 2nd and subsequent files with
C      ASCII extension
C
C AUTHOR/DATE:
C      Janice Tarrant  2/5/92
C
C MODIFICATION HISTORY:
C       25 August, 1992 - modified to check 2nd and subsequent files for
C                         column format compatibility and scaling (EAG)
C
C NOTES:
C
C USAGE:
C      call figmaf(iunit2,tform1,tform2,colpos2,colname, newscale, fistatus)
C
C ARGUMENTS:
C      iunit2    - second file logical unit number
C      tform1    - first extension column format
C      tform2    - second extension column format
C      colpos2   - position of 2nd column
C      colname   - merged file column name
C      newscale  - column new scale flag
C      fistatus  - error status
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      keyroot  - root string for keyword name
C      keyword  - name of a keyword
C      comment  - keyword comment field
C      ftstatus - FITS error status
C      scale2 - scaling factor for first/second FITS file column
C
C CALLED ROUTINES:
C      subroutine ftgkyd - get double keyword value
C      subroutine ftkeyn - build a keyword
C
C******************************************************************************
      subroutine figmaf(iunit2,tform1,tform2,colpos2,colname,newscale,
     &     fistatus)
      character*(*) tform1, tform2, colname
      logical newscale
      integer iunit2, colpos2, fistatus

      character(8) keyroot, keyword
      character(80) context, comment
      integer status2
      double precision scale2

C  check that column formats are compatible
      if (tform1(1:1) .ne. tform2(1:1)) then
         fistatus = 13
         context = 'incompatible tforms for column '//colname
         call fcerr(context)
         goto 999
      else
C  for integer, and exponential and double real,
C  check the scale and zero factors
         status2 = 0
         newscale = .false.
         if ((tform1(1:1) .eq. 'I') .or.
     &        (tform1(1:1) .eq. 'E') .or.
     &        (tform1(1:1) .eq. 'D')) then
            keyroot = 'TSCAL'
            call ftkeyn(keyroot,colpos2,keyword,status2)
            call ftgkyd(iunit2,keyword,scale2,comment,status2)
            if (status2 .eq. 0) then
               newscale = .true.
            else
               status2 = 0
               keyroot = 'TZERO'
               call ftkeyn(keyroot,colpos2,keyword,status2)
               call ftgkyd(iunit2,keyword,scale2,comment,status2)
               if (status2 .eq. 0) newscale = .true.
            endif
         endif
      endif

      call ftcmsg
 999  continue
      return
      end

C******************************************************************************
C SUBROUTINE:
C      figmbf
C
C DESCRIPTION:
C      gets the tform header keyword for a merged binary extension
C
C AUTHOR/DATE:
C      Janice Tarrant  3/3/92
C
C MODIFICATION HISTORY:
C       25 August, 1992 - modified so checks for tform compatibility for 2nd
C                         and subsequent files, and checks for scaling (EAG)
C       1998-10-13 Jeff Guerber - Use results of ftbnfm for comparisons.
C
C NOTES:
C
C USAGE:
C      call figmbf(iunit2,tform1,tform2,colpos2,colname,newscale,fistatus)
C
C ARGUMENTS:
C      iunit2    - second file logical unit number
C      tform1    - first extension column format
C      tform2    - second extension column format
C      colpos2   - position of 2nd column
C      colname   - merged file column name
C      newscale  - column new scale flag
C      fistatus  - error status
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      keyroot  - root string for keyword name
C      keyword  - name of a keyword
C      comment  - keyword comment field
C      ftstatus - FITS error status
C      scale2 - scaling factor for first/second FITS file column
C
C CALLED ROUTINES:
C      subroutine ftbnfm - get binary format
C      subroutine ftgkyd - get double keyword value
C      subroutine ftkeyn - build a keyword
C
C******************************************************************************
      subroutine figmbf(iunit2,tform1,tform2,colpos2,colname,newscale,
     $     fistatus)
      character*(*) tform1, tform2, colname
      logical newscale
      integer iunit2, fistatus, colpos2

      character(8) keyroot, keyword
      character(80) context, comment
      integer status2
      integer dtype, repeat, width, dtype2, repeat2, width2, ftstatus
      data dtype/0/, repeat/0/, width/0/, dtype2/0/, repeat2/0/,
     &    width2/0/, ftstatus/0/
      double precision scale2

C  check that column formats are compatible
      call ftbnfm(tform1,dtype,repeat,width,ftstatus)
      call ftbnfm(tform2,dtype2,repeat2,width2,ftstatus)
      if ((dtype .ne. dtype2) .or. (repeat .ne. repeat2) .or.
     &    (width .ne. width2)) then
         fistatus = 13
         context = 'incompatible tforms for column '//colname
         call fcerr(context)
         goto 999
      else
C  for byte, short and long integer, and exponential and double real,
C  check the scale and zero factors
         newscale = .false.
         status2 = 0
         if ((dtype .eq. 11) .or. (dtype .eq. 21) .or.
     &        (dtype .eq. 41) .or. (dtype .eq. 42) .or.
     &        (dtype .eq. 82)) then
            keyroot = 'TSCAL'
            call ftkeyn(keyroot,colpos2,keyword,status2)
            call ftgkyd(iunit2,keyword,scale2,comment,status2)
            if (status2 .eq. 0) then
               newscale = .true.
            else
               status2 = 0
               keyroot = 'TZERO'
               call ftkeyn(keyroot,colpos2,keyword,status2)
               call ftgkyd(iunit2,keyword,scale2,comment,status2)
               if (status2 .eq. 0) newscale = .true.
            endif
         endif
      endif
      call ftcmsg

 999  continue
c MJT 05July 1996 commented out (not used!) and linux complains...
c 1000 format(I,A)
      return
      end

C******************************************************************************
C SUBROUTINE:
C      figmsf
C
C DESCRIPTION:
C      gets the tscal and tzero header keywords for a merged ASCII extension
C
C AUTHOR/DATE:
C      Janice Tarrant  2/10/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call figmsf(scaling,tcols,iunit,colpos,ounit)
C
C ARGUMENTS:
C      scaling - merged column scale flag
C      tcols   - total number of columns in merged extension
C      iunit   - input file logical unit number
C      colpos  - extension column position
C      ounit   - merged file logical unit number
C
C PRIMARY LOCAL VARIABLES:
C      keyroot  - root string for keyword name
C      keyword  - name of a keyword
C      comment  - keyword comment field
C      ftstatus - FITS error status
C      scale,zero      - keyword value
C
C CALLED ROUTINES:
C      subroutine ftgkyd - get double keyword value
C      subroutine ftkeyn - build a keyword
C
C******************************************************************************
      subroutine figmsf(scaling,tcols,iunit,colpos,ounit)
      integer maxcl
      parameter (maxcl=999)
      logical scaling(maxcl)
      integer tcols, iunit, colpos(maxcl), ounit

      character(8) keyroot, keyword
      character(70) comment
      integer i, ftstatus
      double precision scale, zero

      ftstatus = 0
      scale = 1.0
      zero = 0.0

C  check for scaled data
      do 10 i = 1, tcols
         if (scaling(i)) then
            keyroot = 'TSCAL'
            call ftkeyn(keyroot,colpos(i),keyword,ftstatus)
            call ftgkyd(iunit,keyword,scale,comment,ftstatus)
            if (ftstatus .eq. 0) then
            call ftkeyn(keyroot,i,keyword,ftstatus)
            call ftpkyd(ounit,keyword,scale,14,comment,ftstatus)
            endif
            ftstatus = 0
            keyroot = 'TZERO'
            call ftkeyn(keyroot,colpos(i),keyword,ftstatus)
            call ftgkyd(iunit,keyword,zero,comment,ftstatus)
            if (ftstatus .eq. 0) then
            call ftkeyn(keyroot,i,keyword,ftstatus)
            call ftpkyd(ounit,keyword,zero,14,comment,ftstatus)
            endif
            ftstatus = 0
            call fttscl (ounit,colpos(i),scale,zero,ftstatus)
         endif
 10   continue

      call ftcmsg
      return
      end



C******************************************************************************
C SUBROUTINE:
C      figman
C
C DESCRIPTION:
C      gets the tnull header keyword for a merged ASCII extension
C
C AUTHOR/DATE:
C      Janice Tarrant  3/2/92
C
C MODIFICATION HISTORY:
C       20 August, 1992 - changed so only takes 1st input file into
C                         account (EAG)
C       27 August, 1992 - changed to be call with each input file.  If
C                         null value is not defined, the null from the
C                         current input file is used.  This will destroy
C                         any previous data that has this value. (EAG)
C       12/10/1996 Jeff Guerber - Improved error reporting, added args
C            colist and filename.
C       3/31/1997 Jeff Guerber -  G77 doesn't like "call fcecho('...'
C            //filename)"
C
C NOTES:
C
C USAGE:
C      call figman(iunit1,colpos1,tcols,ounit, callno, colist, filename)
C
C ARGUMENTS:
C      iunit1  - first input file logical unit number
C      colpos1 - first extension column position
C      tcols   - total number of columns in merged extension
C      ounit   - merged file logical unit number
C      callno  - number of calls to this routine
C      colist  - list of column names
C      filename- name of current input file
C
C PRIMARY LOCAL VARIABLES:
C      keyroot  - root string for keyword name
C      keyword  - name of a keyword
C      comment  - keyword comment field
C      ftstatus - FITS error status
C      null1/2  - first extension null keyword value
C      nullval  - merged file null value
C      nullcol  - column position of null value
C
C CALLED ROUTINES:
C      subroutine ftgkys - get string keyword value
C      subroutine ftkeyn - build a keyword
C      subroutine ftpkys - put string keyword value
C      subroutine ftsnul - define string for undefined pixels
C
C******************************************************************************
      subroutine figman(iunit1,colpos1,tcols,ounit,callno,
     &    colist,filename)
      integer maxcl
      parameter (maxcl=999)
      integer iunit1, colpos1(maxcl), tcols, ounit, callno
      character*(*) colist(maxcl), filename

      character(8) keyroot, keyword, nullval(maxcl)
      character(70) comment, null1

      integer i, nullcol(maxcl), status1, status2, ftstatus
      character(80) context

      save nullcol, nullval


C if this is the first call, initialize nullcol
      if (callno .le. 1) then
         do 1 i = 1, maxcl
            nullcol(i) = 0
 1       continue
      endif

C  check for null values
      status1 = 0
      status2 = 0
      keyroot = 'TNULL'
      do 10 i = 1, tcols
         call ftkeyn(keyroot,colpos1(i),keyword,status1)
         call ftgkys(iunit1,keyword,null1,comment,status1)
         if (status1 .eq. 0) then

C check if null value is already defined for this column
            if (nullcol(i) .ne. i) then
               if (callno .eq. 2) then
                  context = ' WARNING: NULL found in 2nd or later file:'
     &                 // filename
                  call fcecho (context)
                  context = ' NULL value ' // null1 //
     &                      ' assigned in column ' // colist(i)
                  call fcecho (context)
               endif

               nullcol(i) = i
               nullval(i) = null1
	       ftstatus = 0
               call ftkeyn(keyroot,i,keyword,ftstatus)
               call ftpkys(ounit,keyword,nullval(i),comment,
     &              ftstatus)
               call ftsnul(ounit,nullcol(i),nullval(i),ftstatus)
            else
C warning if NULLS are different
               if (nullval(i) .ne. null1) then
                  context = ' WARNING: different nulls defined ' //
     &                      colist(i)
                  call fcecho (context)
c                 context = ' in file ' // filename
                  call fcecho(context)
                  context = ' NULL values translated from ' // null1 //
     &                      ' to ' // nullval(i)
                  call fcecho (context)
               endif
            endif
         endif
         status1 = 0
         status2 = 0
 10   continue

      return
      end



C******************************************************************************
C SUBROUTINE:
C      figmbn
C
C DESCRIPTION:
C      gets the tnull header keyword for a merged binary extension
C
C AUTHOR/DATE:
C      Janice Tarrant  3/4/92
C
C MODIFICATION HISTORY:
C       20 August, 1992 - changed so only takes 1st input file into
C                         account (EAG)
C       27 August, 1992 - changed to be call with each input file.  If
C                         null value is not defined, the null from the
C                         current input file is used.  This will destroy
C                         any previous data that has this value. (EAG)
C       12/10/1996 Jeff Guerber - Improved error reporting, added args
C            colist and filename.
C       3/31/1997 Jeff Guerber -  G77 doesn't like "call fcecho('...'
C            //filename)"
C
C NOTES:
C
C USAGE:
C      call figmbn(iunit1,colpos1,tcols,ounit, callno, colist, filename)
C
C ARGUMENTS:
C      iunit1  - first input file logical unit number
C      colpos1 - first extension column position
C      tcols   - total number of columns in merged extension
C      ounit   - merged file logical unit number
C      callno  - number of times the routine has been called
C      colist  - list of column names
C      filename- name of current input file
C
C PRIMARY LOCAL VARIABLES:
C      keyroot  - root string for keyword name
C      keyword  - name of a keyword
C      comment  - keyword comment field
C      ftstatus - FITS error status
C      null1/2  - first extension keyword value
C      nullval  - merged file null value
C      nullcol  - column position of null value
C
C CALLED ROUTINES:
C      subroutine ftgkyj - get integer keyword value
C      subroutine ftkeyn - build a keyword
C      subroutine ftpkyj - put integer keyword value
C      subroutine fttnul - define integer for undefined pixels
C
C******************************************************************************
      subroutine figmbn(iunit1,colpos1,tcols,ounit, callno,
     &    colist, filename)
      integer maxcl
      parameter (maxcl=999)
      integer iunit1, colpos1(maxcl), tcols, ounit, callno
      character*(*)  colist(maxcl), filename

      character(8) keyroot, keyword
      character(70) comment
      character(80) context
      integer i, null1, nullval(maxcl), nullcol(maxcl),
     &     status1, status2, ftstatus

      save nullcol, nullval

C if this is the first call, initialize nullcol
      if (callno .le. 1) then
         do 1 i = 1, maxcl
            nullcol(i) = 0
 1       continue
      endif

C  check for null values
      status1 = 0
      status2 = 0
      keyroot = 'TNULL'
      do 10 i = 1, tcols
         call ftkeyn(keyroot,colpos1(i),keyword,status1)
         call ftgkyj(iunit1,keyword,null1,comment,status1)
         if (status1 .eq. 0) then

C check if null value is already defined for this column
            if (nullcol(i) .ne. i) then
               if (callno .eq. 2) then
                  context = ' WARNING: NULL found in 2nd or later file:'
     &                 // filename
                  call fcecho (context)
                  write (context,1000) null1, colist(i)
 1000             format (' NULL value ',i6,' assigned in column ',a)
                  call fcecho (context)
               endif

               nullcol(i) = i
               nullval(i) = null1
	       ftstatus = 0
               call ftkeyn(keyroot,i,keyword,ftstatus)
               call ftpkyj(ounit,keyword,nullval(i),comment,
     &              ftstatus)
               call fttnul(ounit,nullcol(i),nullval(i),ftstatus)
            else
C warning if NULLS are different
               if (nullval(i) .ne. null1) then
                  write (context, 1001) colist(i)
 1001             format (' WARNING: different nulls defined ',
     &                 'for column ',a)
                  call fcecho (context)
                  context = ' in file ' // filename
                  call fcecho(context)
                  write (context, 1002) null1, nullval(i)
 1002             format (' NULL values translated from ',I6,' to ',I6)
                  call fcecho (context)
               endif
            endif
         endif
         status1 = 0
         status2 = 0
 10   continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C      fimgcl
C
C DESCRIPTION:
C      merges values from one extension's columns
C
C AUTHOR/DATE:
C      Janice Tarrant  2/10/92
C
C MODIFICATION HISTORY:
C      Tarrant  4/17/92  added vector elements
C       20 August, 1992 - changed to write one file's columns at a time (EAG)
C      Jeff Guerber 12/10/1996 - Improved error reporting, adding arg colist
C      Peter Wilson  3/24/1998 - repeat->repeat/8 if a bit column
C
C NOTES:
C
C USAGE:
C      call fimgcl(iunit1,htype,tform,colpos1,colist,nrows1,
C                  nrows2,newscale,ncols,ounit,mgstatus)
C
C ARGUMENTS:
C      iunit1   - input logical unit number
C      htype    - header type
C      tform    - extension column format
C      colpos1  - column position
C      colist   - array of column names
C      nrows1   - total number of rows written so far
C      nrows2   - current extension number of rows
C      newscale - column newscale flag
C      ncols    - number of columns
C      ounit    - output file logical unit number
C      mgstatus - returned status
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      dtype   - symbolic code of the column data type
C      repeat  - length of element vector
C      width   - width of character string field
C      ifrow   - input beginning row number
C      ofrow   - output beginning row number
C      ftstatus - fitsio error number
C
C CALLED ROUTINES:
C      subroutine fcasfm - get ascii format
C      subroutine figdpd - get data from an extension, put it in another
C      subroutine ftbnfm - get binary format
C
C******************************************************************************
      subroutine fimgcl(iunit1, htype, tform, colpos1, colist,
     &     nrows1, nrows2, newscale, ncols, ounit, mgstatus)
      implicit none
      integer maxcl
      parameter (maxcl = 999)
      character*(*) tform(maxcl), colist(maxcl)
      logical newscale(maxcl)
      integer iunit1, htype, colpos1(maxcl),
     &     nrows1, nrows2, ncols, ounit, ftstatus, mgstatus

      character(80) context
      integer i, dtype, repeat, width, ifrow, ofrow

      mgstatus = 0
      ftstatus = 0
C  get one column at a time
      do 10 i = 1, ncols
         if (htype .eq. 1) then
            call fcasfm(tform(colpos1(i)),dtype,ftstatus)
            repeat = 1
         else
            call ftbnfm(tform(colpos1(i)),dtype,repeat,
     &           width,ftstatus)
C if this is a string, fix up the repeat
            if (abs(dtype) .eq. 16) repeat = repeat/width
C if this is a bit column, fix up the repeat
            if (abs(dtype) .eq. 1) repeat = (repeat+7)/8
         endif
C  get data and put it in the merged extension; on error continue w/next col.
         ifrow = 1
         ofrow = nrows1
         call figdpd(ifrow,nrows2,dtype,repeat,iunit1,colpos1(i),
     &        newscale(i),ounit,i,ofrow,ftstatus)

         if (ftstatus .ne. 0) then
            context = 'Problem merging column: ' // colist(i)
            call fcerr(context)
            call ftgerr(ftstatus,context)
            call fcerr(context)
            mgstatus = 1
            ftstatus = 0
         endif

 10   continue
      nrows1 = nrows1 + nrows2

 999  continue
      return
      end

C******************************************************************************
C SUBROUTINE:
C      fmgfil
C
C DESCRIPTION:
C      finds the total number of rows in the input files
C
C AUTHOR/DATE:
C      Emily A. Greene   2/4/93
C
C MODIFICATION HISTORY:
C     Jeff Guerber 12/11/1996: Better error msgs as in fimgex; let fimgex
C         handle calling fcerrm.  Does this have to repeat so much?
C
C NOTES:
C
C USAGE:
C      call fmgfil (iunit, filelist, numfiles, mrows, mcount, status)
C
C ARGUMENTS:
C       iunit     - input unit number
C       filellist - list of input files
C       numfiles  - total number of input files
C       mrows     - total number of rows in all files
C       mcount    - total length of the variable length data area
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C
C CALLED ROUTINES:
C       subroutine ftxxxx - fitsio routines
C******************************************************************************
      subroutine fmgfil (iunit, filelist, numfiles, mrows, mcount,
     &     status)

      integer iunit, numfiles, mrows, status, mcount
      character*(*) filelist(numfiles)

      integer i, block, extnumb, nrows, htype, pcount, clstatus
      integer fcstln
      character(8) key1, key2
      character(256) filename
      character(512) context

C initialize
      mrows = 0
      mcount = 0
      key1 = 'NAXIS2'
      key2 = 'PCOUNT'

C loop over all the input files
      do 100 i = 1, numfiles
         call fcpars (filelist(i), filename, extnumb, status)

C EAG 8/25/93 default to 1st extension
         if (extnumb .eq. -99) extnumb = 1

         if (extnumb .eq. 0) then
            context = ' Primary Extension Not Supported, file '
     &           // filelist(i)
            call fcerr (context)
            goto 999
         endif

         call ftopen (iunit, filename, 0, block, status)
         if (status .ne. 0) then
            write (context, 1000) filename
 1000       format (' Unable to open ',a256)
            call fcerr (context)
            goto 999
         endif

         call ftmrhd (iunit, extnumb, htype, status)
         if (status .ne. 0) then
             write(context,'(a,i4,a,a)')
     &       'Error moving to extension number', extnumb, ' in file ',
     &       filename(:MIN(fcstln(filename),len(context)-45))
            call fcerr (context)
            goto 998
         endif

         call ftgkyj (iunit, key1, nrows, context, status)
         if (status .ne. 0) then
            context = 'Unable to find NAXIS2 keyword, file ' //
     &           filelist(i)
            call fcerr (context)
            goto 998
         endif
         mrows = mrows + nrows

         call ftgkyj (iunit, key2, pcount, context, status)
         if (status .ne. 0) then
            context = 'Unable to find PCOUNT keyword, file ' //
     &           filelist(i)
            call fcerr (context)
            goto 998
         endif
         mcount = mcount + pcount

         call ftclos (iunit, status)
 100  continue

      return

  998 clstatus = 0
      call ftclos(iunit, clstatus)
  999 return
      end

C******************************************************************************
C SUBROUTINE:
C      figdpd
C
C DESCRIPTION:
C      gets data from an extension and puts it into a new extension
C
C AUTHOR/DATE:
C      Janice Tarrant  2/10/92
C
C MODIFICATION HISTORY:
C      Tarrant  4/17/92  added vector elements
C       Greene  2/4/93   added variable length arrays
C          PDW  3/24/98  bit column support added
C
C NOTES:
C
C USAGE:
C      call figdpd(frow,nrows,dtype,repeat,iunit,colpos,newscale,ounit,
C                  colnum,ofrow,ftstatus)
C
C ARGUMENTS:
C      ifrow    - input beginning row number
C      nrows    - number of rows
C      dtype    - symbolic code of the column data type
C      repeat   - length of element vector
C      iunit    - input file logical unit number
C      colpos   - input extension column position
C      newscale - column new scale flag
C      ounit    - output file logical unit number
C      colnum   - output extension column number
C      ofrow    - output beginning row number
C      ftstatus - fitsio error code
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      felem    - first pixel of element vector
C      nelem    - number of data elements to read or write
C      flagvals - data element undefined flag
C      anyf     - data values undefined flag
C      bvalues  - array of byte values
C      lvalues  - array of logical values
C      svalues  - array of string values
C      ivalues  - array of integer*2 values
C      jvalues  - array of integer values
C      evalues  - array of real values
C      dvalues  - array of double values
C      cvalues  - array of complex values
C      mvalues  - array of double complex values
C
C CALLED ROUTINES:
C      subroutine ftgcfx - get x type column values
C      subroutine ftpclx - put x type column values
C      subroutine ftpclu - put undefined column values
C      subroutine fttscl - reset scaling parameters for a column
C
C******************************************************************************
      subroutine figdpd(ifrow,nrows,dtype,repeat,iunit,colpos,
     &     newscale,ounit,colnum,ofrow,ftstatus)
      logical newscale
      integer ifrow,nrows,dtype,repeat,iunit,colpos,ounit,colnum,
     &     ofrow,ftstatus

      integer maxsize
      parameter (maxsize = 1024)
      character(1) bvalues(maxsize)
      character(1024) svalues(maxsize)
      character(80) context
      logical lvalues(maxsize), flagvals(maxsize), anyf
      integer*2 ivalues(maxsize)
      integer i, felem, remain, nelem, jvalues(maxsize), offset
      real evalues(maxsize), cvalues(2,maxsize)
      double precision dvalues(maxsize), mvalues(2,maxsize)
      integer flagofrow, flagelem

      nelem = 0
      felem = 1
      remain = nrows * repeat

C if variable length array
      if (dtype .lt. 0) remain = nrows

C if dtype <0, then we have a variable length array and must go one
C row at a time.  Find the nelem for this row.
      if (dtype .lt. 0) then
         call ftgdes (iunit, colpos, ifrow, remain,
     &        offset, ftstatus)
         repeat = remain
         if ( dtype.eq.-1 ) repeat = (repeat+7)/8
      endif

 10   if (remain .ge. maxsize) then
         nelem = maxsize
      else
         nelem = remain
      end if
      anyf = .false.

C  data type is bits or byte
C  if new column has no scale, but original is scaled, read in scaled data
C  as real exponential floating point
      if (abs(dtype) .eq. 11 .or. abs(dtype).eq.1) then
         if (newscale) then
            call ftgcfe(iunit,colpos,ifrow,felem,nelem,evalues,
     &           flagvals,anyf,ftstatus)
            call ftpcle(ounit,colnum,ofrow,felem,nelem,evalues,
     &           ftstatus)
         else
            call ftgcfb(iunit,colpos,ifrow,felem,nelem,bvalues,
     &           flagvals,anyf,ftstatus)
            call ftpclb(ounit,colnum,ofrow,felem,nelem,bvalues,
     &           ftstatus)
         endif
C  data type is logical
      else if (abs(dtype) .eq. 14) then
         call ftgcfl(iunit,colpos,ifrow,felem,nelem,lvalues,
     &        flagvals,anyf,ftstatus)
         call ftpcll(ounit,colnum,ofrow,felem,nelem,lvalues,ftstatus)
C  data type is ASCII characters
      else if (abs(dtype) .eq. 16) then
         call ftgcfs(iunit,colpos,ifrow,felem,nelem,svalues,
     &        flagvals,anyf,ftstatus)
         call ftpcls(ounit,colnum,ofrow,felem,nelem,svalues,ftstatus)
C  data type is short integer
C  if new column has no scale, but original is scaled, read in scaled data
C  as real exponential floating point
      else if (abs(dtype) .eq. 21) then
         if (newscale) then
            call ftgcfe(iunit,colpos,ifrow,felem,nelem,evalues,
     &           flagvals,anyf,ftstatus)
            call ftpcle(ounit,colnum,ofrow,felem,nelem,evalues,
     &           ftstatus)
         else
            call ftgcfi(iunit,colpos,ifrow,felem,nelem,ivalues,
     &           flagvals,anyf,ftstatus)
            call ftpcli(ounit,colnum,ofrow,felem,nelem,ivalues,
     &           ftstatus)
         endif
C  data type is integer
C  if new column has no scale, but original is scaled, read in scaled data
C  as real exponential floating point
      else if (abs(dtype) .eq. 41) then
         if (newscale) then
            call ftgcfd(iunit,colpos,ifrow,felem,nelem,dvalues,
     &           flagvals,anyf,ftstatus)
            call ftpcld(ounit,colnum,ofrow,felem,nelem,dvalues,
     &           ftstatus)
         else
            call ftgcfj(iunit,colpos,ifrow,felem,nelem,jvalues,
     &           flagvals,anyf,ftstatus)
            call ftpclj(ounit,colnum,ofrow,felem,nelem,jvalues,
     &           ftstatus)
         endif
C  data type is real
      else if (abs(dtype) .eq. 42) then
         call ftgcfe(iunit,colpos,ifrow,felem,nelem,evalues,
     &        flagvals,anyf,ftstatus)
         call ftpcle(ounit,colnum,ofrow,felem,nelem,evalues,ftstatus)
C  data type is double precision
      else if (abs(dtype) .eq. 82) then
         call ftgcfd(iunit,colpos,ifrow,felem,nelem,dvalues,
     &        flagvals,anyf,ftstatus)
         call ftpcld(ounit,colnum,ofrow,felem,nelem,dvalues,ftstatus)
C  data type is complex
      else if (abs(dtype) .eq. 83) then
         call ftgcfc(iunit,colpos,ifrow,felem,nelem,cvalues,
     &        flagvals,anyf,ftstatus)
         call ftpclc(ounit,colnum,ofrow,felem,nelem,cvalues,ftstatus)
C  data type is double complex
      else if (abs(dtype) .eq. 163) then
         call ftgcfm(iunit,colpos,ifrow,felem,nelem,mvalues,
     &        flagvals,anyf,ftstatus)
         call ftpclm(ounit,colnum,ofrow,felem,nelem,mvalues,ftstatus)
      end if

C  set any undefined values
      flagofrow = ofrow
      flagelem = felem
      if (anyf) then
         do 20 i = 1, nelem
            if (flagelem .gt. repeat) then
                flagelem = flagelem - ((flagelem-1)/repeat)*repeat
                if (flagelem .eq. 1) then
                   flagofrow = flagofrow + 1
                end if
            end if
            if (flagvals(i)) then
                call ftpclu(ounit,colnum,flagofrow,flagelem,1,ftstatus)
            end if
            flagelem = flagelem + 1
 20      continue
      end if

C  update the pointers in the column
      if (dtype .gt. 0) then
         remain = remain - nelem
         felem = felem + nelem
         if (felem .gt. repeat) then
            ifrow = ifrow + (felem-1)/repeat
            ofrow = ofrow + (felem-1)/repeat
            felem = felem - ((felem-1)/repeat)*repeat
         endif
      else
         remain = remain - nelem
         felem = felem + nelem
C if dtype <0, then we have a variable length array and must go one
C row at a time.  Find the nelem for this row.
         if (repeat .gt. felem) then
            ifrow = ifrow + 1
            ofrow = ofrow + 1
            felem = 1
            call ftgdes (iunit, colpos, ifrow, remain, offset, ftstatus)
            repeat = remain
         endif
      endif
      if (remain .gt. 0) goto 10

      if (ftstatus .ne. 0) then
         context = 'error in column merge'
         call fcerr(context)
         call fcerrm(ftstatus)
      endif

      return
      end

C******************************************************************************
