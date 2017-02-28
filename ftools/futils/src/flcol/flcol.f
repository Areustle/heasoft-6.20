C*****************************************************************************
C SELECTOR TASK:
C      flcol
C
C FILE:
C      flcol.f
C
C DESCRIPTION:
C       display columns found in a fits file's binary table extension
C
C AUTHOR/DATE:
C       James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C      1/7/91 JKB Added testing of extension number returned by fcpars
C      7/20/92 WDP Added option to print all extensions if user specified
C                  extension number = *
C      8/25/94 EAG 3.0a - add clobber, clear fitsio stack
C      11/4/94 EAG 3.2a - add dimensionality of column
C      12/5/94 EAG 3.3a - make showing dimensionality optional
C      10/15/97 PDW 3.3b- Replace old get header routines
C      5/21/98 NG 3.3c- Replace ftmrhd(...,nmove,...) with ftmahd(..,nmove+1,..) 
C
C NOTES:
C
C MAKE:
C      HOST: make -f mkhflcol
C      IRAF: xc -c xflcol.x flcol.f
C            xc -p stsdas xflcol.o flcol.o -lmisc -lfitsio -liraf77 -o flcol.e
C
C USAGE:
C      HOST: hflcol
C      IRAF: flcol
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file and extension number
C      nlist   - verbose value (1-3)
C      showdims - if true, print out dimensions of column
C
C CALLED ROUTINES:
C      subroutine glcol - gets parameters from environment
C      subroutine figcln - gets column names from FITS file
C
C*****************************************************************************

      subroutine flcol
      character(160) infile,outfile
      integer    nlist
      logical    showdims
      character(40) taskname
      common /task/ taskname

      taskname = 'flcol3.3c'
      call ftcmsg

C get parameters from par file
      call glcol(infile,nlist,outfile, showdims)

C read in and display column names
      call figcln(infile,nlist,outfile, showdims)
      return
      end


C*****************************************************************************
C SUBROUTINE:
C      glcol
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
C       glcol uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call glcol(infile,nlist,outfile,showdims)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      nlist   - verbose value
C      outfile - output ASCII file containing column names
C      showdims - whether to print out dimensions of columns
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      nlist   - verbose value
C      outfile - output ASCII file containing column names
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

      subroutine glcol(infile,nlist,outfile, showdims)

      character*(*) infile,outfile
      integer      nlist
      logical      showdims
      character(80) context
      integer      irafsts

C ---   Initialize variables
      irafsts = 0

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the value of the NLIST (verbose) variable

      call uclgsi('nlist',nlist,irafsts)
      if ( irafsts .ne. 0 ) then
         context = 'Could not get NLIST parameter'
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

C Get whether to output dimensionality of column

      call uclgsb('showdims', showdims, irafsts)
      if ( irafsts .ne. 0) then
         context = ' Could not get SHOWDIMS parameter'
         call fcerr (context)
         goto 999
      endif

 999  continue
      return
      end


C*****************************************************************************
C SUBROUTINE:
C      figcln
C
C DESCRIPTION:
C      This subroutine actually does the reading of column names
C      from the input FITS file and copys them to the OUTFILE based on
C      the values of the parameters passed along from whatever source.
C
C AUTHOR/DATE:
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C      1/7/92 JKB Testing of extensions added
C      7/6/92 WDP changed parameter names to lowercase and simplified some code
C     11/30/1995 JRG 3.3b - changed ttype to char*40; note: format statements
C         1002, 1003, 1004 only use A25 for ttype field.
C
C NOTES:
C
C
C USAGE:
C      call figcln(infile,nlist,outfile,showdims)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      nlist   - verbose value
C      outfile - output ASCII file containing column names
C      showdims - whether to show output dimensions
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      nlist    - verbose value
C      outfile  - output ASCII file containing column names
C      extnumb  - extension number
C      filename - input FITS filename
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcpars - parse infile into a filename and extension
C      subroutine fcecho - echo message to terminal
C      subroutine fishcl - display or write column names
C      the FITSIO library - all subroutines beginning with ftxxxx
C
C*****************************************************************************

      subroutine figcln(infile,nlist,outfile, showdims)

      character*(*) infile,outfile
      integer    nlist,status
      logical    showdims
      integer    iunit,ounit,extnumb,maxcl
      parameter ( maxcl = 999 )
      integer    block,nmove,hdutype
      character(160) filename
      character(80) context
      integer    nrows,rowlen
      character(40) ttype(maxcl)
      character(16) tform(maxcl), tdim(maxcl)
      character(25) tunit(maxcl)
      character(70) extname
      integer    tbcol(maxcl),tfields, nfound, i
      integer    varidat
      logical    allext

C Initialize variables
      iunit = 15
      status = 0
      allext=.false.

c     Removed specific continuation numbering so that we
c     could reuse it before each extension. April 3rd, 1997
      do i = 1, maxcl
        tdim(i) = ' '
      enddo
       
C ---   Parse the INFILE into a filename and extension number ---
      call fcpars(infile,filename,extnumb,status)

C ---   If the extension is 0 then give error and exit
      if ( extnumb .eq. 0 ) then
         context = 'Primary array (Ext# = 0) is not a table'
         call fcerr(context)
         goto 1000
      else if (extnumb .lt. 0)then

C EAG 8/25/93 default to all extensions

         allext=.true.
         extnumb=1
      endif

C ---   Open Existing input FITS file ---
      call ftopen(iunit,filename,0,block,status)
      if ( status .ne. 0 ) then
         context = ' Unable to open INFILE'
         call fcerr(context)
         goto 1000
      endif

      if ( outfile .ne. 'STDOUT' ) then
         ounit = 16
         call faopen (ounit, outfile, 2, 0, status)
         if (status .ne. 0) then
 30         context = 'Could not open outfile; may exist? ' // outfile
            call fcerr(context)
            goto 999
         endif
      endif


      nmove = extnumb
10    continue

c     This modification was made by Brian Elza to fix a problem with
c     column size being left over from earlier extensions. This simply
c     reinitializes the tdim array. April 3rd, 1997
      do i = 1, maxcl
        tdim(i) = ' '
      enddo
       
C ---   Move to the extension to be read ---
      call ftmahd(iunit,nmove+1,hdutype,status)
C ---   If status not zero then extension does not exist so exit
      if ( status .ne. 0 ) then
         if (.not. allext)then
            context = 'Unable to move to extension #'
            call fcerr(context)
         endif
         goto 999
      endif
      nmove=nmove+1

C ---   If this is an ASCII extension then... ---
      if ( hdutype .eq. 1 ) then
         call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,status)

C --- If this is a BINARY extension then... ---
      else if ( hdutype .eq. 2 ) then
         call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,status)

      else
         context = 'Extension is Not Ascii or Binary table'
         call fcecho(context)
         if (allext)then
            go to 10
         else
	    goto 999
         end if
      end if

C get the dimensionality of column, if any, as a string
      if (showdims)
     &     call ftgkns (iunit, 'TDIM', 1, tfields, tdim, nfound, status)

      call fishcl(outfile,tfields,ttype,tform,tdim,tunit,
     &     nlist,showdims,status)

C --- Read next extension, if asked to list all extensions
      if (allext)then
         go to 10
      end if

 999  continue
      if ( outfile .ne. 'STDOUT' ) then
         close(ounit)
      endif

      if (status .ne. 0) then
         if (status .ne. 107) call fcerrm (status)
         status = 0
      endif

      call ftclos(iunit,status)
 1000 continue
      return
      end


C*****************************************************************************
C SUBROUTINE:
C      fishcl
C
C DESCRIPTION:
C       Write out a list of column names found in the FITS extension
C       to the terminal and a file if desired
C
C AUTHOR/DATE:
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C      1/7/92 JKB Now exits when OUTFILE already exists
C
C NOTES:
C
C
C USAGE:
C      call fishcl(outfile,tfields,ttype,tform,tdim,tunit,
C    &             nlist,showdims, status)
C
C ARGUMENTS:
C      outfile - input FITS file and extension number
C      tfields - input FITS file and extension number
C      ttype   - input FITS file and extension number
C      tform   - input FITS file and extension number
C      tdim    - input dimensionality of column, if any as string
C      tunit   - input FITS file and extension number
C      nlist   - input FITS file and extension number
C      showdims - whether to output dimension of column
C      status  - input FITS file and extension number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************
      subroutine fishcl(outfile,tfields,ttype,tform,tdim,tunit,
     &     nlist,showdims,status)

      character*(*) outfile
      integer       tfields,nlist,status,maxcl
      logical       showdims
      parameter ( maxcl = 999 )
      character*(*) ttype(maxcl),tform(maxcl),tunit(maxcl),tdim(maxcl)
      integer       ounit,pages,stub,i,j,k
      character(70)  heading,context

      if ( status .ne. 0 ) goto 999
      ounit = 16
      pages = tfields / 22
      stub = tfields - pages * 22
      if ( stub .gt. 0 ) pages = pages + 1
      k = 1
      if ( nlist .eq. 1 ) then
         heading = '_____Column_Names_____'
      endif
      if ( nlist .eq. 2 ) then
         if (showdims) then
            heading = '___Column_Names_________Formats______Dims____'
         else
            heading = '___Column_Names_________Formats___'
         endif
      endif
      if ( nlist .eq. 3 ) then
         if (showdims) then
            heading =
     &        '___Column_Names_________Formats______Dims______Units___'
         else
            heading =
     &           '___Column_Names_________Formats__________Units___'
         endif
      endif
      do 20 i = 1, pages
         if ( outfile .eq. 'STDOUT' ) then
	    call fcecho(heading)
         endif
         do 10 j = 1, 22
	    if ( k .le. tfields ) then
               if ( nlist .eq. 1 ) then
                  write(context,1001) ttype(k)
                  if ( outfile .eq. 'STDOUT' ) then
                     call fcecho(context)
                  else
                     write(ounit,1000) context
                  endif
               endif
               if ( nlist .eq. 2 ) then
                  if (showdims) then
                     write(context,1002) ttype(k),tform(k),tdim(k)
                  else
                     write(context,1002) ttype(k),tform(k)
                  endif
                  if ( outfile .eq. 'STDOUT' ) then
                     call fcecho(context)
                  else
                     write(ounit,1000) context
                  endif
               endif
               if ( nlist .eq. 3 ) then
                  if (showdims) then
                     write(context,1003)
     &                    ttype(k),tform(k),tdim(k),tunit(k)
                  else
                     write(context,1004) ttype(k),tform(k),tunit(k)
                  endif
                  if ( outfile .eq. 'STDOUT' ) then
                     call fcecho(context)
                  else
                     write(ounit,1000) context
                  endif
               endif
               k = k + 1
	    endif
 10      continue
         if ( outfile .eq. 'STDOUT' ) then
            call fcecho(' ')
         else
            write(ounit,1000)'    '
         endif
 20   continue

 999  continue
 1000 format(A)
 1001 format(A)
 1002 format(A25,2X,A10,A10)
 1003 format(A25,2X,A10,A10,A20)
 1004 format(A25,2X,A10,2X,A20)
      return
      end

