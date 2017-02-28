C******************************************************************************
C SELECTOR TASK:
C      fstruct
C
C FILE:
C      fstruct.f
C
C DESCRIPTION:
C       display information about a fits file's structure or a specific
C       extension
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C       Based on the program FLCOL by
C       James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C       as FLCOL:
C      1/7/91 JKB Added testing of extension number returned by fcpars
C      7/20/92 WDP Added option to print all extensions if user specified
C                  extension number = *
C       as FSTRUCT:
C       10/13/92 EAG Added hidden parameter output
C       1/27/93  EAG Increased size of PCOUNT output
C       9/12/94 EAG 3.0a - use FAOPEN, fix writing to par file problem
C       11/2/94 EAG 3.2a - end on status < 200, 210, 252, or 301
C       11/30/1995 JRG 3.2b - fgstru: Fixed TTYPE.
C       5/21/1998 NG 3.2c- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..)	
C       03/12/99 toliver 4.0 - added flcol-like capability to display column
C                              information (we've come full circle :-)
C       06/21/00 ning    4.1 - added output parameters of totalhdu and
C                              hdunum.
C
C
C NOTES:
C
C USAGE:
C      HOST: fstruct
C      IRAF: fstruct
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output FITS file and extension number
C
C CALLED ROUTINES:
C      subroutine gstruct - gets parameters from environment
C
C******************************************************************************

      subroutine fstrut

      logical colinfo
      character(160) infile,outfile
      character(40) taskname
      common /task/ taskname

      taskname = 'fstruct4.1'

      call ftcmsg

C get parameters from par file
      call gstrut(infile, outfile, colinfo)

C get the information on the structure of the file and output
      call fgstru (infile, outfile, colinfo)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gstrut
C
C DESCRIPTION:
C      gets parameters from the parameter file
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C       based on the program GLCOL by
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C       03/12/99 toliver - added column information display parameter
C
C NOTES:
C       gstruct uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gstrut(infile, outfile)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing structure information
C      colinfo - column information display flag
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing structure information
C      context - error message
C      status - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsb - get logical parameter
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************

      subroutine gstrut(infile, outfile, colinfo)


      character*(*) infile,outfile
      logical colinfo
      character(80) context
      integer      status

C ---   Initialize variables
      status = 0

C ---   Get the name of the input FITS file ---

      call uclgst('infile',infile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C ---   Get the name of the output file ---

      call uclgst('outfile',outfile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C
C     Get column information display flag
C
      CALL uclgsb ('colinfo', colinfo, status)
      IF (status .NE. 0) THEN
         context = ' Could not get COLINFO parameter'
         CALL fcerr (context)
         GOTO 999
      ENDIF

 999  continue
      return
      end

C******************************************************************************
C SUBROUTINE:
C      fgstru
C
C DESCRIPTION:
C      This subroutine actually does the reading of the structure of
C      the input FITS file and copys them to the OUTFILE based on
C      the values of the parameters passed along from whatever source.
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C       based on the program FIGCLN
C      James Kent Blackburn 12/5/91
C
C MODIFICATION HISTORY:
C       as FIGCLN:
C      1/7/92 JKB Testing of extensions added
C      7/6/92 WDP changed parameter names to lowercase and simplified some code
C       as FGSTRU:
C       10/13/92 EAG  Added hidden parameter output
C     11/30/1995 Jeff Guerber - Changed TTYPE to char*40.
C       03/12/99 toliver - added code to display column information for
C                          ASCII and binary tables
C
C NOTES:
C
C
C USAGE:
C      call fgstru(infile, outfile)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output ASCII file containing structure information
C      colinfo - column information display flag
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file containing structure information
C      extnumb  - extension number
C      filename - input FITS filename
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcpars - parse infile into a filename and extension
C      subroutine fcecho - echo message to terminal
C      function fcstln   - index of last non-blank character
C      the FITSIO library - all subroutines beginning with ftxxxx
C
C******************************************************************************

      subroutine fgstru(infile, outfile, colinfo)

      character*(*) infile,outfile
      logical colinfo

      integer    status, kstat
      integer    iunit, ounit, extnumb, nmove, maxcl, maxdim
      parameter ( maxcl = 999 )
      parameter ( maxdim = 20 )
      integer    block, hdutype, count
      character(160) filename
      character(80) context, comment
      integer*8    nrows, varidat
      character(40) ttype(maxcl)
      character(16) tform(maxcl)
      character(25) tunit(maxcl)
      character(70) extname, dims, xtension
      integer    tbcol(maxcl),tfields
      integer  naxis1, naxis2, naxis3, naxis4
      integer    rowlen, bitpix, naxis, naxes(99), pcount, gcount
      logical    extend, simple, ifout, isfits
      character(16) tdim(maxcl)
      integer    nfound
      double precision dlbounds(maxcl), dubounds(maxcl)
      integer hdunum, totalhdu

C Initialize variables
      iunit = 15
      status = 0
      kstat = 0
      ifout = .false.
      isfits = .true.
      if ( outfile .ne. 'STDOUT' ) ifout = .true.

C ---   Parse the INFILE into a filename and extension number ---
      call fcpars(infile,filename,extnumb,status)

C EAG 8/25/93 default to all extensions
      if (extnumb .eq. -99) extnumb = -1

C ---   Open Existing input FITS file ---
      call ftopen(iunit,filename,0,block,status)
      if (status .eq. 210) then
         isfits = .false.
         status = 0
         call uclpsb ('isfits', isfits, status)
         context = ' Requested file not a FITS file'
         call fcerr (context)
         goto 1000
      else if ( status .ne. 0 ) then
         isfits = .false.
         status = 0
         call uclpsb ('isfits', isfits, status)
         context = ' Unable to open INFILE'
         call fcerr(context)
         goto 1000
      endif

      if ( ifout ) then
         ounit = 16
         call faopen (ounit, outfile, 2, 0, status)
         if (status .ne. 0) then
            context = 'Unable to open outfile; may exist? ' // outfile
            call fcerr(context)
            ifout = .false.
            goto 999
         endif
      endif
c
c     get the total hdu number
c 
      if(isfits) call ftthdu(iunit,totalhdu,status)
      status = 0
      call uclpsi ('totalhdu', totalhdu, status)

      write (context, 1001)
      call fprint (ounit, ifout, context)
      context = ' '
      call fprint (ounit, ifout, context)

C read the primary header
      call ftghpr (iunit, maxdim, simple, bitpix, naxis, naxes,
     &     pcount, gcount, extend, status)

C and write primary header information
      if (extnumb .le. 0) then
         xtension = 'PRIMARY'
         extname = ' '

         if (naxis .le. 0) then
            dims = '0'
            write (context, 1005) 0, 'PRIMARY ', ' ', bitpix,
     &           dims, pcount, gcount
         else
            call fgdims (naxis, naxes, dims)
            write (context, 1005) 0, 'PRIMARY ', ' ', bitpix,
     &           dims, pcount, gcount
         endif
         call fprint (ounit, ifout, context)

C --- get the hdu number 
      call ftghdn(iunit,hdunum)
      hdunum = hdunum - 1

C --- get the NAXISn keywords
         call ftgkyj (iunit, 'NAXIS1', naxis1, comment, kstat)
         if (kstat .ne. 0) then
            kstat = 0
            naxis1 = 1
         endif
         call ftgkyj (iunit, 'NAXIS2', naxis2, comment, kstat)
         if (kstat .ne. 0) then
            kstat = 0
            naxis2 = 1
         endif
         call ftgkyj (iunit, 'NAXIS3', naxis3, comment, kstat)
         if (kstat .ne. 0) then
            kstat = 0
            naxis3 = 1
         endif
         call ftgkyj (iunit, 'NAXIS4', naxis4, comment, kstat)
         if (kstat .ne. 0) then
            kstat = 0
            naxis4 = 1
         endif

         if (extnumb .eq. 0) goto 998
      endif

      nmove = extnumb
      if (nmove .lt. 0) nmove = 1
      count = nmove - 1
 10   continue
      count = count + 1
C
C     If we are displaying column information, re-initialize arrays
C        to avoid problems with information being left over from earlier
C        extensions.
C
      IF (colinfo) THEN
         DO i = 1, maxcl
            tdim (i) = ' '
            dlbounds (i) = 0.0D+00
            dubounds (i) = 0.0D+00
         ENDDO
      ENDIF

C ---   Move to the next extension to be read ---
      call ftmahd(iunit,nmove+1,hdutype,status)
      nmove = nmove + 1
C ---   If status 107 then extension does not exist so exit
      if ( status .eq. 107 ) then
         status = 0
         call ftcmsg
         goto 998
      endif

C ---   If status < 200, 210, 252 or 301, print error and exit
      if (((status .lt. 200) .and. (status .gt. 0)) .or.
     &     (status .eq. 210) .or. (status .eq. 252) .or.
     &     (status .eq. 301)) goto 997

C ---   If this is an ASCII extension then... ---
      if ( hdutype .eq. 1 ) then
         call ftghtb(iunit, maxcl, rowlen,nrows,tfields,ttype,tbcol,
     &        tform,tunit,extname,status)

         call fgcols (rowlen, tfields, nrows, dims)
         call fgvals (iunit, xtension, bitpix, pcount, gcount)
         write (context, 1005) count, xtension, extname, bitpix,
     &        dims, pcount, gcount
         call fprint (ounit, ifout, context)

C
C        If we are displaying column information, get the dimension
C           information, then the minimum and maximum value information,
C           then call the column info display routine
C
         IF (colinfo) THEN
            CALL ftgkns (iunit, 'TDIM', 1, tfields, tdim, nfound,
     &                   status)
            CALL ftgknd (iunit, 'TLMIN', 1, maxcl, dlbounds,
     &                   nfound, status)
            CALL ftgknd (iunit, 'TLMAX', 1, maxcl, dubounds,
     &                   nfound, status)
            CALL wrtcolinfo (ounit, tfields, ttype, tform, tdim, tunit,
     &                       ifout, dlbounds, dubounds, status)
         ENDIF

C --- If this is a BINARY extension then... ---
      else if ( hdutype .eq. 2 ) then
         call ftghbnll(iunit, maxcl, nrows,tfields,ttype,tform,tunit,
     &        extname,varidat,status)

         call ftgkyj (iunit, 'NAXIS1', naxis1, comment, kstat)
         call fgcols (naxis1, tfields, nrows, dims)
         call fgvals (iunit, xtension, bitpix, pcount, gcount)
         write (context, 1005) count, xtension, extname, bitpix,
     &        dims, pcount, gcount
         call fprint (ounit, ifout, context)

C
C        If we are displaying column information, get the dimension
C           information, then the minimum and maximum value information,
C           then call the column info display routine
C
         IF (colinfo) THEN
            CALL ftgkns (iunit, 'TDIM', 1, tfields, tdim, nfound,
     &                   status)
            CALL ftgknd (iunit, 'TLMIN', 1, maxcl, dlbounds,
     &                   nfound, status)
            CALL ftgknd (iunit, 'TLMAX', 1, maxcl, dubounds,
     &                   nfound, status)
            CALL wrtcolinfo (ounit, tfields, ttype, tform, tdim, tunit,
     &                       ifout, dlbounds, dubounds, status)

         ENDIF

C --- If this is an IMAGE extension then ... ---
      else if (hdutype .eq. 0) then
         call ftghpr (iunit, maxdim, simple, bitpix, naxis, naxes,
     &        pcount, gcount, extend, status)

         call fgdims (naxis, naxes, dims)
         call ftgkys (iunit, 'XTENSION', xtension, comment, kstat)
         if (kstat .ne. 0) then
            xtension = ' '
            kstat = 0
         endif
         call ftgkys (iunit, 'EXTNAME', extname, comment, kstat)
         if (kstat .ne. 0) then
            extname = ' '
            kstat = 0
         endif

         write (context, 1005) count, xtension, extname, bitpix,
     &        dims, pcount, gcount
         call fprint (ounit, ifout, context)

C --- This is some unknown type of extension
      else
         call fgvals (iunit, xtension, bitpix, pcount, gcount)
         write (context, 1005) count, xtension, extname, bitpix,
     &        dims, pcount, gcount
         call fprint (ounit, ifout, context)
      end if

C if error somewhere, go back and read next extension
      if (status .ne. 0) then
         call fcerrm (status)
         status = 0
      endif

C --- get the hdu number 
      call ftghdn(iunit,hdunum)
      hdunum = hdunum -1

C --- get the NAXISn keywords
      call ftgkyj (iunit, 'NAXIS1', naxis1, comment, kstat)
      if (kstat .ne. 0) then
         kstat = 0
         naxis1 = 1
      endif
      call ftgkyj (iunit, 'NAXIS2', naxis2, comment, kstat)
      if (kstat .ne. 0) then
         kstat = 0
         naxis2 = 1
      endif
      call ftgkyj (iunit, 'NAXIS3', naxis3, comment, kstat)
      if (kstat .ne. 0) then
         kstat = 0
         naxis3 = 1
      endif
      call ftgkyj (iunit, 'NAXIS4', naxis4, comment, kstat)
      if (kstat .ne. 0) then
         kstat = 0
         naxis4 = 1
      endif

C --- Read next extension
      if (extnumb .lt. 0) go to 10

C --- Get here when a 210, corrupted extension is found
 997  isfits = .false.
      call fcerrm(status)

C --- Write last sent of values to parameter file
 998  call uclpsb ('isfits', isfits, status)
      call uclpsi ('hdunum', hdunum, status)
      call uclpst ('type', xtension, status)
      call uclpst ('extname', extname, status)
      call uclpsi ('bitpix', bitpix, status)
      call uclpsi ('naxis', naxis, status)

      call uclpsi ('naxis1', naxis1, status)
      call uclpsi ('naxis2', naxis2, status)
      call uclpsi ('naxis3', naxis3, status)
      call uclpsi ('naxis4', naxis4, status)
      call uclpsi ('tfields', tfields, status)
      call uclpsi ('pcount', pcount, status)
      call uclpsi ('gcount', gcount, status)

 999  continue
      if ( ifout ) close(ounit)
      status = 0
      call ftclos(iunit,status)
 1000 continue
      return
 1001 format('  No. Type     EXTNAME      BITPIX Dimensions(columns)'
     &     ,'      PCOUNT  GCOUNT')
 1005 format (1x,i3,2x,a8,1x,a13,1x,i3,5x,a19,I10,1x,i4)
C 68
      end

C******************************************************************************
C SUBROUTINE:
C      fgdims
C
C DESCRIPTION:
C       this routine formats the dimension values of a primary array or
C       image extension for output
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C      call fgdims (naxis, naxes, dims)
C
C ARGUMENTS:
C       naxis - number of dimensions
C       naxes - array of dimension sizes
C       dims  - string of dimensions returned
C
C PRIMARY LOCAL VARIABLES:
C       naxis - number of dimensions
C       naxes - array of dimension sizes
C       dims  - string of dimensions returned
C
C CALLED ROUTINES:
C     function fcstln   - index of last non-blank character
C
C******************************************************************************

      subroutine fgdims (naxis, naxes, dims)

      integer naxis
      integer naxes(naxis)
      character*(*) dims

      integer i, fcstln, pos, ldims

      pos = 1
      dims = ' '

      do 100 i = 1, naxis
         write (dims(pos:pos+9), 1000) naxes(i)

 10      ldims = fcstln(dims)
         if (dims(pos:pos) .eq. ' ') then
            dims(pos:ldims-1) = dims(pos+1:ldims)
            dims(ldims:ldims) = ' '
            goto 10
         endif
         pos = ldims + 2
 100  continue

      return
 1000 format (i10)
      end

C******************************************************************************
C SUBROUTINE:
C      fgvals
C
C DESCRIPTION:
C       This routine find the values of the passed keywords
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C      call fgvals (iunit, xtension, bitpix, pcount, gcount)
C
C ARGUMENTS:
C       iunit    - input file unit
C       xtension - returned values of the XTENSION keyword
C       bitpix   - returned value of BITPIX keyword
C       pcount   - returned value of the PCOUNT keyword
C       gcount   - returned value of the GCOUNT keyword
C
C PRIMARY LOCAL VARIABLES:
C       iunit    - input file unit
C       xtension - returned values of the XTENSION keyword
C       bitpix   - returned value of BITPIX keyword
C       pcount   - returned value of the PCOUNT keyword
C       gcount   - returned value of the GCOUNT keyword
C       status   - error number
C
C CALLED ROUTINES:
C      the FITSIO library - all subroutines beginning with ftxxxx
C
C******************************************************************************

      subroutine fgvals (iunit, xtension, bitpix, pcount, gcount)

      integer iunit
      character*(*) xtension
      integer bitpix, pcount, gcount

      integer status
      character(70) comment

      status = 0
      call ftgkys (iunit, 'XTENSION', xtension, comment, status)
      if (status .ne. 0) then
         xtension = ' '
         status = 0
      endif

      call ftgkyj (iunit, 'BITPIX', bitpix, comment, status)
      if (status .ne. 0) then
         bitpix = -999
         status = 0
      endif

      call ftgkyj (iunit, 'PCOUNT', pcount, comment, status)
      if (status .ne. 0) then
         pcount = -999
         status = 0
      endif

      call ftgkyj (iunit, 'GCOUNT', gcount, comment, status)
      if (status .ne. 0) then
         gcount = -999
         status = 0
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      fgcols
C
C DESCRIPTION:
C       this routine formats the width, number of columns and rows
C       of an ascii or binary extension for output
C
C AUTHOR/DATE:
C       Emily A. Greene September, 1992
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C
C USAGE:
C         call fgcols (rowlen, tfields, nrows, dims)
C
C ARGUMENTS:
C       rowlen  - width of extension in bytes
C       tfields - number of columns
C       nrows   - number of rows in the extension
C       dims    - string of dimensions returned
C
C PRIMARY LOCAL VARIABLES:
C       rowlen  - width of extension in bytes
C       tfields - number of columns
C       nrows   - number of rows in the extension
C       dims    - string of dimensions returned
C
C CALLED ROUTINES:
C     function fcstln   - index of last non-blank character
C
C******************************************************************************

      subroutine fgcols (rowlen, tfields, nrows, dims)

      integer rowlen, tfields
      integer*8 nrows
      character*(*) dims

      integer fcstln, pos, ldims

      pos = 1
      dims = ' '

      write (dims(pos:pos+9), 1000) rowlen

 10   ldims = fcstln(dims)
      if (dims(pos:pos) .eq. ' ') then
         dims(pos:ldims-1) = dims(pos+1:ldims)
         dims(ldims:ldims) = ' '
         goto 10
      endif
      pos = ldims + 1

      dims(pos:pos) = '('
      pos = pos + 1
      write (dims(pos:pos+9), 1000) tfields

 20   ldims = fcstln(dims)
      if (dims(pos:pos) .eq. ' ') then
         dims(pos:ldims-1) = dims(pos+1:ldims)
         dims(ldims:ldims) = ' '
         goto 20
      endif
      pos = ldims + 1
      dims(pos:pos) = ')'

      pos = pos + 2
      write (dims(pos:pos+9), 1000) nrows

 30   ldims = fcstln(dims)
      if (dims(pos:pos) .eq. ' ') then
         dims(pos:ldims-1) = dims(pos+1:ldims)
         dims(ldims:ldims) = ' '
         goto 30
      endif

      return
 1000 format (i10)
      end

C*****************************************************************************
C
C SUBROUTINE:
C      wrtcolinfo
C
C DESCRIPTION:
C      Write out a list of column information found in the FITS extension
C      to the specified output device
C
C AUTHOR/DATE:
C      toliver, 03/12/1999 (based on subroutine fishcl found in flcol)
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      CALL wrtcolinfo (ounit, tfields, ttype, tform, tdim, tunit,
C     &                 ifout, dlbounds, dubounds, status)
C
C ARGUMENTS:
C      ounit   - output device logical unit number
C      tfields - number of columns in this extension
C      ttype   - column name
C      tform   - column data type
C      tdim    - input dimensionality of column, if any as string
C      tunit   - units of measurement
C      ifout   - output to file flag
C      status  - status value
C
C PRIMARY LOCAL VARIABLES:
C      context - output string
C
C CALLED ROUTINES:
C      subroutine fprint - ftoolslib routine to write a string to the
C                          output device
C
C*****************************************************************************

      SUBROUTINE wrtcolinfo (ounit, tfields, ttype, tform, tdim, tunit,
     &                       ifout, dlbounds, dubounds, status)

      INTEGER tfields, status, maxcl, ounit, colnum
      PARAMETER (maxcl = 999)
      DOUBLE PRECISION dlbounds(maxcl), dubounds(maxcl)
      INTEGER lbounds, ubounds
      CHARACTER*(*) ttype (maxcl), tform (maxcl), tunit (maxcl),
     &              tdim (maxcl)
      LOGICAL ifout
      character(80) context

C
C     Proceed only if status is o.k.
C
      IF (status .EQ. 0) THEN

C
C        Construct output column headers, then write them out
C
         context = '      Column Name                Format ' //
     &             '    Dims       Units     TLMIN  TLMAX'
         CALL fprint (ounit, ifout, ' ')
         CALL fprint (ounit, ifout, context)

C
C        Iterate over all columns in this extension
C
         DO colnum = 1, tfields
C
C           Display the column information, displaying the minimum and
C              maximum value information only if they exist (assumption
C              is that if the minimum and maximum values for the column
C              are both zero, then the values do not exist)
C
            IF ((dlbounds(colnum) .EQ. 0) .and. 
     &          (dubounds(colnum) .EQ. 0)) THEN

                 WRITE (context, 100) colnum, ttype(colnum),
     &                              tform(colnum), tdim(colnum),
     &                              tunit(colnum)

            ELSE IF  (dlbounds(colnum) .gt. -10000000. .and.
     &            dlbounds(colnum) .lt. 100000000.) THEN
                 lbounds = dlbounds(colnum)

                 if (dubounds(colnum) .gt. -10000000. .and.
     &              dubounds(colnum) .lt. 100000000.) THEN
                    ubounds = dubounds(colnum)

C                   TLMIN and TLMAX are both within reasonalble range
                    WRITE (context, 200) colnum, ttype(colnum),
     &                              tform(colnum), tdim(colnum),
     &                              tunit(colnum), lbounds,
     &                              ubounds
                 ELSE
C                   TLMAX is out of range
                    WRITE (context, 300) colnum, ttype(colnum),
     &                              tform(colnum), tdim(colnum),
     &                              tunit(colnum), lbounds,
     &                              dubounds(colnum)
                 ENDIF

            ELSE
                 if (dubounds(colnum) .gt. -10000000. .and.
     &              dubounds(colnum) .lt. 100000000.) THEN
                    ubounds = dubounds(colnum)

C                   TLMIN is out of range
                    WRITE (context, 400) colnum, ttype(colnum),
     &                              tform(colnum), tdim(colnum),
     &                              tunit(colnum), dlbounds(colnum),
     &                              ubounds
                 ELSE
C                   TLMIN and TLMAX are out of range
                    WRITE (context, 500) colnum, ttype(colnum),
     &                              tform(colnum), tdim(colnum),
     &                              tunit(colnum), dlbounds(colnum),
     &                              dubounds(colnum)
                 ENDIF

            endif

            CALL fprint (ounit, ifout, context)
         ENDDO

         CALL fprint (ounit, ifout, ' ')

      ENDIF

100   FORMAT (I7, 1X, A25, 2X, A8, A12, A20)
200   FORMAT (I7, 1X, A25, 2X, A8, A12, A7, I8, 1X, I8)
300   FORMAT (I7, 1X, A25, 2X, A8, A12, A7, I8, 1X, D8.1)
400   FORMAT (I7, 1X, A25, 2X, A8, A12, A7, D8.1, 1X, I8)
500   FORMAT (I7, 1X, A25, 2X, A8, A12, A7, D8.1, 1X, D8.1)

      RETURN
      END
