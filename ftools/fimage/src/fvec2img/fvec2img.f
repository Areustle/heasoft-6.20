C****************************************************************************
C SELECTOR TASK:
C      fvec2img
C
C FILE:
C      fvec2img.f
C
C DESCRIPTION:
C      This routine takes a vector column and constructs an image.  An optional
C      X (or TIME) column can be used to indicate whether there are gaps in
C      the data.  An error image can optionally be created
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC/Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C       2/27/95 3.4a EAG default image datatype to integer
C                       bug in v2ikey in calculating tdelta fixed
C                       rotate image 90 degrees
C       8/11/97 3.5  Banashree M Seifert
C                  . in the subroutine v2iscl, bscale was calculated as
C                     bscale = int(256. / (xmax - bzero))
C                     bscale=1.D0/bscale
C                    thus causing bscale=infinity in SUN systems and
C                    floating point error on Alphas.
C                    This has been taken careof by inserting one line
C                     if(bscale .eq. 0.) bscale=1.d0
C      1998-10-23  Jeff Guerber
C              ftgcvd() was call with "-99.0" for nulval, causing (I think)
C              f.p. exceptions on Alphas.  Changed to -99.0D0.
C
C      2006-03-20  William Pence
C               fixed bug which reversed the WCS keywords for the x and y axes
C
C NOTES:
C      fvec2img supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C     infile    - string - input FITS file names
C     outfile   - string           - output FITS file name
C     rows      - string - rows to use for each input file
C     xname      - string           - name of X (Time) column
C     vector    - string           - name of the column containing the vector
C     error     - string           - name of the column containing the error
C     strtstart - string           - start time: keyword or value
C     strtdelta - string           - delta for X: keyword or value
C     gapfill   - logical - fill in gaps in time
C     maxrows   - integer - maximum number of rows allowed in image
C     datatype  - string           - output datatype for image
C     scale     - logical          - whether to do scaling of output image
C     bzero     - double           - offset for scaling
C     bscale    - double           - scale factor for scaling
C     copyprime - logical          - copy extra keywords from input primary
C     copyall   - logical          - copy all data into extensions of output
C     sensecase - logical          - be case sensitive about column names
C     tchat     - integer          - terminal chattiness level
C     lchat     - integer          - logfile chattiness level
C
C CALLED ROUTINES:
C
C****************************************************************************

      subroutine fvec2g

      character(160) infile, outfile
      character(80) rows, strtstart, strtdelta, datatype
      character(40) error, vector, xname
      logical gapfill, sensecase, copyall, copyprime, scale
      integer maxrows, tchat, lchat
      double precision bzero, bscale

      integer status

      character(40) taskname
      common /task/ taskname

      taskname = 'FVEC2IMG v3.5'

c initialize variables
      infile = ' '
      rows = ' '
      outfile = ' '
      strtstart = ' '
      strtdelta = ' '
      datatype = ' '
      error = ' '
      vector = ' '
      xname = ' '

      status = 0

C  zero out the FITSIO error stack
      call ftcmsg

C  get the parameters from the par file
      call gvec2img (infile, outfile, rows, xname, vector, error,
     &     strtstart, strtdelta, gapfill, maxrows, datatype, scale,
     &     bzero, bscale, copyprime, copyall, sensecase, tchat, lchat,
     &     status)
      if (status .ne. 0) goto 999

C set up chattiness.  Log file is set using FLOGFILE environment variable
      call xchaty (tchat, lchat)

C create image
      call cvec2img (infile, outfile, rows, xname, vector, error,
     &     strtstart, strtdelta, gapfill, maxrows, datatype, scale,
     &     bzero, bscale, copyprime, copyall, sensecase, status)

 999  if (status .ne. 0) call fcerrm (status)

      return
      end


C****************************************************************************
C SUBROUTINE:
C      gvec2img
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gvec2img uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gvec2img (infile, outfile, rows, xname, vector, error,
C          strtstart, strtdelta, maxrows, datatype, scale, bzero, bscale,
C          copyprime, copyall, sensecase, tchat, lchat, status)
C
C ARGUMENTS:
C     infile    - string - input FITS file names
C     outfile   - string - output FITS file name
C     rows      - string - rows to use for each input file
C     xname      - string           - name of X (Time) column
C     vector    - string           - name of the column containing the vector
C     error     - string           - name of the column containing the error
C     strtstart - string           - start time: keyword or value
C     strtdelta - string           - delta for X: keyword or value
C     gapfill   - logical - whether to fill in gaps in X (time)
C     maxrows   - integer - maximum number of rows in output image
C     datatype  - string           - output datatype for image
C     scale     - logical          - whether to do scaling of output image
C     bzero     - double           - offset for scaling
C     bscale    - double           - scale factor for scaling
C     copyprime - logical          - copy extra keywords from input primary
C     copyall   - logical          - copy all data into extensions of output
C     sensecase - logical          - be case sensitive about column names
C     tchat     - integer          - terminal chattiness level
C     lchat     - integer          - logfile chattiness level
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
C****************************************************************************

      subroutine gvec2img (infile, outfile, rows, xname, vector,
     &     error, strtstart, strtdelta, gapfill, maxrows, datatype,
     &     scale, bzero, bscale, copyprime, copyall, sensecase, tchat,
     &     lchat, status)

      character*(*) infile, outfile, rows, xname, vector, error
      character*(*) strtstart, strtdelta, datatype
      integer maxrows, tchat, lchat, status
      logical gapfill, scale, copyprime, copyall, sensecase
      double precision bzero, bscale

      character(80) context

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

C  get the name of the rows to output
      call uclgst ('rows', rows, status)
      if (status .ne. 0) then
         context = 'could not get ROWS parameter'
         call fcerr(context)
         goto 999
      endif

C  get the xname to output
      call uclgst ('xname', xname, status)
      if (status .ne. 0) then
         context = 'could not get XNAME parameter'
         call fcerr(context)
         goto 999
      endif

C  get the vector to output
      call uclgst ('vector', vector, status)
      if (status .ne. 0) then
         context = 'could not get VECTOR parameter'
         call fcerr(context)
         goto 999
      endif

C  get the error to output
      call uclgst ('error', error, status)
      if (status .ne. 0) then
         context = 'could not get ERROR parameter'
         call fcerr(context)
         goto 999
      endif

C  get the strtstart to output
      call uclgst ('tstart', strtstart, status)
      if (status .ne. 0) then
         context = 'could not get STRTSTART parameter'
         call fcerr(context)
         goto 999
      endif

C  get the strtdelta to output
      call uclgst ('tdelta', strtdelta, status)
      if (status .ne. 0) then
         context = 'could not get STRTDELTA parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to fill in gaps in the data
      call uclgsb ('gapfill', gapfill, status)
      if (status .ne. 0) then
         context = 'could not get GAPFILL parameter'
         call fcerr(context)
         goto 999
      endif

C  get the maximum size for dynamic array allocation
       call uclgsi ('maxrows', maxrows, status)
       if (status .ne. 0) then
           context = 'could not get MAXROWS parameter'
           call fcerr(context)
           goto 999
       endif

C  get the datatype to output
      call uclgst ('datatype', datatype, status)
      if (status .ne. 0) then
         context = 'could not get DATATYPE parameter'
         call fcerr(context)
         goto 999
      endif
      call ftupch (datatype)

C  get whether to scale the data or not
      call uclgsb ('scale', scale, status)
      if (status .ne. 0) then
         context = 'could not get SCALE parameter'
         call fcerr(context)
         goto 999
      endif

C  get default scaling values
      call uclgsd ('bzero', bzero, status)
      if (status .eq. 3) then
c INDEF entered
         status = 0
         bzero = -9999.99
      else if (status .ne. 0) then
         context = 'could not get BZERO parameter'
         call fcerr(context)
         goto 999
      endif

C  get default scaling values
      call uclgsd ('bscale', bscale, status)
      if (status .eq. 3) then
c INDEF entered
         status = 0
         bscale = -9999.99
      else if (status .ne. 0) then
         context = 'could not get BSCALE parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to copy extra keywords from 1st input file primary header
      call uclgsb ('copyprime', copyprime, status)
      if (status .ne. 0) then
         context = 'could not get COPYPRIME parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to copyall input data to extensions of output file
      call uclgsb ('copyall', copyall, status)
      if (status .ne. 0) then
         context = 'could not get COPYALL parameter'
         call fcerr(context)
         goto 999
      endif

C  get whether to be case sensistive about column names
       call uclgsb ('sensecase', sensecase, status)
       if (status .ne. 0) then
           context = 'could not get SENSECASE parameter'
           call fcerr(context)
           goto 999
       endif

C  get whether to be case sensistive about column names
       call uclgsi ('tchat', tchat, status)
       if (status .ne. 0) then
           context = 'could not get TCHAT parameter'
           call fcerr(context)
           goto 999
       endif

C  get whether to be case sensistive about column names
       call uclgsi ('lchat', lchat, status)
       if (status .ne. 0) then
           context = 'could not get LCHAT parameter'
           call fcerr(context)
           goto 999
       endif

 999  continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end


C****************************************************************************
C SUBROUTINE:
C      cvec2img
C
C DESCRIPTION:
C      Create the image from the selected vector
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call cvec2img (infile, outfile, rows, xname, vector, error,
C          strtstart, strtdelta, gapfill, maxrows, scale, bzero, bscale,
C          copyprime, copyall, sensecase, status)
C
C ARGUMENTS:
C     infile    - string - input FITS file names
C     outfile   - string           - output FITS file name
C     rows      - string - rows to use for each input file
C     xname      - string           - name of X (Time) column
C     vector    - string           - name of the column containing the vector
C     error     - string           - name of the column containing the error
C     strtstart - string           - start time: keyword or value
C     strtdelta - string           - delta for X: keyword or value
C     gapfill   - logical - whether to fill in gaps in the data
C     maxrows   - integer - maximum number of rows in output image
C     datatype  - string           - output datatype for image
C     scale     - logical          - whether to do scaling of output image
C     bzero     - double           - offset for scaling
C     bscale    - double           - scale factor for scaling
C     copyprime - logical          - copy extra keywords from input primary
C     copyall   - logical          - copy all data into extensions of output
C     sensecase - logical          - be case sensitive about column names
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine cvec2img (infile, outfile, rows, xname, vector,
     &     error, strtstart, strtdelta, gapfill, maxrows, datatype,
     &     scale, bzero, bscale, copyprime, copyall, sensecase, status)

      integer maxfiles, maxdims, maxranges
      parameter (maxfiles = 50)
      parameter (maxdims = 10)
      parameter (maxranges = 15)

      character*(*) infile, outfile, rows, xname, vector, error
      character*(*) strtstart, strtdelta, datatype
      logical gapfill, scale, copyprime, copyall, sensecase
      double precision bzero, bscale
      integer maxrows, status

      integer naxis, naxes(maxdims), nrows
      integer numranges, startrow(maxranges), stoprow(maxranges)

      integer block, htype, ycol, xcol, vecsize, errcol, totalrows
      integer nelem, imgptr, gcount, i, j, fstat, fcstln
      integer iunit, ounit, nfiles, numrows, bitpix, extno
      double precision tstart, tdelta, dblval
      character(160) filelist(maxfiles), filename
      character(80) rowlist(maxfiles), context
      character(16) keyword, keyval
      logical imageok, negflag

      external BADDUMMY, GOODDUMMY
C
C common definition for dynamic memory
C
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

      character(40) taskname
      common /task/ taskname

C initialize variables
      imageok = .false.

C set up the chattiness
      call xwrite (' ', 5)
      call xwrite (taskname, 5)
      call xwrite (' ', 5)

C get the list of input file names and matching row specifications
      call fcgcls (infile, filelist, nfiles, negflag)
      call fcgcls (rows, rowlist, numrows, negflag)
      if ((nfiles .ne. numrows) .and. (numrows .gt. 1)) then
         context = ' Number of input files and row ranges do not match'
         call fcerr (context)
         status = 10
         goto 999
      endif

C figure out datatype of output file, default is integer, for now ...
      bitpix = 32
      call dataty (datatype, bitpix, status)
      write (context, '(a16,i4)') ' Using bitpix = ', bitpix
      call xwrite (context, 15)
      if (status .ne. 0) goto 999

C open the 1st file
      call fcpars (filelist(1), filename, extno, status)
      if (extno .eq. -99) extno = 1
      call ftgiou (iunit, status)
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Error opening input file ' // filename
         call fcerr (context)
         goto 999
      endif
      context = ' opened input file ' // filename
      call xwrite (context, 15)

c move to correct extension
      call ftmrhd (iunit, extno, htype, status)

C and check for requested columns
      call ftgcno (iunit, sensecase, vector, ycol, status)
      if (status .ne. 0) then
         context = ' Could not find column ' // vector
         call fcerr (context)
         goto 998
      endif
      call ftgtdm (iunit, ycol, maxdims, naxis, naxes, status)
      if ((status .ne. 0) .or. (naxis .gt. 1)) then
         context = ' Requested column not a vector ' // vector
         call xwrite (context, 15)
         context = ' First dimension used as vector '
         call xwrite (context, 15)
c         call fcerr (context)
c         if (status .eq. 0) status = 10
c         goto 998
      endif
      vecsize = naxes(1)
      write (context, '(a25,i10)') ' The vector has length = ', vecsize
      call xwrite (context, 15)

C check for X column, if any
      if ((xname .eq. ' ') .or. (xname .eq. '-') .or.
     &     (xname .eq. 'NONE')) then
         xcol = -1
      else
         call ftgcno (iunit, sensecase, xname, xcol, status)
         if (status .ne. 0) then
            context = ' Cound not find column ' // xname
            call fcerr (context)
            goto 998
         endif
      endif
      write (context, '(A27,i4)') ' X (TIME) column number is ', xcol
      call xwrite (context, 15)

C check for error column and whether is actually requested
      if ((error .ne. ' ') .and. (error .ne. 'NONE')) then
         call ftgcno (iunit, sensecase, error, errcol, status)
         if (status .ne. 0) then
            context = ' Could not find column ' // error
            call fcerr (context)
            goto 998
         else
c found column check dimensions
            call ftgtdm (iunit, errcol, maxdims, naxis, naxes, status)
            if ((status .ne. 0) .or. (naxis .gt. 1)) then
               context = ' Requested error column not a vector '
     &              // error
               call fcerr (context)
               if (status .eq. 0) status = 10
               goto 998
            else
               if (vecsize .ne. naxes(1)) then
                  context = ' Vector and error have different dims'
                  call fcerr (context)
                  status = 10
                  goto 998
               endif
            endif
         endif
      else
         errcol = -1
      endif
      write (context, '(A24,i4)') ' Error column number is ', errcol
      call xwrite (context, 15)

C position the file to the first requested record according to rows parameter
      call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
      call fcgrgs (rowlist(1), nrows, numranges, startrow, stoprow)

      write (context, '(a28,i10)') ' The first row requested is ',
     &     startrow(1)
      call xwrite (context, 15)

C get the correct TSTART and TDELTA
      call v2ikeys (strtstart, strtdelta, iunit, xcol, startrow(1),
     &     tstart, tdelta, status)
      if (status .ne. 0) goto 998
      write (context, '(A16,1pe23.15,a14,e23.15)') ' Found tstart = ',
     &     tstart, ' and tdelta = ', tdelta
      call xwrite (context, 15)
      call ftclos (iunit, status)
      call ftfiou (iunit, status)

C figure the total size of the array
      call v2isize (iunit, filelist, nfiles, rowlist, numrows, xcol,
     &     tstart, tdelta, gapfill, totalrows, status)
      if (status .ne. 0) goto 998
      if (totalrows .gt. maxrows) then
         write (context, 1111) totalrows, maxrows
 1111    format (' Requested array is ',i5,
     &        ' rows, maximum allowed is ', i5)
         call fcerr (context)
         context = ' Reduce requested image or increase max allowed'
         call fcerr (context)
         status = 1
         goto 998
      endif

      write (context, '(a21,i10,a5)') ' Image will contain ', totalrows,
     &     ' rows'
      call xwrite (context, 15)

C allocate the need image size
      imgptr = 0
      nelem = totalrows*vecsize
      call udmget (nelem, 7, imgptr, status)
      if (status .ne. 0) then
         context = ' error allocating output image memory'
         call fcerr (context)
         goto 998
      else
         imageok = .true.
      endif

C initialize the array to 0
      call initdoublearray(memd(imgptr), nelem)

C get the main image
      call v2iget (iunit, ycol, xcol, filelist, nfiles, rowlist,
     &     numrows, tstart, tdelta, gapfill, memd(imgptr), totalrows,
     &     vecsize, status)
      if (status .ne. 0) then
         context = ' Error creating output image '
         call fcerr (context)
         goto 998
      endif
      call xwrite (' Sucessfully read in image', 15)

C create output file
      call ftgiou (ounit, status)
      call ffinit (ounit, outfile, status)
      if (status .ne. 0) then
         context = ' Could not create output file, may exist? '
     &        // outfile
         call fcerr (context)
         goto 998
      endif
      context = ' Created output file ' // outfile
      call xwrite (context, 15)

C create the primary header
      naxes(1) = totalrows
      naxes(2) = vecsize

      if (errcol .eq. -1) then
         gcount = 1
      else
         gcount = 2
      endif

      call ftphpr (ounit, .true., bitpix, 2, naxes, 0, gcount,
     &     .true., status)
      call xwrite ( ' Created primary header ', 15)

C get to the correct place in the 1st input file
      call ftgiou (iunit, status)
      call ftopen (iunit, filename, 0, block, status)
      if (copyprime) call copyhead (iunit, ounit, .true., .true.,
     &     BADDUMMY, GOODDUMMY, status)
      if (status .ne. 0) goto 998

C move to the appropriate extension
      call ftmrhd (iunit, extno, htype, status)
      if (status .ne. 0) call fcerrm (status)

C write the WCS keywords appropriately
      call ftpkys (ounit, 'CTYPE1', xname, '1st axis type', status)
      if (status .ne. 0) call fcerrm (status)
      call ftkeyn ('TUNIT', xcol, keyword, status)
      call ftgkys (iunit, keyword, keyval, context, status)
      if (status .eq. 0) then
         call ftpkys (ounit, 'CUNIT1', keyval, context, status)
      else
         status = 0
         call ftcmsg
      endif
      call ftpkyd (ounit, 'CDELT1', tdelta, 13,
     &     '1st axis coordinate increment', status)
      dblval = tstart + tdelta/2.D0
      call ftpkyd (ounit, 'CRVAL1', dblval, 13,
     &     '1st axis value at reference pixel', status)
      call ftpkyd (ounit, 'CRPIX1', 1.D0, 13,
     &     '1st axis reference pixel', status)
      call xwrite (' Wrote TIME axis coordinate keywords ', 15)

      call ftkeyn ('1CTYP', ycol, keyword, status)
      call ftgkys (iunit, keyword, keyval, context, status)
      if (status .eq. 0) then
         call ftpkys (ounit, 'CTYPE2', keyval, context, status)
      else
         status = 0
         call ftcmsg
      endif
      call ftkeyn ('1CRPX', ycol, keyword, status)
      call ftgkyd (iunit, keyword, dblval, context, status)
      if (status .eq. 0) then
         call ftpkyd (ounit, 'CRPIX2', dblval, 13, context, status)
      else
         status = 0
         call ftcmsg
         call ftpkyd (ounit, 'CRPIX2', 1.d0, 13, context, status)
      endif
      call ftkeyn ('1CUNI', ycol, keyword, status)
      call ftgkys (iunit, keyword, keyval, context, status)
      if (status .eq. 0) then
         call ftpkys (ounit, 'CUNIT2', keyval, context, status)
      else
         status = 0
         call ftkeyn ('TUNIT', ycol, keyword, status)
         call ftgkys (iunit, keyword, keyval, context, status)
         if (status .eq. 0) call ftpkys (ounit, 'CUNIT2', keyval,
     &        context, status)
         call ftcmsg
      endif
      call ftkeyn ('1CRVL', ycol, keyword, status)
      call ftgkyd (iunit, keyword, dblval, context, status)
      if (status .eq. 0) then
         call ftpkyd (ounit, 'CRVAL2', dblval, 13, context, status)
      else
         status = 0
         call ftcmsg
         call ftpkyd (ounit, 'CRVAL2', 1.D0, 13, context, status)
      endif
      call ftkeyn ('1CDLT', ycol, keyword, status)
      call ftgkyd (iunit, keyword, dblval, context, status)
      if (status .eq. 0) then
         call ftpkyd (ounit, 'CDELT2', dblval, 13, context, status)
      else
         status = 0
         call ftpkyd (ounit, 'CDELT2', 1.D0, 13, context, status)
         call ftcmsg
      endif

C write some history records
      do 100 i=1, nfiles
         context = taskname(1:fcstln(taskname)) // ' using file '
     &        // filelist(i)
         call ftphis (ounit, context, status)
 100  continue
      if (status .ne. 0) then
         context = ' Error creating output image header '
         call fcerr (context)
         goto 998
      endif

      call xwrite (' Finished writing header keywords ', 15)

C close up the input file
      call ftclos (iunit, status)
      call ftfiou (iunit, status)

C figure out and initialize any requested scaling
      if (scale) call v2iscl (ounit, bzero, bscale, memd(imgptr),
     &     nelem, bitpix, status)

C make sure all header information is registered with FITSIO
      call ftrdef (ounit, status)
      if (status .ne. 0) then
         call fcerr (' Error setting up output primary header ')
         goto 997
      endif

C write out the image
C      call ftp2dd (ounit, 1, vecsize, vecsize, totalrows,
C     &     memd(imgptr), status)
      call v2iwrt (ounit, 1, vecsize, totalrows, memd(imgptr), status)
      if (status .ne. 0) then
         call fcerr (' Error writing image to output file ')
         call fcerrm (status)
         goto 997
      endif

C get the error image, if any
      if (errcol .ne. -1) then
         call initdoublearray(memd(imgptr), nelem)

         call v2iget (iunit, errcol, xcol, filelist, nfiles, rowlist,
     &        numrows, tstart, tdelta, gapfill, memd(imgptr), totalrows,
     &        vecsize, status)
C         call ftp2dd (ounit, 2, vecsize, vecsize, totalrows,
C     &        memd(imgptr), status)
       call v2iwrt (ounit, 2, vecsize, totalrows, memd(imgptr), status)
        if (status .ne. 0) then
            context = ' Error creating error image '
            call fcerr (context)
            goto 997
         endif
      endif

C copy the data, if so requested
      if (copyall) then
         do 800 i = 1, nfiles
            if (numrows .eq. 1) then
               j = 1
            else
               j = i
            endif

            call fcpars (filelist(i), filename, extno, status)
            if (extno .eq. -99) extno = 1
            call ftgiou (iunit, status)
            call ftopen (iunit, filename, 0, block, status)
            if (status .ne. 0) then
               context = ' Error opening input file ' // filename
               call fcerr (context)
               goto 999
            endif

c move to correct extension
            call ftmrhd (iunit, extno, htype, status)

C create the extension in the output file
            call ftcrhd (ounit, status)

C and copy the extension
            call ftcopy (iunit, ounit, 0, status)
            call ftclos (iunit, status)
            call ftfiou (iunit, status)

 800     continue
      endif

 997  fstat = 0
      if (status .ne. 0) then
         call ftdelt (ounit, fstat)
      else
         call ftclos (ounit, fstat)
      endif
      call ftfiou (ounit, fstat)

 998  fstat = 0
      call ftclos (iunit, fstat)
      call ftfiou (iunit, fstat)

      fstat = 0
      if (imageok) call udmfre (imgptr, 7, fstat)

 999  return
      end

C****************************************************************************
C SUBROUTINE:
C      v2ikeys
C
C DESCRIPTION:
C      Determine the start X and step
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call v2ikeys (strtstart, strtdelta, iunit, xcol, frow,
C                    tstart, tdelta, status)
C
C ARGUMENTS:
C      strtstart - string - input - the time/keyword to start the image
C      strtdelta - string - input - the step/keyword for each row of the image
C      iunit     - integer - input - open input file unit number
C      xcol      - integer - input  - the x column number
C      frow      - integer - input  - the 1st row number to use
C      tstart    - double  - output - first value for X (time)
C      tdelta    - double  - output - step in X (time)
C      status    - integer - output - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine v2ikeys (strtstart, strtdelta, iunit, xcol, frow,
     &     tstart, tdelta, status)

      character*(*) strtstart, strtdelta
      integer iunit, xcol, status, frow
      double precision tstart, tdelta

      logical anyf
      character(80) context
      double precision t2

      if (status .ne. 0) return

C if blank or - use first value in file
      if ((strtstart .eq. ' ') .or. (strtstart .eq. '-')) then
         call ftgcvd (iunit, xcol, frow, 1, 1, -9999.99D0, tstart, anyf,
     &        status)
         goto 100
      endif

C see if strtstart is a keyword
      call ftgkyd (iunit, strtstart, tstart, context, status)
      if (status .eq. 202) then
         status = 0
         call ftcmsg
      endif

C OK, it must be a number
      call ftc2dd (strtstart, tstart, status)
C if no error, was read OK
      if (status .eq. 0) goto 100

C and now figure the delta
c if blank or - use difference between 1st 2 rows
 100  if ((strtdelta .eq. ' ') .or. (strtdelta .eq. '-')) then
         call ftgcvd (iunit, xcol, frow+1, 1, 1, -9999.99D0, t2, anyf,
     &        status)
         tdelta = t2 - tstart
         if (tdelta .le. 0.) then
            context = ' Error determining tdelta using 1st input file'
            call fcerr (context)
            status = 10
         endif
         return
      endif

C check if a keyword
      call ftgkyd (iunit, strtdelta, tdelta, context, status)
      if (status .eq. 202) then
         status = 0
         call ftcmsg
      endif

C OK, it must be a number
      call ftc2dd (strtdelta, tdelta, status)
C if no error, was read OK
      if (status .eq. 0) return

 999  return
      end

C****************************************************************************
C SUBROUTINE:
C      v2iget
C
C DESCRIPTION:
C      Create the image from the selected vector
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C     needs to be modified to allow tdelta > natural step of file when
C     gapfill is set
C
C USAGE:
C         call v2iget (iunit, ycol, xcol, filelist, nfiles, rowlist, numrows,
C                      tstart, tdelta, gapfill, image, xmax, ymax, status)
C
C ARGUMENTS:
C      iunit    - integer      - input - unit number of input file to use
C      ycol     - integer      - input - column number of the vector
C      xcol     - integer      - input - column number for X (time) if any
C      filelist - string array - input - filenames to use
C      nfiles   - integer      - input - number of files
C      rowlist  - string array - input - row range for each file (or all)
C      numrows  - integer      - input - number of different row ranges
C      tstart   - double       - input - the initial start value of array
C      tdelta   - double       - input - spacing of array
C      gapfill  - logical      - input - whether to fill in gaps in X (time)
C      image    - double       - output - the output image
C      xmax     - integer      - input  - the x dimension of the image
C      ymax     - integer      - input  - the y dimension of the image
C      status   - integer      - output - the status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine v2iget (iunit, ycol, xcol, filelist, nfiles,
     &     rowlist, numrows, tstart, tdelta, gapfill, image, xmax,
     &     ymax, status)

      integer iunit, ycol, xcol, nfiles, numrows, xmax, ymax
      integer status
      character*(*) filelist(nfiles), rowlist(numrows)
      double precision tstart, tdelta, image(ymax, xmax)
      logical gapfill

      integer fstat, row, i, j, k, extno, block, htype, nrows
      integer maxranges, ii
      parameter (maxranges = 15)

      integer numranges, startrow(maxranges), stoprow(maxranges)
      character(160) filename
      character(80) context
      logical anyf
      double precision time

      if (status .ne. 0) return

      row = 0
      anyf = .false.
C loop throught the input files
      do 100 i = 1, nfiles
         if (numrows .eq. 1) then
            ii = 1
         else
            ii = i
         endif

         call fcpars (filelist(i), filename, extno, status)
         if (extno .eq. -99) extno = 1
         call ftgiou (iunit, status)
         call ftopen (iunit, filename, 0, block, status)
         if (status .ne. 0) then
            context = ' Error opening input file ' // filename
            call fcerr (context)
            goto 999
         endif

c move to correct extension
         call ftmrhd (iunit, extno, htype, status)

c and get nrows
         call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
         call fcgrgs (rowlist(ii), nrows, numranges, startrow, stoprow)

C loop over the requested rows
         do 90 j = 1, numranges
            do 80 k = startrow(j), stoprow(j)

C figure out where to put this row
C check if tstart was specified
               if (xcol .gt. 0) then
                  call ftgcvd (iunit, xcol, k, 1, 1, -99.0D0, time,
     &                 anyf, status)
C check that this time is after the start time
                  if (time .lt. tstart) goto 80
C figure the correct row
                  if (gapfill) then
                     row = row + 1
                  else
C the .49 selects the first row that matches the tdelta criteria
                     row = nint (.49 + (time - tstart) / tdelta) + 1
                  endif
               else
                  row = row + 1
               endif

C read the vector
               call ftgcvd (iunit, ycol, k, 1, ymax, -99.0D0,
     &              image(1,row), anyf, status)
 80         continue
 90      continue
         context = ' completed processing file: ' // filename
         call xwrite (context, 15)
 100  continue
         write (context, '(i10, a)') row, ' rows were processed'
         call xwrite (context, 15)

 998  fstat = 0
      call ftclos (iunit, fstat)
      call ftfiou (iunit, fstat)

 999  return
      end

C****************************************************************************
C SUBROUTINE:
C      v2isize
C
C DESCRIPTION:
C      Figure out the total size of the output image
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C      rows in files are assumed to be ordered by X (time)
C
C      c$$$ lines are for cases where gapfill is requested, and tdelta is
C      greater than the natural time step of the file.  The routine v2iget
C      will have to be modified to get the correct row for the image
C
C USAGE:
C      call v2isize (iunit, filelist, nfiles, rowlist, numrows, xcol, tstart, tdelta,
C     &     gapfill, totalrows, status)
C
C ARGUMENTS:
C      iunit    - integer      - input - unit number of input file to use
c      filelist - string array - input - filenames to use
C      nfiles   - integer      - input - number of files
C      rowlist  - string array - input - row range for each file (or all)
C      numrows  - integer      - input - number of different row ranges
C      xcol     - integer      - input - X (time) column number
C      tstart   - double       - input - the initial start value of array
C      tdelta   - double       - input - spacing of array
C      gapfill  - logical      - input - whether to fill in gaps in X (time)
C      totalrows - integer     - output - the returned number of rows for the image
C      status    - integer     - output - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine v2isize (iunit, filelist, nfiles, rowlist, numrows,
     &     xcol, tstart, tdelta, gapfill, totalrows, status)

      integer iunit, nfiles, xcol, numrows, totalrows, status
      character*(*) filelist(nfiles), rowlist(numrows)
      double precision tstart, tdelta
      logical gapfill

      integer i, j, k
      integer htype, nrows, maxranges, fstat, extno, block
      parameter (maxranges = 15)

      integer numranges, startrow(maxranges), stoprow(maxranges)
      logical anyf
      double precision time, endtime
c$$$      double precision time2, tstep
      character(160) filename
      character(80) context

      totalrows = 0
      endtime = tstart
C loop throught the input files and count the rows
      do 100 i = 1, nfiles
         if (numrows .eq. 1) then
            j = 1
         else
            j = i
         endif

         call fcpars (filelist(i), filename, extno, status)
         if (extno .eq. -99) extno = 1
         call ftgiou (iunit, status)
         call ftopen (iunit, filename, 0, block, status)
         if (status .ne. 0) then
            context = ' Error opening input file ' // filename
            call fcerr (context)
            goto 999
         endif

c move to correct extension
         call ftmrhd (iunit, extno, htype, status)
c and get nrows
         call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
         call fcgrgs (rowlist(j), nrows, numranges, startrow, stoprow)

C if a tstart was specified, check that the values are before
         if ((gapfill) .and. (xcol .gt. 0)) then

c this code is for figuring the time step for gapfill, tdelta > timestep case
c$$$c find the "natural" tdelta of the file. check TIMEDEL keyword first
c$$$            call ftgkyd (iunit, 'TIMEDEL', tstep, context, status)
c$$$            if (status .eq. 202) then
c$$$               call ftcmsg
c$$$               status = 0
c$$$C TIMEDEL not found, calculate tstep from data
c$$$               call ftgcvd (iunit, xcol, 1, 1, 1, -99.0D0,
c$$$     &              time, anyf, status)
c$$$               call ftgcvd (iunit, xcol, 2, 1, 1, -99.0D0,
c$$$     &              time2, anyf, status)
c$$$               tstep = time2 - time
c$$$            endif

            do 10 k = 1, numranges
 5             call ftgcvd (iunit, xcol, startrow(k), 1, 1, -99.0D0,
     &              time, anyf, status)
               if (time .ge. tstart) goto 20
C oops, this is before tstart
               startrow(k) = startrow(k) + 1
               if (startrow(k) .gt. stoprow(k)) goto 10
               goto 5
 10         continue

C reset the values of numranges, etc appropriately
 20         if (startrow(1) .gt. stoprow(1)) then
               do 30 k = 2, numranges
                  startrow(k-1) = startrow(k)
                  stoprow(k-1) = stoprow(k)
 30            continue
               numranges = numranges - 1
               if (numranges .le. 0) goto 100
               goto 20
            endif
c$$$         else
c$$$            tstep = tdelta
         endif

C calculate the number of rows for gapfilled case
c$$$C tstep/tdelta correctly calculates rows for tdelta > natural step of file
         if (gapfill) then
            do 40 k = 1, numranges
               totalrows = totalrows + (stoprow(k) - startrow(k) + 1)
c$$$     &              * tstep/tdelta
 40         continue
         else

C for the non-gap filled case, we just need to find the largest time
C the files are assumed to be time ordered
C note that is really inefficient - should be able to get a keyword with last time value
            call ftgcvd (iunit, xcol, stoprow(numranges), 1, 1, -99.0D0,
     &           time, anyf, status)
            endtime = max(time, endtime)
         endif
         fstat = 0
         call ftclos (iunit, fstat)
         call ftfiou (iunit, fstat)
 100  continue

C for the non-gapfilled case, calculate the number of rows
      if (.not. gapfill) totalrows = nint((endtime - tstart)/tdelta) + 1

 999  return
      end

C****************************************************************************
C SUBROUTINE:
C      v2iscl
C
C DESCRIPTION:
C figure out and initialize any requested scaling
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       January, 1995
C
C MODIFICATION HISTORY:
C       8/11/97 3.5  Banashree M Seifert
C                  . here, bscale was calculated as
C                     bscale = int(256. / (xmax - bzero))
C                     bscale=1.D0/bscale
C                    thus causing bscale=infinity in SUN systems and
C                    floating point error on Alphas.
C                    This has been taken careof by inserting one line
C                     if(bscale .eq. 0.) bscale=1.d0
C
C NOTES:
C      if scaling is requested, but not specified equally spaced values
C      are used
C
C USAGE:
C      call v2iscl (ounit, bzero, bscale, image, nelem, bitpix, status)
C
C ARGUMENTS:
C      ounit  - integer - input  - unit number of output file to use
C      bzero  - double  - input  - the requested offset, if any
C      bscale - double  - input  - the requested scale factor, if any
C      image  - double  - input  - the image, needed for autoscaling
C      nelem  - integer - input  - the number of elements in image
C      bitpix - integer - input  - the image type
C      status - integer - output - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************

      subroutine v2iscl (ounit, bzero, bscale, image, nelem, bitpix,
     &     status)

      integer ounit, nelem, status, bitpix
      double precision bzero, bscale, image(nelem)

      integer i
      double precision xmin, xmax
      character(80) context

      xmin = 1.e37
      xmax = -1.e37
C check if we need to auto-scale
      if ((bzero .eq. -9999.99) .or. (bscale .eq. -9999.99)) then

C find the min and max of the array, not counting 0
         do 100 i = 1, nelem
            if (image(i) .eq. 0.) goto 100
            xmin = min (xmin, image(i))
            xmax = max (xmax, image(i))
 100     continue
         write (context, '(a21,1pe23.15)') ' Array has minimum = ', xmin
         call xwrite (context, 15)
         write (context, '(a21,1pe23.15)') '       and maximum = ', xmax
         call xwrite (context, 15)
      endif

C if we need to calculate the offset
      if (bzero .eq. -9999.99) bzero = xmin
C if we need to calculate the scale
      if (bscale .eq. -9999.99) then
         if (bitpix .gt. 0) then
C if this is an integer image, quantize bscale appropriately
            bscale = int(256. / (xmax - bzero))
            if(bscale .eq. 0.) bscale=1.d0
            bscale = 1.D0 / bscale
         else
            bscale = (xmax - bzero) / 256.
         endif
      endif

C write values to header
      call ftpkyd (ounit, 'BZERO', bzero, 13, 'Scaling offset', status)
      call ftpkyd (ounit, 'BSCALE', bscale, 13, 'Scaling factor',
     &     status)

      write (context, '(A22,1pe23.15)') ' Set scaling offset = ', bzero
      call xwrite (context, 15)
      write (context, '(a22,1pe23.15)') ' Set scaling factor = ', bscale
      call xwrite (context, 15)

      return
      end

C****************************************************************************
C SUBROUTINE:
C      v2iwrt
C
C DESCRIPTION:
C      Write out the image
C
C AUTHOR:
C       Dr. Emily A. Greene
C       NASA/GSFC / Hughes STX
C       February, 1995
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call v2iwrt (ounit, group, ymax, xmax, image, status)
C
C ARGUMENTS:
C      ounit    - integer      - input - unit number of output file to use
C      group    - integer      - input - group to write image to
C      image    - double       - output - the output image
C      xmax     - integer      - input  - the x dimension of the image
C      ymax     - integer      - input  - the y dimension of the image
C      status   - integer      - output - the status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C
C*************************************************************************
      subroutine v2iwrt (ounit, group, xmax, ymax, image, status)

      integer ounit, group, ymax, xmax, status
      double precision image(xmax, ymax)

      integer i, j, fpixel

      fpixel = 0
      do 200 j = 1, xmax
         do 100 i = 1, ymax
            fpixel = fpixel + 1
            call ftpprd (ounit, group, fpixel, 1, image(j,i), status)
 100     continue
 200  continue

      return
      end
