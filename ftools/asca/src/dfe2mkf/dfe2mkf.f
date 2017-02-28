C  FTOOLs info $Header: /headas/headas/ftools/asca/src/dfe2mkf/dfe2mkf.f,v 3.10 2013/05/21 19:08:06 irby Exp $
C      
C******************************************************************************
C SELECTOR TASK:
C      dfe2mkf
C
C FILE:
C      dfe2mkf.f 
C
C DESCRIPTION: 
C       This routine reads the output of FAINTDFE and places the value
C       in the specified columns of an exising MKF file.
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       September, 1994
C
C MODIFICATION HISTORY:
C       Added check on Sn_MODE column so DFE columns are only filled when
C       that SIS was in faint mode (vers 1.1)
C               Koji Mukai, USRA, October 1994
C
C       10/12/94 1.2 EAG  - added timestamp call
C       08/27/98 1.2a PDW - Fixed bad ftgcvd(..., 0,...) subroutine call
C
C
C NOTES:
C      dfe2mkf supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine dfe2mf
      character(160) infile, outfile
      character(16) s0mode, s0c0, s0c1, s0c2, s0c3
      character(16) s1mode, s1c0, s1c1, s1c2, s1c3
      character(16) mkftime
      character(4) instrument
      logical sensecase

      integer status

      character(40) taskname
      common /task/ taskname

      taskname = 'dfe2mkf1.2a'
      call ftcmsg

      infile = ' '
      outfile = ' '
      instrument = ' '
      s0mode = ' '
      s0c0 = ' '
      s0c1 = ' '
      s0c2 = ' '
      s0c3 = ' '
      s1mode = ' '
      s1c0 = ' '
      s1c1 = ' '
      s1c2 = ' '
      s1c3 = ' '
      mkftime = ' '

      status = 0

C  get the parameters from the par file
      call gdfe2mkf (infile, outfile, instrument, s0mode, s0c0, s0c1,
     &     s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3, mkftime,
     &     sensecase, status)
      if (status .ne. 0) goto 999

C populate the columns in the mkf file
      call pdfe2mkf (infile, outfile, instrument, s0mode, s0c0, s0c1,
     &     s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3, mkftime,
     &     sensecase, status)

 999  if (status .ne. 0) call fcerrm (status)

      return
      end


C******************************************************************************
C SUBROUTINE:
C      gdfe2mkf
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       2 September, 1994
C
C MODIFICATION HISTORY:
C       Added s0mode and s1mode - Koji Mukai, USRA, 1994 October
C
C NOTES:
C      gdfe2mkf uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C        call gdfe2mkf (infile, outfile, instrument, s0mode, s0c0, s0c1,
C            s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3, sensecase, status)
C
C ARGUMENTS:
C       infile   - input mkfilter file and extension number
C       outfile  - output filename
C       instrument - string - output - the instrument for this DFE file
C       snmode - string - output - The name of the column in the mkf file
C                                  mode for SIS n
C       sncm - string - output - The name of the column in the mkf file
C                                n is the instrument, m is the chip
C       sensecase - whether to be case sensitive about column names
C       status   - status of operation
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
      subroutine gdfe2mkf (infile, outfile, instrument, s0mode,
     &     s0c0, s0c1, s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3,
     &     mkftime, sensecase, status)

      character*(*) infile, outfile, instrument, mkftime
      character*(*) s0mode, s0c0, s0c1, s0c2, s0c3
      character*(*) s1mode, s1c0, s1c1, s1c2, s1c3
      logical sensecase
      integer status

      character(80) context

C  get the name of the input FITS file
      call uclgst ('infile', infile, status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the mkf FITS file
      call uclgst ('outfile', outfile, status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the instrument
      call uclgst ('instrument', instrument, status)
      if (status .ne. 0) then
         context = 'could not get INSTRUMENT parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S0 mode column
      call uclgst ('s0mode', s0mode, status)
      if (status .ne. 0) then
         context = 'could not get s0mode parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S0 chip 0 DFE column
      call uclgst ('s0c0', s0c0, status)
      if (status .ne. 0) then
         context = 'could not get s0c0 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S0 chip 1 DFE column
      call uclgst ('s0c1', s0c1, status)
      if (status .ne. 0) then
         context = 'could not get s0c1 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S0 chip 2 DFE column
      call uclgst ('s0c2', s0c2, status)
      if (status .ne. 0) then
         context = 'could not get s0c2 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S0 chip 3 DFE column
      call uclgst ('s0c3', s0c3, status)
      if (status .ne. 0) then
         context = 'could not get s0c3 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S1 mode column
      call uclgst ('s1mode', s1mode, status)
      if (status .ne. 0) then
         context = 'could not get s0mode parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S1 chip 0 DFE column
      call uclgst ('s1c0', s1c0, status)
      if (status .ne. 0) then
         context = 'could not get s1c0 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S1 chip 1 DFE column
      call uclgst ('s1c1', s1c1, status)
      if (status .ne. 0) then
         context = 'could not get s1c1 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S1 chip 2 DFE column
      call uclgst ('s1c2', s1c2, status)
      if (status .ne. 0) then
         context = 'could not get s1c2 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the S1 chip 3 DFE column
      call uclgst ('s1c3', s1c3, status)
      if (status .ne. 0) then
         context = 'could not get s1c3 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the time column in mkf file
      call uclgst ('mkftime', mkftime, status)
      if (status .ne. 0) then
         context = 'could not get mfktime parameter'
         call fcerr (context)
         goto 999
      endif

C  get whether to be case sensitive about column names
      call uclgsb ('sensecase', sensecase, status)
      if (status .ne. 0) then
         context = 'could not get SENSECASE parameter'
         call fcerr (context)
         goto 999
      endif

 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      pdfe2mkf
C
C DESCRIPTION: 
C      Populate the appropriate DFE column in an mkf file
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       2 September, 1994
C
C MODIFICATION HISTORY:
C       Added check of SIS mode --- only fills the DFE columns when
C       the SIS in question is operating in Faint mode
C              Koji Mukai, USRA, 6 October 1994
C
C NOTES:
C
C USAGE:
C        call pdfe2mkf (infile, outfile, instrument, s0mode,
C            s0c0, s0c1, s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3,
C            mkftime, sensecase, status)
C
C ARGUMENTS:
C       infile   - input mkfilter file and extension number
C       outfile  - output file name
C       instrument - string - input - instrument for this DFE file
C       snmode - string - input - name of the mode columns in the mkf file
C       sncm - string - input - name of the chip columns in the mkf file
C       mkftime - string - input - name of time column in mkf file
C       sensecase - whether to be case sensitive about column names
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine pdfe2mkf (infile, outfile, instrument, s0mode, s0c0,
     &     s0c1, s0c2, s0c3, s1mode, s1c0, s1c1, s1c2, s1c3, mkftime,
     &     sensecase, status)

      character*(*) infile, outfile, instrument, mkftime
      character*(*) s0mode, s0c0, s0c1, s0c2, s0c3
      character*(*) s1mode, s1c0, s1c1, s1c2, s1c3
      logical sensecase
      integer status

      integer maxdfe
      parameter (maxdfe = 16384)
      real dfe (4, maxdfe)
      double precision dfetime(maxdfe), time
      character(160) filename
      integer extnumb, ounit, block, htype, numdfe, dferow, mkfrow
      integer mkftno, modcolno, c0colno, c1colno, c2colno, c3colno
      integer nrows, felem, nelem, sismode
      logical anyf
      character(80) context

      ounit = 16

C open the mkf file and check that all the requested columns exist
      call fcpars (outfile, filename, extnumb, status)
      if (extnumb .eq. -99) extnumb = 1

      call ftopen (ounit, filename, 1, block, status)
      if (status .ne. 0) then
         context = ' Error opening mkf file ' // filename
         call fcerr (context)
         goto 999
      endif

C goto the requested extension
      call ftmrhd (ounit, extnumb, htype, status)
      if (status .ne. 0) then
         context = ' Error moving to requested mkf extension'
         call fcerr (context)
         goto 998
      endif

C get the column numbers based on instrument
      call ftgcno (ounit, sensecase, mkftime, mkftno, status)

      call ftupch (instrument)
      if (instrument .eq. 'SIS0') then
         call ftgcno (ounit, sensecase, s0mode, modcolno, status )
         call ftgcno (ounit, sensecase, s0c0, c0colno, status)
         call ftgcno (ounit, sensecase, s0c1, c1colno, status)
         call ftgcno (ounit, sensecase, s0c2, c2colno, status)
         call ftgcno (ounit, sensecase, s0c3, c3colno, status)
      else if (instrument .eq. 'SIS1') then
         call ftgcno (ounit, sensecase, s1mode, modcolno, status )
         call ftgcno (ounit, sensecase, s1c0, c0colno, status)
         call ftgcno (ounit, sensecase, s1c1, c1colno, status)
         call ftgcno (ounit, sensecase, s1c2, c2colno, status)
         call ftgcno (ounit, sensecase, s1c3, c3colno, status)
      else
         context = ' unknown instrument ' // instrument
         call fcerr (context)
         goto 998
      endif

      if (status .ne. 0) then
         context = ' Error finding columns in mkf file'
         call fcerr (context)
         goto 998
      endif

C how many rows are there?
      call ftgkyj (ounit, 'NAXIS2', nrows, context, status)
      if (status .ne. 0) then
         context = ' error determinine number of rows in mkf file'
         call fcerr (context)
         goto 998
      endif

C open and read the DFE file
      call realdfe (infile, dfetime, dfe, numdfe, status)

C read through the mkf file until the first dfe time is found
C    NOTE that both mkf and dfe files are assumed to be time ordered

      dferow = 1
      felem = 1
      nelem = 1
      do 100 mkfrow = 1, nrows
         call ftgcvd (ounit, mkftno, mkfrow, felem, nelem, 0.D0, time,
     &        anyf, status)
         call ftgcvj (ounit, modcolno, mkfrow, felem, nelem, 0, sismode,
     &        anyf, status)

 50      if ((time .ge. dfetime(dferow)) .and.
     &        (time .lt. dfetime(dferow+1)) .and.
     &        (sismode .eq. 0 )) then

C this time has a dfe associated with it
            call ftpcle (ounit, c0colno, mkfrow, felem, 1,
     &           dfe(1, dferow), status)

            call ftpcle (ounit, c1colno, mkfrow, felem, 1,
     &           dfe(2, dferow), status)

            call ftpcle (ounit, c2colno, mkfrow, felem, 1,
     &           dfe(3, dferow), status)

            call ftpcle (ounit, c3colno, mkfrow, felem, 1,
     &           dfe(4, dferow), status)
            if (status .ne. 0) then
               context = ' error writing DFE values to MKF file'
               call fcerr (context)
               goto 998
            endif
         else if (time .gt. dfetime(dferow)) then
            dferow = dferow + 1
            if (dferow .gt. numdfe) goto 200
            goto 50
         endif
 100  continue
 200  continue

C sucessful completion, put in history record
      call timestamp (ounit)

 998  if (status .ne. 0) call fcerrm (status)
      status = 0
      call ftclos (ounit, status)

 999  if (status .ne. 0) call fcerrm (status)
      return
      end


C******************************************************************************
C SUBROUTINE:
C       realdfe
C
C DESCRIPTION:
C       This routine reads in the dark frame error values
C
C AUTHOR/DATE:
C       Emily A. Greene  30 November, 1993
C       Hughes STX
C
C MODIFICATION HISTORY:
C       Modified from READDFE to have real output and only try to read file
C           EAG 9/2/94
C       Modified to exclude ! ZERODEF=n line produced by the new faintdfe
C           KM, Mar 1996
C
C NOTES:
C
C USAGE:
C       call realdfe (dfefile, dfetime, dfe, numdfe, status)
C
C ARGUMENTS:
C       dfefile - name of the file containing the dfe information
C       dfetime - returned array of times of dfe values
C       dfe     - returned array of dfe values for each chip
C       numdfe  - the number of dfetimes
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C
C******************************************************************************
      subroutine realdfe (dfefile, dfetime, dfe, numdfe, status)

      integer maxdfe
      parameter (maxdfe = 16384)

      character*(*) dfefile
      double precision dfetime(maxdfe)
      real dfe(4, maxdfe)
      integer numdfe, status

      real chip0, chip1, chip2, chip3
      integer iunit
      character(80) context, inline

      iunit = 18

 300  open (unit = iunit, file=dfefile, status='old', err=900)
      numdfe = 0

 310  continue
        read (iunit, '(a)', end = 800 ) inline
        if( inline( 1: 1 ) .eq. '!' ) then
*         This is a ZERODEF=? line produced by the new faintdfe
*         ignore
          continue
        else         
          numdfe = numdfe + 1
*         Slightly naughty --- free-format internal read is not
*         guaranteed to work on every machine/compiler
          read (inline, *, end=800) dfetime(numdfe),
     &     chip0, chip1, chip2, chip3
 1000     format (F7.0, 4(1x, F5.2))

          dfe(1, numdfe) = (chip0)
          dfe(2, numdfe) = (chip1)
          dfe(3, numdfe) = (chip2)
          dfe(4, numdfe) = (chip3)
        end if

      goto 310

 800  numdfe = numdfe - 1
      close (unit=iunit)
      return

 900  context = ' Error opening DFE input file' // dfefile
      call fcerr (context)
      status = 10

      return
      end



        
