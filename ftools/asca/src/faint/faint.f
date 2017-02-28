C  FTOOLs info $Header: /headas/headas/ftools/asca/src/faint/faint.f,v 3.16 2013/05/21 19:08:06 irby Exp $
C
C*****************************************************************************
C SELECTOR TASK:
C      faint
C
C FILE:
C      faint.f
C
C DESCRIPTION:
C       Routine to assign grades to faint mode data
C       Based on the CLASSIFY program
C
C AUTHOR/DATE:
C       Emily A. Greene   August, 1992
C       Hughes STX
C
C MODIFICATION HISTORY:
C $Log: faint.f,v $
C Revision 3.16  2013/05/21 19:08:06  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.15  2002/04/23 19:12:01  mukai
C Minor initialization bug-fixes, plus attempt at fixing a Linux crash for
C zero-length output file.
C
C Revision 3.14  2001/03/15 15:56:15  irby
C Modifications made by Koji Mukai.
C New echo calculation (using 4 new parameters in faint.par).
C
C Revision 3.14  2001/03/15 mukai
C Updated echo formula based on Dotani-san's revised analysis
C
C Revision 3.13  2000/09/11 20:09:06  peachey
C Corrected typo in modification history.
C
C Revision 3.12  1999/02/01 16:50:06  toliver
C increased size of wrtstr buffer to eliminate possibility of internal
C write overflow.
C
c Revision 3.11  1997/05/06  03:47:51  guerber
c Changed PI column units to "channel", removed units from GRADE column.
c
C Revision 3.10  1997/04/08 22:42:51  guerber
C Increased maxdfe to 10000, and report if it's exceeded.
C
C Revision 3.9  1997/02/24 22:23:39  guerber
C figfnt: Changed `exact' to .false. to make ftgcno calls case-insensitive.
C
C Revision 3.8  1996/11/22 08:40:00  guerber
C Bug in dfefix() let point run off end of the array if time exactly equaled
C dfetime(maxdfe).  Also, dfefix was being called with the full time and ccdid
C arrays instead of i-th elements.
C
C
C       3/3/93 (EAG) Added version selection, and version 4.0 of classify
C       3/4/93 (EAG) Added copyall parameter
C       6/11/93 (EAG) Add version 5.0 of classify, add echo, qfancy,
C                     maxgrade, history, copyprime and bright parameters
C       6/28/93 (EAG) Make echo use correct value based on instrument
C       8/16/93 (JKB) Update NEVENTS keyword in Primary Header with the
C                     NAXIS2 keyword value in the Events extension.
C       9/24/93 (EAG) Change order of output columns
C       10/7/93 (EAG) Deal with TLMIN and TLMAX keywords correctly
C       11/24/93 (EAG) Fix PI column from I to J
C       11/30/93 (EAG) Dark Frame Error correction
C       12/16/93 (EAG) Update various keywords for FRFREAD and ASCALIN changes
C       1/27/94 (EAG) 2.8a Update for more keywords, BRIGHT emulation change
C       12/2/94 (EAG) 3.3a Increase maxdfe (not easy to make infinite)
C                          Use smaller strings
C       12/12/94 (EAG) 3.3b Increase size of author to 16 characters
C       1/20/95  (EAG) 3.3c not declaring dfefile length in subroutines
C                           dfefile beginning with / were misinterpreted to 0.
C                           dfefile not read correctly on ALPHA & DECstation
C                                  remove ability to enter DFE value by hand
C       1/23/95  (EAG) 3.3d put back DFE by hand ability
C       1/25/95  (EAG) 3.3e use FCREATE parser to get DFE values
C       9/15/95  (Srilal) 3.4 SIS[0,1]echo keywords replaced by formulas.
C                             Also, the task version number is printed.
C       01/20/96  (Srilal) 3.5 ZERODEF keyword is read from new faintdfe output
C			       and written to FITS file.
C	02/28/96  (Srilal) 3.6 DFEfile is read for bright=no condition as
C			       well as for bright=yes & zerodef=2 condition.
C
C
C NOTES:
C       Based on the CLASSIFY program
C
C USAGE:
C      HOST: faint
C      IRAF: faint
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C       infile    - input FITS file and extension number
C       outfile   - output FITS file and extension number
C       split     - split threshold level
C       dfefile   - name of the dark frame error correction file
C       maxgrade  - maximum value of grade to output
C       echo      - value to the echo fraction
C       sis0[1,2,3,4,5]echo  - default echo values for SIS0
C       sis1[1,2,3,4,5]echo  - default echo values for SIS1
C       version   - version number of classify to use
C       qfancy    - whether the "fancy" correction should be made
C       bright    - whether to mimic ASCA on board BRIGHT calculation
C       columns   - list of 9 column names
C       phacol    - name of output pha column - hidden
C       gradecol  - name of output grade column - hidden
C       abovecol  - name of output above column - hidden (NONE=omitted)
C       timecol   - name of the input time column
C       idcol     - name of the input id column
C       copyall   - whether to copy all other extensions and information
C       copyprime - whether to copy only primary array
C       history   - whether to output history records
C       status    - status of operation
C
C CALLED ROUTINES:
C      subroutine gfaint - gets parameters from environment
C      subroutine figfnt - generates faint information and puts in FITS file
C
C******************************************************************************

      subroutine faint

      character(160) infile,outfile, dfefile
      character(80) columns, timecol, idcol
      character(80) phacol, gradecol, abovecol
c split threshold level
      integer split, maxgrade, status
      real version, echo
      real sis01echo, sis02echo, sis03echo, sis04echo, sis05echo
      real sis11echo, sis12echo, sis13echo, sis14echo, sis15echo
      logical copyall, qfancy, history, copyprime, bright

      character(40) taskname
      common /task/ taskname

      taskname = 'faint3.14'

      status = 0
      call ftcmsg

C get parameters from par file
      call gfaint (infile, outfile, split, dfefile, echo,
     &     maxgrade, version, qfancy, bright,
     &     columns, phacol, gradecol, abovecol, timecol, idcol,
     &     copyall, copyprime, history, status,
     &     sis01echo, sis02echo, sis03echo, sis04echo, sis05echo,
     &     sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)
      if (status .ne. 0) goto 999

C read in and display column names
      call figfnt (infile, outfile, split, dfefile, echo,
     &     maxgrade, version, qfancy, bright,
     &     columns, phacol, gradecol, abovecol, timecol, idcol,
     &     copyall, copyprime, history, status,
     &     sis01echo, sis02echo, sis03echo, sis04echo, sis05echo,
     &     sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)

 999  return
      end


C******************************************************************************
C SUBROUTINE:
C      gfaint
C
C DESCRIPTION:
C      gets parameters from the parameter file
C
C AUTHOR/DATE:
C       Emily A. Greene    August 1992
C       Hughes STX
C
C MODIFICATION HISTORY:
C       3/3/93 EAG - added version parameter
C       3/4/93 EAG - added copyall parameter
C       2001/3/15 mukai - added two more echo parameters per SIS.
C
C
C NOTES:
C       gfaint uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C       call gfaint (infile, outfile, split, dfefile, echo,
C              maxgrade, version, qfancy, bright,
C              columns, phacol, gradecol, abovecol, timecol, idcol,
C              copyall, copyprime, history, status,
C              sis01echo, sis02echo, sis03echo, sis04echo, sis05echo
C              sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)

C
C ARGUMENTS:
C
C       infile    - input FITS file and extension number
C       outfile   - output FITS file and extension number
C       split     - split threshold level
C       dfefile   - name of the dark frame error correction file
C       echo      - value to the echo fraction
C       sis0[1,2,3,4,5]echo  - default echo values for SIS0
C       sis1[1,2,3,4,5]echo  - default echo values for SIS1
C       maxgrade  - maximum value of grade to output
C       version   - version number of classify to use
C       qfancy    - whether the "fancy" correction should be made
C       bright    - whether to mimic ASCA on board BRIGHT calculation
C       columns   - list of 9 column names
C       phacol    - name of output pha column - hidden
C       gradecol  - name of output grade column - hidden
C       abovecol  - name of output above column - hidden (NONE=omitted)
C       timecol   - name of the input time column
C       idcol     - name of the input ccdid column
C       copyall   - whether to copy all other extensions and information
C       copyprime - whether to copy only primary array
C       history   - whether to output history records
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

      subroutine gfaint (infile, outfile, split, dfefile, echo,
     &     maxgrade, version, qfancy,
     &     bright, columns, phacol, gradecol, abovecol, timecol,
     &     idcol, copyall, copyprime, history, status,
     &     sis01echo, sis02echo, sis03echo, sis04echo, sis05echo,
     &     sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)

      character*(*) infile,outfile, dfefile, idcol
      character*(*) columns, phacol, gradecol, abovecol, timecol
      integer      split, maxgrade
      integer      status
      real version, echo
      real sis01echo, sis02echo, sis03echo, sis04echo, sis05echo
      real sis11echo, sis12echo, sis13echo, sis14echo, sis15echo
      logical copyall, qfancy, bright, copyprime, history

      character(80) context

C Get the name of the input FITS file

      call uclgst('infile',infile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get INFILE parameter'
         call fcerr(context)
         goto 999
      endif

C  Get the name of the output FITS file

      call uclgst('outfile',outfile,status)
      if ( status .ne. 0 ) then
         context = 'Could not get OUTFILE parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SPLIT threshold value

      call uclgsi('split',split,status)
      if ( status .ne. 0 ) then
         context = 'Could not get SPLIT parameter'
         call fcerr(context)
         goto 999
      endif

C  Get the name of the dark frame error ASCII file

      call uclgst('dfefile', dfefile, status)
      if ( status .ne. 0 ) then
         context = 'Could not get DFEFILE parameter'
         call fcerr(context)
         goto 999
      endif


C Get the value of MAXGRADE value

      call uclgsi ('maxgrade', maxgrade, status)
      if ( status .ne. 0 ) then
         context = 'Could not get MAXGRADE parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of ECHO fraction value

      call uclgsr('echo', echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS01 ECHO fraction value

      call uclgsr('sis01echo', sis01echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS01 ECHO parameter'
         call fcerr(context)
         goto 999
      endif


C Get the value of SIS02 ECHO fraction value

      call uclgsr('sis02echo', sis02echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS02 ECHO parameter'
         call fcerr(context)
         goto 999
      endif


C Get the value of SIS03 ECHO fraction value

      call uclgsr('sis03echo', sis03echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS03 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS04 ECHO fraction value

      call uclgsr('sis04echo', sis04echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS04 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS05 ECHO fraction value

      call uclgsr('sis05echo', sis05echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS05 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS11 ECHO fraction value

      call uclgsr('sis11echo', sis11echo, status)
      if ( status .ne. 0 ) then
         context = ' Could not get SIS11 ECHO parameter'
         call fcerr(context)
         goto 999
      endif


C Get the value of SIS12 ECHO fraction value

      call uclgsr('sis12echo', sis12echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS12 ECHO parameter'
         call fcerr(context)
         goto 999
      endif


C Get the value of SIS13 ECHO fraction value

      call uclgsr('sis13echo', sis13echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS13 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS14 ECHO fraction value

      call uclgsr('sis14echo', sis14echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS14 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of SIS15 ECHO fraction value

      call uclgsr('sis15echo', sis15echo, status)
      if ( status .ne. 0 ) then
         context = 'Could not get SIS15 ECHO parameter'
         call fcerr(context)
         goto 999
      endif

C Get the value of the version parameter - hidden
      call uclgsr ('version', version, status)
      if (status .ne. 0) then
         context = ' Could not get VERSION parameter'
         call fcerr (context)
         go to 999
      endif

C Get whether to do QFANCY correction calculation

      call uclgsb ('qfancy', qfancy, status)
      if (status .ne. 0) then
         context = ' Could not get QFANCY parameter'
         call fcerr (context)
         go to 999
      endif

C Get whether to mimic ASCA on board BRIGHT calculation

      call uclgsb ('bright', bright, status)
      if (status .ne. 0) then
         context = ' Could not get BRIGHT parameter'
         call fcerr (context)
         go to 999
      endif

C Get the input column names - hidden parameter

      call uclgst ('columns', columns, status)
      if ( status .ne. 0 ) then
         context = ' Could not get COLUMNS parameter'
         call fcerr (context)
         go to 999
      endif
      if ((columns .eq. ' ') .or. (columns .eq. '-'))
     &     columns = 'pha0 pha1 pha2 pha3 pha4 pha5 pha6 pha7 pha8'

C Get the name of the output PHA columns - hidden parameter

      call uclgst ('phacol', phacol, status)
      if (status .ne. 0) then
         context = ' Could not get PHACOL parameter'
         call fcerr (context)
         go to 999
      endif

C Get the name of the ouput GRADE column - hidden parameter

      call uclgst ('gradecol', gradecol, status)
      if (status .ne. 0) then
         context = ' Could not get GRADECOL parameter'
         call fcerr (context)
         go to 999
      endif

C Get the name of the output ABOVE column - hidden parameter

      call uclgst ('abovecol', abovecol, status)
      if (status .ne. 0) then
         context = ' Could not get ABOVECOL parameter'
         call fcerr (context)
         go to 999
      endif

C Get the name of the input TIME column - hidden parameter

      call uclgst ('timecol', timecol, status)
      if (status .ne. 0) then
         context = ' Could not get TIMECOL parameter'
         call fcerr (context)
         go to 999
      endif

C Get the name of the input ccdid column - hidden parameter

      call uclgst ('idcol', idcol, status)
      if (status .ne. 0) then
         context = ' Could not get IDCOL parameter'
         call fcerr (context)
         go to 999
      endif

C Get whether to COPYALL other extensions and the primary array

      call uclgsb ('copyall', copyall, status)
      if (status .ne. 0) then
         context = ' Could not get COPYALL parameter'
         call fcerr (context)
         go to 999
      endif

C Get whether to COPYPRIME array

      call uclgsb ('copyprime', copyprime, status)
      if (status .ne. 0) then
         context = ' Could not get COPYPRIME parameter'
         call fcerr (context)
         go to 999
      endif

C Get whether to add HISTORY records

      call uclgsb ('history', history, status)
      if (status .ne. 0) then
         context = ' Could not get HISTORY parameter'
         call fcerr (context)
         go to 999
      endif

 999  continue
      if (status .ne. 0) call fcerrm(status)
      return
      end


C******************************************************************************
C SUBROUTINE:
C       figfnt
C
C DESCRIPTION:
C       This routine reads in the input FITS file and passes each record
C       to CLASSIFY, the routine which does the actual grade assignments
C       to the faint mode data.
C
C AUTHOR/DATE:
C       Emily A. Greene  July, 1992
C       Hughes STX
C
C MODIFICATION HISTORY:
C       3/3/93 EAG - add different version of classify capability
C       3/4/93 EAG - add copyall parameter
C       11/22/1996 Jeff Guerber - Dfefix() was called with full time, ccdid
C                  arrays instead of particular elements; fixed.
C       2/24/1997 Jeff Guerber - Changed exact to .false. so ftgcno calls
C                  will be case-insensitive.
C       4/8/1997 Jeff Guerber - Increased maxdfe & added to readdfe arg list
C       5/5/1997 Jeff Guerber - Fixed units for PI and GRADE columns
C       2001/3/15 Koji Mukai - Updated the echo formula to double exponential
C       2002/4/23 Koji Mukai - Proper initialization of TLMIN1/TLMAX1
C                            - Ditto for ZERODEF
C                            - Linux bugfix attempt (remove FTDDEF)
C
C NOTES:
C
C USAGE:
C       call figfnt (infile, outfile, split, dfefile, echo,
C              maxgrade, version, qfancy, bright,
C              columns, phacol, gradecol, abovecol, timecol, idcol,
C              copyall, copyprime, history, status,
C              sis01echo, sis02echo, sis03echo, sis04echo, sis05echo
C              sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)
C
C ARGUMENTS:
C
C       infile    - input FITS file and extension number
C       outfile   - output FITS file and extension number
C       split     - split threshold level
C       dfefile   - file containing dark frame error information
C       echo      - value to the echo fraction
C       sis0[1,2,3,4,5]echo  - default echo values for SIS0
C       sis1[1,2,3,4,5]echo  - default echo values for SIS1
C       maxgrade  - maximum value of grade to output
C       version   - version number of classify to use
C       qfancy    - whether the "fancy" correction should be made
C       bright    - whether to mimic ASCA on board BRIGHT calculation
C       columns   - list of 9 column names
C       phacol    - name of output pha column - hidden
C       gradecol  - name of output grade column - hidden
C       abovecol  - name of output above column - hidden (NONE=omitted)
C       timecol   - name of the input time column
C       idcol     - name of the input id column
C       copyall   - whether to copy all other extensions and information
C       copyprime - whether to copy only primary array
C       history   - whether to output history records
C       status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C       handle   - array of phulse height values
C       sumph    - total pulse height value
C       grade     - grade value
C       above    - number of pixels summed in sumph
C       filename - input FITS filename
C       iunit    - input unit number
C       extnumb  - input file extension number
C       context  - error message
C       block    - block size of the input file
C
C CALLED ROUTINES:
C       subroutine fcpars - parse infile into a filename and extension
C       subroutine fimcol - copies selected columns from the input to output
C       the FITSIO library - all subroutines beginning with ftxxxx
C
C******************************************************************************

      subroutine figfnt (infile, outfile, split, dfefile, echo,
     &     maxgrade, version, qfancy, bright,
     &     columns, phacol, gradecol, abovecol, timecol, idcol,
     &     copyall, copyprime, history, status,
     &     sis01echo, sis02echo, sis03echo, sis04echo, sis05echo,
     &     sis11echo, sis12echo, sis13echo, sis14echo, sis15echo)

      integer maxcl, maxsize, maxdfe

      parameter (maxcl = 22)

C NOTE use maxsize exactly divisible by 9
      parameter (maxsize = 9)
      parameter (maxdfe = 10000)

      character*(*) infile, outfile
      integer split, maxgrade
      character*(*) columns, phacol, gradecol, abovecol
      character*(*) timecol, idcol, dfefile
      real version, echo
      real sis01echo, sis02echo, sis03echo
      real sis11echo, sis12echo, sis13echo
      logical copyall, copyprime, history, bright, qfancy

      integer handle(9)
      integer sumph(maxsize)
      integer grade(maxsize)
      integer above(maxsize)
      integer flen, fcstln
      character(160) filename
      character(80)  extname
      character(80) context
      integer status, hdutype , newfields, fstatus, count
      integer iunit, ounit, extnumb, block, incol(maxcl)
      logical anyf, check, exact, ifdfe
      logical goodlist, negflag, simple, extend, ifabove
      integer tbcol(maxcl), rowlen, nrows, nelem, tfields, varidat
      character(16) ttype(maxcl), tform(maxcl)
      character(25) tunit(maxcl)
      character(40) colist(9)
      character(16) nform(maxcl), ntype(maxcl)
      character(40) nunit(maxcl)
      character(8) keyword, keyval
      character(16) author
      character(1) instrument
      integer irow, orow, felem, values(maxsize, 9), colnum(9)
      integer bitpix, naxis, naxes, pcount, gcount, numcols, remain
      integer i,j, repeat, dtype, width, igood, pcolno, gcolno
      integer nfound, itlmin(maxcl), itlmax(maxcl), otlmin(maxcl)
      integer otlmax(maxcl), phabin, ccdid(maxsize)
      double precision time(maxsize), dfetime(maxdfe)
      integer dfe(4, maxdfe), tcolno, idcolno, numdfe,zerodef
      real mjd
      real*8 tstart, tstop
      real  avrgt
      character(40) taskname
      common /task/ taskname

C Initialize variable
      iunit = 15
      ounit = 16
      status = 0
      check = .false.
      exact = .false.
      bitpix = 16
      simple = .true.
      naxis = 0
      naxes = 0
      pcount = 0
      gcount = 1
      extend = .true.
      if (maxgrade .gt. 7) maxgrade = 7

      ifabove = .false.
      if ((abovecol .ne. 'NONE') .and. (abovecol .ne. ' ') .and.
     &     (abovecol .ne. '-')) ifabove = .true.

C check for reasonable version number
      if ((version .ne. 3.1) .and. (version .ne. 4.0) .and.
     &     (version .ne. 5.0)) then
         context = ' Unknown version number'
         call fcerr (context)
         goto 1000
      endif

C DFEfile is read if bright=no or (bright=yes & zerodef=2) (Srilal)
C read in the dark frame error information

c     Initialize zerodef to -99 before we start --- KM, 2002 Apr
      zerodef = -99
      if ((dfefile .eq. ' ') .or. (dfefile .eq. '-') ) then
         ifdfe = .false.
      else
         call readdfe (dfefile,dfetime,dfe,numdfe,maxdfe,zerodef,status)
c MJT 09July96 (g77/linux) .eq.->.eqv., .ne.->.neqv.
       if ((bright .eqv. .false.) .or.
     & ((bright .eqv. .true.) .and. zerodef .eq. 2) ) then
         ifdfe = .true.
	 else
	 ifdfe = .false.
       endif
      endif

      if (status .ne. 0) goto 1000

C Parse INFILE into a filename and extension number
      call fcpars (infile, filename, extnumb, status)

C EAG 8/25/93 default to 1st extension
      if (extnumb .eq. -99) extnumb = 1

C If the extension is 0 then give error and exit
      if (extnumb .le. 0) then
         context = 'Primary array (Ext# = 0) is not a table'
         call fcerr (context)
         goto 1000
      endif

C Open existing input FITS file
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Unable to open INFILE'
         call fcerr (context)
         goto 1000
      endif

C check for request to mimic ASCA on board processing
      if (bright) then
         echo = 0.
         qfancy = .false.
C have to check date for change in maxgrade
         call ftgkye (iunit, 'MJD-OBS', mjd, context, status)
C on board bright emulation was changed Nov 30, 1993 at 7:30 UT
         if (mjd .lt. 49322.3125) then
            maxgrade = 4
         else
            maxgrade = 6
         endif
      endif

C Create output FITS file
      call ftinit (ounit, outfile, 0, status)
      if (status .gt. 0) then
         context = ' unable to create output FITS files'
         call fcerr(context)
         goto 999
      endif

C copy the primary array, if requested
      if ((copyall) .or. (copyprime)) then
         call ftcopy (iunit, ounit, 4, status)
      else

C create the primary header
         simple = .true.
         naxis = 0
         pcount = 0
         gcount = 1

C write default null primary array keywords
         call ftphpr (ounit, simple, bitpix, naxis, naxes, pcount,
     &        gcount, extend, status)
         if (status .gt. 0) then
            context = ' unable to write primary header keywords'
            call fcerr(context)
            go to 998
         endif

C define the null primary array structure
         call ftpdef (ounit, bitpix, naxis, naxes, pcount, gcount,
     &        status)
         if (status .gt. 0) then
            context = ' unable to define primary array structure'
            call fcerr(context)
            go to 998
         endif

C copy all other keywords from input to output files
         call xcopynoscale (iunit, ounit, status)

         if (status .gt. 0) then
            context = ' problem writing addition keywords'
            call fcerr (context)
            goto 998
         endif

      endif

C Get TSTART and TSTOP values to calculate AVRGT

      call ftgkyd (iunit, 'TSTART', tstart, context, status)
         if (status .ne. 0) then
            context = ' Could not get TSTART parameter'
            call fcerr (context)
            goto 998
         endif
      call ftgkyd (iunit, 'TSTOP', tstop, context, status)
         if (status .ne. 0) then
            context = ' Could not get TSTOP parameter'
            call fcerr (context)
            goto 998
         endif

          avrgt = (tstart + tstop)/2


C check for request to calculate echo values
      call ftgkys (iunit, 'INSTRUME', keyword, context, status)
      if (keyword .eq. 'SIS0') then
         if (echo .lt. 0) then
c (old)	 echo = sis01echo - sis02echo * exp(-avrgt/sis03echo)
c               1.8401%     0.81934%               1.954e7
	 echo = sis01echo - sis02echo * exp(-avrgt/sis03echo)
c (new)         2.0403%     2.5281%                3.9234e6
     &                    - sis04echo * exp(-avrgt/sis05echo)
c                           0.4907%                4.2030e7
	 endif
         instrument = '0'
      else if (keyword .eq. 'SIS1') then
         if (echo .lt. 0) then
c (old)	 echo = sis11echo - sis12echo * exp(-avrgt/sis13echo)
c               1.1135%     0.42187%               1.0695e7
 	 echo = sis11echo - sis12echo * exp(-avrgt/sis13echo)
c (new)         1.2493%     0.4091%                3.9234e6 (fixed)
     &                    - sis14echo * exp(-avrgt/sis15echo)
c                           0.4367%                5.1434e7
	 endif
         instrument = '1'
      else
         context = ' Could not determine instrument:' // keyword
         call fcerr (context)
         status = 1
         goto 998
      endif

C get the author keyword
      call ftgkys (iunit, 'AUTHOR', author, context, status)
      if (status .eq. 202) then
         status = 0
         call ftgkys (iunit, 'CREATOR', author, context, status)
         if (status .ne. 0) then
            context = ' Could not determine AUTHOR/CREATOR keyword'
            call fcerr (context)
            goto 998
         endif
      endif

C add SPLIT_TH and DATAMODE keywords to header
      keyword = 'SPLIT_TH'
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif

C add the FRF read style split keywords
      keyword = 'S' // instrument(1:1) // '_SPTR0'
CEAG    keyword = 'SPLTH0S' // instrument
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif

      keyword = 'S' // instrument(1:1) // '_SPTR1'
CEAG    keyword = 'SPLTH1S' // instrument
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif
      keyword = 'S' // instrument(1:1) // '_SPTR2'
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif
      keyword = 'S' // instrument(1:1) // '_SPTR3'
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif

C write the S?_GRADE keyword to the primary header
      keyword = 'S' // instrument(1:1) // '_GRADE'
      if (maxgrade .eq. 0) then
         keyval = '00000001'
      else if (maxgrade .eq. 1) then
         keyval = '00000011'
      else if (maxgrade .eq. 2) then
         keyval = '00000111'
      else if (maxgrade .eq. 3) then
         keyval = '00001111'
      else if (maxgrade .eq. 4) then
         keyval = '00011111'
      else if (maxgrade .eq. 5) then
         keyval = '00111111'
      else if (maxgrade .eq. 6) then
         keyval = '01111111'
      else
         keyval = '11111111'
      endif
      context = '&'
      call ftmkys (ounit, keyword, keyval, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'grade discrimination level'
         call ftpkys (ounit, keyword, keyval, context, status)
      endif

      keyword = 'DATAMODE'
      if (bright) then
         keyval = 'BRIGHT'
      else
         keyval = 'BRIGHT2'
      endif
      context = 'file generated by FAINT'
      call ftmkys (ounit, keyword, keyval, context, status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ounit, keyword, keyval, context, status)
      endif

C Write the Version number into the HISTORY keyword (9/18/95 Srilal)
        flen = fcstln(filename)
      context = 'TASK: '//taskname(1:12)//
     &     ' on FILENAME: '//filename(1:flen)
      if (history) then
      call ftphis(ounit,context,status)
      call timestamp(ounit)
      endif


C Write ZERODEF keyword (01/18/96 Srilal)
c     Only if zerodef value was read in from faintdfe output - KM, 2002 Apr
      if (zerodef .ne. -99) then
        context = 'Zerodef value from new faintdfe output'
        keyword = 'ZERODEF'
        call ftpkyj (ounit, keyword, zerodef, context, status)
      end if

      write (context, 1111) version
 1111 format (' Classify version number: ',f6.1)
      if (history) call ftphis (ounit, context, status)

      write (context, 1112) echo
 1112 format (' Echo fraction used: ',1pG15.8)
      if ((version .eq. 5.0) .and. (history))
     &     call ftphis (ounit, context, status)

      write (context, 1113) maxgrade
 1113 format (' Maximum grade output: ', i3)
      if (history) call ftphis (ounit, context, status)

      if ((history) .and. (dfefile .ne. ' ')
     &     .and. (dfefile .ne. '-')) then
         context = ' DFE corrected using ' // dfefile
         call ftphis (ounit, context, status)
      endif

      if ((history) .and. (bright)) then
         context = ' Output PHA channels mapped to 0 - 2047'
         call ftphis (ounit, context, status)
      endif


C copy any extensions before the extension to be operated on
      if ((copyall) .and. (extnumb .gt. 1)) then
         do 100 i=1, extnumb-1
            call ftmrhd (iunit, 1, hdutype, status)
            call ftcrhd (ounit, status)
            call ftcopy (iunit, ounit, 0, status)
 100     continue
         if (status .ne. 0) then
            context = ' error copying extensions'
            call fcerr (context)
            goto 998
         endif
      endif

C make sure get specified header by using absolute location

      call ftmahd (iunit, extnumb+1, hdutype, status)
      if (status .ne. 0) then
         context = ' Unable to move to requested extension'
         call fcerr (context)
         goto 998
      endif

C get the header depending on the extension type
      if (hdutype .eq. 1) then
         call ftghtb (iunit, maxcl, rowlen, nrows, tfields, ttype,
     &        tbcol, tform, tunit, extname, status)
      else if (hdutype .eq. 2) then
         call ftghbn (iunit, maxcl, nrows, tfields, ttype, tform,
     &        tunit, extname, varidat, status)
      else
         context = ' HDU for extension not supported'
         call fcerr(context)
         go to 998
      endif

C create a new extension
      call ftcrhd (ounit, status)
      if (status .gt. 0) then
         context = ' unable to create new FITS extension'
         call fcerr (context)
         goto 998
      endif

C seperate the list of column names
      negflag = .false.
      call fcgcls (columns, colist, numcols, negflag)
      if ((numcols .ne. 9) .and.
     &     ((numcols .ne. 1) .or. (hdutype .ne. 2))) then
         context = ' Number of columns requested is not reasonable'
         call fcerr (context)
         goto 998
      endif

C compare with all columns to check that all exist
      call fccmpl (numcols, tfields, colist, ttype, negflag, goodlist)
      if (.not. goodlist) then
         status = 0
         context = ' Not all requested columns exist'
         call fcerr (context)
         go to 998
      endif

C set up the columns for the output file based on FRFREAD version
C 2.995 output columns:
C         FAINT   column number   BRIGHT
C       X       I       1       X       I
C       Y       I       2       Y       I
C       RAWX    I       3       RAWX    I
C       RAWY    I       4       RAWY    I
C       TIME    D       5       TIME    D
C       PHAS    9I      6       PHA     I
C       DETX    I       7       DETX    I
C       DETY    I       8       DETY    I
C       CCDID   B       9       GRADE   B
C                       10      CCDID   B
C
C get the TLMIN/TLMAX keywords, if any
      call ftgknj (iunit, 'TLMIN', 1, tfields, itlmin, nfound, status)
      call ftgknj (iunit, 'TLMAX', 1, tfields, itlmax, nfound, status)
      if (status .ne. 0) then
         context = ' problem reading TLMIN/TLMAX keywords'
         call fcerr (context)
         goto 998
      endif
      if (author(9:) .le. '2.995') then
         do 175 i = 1, 5
            nform(i) = tform(i)
            ntype(i) = ttype(i)
            nunit(i) = tunit(i)
            incol(i) = i
            otlmin(i) = itlmin(i)
            otlmax(i) = itlmax(i)
 175     continue
         ntype(6) = phacol
         nform(6) = '1I'
         nunit(6) = 'pha'
         incol(6) = 0
         pcolno = 6
         otlmin(6) = 0
         otlmax(6) = 4095
         if (bright) otlmax(6) = 2047
         do 176 i = 7,8
            nform(i) = tform(i)
            ntype(i) = ttype(i)
            nunit(i) = tunit(i)
            incol(i) = i
            otlmin(i) = itlmin(i)
            otlmax(i) = itlmax(i)
 176     continue
         ntype(9) = gradecol
         nform(9) = '1B'
         nunit(9) = 'NONE'
         incol(9) = 0
         gcolno = 9
         otlmin(i) = 0
         otlmax(i) = maxgrade
         ntype(10) = ttype(9)
         nform(10) = tform(9)
         nunit(10) = tunit(9)
         incol(10) = 9
         otlmin(10) = itlmin(9)
         otlmax(10) = itlmax(9)
         newfields = 10
      else
C FRFREAD version 2.997 and above
C
C 11/24/93: Some version between 2.997 and 3.000, the PI column was
C       changed from I to J.  Since very little data should exist
C       for this intermediate version of FRFREAD, it will no longer
C       be supported (EAG)
C
C 2.997 output columns:
C         FAINT   column number   BRIGHT
C       TIME    D       5       TIME    D
C       X       I       1       PI      J
C       Y       I       2       X       I
C       RAWX    I       3       Y       I
C       RAWY    I       4       RAWX    I
C       PHAS    9I      6       RAWY    I
C       DETX    I       7       PHA     I
C       DETY    I       8       DETX    I
C       CCDID   I       9       DETY    I
C                       10      GRADE   I
C                       11      CCDID   I
C
         nform(1) = tform(1)
         ntype(1) = ttype(1)
         nunit(1) = tunit(1)
         incol(1) = 1
c         otlmin(1) = itlmin(1)
c         otlmax(1) = itlmax(1)
c         These are the min/max of TIME column: itlmin/max(1) are undefined
c         so changed initialization to 0, which results in these keywords not
c         being written in the output file, which is what we want.
c                                     KM, 2002 April
         otlmin(1) = 0
         otlmax(1) = 0
         nform(2) = '1J'
         ntype(2) = 'PI'
         nunit(2) = 'channel'
         incol(2) = 0
         otlmin(2) = 0
         otlmax(2) = 0
         do 177 i = 2, 5
            nform(i+1) = tform(i)
            ntype(i+1) = ttype(i)
            nunit(i+1) = tunit(i)
            incol(i+1) = i
            otlmin(i+1) = itlmin(i)
            otlmax(i+1) = itlmax(i)
 177     continue
         ntype(7) = phacol
         nform(7) = '1I'
         nunit(7) = 'channel'
         incol(7) = 0
         pcolno = 7
         otlmin(7) = 0
         otlmax(7) = itlmax(6)
         if (bright) otlmax(7) = 2047
         do 178 i = 7, 8
            nform(i+1) = tform(i)
            ntype(i+1) = ttype(i)
            nunit(i+1) = tunit(i)
            incol(i+1) = i
            otlmin(i+1) = itlmin(i)
            otlmax(i+1) = itlmax(i)
 178     continue
         ntype(10) = gradecol
         nform(10) = '1I'
         nunit(10) = ' '
         incol(10) = 0
         gcolno = 10
         otlmin(10) = 0
         otlmax(10) = maxgrade
         ntype(11) = ttype(9)
         nform(11) = tform(9)
         nunit(11) = tunit(9)
         incol(11) = 9
         newfields = 11
         otlmin(11) = itlmin(9)
         otlmax(11) = itlmax(9)
      endif

      if (ifabove) then
         newfields = newfields + 1
         ntype(newfields) = abovecol
         nform(newfields) = '1I'
         tunit(newfields) = 'pixels'
         incol(newfields) = 0
      endif

      if (hdutype .eq. 1) then
         call ftgabc(newfields, nform, 1, rowlen, tbcol, status)
         call ftphtb(ounit, rowlen, 0, newfields, ntype, tbcol,
     &        nform, nunit, extname, status)
      else
         call ftphbn (ounit, 0, newfields, ntype, nform, nunit,
     &        extname, varidat, status)
      endif

C copy all additional keywords
      call xcopynoscale (iunit, ounit, status)

C add SPLIT_TH and DATAMODE keywords to header
      keyword = 'SPLIT_TH'
      context = '&'
      call ftmkyj (ounit, keyword, split, context, status)
      if (status .eq. 202) then
         status = 0
         context = 'split threshold level'
         call ftpkyj (ounit, keyword, split, context, status)
      endif

      keyword = 'DATAMODE'
      if (bright) then
         keyval = 'BRIGHT'
      else
         keyval = 'BRIGHT2'
      endif
      context = 'file generated by FAINT'
      call ftmkys (ounit, keyword, keyval, context, status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ounit, keyword, keyval, context, status)
      endif

C write the TLMIN/TLMAX keywords only if tlmax is non-zero
      do 180 i = 1, newfields
         if (otlmax(i) .gt. 0) then
            call ftkeyn ('TLMIN', i, keyword, status)
            call ftpkyj (ounit, keyword, otlmin(i), ' ', status)
            call ftkeyn ('TLMAX', i, keyword, status)
            call ftpkyj (ounit, keyword, otlmax(i), ' ', status)
         endif
 180  continue
      if (status .ne. 0) then
         context = ' Error writing TLMIN/TLMAX keywords'
         call fcerr (context)
         goto 998
      endif

C modify the keywords written by ASCALIN, if they exist
      call copyasca (iunit, ounit, author, status)
      if (status .ne. 0) then
         context = ' Error copying ASCALIN imaging keywords'
         call fcerr(context)
         goto 998
      endif

C if PHA_BINS exists, update it
      call ftgkyj (ounit, 'PHA_BINS', phabin, context, status)
      if (status .eq. 0) then
         if (author(9:) .le. '2.995') then
            phabin = otlmax(6)+1
         else
            phabin = otlmax(7)+1
         endif
         call ftmkyj (ounit, 'PHA_BINS', phabin, context, status)
      endif
      status = 0

      write (context, 1111) version
      if (history) call ftphis (ounit, context, status)

      write (context, 1112) echo
      if ((version .eq. 5.0) .and. (history))
     &     call ftphis (ounit, context, status)

      write (context, 1113) maxgrade
      if (history) call ftphis (ounit, context, status)

      if ((history) .and. (dfefile .ne. ' ')
     &     .and. (dfefile .ne. '-')) then
         context = ' DFE corrected using ' // dfefile
         call ftphis (ounit, context, status)
      endif

      if ((history) .and. (bright)) then
         context = ' Output PHA channels mapped to 0 - 2047'
         call ftphis (ounit, context, status)
      endif

C define the extension data structure
      if (hdutype .eq. 1) then
         call ftadef(ounit, rowlen, newfields, tbcol, nform, 0,
     &        status)
      else
         call ftbdef (ounit, newfields, nform, varidat,
     &        0, status)
      endif

C find the column number of each pha column
      do 200 i = 1, numcols
         call ftgcno (iunit, exact, colist(i), colnum(i), status)
 200  continue

C find the time column
      call ftgcno (iunit, exact, timecol, tcolno, status)
      call ftgcno (iunit, exact, idcol, idcolno, status)
      if (status .ne. 0) then
         context = ' problem encountered finding column location'
         call fcerr (context)
         goto 998
      endif

C send the data through classify, maxsize rows at a time

      orow = 1
      irow = 1
      felem = 1
      if (hdutype .eq. 1) then
         repeat = 1
      else
         call ftbnfm (tform(colnum(1)), dtype, repeat, width, status)
      endif
      remain = nrows

 205  if (remain .gt. 0) then
         if (remain .ge. maxsize) then
            nelem = maxsize * repeat
         else
            nelem = remain * repeat
         endif
         anyf = .false.

C get the data
         do 210 i = 1, numcols
            call ftgcvj (iunit, colnum(i), irow, felem, nelem, 0,
     &           values(1, i), anyf, status)
 210     continue

C get the time and ccdid for dark frame error calculation
         call ftgcvd (iunit, tcolno, irow, felem, nelem/repeat, 0.D0,
     &        time, anyf, status)
         call ftgcvj (iunit, idcolno, irow, felem, nelem/repeat, 0,
     &        ccdid, anyf, status)

C send it through classify
         do 220 i = 1, nelem/repeat

            do 215 j = 1, 9
               if (repeat .ne. 1) then
                  handle(j) = values((i-1)*9+j, 1)
               else
                  handle(j) = values(i, j)
               endif
 215        continue

C correct for dark frame error
            if (ifdfe) call dfefix (handle, time(i), ccdid(i), dfetime,
     &           dfe, numdfe, status)

            if (version .eq. 3.1) then
               call faint_v31(handle,split,sumph(i),grade(i),above(i))
            else if (version .eq. 4.0) then
c version 4.0
               call faint_v40(handle,split,sumph(i),grade(i),above(i))
            else
c version 5.0
               call faint_v50 (handle, split, echo, qfancy, sumph(i),
     &              grade(i),above(i))
            endif
 220     continue

C check for maximum grade to output
         count = 0
         do 250 i = 1, nelem/repeat
            if (grade(i) .le. maxgrade) then
               count = count + 1
            else if (count .gt. 0) then

               igood = i - count
C to mimic ASCA on board, must re-bin sumph values
               if (bright) call faintf (sumph(igood), count)

C write out the column information
               call ftpclj (ounit, pcolno, orow, felem, count,
     &              sumph(igood), status)
               call ftpclj (ounit, gcolno, orow, felem, count,
     &              grade(igood), status)
               if (ifabove) call ftpclj (ounit, newfields, orow, felem,
     &              count, above(igood), status)

C move all of the other columns to the new file
               call fimcol (iunit, ounit, irow, orow, newfields, count,
     &              nform, incol, hdutype, status)

               count = 0
               irow = irow + 1
            else
               irow = irow + 1
            endif
 250     continue

C and output the last few values
         if (count .gt. 0) then
C to mimic ASCA on board, must re-bin sumph values
            igood = nelem/repeat - count + 1
            if (bright) call faintf (sumph(igood), count)

C write out the column information
            call ftpclj (ounit, pcolno, orow, felem, count,
     &           sumph(igood), status)
            call ftpclj (ounit, gcolno, orow, felem, count,
     &           grade(igood), status)
            if (ifabove) call ftpclj (ounit, newfields, orow, felem,
     &           count, above(igood), status)

C move all of the other columns to the new file
            call fimcol (iunit, ounit, irow, orow, newfields, count,
     &           nform, incol, hdutype, status)

         endif

C  Update the pointers in the column
C note that fimcol updates irow and orow properly

         remain = remain - nelem/repeat
         if (remain .gt. 0) goto 205

      endif

C redefine the size of the extension
      orow = orow - 1

c     FTDDEF may be causing the Linux cras, and is unncessary anyway
c     according to the FTOOLS team - try deleting them.  KM, 2002 April
c      call ftgkyj (ounit, 'NAXIS1', width, context, status)
c      call ftddef (ounit, width*orow, status)
      call ftmkyj (ounit, 'NAXIS2', orow, '&', status)

      if (status .ne. 0) then
         call fcerrm(status)
         status = 0
      endif

C copy any extensions after the extension to be operated on

      if (.not. copyall) goto 997
 300  call ftmrhd (iunit, 1, hdutype, status)
      call ftcrhd (ounit, status)
      call ftcopy (iunit, ounit, 0, status)

C update the DATAMODE keyword in all extensions
      keyword = 'DATAMODE'
      if (bright) then
         keyval = 'BRIGHT'
      else
         keyval = 'BRIGHT2'
      endif
      context = 'file generated by FAINT'
      call ftmkys (ounit, keyword, keyval, context, status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ounit, keyword, keyval, context, status)
      endif

      if (status .eq. 0) goto 300
      status = 0

C Update the NEVENTS keyword in the primary header in output
 997  call ftmahd(ounit,1,hdutype,status)
      call ftmkyj(ounit,'NEVENTS',orow,'&',status)
      if (status .ne. 0) then
         call fcerrm(status)
         status = 0
      endif


C End of routine stuff
C close output file
 998  fstatus = 0
      call ftclos (ounit, fstatus)
C  close input file
 999  fstatus = 0
      call ftclos (iunit, fstatus)

 1000 if (status .ne. 0) call fcerrm(status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C       faintf
C
C DESCRIPTION:
C       This routine compresses the PHA values from 0 - 4095 to 0 - 2047
C       to mimic the on board data compression
C
C AUTHOR/DATE:
C       Emily A. Greene  14 June, 1993
C       Hughes STX
C
C MODIFICATION HISTORY:
C
C NOTES:
C       The new channels map onto the original 4096 by :
C
C                0 - 1023    ->    0 - 1023
C             1024 - 2047    -> 1024 - 1534
C             2048 - 4095    -> 1536 - 2047
C
C USAGE:
C       call faintf (sumph, nelem)
C
C ARGUMENTS:
C
C       sumph - the array of PHA values to change
C       nelem - the number of array elements
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine faintf (sumph, nelem)

      integer nelem
      integer sumph(nelem), i

      do 100 i = 1, nelem
         if (sumph(i) .ge. 2048) then
            sumph(i) = 1536 + (sumph(i) - 2048)/4
         else if (sumph(i) .ge. 1025) then
            sumph(i) = 1024 + (sumph(i) - 1024)/2
         endif
 100  continue

      return
      end

C******************************************************************************
C SUBROUTINE:
C       dfefix
C
C DESCRIPTION:
C       This routine corrects for dark frame error
C
C AUTHOR/DATE:
C       Emily A. Greene  30 November, 1993
C       Hughes STX
C
C MODIFICATION HISTORY:
C     Jeff Guerber, HSTX, Nov 22, 1996.  If time = dfetime(maxdfe) *exactly*,
C     point ran off end of array: changed "time .gt. dfetime(point)" to ".ge."
C     Also added explicit save for point.
C
C NOTES:
C
C USAGE:
C       call dfefix (handle, time, ccdid, dfetime, dfe, maxdfe, status)
C
C ARGUMENTS:
C       handle  - the 9 PHA values to correct
C       time    - the time of the event
C       ccdid   - the CCD ID of the event
C       dfetime - array of times of dark frame error corrections
C       dfe     - dark frame error corrections
C       maxdfe  - number of times of dfe values
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C
C******************************************************************************
      subroutine dfefix (handle, time, ccdid, dfetime, dfe, maxdfe,
     &     status)

      integer handle(9), ccdid, status, maxdfe
      integer dfe (4, maxdfe)
      double precision time, dfetime(maxdfe)
      integer point, i
      data point /1/
      save point

C get the correct dfe for the given time:
C dfetime(point) <= time < dfetime(point+1)
C (note: if true, we just fall through these if's)

 10   if (time .lt. dfetime(point)) then
         if (point .eq. 1) goto 50
         point = point - 1
         goto 10
      else if ((time .ge. dfetime(point)) .and.
     &        (point .eq. maxdfe)) then
         goto 50
      else if (time .ge. dfetime(point+1)) then
         point = point + 1
         goto 10
      endif

C if the times match up
 50   do 100 i = 1, 9
         handle(i) = handle(i) - dfe(ccdid+1, point)
 100  continue

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C       readdfe
C
C DESCRIPTION:
C       This routine reads in the dark frame error values
C
C AUTHOR/DATE:
C       Emily A. Greene  30 November, 1993
C       Hughes STX
C
C MODIFICATION HISTORY:
C
C       Modified to read Zerodef value from  new faintdfe output (Srilal)
C       April 1997 (guerber) Added maxdfe as an arg, and error if exceeded
C
C NOTES:
C
C USAGE:
C       call readdfe (dfefile, dfetime, dfe, numdfe, maxdfe, zerodef, status)
C
C ARGUMENTS:
C       dfefile - name of the file containing the dfe information
C       dfetime - returned array of times of dfe values
C       dfe     - returned array of dfe values for each chip
C       numdfe  - the number of dfetimes
C       maxdfe  - maximum number of dfetimes
C       zerodef  - Zerodef value from new faintdfe output
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C
C******************************************************************************
      subroutine readdfe (dfefile,dfetime,dfe,numdfe,maxdfe,
     &    zerodef,status)

      integer  maxdfe
      character*(*) dfefile
      double precision dfetime(maxdfe)
      integer dfe(4, maxdfe), numdfe, status, icount

      real chip0, chip1, chip2, chip3
      integer iunit, begchar(4), width(4), fields,zerodef
      character(80) commnt
      character(256) context

      iunit = 18

C try reading as a file first
 100  open (unit = iunit, file=dfefile, status='old', err=300)

C check for #zerodef=?
C     First the no. of records are counted and then read
C     excluding the comment line. (srilal)

      icount = 0
  207     read (iunit, 203, end=204, err=600) commnt
  203     format(A80)
          icount = icount + 1
          goto 207
  204 continue

C     First, icount is the number of lines in the dfefile
C     (data + zerodef line --if there is one (for backward compatibility))

      if (commnt(1:1) .eq. '!') then
          icount = icount -1
          if (commnt(11:11) .eq. '0') then
              zerodef = 0
          else if (commnt(11:11) .eq. '1') then
              zerodef = 1
          else if (commnt(11:11) .eq. '2') then
              zerodef = 2
          else
              goto 600
          endif
      endif
      rewind(iunit)

      numdfe = 0

C     Now, icount is the number of records in the dfefile
C     (without zerodef line)

C have to use * format because tab is used as seperator
      do 208 i=1, icount
          if (numdfe .eq. maxdfe) goto 700
          numdfe = numdfe + 1
          read (iunit, *, err=600) dfetime(numdfe),
     &        chip0, chip1, chip2, chip3

          dfe(1, numdfe) = nint(chip0)
          dfe(2, numdfe) = nint(chip1)
          dfe(3, numdfe) = nint(chip2)
          dfe(4, numdfe) = nint(chip3)

  208 continue

      close (unit=iunit)
      return

C jump here if error reading as a file.  Is this a value, or is it
C a non-existant file???

C parse for "tokens" use FCREATE parsing code
C  uses ' ' as seperator so error out if a , was used
 300  if (index(dfefile,',') .gt. 0) then
         context = ' Comma is not a valid seperator for DFE values: '
     &        // dfefile
         call fcerr (context)
         goto 600
      endif

      call gtoken (4, dfefile, fields,begchar,width)

      if (fields .eq. 1) then
         read (dfefile(begchar(1):begchar(1)+width(1)-1), '(f15.0)',
     &        err=600) chip0
         numdfe = 1
         dfe(1, numdfe) = nint(chip0)
         dfe(2, numdfe) = nint(chip0)
         dfe(3, numdfe) = nint(chip0)
         dfe(4, numdfe) = nint(chip0)
         dfetime(numdfe) = 0.D0
         return
      else if (fields .eq. 4) then
         read (dfefile(begchar(1):begchar(1)+width(1)-1), '(f15.0)',
     &        err=600) chip0
         read (dfefile(begchar(2):begchar(2)+width(2)-1), '(f15.0)',
     &        err=600) chip1
         read (dfefile(begchar(3):begchar(3)+width(3)-1), '(f15.0)',
     &        err=600) chip2
         read (dfefile(begchar(4):begchar(4)+width(4)-1), '(f15.0)',
     &        err=600) chip3
         numdfe = 1
         dfe(1, numdfe) = nint(chip0)
         dfe(2, numdfe) = nint(chip1)
         dfe(3, numdfe) = nint(chip2)
         dfe(4, numdfe) = nint(chip3)
         dfetime(numdfe) = 0.D0
         return
      else
         write (context, '(a42,i2,a6)')
     &        ' Either 1 or 4 input DFE values required, ', fields,
     &        ' found'
         call fcerr (context)
         goto 600
      endif

C Test to see if 4 chip numbers were input
 600  context = '  Error opening DFE input file ' // dfefile
      call fcerr (context)
      status = 10
      return

C     Come here if file has more than maxdfe records
  700 write (context, 710) maxdfe, dfefile
  710 format(' Error: Too many records (limit ',i6,') in DFE file ',a)
      call fcerr( context )
      status = 20
      close (unit=iunit)
      return

      end

C******************************************************************************
C SUBROUTINE:
C       copyasca
C
C DESCRIPTION:
C       This routine copies keywords written by ASCALIN
C
C AUTHOR/DATE:
C       Emily A. Greene
C       16 December, 1993
C       Hughes STX
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call copyasca (iunit, ounit, author, status)
C
C ARGUMENTS:
C       iunit   - input unit number
C       ounit   - output unit number
C       author  - the version of FRFREAD which created the input file
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C
C******************************************************************************
      subroutine copyasca (iunit, ounit, author, status)

      integer iunit, ounit, status
      character*(*) author

      character(8) keyword, keyroot
      character(80) comment, svalue
      real value
      integer decimals, detxcol, detycol, xcol, ycol
      logical modify

C determine the input file column number based on FRFREAD version
C the keywords only have to be modifed for more recent versions
      if (author(9:) .le. '2.995') then
         detxcol = 7
         detycol = 8
         xcol = 1
         ycol = 2
         modify = .false.
      else
         detxcol = 7
         detycol = 8
         xcol = 2
         ycol = 3
         modify = .true.
      endif

C OPTIC_DETX
      keyroot = 'OPTIC'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C OPTIC_DETY
      keyroot = 'OPTIC'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRPX_DETX
      keyroot = 'TCRPX'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRPX_DETY
      keyroot = 'TCRPX'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRVL_DETX
      keyroot = 'TCRVL'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRVL_DETY
      keyroot = 'TCRVL'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCDLT_DETX
      keyroot = 'TCDLT'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCDLT_DETY
      keyroot = 'TCDLT'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCTYP_DETX
      keyroot = 'TCTYP'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCTYP_DETY
      keyroot = 'TCTYP'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCD_DETX_DETX
      keyroot = 'TCD0'
      call ftkeyn (keyroot, detxcol, keyword, status)
      keyroot = keyword(1:5) // '0'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) then
         keyroot = 'TCD0'
         call ftkeyn (keyroot, detxcol+1, keyword, status)
         keyroot = keyword(1:5) // '0'
         call ftkeyn (keyroot, detxcol+1, keyword, status)
      endif
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCD_DETX_DETY
      keyroot = 'TCD0'
      call ftkeyn (keyroot, detxcol, keyword, status)
      keyroot = keyword(1:5) // '0'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) then
         keyroot = 'TCD0'
         call ftkeyn (keyroot, detxcol+1, keyword, status)
         keyroot = keyword(1:5) // '0'
         call ftkeyn (keyroot, detycol+1, keyword, status)
      endif
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCD_DETY_DETX
      keyroot = 'TCD0'
      call ftkeyn (keyroot, detycol, keyword, status)
      keyroot = keyword(1:5) // '0'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) then
         keyroot = 'TCD0'
         call ftkeyn (keyroot, detycol+1, keyword, status)
         keyroot = keyword(1:5) // '0'
         call ftkeyn (keyroot, detxcol+1, keyword, status)
      endif
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCD_DETY_DETY
      keyroot = 'TCD0'
      call ftkeyn (keyroot, detycol, keyword, status)
      keyroot = keyword(1:5) // '0'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) then
         keyroot = 'TCD0'
         call ftkeyn (keyroot, detycol+1, keyword, status)
         keyroot = keyword(1:5) // '0'
         call ftkeyn (keyroot, detycol+1, keyword, status)
      endif
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCROT_DETY
      keyroot = 'TCROT'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C PLTSCL_DETX
      keyroot = 'PLTSCL'
      call ftkeyn (keyroot, detxcol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, detxcol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C PLTSCL_DETY
      keyroot = 'PLTSCL'
      call ftkeyn (keyroot, detycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, detycol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C OPTIC_X
      keyroot = 'OPTIC'
      call ftkeyn (keyroot, xcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, xcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C OPTIC_Y
      keyroot = 'OPTIC'
      call ftkeyn (keyroot, ycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, ycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRPX_X
      keyroot = 'TCRPX'
      call ftkeyn (keyroot, xcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, xcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRPX_Y
      keyroot = 'TCRPX'
      call ftkeyn (keyroot, ycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, ycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRVL_X
      keyroot = 'TCRVL'
      call ftkeyn (keyroot, xcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, xcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCRVL_Y
      keyroot = 'TCRVL'
      call ftkeyn (keyroot, ycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, ycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCDLT_X
      keyroot = 'TCDLT'
      call ftkeyn (keyroot, xcol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, xcol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCDLT_Y
      keyroot = 'TCDLT'
      call ftkeyn (keyroot, ycol, keyword, status)
      call ftgkye (iunit, keyword, value, comment, status)
      if (modify) call ftkeyn (keyroot, ycol+1, keyword, status)
      decimals = 8
      call ftpkye (ounit, keyword, value, decimals, comment, status)
      if (status .eq. 202) status = 0

C TCTYP_X
      keyroot = 'TCTYP'
      call ftkeyn (keyroot, xcol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, xcol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0

C TCTYP_Y
      keyroot = 'TCTYP'
      call ftkeyn (keyroot, ycol, keyword, status)
      call ftgkys (iunit, keyword, svalue, comment, status)
      if (modify) call ftkeyn (keyroot, ycol+1, keyword, status)
      call ftpkys (ounit, keyword, svalue, comment, status)
      if (status .eq. 202) status = 0


      return
      end
