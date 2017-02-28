C  FTOOLs info $Header: /headas/headas/ftools/asca/src/mkdtime/mkdtime.f,v 3.9 2013/05/21 19:08:07 irby Exp $
C      
C******************************************************************************
C SELECTOR TASK:
C      mkdtime
C
C FILE:
C      mkdtime.f 
C
C DESCRIPTION: 
C       This routine reads an mkfilter file and outputs the GIS deadtime
C       fraction in either compact or expanded HK format.
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       3 January, 1994
C
C MODIFICATION HISTORY:
C       1/29/94 EAG 1.1 Set negative deadtimes to 0
C       9/6/94  EAG 1.2 Alternately write to exiting MKF file
C       11/7/94 EAG 1.3 Fixed problem with duplicate TIMEs
C       12/1/95 Srilal 1.4 - Fixed the problem of corrupting the input
C		file when "NONE" is specified for the output file.
C		Note: Output cannot be written to a different file.
C		(Also, added timestamp.)
C       2002-02-22 Ed Pier - fixed some uninitialized variable problems
C                  which showed up under Linux. Thanx to Koji Mukai for
C                  diagnosing the problem.
C                  Also made a few other
C                  additions to make the code mroe robust. These are commented
C                  with the date in the code.
C
C NOTES:
C      mkdtime supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine mkdtie
      character(160) infile, outfile
      integer minl1
      real tau, cpucts
      logical compact, sensecase

      integer status

      character(40) taskname
      common /task/ taskname

      character(80) ldcol, l1col, cpuicol, cpuocol, telmcol, timecol
      character(80) deltcol, deadcol
      common / mkdcls / ldcol, l1col, cpuicol, cpuocol, telmcol,
     &     timecol, deltcol, deadcol

      taskname = 'mkdtime1.4'
      call ftcmsg

      infile = ' '
      outfile = ' '
      timecol = ' '
      deadcol = ' '
      status = 0

C  get the parameters from the par file
      call gmkdtime (infile, outfile, minl1, cpucts, tau, compact, 
     &     sensecase, status)
      if (status .ne. 0) goto 999

C calculate the deadtime fraction
      call pmkdtime (infile, outfile, minl1, cpucts, tau, compact, 
     &     sensecase, status)

 999  if (status .ne. 0) call fcerrm (status)

      return
      end


C*****************************************************************************
C SUBROUTINE:
C      gmkdtime
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       3 January, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gmkdtime uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C       call gmkdtime (infile, outfile, minl1, cpucts, tau,
C                         compact, sensecase, status)
C
C ARGUMENTS:
C       infile   - input mkfilter file and extension number
C       outfile  - output filename
C       minl1    - the minimum value of Gn_L1 for which to calculate deadtime
C       cpucts   - Largest acceptable relative error in CPU_I
C       tau      - response time of ADC
C       compact  - whether the output file is in compact or expanded HK format
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
      subroutine gmkdtime (infile, outfile, minl1, cpucts, tau,
     &     compact, sensecase, status)

      character*(*) infile, outfile
      integer minl1
      real tau, cpucts
      logical compact, sensecase
      integer status

      character(80) context

      character(80) ldcol, l1col, cpuicol, cpuocol, telmcol, timecol
      character(80) deltcol, deadcol
      common / mkdcls / ldcol, l1col, cpuicol, cpuocol, telmcol,
     &     timecol, deltcol, deadcol



C  get the name of the input FITS file
      call uclgst ('infile', infile, status)
      if (status .ne. 0) then
         context = 'could not get INFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the output deadtime fraction FITS file
      call uclgst ('outfile', outfile, status)
      if (status .ne. 0) then
         context = 'could not get OUTFILE parameter'
         call fcerr (context)
         goto 999
      endif

C  get the value of minl1
      call uclgsi ('minl1', minl1, status)
      if (status .ne. 0) then
         context = 'could not get MINL1 parameter'
         call fcerr (context)
         goto 999
      endif

C  get the value of cpucts
      call uclgsr ('cpucts', cpucts, status)
      if (status .ne. 0) then
         context = 'could not get CPUCTS parameter'
         call fcerr (context)
         goto 999
      endif

C  get the value of tau
      call uclgsr ('tau', tau, status)
      if (status .ne. 0) then
         context = 'could not get TAU parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the deadtime output file time column
      call uclgst ('timecol', timecol, status)
      if (status .ne. 0) then
         context = 'could not get TIMECOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the name of the data bin width column in the input file
      call uclgst ('deltcol', deltcol, status)
      if (status .ne. 0) then
         context = 'could not get DELDCOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the deadtime file fractional deadtime column
      call uclgst ('deadcol', deadcol, status)
      if (status .ne. 0) then
         context = 'could not get DEADCOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the mkfilter file ld columns
      call uclgst ('ldcol', ldcol, status)
      if (status .ne. 0) then
         context = 'could not get LDCOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the mkfilter L1 columns
      call uclgst ('l1col', l1col, status)
      if (status .ne. 0) then
         context = 'could not get L1COL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the mkfilter CPU_I columns
      call uclgst ('cpuicol', cpuicol, status)
      if (status .ne. 0) then
         context = 'could not get CPUICOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the mkfilter CPU output columns
      call uclgst ('cpuocol', cpuocol, status)
      if (status .ne. 0) then
         context = 'could not get CPUOCOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get the postfix of the mkfilter telemetry columns
      call uclgst ('telmcol', telmcol, status)
      if (status .ne. 0) then
         context = 'could not get TELMCOL parameter'
         call fcerr (context)
         goto 999
      endif

C  get whether to output compact HK format
      call uclgsb ('compact', compact, status)
      if (status .ne. 0) then
         context = 'could not get COMPACT parameter'
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
C      pmkdtime
C
C DESCRIPTION: 
C      Calculate the deadtime fraction for both GIS instruments
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       4 January, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call pmkdtime (infile, outfile, minl1, cpucts, tau, compact, 
C                         sensecase, status)
C
C ARGUMENTS:
C       infile   - input mkfilter file and extension number
C       outfile  - output file name
C       minl1    - minimum value of L1 for which to calculate deadtime
C       cpucts   - Largest allowable relative error in CPU_I
C       tau      - response time of ADC
C       compact  - whether to use compact HK format
C       sensecase - whether to be case sensitive about column names
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine pmkdtime (infile, outfile, minl1, cpucts, tau,
     &     compact, sensecase, status)

      character*(*) infile, outfile
      integer minl1
      real tau, cpucts
      logical compact, sensecase
      integer status

      integer iunit, ounit, fstatus, bitpix, naxis, naxes
      integer irow, nrows, naxis1, varidat, extnum, block, htype
      integer g2orow, g3orow, otfields, pcount, gcount

      real tg2ld, tg2l1, tg2ci, tg2co, tg2tlm, g2dead
      real tg3ld, tg3l1, tg3ci, tg3co, tg3tlm, g3dead
      logical g2done, g3done, g2integrate, g3integrate
      logical extend, simple
      double precision time, delta, g2oldtime, g3oldtime
      double precision g2ttot, g3ttot
      character(30) otform(3), ottype(3), otunits(3), extname
      character(160) filename
      character(80) context

      character(80) ldcol, l1col, cpuicol, cpuocol, telmcol, timecol
      character(80) deltcol, deadcol
      common / mkdcls / ldcol, l1col, cpuicol, cpuocol, telmcol,
     &     timecol, deltcol, deadcol

      integer g2ldno, g2l1no, g2cino, g2cono, g2tlmno
      integer g3ldno, g3l1no, g3cino, g3cono, g3tlmno
      integer tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno
      common /mkdcno / g2ldno, g2l1no, g2cino, g2cono, g2tlmno, 
     &     g3ldno, g3l1no, g3cino, g3cono, g3tlmno, 
     &     tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno

      real g2ldhit, g2l1, g2cpui, g2cpuo, g2telm
      real g3ldhit, g3l1, g3cpui, g3cpuo, g3telm, gismode
      common /mkdvls/ g2ldhit, g2l1, g2cpui, g2cpuo, g2telm,
     &     g3ldhit, g3l1, g3cpui, g3cpuo, g3telm, gismode


      iunit = 15
      ounit = 16
C open the  input file
      call fcpars (infile, filename, extnum, status)
      if (extnum .eq. -99) extnum = 1
      call ftopen (iunit, filename, 1, block, status)
      if (status .ne. 0) then
         context = ' Unable to open infile: ' // filename
         call fcerr (context)
         goto 999
      endif

C move to the requested extension
      call ftmrhd (iunit, extnum, htype, status)
      if (status .ne. 0) then
         write (context, 1001) extnum
 1001    format (' Unable to move to requested extension: ',i4)
         call fcerr (context)
         goto 998
      endif

C get all the requested columns in input file
      call mkdcol (iunit, sensecase, status)
      call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
      if (status .ne. 0) goto 998

C if outfile = 'NONE' then write results to infile
      filename = outfile
      call ftupch (filename)
      if (filename .eq. 'NONE') then
         ounit = iunit
         context = ' TASK:MKDTIME has updated GIS deadtime columns'
         call ftphis (ounit, context, status)
            call timestamp(ounit)
         
         otcolno = tcolno
         call ftgcno (ounit, sensecase, 'G2_' // deadcol, d2colno,
     &        status)
         call ftgcno (ounit, sensecase, 'G3_' // deadcol, d3colno,
     &        status)
         if (status .ne. 0) then
            context =
     &           ' Error finding deadtime columns in existing MKF file'
            call fcerr (context)
            goto 998
         endif
      else
C open the output file

C        The following warning was added by Ed on 2002-03-22
C        Koji pointed out that the fix by Srilal breaks the
C        program under this option - though it fixes the program
C        under the outfile=NONE option
C        Processing uses outfile=NONE. Maybe some day someone
C        can fix the program for both options, but it's probably not worth it

         context='WARNING: writing to a separate outfile not supported!'
         call fcerr(context)

         call ftinit (ounit, outfile, 2880, status)
         if (status .ne. 0) then
            context = ' Cannot open outfile - may exist ' // outfile
            call fcerr (context)
            goto 998
         endif

C create a blank primary extension
         simple = .true.
         bitpix = 8
         naxis = 0
         pcount = 0
         gcount = 1
         extend = .false.
         call ftphpr (ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &        extend,status)
         call ftpdef (ounit,bitpix,naxis,naxes,pcount,gcount,status)


C create the first extension
         call ftcrhd (ounit, status)

C set up the output table
         otfields = 3
         if (compact) then
            otform(1) = '10A'
            ottype(1) = 'NAME'
            otunits(1) = ' '
            otform(2) = '1E'
            ottype(2) = 'VALUE'
            otunits(2) = ' '
            otform(3) = '1D'
            ottype(3) = 'TIME'
            otunits(3) = 'sec'
         else
            otform(1) = '1D'
            ottype(1) = timecol
            otunits(1) = 'sec'
            otform(2) = '1E'
            ottype(2) = 'G2_' // deadcol
            otunits(2) = ' '
            otform(3) = '1E'
            ottype(3) = 'G3_' // deadcol
            otunits(3) = ' '
            otcolno = 1
            d2colno = 2
            d3colno = 3
         endif
         varidat = 0
         g2orow = 0
         g3orow = 0
         extname = ' '
         call ftphbn (ounit, g2orow, otfields, ottype, otform, otunits,
     &        extname, varidat, status)
         call ftbdef (ounit, otfields, otform, varidat, g2orow, status)

C write history records
         context = ' TASK:MKDTIME this file was created from '
     &        // filename
         call ftphis (ounit, context, status)

      endif


      write (context, 1002) minl1, cpucts, tau
 1002 format (' Using minl1 = ',I4,' cpucts = ',1PE10.3,' and tau = ',
     &     1PE10.3)
      call ftphis (ounit, context, status)
      if (status .ne. 0) then
         context = ' Error during initial setup '
         call fcerr (context)
         goto 997
      endif

C read through the mkfilter file




      irow = 0
      g2dead = 0.
      g3dead = 0.
      g2integrate = .false.
      g3integrate = .false.
      g2ttot = 0.
      tg2ld = 0.
      tg2ci = 0.
      tg2l1 = 0.
      tg2co = 0.
      tg2tlm = 0.
      g3ttot = 0.
      tg3ld = 0.
      tg3ci = 0.
      tg3l1 = 0.
      tg3co = 0.
      tg3tlm = 0.

C     The following initialization was added by Ed on Koji's suggestion
C     on 2002-03-22. These variables were uninitialized in the outfile=NONE
C     case and that was causing problems under Linux
      g2orow = 0
      g3orow = 0


      do 100 irow = 1, nrows
         g2done = .false.
         g3done = .false.

C get the values for the current row
         call mkdval (iunit, irow, time, delta, status)
         if (status .ne. 0) goto 997

C mkdval used to return nexttime so delta had to be calculated here
C         delta = nexttime - time
C now that mkdval returns delta directory, this line has been commented
C out (KM, 1995 Aug 4)

C are these values good? (G2)
         if ((g2ldhit .lt. 0.) .or. (g2cpui .lt. 0.) .or. 
     &        (g2cpuo .le. 0.) .or. (g2telm .lt. 0.) .or. 
     &        (g2l1 .lt. 0.) .or. (gismode .ne. 0.)) then
            if (g2integrate) then
C if we were integrating, stop here and output the information
               tg2ld = tg2ld / g2ttot
               g2dead = 1. - (1. - tg2ld * tau) * (tg2ci/tg2l1) *
     &              (tg2tlm/tg2co)
               call mkdwg2 (ounit, g2orow, g3orow, compact,
     &              g2oldtime, g2dead, deadcol, status)
               g2integrate = .false.
               g2ttot = 0.
               tg2ld = 0.
               tg2ci = 0.
               tg2l1 = 0.
               tg2co = 0.
               tg2tlm = 0.
            endif
C write 0 at this time
            g2dead = 0.

            call mkdwg2 (ounit, g2orow, g3orow, compact, time, 
     &           g2dead, deadcol, status)

            g2done = .true.
         endif

C are these values good? (G3)
         if ((g3ldhit .lt. 0.) .or. (g3cpui .lt. 0.) .or.
     &        (g3cpuo .le. 0.) .or. (g3telm .lt. 0.) .or.
     &        (g3l1 .lt. 0.) .or. (gismode .ne. 0.)) then
            if (g3integrate) then
C if we were integrating, stop here and output the information
               tg3ld = tg3ld / g3ttot
               g3dead = 1. - (1. - tg3ld * tau) * (tg3ci/tg3l1) *
     &              (tg3tlm/tg3co)
               call mkdwg3 (ounit, g2orow, g3orow, compact,
     &              g3oldtime, g3dead, deadcol, status)
               g3integrate = .false.
               g3ttot = 0.
               tg3ld = 0.
               tg3ci = 0.
               tg3l1 = 0.
               tg3co = 0.
               tg3tlm = 0.
            endif
C write 0 for this time
            g3dead = 0.
            call mkdwg3 (ounit, g2orow, g3orow, compact, time, 
     &           g3dead, deadcol, status)
            g3done = .true.
         endif

C if G2 dead time has not yet been calculated
         if (.not. g2done) then
            if ((.not. g2integrate) .and. (g2l1 .le. minl1)) then
C we are not integrating, and l1 is smaller than the minimum
               g2dead = 1. - g2telm/g2cpuo
               call mkdwg2 (ounit, g2orow, g3orow, compact,
     &              time, g2dead, deadcol, status)
            else
C we are integrating, or l1 is large enough to continue
               if (.not. g2integrate) g2oldtime = time
               g2ttot = g2ttot + delta
               tg2ld = tg2ld + g2ldhit * delta
               tg2ci = tg2ci + g2cpui * delta
               tg2l1 = tg2l1 + g2l1 * delta
               tg2co = tg2co + g2cpuo * delta
               tg2tlm = tg2tlm + g2telm * delta
               if (tg2ci .gt. 2./ (cpucts*cpucts)) then
C we have enough counts in CPU_I
                  tg2ld = tg2ld / g2ttot
                  g2dead = 1. - (1. - tg2ld * tau) * (tg2ci/tg2l1) * 
     &                 (tg2tlm/tg2co)
                  call mkdwg2 (ounit, g2orow, g3orow, compact, 
     &                 g2oldtime, g2dead, deadcol, status)
                  g2integrate = .false.
                  g2ttot = 0.
                  tg2ld = 0.
                  tg2ci = 0.
                  tg2l1 = 0.
                  tg2co = 0.
                  tg2tlm = 0.
               else
C cpu input counts are too small, must integrate
                  g2integrate = .true.
               endif
            endif
         endif
         
C if G3 dead time has not yet been calculated
         if (.not. g3done) then
            if ((.not. g3integrate) .and. (g3l1 .le. minl1)) then
C we are not integrating, and l1 is smaller than the minimum
               g3dead = 1. - g3telm/g3cpuo
               call mkdwg3 (ounit, g2orow, g3orow, compact,
     &              time, g3dead, deadcol, status)
           else
C we are integrating, or l1 is large enough to continue
               if (.not. g3integrate) g3oldtime = time
               g3ttot = g3ttot + delta
               tg3ld = tg3ld + g3ldhit
               tg3ci = tg3ci + g3cpui * delta
               tg3l1 = tg3l1 + g3l1 * delta
               tg3co = tg3co + g3cpuo * delta
               tg3tlm = tg3tlm + g3telm * delta
               if (tg3ci .gt. 2./ (cpucts*cpucts)) then
C we have enough counts in CPU_I
                  tg3ld = tg3ld / g3ttot
                  g3dead = 1. - (1. - tg3ld * tau) * (tg3ci/tg3l1) * 
     &                 (tg3tlm/tg3co)
                  call mkdwg3 (ounit, g2orow, g3orow, compact, 
     &                 g3oldtime, g3dead, deadcol, status)
                  g3integrate = .false.
                  g3ttot = 0.
                  tg3ld = 0
                  tg3ci = 0.
                  tg3l1 = 0.
                  tg3co = 0.
                  tg3tlm = 0.
               else
C cpu input counts are too small, must integrate
                  g3integrate = .true.
               endif
            endif
         endif

C loop back for next row
 100  continue

C end of input file.  If we are integrating, write the last row
      if (g2integrate) then
         tg2ld = tg2ld / g2ttot
         g2dead = 1. - (1. - tg2ld * tau) * (tg2ci/tg2l1) *
     &        (tg2tlm/tg2co)
         call mkdwg2 (ounit, g2orow, g3orow, compact,
     &        g2oldtime, g2dead, deadcol, status)
      endif
      if (g3integrate) then
         tg3ld = tg3ld / g3ttot
         g3dead = 1. - (1. - tg3ld * tau) * (tg3ci/tg3l1) *
     &        (tg3tlm/tg3co)
         call mkdwg3 (ounit, g2orow, g3orow, compact,
     &        g3oldtime, g3dead, deadcol, status)
      endif

C update the size of the output data unit
      call ftgkyj (ounit, 'NAXIS1', naxis1, context, status)
      call ftddef (ounit, naxis1*max(g2orow, g3orow), status)
      call ftmkyj (ounit, 'NAXIS2', max(g2orow, g3orow), '&', status)

 997  fstatus = 0
      if (ounit .ne. iunit) call ftclos (ounit, fstatus)
 998  fstatus = 0
      call ftclos (iunit, fstatus)
 999  return
      end

C*****************************************************************************
C SUBROUTINE:
C      mkdcol
C
C DESCRIPTION: 
C      Get the column numbers for all of the required columns
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       4 January, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call mkdcol (iunit, sensecase, status)
C
C ARGUMENTS:
C       iunit   - input unit number
C       sensecase - whether to be case sensitive about column names
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine mkdcol (iunit, sensecase, status)

      integer iunit, status
      logical sensecase

      character(80) context

      character(80) ldcol, l1col, cpuicol, cpuocol, telmcol, timecol
      character(80) deltcol, deadcol
      common / mkdcls / ldcol, l1col, cpuicol, cpuocol, telmcol, 
     &     timecol, deltcol, deadcol

      integer g2ldno, g2l1no, g2cino, g2cono, g2tlmno
      integer g3ldno, g3l1no, g3cino, g3cono, g3tlmno
      integer tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno
      common /mkdcno / g2ldno, g2l1no, g2cino, g2cono, g2tlmno, 
     &     g3ldno, g3l1no, g3cino, g3cono, g3tlmno, 
     &     tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno

C for G2
      call ftgcno (iunit, sensecase, 'G2_' // ldcol, g2ldno, status)
      call ftgcno (iunit, sensecase, 'G2_' // l1col, g2l1no, status)
      call ftgcno (iunit, sensecase, 'G2_' // cpuicol, g2cino, status)
      call ftgcno (iunit, sensecase, 'G2_' // cpuocol, g2cono, status)
      call ftgcno (iunit, sensecase, 'G2_' // telmcol, g2tlmno, status)
      if (status .ne. 0) then
         context = ' Unable to find all columns for G2 calculation'
         call fcerr (context)
         goto 999
      endif

C for G3
      call ftgcno (iunit, sensecase, 'G3_' // ldcol, g3ldno, status)
      call ftgcno (iunit, sensecase, 'G3_' // l1col, g3l1no, status)
      call ftgcno (iunit, sensecase, 'G3_' // cpuicol, g3cino, status)
      call ftgcno (iunit, sensecase, 'G3_' // cpuocol, g3cono, status)
      call ftgcno (iunit, sensecase, 'G3_' // telmcol, g3tlmno, 
     &     status)
      if (status .ne. 0) then
         context = ' Unable to find all columns for G3 calculation'
         call fcerr (context)
         goto 999
      endif

C and time, binwidth and gis mode
      call ftgcno (iunit, sensecase, timecol, tcolno, status)
      call ftgcno (iunit, sensecase, deltcol, dlcolno, status)
      call ftgcno (iunit, sensecase, 'GIS_MODE', mcolno, status)
      if (status .ne. 0) then
         context = ' Unable to find time or gismode in input file'
         call fcerr (context)
         goto 999
      endif

 999  return
      end

C******************************************************************************
C SUBROUTINE:
C      mkdval
C
C DESCRIPTION: 
C      Get the values for the current record of the mkf file
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       4 January, 1994
C
C MODIFICATION HISTORY:
C       Koji Mukai (4 August, 1995)
C       Changed to get delta, the bin width of the current colukn,
C       from the current row of mkf file (which has BN_WIDTH column)
C       instead of getting time of the next row then checking the
C       differences.  This version should be more robust.
C
C NOTES:
C
C USAGE:
C       call mkdval (iunit, irow, time, delta, status)
C
C ARGUMENTS:
C       iunit   - input unit number
C       irow    - the current input row number
C       time    - the time of the current row, returned
C       delta   - the bin width of the row
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C******************************************************************************

      subroutine mkdval (iunit, irow, time, delta, status)

      integer iunit, irow, status
      double precision time, delta

      character(80) context
      integer felem, nelem
      logical flag, anyf

      integer g2ldno, g2l1no, g2cino, g2cono, g2tlmno
      integer g3ldno, g3l1no, g3cino, g3cono, g3tlmno
      integer tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno
      common /mkdcno / g2ldno, g2l1no, g2cino, g2cono, g2tlmno, 
     &     g3ldno, g3l1no, g3cino, g3cono, g3tlmno, 
     &     tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno

      real g2ldhit, g2l1, g2cpui, g2cpuo, g2telm
      real g3ldhit, g3l1, g3cpui, g3cpuo, g3telm, gismode
      common /mkdvls/ g2ldhit, g2l1, g2cpui, g2cpuo, g2telm,
     &     g3ldhit, g3l1, g3cpui, g3cpuo, g3telm, gismode

      felem = 1
      nelem = 1
      flag = .false.
      anyf = .false.
C Get the time
      call ftgcfd (iunit, tcolno, irow, felem, nelem, time, flag, 
     &     anyf, status)

C get the GIS mode
      call ftgcfe (iunit, mcolno, irow, felem, nelem, gismode, flag,
     &     anyf, status)

C get the G2 values
      call ftgcfe (iunit, g2ldno, irow, felem, nelem, g2ldhit, flag,
     &     anyf, status)
      call ftgcfe (iunit, g2l1no, irow, felem, nelem, g2l1, flag,
     &     anyf, status)
      call ftgcfe (iunit, g2cino, irow, felem, nelem, g2cpui, flag,
     &     anyf, status)
      call ftgcfe (iunit, g2cono, irow, felem, nelem, g2cpuo, flag,
     &     anyf, status)
      call ftgcfe (iunit, g2tlmno, irow, felem, nelem, g2telm, flag,
     &     anyf, status)

C and the G3 values
      call ftgcfe (iunit, g3ldno, irow, felem, nelem, g3ldhit, flag,
     &     anyf, status)
      call ftgcfe (iunit, g3l1no, irow, felem, nelem, g3l1, flag,
     &     anyf, status)
      call ftgcfe (iunit, g3cino, irow, felem, nelem, g3cpui, flag,
     &     anyf, status)
      call ftgcfe (iunit, g3cono, irow, felem, nelem, g3cpuo, flag,
     &     anyf, status)
      call ftgcfe (iunit, g3tlmno, irow, felem, nelem, g3telm, flag,
     &     anyf, status)

CC Get the next time
C      call ftgcfd (iunit, tcolno, irow+1, felem, nelem, nexttime, 
C     &     flag, anyf, status)
C Get the bin width
      call ftgcfd (iunit, dlcolno, irow, felem, nelem, delta, 
     &     flag, anyf, status)

      if (status .ne. 0) then
         write (context, 1001) irow
 1001    format (' Unable to get values on row: ', i8)
         call fcerr (context)
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      mkdwg2
C
C DESCRIPTION: 
C      Write G2 deadtime fraction values to the output file appropriately
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       6 January, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call mkdwg2 (ounit, g2orow, g3orow, compact,
C                         time, g2dead, deadcol, status)
C
C ARGUMENTS:
C       ounit   - output unit number
C       g2orow - current output row number for g2
C       g3orow - current output row number for g3
C       compact - whether the output file is in compact HK format
C       time    - time for this value of g2dead
C       g2dead  - the deadtime fraction for g2
C       deadcol - postfix of the deadtime fraction column name
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C****************************************************************************** 

      subroutine mkdwg2 (ounit, g2orow, g3orow, compact,
     &     time, g2dead, deadcol, status)

      integer ounit,  status
      integer g2orow, g3orow
      double precision time
      real g2dead
      logical compact
      character*(*) deadcol

      real olddead
      double precision oldtime
      character(80) context
      character(100) tmpstr
      logical anyf, flag

      integer g2ldno, g2l1no, g2cino, g2cono, g2tlmno
      integer g3ldno, g3l1no, g3cino, g3cono, g3tlmno
      integer tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno
      common /mkdcno / g2ldno, g2l1no, g2cino, g2cono, g2tlmno, 
     &     g3ldno, g3l1no, g3cino, g3cono, g3tlmno, 
     &     tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno

C temporarily, if deadtime is negative, set to 0
      if (g2dead .lt. 0.) g2dead = 0.

C if the output is compact, just write to file
      if (compact) then

         g2orow = g2orow + 1
         g3orow = g3orow + 1
         tmpstr = 'G2_' // deadcol
         call ftpcls (ounit, 1, g2orow, 1, 1, tmpstr, status)
         call ftpcle (ounit, 2, g2orow, 1, 1, g2dead, status)
         call ftpcld (ounit, 3, g2orow, 1, 1, time, status)
      else
C output is expanded format
C is  the g2 output row ahead of the g3?


 100     continue
C  Follwing lines were removed to fix the bug of corrupted o/p - Srilal
C100     if (g2orow .ge. g3orow) then
C this is a new row, so just have to output the current information
C            g2orow = g2orow + 1
C          if (d2colno .lt. 3) call ftpcld
C    &           (ounit, otcolno, g2orow, 1, 1, time, status)
C           call ftpcle (ounit, d2colno, g2orow, 1, 1, g2dead, status)
C        else


C we've already written additional rows for g3
C find the record with the current time (if any)
           g2orow = g2orow + 1
            call ftgcfd (ounit, otcolno, g2orow, 1, 1, oldtime, flag, 
     &           anyf, status)

C          This was added by Ed on 2002-03-22. Previously the status
C          was not checked, so if there was a FITSIO error, the program
C          could go into an infinite loop here since the value of 
C          oldtime returned would be garbage.
           if(status .ne. 0 ) return

            if (oldtime .lt. time) then
C fill in the G2 deadtime column with the previous value
               call ftgcfe (ounit, d2colno, g2orow-1, 1, 1, olddead,
     &              flag, anyf, status)
               call ftpcle (ounit, d2colno, g2orow, 1, 1, olddead,
     &              status)
               goto 100
            else if (oldtime .eq. time) then
C this is the row for the current value
               call ftpcle (ounit, d2colno, g2orow, 1, 1, g2dead,
     &              status)
            else
C the time we want was skipped somehow
               context = ' input file is not time ordered, please sort'
               call fcerr (context)
               status = 10
            endif
C        endif
      endif

      return
      end

C******************************************************************************
C SUBROUTINE:
C      mkdwg3
C
C DESCRIPTION: 
C      Write G3 deadtime fraction values to the output file appropriately
C
C AUTHOR:  
C       Emily A. Greene
C       Hughes STX
C       6 January, 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call mkdwg3 (ounit, g2orow, g3orow, compact,
C                         time, g3dead, deadcol, status)
C
C ARGUMENTS:
C       ounit   - output unit number
C       g2orow - current output row number for g2
C       g3orow - current output row number for g3
C       compact - whether the output file is in compact HK format
C       time    - time for this value of g3dead
C       g3dead  - the current deadtime fraction for g3
C       deadcol  - postfix of the deadtime fraction column name
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C
C****************************************************************************** 

      subroutine mkdwg3 (ounit, g2orow, g3orow, compact,
     &     time, g3dead, deadcol, status)

      integer ounit, g2orow, g3orow, status
      double precision time
      real g3dead
      logical compact
      character*(*) deadcol

      real olddead
      double precision oldtime
      character(80) context
      character(100) tmpstr
      logical anyf, flag

      integer g2ldno, g2l1no, g2cino, g2cono, g2tlmno
      integer g3ldno, g3l1no, g3cino, g3cono, g3tlmno
      integer tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno
      common /mkdcno / g2ldno, g2l1no, g2cino, g2cono, g2tlmno, 
     &     g3ldno, g3l1no, g3cino, g3cono, g3tlmno, 
     &     tcolno, dlcolno, otcolno, d2colno, d3colno, mcolno

C temporarily, set deadtime to 0 if is negative
      if (g3dead .lt. 0.) g3dead = 0.

C if the output is compact, just write to file
      if (compact) then
         g2orow = g2orow + 1
         g3orow = g3orow + 1
         tmpstr = 'G3_' // deadcol
         call ftpcls (ounit, 1, g3orow, 1, 1, tmpstr, status)
         call ftpcle (ounit, 2, g3orow, 1, 1, g3dead, status)
         call ftpcld (ounit, 3, g3orow, 1, 1, time, status)
      else
C output is expanded format
C is  the g3 output row ahead of the g2?

 100     continue
C  Follwing lines were removed to fix the bug of corrupted o/p - Srilal
C100     if (g3orow .ge. g2orow) then
C this is a new row, so just have to output the current information
C           g3orow = g3orow + 1
C           if (d3colno .le. 3) call ftpcld
C    &           (ounit, otcolno, g3orow, 1, 1, time, status)
C           call ftpcle (ounit, d3colno, g3orow, 1, 1, g3dead, status)
C        else

C we've already written additional rows for g2
C find the record with the current time (if any)
            g3orow = g3orow + 1
            call ftgcfd (ounit, otcolno, g3orow, 1, 1, oldtime, flag,
     &           anyf, status)

C          This was added by Ed on 2002-03-22. Previously the status
C          was not checked, so if there was a FITSIO error, the program
C          could go into an infinite loop here since the value of 
C          oldtime returned would be garbage.
           if(status .ne. 0 ) return

            if (oldtime .lt. time) then
C fill in the G3 deadtime column with the previous value
               call ftgcfe (ounit, d3colno, g3orow-1, 1, 1, olddead,
     &              flag, anyf, status)
               call ftpcle (ounit, d3colno, g3orow, 1, 1, olddead,
     &              status)
               goto 100
            else if (oldtime .eq. time) then
C this is the row for the current value
               call ftpcle (ounit, d3colno, g3orow, 1, 1, g3dead,
     &              status)
            else
C the time we want was skipped somehow
               context = ' input file is not time ordered, please sort'
               call fcerr (context)
               status = 10
            endif
C        endif        
      endif

      return
      end

