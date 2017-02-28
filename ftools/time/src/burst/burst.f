C******************************************************************************
C SELECTOR TASK:
C      burst
C
C FILE:
C      burst.f 
C
C DESCRIPTION: 
C       This routine removes burst data from an events FITS table.
C
C AUTHOR:
C       Emily A. Greene
C       Hughes STX
C       October, 1992 
C
C MODIFICATION HISTORY:
C       3/10/93 EAG - changed time to doubles
C       9/23/94 EAG 3.1a - shorten strings, etc., FFINIT
C
C NOTES:
C      burst supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file for header and data
C      tdelta   - the maximum time interval of burst
C      keep     - whether to keep the first event in a burst
C      good     - whether to output the non-burst or burst events
C      timecol  - the name of the column containing the event times
C      copy     - whether to copy all other exetension to output file
C
C CALLED ROUTINES:
C      subroutine gburst - gets parameters from environment
C
C******************************************************************************
      subroutine burst

      character(160) infile, outfile
      character(40)  timecol
      double precision tdelta
      logical keep, good, copy

      integer iunit, ounit, status, hdutype
      character(40) taskname
      common /task/ taskname

      taskname = 'burst3.1a'
C       initialize parameters
      iunit = 15
      ounit = 16
      status = 0
      call ftcmsg

C  get the parameters from the par file
      call gburst (infile, outfile, tdelta, keep, good, timecol,
     &     copy, status)
      if (status .ne. 0) goto 999

C  get the input and output files open to the correct places
      call burfil (infile, outfile, iunit, ounit, copy, status)
      if (status .ne. 0) goto 999

C read the data and copy appropriately to the output file
      call burcpy (iunit, ounit, tdelta, keep, good, timecol, status)
      if (status .ne. 0) goto 999

C copy all extensions (if requested) after the extension to be operated on
      if (copy)  then
 10      call ftmrhd (iunit, 1, hdutype, status)
         call ftcrhd (ounit, status)
         call ftcopy (iunit, ounit, 0, status)
         if (status .ne. 0) goto 998
         goto 10
      endif

 998  status = 0
      call ftcmsg
      call ftclos (ounit, status)
      status = 0
      call ftclos (iunit, status)

 999  if (status .ne. 0) call fcerrm(status)
      return
      end

C******************************************************************************
C SUBROUTINE:
C      gburst
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR: 
C       Emily A. Greene
C       Hughes STX
C       October, 1992 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gburst uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C       call gburst (infile, outfile, tdelta, keep, bood, timecol, copy, status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file
C      tdelta   - the maximum time interval of burst
C      keep     - whether to keep the first event in a burst
C      good     - whether to output the non-burst or burst events
C      timecol  - the name of the column containing the event times
C      copy     - whether to copy all other exetension to output file
C      status   - status of operations
C       
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real parameter
C      subroutine uclgsb - get boolean parameter
C
C******************************************************************************
      subroutine gburst(infile,outfile,tdelta, keep, good, timecol,
     &     copy, status)

      character*(*) infile, outfile, timecol
      double precision tdelta
      logical keep, good, copy
      integer status

      character(70) context

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

C  get the tdelta
      call uclgsd('tdelta', tdelta,status)
      if (status .ne. 0) then
         context = 'could not get TDELTA parameter'
         call fcerr(context)
         goto 999
      endif

C  get the keep first event of burst logical
      call uclgsb('keep',keep,status)
      if (status .ne. 0) then
         context = 'could not get KEEP parameter'
         call fcerr(context)
         goto 999
      endif

C  get the output good events logical
      call uclgsb('good', good,status)
      if (status .ne. 0) then
         context = 'could not get GOOD parameter'
         call fcerr(context)
         goto 999
      endif

C  get the time column name
      call uclgst('timecol', timecol, status)
      if (status .ne. 0) then
         context = 'could not get TIMECOL string'
         call fcerr(context)
         goto 999
      endif

C  get the copy all other extensions flag
      call uclgsb('copyall', copy,status)
      if (status .ne. 0) then
         context = 'could not get COPYALL flag'
         call fcerr(context)
         goto 999
      endif

 999  continue
      if (status .ne. 0)  call fcerrm(status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C      burfil
C
C DESCRIPTION: 
C      Do all file manipulation.  Open both files, copy requested extension.
C
C AUTHOR: 
C       Emily A. Greene
C       Hughes STX
C       October, 1992 
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call burfil (infile, outfile, iunit, ounit, copy, status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output ASCII file
C      iunit    - input file unit number
C      ounit    - output file unit number
C      copy     - whether to copy all other exetension to output file
C      status   - error number
C       
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************
      subroutine burfil (infile, outfile, iunit, ounit, copy, status)

      character*(*) infile, outfile
      integer iunit, ounit, status
      logical copy

      integer maxcl
      parameter (maxcl = 512)

      character(160) filename
      character(80) context, extname
      character(16) ttype(maxcl), tform(maxcl)
      character(25) tunit(maxcl)

      integer extnumb, block, bitpix, naxis, naxes(99)
      integer pcount, gcount, hdutype, rowlen, nrows, tfields, varidat
      integer tbcol(maxcl)
      integer i

      logical simple, extend

C initialize variables

C parse the input filename for name and extension number
      call fcpars (infile, filename, extnumb, status)

C EAG 8/25/93 default to 1st extension
      if (extnumb .eq. -99) extnumb = 1

C If the extension is 0 or * then exit
      if (extnumb .le. 0) then
         context = ' Primary array not supported'
         call fcerr (context)
         status = 1
         goto 999
      endif

C open the input file
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Unable to open input file'
         call fcerr (context)
         goto 999
      endif

C read the primary header
      call ftghpr (iunit, maxcl, simple, bitpix, naxis, naxes,
     &     pcount, gcount, extend, status)

C create the output FITS file
      call ffinit (ounit, outfile, status)
      if (status .ne. 0) then
         context = ' Unable to create outfile - already exists?'
         call fcerr (context)
         goto 998
      endif

C copy the primary array, if so requested
      if (copy) then
         call ftcopy (iunit, ounit, 0, status)
         context = 'TASK: BURST on filename: '//filename
         call ftphis (ounit, context, status)
         if (status .ne. 0) then
            context = ' Error copying primary array'
            call fcerr (context)
            goto 997
         endif
      else
C create blank primary array header
         naxis = 0
         call ftphpr(ounit, simple, bitpix, naxis, naxes, pcount,
     &        gcount, extend, status)
         call ftpdef(ounit, bitpix, naxis, naxes, pcount, gcount,
     &        status)
         if (status .ne. 0) then
            context = ' Error writing primary header'
            call fcerr (context)
            goto 997
         endif
      endif

C copy all extensions (if requested) before the extension to be operated on
      if ((copy) .and. (extnumb .gt. 1)) then
         do 100 i = 1, extnumb-1
            call ftmrhd (iunit, 1, hdutype, status)
            call ftcrhd (ounit, status)
            call ftcopy (iunit, ounit, 0, status)
 100     continue
         if (status .ne. 0) then
            context = ' Error copying extensions'
            call fcerr (context)
            goto 997
         endif
      endif

C get extension to be operated on
      call ftmahd (iunit, extnumb+1, hdutype, status)
      if (status .ne. 0) then
         context = ' unable to move to requested extension'      
         call fcerr (context)
         goto 997
      endif

C get the header depending on the extension type
      if (hdutype .eq. 1) then
         call ftghtb (iunit, maxcl, rowlen, nrows, tfields, ttype,
     &        tbcol, tform, tunit, extname, status)
      else if (hdutype .eq. 2) then
         call ftghbn (iunit, maxcl, nrows, tfields, ttype, tform,
     &        tunit, extname, varidat, status)
      else
         context = ' HDUtype for extension not supported'
         call fcerr (context)
         goto 997
      endif

C create the extension in the output file
      call ftcrhd (ounit, status)
      nrows = 0
      if (hdutype .eq. 1) then
         call ftphtb (ounit, rowlen, nrows, tfields, ttype, tbcol,
     &        tform, tunit, extname, status)
      else
         call ftphbn (ounit, nrows, tfields, ttype, tform, tunit,
     &        extname, varidat, status)
      endif

      call xcopyscale (iunit, ounit, status)
      context = ' TASK: BURST on FILENAME: '//filename
      call ftphis(ounit,context,status)

      if (hdutype .eq. 1) then
         call ftadef (ounit, rowlen, tfields, tbcol, tform, nrows,
     &        status)
      else
         call ftbdef (ounit, tfields, tform, varidat, nrows,
     &        status)
      endif

      if (status .ne. 0) then
         context = ' Error creating output extension header'
         call fcerr (context)
         goto 997
      endif

      return
 997  status = 0
      call ftclos (ounit, status)
 998  status = 0
      call ftclos (iunit, status)
 999  status = 1
      return
      end

C******************************************************************************
C SUBROUTINE:
C      burcpy
C
C DESCRIPTION: 
C      Find bursts and copy requested events
C
C AUTHOR: 
C       Emily A. Greene
C       Hughes STX
C       October, 1992 
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       call burcpy (iunit, ounit, tdelta, keep, good, timecol, status)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      ounit    - output file unit number
C      tdelta   - maximum seconds to define a burst
C      keep     - whether to keep the first event in a burst
C      good     - whether to keep "good" events, or bursts
C      timecol  - name of the time column
C      status   - error number
C       
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine uclpsx - put values into parameter file
C
C*****************************************************************************
      subroutine burcpy (iunit, ounit, tdelta, keep, good, timecol,
     &     status)

      integer iunit, ounit, status
      double precision tdelta, dmin, totime
      logical keep, good
      character*(*) timecol

      integer maxcl
      parameter (maxcl=512)

      character(80) context

      logical exact, flagval, anyf, burst

      integer colno, naxis1, naxis2, irow, orow, felem, nelem, start
      integer totburst, toteve

      double precision time1, time2, delta, tburst

C initialize
      exact = .false.
      totburst = 0
      toteve = 0
      totime = 0.0D0
      tburst =0.d0

C find the column number of the time column
      call ftgcno (iunit, exact, timecol, colno, status)
      if (status .ne. 0) then
         context = ' Requested time column not in extension'
         call fcerr (context)
         goto 997
      endif

C get naxis1 value
      call ftgkyj (iunit, 'NAXIS1', naxis1, context, status)
      if (status .ne. 0) then
         context = ' Could not find NAXIS1 value'
         call fcerr (context)
         goto 997
      endif

C get naxis2 value
      call ftgkyj (iunit, 'NAXIS2', naxis2, context, status)
      if (status .ne. 0) then
         context = ' Could not find NAXIS2 value'
         call fcerr (context)
         goto 997
      endif

C Get the first good time in the file
      irow = 1
      felem = 1
      nelem = 1
      orow = 1

 10   call ftgcfd (iunit, colno, irow, felem, nelem, time1, flagval,
     &     anyf, status)
      irow = irow + 1
      if (flagval) goto 10

C  Loop over all remaining rows
      start = irow
      burst = .false.
      dmin = -1.
      do 500 irow = start, naxis2
         call ftgcfd (iunit, colno, irow, felem, nelem, time2,
     &        flagval, anyf, status)
         if (flagval) goto 500

C check time difference
         delta = time2 - time1
         if (delta .lt. 0) then
            context = ' file not time ordered: please sort'
            call fcerr (context)
            goto 997
         endif
         if (dmin .lt. 0.) dmin = delta
         if (delta .lt. dmin) dmin = delta
         if (delta .gt. tdelta) then

C no burst - copy the previous row
            time1 = time2
            if ((good) .and. (.not. burst)) then
               call fcopyr (iunit,irow-1,ounit,orow,
     &              naxis1,status)
               orow = orow + 1
            endif
C if this ended a burst
            if (burst) then
               totime = totime + tburst
               burst = .false.
               if (.not. good) then
                  call fcopyr (iunit,irow-1,ounit,orow,naxis1,
     &                 status)
                  orow = orow + 1
               endif
            endif
         else

C burst
C is this the first event in the burst?
            if (.not. burst) then
C output 1st event if so requested
               if (keep) then
                  call fcopyr(iunit,irow-1,ounit,orow,naxis1,status)
                  orow = orow + 1
               endif
               burst = .true.
               totburst = totburst + 1
C 2 is added for the 1st and 2nd event in the burst
               if ((good) .and. (.not. keep)) toteve = toteve + 1
               if ((.not. good) .and. (keep)) toteve = toteve + 1
               toteve = toteve + 1
               tburst = delta
            else

C subsequent event in a burst
               if (.not. good) then
                  call fcopyr (iunit, irow-1, ounit, orow, naxis1,
     &                 status)
                  orow = orow + 1
               endif
               toteve = toteve + 1
               tburst = delta
            endif
         endif

 500  continue

C output last event if not in a burst (depending on good)
      if ((.not. burst) .and. (good)) then
         call fcopyr(iunit, irow-1, ounit,
     &        orow, naxis1, status)
         orow = orow + 1
      endif
      if ((burst) .and. (.not. good)) then
         call fcopyr(iunit, irow-1, ounit,
     &        orow, naxis1, status)
         orow = orow + 1
      endif

      if (status .ne. 0) then
         context = ' Error copying events to output file'
         call fcerr (context)
         call fcerrm (status)
         goto 997
      endif

C write history records to file
      call timestamp (ounit)
      write (context, 1001) totburst
 1001 format (' TASK:BURST total burst events detected: ',i6)
      call ftphis (ounit, context, status)
      write (context, 1002) toteve
 1002 format (' TASK:BURST total burst pha events rejected: ',i6)
      call ftphis (ounit, context, status)
      write (context, 1003) totime
 1003 format (' TASK:BURST total time of burst events (sec): '
     &     ,1pg20.13)
      call ftphis (ounit, context, status)
      write (context, 1004) dmin
 1004 format (' TASK:BURST minimum time between events (sec): '
     &     ,1pg20.13)
      call ftphis (ounit, context, status)
      if (status .ne. 0) then
         context = ' Error writing history records'
         call fcerr (context)
         goto 997
      endif

C Update Naxis2
      call ftmkyj (ounit, 'NAXIS2', orow-1, '&', status)
      if (status .ne. 0) then
         context = ' Error modifying NAXIS2'
         call fcerr (context)
         goto 997
      endif

C redefine the CDU size
      call ftrdef (ounit, status)
      if (status .ne. 0) then
         context = ' Error making output extension correct size '
         call fcerr (context)
         goto 997
      endif

C write values to parameter file
      call uclpsi ('totburst', totburst, status)
      call uclpsi ('toteve', toteve, status)
      call uclpsd ('totime', totime, status)
      call uclpsd ('tmin', dmin, status)
      if (status .ne. 0) then
         context = ' Error writing values to parameter file'
         call fcerr (context)
         goto 997
      endif

      return

 997  status = 0
      call ftclos (ounit, status)
 998  status = 0
      call ftclos (iunit, status)
 999  status = 1
      return
      end
        
C******************************************************************************
