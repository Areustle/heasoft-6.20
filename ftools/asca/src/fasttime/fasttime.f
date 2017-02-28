C  FTOOLs info $Header: /headas/headas/ftools/asca/src/fasttime/fasttime.f,v 3.11 2013/05/21 19:08:06 irby Exp $
C
C******************************************************************************
C SELECTOR TASK:
C      fasttime
C
C FILE:
C      fasttime.f
C
C DESCRIPTION:
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C	1.1 2/8/94 EAG Allow readonly if new output file
C		       Update/add TIMEDEL keyword to primary and extension
C		       Set maxsize=1 instead of 1024 in fastcorr
C       1.2 6/25/98 PDW Updated for new filename handling
C
C NOTES:
C      fasttime supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine fastte
	character(160) infile, outfile
	character(80) timecol, ycolumn

	integer sourcey, status, fstatus
	integer tcolno, ycolno, iunit, ounit, stclk
	logical sensecase

	character(40) taskname
	common /task/ taskname

	taskname = 'fasttime 1.2'

	outfile = ' '
	infile = ' '
	timecol = ' '
	ycolumn = ' '

	status = 0
	iunit = 15
	ounit = 16

C  get the parameters from the par file
	call gfasttime (infile, outfile, sourcey, timecol, ycolumn,
     &                  sensecase, status)
	if (status .ne. 0) goto 999

C open the input and output file, and check for reasonable input
	call fastinp (infile, iunit, outfile, ounit, timecol,
     &                tcolno, ycolumn, ycolno, sensecase, sourcey,
     &                stclk, status)
	if (status .ne. 0) goto 998

C calculate the new time
	call fastcorr (iunit, ounit, tcolno, ycolno, sourcey, stclk,
     &                 status)
	if (status .ne. 0) goto 998

998	fstatus = 0
	call ftclos (iunit, fstatus)
	if (iunit .ne. ounit) then
		fstatus = 0
		call ftclos (ounit, fstatus)
	endif


999	if (status .ne. 0) call fcerrm (status)
	return
	end


C******************************************************************************
C SUBROUTINE:
C      gfasttime
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gfasttime uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gfasttime (infile, outfile, sourcey, timecol, ycolumn,
C			sensecase, status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output FITS file
C	sourcey - location of the source on the y axis of the detector
C	timecol - the name of the column containing the time
C	ycolumn - the name of the column containing the raw Y values
C	sensecase - be case sensitive about column names
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C
C******************************************************************************
	subroutine gfasttime (infile, outfile, sourcey, timecol,
     &                   ycolumn, sensecase, status)

	character*(*) infile, outfile, timecol, ycolumn
	integer sourcey, status
	logical sensecase

	character(80) context

C  get the name of the input FITS file
	call uclgst ('infile', infile, status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr (context)
	    goto 999
	endif

C  get the name of the output FITS file
	call uclgst ('outfile', outfile, status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the source Y position
	call uclgsi ('sourcey', sourcey, status)
	if (status .ne. 0) then
	    context = 'could not get SOURCEY parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the time column
	call uclgst ('timecol', timecol, status)
	if (status .ne. 0) then
	    context = 'could not get TIMECOL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the raw y values column
	call uclgst ('ycolumn', ycolumn, status)
	if (status .ne. 0) then
	    context = 'could not get YCOLUMN parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to be case sensitive about columns
	call uclgsb ('sensecase', sensecase, status)
	if (status .ne. 0) then
	    context = 'could not get SENSECASE parameter'
	    call fcerr(context)
	    goto 999
	endif

999	continue
	if (status .ne. 0)  call fcerrm(status)

	return
	end


C******************************************************************************
C SUBROUTINE:
C      fastinp
C
C DESCRIPTION:
C      Open the files, and check for reasonable input
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C       PDW 6/25/98: Updated for new filename handling
C
C NOTES:
C
C USAGE:
C	call fastinp (infile, iunit, outfile, ounit, timecol,
C            tcolno, ycolumn, ycolno, sensecase, sourcey, stclk, status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C	iunit   - input unit number
C      outfile  - output FITS file (may be the same as input file)
C	ounit   - output unit number
C	timecol - the name of the column containing the time
C	tcolno  - input time column number
C	ycolumn - the name of the column containing the raw Y values
C	ycolno  - input y column number
C	sensecase - be case sensitive about column names
C	sourcey   - the y position of the source
C	stclk     - number of fast mode stclk from primary header
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C******************************************************************************
	subroutine fastinp (infile, iunit, outfile, ounit,
     &                timecol, tcolno, ycolumn, ycolno, sensecase,
     &                sourcey, stclk, status)

	character*(*) infile, outfile, timecol, ycolumn
	integer tcolno, ycolno, iunit, ounit, status, stclk, sourcey
	logical sensecase

	character(80) context, mode
	character(160) filename
	integer fstatus, array(2), htype, extnumb, block, nfound
	logical already
	double precision timedel

C  open the input file
C PDW 6/25/98: Replace fcpars with ftrtnm and ftextn
C        call fcpars (infile, filename, extnumb, status)
C        if (extnumb .eq. -99) extnumb = 1
	call ftextn (infile, extnumb, status)
	if (extnumb .eq. -99) then
	   extnumb = 1
	else
	   extnumb = extnumb - 1
	endif

C check for input file = output file
	call ftrtnm (infile, filename, status)
	if ((outfile .eq. ' ') .or. (outfile .eq. '-') .or.
     &	    (outfile .eq. filename)) ounit = iunit

	if (iunit .eq. ounit) then
	    call ftopen (iunit, filename, 1, block, status)
	else
	    call ftopen (iunit, filename, 0, block, status)
	endif
        if (status .ne. 0) then
                context = ' Could not open input file' // infile
                call fcerr (context)
                goto 999
        endif

C check that this is a FAST mode file
	call ftgkys (iunit, 'DATAMODE', mode, context, status)
	if (status .ne. 0) then
		context = ' Unable to determine datamode'
		call fcerr (context)
		goto 998
	endif
	if (mode .ne. 'FAST') then
		context = ' This is not a FAST mode file: ' // mode
		call fcerr (context)
		status = 1
		goto 998
	endif

C get the STCLKSn keyword from the primary array
C check for 3.00 keywords - don't know the instrument, so try both
	call ftgkyj (iunit, 'S0_STCLK', stclk, context, status)
	if (status .eq. 202) then
	  status = 0
	  call ftgkyj (iunit, 'S1_STCLK', stclk, context, status)
	endif
	if (status .eq. 202) then
	status = 0
	call ftgknj (iunit, 'STCLKS', 0, 2, array, nfound, status)
	if ((array(1) .eq. 0) .and. (array(2) .eq. 0)) then
		context = ' error reading STCLKSx keyword'
		call fcerr (context)
		status = 1
		goto 998
	endif
	if (array(1) .ne. 0) then
		stclk = array(1)
	else
		stclk = array(2)
	endif
	endif

C new output file, if needed
	if (iunit .ne. ounit) then
		call ftinit (ounit, outfile, 2880, status)
		if (status .ne. 0) then
			context = ' error opening outfile, may exist?'
			call fcerr (context)
			goto 998
		endif

C copy the input file to the output file
		call ftcopy (iunit, ounit, 2, status)
		call copylast (iunit, ounit, status)

	endif

C move to the primary extension
	call ftmahd (iunit, 1, htype, status)
	call ftmahd (ounit, 1, htype, status)

C check if FASTTIME has already been run on this file:
	call ftgkyl (iunit, 'FSTTMCOR', already, context, status)
	if (status .eq. 202) then
	    status = 0
	else if ((status .eq. 0) .and. (already)) then
	    context = ' FASTTIME has already been run on this file '
	    call fcerr (context)
	    status = 1
	    goto 998
	else if (status .ne. 0) then
		context = ' Error reading FSTTMCOR keyword'
		call fcerr (context)
		goto 998
	endif

C move to the correct extension in the input and output files
	call ftmahd (iunit, extnumb+1, htype, status)
	call ftmahd (ounit, extnumb+1, htype, status)
        if (status .ne. 0) then
                write (context, 1001) extnumb
1001            format (' Error moving to extension: ',i3)
                call fcerr (context)
                goto 998
        endif
	call ftphis (ounit, context, status)

C see if the various columns are present
        call ftgcno (iunit, sensecase, timecol, tcolno, status)
        if (status .ne. 0) then
                context = ' TIMECOL column not found:' // timecol
                call fcerr (context)
                goto 998
        endif

        call ftgcno (iunit, sensecase, ycolumn, ycolno, status)
        if (status .ne. 0) then
                context = ' YCOLUMN column not found:' // ycolumn
                call fcerr (context)
                goto 998
        endif

C everything checks out, so we can now proceed to modify the output file
C go back to the primary array

        call ftmahd (iunit, 1, htype, status)
        call ftmahd (ounit, 1, htype, status)

	context = 'Time corrected with FASTTIME'
	already = .true.
 	call ftmkyl (ounit, 'FSTTMCOR', already, context, status)
	if (status .eq. 202) then
	    status = 0
	    call ftpkyl (ounit, 'FSTTMCOR', already, context, status)
	endif
	if (status .ne. 0) then
	    context = ' Error writing FSTTMCOR keyword'
	    call fcerr (context)
	    goto 998
	endif

C correct the value of the TIMEDEL keyword
	timedel = 0.015625
	context = '&'
	call ftmkyd (ounit, 'TIMEDEL', timedel, 15, context, status)
	if (status .eq. 202) then
	    status = 0
	    context = ' minimum time increment for corrected FAST mode'
	    call ftpkyd (ounit, 'TIMEDEL', timedel, 15, context, status)
	endif
	if (status .ne. 0) then
	    context = ' Error modifying/adding TIMEDEL keyword'
	    call fcerr (context)
	    goto 998
	endif

C write history records to primary and main extension
	write (context, 1000) sourcey
1000	format (
     &  ' TASK:FASTTIME  time has been corrected using SOURCEY = ',i5)
	call ftphis (ounit, context, status)

C move back to the requested extension
        call ftmahd (iunit, extnumb+1, htype, status)
        call ftmahd (ounit, extnumb+1, htype, status)
        call ftphis (ounit, context, status)

C correct the value of the TIMEDEL keyword
	timedel = 0.015625
	call ftmkyd (ounit, 'TIMEDEL', timedel, 15, context, status)
	if (status .eq. 202) then
	    status = 0
	    call ftpkyd (ounit, 'TIMEDEL', timedel, 15, context, status)
	endif
	if (status .ne. 0) then
	    context = ' Error modifying/adding TIMEDEL keyword'
	    call fcerr (context)
	    goto 998
	endif

	return

998	fstatus = 0
	call ftclos (iunit, fstatus)

999	return
	end

C******************************************************************************
C SUBROUTINE:
C      fastcorr
C
C DESCRIPTION:
C      Calculate the correct time, and write to the output file
C
C AUTHOR:
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C	This routine is based on the "animal" perl script evtoxronbin
C	by Kazuhisa Mitsuda
C
C USAGE:
C	call fastcorr (iunit, ounit, tcolno, ycolno, sourcey, stclk, status)
C
C ARGUMENTS:
C	iunit   - input unit number
C	ounit   - output unit number
C	tcolno  - input time column number
C	ycolno  - input y column number
C	sourcey - the y posistion of the source
C	stclk   - number of fast mode stclk from primary header
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C******************************************************************************
	subroutine fastcorr (iunit, ounit, tcolno, ycolno, sourcey,
     &                stclk, status)

	integer iunit, ounit, tcolno, ycolno, sourcey, stclk, status

	character(80) context

	integer ywidth, tunit, stlines
	parameter (ywidth = 256)
	parameter (tunit = 4)
	parameter (stlines = 422)

	integer maxsize
	parameter (maxsize = 1)

	double precision tbin, oldtime(maxsize), newtime(maxsize)
	double precision tshift, y(maxsize)
	integer i, nrows, remain, frow, nelem
	logical anyf

	tbin = dble(tunit) / dble(ywidth)
	tshift = dble(int (dble(stlines + sourcey) / dble(stclk)) + 0.5)
     &              * tbin - dble(tunit)/2.

	call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
	remain = nrows
	frow = 1

10	if (remain .gt. maxsize) then
		nelem = maxsize
	else
		nelem = remain
	endif

	call ftgcvd (iunit, tcolno, frow, 1, nelem, -99D0, oldtime,
     &               anyf, status)
	call ftgcvd (iunit, ycolno, frow, 1, nelem, -99D0, y, anyf,
     &               status)

	do 100 i = 1, nelem
		newtime(i) = oldtime(i) - tshift + y(i)*tbin
100	continue

C write out the new times

	call ftpcld (ounit, tcolno, frow, 1, nelem, newtime, status)

	frow = frow + nelem
	remain = remain - nelem
	if (remain .gt. 0) goto 10

	return
	end
