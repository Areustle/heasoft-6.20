C****************************************************************************** 
C SELECTOR TASK:
C      binspec
C
C FILE:
C      binspec.f 
C
C DESCRIPTION: 
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	July, 1993
C
C MODIFICATION HISTORY:
C	1.1  Includes copycol support
C	1.2  Reorganize to allow more common routines with bincurve
C	1.3  Allow for phase file support
C	1.4  Replace fidate with fisio routine fts2dt
C
C NOTES:
C      binspec supported in IRAF and HOST environments
C	the GTI file is assumed to be in time order
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
	subroutine binspc

	character(80) timecol, column, gtistart, gtistop, binmode, rows
	character(80) indate, intime, gtidate, gtitime, phdate, phtime
	character(80) epochky, periodky, phstart, phstop
	character(80) outcol

	integer binrows, status, nhists
	logical copycol, copyprime, copyall, history, sensecase

	include 'commons.f'

	character(80) record(maxhists), context

C task
	taskname = 'binspec 1.4'

C initialize non-common block variables
	timecol = ' '
	column = ' '
	gtistart = ' '
	gtistop = ' '
	binmode = ' '
	rows = ' '
	binrows = 0
	status = 0

C initialize common block variables
	call ibinspec (status)

C  get the parameters from the par file
        call gbinspec (timecol, column, binmode, rows, binrows, indate,
     &                 intime, gtistart, gtistop, gtidate, gtitime,
     &                 epochky, periodky, phstart, phstop, phdate,
     &                 phtime, outcol, copycol, copyprime,
     &                 copyall, history, sensecase, status)
	if (status .ne. 0) goto 999

C check if time column is needed
        if ((gtifile .eq. ' ') .or. (gtifile .eq. '-')) then
           if ((phasefile .eq. ' ') .or. (phasefile .eq. '-')) then
                notimes = .true.
           else
                notimes = .false.
           endif
        endif

C check that all inputs are reasonable
        call ck_input (timecol, column, rows, gtistart, 
     &                 gtistop, epochky, periodky, phstart, phstop,
     &                 sensecase, status)
	if (status .ne. 0) goto 999

C check that the binmode is legal
        call ftupch (binmode)
C       if ((binmode .ne. 'SUM') .and. (binmode .ne. 'MEAN') .and.
C     &      (binmode .ne. 'RATE')) then
C               context = ' Unknown binmode requested: ' // binmode
C               call fcerr (context)
C               status = 1
C               goto 998
C       endif
        if (binmode .ne. 'SUM') then
                context = ' Only SUM more available at this time'
                call fcerr (context)
                status = 1
                goto 998
        endif
 
C get the correct binrows
        if (binrows .le. 0) binrows = nrows

C determine the correct output values
        call init_output (outcol, copycol, status)

C get the output file set up
        nhists = 4
        record(1) = ' TASK:BINSPEC on file ' // infile
        record(2) = '       using GTI file ' // gtifile
        record(3) = '       and phase file ' // phasefile
	record(4) = '    binning mode used ' // binmode
	call make_output (nhists, record, copyall, copyprime, 
     &                    history, status)
	if (status .ne. 0) goto 999

C calculate any time offsets for the GTI and phase files
	call offset (iunit, gunit, punit, ounit, indate, intime,
     &               gtidate, gtitime, phdate, phtime, goffset, poffset,
     &               status)

C do the actual work
	call dobinspec (copycol, binmode, binrows, status)
	if (status .ne. 0) goto 999

C copy any additional output extensions
	if (copyall) call copylast (iunit, ounit, status)

C close up files
998	status = 0
	call ftclos (iunit, status)
	status = 0
	call ftclos (ounit, status)
	status = 0
	if (gunit .gt. 0) call ftclos (gunit, status)
	status = 0
	if (punit .gt. 0) call ftclos (punit, status)

999	if (status .ne. 0) call fcerrm (status)
	return
	end


C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	binvec
C
C DESCRIPTION: 
C	This routine bins the spectrum
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call binvec (status)
C
C ARGUMENTS:
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine binvec (status)

	include 'commons.f'

	integer status
	integer i, j, k, bin

	do 200 k = 1, ncols
	bin = 0
	do 100 j = 1, vrange(k)
	   do 50 i = vstart(j,k), vstop(j,k)
		bin = bin + 1
		outvec(bin,k) = outvec(bin,k) + vector(i,k)
50	   continue
100	continue
200	continue


999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	ck_gti
C
C DESCRIPTION: 
C	check whether GTI related information is correct
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	21 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call ck_gti (gtistart, gtistop, sensecase, status)
C
C ARGUMENTS:
C	gtistart  - name of the gti start column
C	gtistop   - name of the GTI stop column
C	sensecase - whether to be case sensitive
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine ck_gti (gtistart, gtistop, sensecase, status)

	character*(*) gtistart, gtistop
	integer status
	logical sensecase

	integer block, fstatus, varidat, gtfields
	character(160) filename
	character(80) gextname
 
	include 'commons.f'

	character(80) context, gtype(maxcl), gtform(maxcl),
     &                gtunit(maxcl)

C was a GTI file requested?
	if ((gtifile .eq. ' ') .or. (gtifile .eq. '-')) then
		gunit = -gunit
		return
	endif

C now check out the GTI file
	call fcpars (gtifile, filename, gextnumb, status)

C EAG 8/25/93 default to 1st extension
	if (gextnumb .eq. -99) gextnumb = 1

	call ftopen (gunit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Cannot open GTI file: ' // filename
		call fcerr (context)
		goto 999
	endif

C move to the requested extension
	call ftmrhd (gunit, gextnumb, ghtype, status)
	if (status .ne. 0) then
		context = ' Requested GTI extension does not exist'
		call fcerr (context)
		goto 998
	endif

C get the header information
        call ftghbn (gunit, maxcl, gnrows, gtfields, gtype, gtform,
     &               gtunit, gextname, varidat, status)


C check that GTI start and stop columns exist
	call ftgcno (gunit, sensecase, gtistart, gstartno, status)
	if (status .ne. 0) then
		context = ' Requested GTI start column does not exist: '
     &                    // gtistart
		call fcerr (context)
		goto 998
	endif
	call ftgcno (gunit, sensecase, gtistop, gstopno, status)
	if (status .ne. 0) then
		context = ' Requested GTI stop column does not exist: '
     &                    // gtistop
		call fcerr (context)
		goto 998
	endif

C get the first set of GTI information
	call get_more_gtis (0.D0, status)
C error status is returned because 0 isn't in a GTI
	status = 0

	return

C error returns
998	fstatus = 0
	call ftclos (gunit, fstatus)

999	return
	end


C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	ck_infile
C
C DESCRIPTION: 
C	check that input file related inputs are OK
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	21 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call ck_infile (timecol, column, sensecase, status)
C
C ARGUMENTS:
C	timecol   - the name of the input time column
C	column    - the name of the input column to be acted on
C	sensecase - whether to be case sensitive
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 

	subroutine ck_infile (timecol, column, sensecase, status)

	character*(*) timecol, column
	logical sensecase
	integer status

	character(160) filename
	integer block, fstatus, varidat, i
	logical negflag

	include 'commons.f'

	character(80) context, colist(maxcols)

C check that the input file exists
	call fcpars (infile, filename, iextnumb, status)

C EAG 8/25/93 default to 1st extension
	if (iextnumb .eq. -99) iextnumb = 1

	call ftopen (iunit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Cannot open input file:' // filename
		call fcerr (context)
		goto 999
	endif

C move to the requested extension
	call ftmrhd (iunit, iextnumb, ihtype, status)
	if (status .ne. 0) then
		context = ' Requested extension does not exist'
		call fcerr (context)
		goto 998
	endif
	if (ihtype .ne. 2) then
		context = ' Only binary extension supported at this time'
		call fcerr (context)
		goto 998
	endif

C get the header information
	call ftghbn (iunit, maxcl, nrows, tfields, ttype, tform, tunit,
     &               extname, varidat, status)

C check if time column exists
	if (.not. notimes) then
	call ftgcno (iunit, sensecase, timecol, tcolno, status)
	if (status .ne. 0) then
		context = ' Reqested time column does not exist: ' 
     &                    // timecol
		call fcerr (context)
		goto 998
	endif
	endif

C count how many columns were requested
	call fcgcls (column, colist, ncols, negflag)

C loop over all columns to get subset and column number information
	   do 100, i=1, ncols

C parse out any subset information requested
	   call colsubset (iunit, colist(i), sensecase, tform, colno(i),
     &                  vecelem(i), vrange(i), vstart(1,i), vstop(1,i), 
     &                  status)
	   if (status .ne. 0) then
	    context = ' Error parsing column subset data' // colist(i)
	    call fcerr (context)
	    goto 998
	   endif
100	continue

	return

C error returns, close file
998	fstatus = 0
	call ftclos (iunit, fstatus)
999	return
	end


C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	ck_input
C
C DESCRIPTION: 
C	checks that all inputs to the program are reasonable
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	7/21/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call ck_input (timecol, column, rows, gtistart, 
C                            gtistop, epochky, periodky, phstart, 
C                            phstop, sensecase, status)
C
C ARGUMENTS:
C	timecol   - the name of the input time column
C	column    - the name of the input column to operate on
C	rows      - rows to include in the operation
C	gtistart  - the name of the GTI start column
C	gtistop   - the name of the GTI stop column
C	epochky   - the name of the phase epoch keyword
C	periodky  - the name of the phase period keyword
C	phstart   - the name of the phase start column
C	phstop    - the name of the phase stop column
C	sensecase - whether to be case sensitive
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine ck_input (timecol, column, rows, gtistart, 
     &                       gtistop, epochky, periodky, phstart, 
     &                       phstop, sensecase, status)

	character*(*) timecol, column, gtistart, gtistop, rows
	character*(*) epochky, periodky, phstart, phstop
	logical sensecase 
	integer status

	integer fstatus
	logical good
	character(80) context

	include 'commons.f'

C check that the input file exists
	call ck_infile (timecol, column, sensecase, status)
	if (status .ne. 0) goto 999

C now check out the GTI file
	call ck_gti (gtistart, gtistop, sensecase, status)
	if (status .ne. 0) goto 998

C check out the phase file
	call ck_phase (epochky, periodky, phstart, phstop, sensecase, 
     &                 status)
	if (status .ne. 0) goto 997

C check that the row specification is legal
	call fccmpr (rows, good)
	if (.not. good) then
		context = ' Requested row range is not legal: ' // rows
		call fcerr (context)
		status = 1
		goto 996
	endif

C get the requested row ranges
	if (rows .eq. '-') then
		rrange = 1
		rstart(1) = 1
		rstop(1) = nrows
	else
		call fcgrgs (rows, nrows, rrange, rstart, rstop)
	endif

	return

C error returns, close all files first

996	fstatus = 0
	if (punit .gt. 0) call ftclos (punit, fstatus)
997	fstatus = 0
	if (gunit .gt. 0) call ftclos (gunit, fstatus)
998	fstatus = 0
	call ftclos (iunit, fstatus)

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	ck_phase
C
C DESCRIPTION: 
C	Check whether this time has the right phase
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call ck_phase (epochky, periodky, phstart, phstop, 
C                            sensecase, status)
C
C ARGUMENTS:
C	epochky   - the name of the phase epoch keyword
C	periodky  - the name of the phase period keyword
C	phstart   - the name of the phase start column
C	phstop    - the name of the phase stop column
C	sensecase - whether to be case sensitive
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine ck_phase (epochky, periodky, phstart, phstop, 
     &                       sensecase, status)

	logical sensecase
	integer status
	character*(*) epochky, periodky, phstart, phstop

	character(160) filename
	character(80) context, pextname
	integer block, fstatus, varidat, ptfields, pstartno, pstopno
	logical anyf

	include 'commons.f'

	character(80) ptype(maxcl), ptform(maxcl), ptunit(maxcl)

C check if a phase file was requested
	if ((phasefile .eq. ' ') .or. (phasefile .eq. '-')) then
		punit = -punit
		return
	endif

C check out the phase file
	call fcpars (phasefile, filename, pextnumb, status)

C EAG 8/25/93 default to 1st extension
	if (pextnumb .eq. -99) pextnumb = 1

	call ftopen (punit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Cannot open phasefile: ' // filename
		call fcerr (context)
		goto 999
	endif

C move to the requested extension
	call ftmrhd (punit, pextnumb, phtype, status)
	if (status .ne. 0) then
		context = ' Requested phase extension does not exist'
		call fcerr (context)
		goto 998
	endif

C get the header information
        call ftghbn (punit, maxcl, pnrows, ptfields, ptype, ptform,
     &               ptunit, pextname, varidat, status)


C get the epoch start time
	call ftgkyd (punit, epochky, epoch, context, status)
	if (status .ne. 0) then
		context = ' Could not find PH_EPOCH keyword'
		call fcerr (context)
		goto 998
	endif

	call ftgkyd (punit, periodky, period, context, status)
	if (status .ne. 0) then
		context = 'Could not find PH_PRD (phase period) keyword'
		call fcerr (context)
		goto 998
	endif

C check that phase start and stop columns exist
        call ftgcno (punit, sensecase, phstart, pstartno, status)
        if (status .ne. 0) then
                context = 'Requested phase start column does not exist:'
     &                    // phstart
                call fcerr (context)
                goto 998
        endif
        call ftgcno (punit, sensecase, phstop, pstopno, status)
        if (status .ne. 0) then
                context = 'Requested phase stop column does not exist: '
     &                    // phstop
                call fcerr (context)
                goto 998
        endif

C read in the stop and start phase arrays
	call ftgcvd (punit, pstartno, 1, 1, pnrows, 0.D0, pstart, anyf,
     &               status)
	if (status .ne. 0) then
		context = ' Error reading phase start values'
		call fcerr (context)
		goto 998
	endif

        call ftgcvd (punit, pstopno, 1, 1, pnrows, 0.D0, pstop, anyf,
     &               status) 
        if (status .ne. 0) then 
                context = ' Error reading phase stop values'
                call fcerr (context) 
                goto 998 
        endif 

	return

C error, close file
998	fstatus = 0
	call ftclos (punit, fstatus)

999	return
	end


C****************************************************************************** 
C SUBROUTINE:
C	dobinspec
C
C DESCRIPTION: 
C	Calculate the binned spectrum appropriately and write it out
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	22 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C	only binmode=SUM currently supported
C
C USAGE:
C	call dobinspec (copycol, binmode, binrows, status)
C
C ARGUMENTS:
C	copycol - whether to copy other columns to output file
C	binmode - the mode of binning
C	binrows - the number of rows to bin together
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine dobinspec (copycol, binmode, binrows, status)

	character*(*) binmode
	integer status, binrows
	logical copycol

	logical good_gti, good_phase

	include 'commons.f'

	double precision time(maxsize)
	integer remain, irow, nelem, i, j, k, count, naxis1
	integer incol(maxcl), ngtis
	logical anyf
	character(80) context


C initialize things
	count = 0
	do 10 i = 1, tfields
		incol(i) = i
10	continue

C get the offsets correct
	epoch = epoch + poffset
	ngtis = min(maxgti, gnrows)
        do 20 i = 1, ngtis
                tstart(i) = tstart(i) + goffset
                tstop(i) = tstop(i) + goffset
20      continue

C loop over the requested ranges of rows
	do 500 j = 1, rrange

C loop through the input file
	remain = rstop(j) - rstart(j) + 1
	irow = rstart(j)
100	if (remain .gt. maxsize) then
		nelem = maxsize
	else
		nelem = remain
	endif

C get a chunk of times
	if (.not. notimes) call ftgcvd (iunit, tcolno, irow, 1, nelem, 
     &               0.D0, time, anyf, status)

C loop through this chunk of times to see if they are to be used
	do 200 i = 1, nelem
	   if (good_gti(time(i))) then
	      if (good_phase(time(i))) then
		if ((copycol) .and. (count .le. 0)) call fnmcol (iunit,
     &               ounit, irow-1+i, onrows+1, otfields, 1, otform, 
     &               incol, ihtype, status)

		do 150 k = 1, ncols
C this event is good
		call ftgcvd (iunit, colno(k), irow-1+i, 1, vecelem(k),
     &                       0.0D0, vector(1,k), anyf, status)
150		continue
C do whatever with this event
		call binvec (status)
		count = count + 1
		if (count .ge. binrows) then
			call writespec (status)
		if (copycol) call fnmcol (iunit, ounit, irow+i,
     &             onrows+1, otfields, 1, otform, incol, ihtype, status)
			count = 0
		endif
	      endif
	   endif
200	continue

	remain = remain - nelem
	irow = irow + nelem
	if (remain .gt. 0) goto 100

500	continue

C write the result
	if ((count .ge. binrows) .or. ((binrows .eq. nrows) .and.
     &      (count .ne. 0))) call writespec (status)

C re-size the output file extension
	call ftgkyj (ounit, 'NAXIS1', naxis1, context, status)
	call ftddef (ounit, naxis1*onrows, status)
	call ftmkyj (ounit, 'NAXIS2', onrows, '&', status)


999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C      gbinspec
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	21 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gbinspec uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gbinspec (timecol, column, binmode, rows, binrows, 
C                            indate, intime, gtistart, gtistop, gtidate,
C                            gtitime, epochky, periodky, phstart, 
C                            phstop, phdate, phtime, outcol, 
C                            copycol, copyprime, copyall, history,
C                            sensecase, status)
C
C ARGUMENTS:
C	timecol   - the name of the input time oclumn
C	column    - the name of the column to be operated on
C	binmode   - the type of operation to do
C	rows      - the rows to include
C	binrows   - binning factor for the rows
C	indate    - name of the input zero date keyword
C	intime    - name of the input zero time keyword
C	gtistart  - the name of the GTI start column
C	gtistop   - the name of the GTI stop column
C	gtidate   - the name of the GTI zero date keyword
C	gtitime   - name of the GTI zero time keyword
C	epochky   - name of the phase epoch keyword
C	periodky  - name of the phase period keyword
C	phstart   - name of the phase start column
C	phstop    - name of the phase stop column
C	phdate    - name of the phase zero date keyword
C	phtime    - name of the phase zero time keyword
C	outcol    - name(s) of the output vector column(s)
C	copycol   - whether to copy the other columns
C	copyprime - whether to copy the primary array
C	copyall   - whether to copy all other extension
C	history   - whether to write history records
C	sensecase - whether to be case sensitive
C	status    - status of operation
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
	subroutine gbinspec (timecol, column, binmode, rows, binrows, 
     &                       indate, intime, gtistart, gtistop, gtidate,
     &                       gtitime, epochky, periodky, phstart, 
     &                       phstop, phdate, phtime, outcol, 
     &                       copycol, copyprime, copyall, history,
     &                       sensecase, status)

	character*(*) timecol, column, gtistart, gtistop, binmode, rows
	character*(*) indate, intime, gtidate, gtitime
	character*(*) epochky, periodky, phstart, phstop, phdate, phtime
	character*(*) outcol
	integer binrows, status
	logical copycol, copyprime, copyall, history, sensecase

	character(80) context

	include 'commons.f'

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

C  get the name of the GTI file
	call uclgst ('gtifile', gtifile, status)
	if (status .ne. 0) then
	    context = 'could not get GTIFILE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the phase file
	call uclgst ('phasefile', phasefile, status)
	if (status .ne. 0) then
	    context = 'could not get PHASEFILE parameter'
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

C  get the name of the dependant column
	call uclgst ('column', column, status)
	if (status .ne. 0) then
	    context = 'could not get COLUMN parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the vale of the binmode parameter
	call uclgst ('binmode', binmode, status)
	if (status .ne. 0) then
	    context = 'could not get BINMODE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the rows to include
	call uclgst ('rows', rows, status)
	if (status .ne. 0) then
	    context = 'could not get ROWS parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the rows to bin together
	call uclgsi ('binrows', binrows, status)
	if (status .ne. 0) then
	    context = 'could not get BINROWS parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the input file zero date keyword
	call uclgst ('indate', indate, status)
	if (status .ne. 0) then
	    context = 'could not get INDATE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the input file zero time keyword
	call uclgst ('intime', intime, status)
	if (status .ne. 0) then
	    context = 'could not get INTIME parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the GTI start column
	call uclgst ('gtistart', gtistart, status)
	if (status .ne. 0) then
	    context = 'could not get GTISTART parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the GTI stop column
	call uclgst ('gtistop', gtistop, status)
	if (status .ne. 0) then
	    context = 'could not get GTISTOP parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the GTI file zero date keyword
	call uclgst ('gtidate', gtidate, status)
	if (status .ne. 0) then
	    context = 'could not get GTIDATE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the GTI file zero time keyword
	call uclgst ('gtitime', gtitime, status)
	if (status .ne. 0) then
	    context = 'could not get GTITIME parameter'
	    call fcerr(context)
	    goto 999
	endif

C get the epoch keyword name
	call uclgst ('epoch', epochky, status)
        if (status .ne. 0) then
            context = 'could not get EPOCH parameter'
            call fcerr(context)
            goto 999
        endif

C get the period keyword name
        call uclgst ('period', periodky, status)
        if (status .ne. 0) then
            context = 'could not get PERIOD parameter'
            call fcerr(context)
            goto 999
        endif
 
C get the phase file start phase column name
        call uclgst ('phstart', phstart, status)
        if (status .ne. 0) then
            context = 'could not get PHSTART parameter'
            call fcerr(context)
            goto 999
        endif
 
C get the phase file stop phase column name
        call uclgst ('phstop', phstop, status)
        if (status .ne. 0) then
            context = 'could not get PHSTOP parameter'
            call fcerr(context)
            goto 999
        endif
 
C  get the name of the phase file zero date keyword
	call uclgst ('phdate', phdate, status)
	if (status .ne. 0) then
	    context = 'could not get PHDATE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the phase file zero time keyword
	call uclgst ('phtime', phtime, status)
	if (status .ne. 0) then
	    context = 'could not get PHTIME parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output column(s) name(s)
	call uclgst ('outcol', outcol, status)
	if (status .ne. 0) then
	    context = 'could not get OUTCOL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to copy the other columns to the output file
	call uclgsb ('copycol', copycol, status)
	if (status .ne. 0) then
	    context = 'could not get COPYCOL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to copy the primary array
	call uclgsb ('copyprime', copyprime, status)
	if (status .ne. 0) then
	    context = 'could not get COPYPRIME parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to copy all other extensions
	call uclgsb ('copyall', copyall, status)
	if (status .ne. 0) then
	    context = 'could not get COPYALL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to write history records
	call uclgsb ('history', history, status)
	if (status .ne. 0) then
	    context = 'could not get HISTORY parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to to be case sensitive
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
C****************************************************************************** 
C SUBROUTINE:
C	get_more_gtis
C
C DESCRIPTION: 
C	routine reads in GTIs from a file as needed
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C	Currently only up to maxgti can be specified
C
C USAGE:
C	call get_more_gtis (time, status)
C
C ARGUMENTS:
C	time    - the time for which a gti is not currently in memory
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine get_more_gtis (time, status)

	double precision time
	integer status

	include 'commons.f'

	integer grow, nelem
	logical anyf

	data grow /1/

C if no gti, just return
	if (gunit .lt. 0) return

	if (gnrows .lt. maxgti) then
		nelem = gnrows
	else
		nelem = maxgti
	endif

	call ftgcvd (gunit, gstartno, grow, 1, nelem, 0.D0, tstart,
     &               anyf, status)
	call ftgcvd (gunit, gstopno, grow, 1, nelem, 0.D0, tstop,
     &               anyf, status)

C check that the requested time is in this set
	if ((time .lt. tstart(1)) .or. (time .gt. tstop(nelem))) then
	    if (nelem .eq. gnrows) then
C this time is outside all GTI ranges, return with error
		status = 1
	    else 
C NOTE:  put stuff in here to read in part of GTI file
c		if ((time .lt. tstart(1) .and. (grow .gt. 1)) then
c		    grow = grow - nelem
c		    if (grow .le. 0) grow = 1
c		    goto 10
c		else
c		    status = 1
c		endif
		status = 1
	    endif
	endif


999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	good_gti
C
C DESCRIPTION: 
C	returns whether the time is within a GTI or not
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	21 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	if good_gti (time)
C
C ARGUMENTS:
C	time - the time to check
C
C PRIMARY LOCAL VARIABLES:
C	status  - status of operation
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	logical function good_gti (time)

	double precision time
	integer status

	integer ngtis, i

	include 'commons.f'

	status = 0
	good_gti = .true.
	if (gunit .lt. 0) return
	ngtis = min(maxgti, gnrows)

10	if (((time .lt. tstart(1)) .or. (time .gt. tstop(ngtis))) .and. 
     &       (maxgti .lt. gnrows) .and. (status .eq. 0)) then
		call get_more_gtis (time, status)
C correct for any offset
        	do 20 i = 1, ngtis
                    tstart(i) = tstart(i) + goffset
                    tstop(i) = tstop(i) + goffset
20      continue

		goto 10
	else if (status .ne. 0) then
C time is outside bounds of GTIs
		status = 0
		good_gti = .false.
		return
	endif

C loop through GTIs to find
C NOTE this could be made more efficient since the events are probably
C      going to be time ordered

	do 100 i = 1, ngtis
	   if ((time .gt. tstart(i)) .and. (time .le. tstop(i))) return
100	continue
	good_gti = .false.

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	good_phase
C
C DESCRIPTION: 
C	return whether the time is in the correct phase
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	21 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	if good_phase (time)
C
C ARGUMENTS:
C	time - the time to check the phase of
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	logical function good_phase (time)

	double precision time

	include 'commons.f'

	integer i
	double precision phase

	good_phase = .true. 
	if (pnrows .le. 0) return

	good_phase = .false.
C determine the phase of this time
	phase = (time - epoch) - period * int((time-epoch)/period)
	phase = phase / period

	do 100 i = 1, pnrows
	  if ((phase .gt. pstart(i)) .and. (phase .lt. pstop(i))) then
		good_phase = .true.
		return
	  endif
100	continue

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	ibinspec
C
C DESCRIPTION: 
C	initialize all variables
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call ibinspec (status)
C
C ARGUMENTS:
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine ibinspec (status)

	integer status, i, j

	include 'commons.f'


C input file information
	iunit = 15
	nrows = 0
	notimes = .false.

C output file information
	ounit = 16
	onrows = 0

C gti file information
	gunit = 17
	gnrows = 0

C gti data information

C phase file information
	punit = 18
	pnrows = 0

C data values
	do 20 j = 1, maxcols
	    do 10 i = 1, maxvec
		outvec(i,j) = 0
10	    continue
20	continue

C filenames
	infile = ' '
	outfile = ' '
	gtifile = ' '
	phasefile = ' '

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	init_output
C
C DESCRIPTION: 
C	Initialize the output file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call  init_output (outcol, copycol, status)
C
C ARGUMENTS:
C	outcol  - name of the output column(s)
C	copycol - whether to copy all other columns to the output file
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine  init_output (outcol, copycol, status)

	logical copycol
	character*(*) outcol

	include 'commons.f'

	integer status

	character(20) dtype, tdisp
	character(80) ocolist(maxcols), context
	double precision tscal, tzero
	integer repeat, i, j, tnull, ipos, ocols
	logical negflag

C if we are to copy all the other columns, put the new output in the
C same place
	if (copycol) then
		do 10 i = 1, tfields
			ottype(i) = ttype(i)
			otform(i) = tform(i)
			otunit(i) = otunit(i)
10		continue
		otfields = tfields
		do 15 i = 1, ncols
			outcolno(i) = colno(i)
15		continue
	else
		otfields = ncols
		do 20 i = 1, ncols
			outcolno(i) = i
			ottype(i) = ttype(colno(i))
			otunit(i) = tunit(colno(i))
20		continue
	endif

C check if output column name(s) were specified
        if ((outcol .ne. ' ') .and. (outcol .ne. '-')) then
            call fcgcls (outcol, ocolist, ocols, negflag)
            if (ocols .ne. ncols) then
                context = ' Incorrect number of out columns' // outcol
                call fcerr (context)
                status = 1
                goto 999
            endif
	    do 30 i = 1, ncols
		ottype(outcolno(i)) = ocolist(i)
30	    continue
        endif
	
C find the output number of vector elements 
	do 150 j = 1, ncols
        ovecelem(j) = 0.
        do 100 i = 1, vrange(j)
                ovecelem(j) = ovecelem(j) + vstop(i,j) - vstart(i,j) + 1.
100     continue

C and set tform accordingly
        if (ovecelem(j) .eq. vecelem(j)) then
            otform(outcolno(j)) = tform(colno(j)) 
	else
	   call ftgbcl (iunit, colno(j), ttype(colno(j)),
     &                  tunit(colno(j)), dtype, repeat, tscal, tzero,
     &                  tnull, tdisp, status)
	    write (otform(outcolno(j)), 1001) ovecelem(j), dtype
1001	    format (f10.0,A)
	endif

C for short data type, set to long to avoid overflows
	ipos = index(otform(outcolno(j)), 'I')
	if (ipos .gt. 0) otform(outcolno(j))(ipos:ipos) = 'J'

150	continue

C        outextname = extname 
	outextname = ' '

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	make_output
C
C DESCRIPTION: 
C	create the output file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	22 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call make_output (nhists, record, copyall, 
C                               copyprime, history, status)
C
C ARGUMENTS:
C	nhists     - number of history records to write
C	record     - the array of history records
C	copyall    - whether to copy all other extensions
C	copyprime  - whether to copy the primary array
C	history    - whether to write history records
C	status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine make_output (nhists, record, copyall, 
     &                          copyprime, history, status)

	logical copyall, copyprime, history 
	integer status

	integer nhists, fstatus, i
	character(80) record(nhists), context

	include 'commons.f'


C open the output file
	call ftinit (ounit, outfile, 2880, status)
	if (status .ne. 0) then
		context = ' Error opening output file, exists? ' 
     &                    // outfile
		call fcerr (context)
		goto 999
	endif

C create primary and copy additional extensions, if requested
        call copyfirst (iunit, ounit, iextnumb, copyprime, copyall,
     &                  history, nhists, record, status)
	if (status .ne. 0) goto 998

C move to the correct input file extension
	call ftmahd (iunit, iextnumb+1, ihtype, status)
	if (status .ne. 0) then
		context = ' Error moving to input file extension'
		call fcerr (context)
		goto 998
	endif

C create the output extension
	call ftcrhd (ounit, status)

	call ftphbn (ounit, onrows, otfields, ottype, otform, otunit, 
     &               outextname, 0, status)
	call xcopynoscale (iunit, ounit, status)
	if (history) then
		do 10 i = 1, nhists
			call ftphis (ounit, record(i), status)
10		continue
C		call fptime (ounit, status)
	endif
	call ftbdef (ounit, otfields, otform, 0, onrows, status)
	if (status .ne. 0) then
		context = ' Error creating output extension'
		call fcerr (context)
		goto 998
	endif

	return

C error return
998	fstatus = 0
	call ftclos (ounit, fstatus)
999	return
	end

C****************************************************************************** 
C******************************************************************************
C SUBROUTINE:
C      offset
C
C DESCRIPTION:
C      Determines the time offset for the GTI and Phase files
C
C AUTHOR/DATE:
C      Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C       4/18/96 MJT - using xftgkys for indate, gtidate, phdate to
C                     catch I/F pairs if present
C
C NOTES:
C
C USAGE:
C	call offset (iunit, gunit, punit, ounit, indate, intime, 
C                          gtidate, gtitime, phdate, phtime, goffset,
C                          poffset, status)
C
C ARGUMENTS:
C	iunit   - input file unit number
C	gunit   - gti file unit number
C	punit   - phase file unit number
C	ounit   - output file unit number
C	indate  - input file zero date keyword name
C	intime  - input file zero time keyword name
C	gtidate - GTI file zero date keyword name
C	gtitime - GTI file zero time keyword name
C	phdate  - phase file zero date keyword name
C	phtime  - phase file zero time keyword name
C	goffset - GTI file offset value (to be added to GTI times)
C	poffset - phase file offset value (to be added to phase times)
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine offset (iunit, gunit, punit, ounit, indate, intime, 
     &                     gtidate, gtitime, phdate, phtime, goffset,
     &                     poffset, status)

	integer iunit, gunit, punit, ounit
	character*(*) indate, intime, gtidate, gtitime, phdate, phtime
	double precision goffset, poffset
	integer status

	double precision idayno, gdayno, pdayno
	character(80) context, date, time, daystr, timestr
	integer iday, imonth, iyear, ihour, imin
	real isec
	integer gday, gmonth, gyear, ghour, gmin
	real gsec
	integer pday, pmonth, pyear, phour, pmin
	real psec
	integer fccmtd
	logical julian

C initialize to no offset
	goffset = 0.D0
	poffset = 0.D0

C determine if there is any zero date checking requested
	if ((indate .eq. ' ') .or. (indate .eq. '-')) return

C determine the format of the input file zero date
	call xftgkyd (iunit, indate, idayno, daystr, status)
	if ((status .ne. 202) .and. (status .ne. 0)) then
C the keyword wasn't a double
		status = 0
		julian = .false.
		call xftgkys (iunit, indate, date, daystr, status)
		call fts2dt (date, iyear, imonth, iday, status)
		call ftgkys (iunit, intime, time, timestr, status)
		call fiptim (time, ihour, imin, isec, status)
	else
		julian = .true.
	endif
	if (status .ne. 0) then
	   context = 'error determining infile zero date/time' // indate
	   call fcerr (context)
	   goto 999
	endif

C do we have any GTI zero information?
	if ((gtidate .eq. ' ') .or. (gtidate .eq. '-') .or.
     &      (gunit .le. 0)) goto 500

C if so, determine the values
	if (julian) then
        	call xftgkyd (gunit, gtidate, gdayno, context, status)
	else
                call xftgkys (gunit, gtidate, date, context, status)
                call fts2dt (date, gyear, gmonth, gday, status)
                call ftgkys (gunit, gtitime, time, context, status)
                call fiptim (time, ghour, gmin, gsec, status)
	endif
	if (status .ne. 0) then
           context = 'error determining GTI zero date/time' // gtidate
	   call fcerr (context)
           goto 999
        endif

C and determine the gti offset
	if (julian) then
	    goffset = (gdayno - idayno) * 86400.0
	else
C NOTE need to worry about leap years!
	    goffset = (gyear - iyear) * 365
	    goffset = goffset + fccmtd(gyear,gmonth) 
     &                        - fccmtd(iyear,imonth)
	    goffset = (goffset + gday - iday) * 24
	    goffset = (goffset + ghour - ihour) * 60
	    goffset = (goffset + gmin - imin) * 60
	    goffset = goffset + gsec - isec
	endif

C do we have any phase zero time checking to do?
500     if ((phdate .eq. ' ') .or. (phdate .eq. '-') .or.
     &      (punit .le. 0)) goto 999

C if so, determine the values
        if (julian) then
                call xftgkyd (punit, phdate, pdayno, context, status)
        else
                call xftgkys (punit, phdate, date, context, status)
                call fts2dt (date, pyear, pmonth, pday, status)
                call ftgkys (punit, phtime, time, context, status)
                call fiptim (time, phour, pmin, psec, status)
        endif
        if (status .ne. 0) then
           context = 'error determining phase zero date/time' // phdate
           call fcerr (context)
           goto 999
        endif

C and determine the phase offset
        if (julian) then
            poffset = (pdayno - idayno) * 86400.0
        else
C NOTE need to worry about leap years!
            poffset = (pyear - iyear) * 365
            poffset = poffset + fccmtd(pyear,pmonth)
     &                        - fccmtd(iyear,imonth)
            poffset = (poffset + pday - iday) * 24
            poffset = (poffset + phour - ihour) * 60
            poffset = (poffset + pmin - imin) * 60
            poffset = poffset + psec - isec
        endif

999     return
        end

C******************************************************************************
C****************************************************************************** 
C SUBROUTINE:
C	writespec
C
C DESCRIPTION: 
C	write the binned spectrum to the output file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	27 July, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call writespec (status)
C
C ARGUMENTS:
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine writespec (status)

	integer status

	integer i, j, nelem

	include 'commons.f'

	onrows = onrows + 1

	do 200 j = 1, ncols
	nelem = ovecelem(j)
	call ftpcld (ounit, outcolno(j), onrows, 1, nelem,
     &               outvec(1,j), status)

C re-initialize outvec
	do 100 i = 1, nelem
		outvec(i,j) = 0.
100	continue
200	continue

999	return
	end

C****************************************************************************** 
