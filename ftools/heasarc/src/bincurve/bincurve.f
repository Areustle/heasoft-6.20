C****************************************************************************** 
C SELECTOR TASK:
C      bincurve
C
C FILE:
C      bincurve.f 
C
C DESCRIPTION: 
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C	version 1.1 Add phase file support
C
C NOTES:
C      bincurve supported in IRAF and HOST environments
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
	subroutine bincue

	character(80) timecol, column, gtistart, gtistop, binmode, rows
	character(80) outtimecol, outcol, outerr, outlive
	character(80) indate, intime, gtidate, gtitime, phdate, phtime
	character(80) epochky, periodky, phstart, phstop

	integer status, nhists
	logical copyprime, copyall, history, sensecase
	logical getmin, getmax

	include 'commons.f'

	character(80) record(maxhists), context
	integer fstatus

C task
	taskname = 'bincurve 1.1'

C initialize non-common block variables
	timecol = ' '
	column = ' '
	gtistart = ' '
	gtistop = ' '
	binmode = ' '
	rows = ' '
	outtimecol = ' '
	outcol = ' '
	outerr = ' '
	outlive = ' '
	status = 0

C initialize common block variables
	call ibinspec (status)

C  get the parameters from the par file
        call gbincurve (timecol, column, binmode, rows, indate, intime, 
     &                  gtistart, gtistop, gtidate, gtitime, epochky, 
     &                  periodky, phstart, phstop, phdate, phtime, 
     &                  outtimecol, outcol, outerr, outlive, copyprime, 
     &                  copyall, history, sensecase, getmin, getmax, 
     &                  status)
	if (status .ne. 0) goto 999

C check that the binmode is legal
        call ftupch (binmode)
       if ((binmode .ne. 'SUM') .and. (binmode .ne. 'MEAN') .and.
     &      (binmode .ne. 'RATE') .and. (binmode .ne. 'EVENT_SUM')
     &      .and. (binmode .ne. 'EVENT_RATE')) then
               context = ' Unknown binmode requested: ' // binmode
               call fcerr (context)
               status = 1
               goto 999
       endif
 
C check that all inputs are reasonable
        call ck_input (timecol, column, rows, gtistart,
     &                       gtistop, epochky, periodky, phstart,
     &                       phstop, sensecase, status)
	if (status .ne. 0) goto 999

C determine the light curve binning limits
	call binlimits (getmin, getmax, status)
	if (status .ne. 0) goto 998

C set up the output file
	call icrvout (binmode, outtimecol, outcol, outerr, outlive, 
     &                status)
	if (status .ne. 0) goto 998

C create the output file
        nhists = 4
        record(1) = ' TASK:BINCURVE on file ' // infile
        record(2) = '       using GTI file ' // gtifile
        record(3) = '       and phase file ' // phasefile
	record(4) = '    binning mode used ' // binmode
	call make_output (nhists, record, copyall, copyprime, history, 
     &                    status)
	if (status .ne. 0) goto 998
	
C calculate any time offsets for the GTI and phase files
        call offset (iunit, gunit, punit, ounit, indate, intime,
     &               gtidate, gtitime, phdate, phtime, goffset, poffset,
     &               status)

C do the actual work
	call dobincurve (binmode, outerr, outlive, status)
	if (status .ne. 0) goto 998

C copy any additional output extensions
	if (copyall) call copylast (iunit, ounit, status)

C close up files
998	fstatus = 0
	call ftclos (iunit, fstatus)
	fstatus = 0
	call ftclos (ounit, fstatus)
	fstatus = 0
	if (gunit .gt. 0) call ftclos (gunit, fstatus)
	fstatus = 0
	if (punit .gt. 0) call ftclos (punit, fstatus)

999	if (status .ne. 0) call fcerrm (status)
	return
	end


C****************************************************************************** 
C SUBROUTINE:
C	addcurve
C
C DESCRIPTION: 
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call addcurve (time, binmode, status)
C
C ARGUMENTS:
C	time    - the time of this event
C	binmode - the binning mode
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine addcurve (time, binmode, status)

	double precision time
	character*(*) binmode
	integer status

	include 'commons.f'

	integer i, bin

C return if outside requested range
	if (time .lt. lowval) return
	if (time .gt. highval) return

C determine the bin for this event
	bin = (time - lowval) / binsize + 1
	if (bin .gt. nbins) bin = nbins
	if (bin .lt. 1) bin = 1

C loop over all requested columns
	do 100 i = 1, ncols

C do the appropriate action based on binmode
	    if (index(binmode, 'EVENT') .gt. 0) then
C are only counting number of events
		values(bin,i) = values(bin,i) + 1
	    else
c are adding up PHAS
		values(bin,i) = values(bin,i) + ovecelem(i)
C note are adding ncols times too many counts here.  Divide in writecurve
		counts(bin) = counts(bin) + 1
	    endif

100	continue

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C
C DESCRIPTION: 
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
C	call binelem (status)
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
	subroutine binelem (status)

	integer status

	include 'commons.f'

	integer i, j, k

C loop over multiple columns
	do 300 k = 1, ncols
	   ovecelem(k) = 0

C loop over ranges
	   do 200 j = 1, vrange(k)

		do 100 i = vstart(j,k), vstop(j,k)
		    ovecelem(k) = ovecelem(k) + vector(i,k)
100		continue
200	   continue
300	continue

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	binlimits
C
C DESCRIPTION: 
C	determine the high and low time value, binsize and number of bins
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call binlimits (getmin, getmax, status)
C
C ARGUMENTS:
C	getmin  - whether to get the minimum time value from the file
C	getmax  - whether to get the maximum time value from the file
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine binlimits (getmin, getmax, status)

	logical getmin, getmax
	integer status

	include 'commons.f'

	integer nelem, remain, felem, frow, i
	logical anyf
	double precision time(1024)
	character(80) context

C determine the upper and lower limits of the light curve
	if (getmin .or. getmax) then
	    felem = 1
	    frow = 1
	    remain = nrows
10	    if (remain .gt. maxsize) then
		nelem = maxsize
	    else
		nelem = remain
	    endif
	    call ftgcvd (iunit, tcolno, frow, felem, nelem, 0.D0,
     &                   time, anyf, status)
	    if (frow .eq. 1) then
		if (getmin) lowval = time(1)
		if (getmax) highval = time(1)
	    endif

	    if (getmin) then
		do 20 i = 1, nelem
		    if (time(i) .lt. lowval) lowval = time(i)
20		continue
	    endif

	    if (getmax) then
		do 30 i = 1, nelem
		    if (time(i) .gt. highval) highval = time(i)
30		continue
	    endif

	    remain = remain - nelem
	    frow = frow + nelem
	    if (remain .gt. 0) goto 10

	endif

C determine the bin_size and number of bins
	if (binsize .le. 0) then
		nbins = 100
		binsize = (highval - lowval) / (nbins - 1)
	else
		nbins = (highval - lowval) / binsize + 1
	endif

C currently, limit number of bins
	if (nbins .gt. maxbins) then
		write (context, 1000) nbins
1000		format (' Too many bins requested:', i10)
		call fcerr (context)
		status =1 
	endif

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	dobincurve
C
C DESCRIPTION: 
C	Calculate the binned light curve appropriately and write it out
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call dobincurve (binmode, outerr, outlive, status)
C
C ARGUMENTS:
C	binmode - the mode of binning
C	outerr  - the name(s) of the output error column(s)
C	outlive - the name of the output livetime column
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine dobincurve (binmode, outerr, outlive, status)

	character*(*) binmode, outerr, outlive
	integer status

	logical good_gti, good_phase

	include 'commons.f'

	double precision time(maxsize)
	integer remain, irow, nelem, i, j, k, count, naxis1, ngtis
	logical anyf
	character(80) context


C initialize things
	count = 0

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

C loop over the requested columns
		do 150 k = 1, ncols
C this event is good
		call ftgcvd (iunit, colno(k), irow-1+i, 1, vecelem(k),
     &                       0.D0, vector(1,k), anyf, status)
150		continue

C do whatever with this event
C bin the elements in the vector appropriately
		call binelem (status)

C add this event to the correct part of the light curve
		call addcurve (time(i), binmode, status)

	      endif
	   endif
200	continue

	remain = remain - nelem
	irow = irow + nelem
	if (remain .gt. 0) goto 100

500	continue

C write the result
	call writecurve (binmode, outerr, outlive, status)

C re-size the output file extension
	call ftgkyj (ounit, 'NAXIS1', naxis1, context, status)
	call ftddef (ounit, naxis1*nbins, status)
	call ftmkyj (ounit, 'NAXIS2', nbins, '&', status)

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	gbincurve
C
C DESCRIPTION: 
C	get parameters from parameter file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call gbincurve (timecol, column, binmode, rows, indate, 
C                             intime, gtistart, gtistop, gtidate, 
C                             gtitime, epochky, periodky, phstart, 
C                             phstop, phdate, phtime, outtimecol, 
C                             outcol, outerr, outlive, copyprime, 
C                             copyall, history, sensecase, getmin, 
C                             getmax, status)
C
C ARGUMENTS:
C       timecol    - the name of the input time oclumn
C       column     - the name of the column(s) to be operated on
C       binmode    - the type of operation to do
C       rows       - the rows to include
C	indate     - name of the input file zero date keyword
C	intime     - name of the input file zero time keyword
C       gtistart   - the name of the GTI start column
C       gtistop    - the name of the GTI stop column
C	gtidate    - name of the GTI file zero date keyword
C	gtitime    - name of the GTI file zero time keyword
C	epochky    - name of the phase file epoch keyword
C	periodky   - name of the phase file period keyword
C	phstart    - name of the phase file start column
C	phstop     - name of the phase file stop column
C	phdate     - name of the phase file zero date keyword
C	phtime     - name of the phase file zero time keyword
C	outtimecol - the name of the output time column
C	outcol     - the name(s) of the output columns
C	outerr     - the name(s) of the output error columns
C	outlive    - the name of the output livetime
C       copyprime  - whether to copy the primary array
C       copyall    - whether to copy all other extension
C       history    - whether to write history records
C       sensecase  - whether to be case sensitive
C	getmin     - whether to determine min time from file
C	getmax     - whether to determine max time from file
C       status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine gbincurve (timecol, column, binmode, rows, indate, 
     &                        intime, gtistart, gtistop, gtidate, 
     &                        gtitime, epochky, periodky, phstart, 
     &                        phstop, phdate, phtime, outtimecol, 
     &                        outcol, outerr, outlive, copyprime, 
     &                        copyall, history, sensecase, getmin, 
     &                        getmax, status)

	character*(*) timecol, column, gtistart, gtistop, binmode, rows
	character*(*) outtimecol, outcol, outerr, outlive
	character*(*) indate, intime, gtidate, gtitime, phdate, phtime
	character*(*) epochky, periodky, phstart, phstop
	integer status
	logical copyprime, copyall, history, sensecase

	character(80) context
	logical getmin, getmax

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

C  get the size of bins, INDEF for 100 bins total
	call uclgsd ('binsize', binsize, status)
	if (status .eq. 3) then
	    status = 0
	    binsize = -1
	else if (status .ne. 0) then
	    context = 'could not get BINSIZE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the lower limit of time for binning
	getmin = .false.
	call uclgsd ('lowval', lowval, status)
	if (status .eq. 3) then
	    status = 0
	    getmin = .true.
	else if (status .ne. 0) then
	    context = 'could not get LOWVAL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the upper limit of time for binning
	getmax = .false.
	call uclgsd ('highval', highval, status)
	if (status .eq. 3) then
	    status = 0
	    getmax = .true.
	else if (status .ne. 0) then
	    context = 'could not get HIGHVAL parameter'
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
	call ftupch (binmode)

C  get the rows to include
	call uclgst ('rows', rows, status)
	if (status .ne. 0) then
	    context = 'could not get ROWS parameter'
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

C  get the name of the output time column
	call uclgst ('outtimecol', outtimecol, status)
	if (status .ne. 0) then
	    context = 'could not get OUTTIMECOL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output column(s)
	call uclgst ('outcol', outcol, status)
	if (status .ne. 0) then
	    context = 'could not get OUTCOL parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output error column(s)
	call uclgst ('outerr', outerr, status)
	if (status .ne. 0) then
	    context = 'could not get OUTERR parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output livetime column
	call uclgst ('outlive', outlive, status)
	if (status .ne. 0) then
	    context = 'could not get OUTLIVE parameter'
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
C	icrvout
C
C DESCRIPTION: 
C	Initialize the output file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call icrvout (binmode, outtimecol, outcol, outerr, 
C                           outlive, status)
C
C ARGUMENTS:
C	binmode    - the type of binning operation to perform
C	outtimecol - the name of the output time column
C	outcol     - the name(s) of the output column(s)
C	outerr     - the name(s) of the output error column(s)
C	outlive    - the name of the output livetime column
C	status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine icrvout (binmode, outtimecol, outcol, outerr, 
     &                      outlive, status)

	character*(*) outtimecol, outcol, outerr, outlive, binmode

	include 'commons.f'

	integer status
	character(80) context, oerrlist(maxcols), ocolist(maxcols)

	integer i, ocols, fcstln
	logical negflag

	otfields = 1

C set up the ttype, tform and tunit values
C for the time column
	if ((outtimecol .eq. ' ') .or. (outtimecol .eq. '-')) then
		ottype(otfields) = ttype(tcolno)
	else
		ottype(otfields) = outtimecol
	endif
	otform(otfields) = tform(tcolno)
	otunit(otfields) = tunit(tcolno)

C for event binmodes, force one output column
	if (index(binmode,'EVENT') .gt. 0) ncols = 1

C must have some columns
	if (ncols .le. 0) then
		context = ' Please specify input column'
		call fcerr (context)
		status = 1
		goto 999
	endif

C get the list of column names, if any
	if ((outcol .ne. ' ') .and. (outcol .ne. '-')) then
	    call fcgcls (outcol, ocolist, ocols, negflag)
	    if (ocols .ne. ncols) then
		context = ' Incorrect number of out columns' // outcol
		call fcerr (context)
		status = 1
		goto 999
	    endif
	endif

C get the list of error column names, if any
	if ((outerr .ne. ' ') .and. (outerr .ne. '-') .and.
     &      (outerr .ne. 'NONE')) then
	    call fcgcls (outerr, oerrlist, ocols, negflag)
	    if (ocols .ne. ncols) then
		context = ' Incorrect number of error columns' // outcol
		call fcerr (context)
		status = 1
		goto 999
	    endif
	endif

C for all requested columns
	do 10 i = 1, ncols
		otfields = otfields + 1
		outcolno(i) = otfields
		if ((outcol .eq. ' ') .or. (outcol .eq. '-')) then
		    ottype(otfields) = ttype(colno(i))
		    if (index(binmode,'EVENT') .gt. 0) 
     &                  ottype(otfields) = binmode
		else
		    ottype(otfields) = ocolist(i)
		endif

		if (ihtype .eq. 1) then
		    otform(otfields) = 'F23.15'
		else
		    otform(otfields) = 'D'
		endif

		if (binmode .eq. 'RATE') then
		    otunit(otfields) = 'counts/s'
		else if (binmode .eq. 'EVENT_RATE') then
		    otunit(otfields) = 'events/s'
		else if (binmode .eq. 'EVENT_SUM') then
		    otunit(otfields) = 'events'
		else
		    otunit(otfields) = 'counts'
		endif

C output error column
		if (outerr .ne. 'NONE') then
		    otfields = otfields + 1
		    if ((outerr .eq. ' ') .or. (outerr .eq. '-')) then
			ottype(otfields) = 
     &       ottype(otfields-1)(1:fcstln(ottype(otfields-1))) // '_ERR'
		    else
			ottype(otfields) = oerrlist(i)
		    endif
		    otform(otfields) = otform(otfields-1)
		    otunit(otfields) = otunit(otfields-1)
		endif
10	continue

C output livetime column
	if ((outlive .ne. ' ') .and. (outlive .ne. '-')) then
		otfields = otfields + 1
		ottype(otfields) = outlive
		if (ihtype .eq. 1) then
		    otform(otfields) = 'F23.15'
		else
		    otform(otfields) = 'D'
		endif
		otunit(otfields) = ' '
	endif
		    
C        outextname = extname 
	outextname = ' '

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	livetime
C
C DESCRIPTION: 
C	calculate the amount of livetime in each bin
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call livetime (bin_center, nelem, live, status)
C
C ARGUMENTS:
C	bin_center - array of bin centers
C	nelem      - number of bin centers
C	live       - returned array of nelem fractional livetimes
C	status     - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine livetime (bin_center, nelem, live, status)

	integer nelem, status
	double precision bin_center(nelem)
	double precision live(nelem)

	include 'commons.f'

	double precision bin_start, bin_stop
	integer i, j

C if no gti, live time is 1.0
	if (gnrows .le. 0) then
	    do 10 i = 1, nelem
		live(i) = 1.D0
10	    continue
	    return
	endif

C loop over all bins
	do 100 i = 1, nelem

C initialize some things
	    live(i) = 0.D0
	    bin_start = bin_center(i) - binsize / 2.
	    bin_stop = bin_center(i) + binsize / 2.

C calculate the live time in this bin
C loop over good time intervals
C NOTE this may have to be re-thought out for phases
	    do 20 j = 1, gnrows
C the start time is within this GTI
		if ((bin_start .ge. tstart(j)) .and. 
     &              (bin_start .lt. tstop(j))) then
	    	    if (bin_stop .le. tstop(j)) then
C both start and stop in same interval
			live(i) = live(i) + bin_stop - bin_start
			goto 90
	    	    else
			live(i) = live(i) + tstop(j) - bin_start
	    	    endif

C the start time is between GTIs, before this interval
C or the bin is not all in one GTI interval
		else if (bin_start .lt. tstart(j)) then
		    if (bin_stop .lt. tstart(j)) then
C end of bin was between GTIs
			goto 90
		    else if (bin_stop .le. tstop(j)) then
C end was in this bin
			live(i) = live(i) + bin_stop - tstart(j)
			goto 90
		    else 
C haven't found the end yet
			live(i) = live(i) + tstop(j) - tstart(j)
		    endif
		else
C have not found the start interval yet
C this section executed if bin_start .gt. tstart and bin_stop .gt. tstop
		    continue
		endif
20	    continue

C normalize
90	    live(i) = live(i) / binsize

100	continue

999	return
	end

C****************************************************************************** 
C****************************************************************************** 
C SUBROUTINE:
C	writecurve
C
C DESCRIPTION: 
C	write out the collected light curve information
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	August, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call writecurve (binmode, outerr, outlive, status)
C
C ARGUMENTS:
C	binmode - the binning mode
C	outerr  - name(s) of the output error column(s)
C	outlive - name of the output livetime column
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine writecurve (binmode, outerr, outlive, status)

	character*(*) outerr, binmode, outlive
	integer status

	include 'commons.f'

	double precision bin_center(maxsize), bin_err(maxsize)
	double precision  live(maxsize)
	integer i, j, frow, nelem, remain

	frow = 1
	remain = nbins
100	if (nbins .gt. maxsize) then
	    nelem = maxsize
	else
	    nelem = nbins
	endif

C calculate the bin centers
	bin_center(1) = lowval + binsize / 2. + binsize*(frow-1)
	do 10 i = 1, nelem-1
	    bin_center(i+1) = bin_center(i) + binsize
10	continue

C write the bin centers to the output file
	call ftpcld (ounit, 1, frow, 1, nelem, bin_center,
     &               status)

C calculate the livetime
C NOTE live time is the fraction of good times in each bin
        call livetime (bin_center, nelem, live, status)
 
C loop over the requested columns
	do 200 i = 1, ncols

C Determine the output based on binmode
	    if (index(binmode,'RATE') .gt. 0) then
		do 110 j = 1, nbins
C the live time is the fraction of the total bin that is a good time
		    if (live(j) .gt. 0.D0) then
			bin_err(j) = sqrt(abs(values(j,i))) / 
     &                                         (live(j) * binsize)
			values(j,i) = values(j,i) / (live(j) * binsize)
		    else
			bin_err(j) = 0.D0
			values(j,i) = 0.D0
		    endif
		   
110		continue
	    else if (index(binmode, 'SUM') .gt. 0) then
		do 120 j = 1, nbins
		    bin_err(j) = sqrt(abs(values(j,i)))
120		continue
	    else
		do 130 j = 1, nbins
		    if (counts(i) .gt. 0) then
			bin_err(j) = sqrt(abs(values(j,i))) / counts(i)

C note we counted ncols times too many events/bin in addcurve
			values(j,i) = values(j,i) / counts(i) / ncols
		    else
			bin_err(j) = 0.D0
			values(j,i) = 0.D0
		    endif
130		continue
	    endif

C now write out the values	
	    call ftpcld (ounit, outcolno(i), frow, 1, nelem, 
     &                   values(1,i), status)

C and the error, if there is any
	    if (outerr .ne. 'NONE') call ftpcld (ounit, outcolno(i)+1,
     &            frow, 1, nelem, bin_err, status)

200	continue
 
C write out the livetime
	if ((outlive .ne. ' ') .and. (outlive .ne. '-')) 
     &       call ftpcld (ounit, otfields, frow, 1, nelem, live, status)

	frow = frow + nelem
	remain = remain - nelem
	if (remain .gt. 0) goto 100

999	return
	end

C****************************************************************************** 
