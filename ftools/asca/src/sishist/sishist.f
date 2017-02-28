C  FTOOLs info $Header: /headas/headas/ftools/asca/src/sishist/sishist.f,v 3.8 2013/05/21 19:08:07 irby Exp $
C      
C****************************************************************************** 
C SELECTOR TASK:
C      sishist
C
C FILE:
C      sishist.f 
C
C DESCRIPTION: 
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C       3/29/94 JCI Modified to start from 0 pixels rather than 1
C               and to use same normalization as SISCLEAN.
C NOTES:
C      sishist supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
	subroutine sishit

	integer maxfiles
	parameter (maxfiles=150)

	character(160) infile, outfile, filelist(maxfiles)
	character(80) rawxname, rawyname, ccdidname, rows, context
	logical fit, sensecase

	integer maxx, maxy, maxccd, maxcounts
	parameter (maxx = 640)
	parameter (maxy = 640)
	parameter (maxccd = 4)
	parameter (maxcounts = 100)

	integer iunit, ounit, rawxno, rawyno, ccdidno, fstatus
	integer values(maxx, maxy, maxccd)
	real histo(maxcounts, maxccd+1)
	real fitarray(maxcounts, maxccd+1)
	integer status, i, j, k, nfiles
	logical negflag

	character(40) taskname
	common /task/ taskname

	taskname = 'sishist 1.0'

	outfile = ' '
	infile = ' '
	rawxname = ' '
	rawyname = ' '
	ccdidname = ' '
	rows = ' '

	iunit = 15
	ounit = 16

C initialize values to 0
	do 30 k = 1, maxccd
	    do 20 j = 1, maxy
		do 10 i = 1, maxx
		    values(i,j,k) = 0
10		continue
20	    continue
30	continue

	do 50 j = 1, maxccd+1
	do 40 i = 1, maxcounts
		histo(i,j) = 0.0
40	continue
50	continue

C  get the parameters from the par file
	call gsishist (infile, outfile, rawxname, rawyname, ccdidname,
     &                 fit, rows, sensecase, status)
	if (status .ne. 0) goto 999

C open the output file
	call ftinit (ounit, outfile, 2880, status)
	if (status .ne. 0) then
		context = ' Output file may already exist? ' // outfile
		call fcerr (context)
		goto 999
	endif

C loop over all requested input files
	call fcgcls (infile, filelist, nfiles, negflag)

	do 100 i = 1, nfiles
C open the input file and check that all the input parameters are OK
	call sishck (filelist(i), iunit, rawxname, rawxno,
     &               rawyname, rawyno, ccdidname, ccdidno, sensecase,
     &               status)
	if (status .ne. 0) goto 998

C create the 3D histogram array
	call sis3d (iunit, rawxno, rawyno, ccdidno, rows, values, 
     &               status)
	if (status .ne. 0) goto 998
	call ftclos (iunit, status)

100	continue

C calculate the histogram of values
	call sis1d (values, histo, status)
	if (status .ne. 0) goto 998

C calculate the fit, if requested
	if (fit) call sisfit (histo, fitarray, status)
	if (status .ne. 0) goto 998

C integrate the histogram and fit
	call sisint (histo, fitarray, fit, status)
	if (status .ne. 0) goto 998

C write the output file
	call sisout (iunit, ounit, histo, fitarray, fit, status)
	if (status .ne. 0) goto 998

998	fstatus = 0
	call ftclos (iunit, fstatus)
	fstatus = 0
	call ftclos (ounit, fstatus)

999	return
	end


C****************************************************************************** 
C SUBROUTINE:
C      gsishist
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gsishist uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gsishist (infile, outfile, rawxname, rawyname, ccdidname, fit, rows, 
C                      sensecase, status)
C
C ARGUMENTS:
C      infile     - input FITS file and extension number
C      outfile    - output ASCII file
C	rawxname  - the name of the RAWX column
C	rawyname  - the name of the RAWY column
C	ccdidname - the name of the CCDID column
C	fit       - whether to fit the resulting histogram
C	rows      - the rows of the input file to include
C	sensecase - whether to be case sensitive about column names
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsb - get boolean parameter
C
C****************************************************************************** 
	subroutine gsishist (infile, outfile, rawxname, rawyname,
     &                  ccdidname, fit, rows, sensecase, status)

	character*(*) infile, outfile, rawxname, rawyname, ccdidname
	character*(*) rows
	logical fit, sensecase
	integer status

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

C  get the name of the raw X column
	call uclgst ('rawxname', rawxname, status)
	if (status .ne. 0) then
	    context = 'could not get RAWX parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the raw Y column
	call uclgst ('rawyname', rawyname, status)
	if (status .ne. 0) then
	    context = 'could not get RAWY parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the ccd id column
	call uclgst ('ccdidname', ccdidname, status)
	if (status .ne. 0) then
	    context = 'could not get CCDID parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get whether to output the fit to the results
	call uclgsb ('fit', fit, status)
	if (status .ne. 0) then
	    context = 'could not get FIT parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the rows of the input file to include
	call uclgst ('rows', rows, status)
	if (status .ne. 0) then
	    context = 'could not get ROWS parameter'
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
C      sishck
C
C DESCRIPTION: 
C      Check the validity of the input parameters and open the input file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sishck (infile, iunit, rawxname, rawxno, rawyname, 
C                    rawyno, ccdidname, ccdidno, sensecase, status)
C
C ARGUMENTS:
C       infile    - input FITS file and extension number
C	iunit     - input file unit number
C	rawxname  - the name of the RAWX column
C	rawxno    - the column number of the RAWX column
C	rawyname  - the name of the RAWY column
C	rawyno    - the column number of the RAWY column
C	ccdidname - the name of the CCDID column
C	ccdidno   - the column number of the CCDID column
C	sensecase - whether to be case sensitive about column names
C	status    - status of operation

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
	subroutine sishck (infile, iunit, rawxname,
     &               rawxno, rawyname, rawyno, ccdidname, ccdidno, 
     &               sensecase, status)

	character*(*) infile, rawxname, rawyname, ccdidname
	integer iunit, rawxno, rawyno, ccdidno, status
	logical sensecase

	integer fstatus, htype, extnumb, block
	character(80) context
	character(160) filename

C open the input file
	call fcpars (infile, filename, extnumb, status)
	if (extnumb .eq. -99) extnumb = 1
	call ftopen (iunit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Could not open input file' // infile
		call fcerr (context)
		goto 999
	endif

C move to the requested extension
	call ftmrhd (iunit, extnumb, htype, status)
	if (status .ne. 0) then
		write (context, 1000) extnumb
1000		format (' Error moving to extension: ',i3)
		call fcerr (context)
		goto 998
	endif

C see if the various columns are present
	call ftgcno (iunit, sensecase, rawxname, rawxno, status)
	if (status .ne. 0) then
		context = ' RAWX column not found:' // rawxname
		call fcerr (context)
		goto 998
	endif

	call ftgcno (iunit, sensecase, rawyname, rawyno, status)
	if (status .ne. 0) then
		context = ' RAWY column not found:' // rawyname
		call fcerr (context)
		goto 998
	endif

	call ftgcno (iunit, sensecase, ccdidname, ccdidno, status)
	if (status .ne. 0) then
		context = ' CCDID column not found:' // ccdidname
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
C      sis3d
C
C DESCRIPTION: 
C      Check the validity of the input parameters and open the input file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sis3d (iunit, rawxno, rawyno, ccdidno, rows, values, status)
C
C ARGUMENTS:
C	iunit   - the input file unit number
C	rawxno  - the column number of the RAWX column
C	rawyno  - the column number of the RAWY column
C	ccdidno - the column number of the CCDID column
C	rows    - the rows of the input file to include
C	values  - the output 3D histogram of pixels
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine sis3d (iunit, rawxno, rawyno, ccdidno, rows, values,
     &                    status)

	integer iunit, rawxno, rawyno, ccdidno, status
	character*(*) rows

	integer maxx, maxy, maxccd, maxranges, maxsize
	parameter (maxx = 640)
	parameter (maxy = 640)
	parameter (maxccd = 4)
	parameter (maxranges = 10)
	parameter (maxsize = 1024)

	integer values(maxx, maxy, maxccd)
	integer ranges, startrow(maxranges), stoprow(maxranges)
	integer nrows, frow, i, j, nelem, remain 
	integer rawx(maxsize), rawy(maxsize), ccdid(maxsize)
	character(80) context
	logical goodrows, anyf

C find the number of rows
	call ftgkyj (iunit, 'NAXIS2', nrows, context, status)
	if (status .ne. 0) then
	    context = ' Error determining number of rows in input file'
	    call fcerr (context)
	    goto 999
	endif

C find which rows to include
	call fccmpr (rows, goodrows)
	if (.not. goodrows) then
		status = 1
		context = ' illegal rows specification ' // rows
		call fcerr (context)
		goto 999
	endif

	if (rows .eq. '-') then
		ranges = 1
		startrow(1) = 1
		stoprow(1) = nrows
	else
		call fcgrgs (rows, nrows, ranges, startrow, stoprow)
	endif

C loop through the events
	do 100 i = 1, ranges
	    remain = stoprow(i) - startrow(i) + 1
	    frow = startrow(i)
10	    if (remain .gt. maxsize) then
		nelem = maxsize
	    else
		nelem = remain
	    endif

C get the values
	    call ftgcvj (iunit, rawxno, frow, 1, nelem, -99, rawx, anyf,
     &                   status)
	    call ftgcvj (iunit, rawyno, frow, 1, nelem, -99, rawy, anyf,
     &                   status)
	    call ftgcvj (iunit, ccdidno, frow, 1, nelem, -99, ccdid, 
     &                   anyf, status)

	    if (status .ne. 0) then
		context = ' Error reading in values'
		call fcerr (context)
		goto 999
	    endif

C put each event into the correct bin
	    do 50 j = 1, nelem
		if ((rawx(j) .ge. 0) .and. (rawx(j) .lt. maxx) .and.
     &              (rawy(j) .ge. 0) .and. (rawy(j) .lt. maxy) .and.
     &              (ccdid(j) .ge. 0) .and. (ccdid(j) .lt. maxccd)) then
		    values(rawx(j)+1,rawy(j)+1,ccdid(j)+1) =  
     &                     values(rawx(j)+1,rawy(j)+1,ccdid(j)+1) + 1
		else
		    write (context, 1001) rawx(j), rawy(j), ccdid(j)
1001		    format (' Bad event (rawx, rawy, ccdid) = ', 3i10)
		    call fcecho (context)
		endif
50	    continue

C loop back for more
	    remain = remain - nelem
	    frow = frow + nelem
	    if (remain .gt. 0) goto 10

100 	continue

999	return
	end

C****************************************************************************** 
C SUBROUTINE:
C      sis1d
C
C DESCRIPTION: 
C      Calculate the histogram of the 3D histogram
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sis1d (values, histo, status)
C
C ARGUMENTS:
C	values  - the 3D histogram of pixels
C	histo   - output 1D histogram of pixel events
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine sis1d (values, histo, status)

	integer maxx, maxy, maxccd, maxcounts
	parameter (maxx = 640)
	parameter (maxy = 640)
	parameter (maxccd = 4)
	parameter (maxcounts = 100)

	integer values(maxx, maxy, maxccd),index
	real histo(maxcounts, maxccd+1)
	integer status

	integer i,j,k

C loop through the 3D array and form the histogram
C note keep one for all chips, and one for each seperate chip
C
	do 300 k = 1, maxccd
	   do 200 j = 1, maxy
	      do 100 i = 1, maxx
		 index = values(i,j,k)+1
		 if ((index .le. maxcounts) .and.
     &      	      (index .gt. 0)) then
		    histo(index,1) =  histo(index,1) + 1.
		    histo(index,k+1) =  histo(index,k+1) + 1.
		 endif
 100	      continue
 200	   continue
 300	continue
	
	return
	end

C****************************************************************************** 
C SUBROUTINE:
C      sisfit
C
C DESCRIPTION: 
C      Calculate the fit to the generated histogram
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sisfit (histo, fitarray, status)
C
C ARGUMENTS:
C	histo    - the input histogram of number of pixel events
C	fitarray - the output array of fitted values
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine sisfit (histo, fitarray, status)

	integer maxx, maxy, maxccd, maxcounts
	parameter (maxx = 640)
	parameter (maxy = 640)
	parameter (maxccd = 4)
	parameter (maxcounts = 100)

	integer status
	real fitarray(maxcounts, maxccd+1),histo(maxcounts, maxccd+1)

	integer i, j
	real mean,mean1c

C "Calculate a Poisson distribution whose peak matches the histo(1) data"
C        -- from SISCLEAN kaa version
C
C the following is a copy of the code in sisclean the kaa/XSELECT version
C the only change is to the denominator in the calculation of mean since
C SISCLEAN assumed a full SIS image, and this program uses a 3rd dimension
C to take the 4 chips into account
C
 	mean = -log(histo(1,1)/real(maxx)/real(maxy)/real(maxccd))
	fitarray(1,1) = histo(1,1)
	do 150 i = 2, maxcounts
	   fitarray(i,1) = fitarray(i-1,1) * mean / real(i-1)
 150	continue
	do 500 j = 2, maxccd+1
	   fitarray(1,j) = histo(1,j)
	   mean1c = -log(histo(1,j)/real(maxx)/real(maxy))
	   do 100 i = 2, maxcounts
	      fitarray(i,j) = fitarray(i-1,j) * mean / real(i-1)
 100	   continue
 500	continue

	return
	end

C****************************************************************************** 
C SUBROUTINE:
C      sisint
C
C DESCRIPTION: 
C      integrate the histogram and the fit (if requested)
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sisint (histo, fitarray, fit, status)
C
C ARGUMENTS:
C	histo    - histogram of pixel events
C	fitarray - fitted array of pixel events
C	fit      - whether fitarray is populated
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine sisint (histo, fitarray, fit, status)

	integer maxcounts, maxccd
	parameter (maxcounts = 100)
	parameter (maxccd = 4)

	integer  status
	real histo(maxcounts, maxccd+1),fitarray(maxcounts, maxccd+1)
	logical fit

	integer i, j

C "Convert the observed and Poisson distribution from differential to integral"
C    ----  from SISCLEAN the kaa version
C
	do 500 j = 1, maxccd+1
	do 100 i = maxcounts-1, 1, -1
	    histo(i,j) = histo(i,j) + histo(i+1,j)
100	continue

	if (fit) then
	    do 200 i = maxcounts-1, 1, -1
		fitarray(i,j) = fitarray(i,j) + fitarray(i+1,j)
200	    continue
	endif

500	continue

	return
	end

C****************************************************************************** 
C SUBROUTINE:
C      sisout
C
C DESCRIPTION: 
C      write the calculated values to the output file
C
C AUTHOR:  
C	Emily A. Greene
C	Hughes STX
C	September, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call sisout (iunit, ounit, histo, fitarray, fit, status)
C
C ARGUMENTS:
C	iunit    - input unit number
C	ounit    - output unit number
C	histo    - histogram of pixel events
C	fitarray - fitted histogram of pixel events
C	fit      - whether fitarray is populated
C	status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to terminal
C
C****************************************************************************** 
	subroutine sisout (iunit, ounit, histo, fitarray, fit, status)

	integer iunit, ounit, status

	integer maxcounts, maxccd
	parameter (maxcounts = 100)
	parameter (maxccd = 4)

	real histo(maxcounts,maxccd+1)
	real fitarray(maxcounts,maxccd+1)
	logical fit

	integer varidat, tfields, naxis, i, icol
	integer pcount, gcount, bitpix, naxes
	character(80) ttype(11), tform(11), tunit(11), extname
	logical simple, extend

C set up the output file primary header
          simple = .true.
	  bitpix = 8
          naxis = 0
	  naxes = 0
          pcount = 0
          gcount = 1
	  extend = .true.
          call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,gcount,
     &                extend,status)
          call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)

C  create a new binary extension
        call ftcrhd(ounit, status)

C set up the binary header information
	if (fit) then
	    tfields = 11
	    ttype(1) = 'CTS_PXL'
	    ttype(2) = 'ALLCCD'
	    ttype(3) = 'ALLCCD_FIT'
	    ttype(4) = 'CCD0'
	    ttype(5) = 'CCD0_FIT'
	    ttype(6) = 'CCD1'
	    ttype(7) = 'CCD1_FIT'
	    ttype(8) = 'CCD2'
	    ttype(9) = 'CCD2_FIT'
	    ttype(10) = 'CCD3'
	    ttype(11) = 'CCD3_FIT'
	else
	    tfields = 6
	    ttype(1) = 'CTS_PXL'
	    ttype(2) = 'ALLCCD'
	    ttype(3) = 'CCD0'
	    ttype(4) = 'CCD1'
	    ttype(5) = 'CCD2'
	    ttype(6) = 'CCD3'
	endif

	tform(1) = 'I'
	tunit(1) = 'Counts/per pixel'
	do 50 i = 2, 11
	    tform(i) = 'E'
	    tunit(i) = 'number of pixels'
50	continue
	extname = ' '
	varidat = 0

        call ftphbn(ounit,maxcounts,tfields,ttype,tform,tunit,
     &                  extname,varidat,status)
        call ftbdef(ounit,tfields,tform,varidat,maxcounts,status)

C copy any additional keywords
	call xcopynoscale (iunit, ounit, status)

	icol = 1
	do 85 i = 0, maxcounts - 1
	   call ftpclj(ounit, icol, i+1, 1, 1,i, status)
 85	continue
	icol = 2
	do 100 i = 1, maxccd+1
C write the calculated histogram
	   call ftpcle (ounit, icol, 1, 1, maxcounts, histo(1,i), status)
	   icol = icol + 1
	   
C write the fit, if desired
	   if (fit) then
	      call ftpcle (ounit, icol, 1, 1, maxcounts, 
     &                                 fitarray(1,i),status)
	      icol = icol + 1
	   endif
	   
 100	continue

 999	return
	end
