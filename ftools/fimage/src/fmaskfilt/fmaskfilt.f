C****************************************************************************** 
C SELECTOR TASK:
C      fmaskfilt
C
C FILE:
C      fmaskfilt.f 
C
C DESCRIPTION: 
C	Takes an input event list and an input mask, and filters the
C	event list based on non-zero values of the mask.
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	April, 1993
C
C MODIFICATION HISTORY:
C       Ken Ebisawa, USRA, Aug 9, 1996
C       A bug in the function 'filtrow' was fixed.
C     12/03/97 PDW 2.2c - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C      3/16/98 kaa 2.2d - Removed calls to datatopix from filtrow since I
C                         believe that they should not be there.
C      2/28/00 ng  2.2e - Added option for the inverse mask
C      6/06/01 bki 2.2f - Made inverse a global(common) variable.
C			  Added coordinate transformations.
C      2/17/04 zpan 2.2g - use default WCS values if 
C                          there are no WCS keywords for the x and y columns 
C      3/31/04 zpan 2.2h - deal with the case when the region of mask is
C                          smaller that of event list
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C       infile   - input event file
C	maskfile - input file containing mask
C       outfile  - output file filtered event file
C	xcolname    - name of the X column
C	ycolname    - name of the Y column
C	crval1  - Coordinate value of X reference pixel
C	crval2  - Coordinate value of Y reference pixel
C	crpix1  - X axis reference pixel number
C	crpix2  - Y axis reference pixel number
C	cdelt1  - X axis coordinate increment
C	cdelt2  - Y axis coordinate increment
C	copyprime - whether to copy primary array
C	copyall   - whether to copy all other extensions
C
C CALLED ROUTINES:
C      subroutine gmask - gets parameters from environment
C
C****************************************************************************** 
      subroutine fmaskt
      character(160) infile, outfile, maskfile 
      character(80) xcolname, ycolname
	double precision crval(2), cdelt(2), crpix(2)
	logical copyprime, copyall
        logical inverse
	integer status

        character(40) taskname
        common /task/ taskname

	common / cvalues / crval, cdelt, crpix
	common // inverse

        taskname = 'fmaskfilt2.2f'
	infile = ' '
	outfile = ' '
	maskfile = ' '
	xcolname = ' '
	ycolname = ' '

C  get the parameters from the par file
	call gmaskfilt (infile, maskfile, outfile, xcolname, ycolname, 
     &              copyprime, copyall, status)
	if (status .ne. 0) goto 999

C get the mask to data transformation parameters from keywords
	call gimgkey (maskfile, xcolname, ycolname, status)
	if (status .ne. 0) goto 999

C filter the event file
	call filtevent (infile, maskfile, outfile, xcolname, ycolname,
     &              copyprime, copyall, status)

999	if (status .ne. 0) call fcerrm (status)
	return
	end


C****************************************************************************** 
C SUBROUTINE:
C      gmaskfilt
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	20 April, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gmaskfilt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gmaskfilt (infile, maskfile, outfile, xcolname, ycolname, 
C			copyprime, copyall, status)
C
C ARGUMENTS:
C      infile   - input events file
C	maskfile - input maskfile name
C      outfile  - output filtered events file name
C	xcolname    - X column name
C	ycolname    - Y column name
C	copyprime - whether to copy primary array
C	copyall   - whether to copy all other extensions
C	status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C	crval1  - Coordinate value of X reference pixel
C	crval2  - Coordinate value of Y reference pixel
C	crpix1  - X axis reference pixel number
C	crpix2  - Y axis reference pixel number
C	cdelt1  - X axis coordinate increment
C	cdelt2  - Y axis coordinate increment
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsx - get parameter
C
C****************************************************************************** 
	subroutine gmaskfilt (infile, maskfile, outfile, xcolname, 
     &                        ycolname, copyprime, copyall, status)

	character*(*) infile, maskfile, outfile, xcolname, ycolname
	logical copyprime, copyall
	integer status

	character(80) context
	double precision crval(2), cdelt(2), crpix(2)
        logical inverse
	common / cvalues / crval, cdelt, crpix
	common // inverse

C  initialize variables
	status = 0

C  get the name of the input events file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the input mask file
	call uclgst('maskfile', maskfile, status)
	if (status .ne. 0) then
	    context = 'could not get MASKFILE parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the output event file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the X column
	call uclgst('xcolname', xcolname, status)
	if (status .ne. 0) then
	    context = 'could not get XCOLNAME parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the Y column
	call uclgst('ycolname', ycolname, status)
	if (status .ne. 0) then
	    context = 'could not get YCOLNAME parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CRVAL1 coordinate value
	call uclgsd('crval1', crval(1), status)
	if (status .ne. 0) then
	    context = 'could not get CRVAL1 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CRVAL2 coordinate value
	call uclgsd('crval2', crval(2), status)
	if (status .ne. 0) then
	    context = 'could not get CRVAL2 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CRPIX1 coordinate value
	call uclgsd('crpix1', crpix(1), status)
	if (status .ne. 0) then
	    context = 'could not get CRPIX1 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CRPIX2 coordinate value
	call uclgsd('crpix2', crpix(2), status)
	if (status .ne. 0) then
	    context = 'could not get CRPIX2 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CDELT1 coordinate value
	call uclgsd('cdelt1', cdelt(1), status)
	if (status .ne. 0) then
	    context = 'could not get CDELT1 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CDELT2 coordinate value
	call uclgsd('cdelt2', cdelt(2), status)
	if (status .ne. 0) then
	    context = 'could not get CDELT2 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  The mask is inversed? 
	call uclgsb('inverse', inverse, status)
	if (status .ne. 0) then
	    context = 'could not get Inverse parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get whether to copy primary array
	call uclgsb('copyprime', copyprime, status)
	if (status .ne. 0) then
	    context = 'could not get COPYPRIME parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get whether to copy all other extensions
	call uclgsb('copyall', copyall, status)
	if (status .ne. 0) then
	    context = 'could not get COPYALL parameter'
	    call fcecho(context)
	    goto 999
	endif

999	continue
	if (status .ne. 0)  call fcerrm(status)

	return
	end

C****************************************************************************** 
C SUBROUTINE:
C      filtevent
C
C DESCRIPTION: 
C	filter the input event file based on the mask file
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	April, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call filtevent (infile, maskfile, outfile, xcolname, ycolname,
C			copyprime, copyall, status))
C
C ARGUMENTS:
C      infile   - input event file
C	maskfile - input name of the mask file
C      outfile  - output filtered event file
C	xcolname    - name of the X column
C	ycolname    - name of the Y column
C	copyprime - whether to copy primary array
C	copyall   - wheterh to copy all other extensions
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message 
C
C CALLED ROUTINES:
C
C****************************************************************************** 
	subroutine filtevent (infile, maskfile, outfile, xcolname, 
     &      ycolname, copyprime, copyall, status)

	character*(*) infile, outfile, maskfile, xcolname, ycolname
	logical copyprime, copyall
	integer status

	integer maxcl
	parameter (maxcl = 512)

	integer iunit, ounit, munit, htype, i, rowlen, nrows, tfields
cebi	character(80) context, ttype(maxcl), tform(maxcl), tunit(maxcl)
	character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
	character(80) extname, keyword
cebi	character(160) filename
	character(160) filename, context
	integer iextnum, block, bitpix, naxis, pcount, gcount, tbcol
	integer varidat, xcolno, ycolno, repeat, dtype, width, mextnum
	integer naxes(maxcl), nelem, orow, irow, mhtype, fstat
	logical simple, extend, exact, anyf, flagval
        integer filtrow, maskval
        integer clenact
C
C	WCS parameters for the input event file:
C
	double precision ecrpix(2), ecrval(2), ecdelt(2), rot
	character(80) ctype
        logical ewcs
C
C	WCS parameters for the input mask file:
C
	double precision mcrpix(2), mcrval(2), mcdelt(2), mrot
	character(80) mctype

	double precision x, y, xpos, ypos, xpix, ypix
	integer ix, iy

	double precision crval(2), cdelt(2), crpix(2)
        character(40) taskname
        logical inverse
        common /task/ taskname
	common / cvalues / crval, cdelt, crpix
	common // inverse

C initialize unit numbers
	iunit = 15
	ounit = 16
	munit = 17
	exact = .false.
        ewcs = .true.

C open the input event file
	call fcpars (infile, filename, iextnum, status)

C EAG 8/25/93 default to 1st extension
	if (iextnum .eq. -99) iextnum = 1

	call ftopen (iunit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Unable to open input event file'
		call fcerr (context)
		goto 999
	endif

C and create the output file
	call ffinit (ounit, outfile, status)
	if (status .ne. 0) then
		context = ' Unable to open outfile - may exist?'
		call fcerr (context)
		goto 998
	endif

C copy the primary array, if so requested
	if ((copyprime) .or. (copyall)) then
		call ftcopy (iunit, ounit, 0, status)
	else
		simple = .true.
		bitpix = 16
		naxis = 0
		pcount = 0
		gcount = 1
		extend = .true.
		call ftphpr (ounit, simple, bitpix, naxis, naxes,
     &   		     pcount, gcount, extend, status)
		call ftpdef (ounit, bitpix, naxis, naxes, pcount,
     &			     gcount, status)
	endif
	if (status .ne. 0) then
		context = ' Error creating primary array'
		call fcerr (context)
		goto 997
	endif

C copy other extensions, if so requested
	if ((copyall) .and. (iextnum .gt. 1)) then
		do 100 i = 1, iextnum-1
			call ftmrhd (iunit, 1, htype, status)
			call ftcrhd (ounit, status)
			call ftcopy (iunit, ounit, 0, status)
100		continue
		if (status .ne. 0) then
			context = ' error copying extensions'
			call fcerr (context)
			goto 997
		endif
	endif

C move to the correct extension in the input file
	call ftmahd (iunit, iextnum+1, htype, status)
	if (htype .eq. 1) then
		call ftghtb (iunit, maxcl, rowlen, nrows, tfields, 
     &		     ttype, tbcol, tform, tunit, extname, status)
	else if (htype .eq. 2) then
		call ftghbn (iunit, maxcl, nrows, tfields, ttype,
     &		     tform, tunit, extname, varidat, status)
		keyword = 'NAXIS1'
		call ftgkyj (iunit, keyword, rowlen, context, status)
	else
		context = ' Extension type not supported'
		call fcerr (context)
		goto 997
	endif

C check that xcolname and ycolname are real column names
	call ftgcno (iunit, exact, xcolname, xcolno, status)
	if (status .ne. 0) then
		context = ' x column name not found'
		call fcerr (context)
		goto 997
	endif
	call ftgcno (iunit, exact, ycolname, ycolno, status)
	if (status .ne. 0) then
		context = ' y column name not found'
		call fcerr (context)
		goto 997
	endif

C are these vector columns?  If so, we don't know what to do with them!
	if (htype .eq. 2) then
	    call ftbnfm (tform(xcolno), dtype, repeat, width, status)
	    if (repeat .ne. 1) then
		context = ' X is a vector column'
		call fcerr (context)
		goto 997
	    endif
	    call ftbnfm (tform(ycolno), dtype, repeat, width, status)
	    if (repeat .ne. 1) then
		context = ' Y is a vector column'
		call fcerr (context)
		goto 997
	    endif
	endif
C
C read the WCS keywords from the input event file
C
	call ftgtcs (iunit, xcolno, ycolno, ecrval(1), ecrval(2), 
     &       ecrpix(1), ecrpix(2), ecdelt(1), ecdelt(2),
     &       rot, ctype, status)
	if (status .ne. 0) then
                ecrval(1) =0
                ecrval(2) =0
                ecrpix(1) =0
                ecrpix(2) =0
                ecdelt(1) =1
                ecdelt(2) =1
                ctype ='-CAR'
                status = 0
                ewcs =.false.
                rot =0.0
        endif
C	if (status .ne. 0) then
C		context = ' Error reading event file WCS keywords'
C		call fcerr (context)
C		goto 997
C	endif

C set up the output file - don't know how many rows at this time
	call ftcrhd (ounit, status)
	if (htype .eq. 1) then
		call ftphtb (ounit, rowlen, 0, tfields, ttype, tbcol,
     &                       tform, tunit, extname, status)
	else
		call ftphbn (ounit, 0, tfields, ttype, tform, tunit,
     &                       extname, varidat, status)
	endif

	if ((copyall) .or. (copyprime)) call xcopyscale (iunit, ounit,
     &             status)
	context = ' TASK: '//taskname(1:clenact(taskname))//
     $       ' on FILENAME: '//filename(1:clenact(filename))//
     $       ' with the MASFFFILE '// maskfile(1:clenact(maskfile))

	call ftphis(ounit,context,status)

	if (htype .eq. 1) then
		call ftadef (ounit, rowlen, tfields, tbcol, tform, 0,
     &                       status)
	else
		call ftbdef (ounit, tfields, tform, varidat, 0, status)
	endif
	if (status .ne. 0) then
		context = ' Unable to initialize output extension'
		call fcerr (context)
		goto 997
	endif

C open the mask file
	call fcpars (maskfile, filename, mextnum, status)
C EAG 8/25/93 default to primary array
	if (mextnum .eq. -99) mextnum = 0

	call ftopen (munit, filename, 0, block, status)
	if (status .ne. 0) then
		context = ' Unable to open mask file'
		call fcerr (context)
		goto 997
	endif
	call ftmahd (munit, mextnum+1, mhtype, status)
	if (mhtype .ne. 0) then
		context = ' mask file must be an image'
		call fcerr (context)
		goto 996
	endif
	call ftghpr (munit, maxcl, simple, bitpix, naxis, naxes,
     &               pcount, gcount, extend, status)
	if (status .ne. 0) then
		context = ' Unable to get mask header infomation'
		call fcerr (context)
		goto 996
	endif
	call ftgics (munit, mcrval(1), mcrval(2), mcrpix(1),
     &	     mcrpix(2), mcdelt(1), mcdelt(2), mrot, mctype, status)
	if (status .ne. 0) then
		context = ' Error reading mask WCS keywords '
		call fcerr (context)
		context = ' Using parameter file WCS keywords '
		call fcerr (context)
		mcrpix(1)=crpix(1)
		mcrpix(2)=crpix(2)
		mcdelt(1)=cdelt(1)
		mcdelt(2)=cdelt(2)
		mcrval(1)=crval(1)
		mcrval(2)=crval(2)
	endif

        if ( mctype(1:1) .eq.' ' ) then
                mctype ='-CAR'
        endif


C loop over all events in the input file
	nelem = 1
	orow = 0
	do 500 irow = 1, nrows
C NOTE: this could be done more efficiently, but let's get something
C       that works first.  Should read as large a chunk at a time
C       as possible
 		call ftgcfd (iunit, xcolno, irow, 1, nelem, x,
     &                       flagval, anyf, status)
 		if (anyf) goto 500
 		call ftgcfd (iunit, ycolno, irow, 1, nelem, y,
     &                       flagval, anyf, status)
 		if (anyf) goto 500
		if (status .ne. 0) then
			context = ' Error reading values '
			call fcerr (context)
			goto 996
		endif
C
C	Convert the event file pixel coordinates to RA/DEC:
C
 		call ftwldp (x, y, ecrval(1), ecrval(2), ecrpix(1),
     &                       ecrpix(2), ecdelt(1), ecdelt(2),
     &                       rot, ctype, xpos, ypos, status)
		if (status .ne. 0) then
			context = ' Error converting to RA/DEC '
			call fcerr (context)
			goto 996
		endif
C
C	Find the pixel in the mask file that this RA/DEC corresponds to:
C
               if (mctype(1:4).eq.'-CAR') then
                      if (xpos.gt.180) then
                      xpos =360+xpos 
                      endif
                      if (xpos.lt.-180) then
                      xpos =xpos-360
                      endif
               endif
 		call ftxypx (xpos, ypos, mcrval(1), mcrval(2),
     &               mcrpix(1), mcrpix(2), mcdelt(1), mcdelt(2),
     &               mrot, mctype, xpix, ypix, status)

		if (status .ne. 0) then
			context = ' Error converting to pixel space '
			call fcerr (context)
			goto 996
		endif
C
C	Filter the event
C
                if ( ewcs ) then
		ix=(xpix+.5) 
		iy=(ypix+.5) 
                else 
		ix=(xpix+.5) + 1
		iy=(ypix+.5) + 1
                endif


                maskval = filtrow (munit, naxes(1), naxes(2),ix, iy)
                if (maskval .eq. 999) return
		if (inverse) then
                 if (maskval.eq.0) then
		  orow = orow + 1
		  call fcopyr (iunit, irow, ounit, orow, rowlen, status)
		 endif
		else
                 if (maskval.eq.1) then
		  orow = orow + 1
		  call fcopyr (iunit, irow, ounit, orow, rowlen, status)
		 endif
		endif
500	continue

C update the output file size
	call ftddef (ounit, orow*rowlen, status)
	keyword = 'NAXIS2'
	call ftmkyj (ounit, keyword, orow, '&', status)
	if (status .ne. 0) then
		context = ' Error specifying size of output extension'
		call fcerr (context)
		goto 996
	endif

C copy all additional extensions, if so requested
	if (copyall) then
600		call ftmrhd (iunit, 1, htype, status)
		call ftcrhd (ounit, status)
		call ftcopy (iunit, ounit, 0, status)
		if (status .eq. 0) goto 600
		status = 0
	endif

996	fstat = 0
	call ftclos (munit, fstat)
997	fstat = 0
	call ftclos (ounit, fstat)
998	fstat = 0
	call ftclos (iunit, fstat)
999	return
	end

C****************************************************************************** 
C FUNCTION:
C      filtrow
C
C DESCRIPTION: 
C	filter the input event based on the mask file
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	April, 1993
C
C MODIFICATION HISTORY:
C       Ken Ebisawa, USRA Aug, 9, 1996
C	  Originally, the 1D pixel number corrponding to the
C         pixel (px,py) was referred to as (px-1)*naxis1 + py, 
C         but this has to be px + (py-1)*naxis2.
C       kaa 3/16/98 Removed calls to datatopix - the input x and y
C         are in pixel coordinates so they should not be necessary.
C	bki 6/6/01 - fpixel should be (y-1)*naxis1 + x.
C	  Changed x & y to type integer, added status checking and
C	  calls to fcerr.
C
C NOTES:
C
C USAGE:
C	filtrow (munit, naxis1,naxis2, x, y, status)
C
C ARGUMENTS:
C	munit  - mask file unit number
c	naxis1 - x axis dimension
C	x      - x value of this event
C	y      - y value of this event
C	status - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
	integer function filtrow (munit, naxis1, naxis2,x, y)

	integer munit, naxis1, x, y

	integer status, fpixel, value
	integer flagval, anyf
	character(160) context

	filtrow = 0
	flagval = 0
	anyf = 0
	value = 0
        status =0

C for now, read in the specific value from the file each time
C later, should hold a chunk of mask in memory to cut down on i/o

        if ( x.lt.1.or.x.gt.naxis1.or.y.lt.1.or.y.gt.naxis2) then
            filtrow =0
            return
        endif
  	fpixel = x + (y-1)*naxis1

  	call ftgpfj (munit, 1, fpixel, 1, value, flagval, anyf, status)

	if ((flagval .ne. 0).or.(anyf .ne. 0)) then
		context = ' Undefined data value in mask file '
		call fcerr (context)
	endif
 	if (status .ne. 0) then
 		context = ' Error reading mask file '
 		call fcerr (context)
		filtrow = 999
		return
 	endif

	if (value .ne. 0) filtrow = 1

	return
	end

C****************************************************************************** 
