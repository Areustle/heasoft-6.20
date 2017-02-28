C****************************************************************************** 
C SELECTOR TASK:
C      fsaoi
C
C FILE:
C      fsaoi.f 
C
C DESCRIPTION: 
C	Takes an output region file from SAOImage, and converts it
C	to the input format for FSELECT
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	March, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C	does not currently support polygon regions
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input SAOImage region filename
C      outfile  - output file to be used as input for FSELECT
C	imgfile - file containing the original image
C	xcolname    - name of the X column
C	ycolname    - name of the Y column
C	crval1  - Coordinate value of X reference pixel
C	crval2  - Coordinate value of Y reference pixel
C	crpix1  - X axis reference pixel number
C	crpix2  - Y axis reference pixel number
C	cdelt1  - X axis coordinate increment
C	cdelt2  - Y axis coordinate increment
C
C CALLED ROUTINES:
C      subroutine gsaoi - gets parameters from environment
C	subroutine fsaorw - read SAOImage file and write FSELECT file
C
C****************************************************************************** 
	subroutine fsaoi
	character(160) infile,outfile,imgfile
	character(80)  xcolname, ycolname
	double precision crval(2), cdelt(2)
	integer crpix(2)
	integer status

        character(40) taskname
        common /task/ taskname

	common / cvalues / crval, cdelt, crpix

        taskname = 'fsaoi2.2a'
	infile = ' '
	outfile = ' '
	xcolname = ' '
	ycolname = ' '

C  get the parameters from the par file
	call gsaoi (infile, outfile, imgfile, xcolname, ycolname, 
     &              status)
	if (status .ne. 0) goto 999

C  get the transformation and column name values
	call gimgkey (imgfile, xcolname, ycolname, status)
	if (status .ne. 0) goto 999

C  read in the SAOImage reg file and write out the FSELECT file
	call fsaorw (infile, outfile, imgfile, xcolname, ycolname)

999	return
	end


C****************************************************************************** 
C SUBROUTINE:
C      gsaoi
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	23 February, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gsaoi uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C	call gsaoi (infile, outfile, imgfile, xcolname, ycolname, status)
C
C ARGUMENTS:
C      infile   - input SAOImage region file
C      outfile  - output file for input to FSELECT
C	imgfile - name of the original image file
C	xcolname    - X column name
C	ycolname    - Y column name
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
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgst - get string parameter
C
C****************************************************************************** 
	subroutine gsaoi (infile, outfile, imgfile, xcolname, ycolname,
     &                    status)

	character*(*) infile, outfile, imgfile, xcolname, ycolname
	character(80) context

	double precision crval(2), cdelt(2)
	integer status, crpix(2)
	common / cvalues / crval, cdelt, crpix

C  initialize variables
	status = 0

C  get the name of the input SAOImage region file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the output FSELECT file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get OUTFILE parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the name of the original image file
	call uclgst('imgfile', imgfile, status)
	if (status .ne. 0) then
	    context = 'could not get IMGFILE parameter'
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
	call uclgsi('crpix1', crpix(1), status)
	if (status .ne. 0) then
	    context = 'could not get CRPIX1 parameter'
	    call fcecho(context)
	    goto 999
	endif

C  get the CRPIX2 coordinate value
	call uclgsi('crpix2', crpix(2), status)
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

999	continue
	if (status .ne. 0)  call fcerrm(status)

	return
	end


C****************************************************************************** 
C SUBROUTINE:
C      fsaorw
C
C DESCRIPTION: 
C	read the requested SAOImage region file and convert it to
C	FSELECT input style
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	February, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call fsaorw (infile, outfile, imgfile, xcolname, ycolname)
C
C ARGUMENTS:
C      infile   - input SAOImage region file
C      outfile  - output FSELECT file
C	xcolname    - name of the X column
C	ycolname    - name of the Y column
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message 
C	line     - the current line of the saoimage region file being parsed
C	oppos    - index of open paraenthesis
C	cpos     - index of first comma
C	cppos    - index of close paranthesis
C
C CALLED ROUTINES:
C	function fcstln - returns index of last non-blank character
C	function convrtstr - converts a string in pixels to data uints string
C
C****************************************************************************** 
	subroutine fsaorw (infile, outfile, imgfile, xcolname, ycolname)

	character*(*) infile, outfile, imgfile, xcolname, ycolname

	integer iunit, ounit, status
	integer fcstln
	integer oppos, cpos, cppos, outpos, bangpos, oldcpos
	character(80) context, object
	character(8192) line, outstr
	logical exclude, first, offset

	double precision value, pixtodata
	integer ivalue

	character(23) convrtstr

C initialize unit numbers
	iunit = 15
	ounit = 16

C open the input region file
	open(unit=iunit,file=infile,iostat=status,status='old')
	if (status .ne. 0) then
		context = ' could not input file'
		call fcerr (context)
		goto 999
	endif

C and open the output FSELECT file
	open(unit=ounit, file=outfile, iostat=status, status='new')
	if (status .ne. 0) then
		context = ' could not output file'
		call fcerr (context)
	 	close (iunit)
		goto 999
	endif

C the first line is treated slightly differently
	first = .true.

C read a line from the input file
100	read (iunit, 1000, err=998, end=998) line
1000	format (A)

C check for blank line - if so, read next line
	if (fcstln(line) .le. 0) goto 100

C check for comment character - if so, read next line
	if (index(line, '#') .gt. 0) goto 100

C parse the line
C the exclude flag is always in the first column
	if (line(1:1) .eq. '-') then
		exclude = .true.
	else
		exclude = .false.
	endif

C next, get the type of object
	oppos = index(line, '(')
	cpos = index(line, ',')
	cppos = index(line, ')')
	if (oppos .le. 0) goto 997
	if (cpos .le. 0) goto 997
	if (cppos .le. 0) goto 997

	read (line(2:oppos-1), 1000, err=997) object

C get the output string started
	if (first) then
		outstr = '('
		outpos = 2
		first = .false.
	else
	    if (exclude) then
		outstr = '&& ('
	    else
		outstr = '|| ('
	    endif
		outpos = 5
	endif
	if (exclude) then
		outstr(outpos:) = '! ('
		outpos = outpos + 3
	endif

C deal with known objects
	if (object .eq. 'POINT') then

C point consists of x, y location
C point is really a box with size 1.0 centered on center of this pixel

	   outstr(outpos:) = 'BOX('
	   outpos = outpos + 4
	   read (line(oppos+1:cpos-1), '(f23.0)') value

C note that integral pixel number is in the center of the pixel
	   ivalue = nint(value)
	   offset = .true.
	   write (outstr(outpos:), '(f23.15)') pixtodata(dble(ivalue),
     &               1, offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2

	   read (line(cpos+1:cppos-1), '(f23.0)') value
	   ivalue = nint(value)
	   offset = .true.
	   write (outstr(outpos:), '(f23.15)') pixtodata(dble(ivalue),
     &                 2, offset)
	   outpos = outpos + 23
           outstr(outpos:) = ', '
           outpos = outpos + 2

	   offset = .false.
	   value = 1.D0
	   write (outstr(outpos:), '(f23.15)') pixtodata(value, 1,
     &                offset)
	   outpos = outpos + 23
           outstr(outpos:) = ', '
           outpos = outpos + 2

	   offset = .false.
	   value = 1.D0
           write (outstr(outpos:), '(f23.15)') pixtodata(value, 2,
     &                offset)
           outpos = outpos + 23
	   outstr(outpos:) = ', 0.0, '
	   outpos = outpos + 7
	   
	   outstr(outpos:) = 
     &         xcolname(1:fcstln(xcolname)) // ', ' // 
     &         ycolname(1:fcstln(ycolname)) // ')'
	   outpos = outpos + fcstln(xcolname) + 
     &                     fcstln(ycolname) + 4

	else if ((object .eq. 'ELLIPSE') .or. 
     &           (object .eq. 'CIRCLE') .or.  
     &           (object .eq. 'BOX')) then

C ellipse has arguments x, y, a, b, theta(degrees)
C box has arguments x, y, rx, ry, theta(degrees)
C circle has arguments x, y, radius
C	Note: circles must be treated as ellipses since the conversion
C	      in the X and Y direction may not be the same

C put in the object name
	if (object .ne. 'CIRCLE') then
	   outstr(outpos:) = object(1:fcstln(object)) // '('
	   outpos = outpos + fcstln(object) + 1
	else
	   outstr(outpos:) = 'ELLIPSE('
	   outpos = outpos + 8
	endif

C x
	   offset = .true.
	   outstr(outpos:) = convrtstr (line(oppos+1:cpos-1), 1, offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C y
	   offset = .true.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 2, 
     &                           offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C for circles, there isn't another comma, so use the )
	   if (cpos .eq. oldcpos) cpos = cppos

C a or rx or r for the X axis
	   offset = .false.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 1, 
     &                          offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   if (object .eq. 'CIRCLE') goto 200
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C for boxes and ellipses with no rotation, no angle is written
	   if (cpos .eq. oldcpos) cpos = cppos

C b or ry or r transformed for the Y axis
200	   offset = .false.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 2, 
     &                          offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2

C angle (no translation needed)
C for no rotation objects (circle, ellipse or box with no rotation)
	   if ((object .eq. 'CIRCLE') .or. (cpos .eq. cppos)) then
		outstr(outpos:) = '0.0, '
	        outpos = outpos + 5
	   else
	   outstr(outpos:) = line(cpos+1:cppos-1)
	   outpos = outpos + cppos - cpos
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	endif

C and finally add the column names on the end
	   outstr(outpos:) = 
     &         xcolname(1:fcstln(xcolname)) // ', ' // 
     &         ycolname(1:fcstln(ycolname)) // ')'
	   outpos = outpos + fcstln(xcolname) + 
     &                     fcstln(ycolname) + 4

C	 check for annulus
	   bangpos = index(line, '!')
	   if (bangpos .ne. 0) then

C  we have an annulus, and the expression isn't quite right
C  also, print out the string so far and continue on the next line
		write (ounit, 1000) outstr(1:fcstln(outstr))
		outstr = ' '
		outpos = 1

		outstr(outpos:) = ' && (!'  
		outpos = outpos + 6
		oppos = index (line(bangpos:), '(') + bangpos - 1
	   	cpos = index(line(oppos:), ',') + oppos - 1
		cppos = index(line(oppos:), ')') + oppos - 1

C put in the object name
	if (object .ne. 'CIRCLE') then
	   outstr(outpos:) = object(1:fcstln(object)) // '('
	   outpos = outpos + fcstln(object) + 1
	else
	   outstr(outpos:) = 'ELLIPSE('
	   outpos = outpos + 8
	endif

C x
	   offset = .true.
	   outstr(outpos:) = convrtstr (line(oppos+1:cpos-1), 1, offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C y
	   offset = .true.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 2, 
     &                         offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C for circles, there isn't another comma, so use the )
	   if (cpos .eq. oldcpos) cpos = cppos

C a or rx
	   offset = .false.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 1, 
     &                           offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2
	   if (object .eq. 'CIRCLE') goto 300
	   oldcpos = cpos
	   cpos = index(line(oldcpos+1:), ',') + oldcpos

C for boxes and ellipses with no rotation, no angle is written
	   if (cpos .eq. oldcpos) cpos = cppos

C b or ry
300	   offset = .false.
	   outstr(outpos:) = convrtstr (line(oldcpos+1:cpos-1), 2, 
     &                         offset)
	   outpos = outpos + 23
	   outstr(outpos:) = ', '
	   outpos = outpos + 2

C angle (no translation needed)
C for no rotation objects (circle, ellipse or box with no rotation)
	   if ((object .eq. 'CIRCLE') .or. (cpos .eq. cppos)) then
		outstr(outpos:) = '0.0, '
	        outpos = outpos + 5
	   else
	   	outstr(outpos:) = line(cpos+1:cppos-1)
	   	outpos = outpos + cppos - cpos
	   	outstr(outpos:) = ', '
	   	outpos = outpos + 2
	   endif

C and finally add the column names
	   outstr(outpos:) = 
     &         xcolname(1:fcstln(xcolname)) // ', ' // 
     &         ycolname(1:fcstln(ycolname)) // '))'
	   outpos = outpos + fcstln(xcolname) + 
     &                     fcstln(ycolname) + 4

	   endif


	else
	   context = ' object not currently supported: ' //
     &                  object(1:fcstln(object))
	   call fcerr(context)
	   goto 998
	endif

C add final )
	outstr(fcstln(outstr)+1:) = ')'
C add final ) for exclude strings
	if (exclude) outstr(fcstln(outstr)+1:) = ')'

C write the string to the output file
	write (ounit, 1000) outstr(1:fcstln(outstr))

C loop back for next line
	goto 100

997	context = ' Error parsing line ' // line(1:fcstln(line))
	call fcerr (context)

998	close (iunit)
	close (ounit)
999	return
	end

C****************************************************************************** 
C FUNCTION:
C      convrtstr
C
C DESCRIPTION: 
C	converts a string in pixel units to a string in data units
C
C AUTHOR:  
C      Emily A. Greene
C	Hughes STX
C	March, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	converted_string = convrtstr(string, axis, offset)
C
C ARGUMENTS:
C	string  - the string to be converted in pixel units
C	axis    - 1 for X and 2 for Y
C	offset  - whether to apply the offset or not
C
C PRIMARY LOCAL VARIABLES:
C	value    - the value in pixel units
C
C CALLED ROUTINES:
C	pixtodata - converts from pixel to data coordinates
C
C****************************************************************************** 
	character(23) function convrtstr (string, axis, offset)

	character*(*) string
	integer axis
	logical offset

	double precision pixtodata
	double precision value

	read (string, '(f23.0)') value
	write (convrtstr, '(g23.15)') pixtodata(value, axis, offset)
	
	return
	end

C****************************************************************************** 

