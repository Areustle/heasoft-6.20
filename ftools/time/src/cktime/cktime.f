C******************************************************************************
C SELECTOR TASK:
C      cktime
C
C FILE:
C      cktimeorder.f
C
C DESCRIPTION:
C       Determine if a column in a FITS file is time ordered
C
C AUTHOR/DATE:
C       Kent Blackburn Oct. 12th, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C      cktime supported in IRAF and HOST environments
C
C MAKE:
C
C USAGE:
C      cktimeorder INFILE.FITS TIME
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      INFILE  - input FITS file and extension number
C      COLNAME  - Column value 
C
C CALLED ROUTINES:
C      subroutine gcktim - gets parameters from environment
C      subroutine fcktorder - gets column names from FITS file
C
C******************************************************************************

	subroutine cktime
	character(160) infile
        character * 20 colname 
	integer status
	logical ckeql
	character(40) taskname
	common /task/ taskname

	taskname = 'cktimeorder2.6'
C get parameters from par file
	call gcktim(infile,colname,ckeql,status) 
	if (status .ne. 0) goto 999

C read in and display column names
	call fcktorder(infile,colname,ckeql) 
999	return
	end


C******************************************************************************
C SUBROUTINE:
C      gcktim
C
C DESCRIPTION:
C      gets parameters from the parameter file 
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/12/93
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C       gstat uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gcktim(infile,colname,ckequal,irafsts) 
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      colname - verbose value
C      ckequal - flag to check for equal time stamps
C      irafsts - F77/VOS status value
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      irafsts - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

	subroutine gcktim(infile,colname,ckequal,irafsts) 

	character*(*) infile, colname
	character(80) context
	logical      ckequal
	integer      irafsts

C ---   Initialize variables 
	irafsts = 0

C ---   Get the name of the input FITS file ---
	call uclgst('infile',infile,irafsts)
	if ( irafsts .ne. 0 ) then
	  context = 'Could not get INFILE parameter'
	  call fcerr(context)
	  goto 999
	endif

C ---   Get the COLUMN NAME from the par file.
	call uclgst('colname',colname,irafsts)
	if ( irafsts .ne. 0 ) then
	  context = 'Could not get COLNAME parameter'
	  call fcerr(context)
	  goto 999
	endif

C ---   Get the CKEQUAL FLAG from the par file.
	call uclgsb('ckequal',ckequal,irafsts)
	if ( irafsts .ne. 0 ) then
	  context = 'Could not get COLNAME parameter'
	  call fcerr(context)
	  goto 999
	endif

  999	continue
	return
	end


C******************************************************************************
C SUBROUTINE:
C      fcktorder
C
C DESCRIPTION:
C      This subroutine actually does the reading of time column valuess 
C      helpful to compute mean and standard deviation.
C
C AUTHOR/DATE:
C      James Kent Blackburn 10/12/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C       
C
C USAGE:
C      call fcktorder(infile,colname) 
C
C ARGUMENTS:
C      INFILE  - input FITS file and extension number
C      COLNAME    - Column value 
C
C PRIMARY LOCAL VARIABLES:
C      INFILE   - input FITS file and extension number
C      COLNAME  - Column value 
C      OUTFILE  - output ASCII file containing column names
C      extnumb  - extension number 
C      filename - input FITS filename
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcpars - parse INFILE into a filename and extension
C      subroutine ft____ - FITSIO subroutines
C      subroutine fcecho - echo message to terminal
C
C******************************************************************************

	subroutine fcktorder(infile,colname,ckeql) 

        integer         status,iunit,extnumb,maxcl
	integer         block,nmove,hdutype,nrows,rowlen
	parameter       ( maxcl = 512 ) 
	integer	        maxlst
	parameter       (maxlst = 512)
	integer         tbcol(maxcl),tfields
	integer         varidat,colnum,frow,nelem,felem
C ---
	character*(*)  infile,colname 
	character(70)   ttype(maxcl),tform(maxcl),tunit(maxcl)
	character(70)   extname
	character(160)  filename
	character(80)   context
	character(16)   rownumb
C ---
	logical        flagval,exact,anyf,ckeql,inorder,eqfnd
C ---
        double precision    time_i, time_j
C ---
C Initialize variables
C ---
	iunit = 15
	status = 0
        frow = 1
        felem = 1
	nelem = 1
	exact = .false.
	inorder = .true.
	eqfnd = .false.

C ---   Parse the INFILE into a filename and extension number ---
	call fcpars(INFILE,filename,extnumb,status)
        if (status .ne. 0) goto 999

C ---   default to 1st extension
	if (extnumb .eq. -99) extnumb = 1

C ---   If the extension is 0 the give error and exit
	if ( extnumb .eq. 0 ) then
	  context = 'Primary Extension Not Supported'
	  call fcerr(context)
	  goto 999
	endif

C ---   Open Existing input FITS file ---
	call ftopen(iunit,filename,0,block,status)
	if ( status .ne. 0 ) then
	  context = ' Unable to open INFILE'
	  call fcerr(context)
	  goto 999
	endif
	  
C ---   Move to the extension to be read ---
	nmove = extnumb
	call ftmrhd(iunit,nmove,hdutype,status)

C ---   If status not zero then extension does not exist so exit
	if ( status .ne. 0 ) then
	  context = 'Extension Not Found'
	  call fcerr(context)
	  goto 999
	endif

C ---   If this is not an ASCII or BINARY extension then exit
	if (( hdutype .ne. 1) .and. 
     &      ( hdutype .ne. 2).and.
     &      ( status .eq. 0 )) then
	  context = 'Extension is Not Ascii or Binary'
	  call fcerr(context)
	  goto 999
	endif

C ---   Get the column names.
	if ( hdutype .eq. 1 ) then
	   call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &                tform,tunit,extname,status)
	else       
	   if ( hdutype .eq. 2 ) then
	      call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &                extname,varidat,status)
	   endif
        endif

C ---   now obtain the time column number
        call ftgcno(iunit,exact,colname, colnum, status)
        if (colnum .eq. 0 .or. status .ne. 0) then
           context = ' The time column selected is not found'
           call fcerr(context)
           goto 999
        endif

C ---   Loop over all sets data          
        call ftgcfd(iunit,colnum,frow,felem,nelem,time_i,
     &              flagval,anyf,status)
        if (status .ne. 0) goto 999
        do 100 frow = 2, nrows
          call ftgcfd(iunit,colnum,frow,felem,nelem,time_j,
     &                flagval,anyf,status)
           if ((time_j .eq. time_i) .and. ckeql) then
	     eqfnd = .true.
	     write(rownumb,1000) frow
	     context = 'Same time as previous for row # : '//rownumb
	     call fcecho(context)
	   else if ( time_j .lt. time_i) then
	     inorder = .false.
	     write(rownumb,1000) frow
	     context = 'Out of time order for row # : '//rownumb
	     call fcecho(context)
           endif
	  time_i = time_j
  100	continue 
 1000	format(i11)

C ---   Give a summary
	context = ' '
	call fcecho(context)
	if (inorder) then
	  context = 'Time column is TIME ORDERED'
	  call fcecho(context)
	  if (eqfnd) then
	    context =  '- but Equal time values were found -'
	    call fcecho(context)
	  endif
	endif
	  
  999	continue
        if (status .ne. 0) call fcerrm(status)
	call ftclos(iunit,status)
	return
	end
