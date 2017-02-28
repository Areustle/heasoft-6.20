C******************************************************************************
C SELECTOR TASK:
C      fmemsort
C
C FILE:
C      fmemsort.f
C
C DESCRIPTION:
C      sorts rows from a FITS file for a particular expression
C
C AUTHOR/DATE:
C      Kent Blackburn  3/9/92
C
C MODIFICATION HISTORY:
C      Added a load2mem flag to attempt to speed up IO when system
C      resources were available; JKB - 6/6/94
C
C      8/26/94 EAG 3.0a add clobber, clear FITSIO stack, etc.
C     11/30/1995 JRG 3.0b fmsort: Fix TTYPE
C      2/25/96 (Srilal) 3.0c - timestamp added
C      8/14/97 4.0 - Banashree M Seifert
C                      if input file extension has zero rows, then
C                      instead of erroring out, close the outfile
C                      and send a warning message to screen that it 
C                      has zero rows.
C                      This change is done in subroutine FMSORT.
C                      To do this, introduced few lines to see if
C                      NAXIS2 == 0. Earlier versions this test was
C                      not at all done.
C      10/15/97 PDW 4.0a Replace old get header routines
C      11/10/97 PDW 4.1  Added unique option, and multi-column sorting
C      08/05/98 PDW 4.1a Bug fix in memsort.c for heap sorting tables
C                        with only 1 row
C
C NOTES:
C      fmemsort supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fmemsort
C      IRAF: task fmemsort
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile   - input FITS file and extension number
C      outfile  - output sorted FITS file
C      columns  - column names to sort
C      method   - sorting method to use
C      ascend   - boolean flag for ascend or desend sort
C      load2mem - boolean flag for loading whole table in memory
C      copyprime- whether to copy primary array
C      copyall  - copy all other extensions flag
C      unique   - whether to purge rows with identical keys
C      history  - whether to write history records
C      taskname - name of this task
C
C CALLED ROUTINES:
C      subroutine gmemst - gets parameters from parameter file
C      subroutine fmsort - sort out rows
C
C******************************************************************************

        subroutine fmemst
        character(160) infile, outfile, columns
        character(40)  method
        logical       ascend,load2mem,copyprime,copyall,unique,history
        character(40) taskname
        common /task/ taskname

C  set the task name for the error reports
        taskname = 'fmemsort4.1'

        call ftcmsg

C  get parameters from parameter file
        call gmemst(infile,outfile,columns,method,
     &              ascend,load2mem,copyprime,copyall,unique,history)

C  extract data to new FITS file
        call fmsort(infile,outfile,columns,method,
     &              ascend,load2mem,copyprime,copyall,unique,history)

        return
        end


C****************************************************************************
C SUBROUTINE:
C      gmemst
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C AUTHOR:
C      Kent Blackburn  3/9/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       gmemst uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gmemst(infile,outfile,columns,method,
C     &	           ascend,load2mem,copyprime,copyall,unique,history)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output sorted FITS file
C      columns - column names to sort
C      method  - sorting method to use
C      ascend  - boolean flag for ascend or desend sort
C      load2mem  - boolean flag for loading whole table in memory
C      copyprime - whether to copy primary array
C      copyall - copy all other extensions flag
C      unique  - whether to purge rows with identical keys
C      history - whether to write history records
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

	subroutine gmemst(infile,outfile,columns,method,ascend,
     &	                  load2mem,copyprime,copyall,unique,history)
	character*(*) infile, outfile, columns, method
	logical       ascend,load2mem,copyprime,copyall,unique,history

	character(80) context
	integer status

C  initialize variables
	status = 0

C  get the name of the input FITS file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
	    context = 'could not get INFILE parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output FITS file
	call uclgst('outfile',outfile,status)
	if ( status .ne. 0 ) then
	  context = 'Could not get OUTFILE parameter'
	  call fcerr(context)
	  goto 999
	endif

C  get the names of the columns
	call uclgst('columns',columns,status)
	if (status .ne. 0) then
	    context = 'could not get COLUMNS parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the method
	call uclgst('method',method,status)
	if (status .ne. 0) then
	    context = 'could not get METHOD parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the value of the ascend flag
	call uclgsb('ascend',ascend,status)
	if (status .ne. 0) then
	    context = 'could not get ASCEND parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the value of the load2mem flag
	call uclgsb('load2mem',load2mem,status)
	if (status .ne. 0) then
	    context = 'could not get LOAD2MEM parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the copy primary array boolean flag's value
	call uclgsb('copyprime', copyprime, status)
	if ( status .ne. 0 ) then
	  context = 'Could not get COPYPRIME parameter'
	  call fcerr(context)
	  goto 999
	endif

C  get the copy all other extensions boolean flag's value
	call uclgsb('copyall',copyall,status)
	if ( status .ne. 0 ) then
	  context = 'Could not get COPYALL parameter'
	  call fcerr(context)
	  goto 999
	endif

C  get the unique boolean flag's value
	call uclgsb('unique',unique,status)
	if ( status .ne. 0 ) then
	  context = 'Could not get UNIQUE parameter'
	  call fcerr(context)
	  goto 999
	endif

C  get the history boolean flag's value
	call uclgsb('history',history,status)
	if ( status .ne. 0 ) then
	  context = 'Could not get HISTORY parameter'
	  call fcerr(context)
	  goto 999
	endif

999	continue
	if (status .ne. 0) then
            write (context,'(A19,i4)') ' XPI error number: ', status
	    call fcerr(context)
	    stop
	endif

	return
	end


C******************************************************************************
C SUBROUTINE:
C      fmsort
C
C DESCRIPTION:
C      sort rows from a FITS file extension
C
C AUTHOR/DATE:
C      J. Kent Blackburn 3/9/93
C
C MODIFICATION HISTORY:
C
C   11/30/1995 Jeff Guerber: Changed TTYPE to char*40.
C    8/14/97  - Banashree M Seifert
C               if input file extension has zero rows, then
C               instead of erroring out, close the outfile
C               and send a warning message to screen that it 
C               has zero rows.
C               To do this, introduced 9 lines to check if
C               NAXIS2 == 0. Earlier versions this test was
C               not at all done.
C
C NOTES:
C
C
C USAGE:
C      call fmsort(infile,outfile,columns,method,ascend,
C     &	                  load2mem,copyprime,copyall,unique,history)
C
C ARGUMENTS:
C      infile  - input FITS file and extension number
C      outfile - output sorted FITS file
C      columns - column names to sort
C      method  - sorting method to use
C      ascend  - boolean flag for ascending sort
C      load2mem  - boolean flag for loading whole table in memory
C      copyprime - whether to copy primary array
C      copyall - copy all other extensions flag
C      unique  - whether to purge rows with identical keys
C      history - whether to write history records
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      history - history string
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      bool    - expression evaluates to boolean flag
C      extnum  - extension number in input FITS file
C      ftstat  - fitsio library call return status
C      iunit   - input file unit number
C      record  - string containing contents of one line from eunit file
C      block   - fits file block size
C      htype   - fits file header type
C      bitpix  - number of bits per pixel in primary array
C      naxis   - number of dimensions of in array
C      naxes   - number of points along each dimension
C      pcount  - value of pcount keyword
C      gcount  - value of gcount keyword
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area
C      cols    - array of column numbers to sort
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ft____ - FITSIO routines
C      subroutine fcerr - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************

	subroutine fmsort(infile,outfile,columns,method,ascend,
     &	                  load2mem,copyprime,copyall,unique,history)

	character*(*) infile,outfile,columns,method
	logical ascend,load2mem,copyprime,copyall,unique,history
	integer maxcl
	parameter ( maxcl = 999 )
	character(160) fname
	character(80) context,errstr,comment
	character(80) histkw
	logical simple,extend,exact,inopen,outopen,negflag,goodlist
	integer extnum,ftstat,iunit,ounit,block,htype
	integer rowlen,nrows,repeat(maxcl),dattyp(maxcl),fcafml
        integer modflag(maxcl)
	integer bitpix,naxis,naxes(99),pcount,gcount,excount
	integer tfields,varidat,wd,tcols,nmove,nhdu,wid
        character(40) ttype(maxcl),colist(maxcl)
	character(16) tform(maxcl), form
        character(25) tunit(maxcl)
	character(70) extname
	integer tbcol(maxcl),bcol,cols(maxcl),i
C

C   initialize variables
	ftstat = 0
	iunit = 15
	ounit = 16
	block = 0
	exact = .false.
	inopen = .false.
	outopen = .false.
	context = ' '
	errstr = ' '


C   make the case of the method upper case and verify validity
	call ftupch(method)
	if ((method .ne. 'HEAP') .and.
     &	    (method .ne. 'SHELL')  .and.
     &	    (method .ne. 'INSERT')) then
	  context = 'Sorting method must be SHELL, HEAP or INSERT'
	  call fcerr(context)
	  goto 999
	endif

C   get the filename and extension
	call fcpars(infile,fname,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
	if (extnum .eq. -99) extnum = 1

C   if the extension is less than 1 then give an error and exit
	if (extnum .lt. 1) then
	  context = 'Primary extension not supported'
	  call fcerr(context)
	  goto 999
	endif

C   open the input FITS file
	call ftopen(iunit,fname,0,block,ftstat)
	if ( ftstat .ne. 0 ) then
	  context = 'Unable to open infile'
	  call fcerr(context)
	  goto 999
	endif
	inopen = .true.

C  open output FITS file
	call ffinit(ounit,outfile,ftstat)
	if ( ftstat .ne. 0 ) then
	  context = ' Unable to open OUTFILE, may exist? ' // outfile
	  call fcerr(context)
	  ftstat = 0
	  call ftclos(iunit,ftstat)
	  goto 999
	endif
	outopen = .true.

C  provide a primary header to the outfile based on allflg
	if ((copyall) .or. (copyprime)) then
	  call ftcopy(iunit,ounit,0,ftstat)
	else
	  call ftghpr(iunit,99,simple,bitpix,naxis,naxes,
     &           pcount,gcount,extend,ftstat)
	  simple = .true.
	  naxis = 0
	  pcount = 0
	  gcount = 1
	  call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &                pcount,gcount,extend,ftstat)
	call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,ftstat)
	endif

C  if all extension should be copied then copy each extension
	excount = 0
	if ( copyall ) then
	  nmove = 1
   10	  call ftmrhd(iunit,nmove,htype,ftstat)
	  if ( ftstat .eq. 0 ) then
	    excount = excount + 1
	    if ( excount .eq. extnum .and. history) then
	      call ftcrhd(ounit,ftstat)
	      call ftcopy(iunit,ounit,1,ftstat)
	    else
	      call ftcrhd(ounit,ftstat)
	      call ftcopy(iunit,ounit,0,ftstat)
	    endif
	    goto 10
	  else
	    if ((ftstat.eq.107).or.(ftstat.eq.252)) ftstat = 0
	  endif
	else
	  nhdu = extnum + 1
	  call ftmahd(iunit,nhdu,htype,ftstat)
	  if ( ftstat .ne. 0 ) then
	    errstr = 'Error moving to extension in input file number'
	    write(context,1000) errstr, extnum
	    call fcerr(context)
	    goto 999
	  endif
	  call ftcrhd(ounit,ftstat)
	  if (history) then
	    call ftcopy(iunit,ounit,1,ftstat)
	  else
	    call ftcopy(iunit,ounit,0,ftstat)
	  endif
	endif

C   move to the extension in the input file
	nhdu = extnum + 1
	call ftmahd(iunit,nhdu,htype,ftstat)
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error moving to extension In input file number'
	  write(context,1000) errstr, extnum
 1000	  format(A46,I3)
	  call fcerr(context)
	  goto 999
	endif

C   move to the extension in the output file
	if (copyall) then
	  nhdu = extnum + 1
	  call ftmahd(ounit,nhdu,htype,ftstat)
	else
	  nhdu = 2
	  call ftmahd(ounit,nhdu,htype,ftstat)
	endif
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error moving to extension in output file number'
	  nhdu = nhdu - 1
	  write(context,1001) errstr, nhdu
 1001	  format(A47,I3)
	  call fcerr(context)
	  goto 999
	endif

C   get extension header's keywords depending on extension type
	if ( htype .eq. 1 ) then
	  call ftghtb(iunit,maxcl,rowlen,nrows,tfields,ttype,tbcol,
     &              tform,tunit,extname,ftstat)
	else if ( htype .eq. 2 ) then
	  call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &              extname,varidat,ftstat)
	else
	  context = 'Extension type not supported'
	  call fcerr(context)
	  goto 999
	endif

	if (columns .eq. ' ') then
	  context = 'error column list names blank'
	  call fcerr(context)
	  goto 999
	else
	  call fcgcls(columns,colist,tcols,negflag)
CC
	  call fcgmod(colist,tcols,modflag)
CC
	  call fccmpl(tcols,tfields,colist,ttype,negflag,goodlist)
	  if (.not. goodlist) then
	     context = 'error in column list names'
	     call fcerr(context)
	     goto 999
	  endif
	endif

C-------------------------------------------------------------------- 
C  this block below (9 lines) is introduced for checking that
C  if any empty extension is given, then exit with warning that
C  the extension for sorting does not have any data to sort
C  Banashree M Seifert (August 14, 1997)
C       first check that the file does not contain zero rows.
C       if it contains zero rows, then print warning message
C       and exit gracefully
C ------------------------------------------------------------------
	call ftgkyj(iunit,'NAXIS2',wd,comment,ftstat)
        if(wd .eq. 0) then
           context=' Warning: Input file has no. data (no. of rows=0)'
           call fcerr(context)
           context=' Sorting is not done'
           call fcerr(context)
           goto 999
        endif
        if(ftstat .ne. 0) goto 999


C       get the input row width
	call ftgkyj(iunit,'NAXIS1',wd,comment,ftstat)
     
        exact = .false.
        do 1010 i=1,tcols
C   determine the column number from column name
           call ftgcno(iunit,exact,colist(i),cols(i),ftstat)

C   determine the form of the data
           form = tform(cols(i))
           bcol = tbcol(cols(i))
           if ( htype .eq. 1 ) then
              call fcasfm(form,dattyp(i),ftstat)
              repeat(i) = fcafml(form)
           else if ( htype .eq. 2 ) then
              call ftbnfm(form,dattyp(i),repeat(i),wid,ftstat)
           endif
 1010   continue

	call memsort(iunit,ounit,wd,repeat,nrows,
     &  tcols,cols,dattyp,
     &	method,ascend,load2mem,unique,ftstat,modflag)
        if (ftstat .eq. 1000) then
C delete output file if there was an error
          call ftdelt(ounit, ftstat)
          outopen = .false.
          ftstat = 0
          goto 999
        endif

C   write out a history record
	if (history) then
	  histkw = 'TASK: FMEMSORT on FILENAME: '//fname
	  call ftphis(ounit,histkw,ftstat)
	  call timestamp(ounit)
	endif

  999	continue
	if ( ftstat .ne. 0 ) then
	  call fcerrm(ftstat)
	  ftstat = 0

C delete output file if there was an error
          if (outopen) call ftdelt(ounit, ftstat)
          ftstat = 0
          outopen = .false.
	endif
	if ( inopen ) then
	  call ftclos(iunit,ftstat)
	  ftstat = 0
	endif
	if ( outopen ) then
	  call ftclos(ounit,ftstat)
	  ftstat = 0
	endif
	return
	end

	subroutine fcgmod(colist,tcols,modflag)

        integer tcols
        character*(*) colist(tcols)
        integer modflag(tcols)

        integer i,n,j,k

        do 10 i=1,tcols
        n = len(colist(i))
        do 5 j=n,1,-1
        if ( colist(i)(j:j) .ne. ' ' .and. ichar(colist(i)(j:j)).ne.0)
     *  goto 8
5       continue
8       continue
        n=j
        modflag(i) = -1

        if (colist(i)(1:1) .eq. '%') then
        modflag(i)=1
        do 20 k=1,n
        colist(i)(k:k)=colist(i)(1+k:1+k)
20      continue
        endif

10      continue

        return
        end

