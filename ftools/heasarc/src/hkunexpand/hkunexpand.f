C***************************************************************************
C SELECTOR TASK:
C      hkunexpand
C
C FILE:
C      hkunexpand.f
C
C DESCRIPTION:
C      Un-Expand format of Astro-D style House Keeping Fits file 
C
C AUTHOR/DATE:
C      Kent Blackburn  8/24/92
c
C MODIFICATION HISTORY:
C	4/6/93 EAG  Add copyall parameter, remove time-consuming ftgcno call
C 2.6 11/15/93 EAG Allow for real output parameters
C 3.0   2/6/98 NG Add supports for character(30), byte, single
C              precision, and  short integer. However, the scalar
C              columns and string columns can not be mixed.
C              The valid outtype:        "F","E"  single precision float. 
C                                        "R","D"  double precision float.
C                                        "B"  byte.
C                                        "S"  short integer.  
C                                        "I"  integer. 
C                                        "A"  string 
C              If there are  mixed scalar types and the outtype is not 
C              given,  the outtype is  determined by the highest data type.    
C              hierarchy: Byte < short < int     < double. 
C                                        single
C
C 3.1 03/18/98 toliver Replaced call to obsolete fitsio routine 'ftgbnh'
C                      with call to new cfitsio function 'ftghbn'
C
C NOTES:
C      hkunexpand supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fggti
C      IRAF: task fggti
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input GTI FITS file and extension number
C      outfile - output HK FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in outfile which contains names of parameters
C      value   - Column in outfile which contains values of parameters
C      time    - Column in outfile which contains times for parameter values
C      copyall - If true, copies all the other extensions to the outfile
C
C CALLED ROUTINES:
C      subroutine ghkued - gets parameters from parameter file
C      subroutine fiuehk - generates the GTI FITS file 
C
C***************************************************************************

	subroutine hkuned
	character(160) infile,outfile
	character(80)  name, value, time, tnull
	character(40) taskname
	character(1) outtype
	logical copyall
	integer status

	common /task/ taskname
	
	taskname = 'hkunexpand2.6'
	infile  = ' '
	outfile = ' '
	name    = ' '
	value   = ' '
	time    = ' '
	tnull   = ' '

C  get parameters from parameter file
	call ghkued(infile,outfile,name,value,time,tnull,outtype,
     &              copyall, status)
	if (status .ne. 0) goto 999

C  extract data to new FITS file
	call fiuehk(infile,outfile,name,value,time,tnull, outtype,
     &              copyall, status)


999	if (status .ne. 0) call fcerrm(status)

	return
	end


C*************************************************************************** 
C SUBROUTINE:
C      ghkued
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Kent Blackburn  8/24/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       ghkued uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call ghkued(infile,outfile,expr,name,value,time,tnull,outtype,copyall,
C			status)
C
C ARGUMENTS:
C      infile  - input GTI FITS file and extension number
C      outfile - output HK FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in outfile which contains names of parameters
C      value   - Column in outfile which contains values of parameters
C      time    - Column in outfile which contains times for parameter values
C      tnull   - String containing Null value keyword
C      outtype - output column type
C      copyall - If true, copy all the other extensions into outfile
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*************************************************************************** 

	subroutine ghkued(infile,outfile,name,value,time,tnull,
     &                    outtype, copyall, status)

	character*(*) infile,outfile,name,value,time,tnull
	character*(*) outtype
	character(80) context
	logical copyall
	integer status

C  initialize variables
	status = 0

C  get the name of the input FITS file
	call uclgst('infile',infile,status)
	if (status .ne. 0) then
	    context = 'could not get infile parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name of the output FITS file
	call uclgst('outfile',outfile,status)
	if (status .ne. 0) then
	    context = 'could not get outfile parameter'
	    call fcerr(context)
	    goto 999
	endif

C  get the name column
	call uclgst('name',name,status)
	if (status .ne. 0) then
	    context = 'could not get name column'
	    call fcerr(context)
	    goto 999
	endif

C  get the value column
	call uclgst('value',value,status)
	if (status .ne. 0) then
	    context = 'could not get value column'
	    call fcerr(context)
	    goto 999
	endif

C  get the time column
	call uclgst('time',time,status)
	if (status .ne. 0) then
	    context = 'could not get time column'
	    call fcerr(context)
	    goto 999
	endif

C  get the tnull value
	call uclgst('tnull',tnull,status)
	if (status .ne. 0) then
	    context = 'could not get tnull value'
	    call fcerr(context)
	    goto 999
	endif
	
C  get the outtype value
	call uclgst('outtype', outtype,status)
	if (status .ne. 0) then
	    context = 'could not get outtype value'
	    call fcerr(context)
	    goto 999
	endif
	
C  get the copyall value
	call uclgsb('copyall',copyall,status)
	if (status .ne. 0) then
	    context = 'could not get copyall value'
	    call fcerr(context)
	    goto 999
	endif

999	continue
	if (status .ne. 0) then
	    call fcerrm(status)
	    stop
	endif

	return
	end


C***************************************************************************
C SUBROUTINE:
C      fiuehk
C
C DESCRIPTION:
C      expand an HK FITS file extension 
C
C AUTHOR/DATE:
C      J. Kent Blackburn 8/24/92
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C       
C
C USAGE:
C      call fiuehk(infile,outfile,name,value,time,tnull,outtype,copyall,
C			status)
C
C ARGUMENTS:
C      infile  - input GTI FITS file and extension number
C      outfile - output HK FITS file
C      expr    - expression which evaluates to a boolean
C      name    - Column in outfile which contains names of parameters
C      value   - Column in outfile which contains values of parameters
C      time    - Column in outfile which contains times for parameter values
C      tnull   - String containing Null value keyword
C	outtype - output column type
C      copyall - If true, copy all the other extensions into outfile
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      clname  - column name found in FITS index
C      history - history string 
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      outopen - output file open flag
C      extnum  - extension number in input FITS file
C      ftstat  - fitsio library call return status
C      iunit   - input file unit number
C      ounit   - output file unit number
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
C      clnum   - column number
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C      datacode- input data type code of input value column
C      width   - width of the input  value column
C      decimal - decimal width of the input value column (for ASCII table only)
C      repeat  - repeat  of the input value column (for Binary table only)
C      max_width - maximum width   of the input parameter column
C      max_deci  - maximum decimal of the input parameter column
C      highest_code - highest data code  of the input parameter column
C      highest_type - highest data type  of the input parameter column
C      highest_form - highest data form  of the input parameter column
C
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ftopen - open a FITS file
C      subroutine ftinit - initialize a FITS file
C      subroutine ftmrhd - relative move to header in FITS file
C      subroutine ftgtbh - get ascii table header keywords
C      subroutine ftghbn - get binary table header keywords
C      subroutine ftgkys - get character string keyword
C      subroutine ftgkyj - get integer keyword
C      subroutine ftgprh - get primary header in FITS file
C      subroutine ftphpr - put primary header in FITS file
C      subroutine ftpdef - define primary header in FITS file
C      subroutine ftcrhd - create header in FITS file
C      subroutine ftphtb - put ascii table header in FITS file
C      subroutine ftadef - define ascii table header in FITS file
C      subroutine ftphbn - put binary table header in FITS file
C      subroutine ftbdef - define binary table header in FITS file
C      subroutine ftphis - put history keyword in FITS file
C      subroutine fimrow - move rows from input to output FITS file
C      subroutine ftclos - close a FITS file
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C***************************************************************************

	subroutine fiuehk(infile,outfile,name,value,time,tnull,
     &                    outtype, copyall, ftstat)

	character*(*) infile,outfile,name,value,time,tnull,outtype
	integer maxcl
	parameter ( maxcl = 999 )
	character(160) fname
	character(80) context,errstr
 	character(80) history,record,comment
	logical simple,extend,exact,inopen,outopen,copyall
	integer extnum,ftstat,iunit,ounit,block,htype,bitpix,inull
	integer naxis,naxes(99),pcount,gcount,rowlen,nrows,nnames
	integer i,j,tfields,varidat,count,colprm,colval,coltim,jvalue
	character(70) ttype(maxcl),tform(maxcl),tunit(maxcl)
	character(70) narray(maxcl),extname
	integer tbcol(maxcl),moreky,naxis1
        integer width(maxcl),decimals(maxcl),repeat(maxcl)
        integer datacode(maxcl)
	integer hdatacode
	integer max_width
	integer max_deci
        integer highest_code 
        character(1) highest_type
	character(6) highest_form
	integer string_flag
	integer scalar_flag
	integer ii
	integer itmp,jtmp
	character(1) datatag
C   initialize variables
	iunit = 15
	ounit = 16
	block = 0
	colprm = 0
	colval = 0
	coltim = 0
	inull = 0
	exact = .false.
	inopen = .false.
	outopen = .false.
	context = ' '
	errstr = ' '

C   get the filename and extension
	call fcpars(infile,fname,extnum,ftstat)

C EAG 8/25/93 default to 1st extension
	if (extnum .eq. -99) extnum = 1

C   if the extension is 0 then give an error and exit
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

C   read in the primary array header required keywords
	call ftgprh(iunit,simple,bitpix,naxis,naxes,
     &	            pcount,gcount,extend,ftstat)

C   move to the extension in the input file
	call ftmrhd(iunit,extnum,htype,ftstat)
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error moving to extension number '
	  write(context,1000) errstr, extnum
 1000	  format(A34,I3)
	  call fcerr(context)
	  goto 999
	endif

C   get extension header's keywords depending on extension type
	if ( htype .eq. 1 ) then
	  call ftgtbh(iunit,rowlen,nrows,tfields,ttype,tbcol,
     &              tform,tunit,extname,ftstat)
	else if ( htype .eq. 2 ) then
	  call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform,tunit,
     &              extname,varidat,ftstat)
	else 
	  context = 'Extension type not supported'
	  call fcerr(context)
	  goto 999
	endif
	
	nnames = tfields - 1
	j = 1
	string_flag = 0
	scalar_flag = 0
	do 10 i = 1, tfields
	  if ( ttype(i) .ne. time ) then
	    narray(j) = ttype(i)
	    j = j + 1
	  endif
   10	continue

C   verify time column to be present

	call ftgcno(iunit,exact,time,coltim,ftstat)
	if ((coltim .eq. 0).or.(ftstat .ne. 0)) then
	  context = 'Time column not found'
	  call fcerr(context)
	  goto 999
	endif

C
C   get the datacode of the output data type.
C
       out_datacode = 0
       if(outtype.eq.'F'.and.outtype.eq.'E') out_datacode = 42
       if(outtype.eq.'R'.and.outtype.eq.'D') out_datacode = 82
       if(outtype.eq.'B') out_datacode = 11
       if(outtype.eq.'S') out_datacode = 21
       if(outtype.eq.'I') out_datacode = 41
       if(outtype.eq.'A') out_datacode = 16
C
C  get the datacode, width, decimals, and repeat and find the maximum 
C  width. Do some error checkings.  
C  
        max_width = 0 
        highest_code = 0
        highest_type = ' '
        max_deci = 0
        do 20 i = 1, tfields
          if(htype.eq.1) then 
	    datacode(i)=0
	    width(i)=0
	    decimals(i)=0
            call ftasfm(tform(i),datacode(i), width(i), 
     *                  decimals(i), status)
          else if(htype.eq.2) then
	    datacode(i)=0
	    width(i)=0
	    repeat(i)=0
            call ftbnfm(tform(i),datacode(i), repeat(i), 
     *                  width(i), status)
c
c             excluding the scalar array.
c
            if((datacode(i) .ne. 16) .and.(repeat(i).gt.1)) then
              context = 'The scalar vector is not supported: tform '//
     *                  tform(i)
              call fcerr(context)
              goto 999
            endif
          endif
          
          if(ttype(i).ne.time) then
C
C         Verify  the tform of the value is Byte, short, 
C         Integer, real, double.
C         The complex and double complex are not supported.
C
            if((datacode(i) .ne. 11).and.
     *        (datacode(i) .ne. 16).and.
     *        (datacode(i) .ne. 21).and.
     *        (datacode(i) .ne. 41).and.
     *        (datacode(i) .ne. 42).and.
     *        (datacode(i) .ne. 82))  then
	      ii = index(tform(i),' ')
              context =
     *        'The data type of value '//
     *        tform(i)(1:ii)//
     *       ' is not supported.'//
     *       'only the data types of '//
     *       'B, I, J, E, F, D, A are valid.'
             call fcerr(context)
             goto 999
            endif
C
C          Check whether there is a mixed string and scalar column in the input 
C          files.
C
	    if(datacode(i).eq.16)string_flag=1
	    if(datacode(i).ne.16)scalar_flag=1
	    if(scalar_flag.eq.1.and.string_flag.eq.1) then 
                context = 'check the tforms of input.'//
     *   ' Combination of string and scalar col. is not allowed.'
                call fcerr(context)
                goto 999
            endif
C
C         only supporte string with max. length of 30.
C
          if((datacode(i).eq.16). and. (width(i).gt. 30)) then 
             context = 'The maximum length of string is 30'
             call fcerr(context)
             goto 999
          endif
C
C      avoid conflicts between the value datatypes of  input and
C      output file. The scalar and string parameters can not be 
C      mixed.
C

          if(((out_datacode.eq.16).and.(datacode(i).ne.16)).or.
     *      ((out_datacode.ne.0).and.(out_datacode.ne.16)
     *       .and.(datacode(i).eq.16))) then
	     call code2tag2(datacode(i),datatag)
             context = 'input value type '//datatag //
     *              ' can not be converted to outtype '//outtype
             call fcerr(context)
             goto 999
          endif  

          if(highest_code.lt.datacode(i))  then
C      do not promote the integer to single precision because of the 
C      possibility of losing precision. Instead, promote the integer 
C      to double precision. 
	      if(highest_code.eq.41) then 
		 highest_code = 82   
              else
                 highest_code = datacode(i)
              endif
              if(max_width.lt.width(i)) 
     *           max_width = width(i)
              if(max_deci.lt.decimals(i)) 
     *           max_deci = decimals(i)
	  else
	      if((highest_code.eq.42).and.(datacode(i).eq.41)) then
		 highest_code = 82
	         max_width = 23
	         max_deci  = 15
              endif
          endif
       endif
20     continue
       if(highest_code.eq.82) then
	     max_width = 23
	     max_deci  = 15
       endif
          
       call code2tag2(highest_code,highest_type)

       if((out_datacode.ne.0).and.(out_datacode.lt.highest_code)) then
         context = 'Warning: The type of input value '// 
     *              highest_type//' demotes to '//outtype
         call fcerr(context)
       endif

       if(out_datacode.eq.42.and.highest_code.eq.41) then
         context = 'Warning: Input value type '
     *              //highest_type//
     *             ' is converted to outtype '//outtype
     *              //'. Loss of precision.'
         call fcerr(context)
       endif


C
C   if the outtype is not given, construct the highest form in input for
C   later use.
C     
       if(out_datacode .eq.0) then 
         if(highest_type.eq.' ') then 
           context = 'Warning: can not find' //
     *        ' the highest data type in datafile'
           call fcerr(context)
           goto 999
         endif
         if(htype.eq.1) then
           if(highest_code.eq.42) write(highest_form,742)max_width,
     *        max_deci
           if(highest_code.eq.82) write(highest_form,782)max_width,
     *        max_deci
           if(highest_code.eq.11) write(highest_form,711)max_width
           if(highest_code.eq.21) write(highest_form,721)max_width
           if(highest_code.eq.41) write(highest_form,741)max_width
           if(highest_code.eq.16) write(highest_form,716)max_width
742      format('E',i2,'.',i2)
782      format('E',i2,'.',i2)
711      format('B',i5)
721      format('I',i5)
741      format('J',i5)
716      format('A',i5)
	 call  frmblk(highest_form)
         else if (htype.eq.2) then 
           if(highest_code.eq.42) write(highest_form,842)
           if(highest_code.eq.82) write(highest_form,882)
           if(highest_code.eq.11) write(highest_form,811)
           if(highest_code.eq.21) write(highest_form,821)
           if(highest_code.eq.41) write(highest_form,841)
           if(highest_code.eq.16) write(highest_form,816)max_width
842      format('1E',4x)
882      format('1D',4x)
811      format('1B',4x)
821      format('1I',4x)
841      format('1J',4x)
816      format(i5,'A')
	 call  frmblk(highest_form)
         endif
       endif


 

C   move to the primary array in the input file
	call ftmahd(iunit,1,htype,ftstat)
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error moving to primary array '
	  write(context,1000) errstr, extnum
	  call fcerr(context)
	  goto 999
	endif

C   open the output FITS file
        call ftinit(ounit,outfile,block,ftstat)
        if (ftstat .ne. 0) then
            context = 'unable to open outfile'
            call fcerr(context)
            goto 999
        endif
        outopen = .true.

C   copy primary header for OUTPUT file
	moreky = 0
        call ftcopy(iunit,ounit,moreky,ftstat)

C copy any extensions before the extension to be operated on
	if ((copyall) .and. (extnum .gt. 1)) then
		do 100 i=1, extnum-1
			call ftmrhd (iunit, 1, htype, ftstat)
			call ftcrhd (ounit, ftstat)
			call ftcopy (iunit, ounit, 0, ftstat)
100		continue
		if (ftstat .ne. 0) then
			context = ' error copying extensions'
			call fcerr (context)
			goto 999
		endif
	endif
	
C   move to the extension in the input file
	call ftmahd(iunit,extnum + 1,htype,ftstat)
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error moving to extension number '
	  write(context,1000) errstr, extnum
	  call fcerr(context)
	  goto 999
	endif

C   create a new extension in the output FITS file
        call ftcrhd(ounit,ftstat) 

C   Initialize Header Keywords for the GTI FITS file
	tfields = 3
	ttype(1) = name
	ttype(2) = value
	ttype(3) = time
	if ( htype .eq. 1 ) then
	        tform(1) = 'A10'
	        tbcol(1) = 1
c               tbcol(2) = 9
                tbcol(2) = 11
		if      (outtype .eq. 'F'.or.outtype.eq.'E') then
	          tform(2) = 'E15.7'
	          tbcol(3) = tbcol(2) + 15
		else if (outtype .eq. 'R'.or.outtype.eq.'D') then
	          tform(2) = 'E23.15'
	          tbcol(3) = tbcol(2) + 23
		else if (outtype .eq. 'B') then
	          tform(2) = 'I3'
	          tbcol(3) = tbcol(2) + 3
		else if (outtype .eq. 'S') then
	          tform(2) = 'I6'
	          tbcol(3) = tbcol(2) + 6
		else if (outtype .eq. 'I') then
	          tform(2) = 'I11'
	          tbcol(3) = tbcol(2) + 11
		else if (outtype .eq. 'A') then
	          tform(2) = 'A30'
	          tbcol(3) = tbcol(2) + 30
                else 
                  tform(2) = highest_form 
                  tbcol(3) = tbcol(2) + max_width
		endif
	  tform(3) = 'E23.15'
	  rowlen = tbcol(3) + 23
	else
	  tform(1) = '10A'
		if      (outtype .eq. 'F'.or.outtype.eq.'E') then
	  tform(2) = '1E'
	        else if (outtype .eq. 'R'.or.outtype.eq.'D') then
	  tform(2) = '1D'
		else if (outtype .eq. 'B') then
	  tform(2) = '1B'
		else if (outtype .eq. 'S') then
	  tform(2) = '1I'
		else if (outtype .eq. 'I') then
	  tform(2) = '1J'
		else if (outtype .eq. 'A') then
	  tform(2) = '30A'
		else 
          tform(2) = highest_form 
		endif
	  tform(3) = '1D'
	endif
	tunit(1) = ' '
	tunit(2) = ' '
	tunit(3) = 'sec'

C      get the outtype if it was not given
C
        if(outtype.ne.'F'.and.outtype.ne.'R'.and.
     *     outtype.ne.'B'.and.outtype.ne.'S'.and.
     *     outtype.ne.'D'.and.outtype.ne.'E'.and.
     *     outtype.ne.'I'.and.outtype.ne.'A') then
          if ( htype .eq. 1 ) then
              call ftasfm(highest_form, hdatacode, itmp,
     *              jtmp, status)
          else if ( htype .eq. 2 ) then
              call ftbnfm(highest_form, hdatacode, itmp,
     *             jtmp, status)
          endif
	  call code2tag2(hdatacode,outtype)
        endif


	extname = 'UNEXP_HK'

C   write extension header's keywords depending on the table type
	if ( htype .eq. 1 ) then
	  call ftphtb(ounit,rowlen,nrows,tfields,ttype,tbcol,
     &                tform,tunit,extname,ftstat)
	  call xhkued(iunit,ounit,ftstat)
	  call ftadef(ounit,rowlen,tfields,tbcol,tform,nrows,
     &	              ftstat)
	else if ( htype .eq. 2 ) then
	  call ftphbn(ounit,nrows,tfields,ttype,tform,tunit,
     &                extname,varidat,ftstat)
	  call xhkued(iunit,ounit,ftstat)
	  call ftbdef(ounit,tfields,tform,varidat,nrows,
     &	              ftstat)
	endif

C   define undefined pixels values 
	if ( tnull .ne. ' ' ) then
	  read(tnull,1010) inull
	  record = 'NULL value for Parameter column'
	  if ( htype .eq. 1 ) then
	    call ftsnul(ounit,2,tnull,ftstat)
	    call ftpkns(ounit,'TNULL',2,1,tnull,record,ftstat)
	  endif
	  if ( htype .eq. 2 ) then
	    call fttnul(ounit,2,inull,ftstat)
	    call ftpknj(ounit,'TNULL',2,1,inull,record,ftstat)
	  endif
	endif
 1010	format(BN,I9)

C   write out the history record
	  history = 'TASK: hkunexpand on FILENAME: '//fname
	  call ftphis(ounit,history,ftstat)

C   write out the parameter values in the table
        if(outtype.eq.'A')  then
	   call fgpnvt_a(iunit,ounit,tnull,coltim,nrows,
     &              nnames,narray,count,ftstat)
        else  
	   call fgpnvt(iunit,ounit,tnull,coltim,nrows,
     &              nnames,narray,count,ftstat)
        endif

C   update the NAXIS2 keyword which holds the number of rows
	call ftgkyj(ounit,'NAXIS2',jvalue,record,ftstat)
	call ftmkyj(ounit,'NAXIS2',count,record,ftstat)

C   Now redefine the size of the extension
        call ftgkyj(ounit,'NAXIS1',naxis1,context,ftstat)
        call ftddef(ounit,count*naxis1,ftstat)

C Write the new HDUCLASS keywords
	comment = 'format conforms to OGIP/GSFC conventions'
	call ftpkys(ounit,'HDUCLASS','OGIP',comment,ftstat)
	comment = 'Extension contains temporal data'
	call ftpkys(ounit,'HDUCLAS1','TEMPORALDATA ',comment,ftstat)
	comment = 'Extension contains housekeeping parameters'
	call ftpkys(ounit,'HDUCLAS2','HKP',comment,ftstat)
	comment = 'Only changes in HKP status are given'
	call ftpkys(ounit,'HDUCLAS3','DELTAS',comment,ftstat)

	if ( copyall) then
300	   call ftmrhd (iunit, 1, htype, ftstat)
	   call ftcrhd (ounit, ftstat)
	   call ftcopy (iunit, ounit, 0, ftstat)
	   if (ftstat .eq. 0) goto 300
	   ftstat = 0
	endif
      
C   if error has occurred report it
	if ( ftstat .ne. 0 ) then
	  call fcerrm(ftstat)
	endif

999	continue
	if ( inopen ) then
	  ftstat = 0
	  call ftclos(iunit,ftstat)
	endif
	if ( outopen ) then
	  ftstat = 0
	  call ftclos(ounit,ftstat)
	endif
	return
	end


C***************************************************************************
C SUBROUTINE:
C      fgpnvt
C
C DESCRIPTION:
C      write out all scalar parameters in an un-expanded format
C
C AUTHOR/DATE:
C      J. Kent Blackburn 8/24/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fgpnvt(iunit,ounit,tnull,cltime,inrows,
C    &             nnames,narray,count,ftstat)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      ounit    - output file unit number
C      inrows   - number of rows in input file
C      tnull    - string containing null value
C      cltime   - column no in input file containing times
C      nname    - number of distinct named parameters
C      narray   - array containing parameter names 
C      count    - number of rows in output file
C      ftstat   - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine ftpcld - put FITS table column array of doubles
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C***************************************************************************

	subroutine fgpnvt(iunit,ounit,tnull,cltime,inrows,
     &             nnames,narray,count,ftstat)

	integer iunit,ounit,inrows,nnames,ftstat
	integer cltime,count
	character*(*) tnull
	integer maxcl
	parameter ( maxcl = 999 )
	integer i,j,clnum(maxcl),cln,clv,clt,lrow
	character(70) narray(maxcl), context
	double precision curtim
	double precision curval(maxcl)
	double precision value, inull
	logical undef,anyf,exact,tflag

C   Initialize current values to uninitialized settings
	cln = 1
	clv = 2
	clt = 3
	lrow = 1
	exact = .false.
	count = 0
	inull = 0.D0
	if (tnull .ne. ' ') then
	  read(tnull,1000) inull
	endif
 1000	format(BN,I9)
	do 10 i = 1, nnames
	  curval(i) = 0.D0
	   call ftgcno(iunit,exact,narray(i),clnum(i),ftstat)
   10	continue

C  Loop through all the input rows
	do 50 j = 1, inrows
	  anyf = .false.
	  tflag = .false.
	
C  Get this rows time stamp
	call ftgcfd(iunit,cltime,j,1,1,curtim,tflag,anyf,ftstat)
	
C  Get the current rows list of parameter values
	do 30 i = 1, nnames
	  undef = .false.
	  anyf = .false.
	   call ftgcfd(iunit,clnum(i),j,1,1,value,undef,anyf,ftstat)
	   if (ftstat .ne. 0) then
		context = ' Error reading input file - ASCII column?'
		call fcerr (context)
		goto 999
	   endif
	   if (undef .and. (tnull .ne. ' ')) then
	     value = inull
	   endif
	   
C  If this is initial row or a value has changed then write it out
	   if ((j .eq. 1).or.(value .ne. curval(i))) then
	     call ftpcls(ounit,cln,lrow,1,1,narray(i),ftstat)
	     call ftpcld(ounit,clv,lrow,1,1,value,ftstat)
	     if (undef .and. (tnull .ne. ' ')) then
	     	call ftpclu(ounit,clv,lrow,1,1,ftstat)
	     endif
	     call ftpcld(ounit,clt,lrow,1,1,curtim,ftstat)
	     lrow = lrow + 1
	     curval(i) = value
	   endif
   30	continue

   50	continue
	
	count = lrow - 1
999	return
	end

C***************************************************************************
C SUBROUTINE:
C      fgpnvt_a 
C
C DESCRIPTION:
C      write out all string parameters in an un-expanded format
C
C AUTHOR/DATE:
C      N. Gan 2/6/98
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C     Modified from Kent Blackburn's fgpnvt
C
C USAGE:
C      call fgpnvt_a(iunit,ounit,tnull,cltime,inrows,
C    &             nnames,narray,count,ftstat)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      ounit    - output file unit number
C      inrows   - number of rows in input file
C      tnull    - string containing null value
C      cltime   - column no in input file containing times
C      nname    - number of distinct named parameters
C      narray   - array containing parameter names 
C      count    - number of rows in output file
C      ftstat   - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine ftpcld - put FITS table column array of doubles
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C***************************************************************************

	subroutine fgpnvt_a(iunit,ounit,tnull,cltime,inrows,
     &             nnames,narray,count,ftstat)

	integer iunit,ounit,inrows,nnames,ftstat
	integer cltime,count
	character*(*) tnull
	integer maxcl
	parameter ( maxcl = 999 )
	integer i,j,clnum(maxcl),cln,clv,clt,lrow
	character(70) narray(maxcl), context
	double precision curtim
	character(30) curval(maxcl)
	character(30) value, inull
        character(1) anull
	logical undef,anyf,exact,tflag
        logical changed

C   Initialize current values to uninitialized settings
	cln = 1
	clv = 2
	clt = 3
	lrow = 1
	exact = .false.
	count = 0
        anull = char(32)
        write(inull,1000)(anull,i=1,30)
1000    format(30a)

	do 10 i = 1, nnames
	  curval(i) = inull
	   call ftgcno(iunit,exact,narray(i),clnum(i),ftstat)
   10	continue

C  Loop through all the input rows
	do 50 j = 1, inrows
	  anyf = .false.
	  tflag = .false.
	
C  Get this rows time stamp
	call ftgcfd(iunit,cltime,j,1,1,curtim,tflag,anyf,ftstat)
	
C  Get the current rows list of parameter values
	do 30 i = 1, nnames
	  undef = .false.
	  anyf = .false.
	   call ftgcfs(iunit,clnum(i),j,1,1,value,undef,anyf,ftstat)
	   if (ftstat .ne. 0) then
		context = ' Error reading input file - ASCII column?'
		call fcerr (context)
		goto 999
	   endif
	   if (undef .and. (tnull .ne. ' ')) then
	     value = inull
	   endif
	   
C  If this is initial row or a value has changed then write it out
           changed = lgt(value,curval(i)).or.
     *               llt(value,curval(i))
	   if ((j .eq. 1).or.changed) then
	     call ftpcls(ounit,cln,lrow,1,1,narray(i),ftstat)
	     call ftpcls(ounit,clv,lrow,1,1,value,ftstat)
	     if (undef .and. (tnull .ne. ' ')) then
	     	call ftpclu(ounit,clv,lrow,1,1,ftstat)
	     endif
	     call ftpcld(ounit,clt,lrow,1,1,curtim,ftstat)
	     lrow = lrow + 1
	     curval(i) = value
	   endif
   30	continue

   50	continue
	
	count = lrow - 1
999	return
	end


C***************************************************************************
C SUBROUTINE:
C      xhkued
C
C DESCRIPTION:
C      This subroutine moves the extra keywords,i.e., the
C      keywords which don't contain: XTENSION,BITPIX,
C      NAXIS*,PCOUNT,GCOUNT,TFIELDS,TTYPE*,TFORM*,TUNIT*,
C      END,TBCOL*,and EXTNAME, from the input file to 
C      the output file
C
C AUTHOR/DATE:
C      James Kent Blackburn 8/24/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C USAGE:
C      call xhkued(iunit,ounit,fname,status)
C
C ARGUMENTS:
C      iunit - input unit number
C      ounit - output unit number
C      fname - input FITS file name
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      i* -index to substring
C      l* - substring presence flag
C      nkeys - number of keywords
C      copyflg - copy keyword flag
C
C CALLED ROUTINES:
C      subroutine ftghsp - get number of keywords in extension
C      subroutine ftgrec - get keyword record
C      subroutine ftprec - put keyword record
C
C***************************************************************************

        subroutine xhkued(iunit,ounit,status)

        integer     iunit,ounit,status
        integer     i1,i2,i3,i4,i5,i6,i7,i8,i9,ia,ib,ic,id,ie
        integer     i,nkeys,nmore
        logical     l1,l2,l3,l4,l5,l6,l7,l8,l9,la,lb,lc,ld,le
        logical     copyflg
	character(80) record

        call ftghsp(iunit,nkeys,nmore,status)
        do 10 i = 1, nkeys
          call ftgrec(iunit,i,record,status)
          i1 = index(record(1:8),'XTENSION')
          i2 = index(record(1:6),'BITPIX')
          i3 = index(record(1:5),'NAXIS')
          i4 = index(record(1:6),'PCOUNT')
          i5 = index(record(1:6),'GCOUNT')
          i6 = index(record(1:7),'TFIELDS')
          i7 = index(record(1:5),'TTYPE')
          i8 = index(record(1:5),'TFORM')
          i9 = index(record(1:5),'TUNIT')
          ia = index(record(1:7),'EXTNAME')
          ib = index(record(1:3),'END')
          ic = index(record(1:5),'TBCOL')
          id = index(record(1:5),'TNULL')
          ie = index(record(1:7),'HDUCLAS')
          l1 = i1 .eq. 0
          l2 = i2 .eq. 0
          l3 = i3 .eq. 0
          l4 = i4 .eq. 0
          l5 = i5 .eq. 0
          l6 = i6 .eq. 0
          l7 = i7 .eq. 0
          l8 = i8 .eq. 0
          l9 = i9 .eq. 0
          la = ia .eq. 0
          lb = ib .eq. 0
          lc = ic .eq. 0
          ld = id .eq. 0
          le = id .eq. 0
          copyflg = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. la
     &        .and. lb .and. lc .and. ld .and. le
          if ( copyflg ) then
            call ftprec(ounit,record,status)
          end if
   10   continue
        return
        end
        


C*************************************************************************** 
C
C    subroutine code2tag2(datacode, datatag) 
C  
C DESCRIPTION:  
C     Convert the internal datacode to the data type tag.
C
C
C AUTHOR/DATE: 
C     Ning Gan 2/25/98
C
C NOTES:
C     It should be same as the code2tag2 in hkexpand.
C
C MODIFICATION HISTORY:
C
C*************************************************************************** 
	  subroutine code2tag2(datacode, datatag)
	  integer datacode
	  character(1) datatag
	  datatag = ' '
          if(datacode.eq.11) datatag='B'
          if(datacode.eq.16) datatag='A'
          if(datacode.eq.21) datatag='S'
          if(datacode.eq.41) datatag='I'
          if(datacode.eq.42) datatag='F'
          if(datacode.eq.82) datatag='D'
	  return
	  end
