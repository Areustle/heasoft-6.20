C SELECTOR TASK:
C      hkexpand
C
C FILE:
C      hkexpand.f
C
C DESCRIPTION:
C      Expand format of Astro-D style House Keeping Fits file 
C
C AUTHOR/DATE:
C      Kent Blackburn  8/18/92
C
C MODIFICATION HISTORY:
C      JKB 10/28/92 Made changes for Quick Analysis Integration
C      EA  1/22/93 Added BN to 2 format statments for VAX
C      JCI  1/29/93 maxcl -> 999, added error for more than 512 output
C      JCI  2/05/93 Added CONSTDIV, to write out for mult. of dtime
C      JCI  3/12/93 Added copyall parameter, task common block and
C                      fcecho -> fcerr where appropriate
C  2.6 EAG 11/15/93 Output column type can be real
C  3.0  NG   2/1/98  Added supports for character(30), byte, single precision, and 
C                    short integer. However, the scalar columns and string 
C                    columns can not be mixed. 
C                    The valid outtype: "E","E"  single precision float. 
C                                       "D","R"  double precision float.
C                                       "B"  byte.
C                                       "S"  short integer. 
C                                       "I"  integer. 
C                                       "A"  string (with 30 characters) 
C		     Added the new routine fgppvl_a to process the string 
C                    The default datatype of the output value column now is the 
C                    same as the datatype of the input value column.
C  3.1 toliver 03/18/98 Replaced call to obsolete fitsio routine 'ftgbnh'
C                       with call to new cfitsio function 'ftghbn'
C  3.2 ngan    05/18/98 Fixed a bug for the option dtime < 0.  
C  3.3 ngan    05/18/98 Added a new feature:
C                       dtime < -1.0:  The parameters are
C                       written to the oupput file only if
C                       at least one of them has been changed.
C                       This feature prevents recording the multiple
C                       identicle parameter lists.
C  3.4 ngan    05/19/98 Fixed a bug of dealing tnull and dtime < -1.0.
C  3.5 ngan    05/21/98 minor bug fix.
C  3.6 ngan    04/25/00 Removed the extraneous rows in the output file.
C                       Added the check for checksum and datasum keyword
C                       in xhkexd.
C              05/03/00 Fixed "pass EOF" bug. 
C  3.7 ngan    05/08/00 Defined a temp work array to avoid reading/writing
C                       the output file twice. There will be no extraneous rows 
C                       in the output file. 
C                        
C
C NOTES:
C      hkexpand supported in IRAF and HOST environments
C
C USAGE:
C      HOST: call fggti
C      IRAF: task fggti
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      param   - parameter names list
C      dtime   - delta time between value updates
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C	 constdiv - If true, only write out at integral multiples of dtime
C      copyall - If true, copy all the other extensions into outfile
C
C CALLED ROUTINES:
C      subroutine ghkexd - gets parameters from parameter file
C      subroutine fiexhk - generates the GTI FITS file 
C
C***************************************************************************
	subroutine hkexpd
        character(40) taskname
	character(160) infile,outfile
	character(80)  name,value,time,tnull
	character(256)  param
	character(1) outtype
	logical constdiv, copyall
	double precision dtime
        common /task/ taskname

        taskname = 'hkexpand3.7'

	infile  = ' '
	outfile = ' '
	param   = ' '
	name    = ' '
	value   = ' '
	time    = ' '
	tnull   = ' '
	dtime   = -1.0
	outtype = ' '

C  get parameters from parameter file
	call ghkexd(infile,outfile,param,dtime,name,value,
     &                           time,tnull,outtype,constdiv,copyall)

C  extract data to new FITS file
	call fiexhk(infile,outfile,param,dtime,name,value,
     &                           time,tnull,outtype,constdiv,copyall)

	return
	end


C*************************************************************************** 
C SUBROUTINE:
C      ghkexd
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      Kent Blackburn  8/18/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C       ghkexd uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call ghkexd(infile,outfile,param,dtime,name,value,
C                                        time,tnull,outtype,constdiv,copyall)
C
C ARGUMENTS:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      param   - parameter names list
C      dtime   - delta time between value updates
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C      tnull   - String containing Null value keyword
C	outtype - output column type
C	 constdiv - If true, write only on integral multiples of dtime
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

	subroutine ghkexd(infile,outfile,param,dtime,
     &	                 name,value,time,tnull,outtype,constdiv,copyall)

	character*(*) infile,outfile,param,name,value,time,tnull
	character*(*) outtype
	double precision dtime
	character(80) context
	logical constdiv,copyall
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

C  get the list of parameters in FITS file
	call uclgst('param',param,status)
	if (status .ne. 0) then
	    context = 'could not get parameter list'
	    call fcerr(context)
	    goto 999
	endif

C  get the value of dtime for row updates
	call uclgsd('dtime',dtime,status)
	if (status .ne. 0) then
	    context = 'could not get dtime value'
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
	call uclgst('outtype', outtype, status)
	if (status .ne. 0) then
	    context = 'could not get outtype value'
	    call fcerr(context)
	    goto 999
	endif

C  get the constdiv value
	call uclgsb('constdiv',constdiv,status)
	if (status .ne. 0) then
	    context = 'could not get constdiv value'
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
C      fiexhk
C
C DESCRIPTION:
C      expand an HK FITS file extension 
C
C AUTHOR/DATE:
C      J. Kent Blackburn 8/18/92
C
C MODIFICATION HISTORY:
C       JKB 10/27/92 Added support for a list of HK parameter names
C
C NOTES:
C       
C
C USAGE:
C      call fiexhk(infile,outfile,param,dtime,name,value,
C                  time,tnull,outtype,constdiv,copyall)
C
C ARGUMENTS:
C      infile  - input HK FITS file and extension number
C      outfile - output GTI FITS file
C      param   - parameter names list
C      dtime   - delta time between value updates
C      name    - Column in infile which contains names of parameters
C      value   - Column in infile which contains values of parameters
C      time    - Column in infile which contains times for parameter values
C      tnull   - String containing Null value keyword
C	outtype - output column type
C      constdiv - If true, only write out at multiples of dtime
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
C      tform_input   - array of column formats for input file.
C      tbcol   - column number of first char in each field
C      extname - extension name
C      datacode- input data type code of input value column  
C      width   - width of the input  value column
C      decimal - decimal width of the input value column (for ASCII table only)
C      repeat  - repeat  of the input value column (for Binary table only)
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

	subroutine fiexhk(infile,outfile,param,dtime,
     &	              name,value,time,tnull,outtype,constdiv,copyall)

        implicit none
	character*(*) infile,outfile,param,name,value,time,tnull
	character*(*) outtype
	double precision dtime
	integer maxcl
	parameter ( maxcl = 999 )
	character(160) fname
	character(80) context,errstr
	character(80) history,record
	logical simple,extend,exact,inopen,outopen
	logical nflg,gdlst,constdiv,copyall
	integer extnum,ftstat,iunit,ounit,block,htype,bitpix
	double precision inull
	integer naxis,naxes(99),pcount,gcount,rowlen,nrows,nnames,pnames
	integer i,tfields,varidat,count,colprm,colval,coltim,jvalue
	character(70) ttype(maxcl),tform(maxcl),tunit(maxcl)
        character(70) tform_input(3)
	character(70) narray(maxcl),parray(maxcl),extname
	integer tbcol(maxcl),moreky,drctn,naxis1
        integer datacode, repeat, width, decimals
        integer out_datacode 
        character(1) datatag 
        integer ii, status

C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

      integer p_time

	
C   initialize variables
	ftstat = 0
	iunit = 15
	ounit = 16
	block = 0
	colprm = 0
	colval = 0
	coltim = 0
	inull = 0.D0
	exact = .false.
	inopen = .false.
	outopen = .false.
	context = ' '
	errstr = ' '
	i =0

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
     &              tform_input,tunit,extname,ftstat)
	else if ( htype .eq. 2 ) then
	  call ftghbn(iunit,maxcl,nrows,tfields,ttype,tform_input,tunit,
     &              extname,varidat,ftstat)
	else 
	  context = 'Extension type not supported'
	  call fcerr(context)
	  goto 999
	endif

C   verify the name, value, and time columns to be present

	call ftgcno(iunit,exact,name,colprm,ftstat)
	if ((colprm .eq. 0).or.(ftstat .ne. 0)) then
	  context = 'Name column not found'
	  call fcerr(context)
	  goto 999
	endif

	call ftgcno(iunit,exact,value,colval,ftstat)
	if ((colval .eq. 0).or.(ftstat .ne. 0)) then
	  context = 'Value column not found'
	  call fcerr(context)
	  goto 999
	endif

	call ftgcno(iunit,exact,time,coltim,ftstat)
	if ((coltim .eq. 0).or.(ftstat .ne. 0)) then
	  context = 'Time column not found'
	  call fcerr(context)
	  goto 999
	endif

C
C      get the width of the data from the  it tform.
C
	if ( htype .eq. 1 ) then
          datacode=0
          width=0
          decimals=0
	  call ftasfm(tform_input(colval), datacode, width, decimals,
     *                status)
	else if ( htype .eq. 2 ) then
          datacode=0
          width=0
          repeat=0
	  call ftbnfm(tform_input(colval), datacode, repeat, width,
     *                status)
c         
c        Do not support the  scalar vector.     
c 
          if((datacode.ne.16) .and.(repeat.gt.1)) then
	      ii = index(tform(colval),' ')
              context = 'The scalar vector is not supported: '
     *        //'tform '//tform_input(colval)(1:i)
              call fcerr(context)
              goto 999
          endif
        endif


C
C       verify  the tform of the value is Byte, short, integer, real, double. 
C       The complex and double complex are not supported.        
C 
          if ( (datacode .ne. 11).and.
     *         (datacode .ne. 16).and. 
     *         (datacode .ne. 21).and. 
     *         (datacode .ne. 41).and. 
     *         (datacode .ne. 42).and. 
     *         (datacode .ne. 82))  then 
	      ii = index(tform(colval),' ')
              context = 
     *        'The data type of value '// 
     *        tform_input(colval)(1:ii)// 
     *        ' is not supported.'//
     *        'only the data types of B, I,'// 
     *        'J, E, F, D, A are valid.'
	      call fcerr(context)
              goto 999
          endif  
        
C
C      exclude the input binary scalar array.    
C 
        if( htype.eq.2) then 
          if ((datacode .ne. 16) .and.  (repeat.gt.1)) then 
              context = 'The feature of repeat counts is not supported'
              call fcerr(context)
              goto 999
          endif
        endif


C
C     only support strings with length less than  30.
C
       if ((datacode .eq. 16). and. (width .gt. 30)) then 
         context = 'The maximum length of string is 30'
         call fcerr(context)
         goto 999
       endif
    
       out_datacode = 0 
       if(outtype.eq.'F'.or.outtype.eq.'E') out_datacode = 42     
       if(outtype.eq.'R'.or.outtype.eq.'D') out_datacode = 82     
       if(outtype.eq.'B') out_datacode = 11     
       if(outtype.eq.'S') out_datacode = 21     
       if(outtype.eq.'I') out_datacode = 41     
       if(outtype.eq.'A') out_datacode = 16  
C
C      warnning if  the input value is demoted.  
C
       if((out_datacode.ne.0).and.(out_datacode.lt.datacode)) then   
         call code2tag(datacode,datatag)
         context = 'Warning: Input value type '
     *              //datatag// 
     *             ' demotes to outtype '//outtype
         call fcerr(context)
       endif

       if(out_datacode.eq.42.and.datacode.eq.41) then 
         call code2tag(datacode,datatag)
         context = 'Warning: Input value type '
     *              //datatag// 
     *             ' is converted to outtype '//outtype
     *              //'. Loss of precision.'
         call fcerr(context)
       endif


C
C      avoid conflicts between the value datatype in  input and 
C      output file
C      
       if(((out_datacode.eq.16).and.(datacode.ne.16)).or. 
     *    ((out_datacode.ne.0).and.(out_datacode.ne.16)
     *    .and.(datacode.eq.16))) then 
         call code2tag(datacode,datatag)
         context = 'input column type '//datatag
     *              //' can not be converted to outtype '//outtype    
         call fcerr(context)
         goto 999
       endif   
C   Get the parameter names from the name column
 	call fignmv(iunit,nrows,name,nnames,narray,ftstat)
	if ( ftstat .ne. 0 ) then
	  errstr = 'Error in extension '
	  write(context,1000) errstr, extnum
	  call fcerr(context)
	  goto 999
	endif

C   if parameter list is not blank then parse into array
	if (param .ne. ' ') then
	  call fcgcls(param,parray,pnames,nflg)
	  call excmpl(pnames,nnames,parray,narray,gdlst)
	  if ( .not. gdlst ) then
	    context = 'Parameter names  not found in file:'
	    call fcerr(context)
            jvalue = pnames/5
            jvalue = jvalue*5
            do 137 i=0,jvalue-5,5
                context = parray(i+1)(1:8)//' '//
     &                    parray(i+2)(1:8)//' '//
     &                    parray(i+3)(1:8)//' '//
     &                    parray(i+4)(1:8)//' '//
     &                    parray(i+5)(1:8)
 137            call fcerr(context) 
            context = parray(jvalue+1)(1:8)
            nnames = 8
            do 139 i=jvalue+2,pnames
                context = context(1:nnames)//' '//parray(i)(1:8)
                nnames = nnames + 9
 139        continue
            call fcerr(context)
	    goto 999
	  endif
	  do 30 i = 1,nnames
	    narray(i) = parray(i)
   30	  continue
	  nnames = pnames
	endif
C-JCI

       if(nnames.gt.512) then
          context = 'Too many parameters in parameter list (max is 512)'
          call fcerr(context)
          ftstat = -10
          goto 999
       endif
    
C-JCI

C   move to the primary array in the input file
	drctn = -1
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
	tfields = nnames + 1
	ttype(1) = time
	if ( htype .eq. 1 ) then
	  tform(1) = 'E23.15'
	  tbcol(1) = 1
	  tbcol(2) = 24
	  rowlen = 23
	else
	  tform(1) = '1D'
	endif
	tunit(1) = 'sec'

C   Copy names of parameters into column keywords 
	do 10 i = 1,nnames
	  ttype(i+1) = narray(i)
	  if ( htype .eq. 1 ) then
		if (outtype .eq. 'F'.or.outtype.eq.'E') then
		   tform(i+1) = 'E15.7'
		   tbcol(i+1) = tbcol(i) + 15
		else if (outtype .eq. 'R'.or.outtype.eq.'D') then
		   tform(i+1) = 'E23.15'
		   tbcol(i+1) = tbcol(i) + 23
		else if (outtype .eq. 'B') then
		   tform(i+1) = 'I4'
	           tbcol(i+1) = tbcol(i) + 4
		else if (outtype .eq. 'S') then
		   tform(i+1) = 'I6'
	           tbcol(i+1) = tbcol(i) + 6
		else if (outtype .eq. 'I') then
		   tform(i+1) = 'I11'
	           tbcol(i+1) = tbcol(i) + 11
		else if  (outtype .eq. 'A') then
		   tform(i+1) = 'A30'
		   tbcol(i+1) = tbcol(i) + 30
                else   
                   tform(i+1) = tform_input(colval)
		   tbcol(i+1) = tbcol(i) + width 
		endif
	        rowlen = tbcol(i+1) - 1
	  else
		if (outtype .eq. 'F'.or.outtype.eq.'E') then
		   tform(i+1) = 'E'
		else if (outtype .eq. 'R'.or.outtype.eq.'D') then
		   tform(i+1) = 'D'
                else if (outtype .eq. 'B') then
	           tform(i+1) = 'B'
                else if (outtype .eq. 'S') then
	           tform(i+1) = 'I'
                else if (outtype .eq. 'I') then
	           tform(i+1) = 'J'
		else if (outtype .eq. 'A') then
		   tform(i+1) = '30A'
                else  
                   tform(i+1) = tform_input(colval)
		   tbcol(i+1) = tbcol(i) + width 
		endif
	  endif
	  tunit(i+1) = ' '
   10	continue

C
C      get the outtype if it was not given
C
        if(outtype.ne.'F'.and.outtype.ne.'R'.and.
     *     outtype.ne.'B'.and.outtype.ne.'S'.and.
     *     outtype.ne.'I'.and.outtype.ne.'A'.and
     *     .outtype.ne.'D'.and.outtype.ne.'E') then
	  if ( htype .eq. 1 ) then
	      call ftasfm(tform_input(colval), datacode, width, 
     *              decimals, status)
	  else if ( htype .eq. 2 ) then
	      call ftbnfm(tform_input(colval), datacode, repeat, 
     *             width, status)
          endif
          call code2tag(datacode,outtype)
        endif

	extname = 'EXP_HKF'

C   write extension header's keywords depending on the table type
        call ftcrtb(ounit,htype,0,tfields,ttype,tform,tunit,extname,
     &         ftstat)
	call xhkexd(iunit,ounit,ftstat)


C   define undefined pixels values 
	if ( tnull .ne. ' ') then
	  read(tnull,1010) inull
	  record = 'NULL value for Parameter column'
	  do 20 i = 1, nnames
	    if ( htype .eq. 1 ) then
	      call ftsnul(ounit,i+1,tnull,ftstat)
	      call ftpkns(ounit,'TNULL',i+1,1,tnull,record,ftstat)
	    endif
	    if ( htype .eq. 2 ) then
	      call fttnul(ounit,i+1,inull,ftstat)
	      call ftpknd(ounit,'TNULL',i+1,1,inull,record,ftstat)
	    endif
   20	  continue
	endif
 1010	format(BN,I9)

C   write out the history record
	  history = 'TASK: HKEXPAND on FILENAME: '//fname
	  call ftphis(ounit,history,ftstat)

C   allocate the memory
          p_time = 0
          call udmget(nrows, 7, p_time, ftstat)
          if(ftstat.ne.0) then 
              call fcerr('Can not allocate memory for time array')  
              goto 999
          endif
          
C   write out the time column in expanded output file
	call fgptim(iunit,nrows,MEMD(p_time),time,dtime,
     *        count,constdiv,ftstat)


C   write out the parameter values in the table
        if( outtype.eq. 'A' )then 
	    call fgppvl_a(iunit,ounit,nrows,count,MEMD(p_time),tnull,
     &              colprm,colval,coltim,nnames,narray,dtime,ftstat)
        else 
	    call fgppvl(iunit,ounit,nrows,count,MEMD(p_time), tnull,
     &              colprm,colval,coltim,nnames,narray,dtime,ftstat)
	endif

C   if error has occurred report it
	if ( ftstat .ne. 0 ) then
	  call fcerrm(ftstat)
	  goto 999
	endif

C       
        call udmfre(p_time,7,ftstat)
        if(ftstat.ne.0) then 
              call fcerr('Can not free  memory for time array')  
              goto 999
        endif

C   Now redefine the size of the extension
        call ftgkyj(ounit,'NAXIS1',naxis1,context,ftstat)
        call ftddef(ounit,count*naxis1,ftstat)

	if ( copyall) then
 300	   call ftmrhd (iunit, 1, htype, ftstat)
	   call ftcrhd (ounit, ftstat)
	   call ftcopy (iunit, ounit, 0, ftstat)
	   if (ftstat .eq. 0) goto 300
	   ftstat = 0
	endif

C  Come here on error
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
C      fignmv
C
C DESCRIPTION:
C      count up the number of distinct parameter names in column
C
C AUTHOR/DATE:
C      J. Kent Blackburn 8/19/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C
C USAGE:
C      call fignmv(iunit,nrows,name,nnames,narray,ftstat) 
C
C ARGUMENTS:
C      iunit   - input FITS file unit number
C      nrows   - number of rows in input file
C      name    - column name containing parameter names
C      nnames  - number of names contained parameter column
C      narray  - string array of parameters found
C      ftstat  - error reporting integer
C
C PRIMARY LOCAL VARIABLES:
C      count  - number of distinct parameter names
C      iarray - mapping of first rows in compact FITS file with distinct 
C               parameter names
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcgcno - get column number associated with column name
C      subroutine fcecho - echo message to terminal
C      subroutind ftgcvs - get string array from FITS table
C
C***************************************************************************

        subroutine fignmv(iunit,nrows,name,nnames,narray,ftstat)

	character*(*) name
	integer iunit,ftstat
	logical exact,anyf
	integer colnum,felem,nelem,count
	integer maxcl,nnames
	parameter ( maxcl = 999 )
	integer i,j,nrows,iarray(maxcl)
	character(70) pname,nullval,narray(maxcl)
	character(70) context
	
	exact = .false.
	ftstat = 0
	count = 0
	nnames = 0
	felem = 1
	nelem = 1
	nullval = ' '
	call ftgcno(iunit,exact,name,colnum,ftstat)
	if ((ftstat .eq. 0).and.(colnum .gt. 0)) then
	  do 10 i = 1, nrows
	    call ftgcvs(iunit,colnum,i,felem,nelem,nullval,
     &	                pname,anyf,ftstat) 
	  if ((.not. anyf) .and. (ftstat .eq. 0)) then
	    do 20 j = 1, count
	      if ( pname .eq. narray(j)) goto 30
   20	    continue
   	    if (count .eq. maxcl) then
	       context = 'Error: Maximum parameters in HK file exceeded'
	       call fcerr(context)
	       ftstat = 500
	       return
	    endif
	    count = count + 1
	    narray(count) = pname
	    iarray(count) = i
   30       continue
	  endif
   10	  continue
	else
	  context = 'Name column not found in table'
	  call fcerr(context)
	endif
	nnames = count
	return
	end


C***************************************************************************
C SUBROUTINE:
C      fgptim
C
C DESCRIPTION:
C      count up the number of distinct time stamps in column and move
C      distinct times to output file
C
C AUTHOR/DATE:
C      J. Kent Blackburn 6/10/92
C
C MODIFICATION HISTORY:
C      JKB 10/26/92 Added delta time interval time stamps
C      EAG  1/22/93 Changed oldtime value to be VAX compatible
C
C NOTES:
C
C
C USAGE:
C      call fgptim(iunit,ounit,nrows,time,dtime,count,constdiv,ftstat) 
C
C ARGUMENTS:
C      iunit  - input file unit number
C      ounit - output file unit number
C      nrows  - number of rows in the input file
C      time   - column name containing time stamps
C      dtime  - time interval between writes to FITS table
C      count  - number of distinct time stamps
C      constdiv - if true, write out only at multiples of dtime
C      ftstat - error reporting integer
C
C PRIMARY LOCAL VARIABLES:
C      tarray  - double array of time stamps found
C      context - error message
C
C CALLED ROUTINES:
C      subroutine fcgcno - get column number associated with column name
C      subroutine fcecho - echo message to terminal
C      subroutind ftgcvd - get double array from FITS table
C
C***************************************************************************


        subroutine fgptim(iunit,nrows,timeval,time,dtime,
     &                                 count,constdiv,ftstat)

	character*(*) time
	integer count,ftstat
	logical exact,anyf,constdiv
	double precision dtime
	integer colnum,felem,nelem
	integer maxcl,loops
	parameter ( maxcl = 999 )
	integer i,j,iunit,nrows
        double precision timeval(nrows)
	double precision ptime,oldtime,nullval
	character(70) context
	
	i = 0
	exact = .false.
	ftstat = 0
	count = 0
	felem = 1
	nelem = 1
	nullval = 0.0D0
ceag	oldtime = -9.9999999999e-99
        oldtime = -1.D-38
	call ftgcno(iunit,exact,time,colnum,ftstat)
	if ((ftstat .eq. 0).and.(colnum .gt. 0)) then
   10	  i = i + 1
	  call ftgcvd(iunit,colnum,i,felem,nelem,nullval,
     &	              ptime,anyf,ftstat) 
	  if ((.not. anyf) .and. (ftstat .eq. 0)) then
	    if (i .eq. 1) then
	        count = count + 1
	        oldtime = ptime
                timeval(count) = ptime 
C	        call ftpcld(ounit,1,count,1,1,ptime,ftstat)
	    else
           if ( ptime .gt. oldtime) then
		if (dtime .gt. 0.0d0) then
		  loops = int((ptime - oldtime) / dtime)
		  do 20 j = 1, loops
		    count = count + 1
		    oldtime = oldtime + dtime
                    timeval(count) = oldtime
C	            call ftpcld(ounit,1,count,1,1,oldtime,ftstat)
   20		  continue
		endif
		
C-JCI CONSTDIV = .true. data is output only at multiples of dtime

               if(.not.constdiv.or.dtime.le.0.0d0) then
		  if ( ptime .gt. oldtime ) then		
	          count = count + 1
	          oldtime = ptime
                  timeval(count) = ptime
C	          call ftpcld(ounit,1,count,1,1,ptime,ftstat)
		  endif
               endif
	     endif
	    endif
	  endif
	  if (i.lt.nrows) goto 10
	else
	  context = 'Time column not found in table'
	  call fcerr(context)
	endif
	end


C***************************************************************************
C SUBROUTINE:
C      fgppvl
C
C DESCRIPTION:
C      write out all parameters in an expanded format
C
C AUTHOR/DATE:
C      J. Kent Blackburn 8/19/92
C
C MODIFICATION HISTORY:
C      JKB 10/27/92 Added support for list of HK parameters names
C      NG   2/10/98 Add dtime as argument to introduce the tolerance.
C
C NOTES:
C
C
C USAGE:
C      call fgppvl(iunit,ounit,nrows,onrows,tnull,
C                  clname,clvalu,cltime,nname,narray,dtime,ftstat)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      ounit    - output file unit number
C      inrows   - number of rows in input file
C      onrows   - number of rows in output file
C      tnull    - string containing null value
C      clname   - column no in input file containing parameters
C      clvalu   - column no in input file containing values
C      cltime   - column no in input file containing times
C      nname    - number of distinct named parameters
C      narray   - array containing parameter names 
C      dtime    - time step. 
C      ftstat   - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      ftstat  - error number
C
C CALLED ROUTINES:
C      subroutine ftpcld - put FITS table column array of doubles
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C***************************************************************************

	subroutine fgppvl(iunit,ounit,inrows,onrows,timeval,tnull,
     &         clname,clvalu,cltime,nname,narray,dtime,ftstat)

	integer iunit,ounit,inrows,onrows,nname,ftstat
        double precision timeval(inrows)
	integer clname,clvalu,cltime
	double precision dtime
	character*(*) tnull
	integer maxcl,i,j,k,jrow
	parameter ( maxcl = 999 )
	character(70) narray(maxcl)
	double precision ktime,xtime
	character(64) kname
	integer colnum, jk
	double precision kvalu, curval(maxcl), inull
c       preval: the parameters at the previous time. 
	double precision preval(maxcl)
        logical preund(maxcl)
	logical curund(maxcl),anyf,xtest,ntest,vtest,ttest,exact
	logical copyit
        double precision delta

    
C   Initialize current values to uninitialized settings
	inull = 0.D0
	exact = .false.
	copyit = .false.
	if (tnull .ne. ' ') then
	  read(tnull,1000) inull
	endif
 1000	format(BN,I9)

	do 10 i = 1,nname
	  curval(i+1) = inull
	  curund(i+1) = .true.
	  preval(i+1) = inull
	  preund(i+1) = .true.
   10	continue

        delta = 1.0e-12
	if(dtime.gt.0.0) delta = dtime/2.0
	j = 1
	k = 1
	jrow = 1 
   20   continue
C   Get the unique time stamp from row j of output file
        xtime = timeval(j)
C	call ftgcfd(ounit,1,j,1,1,xtime,xtest,anyf,ftstat)

   30	continue
C   Get the name, value, and time from row k of input file
	call ftgcfs(iunit,clname,k,1,1,kname,ntest,anyf,ftstat)
	call ftgcfd(iunit,clvalu,k,1,1,kvalu,vtest,anyf,ftstat)
	call ftgcfd(iunit,cltime,k,1,1,ktime,ttest,anyf,ftstat)

C   Find the column number for this parameter name in the output file
C     JKB call ftgcno(ounit,exact,kname,colnum,ftstat)
       do 32 jk = 1, nname
          if (kname .eq. narray(jk)) then
            colnum = jk + 1
            goto 34
          endif
32 	continue
        colnum = 0
34 	continue

C   If this parameter is to be place in the outfile then continue
	if ((colnum .gt. 0) .and. (ftstat .eq. 0)) then

C   Since parameter value has changed, update current values
C   Repeat loop thru input file until current time stamp changes 
	if ( (ktime .le. xtime+delta)) then
	  curval(colnum) = kvalu
	  curund(colnum) = vtest
	  if(k .lt. inrows)  then 
              k = k + 1
              goto 30
          endif
	endif

C   Otherwise this parameter is not part of output, get next parameter
	else
	  ftstat = 0
	  if(k .lt. inrows)  then 
              k = k + 1
              goto 30
          endif
	endif

C   Compared to the parameters if dtime <=-1.0
	if(dtime .le. -1.0) then  
	    do 35 i = 1, nname
                 if((tnull.ne.' ').and.curund(i+1).and.preund(i+1))
     *                goto  35
                 if((tnull.ne.' ').and.(.not.curund(i+1))
     *                .and.preund(i+1)) then 
                         copyit=.true.
                         goto 37
                 endif
                 if((tnull.ne.' ').and.curund(i+1)
     *                .and.(.not.preund(i+1))) then 
                         copyit=.true.
                         goto 37
                 endif
                 if(abs(curval(i+1)-preval(i+1)).gt.
     *                abs(preval(i+1))*10e-6 ) then
                         copyit=.true.
                         goto 37
                 endif
35          continue
37          if(.not.copyit)goto  42
            copyit = .false.
        endif

C   Write parameter values to output file
	do 40 i = 1, nname
   	  call ftpcld(ounit,i+1,jrow,1,1,curval(i+1),ftstat)
   	  if ( curund(i+1) .and. (tnull .ne. ' ') ) then
	    call ftpclu(ounit,i+1,jrow,1,1,ftstat)
	  endif
          if(dtime.le.-1.0) then 
              preval(i+1) = curval(i+1)
              preund(i+1) = curund(i+1)
          endif
	  if(dtime.le.-1.0)call ftpcld(ounit,1,jrow,1,1,xtime,ftstat)
   40	continue
	if ( jrow .le. onrows ) jrow = jrow + 1
   42   continue
C   Repeat loop thru output file until we have reached last time stamp 
	if ( j .lt. onrows ) then
	  j = j + 1
	  goto 20
	endif
        onrows = jrow - 1
	return
	end

C***************************************************************************
C SUBROUTINE:
C      fgppvl_a
C
C DESCRIPTION:
C      write out all string parameters in an expanded format
C
C AUTHOR/DATE:
C      Ning Gan 2/10/98
C
C MODIFICATION HISTORY:
C      JKB 10/27/92 Added support for list of HK parameters names
C
C NOTES:
C      Base on JKB's fgppvl
C
C USAGE:
C      call fgppvl_a(iunit,ounit,nrows,onrows,tnull,
C                  clname,clvalu,cltime,nname,narray,dtime,ftstat)
C
C ARGUMENTS:
C      iunit    - input file unit number
C      ounit    - output file unit number
C      inrows   - number of rows in input file
C      onrows   - number of rows in output file
C      tnull    - string containing null value
C      clname   - column no in input file containing parameters
C      clvalu   - column no in input file containing values
C      cltime   - column no in input file containing times
C      nname    - number of distinct named parameters
C      narray   - array containing parameter names 
C      dtime    - time step. 
C      ftstat   - FITSIO error number
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      ftstat  - error number
C
C CALLED ROUTINES:
C      subroutine ftpcld - put FITS table column array of doubles
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C***************************************************************************

	subroutine fgppvl_a(iunit,ounit,inrows,onrows,timeval,tnull,
     &              clname,clvalu,cltime,nname,narray,dtime,ftstat)

	integer iunit,ounit,inrows,onrows,nname,ftstat
        double precision timeval(inrows)
	integer clname,clvalu,cltime
	double precision dtime
	character*(*) tnull
	integer maxcl,i,j,k,jrow
	parameter ( maxcl = 999 )
	character(70) narray(maxcl)
	double precision ktime,xtime
	character(64) kname
	integer colnum, jk
	character(30) kvalu, curval(maxcl)
c       preval: the parameters at the previous time.
        character(30) preval(maxcl)
        logical preund(maxcl)
	character(30) inull
	character(1) anull
	logical curund(maxcl),anyf,xtest,ntest,vtest,ttest,exact
        logical copyit
        double precision delta
C   Initialize the null  values to uninitialized settings
	anull = char(32)
	write(inull,1000)(anull,i=1,30)
1000    format(30a)
	exact = .false.
        copyit = .false.
C       if (tnull .ne. ' ') then
C         read(tnull,1000) inull
C       endif
C1000	format(BN,I9)

	do 10 i = 1,nname
	  curval(i+1) = inull
	  curund(i+1) = .true.
          preval(i+1) = inull
          preund(i+1) = .true.
   10	continue

        delta = 1.0e-12
	if(dtime.gt.0.0) delta = dtime/2.0
	j = 1
	k = 1
        jrow = 1
   20   continue
C   Get the unique time stamp from row j of output file
        xtime = timeval(j)
c	call ftgcfd(ounit,1,j,1,1,xtime,xtest,anyf,ftstat)

   30	continue
C   Get the name, value, and time from row k of input file
	call ftgcfs(iunit,clname,k,1,1,kname,ntest,anyf,ftstat)
	call ftgcfs(iunit,clvalu,k,1,1,kvalu,vtest,anyf,ftstat)
	call ftgcfd(iunit,cltime,k,1,1,ktime,ttest,anyf,ftstat)

C   Find the column number for this parameter name in the output file
C     JKB call ftgcno(ounit,exact,kname,colnum,ftstat)
       do 32 jk = 1, nname
          if (kname .eq. narray(jk)) then
            colnum = jk + 1
            goto 34
          endif
32 	continue
        colnum = 0
34 	continue

C   If this parameter is to be place in the outfile then continue
	if ((colnum .gt. 0) .and. (ftstat .eq. 0)) then

C   Since parameter value has changed, update current values
C   Repeat loop thru input file until current time stamp changes 
	if ( (ktime .le. xtime+delta)) then
	  curval(colnum) = kvalu
	  curund(colnum) = vtest
	  if(k .lt. inrows)  then 
              k = k + 1
              goto 30
          endif
	endif

C   Otherwise this parameter is not part of output, get next parameter
	else
	  ftstat = 0
	  if(k .lt. inrows)  then 
              k = k + 1
              goto 30
          endif
	endif

C   Compare with the previous time if dtime <=-0.1
        if(dtime .le. -1.0) then
            do 35 i = 1, nname
                 if((tnull.ne.' ').and.curund(i+1).and.preund(i+1))
     *                goto  35
                 if((tnull.ne.' ').and.(.not.curund(i+1))
     *                .and.preund(i+1)) then
                         copyit=.true.
                         goto 37
                 endif
                 if((tnull.ne.' ').and.curund(i+1)
     *                .and.(.not.preund(i+1))) then
                         copyit=.true.
                         goto 37
                 endif
                 if(lgt( curval(i+1),preval(i+1)).or.
     *              llt( curval(i+1),preval(i+1)) ) then
                      copyit=.true.
                      goto 37
                 endif
35          continue
37          if(.not.copyit)goto  42
            copyit = .false.
        endif




C   Write parameter values to output file
	do 40 i = 1, nname
c   	  call ftpcls(ounit,i+1,j,1,1,curval(i+1),ftstat)
   	  call ftpcls(ounit,i+1,jrow,1,1,curval(i+1),ftstat)
   	  if ( curund(i+1) .and. (tnull .ne. ' ') ) then
	    call ftpclu(ounit,i+1,jrow,1,1,ftstat)
	  endif
          if(dtime.le.-1.0) then
              preval(i+1) = curval(i+1)
              preund(i+1) = curund(i+1)
          endif
	  if(dtime.le.-1.0)call ftpcld(ounit,1,jrow,1,1,xtime,ftstat)
   40	continue
        if ( jrow .le. onrows ) jrow = jrow + 1
   42   continue


C   Repeat loop thru output file until we have reached last time stamp 
	if ( j .lt. onrows ) then
	  j = j + 1
	  goto 20
	endif

        onrows = jrow - 1
	return
	end

C***************************************************************************
C SUBROUTINE:
C      xhkexd
C
C DESCRIPTION:
C      This subroutine moves the extra keywords,i.e., the
C      keywords which don't contain: XTENSION,BITPIX,
C      NAXIS*,PCOUNT,GCOUNT,TFIELDS,TTYPE*,TFORM*,TUNIT*,
C      END,TBCOL*,and EXTNAME, from the input file to
C      the output file
C
C AUTHOR/DATE:
C      James Kent Blackburn 8/19/92
C
C MODIFICATION HISTORY:
C
C
C NOTES:
C
C USAGE:
C      call xhkexd(iunit,ounit,fname,status)
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

        subroutine xhkexd(iunit,ounit,status)

        integer     iunit,ounit,status
        integer     i1,i2,i3,i4,i5,i6,i7,i8,i9,ia,ib,ic,id,ie,i10,i11
        integer     i,nkeys,nmore
        logical     l1,l2,l3,l4,l5,l6,l7,l8,l9,la,lb,lc,ld,le,l10,l11
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
          ie = index(record(1:8),'HDUCLAS3')
          i10 = index(record(1:7),'DATASUM')
          i11 = index(record(1:8),'CHECKSUM')
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
          le = ie .eq. 0
          l10 = i10.eq.0
          l11 = i11.eq.0
          copyflg = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. la
     &        .and. lb .and. lc .and. ld .and. le .and. l10
     &        .and. l11
          if ( copyflg ) then
            call ftprec(ounit,record,status)
          end if
   10   continue
        return
        end
C*******************************************************************
C  SUBROUTINE:
C       excmpl
C
C  DESCRIPTION:
C       Compares two arrays to see if the first is a subset
C       of the second.  If yes, it returns the positions of the first in
C       the second, if no, then it overwrites onto the first, the
C       elements which are not in the second.
C
C  AUTHOR/DATE:
C       Jim Ingham 12/15/92
C
C  MODIFICATION HISTORY:
C
C  NOTES:
C
C  USAGE:
C          call excmpl(nlist1,nlist2,list1,list2,subset)
C
C  ARGUMENTS:
C       nlist1  - number of items in list1
C       nlist2  - number of items in list2
C       list1           - first array of strings, overwritten to the
C                          missing elements if susbet = false
C       list2           - second arrau of strings
C       subset  - true if list1 is a subset of list2
C
C  PRIMARY LOCAL VALUES:
C
C  CALLED ROUTINES
C
C*****************************************************************
        subroutine excmpl(nlist1,nlist2,list1,list2,subset)

        integer extmax
        parameter(extmax = 999)
        integer nlist1,nlist2,i,j,itemp
        character*(*) list1(extmax), list2(extmax)
        character(80) temp
        logical subset

        subset = .true.
        itemp = 0

        do 100 i=1,nlist1
                temp = list1(i)
                do 110 j=1,nlist2
                   if(temp.eq.list2(j)) then
                        goto 100
                   endif
 110            continue
                itemp = itemp + 1
                list1(itemp) = temp
                subset = .false.
 100    continue
        if(.not.subset) nlist1 = itemp

 999    return
        end

C***************************************************************************
C
C    subroutine code2tag(datacode, datatag)
C
C DESCRIPTION:
C     Convert the internal datacode to the data type tag.
C
C
C AUTHOR/DATE:
C     Ning Gan 2/25/98
C
C NOTES:
C
C MODIFICATION HISTORY:
C
C***************************************************************************
          subroutine code2tag(datacode, datatag)
          integer datacode
          character(1) datatag
          datatag = ' '
          if(datacode.eq.11) datatag='B'
          if(datacode.eq.16) datatag='A'
          if(datacode.eq.21) datatag='S'
          if(datacode.eq.41) datatag='I'
          if(datacode.eq.42) datatag='E'
          if(datacode.eq.82) datatag='D'
          return
          end


