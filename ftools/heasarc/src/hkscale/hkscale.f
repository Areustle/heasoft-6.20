C*************************************************************************
C SUBROUTINE:
C	hkscale
C
C FILE: 
C	hkscale.F
C
C DESCRIPTION:
C	Uses the cparfile to scale an AstroD housekeeping file, which has
C     been run through fhkexpd.  
C
C AUTHOR/DATE:
C     Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C     1/19/93 - Include columns that depend on other table values
C	4/7/93 EAG - add copyall parameter
C
C NOTES:
C     The list of converted and passed parameters can also
C     be put in a file, and read by @filename
C
C USAGE:
C     call hkscale
C     
C ARGUMENTS:
C     none
C
C PRIMARY LOCAL VARIABLES:
C     extmax      - The maximun no. of conversion parameters 
C     maxnoval    - the maximun number of text values
C     maxpow      - the maximun order of the power law conversion
C     outfile     - the name of the output file
C     ounit       - unit number for outfile
C     infile      - the name of the input file
C     inextno     - input file extension number
C     iunit       - unit number for infile
C     cparunit    - unit number for the conversion parameter file
C     name        - list of the names of the parameters in cparfile
C     nname       - number of elements in name
C     type        - the type of the elements in name, for now LINEAR,
C                    TEXT and INT are supported
C     val         - the text value of the elements in name
C     power       - conversion parameters for linear elements of name
C     inname      - the list of variables in infile
C     inno        - number of elements in inname
C     convcol     - the list of variables in infile to be converted
C     nconv       - the number of elements in nconv
C     passcol     - the list of variables in infile to be passed
C     npass       - the number of elements in passcol
C     pass2name   - the placement of the passcol elements in name:
C                         name(pass2name(i)) = passcol(i)
C     pass2in     - ditto between passcol and inname
C     conv2name   - ditto between convcol and name
C     conv2in     - guess
C     in2name     - placement of the elements of inname in name
C     nrows       - the number of rows in infile
C	copyall   - copyall other extensions to the output file
C
C CALLED ROUTINES:
C     fsgpar      - gets the list of passed and converted columns
C     fsocpar     - open infile, and cparfile and reads in conv. params.
C     fschk       - check that the infile parameters are in cparfile,
C     fsinit      - opens the outfile, and writes the headers
C     fstrans     - scales transfers the columns
C     ftclos      - FITSIO routine, close the files
C
C***********************************************************************
      subroutine hkscae

      integer extmax,mxnoval,maxpow
      parameter (extmax = 800, mxnoval = 8,maxpow = 2)
      character(8) name(extmax),cname,type(extmax),ctype
      character(8) val(extmax,mxnoval),inname(extmax),valstem,powstem
      real power(extmax,maxpow)
      character(160) infile, outfile
      character(80) context
      character(8) convcol(extmax), passcol(extmax)
      integer pass2name(extmax),pass2in(extmax)
      integer conv2name(extmax),conv2in(extmax),in2name(extmax)
      integer npass,nconv,inno,nrows,nname
      integer fstat,cparunit,iunit,ounit,inextno, hdutype

	logical copyall

	character(40) taskname
	common /task/ taskname

	taskname = 'hkscale2.1b'
      infile = ' '
      outfile = ' '
      fstat = 0
      iunit = 15
      ounit = 16
      cparunit = 17

C  Get the filenames and the lists of passed and converted variables

      call fsgpar(infile,inextno,outfile,passcol,npass,convcol,nconv,
     &               cname,ctype,valstem,powstem,copyall, fstat)
      if(fstat.ne.0)then
            context = 'Error in fsgpar'
            goto 999
      endif

C   Now open infile, and the cparfile, and read 
C   in the appropriate conversions

      call fsocpar(infile,iunit,inextno,cparunit,name,cname,
     &               type,ctype,power,powstem,val,valstem,nname,fstat)
      if(fstat.ne.0)then
            context = 'Error in fsopar'
            goto 999
      endif


C   Check that the infile, the pass & convcol parameters are all 
C   in cparfile, and create all the index vectors.

	call fschk(iunit,passcol,pass2in,pass2name,npass,
     &		convcol,conv2in,conv2name,nconv,
     &		inname,in2name,inno,name,nname,fstat)

	if(fstat.ne.0)then
		context = 'Error in fschk'
		goto 999
	endif


C   Open the outfile, copy the header, and write the bintable header

 	call fsinit(ounit,outfile,iunit,inextno,nrows,name,type,
     &			power,val,in2name,inno,npass,passcol,
     &				nconv,conv2name,copyall,fstat)
	if(fstat.ne.0)then
		context = 'Error in fsinit'
		goto 999
	endif


C   Transfer the data, and close the files

	call fstrans(ounit,iunit,nrows,name,type,power,val,
     &		inname,in2name,inno,npass,pass2name,pass2in,
     &		nconv,conv2name,conv2in,fstat)
	if(fstat.ne.0)then
		context = 'Error in fstrans'
		goto 999
	endif

C now copy all extensions after the extension
	if (copyall) then
300		call ftmrhd (iunit, 1, hdutype, fstat)
		call ftcrhd (ounit, fstat)
		call ftcopy (iunit, ounit, 0, fstat)
		if (fstat .eq. 0) goto 300
		fstat = 0
	endif

C   Finally close all the files

	call ftclos(iunit,fstat)
	if(fstat.ne.0)then
		context = 'Error closing infile'
		goto 999
	endif

	call ftclos(ounit,fstat)
	if(fstat.ne.0)then
		context = 'Error closing outfile'
		goto 999
	endif

 999	return
	end


C*************************************************************************
C SUBROUTINE:
C	FSGPAR
C
C FILE: 
C	hkscale.f
C
C DESCRIPTION:
C	Gets the parfile parameters, and the lists of passed 
C	and converted variables, and parses the latter	
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C	The list of converted and passed parameters can also
C	be put in a file, and read by @filename
C
C USAGE:
C	call fsgpar(infile,inextno,outfile,passcol,npass,convcol,nconv,
C     &		   cname,ctype,valstem,powstem,copyall,fstat)
C
C	
C ARGUMENTS:
C	infile	- The input (HKEXPD'ed) file
C	inextno	- The input file extension number
C	outfile	- The output file
C	passcol	- The array of column names to be passed unscaled
C	npass		- The number of parameters in passcol, 
C	convcol	- The array of column names to be converted
C	nconv		- The number of parameters in convcol, 
C			   negative means convert all the parameters in infile
C	copyall		- whether to copy all other extensions to output
C	fstat		- Error flag
C
C PRIMARY LOCAL VARIABLES:
C	convlist	- List of columns to convert
C	passlist	- list of columns to pass unconverted
C	convflg	- Boolean, true if convlist contains a -
C
C CALLED ROUTINES:
C	ucgl*		- gets input from par file or user
C	fcstln	- calculates the length of string, stripping off 
C				trailing spaces
C	fcgcls	- parses a list into a vector of words
C	fcpars	- parses the combination infile[#] into infile and #
C
C******************************************************************************
	subroutine fsgpar(infile,inextno,outfile,passcol,npass,
     &	   convcol,nconv,cname,ctype,valstem,powstem,copyall,fstat)
     	integer extmax
	parameter (extmax = 800)
	character*(*) infile,outfile
	character(160) filename
	character(80) convlist, passlist,context
	character(8) convcol(extmax), passcol(extmax)
	character(8) powstem,valstem,cname,ctype
	integer npass, nconv,inextno
	integer fstat,fcstln
	logical convflg, passflg, copyall

	context = ' '
	convflg = .false.
	passflg = .false.

C  get the name of the input FITS file from user
	call uclgst('infile',filename,fstat)
	if(fstat.ne.0) then
		context = 'could not get infile parameter'
		call fcerr(context)
	goto 999
	endif

C   Now parse the filename to extract extension number
	call fcpars(filename,infile,inextno,fstat)

C EAG 8/25/93 default to 1st extension
	if (inextno .eq. -99) inextno = 1

	if(fstat.ne.0) then
		context = 'Input file name has incorrect syntax'
		call fcerr(context)
		goto 999
	endif

C  get the name of the output FITS file from user
	call uclgst('outfile',outfile,fstat)
	if(fstat.ne.0) then
		context = 'could not get outfile parameter'
		call fcerr(context)
	goto 999
	endif

C   Now get the column names and stems

	call uclgst('name',cname,fstat)
	if(fstat.ne.0) then
	   	context = 'Could not get name from conversion par. file'
		call fcerr(context)
		go to 999
	endif

	call uclgst('type',ctype,fstat)
	if(fstat.ne.0) then
	   	context = 'Could not get type from conversion par. file'
		call fcerr(context)
		go to 999
	endif
	
	call uclgst('powstem',powstem,fstat)
	if(fstat.ne.0) then
	   	context = 
     &	 'Could not get powstem from conversion par. file'
		call fcerr(context)
		go to 999
	endif

	call uclgst('valstem',valstem,fstat)
	if(fstat.ne.0) then
	   	context = 
     &	 'Could not get valstem from conversion par. file'
		call fcerr(context)
		go to 999
	endif

C  get whether to copyall other extensions

	call uclgsb('copyall', copyall,fstat)
	if(fstat.ne.0) then
		context = 'could not COPYALL parameter'
		call fcerr(context)
		goto 999
	endif

C  get list of converted variables from user and parse
	call uclgst('convlist',convlist,fstat)
	if(fstat.ne.0) then
		context='could not get list of parameters to be converted'
		call fcerr(context)
	goto 999
	endif

C   Don't pass fcgcls a zero length string

	if(fcstln(convlist).ne.0) then
	  call fcgcls(convlist, convcol, nconv, convflg)
	  if(convflg) then
		nconv = -1
		goto 999
	  endif
	else
	   context = 'Convlist is empty'
	   call fcerr(context)
	   fstat = -10
	   goto 999
	endif	

C  get list of passed variables from user

	call uclgst('passlist',passlist,fstat)
	if(fstat.ne.0) then
		context = 'could not get list of parameters to be passed'
		call fcerr(context)
		goto 999
	endif
	if(fcstln(passlist).ne.0) then
	   call fcgcls(passlist, passcol, npass, passflg)
	   if(passflg) then
		npass = -1
	   endif
	else
	   npass = 0
	endif



 999	return
	end		  

C*************************************************************************
C SUBROUTINE:
C	FSOCPAR
C
C FILE: 
C	hkscale.f
C
C DESCRIPTION:
C	Opens infile, and uses INSTRUME to
C	move to the correct conversion parameter file and extension
C	
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C	There is a call to UCLGST here, only one outside of fsgpar.
C	
C USAGE:
C
C	call fsocpar(infile,iunit,inextno,cparunit,name,cname,
C     &		   type,ctype,power,powstem,val,valstem,nname,fstat)
C	
C ARGUMENTS:
C	infile	- Input data file
C	iunit		- unit number for the input file
C	inextno	- The input extension number
C	cparunit	- unit number for the conversion parameter file
C	name		- array of names of conversion parameters
C	cname		- name of the column for name in cparfile
C	type		- array of types of same
C	ctype		- name of the column for type in cparfile
C	power(*,i)	- ith coefficient in the power series conversion
C	powstem	- stem for the name of the power columns
C	val(*,i)	- array of translation for input = i-1
C	valstem	- name for the stem of the val columns
C	end		- index of the last parameter in the arrays
C	fstat		- error flag	
C	
C
C PRIMARY LOCAL VARIABLES:
C	instname 	- The name of the instrument in infile
C	cpardir	- the path for the conversion parameter files
C	cparname	- The name of the specific HK file
C	cparfile	- name of the cparfile
C	ghkextno	- extension no. of the Common HK extension
C	insextno	- extension no. for the infile instrument in cparfile
C
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	fcecho	- echoes to the screen
C	fcstln	- calculates the length of string, stripping off 
C				trailing spaces
C	uclg*		- gets parameters from the user, or the par file
C	fsrdcpar	- reads in the input from one extension of cparfile
C******************************************************************************
	subroutine fsocpar(infile,iunit,inextno,cparunit,name,cname,
     &		   type,ctype,power,powstem,val,valstem,nname,fstat)

	integer extmax,maxpow,mxnoval
	parameter (extmax = 800, mxnoval = 8,maxpow = 2)
	character*(*) infile
	character(80) context,stemp
	character(8) instname,depname,dtype,stemp2
	character(8) name(extmax),type(extmax),val(extmax,mxnoval)
	character(8) cname, ctype,powstem,valstem
	character(160) filename
	character(80) cparname,cpardir
	real power(extmax,maxpow),etemp1,etemp2
	integer ghkextno,insextno,rwmode,block,fcstln,move,inextno
	integer fstat,cparunit,iunit,start,length,nname,hdutype,i,j,k
	integer numdval,itemp1,itemp2
	logical kwflag

	cparname= ' '
	cpardir = ' '
	context = ' '
	fstat = 0
	block = 0
	rwmode = 0
	j = 0

C  Now open the input file

	call ftopen(iunit,infile,rwmode,block,fstat)
	if(fstat.ne.0) then
		context = 'Unable to open infile'
		call fcerr(context)
		goto 999
	endif

C  Get the instrument name

	call ftgkys(iunit,'INSTRUME',instname,context,fstat)
	if (fstat.ne.0) then
		context = 'parameter INSTRUME not present in infile'
		call fcerr(context)
	endif
	
C   Now move to the data extension of infile

	move = inextno+1
	call ftmahd(iunit,move,hdutype,fstat)
	if(hdutype.ne.2) then
	   	context = 'Only BINTABLE supported for infile'
		call fcerr(context)
		goto 999
	endif
	if(fstat.ne.0) then
		context = 'Error moving to binary extension in infile'
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif


	
C  Now locate the dir for the conversion parameter file 

	call uclgst('cpardir',cpardir,fstat)
	if(fstat.ne.0) then
	   	context = 'Could not get dir. for conversion par. file'
		call fcerr(context)
		go to 999
	endif

	length = fcstln(cpardir)

C   get the appropriate extension numbers for infile

	call uclgst('ghk',filename,fstat)
	if(fstat.ne.0) then
		context = 'no extension filename for GHK data'
		call fcerr(context)
		goto 999
	endif
	
	call fcpars(filename,cparname,ghkextno,fstat)
	if (fstat .ne.0) then
		context = 'incorrect syntax for ghkextno name'
		goto 999
	endif

C EAG 8/25/93 default to 1st extension
	if (ghkextno .eq. -99) ghkextno = 1

C   Now open the ghk cparfile:

	stemp = cparname
	cparname = cpardir(1:length)//cparname

	call ftopen(cparunit,cparname,rwmode,block,fstat)
	if(fstat.ne.0) then
		context = 'Unable to open cparfile'
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif	

C  Now get the data from the CHK extension of the cparfile
C

	start = 1
	call fsrdcpar(cparunit,ghkextno,name,cname,type,ctype,
     &			power,powstem,val,valstem,start,nname,fstat)
      if(fstat.ne.0) then
        context = 'Error in fsrdcpar'
        call fcerr(context)
        goto 999
      endif
     
	if(fstat.ne.0) goto 999
	
C   Then  get the instrument data
	
	call uclgst(instname,filename,fstat)
	if (fstat.ne.0) then
		context = 'no extension for '// instname //' HK data'
		call fcerr(context)
		goto 999
	endif
	
	call fcpars(filename,cparname,insextno,fstat)
	if (fstat .ne.0) then
		context = 'incorrect syntax for inst name'
		goto 999
	endif

C EAG 8/25/93 default to 1st extension
	if (insextno .eq. -99) insextno = 1
	
C   Now open the inst. cparfile (if different):

	if(stemp.ne.cparname) then
		call ftclos(cparunit,fstat)
		
		cparname = cpardir(1:length)//cparname

		call ftopen(cparunit,cparname,rwmode,block,fstat)
		if(fstat.ne.0) then
		   context = 'Unable to open cparfile'
		   call fcerr(context)
		   call fcerrm(fstat)
		   goto 999
		endif	
	endif
	start = nname + 1
	call fsrdcpar(cparunit,insextno,name,cname,type,ctype,
     &			power,powstem,val,valstem,start,nname,fstat)
      if(fstat.ne.0) then
         context = 'Error in fsrdcpar'
         call fcerr(context)
         goto 999
      endif
     
	call ftclos(cparunit,fstat)

C   Now deal with the keyword dependent variables (if any are present)

	do 300 i=1, nname
	   if(type(i).eq.'DEPENDKW') then
	      kwflag = .true.
		depname = val(i,1)
		call ftc2ii(val(i,2),numdval,fstat)
		if(fstat.ne.0)then
		   context = 'Error parsing no. of rows for var. '//name(i)
		   call fcerr(context)
		   goto 999
		endif
 				
		call ftgkey(iunit,depname,stemp,context,fstat)
		if(fstat.ne.0) then
		   context = 'Cannot get keyword '//depname(1:8)
		   call fcerr(context)
		   fstat = 0
		   kwflag = .false.
		   
C   KWFLAG false means keyword is not present, or values don't match
C	In this case we will write a dummy row

		   goto 325
		endif
				
		call ftdtyp(stemp,dtype,fstat)
		if(fstat.ne.0) then
		   context = 'Error in type for keyword'//depname(1:8)
		   call fcerr(context)
		   goto 999
		endif
		
		if(dtype.eq.'C') then
		   do 310 j=1,numdval
		      if(index(stemp,name(i+j)).ne.0) goto 325
 310		   continue
 		   goto 323
 		else if(dtype.eq.'L') then
 		   do 312 j=1,numdval
 		     	if(index(stemp,name(i+j)).ne.0) goto 325
 312		   continue
 		   goto 323
 		else if(dtype.eq.'I') then
 		   call ftc2ii(stemp,itemp1,fstat)
 		   do 314 j=1,numdval
 		   	call ftc2ii(name(i+j),itemp2,fstat)
 		   	if(itemp1.eq.itemp2) goto 325
 314		   continue
		   goto 323
		else if (dtype.eq.'F') then 		   	
 		   call ftc2rr(stemp,etemp1,fstat)
 		   do 316 j=1,numdval
 		   	call ftc2rr(name(i+j),etemp2,fstat)
 		   	if(etemp1.eq.etemp2) goto 325
 316		   continue
		   goto 323
 		endif	   	 	  

C do this if there is an error
 		
 323		length = fcstln(stemp)
 		context = depname(1:8) // ' keyword value: ' //
     &                    stemp(1:length) //
     &                    ' not in list of values for var. ' //
     &                    name(i)
 		call fcecho(context)
		kwflag = .false.

C   Don't die if a dependent keyword is not found, 
C   just pass the variable unscaled
C
C	Put the correct row in the place of the first row

 325		if(kwflag) then
 		  type(i) = type(i+j)
 		  do 330 k=1, mxnoval
 330			val(i,k) = val(i+j,k)		
		  do 335 k=1,maxpow
 335			power(i,k) = power(i+j,k) 
		else
		  type(i) = type(i+1)
 		  do 336 k=0, mxnoval-1
 		      write(stemp2,550) k
 550 		      format(a8)
 336			val(i,k) = stemp2		
 		  power(i,1) = 0.0
 		  do 338 k=2,maxpow
 338			power(i,k) = 1.0
 		endif		
C	Now shift up
		do 340 j=i+1,nname
		   name(j) = name(j+numdval)
		   type(j) = type(j+numdval)
		   do 342 k=1,mxnoval
 342		   	val(j,k) = val(j+numdval,k)
		   do 345 k=1,maxpow
 345		   	power(j,k) = power(j+numdval,k) 
 340		continue
 		nname = nname - numdval
 	   endif
 300 	continue   
 999	return
	end

C*************************************************************************
C SUBROUTINE:
C	FSRDCPAR
C
C FILE: 
C	hkscale.f
C
C DESCRIPTION:
C	Reads in the conversion parameters from the cparfile 
C	extension EXTNO.  Puts them in the cpar vectors starting 
C	at START.
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C	This routine assumes all the extensions in cparfile have the same 
C	form.  There is a column of parameter names, one of par. type, 
C	and then a slope and offset column for LINEAR types, and MXNOVAR *8
C	columns of character strings for the TEXT type.  The column names 
C	are read in from the par file however. 
C	
C USAGE:
C
C	       call fsrdcpar(cparunit,extno,name,cname,type,ctype,
C     &		power,powstem,val,valstem,start,end,fstat)
C	
C ARGUMENTS:
C	cparunit	- unit number for the conversion parameter file
C	extno		- The extension from which to read the data
C	name		- array of names of conversion parameters
C	cname		- The column name for name in cparfile
C	type		- array of types of same
C	ctype		- The column name for type in cparfile
C	power(i,1)	- array of offset values
C	power(i,2)	- array of slope values
C	powstem	- stem for the name of the power columns in cparfile
C	val(*,i)	- array of translation for input = i-1
C	valstem	- stem for the name of the val columns in cparfile
C	start		- position to start reading the data into the arrays
C	end		- index of the last element read into the arrays
C
C PRIMARY LOCAL VARIABLES:
C	c* 		- name of the * column in cparfile, except for:
C	cnull1,2	- null value for the name and val columns respectively
C	*cno		- column number of the * column in cparfile
C	valno		- number of text values in extension extno of cparfile
C
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	fcecho	- echoes to the screen
C	fcerrm	- echoes error messages to the screen
C	uclg*		- gets input from par files or the user
C	
C******************************************************************************
	subroutine fsrdcpar(cparunit,extno,name,cname,type,ctype,
     &		power,powstem,val,valstem,start,end,fstat)

	integer extmax,mxnoval,maxpow
	parameter (extmax = 800, mxnoval = 8, maxpow = 2)
	character(80) context
	character(8) cname, ctype, cpower(maxpow),powstem,valstem
	character(8) cval(mxnoval),cnull1,cnull2,stemp(extmax)
	character(8) name(extmax),type(extmax),val(extmax,mxnoval)
	real  power(extmax,maxpow),nullval,etemp(extmax)
	integer start,end
	integer namcno, typcno,powcno(maxpow),valcno(mxnoval)
	integer extno,felem,nelem,frow,valno,powno
	integer fstat,cparunit,hdutype,i,j
	logical exact,nullflg

	felem = 1
	frow = 1
	exact = .false.

C   Move to the HK extension

	extno = extno + 1
	call ftmahd(cparunit,extno,hdutype,fstat)
	if(fstat.ne.0) then
		write(context,50) extno
 50		format('Error moving to header unit, no. ',i2)
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif
	if(hdutype.ne.2) then
		context = 'Only BINTABLES supported'
		call fcerr(context)
		goto 999
	endif

C   Get the maximum number of text values in this extension
	call ftgkyj(cparunit,'valno',valno,context,fstat)
	if (fstat.ne.0)then
		write(context,56) extno
 56		format('VALNO keyword not present in header unit, no: ',i2)
 		call fcerr(context)
 		goto 999
 	endif
 	
C   Get the maximum order for the power series in this extension
	call ftgkyj(cparunit,'powno',powno,context,fstat)
	if (fstat.ne.0)then
		write(context,58) extno
 58		format('POWNO keyword not present in header unit, no: ',i2)
 		call fcerr(context)
 		goto 999
 	endif
C   Now read in the HK conversion parameters
C	Get the number of rows, and the null values

	call ftgkyj(cparunit,'NAXIS2',nelem,context,fstat)
	cnull1 = ' '
	cnull2 = 'NA'
	nullflg = .false.
	end = start + nelem - 1

C
C   Now get the columns
C	First the name column
C
	call ftgcno(cparunit,exact,cname,namcno,fstat)
	if(fstat.ne.0) then
	   	context = 'Column'//cname//' not in cparfile'
		call fcerr(context)
		go to 999
	endif
	call ftgcvs(cparunit,namcno,frow,felem,nelem,cnull1,
     &			stemp,nullflg,fstat)
	if(nullflg) then
		write(context,55) cname,extno
		call fcerr(context)
		goto 999
	endif
	do 110 i= 1,nelem
		name(i+start - 1) = stemp(i)
 110	continue
C 
C   Then the type column
C
	call ftgcno(cparunit,exact,ctype,typcno,fstat)
	if(fstat.ne.0) then
	   	context = 'Column'//ctype//' not in cparfile'
		call fcerr(context)
		go to 999
	endif
	call ftgcvs(cparunit,typcno,frow,felem,nelem,cnull1,
     &			stemp,nullflg,fstat)
	if(nullflg) then
		write(context,55) ctype,extno
		call fcerr(context)
		goto 999
	endif
 	do 111 i=1,nelem
		type(i + start - 1) = stemp(i)
 111	continue
C
C	Now the power columns
C
      do 115 j=1,powno
	   call ftkeyn(powstem,j-1,cpower(j),fstat)
 	   call ftgcno(cparunit,exact,cpower(j),powcno(j),fstat)
	   if(fstat.ne.0) then
	   	context = 'Column'//cpower(j)//' not in cparfile'
		call fcerr(context)
		go to 999
	   endif
	   call ftgcve(cparunit,powcno(j),frow,felem,nelem,nullval,
     &				etemp,nullflg,fstat)
	  if(nullflg) then
		write(context,55) cpower(j),extno
		call fcerr(context)
		goto 999
	  endif
	  do 113 i=1,nelem
		power(i+start-1,j)  = etemp(i)
 113	  continue
 115 	continue
C
C	Finally the value column
C
	  do 120 j=1,valno
	   call ftkeyn(valstem,j-1,cval(j),fstat)
 	   call ftgcno(cparunit,exact,cval(j),valcno(j),fstat)
	   if(fstat.ne.0) then
	   	context = 'Column'//cval(j)//' not in cparfile'
		call fcerr(context)
		go to 999
	   endif
	   call ftgcvs(cparunit,valcno(j),frow,felem,nelem,cnull2,
     &				stemp,nullflg,fstat)
	   do 118 i = 1,nelem
		  val(i + start - 1,j) = stemp(i)
 118	   continue
 120	continue

 55	format('Empty ',a8,' in HKEXT ',i2)

 999	return
	end




*************************************************************************
C SUBROUTINE:
C	FSCHK
C
C FILE: 
C	hkscale.f
C
C DESCRIPTION:
C	Checks that the items on the passed and converted lists are 
C	in the infile, and the cparfile, and creates the index vectors
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C
C USAGE:
C
C	call fschk(iunit,passcol,pass2in,pass2name,npass,
C     &			convcols,conv2in,conv2name,nconv,
C     &			inname,in2name,inno,name,end,fstat)
C	
C ARGUMENTS:
C	iunit		- The input (HKEXPD'ed) file
C	passcol	- The list of columns to pass unscaled
C	pass2name	- position of the passcol entries in name
C	pass2in	- position of the passcol entries in inname
C	npass		- The number of parameters in passcol, 
C			  negative means pass all the parameters in infile
C	convcol	- The list of columns to convert
C	conv2name	- Position of convcol entries in name
C	conv2in	- position of the convcol entries in inname
C	nconv		- The number of parameters in convcols, 
C			  negative means convert all the parameters in infile
C	inname	- names of the parameters in the input file
C	in2name	- position of the inname entries in name
C	inno		- number of elements in inname
C	name		- The list of parameter names in cparfile
C	end		- Index of the last element in name array
C	fstat		- An error flag, 0 means successful completion
C
C PRIMARY LOCAL VARIABLES:
C		
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	fcecho	- echoes to the screen
C	fcerrm	- echoes FITSIO error messages to the screen
C	fcstln	- returns length of string, stripping trailing spaces
C	fscmpl	- compares two strings to see if first is subset of
C			  second, and returns index of first in second
C**********************************************************************
	subroutine fschk(iunit,passcol,pass2in,pass2name,npass,
     &			convcol,conv2in,conv2name,nconv,
     &			inname,in2name,inno,name,nname,fstat)
	integer extmax
	parameter (extmax = 800)
	character(80)  context
	character(40) context1,context2
	character*(*) convcol(extmax), passcol(extmax)
	character*(*) inname(extmax),name(extmax)
	character(20) temp(extmax)
	character(8) ttypenn
	integer npass, nconv,inno,fstat,nname,iunit
	integer in2name(extmax),pass2name(extmax),pass2in(extmax)
	integer conv2name(extmax),conv2in(extmax)
	integer i,j,k,pointer,len1,len2,fcstln, ntemp
	logical subset



	len1=0

C   Read the names of the infile parameters

	call ftgkyj(iunit,'TFIELDS',inno,context,fstat)
	if(inno.gt.extmax) then
		context = 'Too many columns in infile'
		call fcerr(context)
		goto 999
	endif

C   Remember that column 1 is the time column

	inno = inno - 1
	do 100 i=1,inno
		call ftkeyn('TTYPE',i+1,ttypenn,fstat)
		call ftgkys(iunit,ttypenn,inname(i),context,fstat)
 100	continue
	
C   Check that the infile parameter names are all in cparfile:

C!  	Work on a temporary vector and don't change inno

	do 103 i=1,inno
103	   temp(i) = inname(i)
      ntemp = inno
C!

	call fscmpl(ntemp,nname,temp,name,in2name,subset)
	if(.not.subset) then
		context1 = 'The following input file col. names'
		context2 = ' are not contained in the cpar file:'
		len1 = fcstln(context1)
		len2 = fcstln(context2)
		context = context1(1:len1)//context2(1:len2)
		call fcecho(context)
		do 150 i=1,ntemp
 150			call fcecho(temp(i))
C
C   Now shift the missing parameters to passcol
C   and remove them from convcol
C
		if(nconv.lt.0) then
		   nconv = 0
		   npass = 0
		   do 107 i=1,inno
		    do 109 j=1,ntemp
		     if(inname(i).eq.temp(j)) then
		           npass = npass + 1
		           passcol(npass) = inname(i)
		           goto 107
		     endif
 109		    continue
		    nconv = nconv + 1
		    convcol(nconv) = inname(i)
 107	         continue
 		else
 		   if(npass.lt.0) then
 		      npass = 0
 		      do 612 i=1,inno
 		         do 615 j=1,nconv
 615 		            if(convcol(j).eq.inname(i)) goto 612
 		         npass = npass + 1
 		         passcol(npass) = inname(i)
 612		      continue
 		   endif   
 		   do 112 i = 1,ntemp
 		      do 115 j=1,nconv
 		         if(convcol(j).eq.temp(i)) then
 		            do 114 k=j,nconv-1
 114 		               convcol(k) = convcol(k+1)
 				nconv = nconv - 1
 				npass = npass + 1
 				passcol(npass) = temp(i)
 				goto 112
 			    endif
 115 		 	continue
 112 	   	   continue
 		endif
C 		fstat = -24
C		goto 999
	endif

C   Check convcol

	if(nconv.gt.0) then

C   First against inname, and set conv2in

	   call fscmpl(nconv,inno,convcol,inname,conv2in,subset)
	   if(.not.subset) then
		context1 = 'The following conv col. names'
		context2 = ' are not contained in infile:'
		len1 = fcstln(context1)
		len2 = fcstln(context2)
		context = context1(1:len1)//context2(1:len2)
		call fcecho(context)
		do 154 i=1,nconv
 154			call fcecho(convcol(i))
 		fstat = -24
		goto 999
	   endif

C   Then set conv2name

	   call fscmpl(nconv,nname,convcol,name,conv2name,subset)
	   if(.not.subset) then
		context1 = 'The following conv col. names'
		context2 = ' are not contained in cparfile:'
		len2 = fcstln(context2)
		context = context1(1:len1)//context2(1:len2)
		call fcecho(context)
		do 156 i=1,nconv
 156			call fcecho(convcol(i))
 		fstat = -24
		goto 999
	   endif
	endif	

C   Set passcol to everything in inname not in convcol, if npass < 0.
	
	if (npass .lt. 0) then
		npass = inno - nconv
		pointer = 1
		do 300 i=1,inno
			do 310 j=1,nconv
			   if(i.eq.conv2in(j)) goto 300
 310			continue 
 			passcol(pointer) = inname(i)
 			pointer = pointer + 1
 300 		continue
	  endif
C   Now check passcol

	if(npass.gt.0) then

C   This call checks that passcol is in both name and inname

	   call fscmpl(npass,inno,passcol,inname,pass2in,subset)
	   if(.not.subset) then
		context1 = 'The following pass col. names'
		context2 = ' are not contained in the input file:'
		len1 = fcstln(context1)
		len2 = fcstln(context2)
		context = context1(1:len1)//context2(1:len2)
		call fcecho(context)
		do 151 i=1,npass
 151			call fcecho(passcol(i))
 		fstat = -24
		goto 999
	   endif

C   This call is to set pass2name to its value

C	   call fscmpl(npass,nname,passcol,name,pass2name,subset)
C	   if(.not.subset) then
C		context1 = 'The following pass col. names'
C		context2 = ' are not contained in the cparfile:'
C		len2 = fcstln(context2)
C		context = context1(1:len1)//context2(1:len2)
C		call fcecho(context)
C		do 152 i=1,npass
C 152			call fcecho(passcol(i))
C 		fstat = -24
C		goto 999
C	   endif
	endif

 999	return
	end  


***********************************************************************
C SUBROUTINE:
C	FSINIT
C FILE: 
C	hkscale.f
C
C DESCRIPTION:
C	Opens the outfile, and sets up the header for the binary table
C	
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C
C USAGE:
C	call fsinit(ounit,outfile,iunit,inextno,nrows,name,type,power,
C     &	val,in2name,inno,npass,pass2name,nconv,conv2name, copyall, fstat)
C
C	
C ARGUMENTS:
C	ounit		- The output unit number
C	outfile		- The output file name
C	iunit		- The input unit number
C	inextno		- The input extension number
C	nrows		- the number of rows in infile
C	name		- The list of parameter names in cparfile
C	type		- The list of types in cparfile
C	power(i,2)	- Slope conversion factors
C	power(i,1)	- offset conversion factors
C	val			- The text values of the parameters from cparfile
C	in2name		- position of the infile columns in name
C	inno		- number of elements in inname
C	passcol		- names of passed columns
C	npass		- The number of parameters in passcol, 
C			        negative means pass all the parameters in infile
C	conv2name	- position of the converted entries name
C	nconv		- The number of parameters in convcols, 
C			    negative means convert all the parameters in infile
C	copyall		- whether to copyall other extensions
C	fstat		- status of operation
C
C PRIMARY LOCAL VARIABLES:
C	ttype		- name of columns in outfile
C	tform		- variable type of columns in outfile
C	tunit		- units for columns in outfile
C	tscal		- slope factor for columns in outfile
C	tzero		- offset for columns in outfile
C
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	fcecho	- echoes to the screen
C	fcstln	- length of a string stripped of trailing spaces
C	fcerrm	- echoes FITSIO error messages to the screen
C	fsxhk		- copies extra header keywords from file to file
C
C**********************************************************************
	subroutine fsinit(ounit,outfile,iunit,inextno,nrows,name,type,
     &    	   power,val,in2name,inno,npass,passcol,nconv,conv2name,
     &             copyall, fstat)

	integer extmax,mxnoval,maxpow
	parameter (extmax = 800, mxnoval = 8, maxpow = 2)
	character*(*) outfile
	character(80) context,extname
	character(8) ttype(extmax),tform(extmax),tunit(extmax),scratch
	character*(*) passcol(extmax)
	real tzero(extmax),tscal(extmax)
	character(8) name(extmax),type(extmax),val(extmax,mxnoval)
	real  power(extmax,maxpow)
	integer conv2name(extmax),in2name(extmax)
	integer felem,morekeys,hdutype,fcstln,decimals,inextno
	integer tfields,length,ounit,iunit,nrows,inno,npass,nconv
	integer fstat,block,i,j,k,ip1,varidat,move,junk,ndval
	logical copyall

	block = 0
	morekeys = 1
	felem = 1
	varidat = 0
	decimals = 6

	do 160 i=1,extmax
		ttype(i)= ' '
		tform(i) = ' '
		tunit(i) = ' '
		tscal(i) = 1.
		tzero(i) = 0.
 160	continue

C   Move back to the primary header of infile

	move = 1
	call ftmahd(iunit,move,hdutype,fstat)

C   Now open the outfile and copy over the primary header

	call ftinit(ounit, outfile(1:32), block, fstat)
	if(fstat.ne.0) then
		context = 'unable to open outfile'
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif

C   Copy the primary extension from the infile to the outfile

	call ftcopy(iunit,ounit,morekeys, fstat)
	if(fstat.ne.0) then
		context = 'unable to copy primary header'
		call fcerr(context)
		goto 999
	endif

C   Add a comment identifying the processing

	context = ' Transformed into scaled form using HKSCALE'
	call ftphis(ounit,context,fstat)

C   if so requested, copyall other extensions
	if ((copyall) .and. (inextno .gt. 1)) then
		do 200 i = 1, inextno-1
			call ftmrhd (iunit, 1, hdutype, fstat)
			call ftcrhd (ounit, fstat)
			call ftcopy (iunit, ounit, 0, fstat)
200		continue
		if (fstat .ne. 0) then
			context = ' Error copying other extensions'
			call fcerr (context)
			goto 999
		endif
	endif


C   Move to the binary extension of infile

	move = inextno + 1
	call ftmahd(iunit,move,hdutype, fstat)
		if(fstat.ne.0) then
			context = 'could not go to binheader'
			call fcerr(context)
			goto 999
		endif

C   Move to the bintable extension of the outfile

	call ftcrhd(ounit,fstat)

C   Now put the header keywords into the extension header
C	First the time column

	ttype(1) = 'time'
	tform(1) = '1D'
	tunit(1) = 'sec'

	call ftgkyj(iunit,'NAXIS2',nrows,context,fstat)

C   	Now the rest of the columns

	ip1 = 1
	if(nconv.lt.0) then
		tfields = inno+1
		do 100 i=1, inno
			ip1 = ip1 + 1
			ttype(ip1) = name(in2name(i))
			if(type(in2name(i)).eq.'LINEAR') then
			   tform(ip1) = '1E'
			   tscal(ip1) = power(in2name(i),2)
			   tzero(ip1) = power(in2name(i),1)
			else if (type(in2name(i)).eq.'TEXT') then
			   length = 0
			   do 110 j=1, mxnoval
				length = max(length,fcstln(val(in2name(i),j)))
 110			   continue
			   write(scratch,55) length
  55			   format(i1,'A')
			   tform(ip1) = scratch
			else if (type(in2name(i)).eq.'INT') then
			   tform(ip1) = '1I'
			else if (type(in2name(i)).eq.'DISCINT') then
			   tform(ip1) = '1I'
			else if (type(in2name(i)).eq.'DISCRE') then
			   tform(ip1) = '1E'
			else if (type(in2name(i)).eq.'POW_SER') then
			   tform(ip1) = '1E'
			else if (type(in2name(i))(1:5).eq.'FUNCT') then
			      if(type(in2name(i))(6:6).ne.'A') then
			         tform(ip1) = '1'//type(in2name(i))(6:6)
			      else
			         length = fcstln(type(in2name(i)))
			         tform(ip1) = 
     &				type(in2name(i))(7:length)//'A'
     				endif
			else if (type(in2name(i)).eq.'DEPENDTV') then
			   if((type(in2name(i)+1)).eq.'TEXT') then
			      length = 0
			      call ftc2ii(val(in2name(i),2),ndval,fstat)
			      do 112 j=1,ndval
			   	  do 113 k=1, mxnoval
				   length = max(length,fcstln(val(in2name(i)+j,k)))
 113			   	  continue
 112				continue
			   	write(scratch,55) length
			      tform(ip1) = scratch
			   else if((type(in2name(i)+1)).eq.'DISCINT') then
			      tform(ip1) = '1I'
			   else if((type(in2name(i)+1)).eq.'DISCRE') then
			      tform(ip1) = '1E'
			   else if((type(in2name(i)+1)).eq.'POW_SER') then
			      tform(ip1) = '1E'
			   else if (type(in2name(i)+1)(1:5).eq.'FUNCT') then
			      if(type(in2name(i)+1)(6:6).ne.'A') then
			         tform(ip1) = '1'//type(in2name(i)+1)(6:6)
			      else
			         length = fcstln(type(in2name(i)+1))
			         tform(ip1) = 
     &				type(in2name(i)+1)(7:length)//'A'
     				endif
			   endif
			else
			   context='type '//type(in2name(i))//
     &			                       ' not supported'
			   call fcerr(context)
			   goto 999
			endif
 100		continue
	else
		tfields = nconv + npass + 1
		do 120 i=1,nconv
			ip1 = ip1 + 1
 			ttype(ip1) = name(conv2name(i))
			if(type(conv2name(i)).eq.'LINEAR') then
			   tform(ip1) = '1E'
			   tscal(ip1) = power(conv2name(i),2)
			   tzero(ip1) = power(conv2name(i),1)
			else if (type(conv2name(i)).eq.'TEXT') then
			   length = 0
			   do 226 j=1, mxnoval
				length = max(length,fcstln(val(conv2name(i),j)))
 226			   continue
			   write(scratch,55) length
			   tform(ip1) = scratch
			else if (type(conv2name(i)).eq.'INT') then
			   tform(ip1) = '1I'
			else if (type(conv2name(i)).eq.'DISCINT') then
			   tform(ip1) = '1I'
			else if (type(conv2name(i)).eq.'DISCRE') then
			   tform(ip1) = '1E'	
			else if (type(conv2name(i)).eq.'POW_SER') then
			   tform(ip1) = '1E'
			else if (type(conv2name(i))(1:5).eq.'FUNCT') then
			      if(type(conv2name(i))(6:6).ne.'A') then
			         tform(ip1) = '1'//type(conv2name(i))(6:6)
			      else
			         length = fcstln(type(conv2name(i)))
			         tform(ip1) = 
     &				type(conv2name(i))(7:length)//'A'
     				endif
			else if (type(conv2name(i)).eq.'DEPENDTV') then
			   if((type(conv2name(i)+1)).eq.'TEXT') then
			      length = 0
			      call ftc2ii(val(conv2name(i),2),ndval,fstat)
			      do 212 j=1,ndval
			   	  do 213 k=1, mxnoval
				   length = max(length,fcstln(val(conv2name(i)+j,k)))
 213			   	  continue
 212				continue
			   	write(scratch,55) length
			      tform(ip1) = scratch
			   else if((type(conv2name(i)+1)).eq.'DISCINT') then
			      tform(ip1) = '1I'
			   else if((type(conv2name(i)+1)).eq.'DISCRE') then
			      tform(ip1) = '1E'
			   else if((type(conv2name(i)+1)).eq.'POW_SER') then
			      tform(ip1) = '1E'
			   else if(type(conv2name(i)+1)(1:5).eq.'FUNCT')then
			      if(type(conv2name(i)+1)(6:6).ne.'A') then
			         tform(ip1) = '1'//type(conv2name(i)+1)(6:6)
			      else
			         length = fcstln(type(conv2name(i)+1))
			         tform(ip1) = 
     &				type(conv2name(i)+1)(7:length)//'A'
     				endif
			   endif
			else
			 context='type '//type(conv2name(i))
     &                                   //' not supported'
			 call fcerr(context)
			 goto 999
			endif
 120		continue
		do 125 i=1,npass
			ip1 = ip1 + 1
 			ttype(ip1) = passcol(i)
			tform(ip1) = '1I'
125		continue
	endif
	varidat = 0
	extname = 'fscaled'

C   Now write the required keywords

	call ftphbn(ounit,nrows,tfields,ttype,tform,tunit,
     &					extname,varidat,fstat)
	if(fstat.ne.0) then
		context = 'error writing outfile binary extension keywords'
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif
	
C   Now write out the tzero and tscale keywords

	if(nconv.lt.0) then
		junk = inno + 1
	else
		junk = nconv + 1
	endif
	context = ' '
	do 300 i=1,junk
	   if(tzero(i).ne.0) then
		 call ftkeyn('tzero',i,scratch,fstat)
	       call ftpkye(ounit,scratch,tzero(i),decimals,context,fstat)
	      endif
	   if(tscal(i).ne.1) then
		 call ftkeyn('tscal',i,scratch,fstat)
	       call ftpkye(ounit,scratch,tscal(i),decimals,context,fstat)
	   endif
 300	continue
C   Now copy over the other keywords

	call fsxhk(iunit,ounit,fstat)
C   Now add a history keyword to indicate the conversion

	context = 'Transformed into scaled form with HKSCALE'    
	call ftphis(ounit,context,fstat)
	if(nconv.le.0.or.npass.eq.0) then
		context = 'All parameters scaled'
		call ftphis(ounit,context,fstat)
	else
		write(context,510) (nconv+1)
 510		format('Columns 2 to ',i3,' converted')
		call ftphis(ounit,context,fstat)
	endif
C   Now format the binary table

	call ftbdef(ounit,tfields,tform,varidat,nrows,fstat)
	if(fstat.ne.0) then
		context = 'error defining outfile binary extension'
		call fcerr(context)
		call fcerrm(fstat)
		goto 999
	endif

 999	return
	end

C**********************************************************************
C SUBROUTINE:
C      fsxhk
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
C      call fsxhk(iunit,ounit,fname,status)
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
C**********************************************************************

 	subroutine fsxhk(iunit,ounit,status)

        integer     iunit,ounit,status
        integer     i1,i2,i3,i4,i5,i6,i7,i8,i9,ia,ib,ic,id
        integer     i,nkeys,nmore
        logical     l1,l2,l3,l4,l5,l6,l7,l8,l9,la,lb,lc,ld
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
          copyflg = l1 .and. l2 .and. l3 .and. l4 .and. l5
     &        .and. l6 .and. l7 .and. l8 .and. l9 .and. la
     &        .and. lb .and. lc .and. ld
          if ( copyflg ) then
            call ftprec(ounit,record,status)
          end if
   10   continue
        return
        end

C*********************************************************************
C SUBROUTINE:
C	FSTRANS
C FILE: 
C	fscale.f
C
C DESCRIPTION:
C	Transfers the actual data from infile to outfile
C	
C
C AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C MODIFICATION HISTORY:
C	1/19/93 - Include columns that depend on other table values
C
C NOTES:
C	
C
C USAGE:
C	call fstrans(ounit,iunit,nrows,name,type,power,val,
C    &		inname,in2name,inno,npass,pass2name,pass2in,
C    &		nconv,conv2name,conv2in,fstat)
C
C	
C ARGUMENTS:
C	ounit		- The output unit number
C	iunit		- The input unit number
C	nrows		- number of rows in infile
C	name		- The list of  names in cparfile
C	type		- The list of types in cparfile
C	power		- Array of the POW_SER and LINEAR conversion factors
C	val		- The text values of the variables in cparfile
C	inname	- Vector of the column names in infile
C	in2name	- position of the infile columns in cparfile
C	inno		- number of elements in inname
C	pass2name	- position of the passed entries in name
C	pass2in	- position of the passed entries in inname
C	npass		- The number of names in passcol, 
C				negative means pass all the variables in infile
C	conv2name	- position of the converted entries in name
C	conv2in	- position of the converted entries in inname
C	nconv		- The number of variables in convcols
C	fstat		- Error flag - 0 means normal completion
C
C PRIMARY LOCAL VARIABLES:
C	pointer	- current column number
C	*values	- temp. array of * type
C	dkw*		- Relates to the dependent keywords
C	kwd*		- Relates to the keywords they depend upon
C	ndkw		- number of dependent columns in the conversion list
C	ndupon	- number of columns depending on a particular column
C
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	fcecho	- echoes to the screen
C	fcerrm	- echoes FITSIO error messages to the screen
C	fstrblk	- Transfers a block of a column from iunit to ounit
C
C	
C***********************************************************************
 	subroutine fstrans(ounit,iunit,nrows,name,type,power,
     &		val,inname,in2name,inno,npass,pass2name,pass2in,
     &		nconv,conv2name,conv2in,fstat)
     
	integer extmax,mxnoval,maxpow,nelem,mxndkw
	parameter (extmax = 800, mxnoval = 8, maxpow = 2)
	parameter (nelem = 100,mxndkw = 2)
	character(8) name(extmax),type(extmax),inname(extmax)
	character(80) context
	character(8) val(extmax,mxnoval),vartype
	character(8) dkwname(mxndkw),kwdname(mxndkw),dkwtype(mxndkw)
	real  power(extmax,maxpow)
	double precision dvalues(nelem),dnull
	integer npass, nconv,inno,fstat,iunit,ounit
	integer nrows,inull,dkwfound,dkw2conv(mxndkw)
	integer ndkw,dkw2in(mxndkw),kwd2in(mxndkw),kwdnval(mxndkw)
	integer dkwcol(mxndkw),dkwtmp(mxndkw),ndupon,junk(nelem),pointer
	integer in2name(extmax),pass2name(extmax),pass2in(extmax)
	integer conv2name(extmax),conv2in(extmax),jvalues(nelem),i,j,k,m
	integer frow,felem,niter,nleft,tempname,tempin,timecol,one
	logical remainder,anyf,dkwflg
	
	dnull = 0
	inull = 0
	one = 1
	felem = 1
	remainder=.false.
	
	niter = nrows/nelem
	nleft = mod(nrows,nelem)
	if(nleft.ne.0) then
		remainder = .true.
	endif

C   First transfer the time column

	frow = 1
	timecol = 1	
	do 105 j=1,niter 
		call ftgcvd(iunit,timecol,frow,felem,nelem,dnull,
     &				dvalues,anyf,fstat)
		if(fstat.ne.0)then
		   context = 'Error reading time column'
		   call fcerr(context)
		   goto 999
		endif
		call ftpcld(ounit,timecol,frow,felem,nelem,
     &				dvalues,fstat)
		if(fstat.ne.0)then
		   context = 'Error writing time column'
		   call fcerr(context)
		   goto 999
		endif
		frow = frow + nelem
 105	continue
	if(remainder) then
		call ftgcvd(iunit,timecol,frow,felem,nleft,dnull,
     &				dvalues,anyf,fstat)
		if(fstat.ne.0)then
		   context = 'Error reading time column'
		   call fcecho(context)
		endif
		call ftpcld(ounit,timecol,frow,felem,nleft,
     &				dvalues,fstat)
	endif

 
C   Now do the case where all of infile is converted

	if(nconv.lt.0) then
		      	
	
C   Now find the dependent keywords:
 
	   ndkw = 0
	   do 410 i=1,inno
	     if(type(in2name(i)).eq.'DEPENDTV') then
		   
C   See if the keyword it depends upon is present, 
C   and if not, pass the variable unscaled

		  do 416 j=1,inno
		    if(inname(j).eq.val(in2name(i),1)) then
		       ndkw = ndkw + 1
		       if(ndkw.gt.mxndkw) then
		          context = 'Too many type DEPENDTV parameters'
		          call fcerr(context)
		          fstat = -10
		          goto 999
		       endif
		       kwd2in(ndkw) = j
		       dkw2in(ndkw) = i
 		       dkwname(ndkw) = inname(i)
		       kwdname(ndkw) = val(in2name(i),1)
		       call ftc2ii(val(in2name(i),2),
     &						kwdnval(ndkw),fstat)
		       goto 410
		    endif
 416		  continue
 		  context = 'Could not find'//kwdname(i)//
     &				'for DEPENDTV '//dkwname(i)
		  call fcecho(context)
		  type(in2name(i)) = 'INT'
		  goto 410
		      
	      endif
 410	   continue
 
C  transfer the int, and linear scaled columns
		do 100 i=2,inno+1
		   tempname = in2name(i-1)
		   vartype = type(tempname)

C   Do the DEPENDTV columns along with the column they depend upon.

		   if(vartype.eq.'DEPENDTV') goto 100

C   Find the columns that depend upon the current column

		   dkwflg = .false.
		   ndupon = 0
		   do 107 j=1,ndkw
		      if((i-1).eq.kwd2in(j)) then
		         dkwflg = .true.
		         ndupon = ndupon + 1
		         dkwcol(ndupon) = dkw2in(j)
		         dkwtmp(ndupon) = in2name(dkwcol(ndupon)) + 1 
		         dkwtype(ndupon) = type(dkwtmp(ndupon))
		      endif
 107		   continue
 
C   Now transfer the columns in blocks of size nelem, 
C	first the current column: 
 
		   frow = 1
		   do 110 j = 1, niter
		      call fstrblk(iunit,ounit,vartype,tempname,i,i,nelem,
     &					frow,val,power,jvalues,fstat)
     			if(fstat.ne.0) then
        			context = 'Error in fstrblk'
        			call fcerr(context)
        			call fcerrm(fstat)
        			goto 999
     			endif

C      Then all those that depend upon it, row by row:

     			if(dkwflg) then
     			  do 113 m=1,ndupon
     			   do 112 k = 1,nelem
 		            call fstrblk(iunit,ounit,dkwtype(m),
     &	             dkwtmp(m)+jvalues(k),dkwcol(m)+1,dkwcol(m)+1,
     &			 one,frow+k-1,val,power,junk,fstat)
 112			   continue
 113			  continue
 			endif    			   
     		      frow = frow + nelem
110		   continue

C   Now do the same for the remaining bit:

		   if (remainder) then
		      call fstrblk(iunit,ounit,vartype,tempname,i,i,nleft,
     &					frow,val,power,jvalues,fstat)
     			if(fstat.ne.0) then
        			context = 'Error in fstrblk'
        			call fcerr(context)
        			call fcerrm(fstat)
        			goto 999
     			endif
     			if(dkwflg) then
     			  do 115 m=1,ndupon
     			   do 116 k = 1,nleft
 		            call fstrblk(iunit,ounit,dkwtype(m),
     &		        dkwtmp(m)+jvalues(k),dkwcol(m)+1,dkwcol(m)+1,
     &			  one,frow+k-1,val,power,junk,fstat)
 116			   continue
 115			  continue
 			endif    			   

		   endif
100		continue
	else

C   Do the case of selective conversion

C   First find the dependent keywords:
 
	   ndkw = 0
	   do 510 i=1,nconv
	     if(type(conv2name(i)).eq.'DEPENDTV') then
		   
C   See if the keyword it depends upon is present, 
C   and if not, pass the variable unscaled

		  do 516 j=1,inno
		    if(inname(j).eq.val(conv2name(i),1)) then
		       ndkw = ndkw + 1
		       if(ndkw.gt.mxndkw) then
		          context = 'Too many type DEPENDTV parameters'
		          call fcerr(context)
		          fstat = -10
		          goto 999
		       endif
		       kwd2in(ndkw) = j
		       dkw2in(ndkw) = conv2in(i)
		       dkw2conv(ndkw) = i
 		       dkwname(ndkw) = name(conv2name(i))
		       kwdname(ndkw) = val(conv2name(i),1)
		       call ftc2ii(val(conv2name(i),2),
     &						kwdnval(ndkw),fstat)
		       goto 510
		    endif
 516		  continue
 		  context = 'Could not find'//kwdname(i)//
     &				'for DEPENDTV '//dkwname(i)
		  call fcecho(context)
		  type(in2name(i)) = 'INT'
		endif
 510	   continue
 
C  Now transfer the scaled columns, except the DEPENDTV ones

		do 200 i=2,nconv+1

		   tempname = conv2name(i-1)
		   vartype = type(tempname)
		   if(vartype.eq.'DEPENDTV') goto 200
		   tempin = conv2in(i-1) + 1
		   frow = 1
		   do 210 j = 1, niter
		      call fstrblk(iunit,ounit,vartype,tempname,tempin,i,
     &					nelem,frow,val,power,jvalues,fstat)
     		      frow = frow + nelem
210		   continue
		   if (remainder) then
		      call fstrblk(iunit,ounit,vartype,tempname,tempin,
     &				   i,nleft,frow,val,power,jvalues,fstat)
		   endif		   		   
 200		continue
		   
C   Now pick up the DEPENDTV keywords:

		dkwfound = 0
		do 230 i=2,inno+1
		   ndupon = 0
		   dkwflg = .false.
		   do 233 j=1,ndkw
		      if((i-1).eq.kwd2in(j)) then
		         dkwflg = .true.
		         dkwfound = dkwfound + 1
		         ndupon = ndupon + 1
		         dkwcol(ndupon) = dkw2in(j)
		         dkwtmp(ndupon) = in2name(dkwcol(ndupon)) + 1 
		         dkwtype(ndupon) = type(dkwtmp(ndupon))
		      endif
 233		   continue

		   if(dkwflg) then
		     frow = 1
		     do 240 j = 1, niter
			  call ftgcvj(iunit,i,frow,felem,nelem,inull,
     &				jvalues,anyf,fstat)
     			  do 243 m=1,ndupon
     			    do 242 k = 1,nelem
 		           call fstrblk(iunit,ounit,dkwtype(m),
     &	            dkwtmp(m)+jvalues(k),dkwcol(m)+1,dkw2conv(m)+1,
     &			one,frow+k-1,val,power,junk,fstat)
 242			    continue
 243			  continue
     		        frow = frow + nelem
 240		     continue
		     if (remainder) then
			  call ftgcvj(iunit,i,frow,felem,nleft,inull,
     &				jvalues,anyf,fstat)
     			  do 245 m=1,ndupon
     			   do 246 k = 1,nleft
 		           call fstrblk(iunit,ounit,dkwtype(m),
     &		      dkwtmp(m)+jvalues(k),dkwcol(m)+1,dkw2conv(m)+1,
     &			one,frow+k-1,val,power,junk,fstat)
 246			   continue
 245			  continue
 			endif    			   
		   endif

C   Break when all are found

		   if(dkwfound.eq.ndkw) goto 238
		   
 230		continue
 
 238		continue
 	
C   Now pass the unconverted ones		
		pointer = nconv +1   
		do 250 i= 1, npass
		   	tempin = pass2in(i) + 1
			pointer = pointer + 1
	 	    	frow = 1	
			do 255 j=1,niter 
			   call ftgcvj(iunit,tempin,frow,felem,nelem,inull,
     &				jvalues,anyf,fstat)
			   call ftpclj(ounit,pointer,frow,felem,nelem,
     &				jvalues,fstat)
			   frow = frow + nelem
 255			continue
			if(remainder) then
			   call ftgcvj(iunit,tempin,frow,felem,nleft,inull,
     &				jvalues,anyf,fstat)
			   call ftpclj(ounit,pointer,frow,felem,nleft,
     &				jvalues,fstat)
			endif
 250		continue
	endif	

 999	return
	end
C**********************************************************************
C SUBROUTINE:
C	FSTRBLK	
C
C FILE: 
C	fscale.f
C
C DESCRIPTION:
C	This routine transfers a block of length NROWS of the column 
C   INCOLNO starting at row FROW in IUNIT, to the same position in the 
C	column OUTCOLNO, scaling it appropriately, using VARTYPE as its 
C 	variable type.
C	  
C
C AUTHOR/DATE:
C	Jim Ingham 1/19/93 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C	
C
C USAGE:
C	call fstrblk(iunit,ounit,vartype,tempname,incolno,outcolno,
C    &				   nrows,frow,val,power,jvalues,fstat)
C	
C ARGUMENTS:
C	iunit		- The input unit number
C	ounit		- The output unit number
C	vartype	- The type of the variable to be transferred
C	tempname	- Its row number in the power and val arrays
C	incolno	- It's column no. in iunit
C	outcolno	- It's column no. in ounit
C	nrows		- No. of rows to be transferred
C	frow		- Starting row to be written
C	val		- Array for lookup variables
C	power		- Array for POW_SER and LINEAR variables
C	jvalues	- Vector of the unscaled values
C	
C	
C
C PRIMARY LOCAL VARIABLES:
C	*values	- A scratch vector for writing out values of *-type	
C	
C	
C CALLED ROUTINES:
C	ft*		- FITSIO routines
C	*fnct*	- User input functions, for functional conversions	
C	
C**********************************************************************
	subroutine fstrblk(iunit,ounit,vartype,tempname,incolno,outcolno,
     &				nrows,frow,val,power,jvalues,fstat)
      integer mxnoval,maxpow,extmax,mxnelem
	parameter (extmax = 800, mxnoval = 8, maxpow = 2, mxnelem = 100)
      character(8) vartype
      character(80) context
      character*(*) val(extmax,mxnoval)
      character(20) svalues(mxnelem),sfnct1,sfnct2
      real etemp(mxnelem),evalues(mxnelem)
	real  power(extmax,maxpow)
	double precision dvalues(mxnelem),dfnct1,dfnct2
      integer iunit,ounit,incolno,outcolno,nrows,frow,fstat,felem,inull
      integer jvalues(nrows),j2values(mxnelem),j,k
      integer tempname,itemp(mxnelem),jfnct1,jfnct2
      logical anyf
          
      felem = 1
      inull = 0
	     
	call ftgcvj(iunit,incolno,frow,felem,nrows,inull,
     &				jvalues,anyf,fstat)
     
	if(vartype.eq.'LINEAR') then		   
		call ftpclj(ounit,outcolno,frow,felem,nrows,
     &				jvalues,fstat)
	else if(vartype.eq.'INT') then
		call ftpcli(ounit,outcolno,frow,felem,nrows,
     &				jvalues,fstat)
	else if(vartype.eq.'POW_SER') then
     	   do 100 j=1,nrows
     		evalues(j) = power(tempname,1)
     		do 110 k=1,maxpow-1
		 evalues(j) = evalues(j) + 
     &           jvalues(j)**k*power(tempname,k+1)
 110    	continue
 100     continue  		   
	   call ftpcle(ounit,outcolno,frow,felem,nrows,
     &				evalues,fstat)
C
C   Transfer the text columns
C
	else if(vartype.eq.'TEXT') then
		do 127 k=1,nrows
			svalues(k) = val(tempname,jvalues(k)+1)
 127		continue
		call ftpcls(ounit,outcolno,frow,felem,nrows,svalues,fstat)
	else if (vartype.eq.'DISCINT') then
		do 132 j=1,mxnoval
			call ftc2ii(val(tempname,j),itemp(j),fstat)
C     			if(fstat.ne.0) then
C     				write(context,133) name(tempname)
C 133    			format('Error converting ',a8,' in fstrans')
C 				goto 999
C 			endif
 132		continue	
		do 137 k=1,nrows
			j2values(k) = itemp(jvalues(k)+1)
 137		continue
		call ftpclj(ounit,outcolno,frow,felem,nrows,
     &				j2values,fstat)
	else if (vartype.eq.'DISCRE') then
		do 142 j=1,mxnoval
			call ftc2rr(val(tempname,j),etemp(j),fstat)
C     			if(fstat.ne.0) then
C     				write(context,143) name(tempname)
C143    			format('Error converting ',a8,' in fstrans')
C				goto 999
C 			endif
 142		continue	
		do 147 k=1,nrows
			 evalues(k) = etemp(jvalues(k)+1)
 147		continue
		call ftpcle(ounit,outcolno,frow,felem,nrows,
     &				evalues,fstat)
	else if (vartype(1:5).eq.'FUNCT') then
	   	if(vartype(6:6).eq.'I'.or.vartype(6:6).eq.'J') then
	   	    if(val(tempname,1).eq.'1') then
	   	    	do 210 k=1,nrows     
 210	   	         j2values(k) = jfnct1(jvalues(k))
	   	    else if(val(tempname,1).eq.'2') then
	   	    	do 212 k=1,nrows     
 212	   	         j2values(k) = jfnct2(jvalues(k))
 		    endif
		    call ftpclj(ounit,outcolno,frow,felem,nrows,
     &				j2values,fstat)
	   	else if(vartype(6:6).eq.'E'.or.vartype(6:6).eq.'D') then
	   	    if(val(tempname,1).eq.'1') then
	   	       do 220 k=1,nrows     
 220	   	          dvalues(k) = dfnct1(jvalues(k))
	   	    else if(val(tempname,1).eq.'2') then
	   	       do 222 k=1,nrows     
 222	   	          dvalues(k) = dfnct2(jvalues(k))
 		    endif
		    call ftpcld(ounit,outcolno,frow,felem,nrows,
     &				dvalues,fstat)
     		else if(vartype(6:6).eq.'A') then
	   	    if(val(tempname,1).eq.'1') then
	   	       do 230 k=1,nrows     
 230	   	          svalues(k) = sfnct1(jvalues(k))
	   	    else if(val(tempname,1).eq.'2') then
	   	       do 232 k=1,nrows     
 232	   	          svalues(k) = sfnct2(jvalues(k))
 		    endif
		    call ftpcls(ounit,outcolno,frow,felem,nrows,
     &				svalues,fstat)
 		else
 		    context = 'Error for function type '//vartype
 		    call fcerr(context)
 		    fstat = -10
 		    goto 999
 		endif
	endif
	
 999	return
 	end
C----------------------------------------------------------------------
C
C	This part of HKSCALE contains the conversion functions.  These 
C	should be altered as the mission demands.
C
C----------------------------------------------------------------------
C**********************************************************************
C FUNCTION:
C	JFNCT1
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function is a dummy for the user to put in a functional 
C	conversion.  Currently it is unused	
C	  
C AUTHOR/DATE:
C	Jim Ingham 1/20/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C USAGE:
C	i = jfnct1(input)	
C	
C ARGUMENTS:
C	input		- The number to be converted, an integer
C
C PRIMARY LOCAL VARIABLES:
C	context	- Error string	
C	
C CALLED ROUTINES:
C	fcecho	- Echoes to the screen	
C	
C*********************************************************************
	integer function jfnct1(input)
	
	integer input
	character(80) context
	
	jfnct1 = 0
	context = 'This function, jfunct1, currently undefined'
	call fcecho(context)
	
	return
	end
	
C******************************************************************
C FUNCTION:
C	JFNCT2
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function is a dummy for the user to put in a functional 
C	conversion.  Currently it is unused	
C	  
C AUTHOR/DATE:
C	Jim Ingham 1/20/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C USAGE:
C	i = jfnct2(input)	
C	
C ARGUMENTS:
C	input		- The number to be converted, an integer
C
C PRIMARY LOCAL VARIABLES:
C	context	- Error string	
C	
C CALLED ROUTINES:
C	fcecho	- Echoes to the screen	
C	
C**********************************************************************
	integer function jfnct2(input)
	
	integer input
	character(80) context
	
	jfnct2 = 0
	context = 'This function, jfnct2, currently undefined'
	call fcecho(context)
	
	return
	end
	 
C********************************************************************
C SUBROUTINE:
C	DFNCT1
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function converts the GHK ssas-sa grey code to the sun angle	
C	  
C
C AUTHOR/DATE:
C	Ken Ebisawa 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	This is the translation for the Astro-D COMMON housekeeping
C	parameter SUNANG when SSASSASB is 0
C USAGE:
C	d = dfnct1(input)
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C CALLED ROUTINES:
C	
C**********************************************************************
	Double precision function dfnct1(input)
	
      integer*2 bin(7), binout(7)
      logical lgin(7), lgout(7)
      integer input
      double precision sunaxs

      call int2bn(input,bin)
      call bn2lgc(bin, lgin)
      call gry2bn(lgin, lgout)
      call lgc2bn(lgout, binout)
      call bn2val(binout, sunaxs)
      dfnct1 = sunaxs
	return
      end

C********************************************************************
C FUNCTION:
C	DFNCT2
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function converts the GHK ssas-sb grey code to the sun angle	
C	  
C
C AUTHOR/DATE:
C	Ken Ebisawa 
C
C MODIFICATION HISTORY:
C
C NOTES:
C	This is the translation for the Astro-D COMMON housekeeping
C	parameter SUNANG when SSASSASB is 1
C	
C USAGE:
C	d = dfnct2(input)
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:
C	
C	
C CALLED ROUTINES:
C	
C*********************************************************************
	Double precision function dfnct2(input)
	
      integer*2 bin(7), binout(7)
      logical lgin(7), lgout(7)
      integer input
      double precision sunaxs

      call int2bn(input,bin)
      call bn2lgc(bin, lgin)
      call gry2bn(lgin, lgout)
      call lgc2bn(lgout, binout)
      call bn2val(binout, sunaxs)
      dfnct2 = sunaxs + 70
	return
      end
C*********************************************************************
C
C	These are subroutines needed for dfnct1 and dfnct2
C
C*********************************************************************
      subroutine int2bn(input,bin)
      integer*2 bin(7)
      integer input,i,temp

      temp = input
      do 100 i=1,7
	if(mod(temp,2).eq.1) then
	   bin(i) = 1
	else
	  bin(i) = 0
	endif
	temp = temp/2
 100  continue
      return
      end
      
      subroutine bn2val(bin, value)
      integer*2 bin(7)
      double precision value
      value = 2**6*bin(1)+2**5*bin(2)+2**4*bin(3)+2**3*bin(4)
     &     +2**2*bin(5)+2**1*bin(6)+2**0*bin(7)
      value = -62.5 + 1.0*(value - 1)
      end


      subroutine bn2lgc(bin, logic)
      integer*2 bin(7)
      logical logic(7)
      integer i
      do 100 i = 1, 7
         if(bin(i).eq.1) then
            logic(i) = .true.
         elseif(bin(i).eq.0) then
            logic(i) = .false.
         endif
 100  continue
      end


      subroutine lgc2bn(logic, bin)
      logical logic(7)
      integer*2 bin(7)
      integer i
      do 100 i = 1, 7
         if(logic(i)) then
            bin(i) = 1
         else
            bin(i) = 0
         endif
 100  continue
      end


      subroutine gry2bn(gray, b)
      logical gray(7), b(7)
      integer i
      b(1) = gray(1)
      do 100 i = 2, 7
         b(i) = b(i-1).neqv.gray(i)
 100  continue
      end

C*********************************************************************
C
C   Back to fscale proper
C
C*********************************************************************
	
C*********************************************************************
C FUNCTION:
C	SFNCT1
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function translates the Astro-D COMMON housekeeping parameter
C	
C	  
C AUTHOR/DATE:
C	Jim Ingham 1/20/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C USAGE:
C	i = sfnct1(input)	
C	
C ARGUMENTS:
C	input		- The number to be converted, an integer
C
C PRIMARY LOCAL VARIABLES:
C	context	- Error string	
C	
C CALLED ROUTINES:
C	fcecho	- Echoes to the screen	
C	
C**********************************************************************
	character(20) function sfnct1(input)
	
	integer input
	
	if(input.eq.0) then
	   sfnct1 = 'NORMAL'
	else if (input.eq.1) then
	   sfnct1 = 'RWA0M'
	else if(input.eq.2) then
	   sfnct1 = 'RWB0M'
	else if (input.eq.3) then
	   sfnct1 = 'RWC0M'
	else if(input.eq.4) then
	   sfnct1 = 'RWD0M'
	else if (input.eq.5) then
	   sfnct1 = 'RWA0M'
	else if(input.ge.6.and.input.le.43) then
	   sfnct1 = 'BC'
	else if (input.eq.44) then
	   sfnct1 = 'MNVTN0'
	else if(input.eq.45) then
	   sfnct1 = 'MNVTN0'
	else if (input.eq.46) then
	   sfnct1 = 'MNVSNAG'
	else if(input.eq.47) then
	   sfnct1 = 'NOTSTTPS'
	else if (input.eq.48) then
	   sfnct1 = 'INITQABS'
	else if(input.eq.49) then
	   sfnct1 = 'NSASRDY'
	endif
	return
	end
	
C*********************************************************************
C FUNCTION:
C	SFNCT2
C
C FILE: 
C	fscale.f	
C
C DESCRIPTION:
C	This function is a dummy for the user to put in a functional 
C	conversion.  Currently it is unused	
C	  
C AUTHOR/DATE:
C	Jim Ingham 1/20/93
C
C MODIFICATION HISTORY:
C
C NOTES:
C	
C USAGE:
C	i = sfnct2(input)	
C	
C ARGUMENTS:
C	input		- The number to be converted, an integer
C
C PRIMARY LOCAL VARIABLES:
C	context	- Error string	
C	
C CALLED ROUTINES:
C	fcecho	- Echoes to the screen	
C	
C*********************************************************************
	character(20) function sfnct2(input)
	
	integer input
	character(80) context
	
	
	context = 'This function, sfnct2, currently undefined'
	call fcecho(context)
	
	return
	end
	

C*******************************************************************
C  SUBROUTINE:
C	fscmpl
C
C  DESCRIPTION:
C	Compares two arrays to see if the first is a subset 
C	of the second.  If yes, it returns the positions of the first in 
C	the second, if no, then it overwrites onto the first, the   
C	elements which are not in the second.	
C
C  AUTHOR/DATE:
C	Jim Ingham 12/15/92
C
C  MODIFICATION HISTORY:
C
C  NOTES:
C
C  USAGE:
C	   call fscmpl(nlist1,nlist2,list1,list2,indx,subset)
C
C  ARGUMENTS:
C	nlist1	- number of items in list1
C	nlist2	- number of items in list2
C	list1		- first array of strings, overwritten to the 
C			   missing elements if susbet = false
C	list2		- second arrau of strings
C 	indx		- list1(I) = list2(indx(i)), overwritten to place
C			   in list1 of missing elements
C	subset	- true if list1 is a subset of list2
C
C  PRIMARY LOCAL VALUES:
C
C  CALLED ROUTINES
C
C*****************************************************************
	subroutine fscmpl(nlist1,nlist2,list1,list2,indx,subset)

	integer extmax
	parameter(extmax = 800)
	integer nlist1,nlist2,i,j,itemp
	integer indx(extmax)
	character*(*) list1(extmax), list2(extmax)
	character(80) temp
	logical subset

	subset = .true.
	itemp = 0

	do 100 i=1,nlist1
		temp = list1(i) 
		do 110 j=1,nlist2
		   if(temp.eq.list2(j)) then
		   	indx(i) = j
			goto 100
		   endif
 110		continue
 		itemp = itemp + 1
 		list1(itemp) = temp
 		indx(i) = 0
		subset = .false.
 100	continue
	if(.not.subset) nlist1 = itemp

 999	return
	end
