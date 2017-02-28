*+WTOBF1
	subroutine wtobf1(ounit, chatter, 
     &		nk_hist, hist, nk_comm, comment, obfversn,
     &		telescop, instrume, detnam, filter, csystem, 
     &		maxx, maxy,
     &		nix, niy, x, y, xunit, yunit,obsfact, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer nix, niy, maxx, maxy
	real x(maxx), y(maxy),obsfact(maxx,maxy)
	character*(*) obfversn
	character*(*) telescop, instrume, detnam, filter
	character*(*) csystem, xunit, yunit
	character*(*) hist(*), comment(*)
c 
c Description:
c  Creates and Writes the OBSFACT extension in one of the formats 
c  conforming to the HDUVERS2='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-022)
c   HDUVERS2 = '1.0.0'
c   HDUCERS2 = '1.1.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format listed.
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  OBFVERSN      i   : String denoting OGIP HDUVERS2 family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name   
c  FILTER        i   : String listing instrument filter in use
c  CSYSTEM       i   : String giving the coordinate system in use
c  NIX		 i   : Number of values for coordinate-1
c  NIY		 i   : Number of values for coordinate-2
c  X		 i   : Array of coordinate-1 values
c  Y             i   : Array of coordinate-2 values
c  XUNIT	 i   : String giving units of coord-1 values
c  YUNIT	 i   : String giving units of coord-2 values
c  OBSFACT       i   : 2-d array of obscuration factors
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine CRMVBLK    : (CALLIB) Removes blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Orihinal
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1993 Nov 25), first draft
c  Ian M George     (1.0.1; 1994 Jul 29), added gt_csysnms call
	character(7) version
	parameter (version = '1.0.1')
*- 

c Internals
	integer status, decimals, nfields
	integer i, j, jj, maxpos, mat_siz
	integer itemp
        parameter (nfields=3, decimals=6, maxpos=4096)
	real values(maxpos)
	character(5) hduvers2
        character(8) keywrd
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) col1, col2
	character(40) keyval
	character(80) message
        character(40)  errstr, wrnstr
        character(40) tcomm(nfields)
c Initialize
	jj = 0
	ierr = 0
	status = 0
        errstr = '** WTOBF1 '//version//' ERROR: '
        wrnstr = '** WTOBF1 '//version//' WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using WTOBF1 '// version
	     call fcecho(message)
        endif

c Check for sillies
	if(obfversn(1:1).NE.'1') then
	   message = wrnstr // ' Format/subroutine mismatch'
	   call fcecho(message)
	   message = 
     &		' ...... This routine writes only the 1.*.* family' //
     &		' of formats'
	   call fcecho(message)
	   message = 
     &		' ...... requested format: '// obfversn
	   call fcecho(message)
	   ierr = 15
	   goto 998
	endif
c Check that we know the format
	if((obfversn.EQ.'1.0.0').OR.(obfversn.EQ.'1.1.0')) then
	   hduvers2 = obfversn
	else
	   hduvers2 = '1.1.0'
	   message = wrnstr // ' Unknown format: '// obfversn
	   call fcecho(message)
	   message = 
     &	      ' ...... Resetting format (HDUVERS2) to '//hduvers2
	   call fcecho(message)
	endif
c Check we dont exceed the array size
	mat_siz = nix*niy
	if(mat_siz.GT.maxpos) then
		message = errstr // 'Max array size exceeded'
		call fcecho(message)
		ierr = 1
		goto 998
	endif


c Sort out the coordinate system & column names
	call gt_csysnms(chatter, csystem, col1, col2, ierr)
	ierr = 0

c Create a new extension
	call FTCRHD(ounit,status)
	message = errstr
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... new extension created'
   	  call fcecho(message)
	endif
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif


 
c Set up the columns n stuff
	ttype(1)   = col1
          write(message,'(i12,a)') nix,'E'
          call crmvlbk(message)
	tform(1)   = message(1:10)
	tunits(1)  = xunit
	tcomm(1) = 'Spatial coordinate-1'

	ttype(2)   = col2
          write(message,'(i12,a)') niy,'E'
          call crmvlbk(message)
	tform(2)   = message(1:10)
	tunits(2)  = yunit
	tcomm(2) = 'Spatial coordinate-2'

	ttype(3)   = 'OBSFACT'
          write(message,'(i12,a)') nix*niy,'E'
          call crmvlbk(message)
	tform(3)   = message(1:10)
	tunits(3)  = ' '
	tcomm(3) = 'Obscuration Factor array'


c Write the required header keywords
	call FTPHBN(ounit,1,nfields,ttype,tform,tunits,
     &		'OBSCURATION FACTOR',0,status)
	message = errstr 
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... written the extension header keywords'
   	  call fcecho(message)
	endif
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif

c - Fix up the Comments in the TTYPE keywords
c
	status = 0
	do i = 1, nfields
	  write(keywrd,'(a5,i2)') 'TTYPE',i
	  call crmvblk(keywrd)
	  call ftmcom(ounit,keywrd,tcomm(i),status)
	  if(status.NE.0) then
	   message = wrnstr // 'Problem altering '// keywrd
	   call wt_ferrmsg(status,message)
	   status = 0
	  endif
	enddo

c The TDIM (matrix ordering) keyword
	keywrd = 'TDIM3'
        write(keyval,'(a,i12,a,i12,a)')
     &    '(',nix,',',niy,')'
        call crmvblk(keyval)
        call FTPKYS(ounit, keywrd,
     &          keyval,
     &          'Ordering of n-d array',
     &          status)
 
c
c --- WRITE THE HDUCLASn & HDUVERSn keywords
c
	call FTPKYS(ounit,'HDUCLASS ',
     &		'OGIP',
     & 		'format conforms to OGIP standard',
     &		status)
	message = wrnstr // ' Problem putting HDUCLASS keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUCLAS1 ',
     &		'RESPONSE',
     & 		'dataset relates to spatial response',
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS1 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUVERS1 ',
     &		'1.0.0',
     & 		'Version of family of formats',
     &		status)
	message = wrnstr // ' Problem putting HDUVERS1 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUCLAS2 ',
     &		'OBSFACTOR',
     & 		'dataset describes obscurration factor',
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUVERS2 ',
     &		hduvers2,
     &          'Version of format (OGIP memo CAL/GEN/92-022)',
     &		status)
	message = wrnstr // ' Problem putting HDUVERS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

c Add the other (passed) OGIP required keywords
 	call FTPKYS(ounit,'CSYSNAME',
     &		csystem,
     &   	'spatial coordinate system in use',
     &		status)
	message = wrnstr // ' Putting CSYSNAME keyword '
	call wt_ferrmsg(status, message)
	status = 0

 	call FTPKYS(ounit,'TELESCOP ',
     &		telescop,
     &   	'mission/satellite name',
     &		status)
	message = wrnstr // ' Putting TELESCOP keyword '
	call wt_ferrmsg(status, message)
	status = 0
	
	call FTPKYS(ounit,'INSTRUME ',
     &		instrume,
     &   	'instrument/detector name',
     &		status)
	message = wrnstr // ' Putting INSTRUME keyword '
	call wt_ferrmsg(status, message)
	status = 0

	if(detnam.NE.' ') then
	call FTPKYS(ounit,'DETNAM ',
     &		detnam,
     &   	'specific detector name in use',
     &		status)
	message = wrnstr // ' Putting DETNAM keyword '
	call wt_ferrmsg(status, message)
	status = 0
	endif

	call FTPKYS(ounit,'FILTER   ',
     &		filter,
     &   	'filter in use',
     &		status)
	message = wrnstr // ' Putting FILTER keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'OBFVERSN ',
     &		'1992a',
     &   	'OGIP code for format in use',
     &		status)
	message = wrnstr // ' Putting OBFVERSN keyword '
	call wt_ferrmsg(status, message)
	status = 0

	if(chatter.GE.20) then
	  message = ' ... written the OGIP required keywords'
   	  call fcecho(message)
	endif


c Add the (passed) history cards, adding one related to this programme
	itemp = 0
	do i = 1, nk_hist
		call FTPHIS(ounit, hist(i), status)
		if(status.NE.0) then
			itemp = status
			status = 0
                        call FTPHIS(ounit,
     &		' - (missing record) fitsio illegal character ?',
     &		 status)
		endif
	enddo
        write(message,'(2a)') 
     &			' OBSFACT extension written by WTOBF1 ',
     &                   version
	call FTPHIS(ounit,message(:70),status)
	message = wrnstr // ' Putting at least one History record'
	call wt_ferrmsg(itemp, message)
	if(chatter.GE.20) then
	  message = ' ... written the history keywords'
   	  call fcecho(message)
	endif
	status = 0

c Add the (passed) comment cards
	itemp = 0
	do i = 1, nk_comm
		call FTPCOM(ounit, comment(i), status)
		if(status.NE.0) then
			itemp = status
			status = 0
                       	call FTPCOM(ounit,
     &          ' - (missing record) fitsio illegal character ?',
     &           status)
 		endif
	enddo
	message = wrnstr // ' Putting at least one Comment record'
	call wt_ferrmsg(itemp, message)
	status = 0
	if(chatter.GE.20) then
	  message = ' ... written the comment header keywords'
   	  call fcecho(message)
	endif

c Define the extension data structure
	call FTBDEF(ounit,nfields,tform,0,1,status)
	message = errstr // ' Defining Data Structure '
	call wt_ferrmsg(status, message)
        if(status.NE.0) then
                ierr = 1
                goto 998
        endif
	if(chatter.GE.20) then
	  message = ' ... defined the extension data structure'
   	  call fcecho(message)
	endif

c Write the data
c 		... coordinate-1
		call FTPCLE(ounit, 1, 1, 1, nix,x,status)

c 		... coordinate-2
		call FTPCLE(ounit, 2, 1, 1, niy,y,status)

c 		... the obsfactor 
		do j = 1, niy
		  do i = 1, nix
			jj = jj + 1
			values(jj) = obsfact(i,j)
		  enddo
		enddo
		call FTPCLE(ounit, 3, 1, 1, mat_siz,values,status)


	if(chatter.GE.20) then
	  message = ' ... written the data'
   	  call fcecho(message)
	endif

c Final check for errors
	message = wrnstr // ' Writing Data '
	call wt_ferrmsg(status, message)

998	if(ierr.NE.0) then
	   message = errstr // ' FATAL: Extension not written'
	   call fcecho(message)
	endif

	return
	end
	

