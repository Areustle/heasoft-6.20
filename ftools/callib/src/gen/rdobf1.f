*+RDOBF1
	subroutine rdobf1(iunit, chatter, 
     &		obfversn,
     &		telescop, instrume, detnam, filter, 
     &		col1, col2,
     &		maxx, maxy,
     &		nix, niy, x, y, xunit, yunit,obsfact, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer nix, niy, maxx, maxy
	real x(maxx), y(maxy),obsfact(maxx,maxy)
	character*(*) obfversn
	character*(*) telescop, instrume, detnam, filter
	character*(*) col1, col2
	character*(*) xunit, yunit
c 
c Description:
c  Reads an OBSFACT extension in one of the formats 
c  conforming to the HDUVERS2='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-022)
c   HDUVERS2 = '1.0.0'
c   HDUCERS2 = '1.1.0'
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c
c Passed parameters
c  iunit         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  OBFVERSN      i   : String denoting OGIP HDUVERS2 family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name   
c  FILTER        i   : String listing instrument filter in use
c  COL1            o : Name of 1st coord axes
c  COL2            o : Name of 2nd coord axes
c  MAXX          i   : Max size of Coord-1 array
c  MAXY          i   : Max size of Coord-2 array
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
c  subroutine FCECHO     : (FTOOLS) Writes to STDOUT
c  subroutine FTGBCL     : (FITSIO) Gets BINTABLE info
c  subroutine FTGCNO     : (FITSIO) Gets column number
c  subroutine FTGCVE	 : (FITSIO) Reads BINTABLE dataset
c  subroutine FTGHBN     : (FITSIO) Gets essential BINTABLE header info
c  subroutine FTGKYx   	 : (FITSIO) Reads keyword value
c  subroutine GT_CSYSNMS : (CALLIB) Gets CALDB coord system col names
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.1; 1994 Jul 29), added gt_csysnms call
c  Ian M George     (1.0.2; 1994 Oct 20), deleted internal arrays
	character(7) version
	parameter (version = '1.0.2')
*- 
c Internals
	character(20) dummy20
	double precision doubdum
	integer status, decimals, nfields
	integer i, j, jj, mat_siz
	integer irows, ncols, ivar, frow, felem
	integer colnum, enull, clenact
        parameter (nfields=3, decimals=6)
	real value
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) csystem
	character(40) comm
	character(80) message
        character(40)  errstr, wrnstr
	logical anyflg
c Initialize
	ierr = 0
	status = 0
        errstr = '** RDOBF1 '//version//' ERROR: '
        wrnstr = '** RDOBF1 '//version//' WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using RDOBF1 '// version
	     call fcecho(message)
        endif



c --- READING KEYWORD VALUES ---

      status = 0
      call ftghbn(iunit,nfields,irows,ncols,ttype,tform,tunits,
     &            comm,ivar,status)
      message = errstr//' reading binary header info'
      call wt_ferrmsg(status,message)
      IF (status.NE.0) THEN
        ierr = 2
        goto 998
      ENDIF
      IF (irows.GT.1) then
        message = wrnstr//' Dataset contains more than one row'
        call fcecho(message)
        message =
     &    ' ... Sorry, this version will ignore all but 1st row'
        call fcecho(message)
      ENDIF

c OBFVERSN ...
      obfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',obfversn,comm,status)
      message = wrnstr//' reading HDUVERS2'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,message)
      ENDIF
      IF (obfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'OBFVERSN',obfversn,comm,status)
        message = wrnstr//' reading OBFVERSN'
        IF (chatter.GE.20) THEN
          call wt_ferrmsg(status,message)
        ENDIF
      ENDIF


c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      message = wrnstr//' reading TELESCOP '
      call wt_ferrmsg(status,message)
      If (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF

cINSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      message = wrnstr//' reading INSTRUME '
      call wt_ferrmsg(status,message)
      If (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      If (status.NE.0) THEN
	status = 0
	filter = 'NONE'
      ENDIF

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      else
        message = wrnstr//' reading DETNAM '
        call wt_ferrmsg(status,message)
	message = ' ... assuming no DETNAM reqd'
	call fcecho(message)
	status = 0
	detnam = 'NONE'
      ENDIF


c Coordinate system in use
      status = 0
      call ftgkys(iunit,'CSYSNAME',csystem,comm,status)
      IF (status.EQ.202) THEN
         csystem = 'XMA_POL'
      elseif(status.NE.0) then
        message = wrnstr//' reading CSYSNAME '
        call wt_ferrmsg(status,message)
        message = ' ... assuming CSYSNAME = XMA_POL'
        call fcecho(message)
	status = 0
	csystem = 'XMA_POL'
      ENDIF

c Sort out the coordinate system & column names
	call gt_csysnms(chatter, csystem, col1, col2, ierr)
	ierr = 0

c --- READ IN THE DATA
c ... The spatial grid
c ...... Coord-1
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,col1,colnum,status)
      		If (status.NE.0) THEN
         	  message = errstr//col1(:clenact(col1))// 
     &			' column not present'
         	  call fcecho(message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, xunit, dummy20,
     &			nix, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nix,
     &		  enull,x,anyflg,status)
      		IF (status.NE.0) THEN
        	  message = errstr//' reading Coord-1 dataset'
        	  call fcecho(message)
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... Coord-2
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,col2,colnum,status)
      		If (status.NE.0) THEN
         	  message = errstr//col2(:clenact(col2))// 
     &			' column not present'
         	  call fcecho(message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, yunit, dummy20,
     &			niy, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nix,
     &		  enull,y,anyflg,status)
      		IF (status.NE.0) THEN
        	  message = errstr//' reading Coord-2 dataset'
        	  call fcecho(message)
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... The beast itself
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,'OBSFACT',colnum,status)
      		If (status.NE.0) THEN
         	  message = errstr//
     &			' OBSFACT column not present'
         	  call fcecho(message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			mat_siz, doubdum,doubdum,enull,dummy20,status)
		if(mat_siz.NE.nix*niy) then
		   message = errstr//
     &			' incompatible OBSFACT dataset size'
		   call fcecho(message)
		   write(message,'(a,i12)')
     &			' ... number of elements in OBSFACT =', mat_siz
		   call fcecho(message)
		   write(dummy20,'(i4,a,i4,a,i10)') 
     &			nix,'x',niy,'=',nix*niy
		   call crmvblk(dummy20)
		   message = ' ... expected '//
     &			dummy20(:clenact(dummy20))// ' elements'
		   call fcecho(message)
         	   ierr = 5
		   goto 998
		elseif(mat_siz.GT.maxx*maxy) then
                   message = errstr//
     &                  ' OBSFACT dataset too large'
                   call fcecho(message)
                   write(message,'(a,i12)')
     &                  ' ... number of elements in OBSFACT =', mat_siz
                   call fcecho(message)
		   write(dummy20,'(i4,a,i4,a,i10)') 
     &			maxx,'x',maxy,'=',maxx*maxy
		   call crmvblk(dummy20)
                   write(message,'(a,i12)')
     &                  ' ... current maximum allowed       ='//
     &			dummy20(:clenact(dummy20))// ' elements'
                   call fcecho(message)
         	   ierr = 5
		   goto 998
		endif
		jj = 0
		do j = 1, niy
		  do i = 1, nix
		    jj = jj + 1
      		    call ftgcve(iunit,colnum,frow,jj,1,
     &		        enull,value,anyflg,status)
      		    IF (status.NE.0) THEN
        	  	message = errstr//' reading OBSFACT dataset'
        	  	call fcecho(message)
        	  	ierr = 1
		  	goto 998
      		    ENDIF
		    obsfact(i,j) = value
		  enddo 
		enddo
c -----

998     if(ierr.NE.0) then
                message = errstr //
     :                  ' FATAL: Extension NOT successfully read'
                call fcecho(message)
        else
          if(chatter.GE.20) then
                message = ' ... OBSFACT extension successfully read'
                call fcecho(message)
          endif
        endif

	return
	end

