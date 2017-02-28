*+RDCOL1
	subroutine rdcol1(iunit, chatter, 
     &		crspvers,
     &		telescop, instrume, detnam, filter, 
     &		col1, col2,
     &		maxen, maxx, maxy,
     & 		nen, energ_lo, energ_hi, enunits,
     &		nix, niy, x, y, xunit, yunit,collresp, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer maxx, maxy, maxen
	integer nix, niy, nen
	real energ_lo(maxen), energ_hi(maxen)
	real x(maxx), y(maxy), collresp(maxen,maxx,maxy)
	character*(*) crspvers
	character*(*) telescop, instrume, detnam, filter
	character*(*) col1, col2
	character*(*) xunit, yunit, enunits
c 
c Description:
c  Reads an COLLRESP extension in one of the formats 
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
c  subroutine FTGBCL     : (FITSIO) Gets BINTABLE info
c  subroutine FTGCNO     : (FITSIO) Gets column number
c  subroutine FTGCVE	 : (FITSIO) Reads BINTABLE dataset
c  subroutine FTGHBN     : (FITSIO) Gets essential BINTABLE header info
c  subroutine FTGKYx   	 : (FITSIO) Reads keyword value
c  subroutine GCSYNM     : (CALLIB) Gets CALDB coord system col names
c  subroutine WTERRM     : (CALLIB) Writes CALDB-standard error message
c  subroutine WTINFO     : (CALLIB) Writes CALDB-standard info message
c  subroutine WTFERR     : (CALLIB) Writes CALDB-standard  FITSIO error message
c  subroutine WTWARM     : (CALLIB) Writes CALDB-standard warning message
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1994 Oct 14), original
c  Ian M George     (1.1.0; 1996 Feb 02), fixed Greenbank Conv for ENERG,
	character(7) version
	parameter (version = '1.1.0')
*- 
c Internals
        character(6) subname
        parameter (subname = 'rdcol1')
        character(8) dummy8
	character(20) dummy20
	double precision doubdum
	integer status, decimals, nfields
	integer i, j, k, jj, maxpos, mat_siz
	integer irows, ncols, ivar, frow, felem
	integer colnum, enull, clenact
        parameter (nfields=3, decimals=6, maxpos=40960)
	real values(maxpos)
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) csystem
	character(40) comm
	character(80) message
	logical anyflg, qenerg
c Initialize
	ierr = 0
	status = 0

c Give user info if requested
	message = ' using '//subname//' '//version
        call wtinfo(chatter,20,1,message) 

c --- READING KEYWORD VALUES ---

      status = 0
      call ftghbn(iunit,nfields,irows,ncols,ttype,tform,tunits,
     &            comm,ivar,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version, status,
     &		' reading binary header info')
        ierr = 2
        goto 998
      ENDIF
      IF (irows.GT.1) then
	call wtwarm(subname, version, 1, 1,
     &		' Dataset contains more than one row')
	call wtinfo(chatter,1,1,
     &    ' ... Sorry, this version will ignore all but 1st row')
      ENDIF

c Sort our whether we have energy columns present
	qenerg = .false.
	do i = 1, ncols
	  if(ttype(i).EQ.'ENERG_LO') then
		qenerg = .true.
	  elseif(ttype(i).EQ.'ENERG_HI') then
		qenerg = .true.
	  endif
	enddo

c CRSPVERS ...
      crspvers = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',crspvers,comm,status)
      if(status.ne.0) then
        call wtwarm(subname, version, 20, 1,
     &		'Problem reading HDUVERS2')
      ENDIF
      IF (crspvers.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'CRSPVERS',crspvers,comm,status)
        if(status.ne.0) then
          call wtwarm(subname, version, 20, 1,
     &		'Problem reading CRSPVERS')
        endif
      ENDIF


c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtferr(subname,version, status,
     &		' reading TELESCOP keyword')
      If (status.EQ.202) THEN
        telescop = 'UNKNOWN'
	call wtinfo(chatter,1,1,'Setting TELESCOP = UNKNOWN')
      ENDIF

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtferr(subname,version, status,
     &		' reading INSTRUME keyword')
      If (status.EQ.202) THEN
        instrume = 'UNKNOWN'
	call wtinfo(chatter,1,1,'Setting INSTRUME = UNKNOWN')
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
      call wtferr(subname,version, status,
     &		' reading DETNAM keyword')
      If (status.EQ.202) THEN
        detnam = 'NONE'
	call wtinfo(chatter,20,2,'assuming DETNAM = NONE')
      elseif(status.ne.0) then
        detnam = 'NONE'
	call wtwarm(subname,version, chatter,1,'Setting DETNAM = NONE')
      ENDIF

c Coordinate system in use
      status = 0
      call ftgkys(iunit,'CSYSNAME',csystem,comm,status)
      call wtferr(subname,version, status,
     &		' reading CSYSNAME keyword')
      IF (status.EQ.202) THEN
         csystem = 'XMA_POL'
	 call wtinfo(chatter,20,2,'assuming CSYSNAME = XMA_POL')
      elseif(status.ne.0) then
	 csystem = 'XMA_POL'
	call wtwarm(subname,version, chatter,1,
     &		'Setting CSYSNAME = XMA_POL')
      ENDIF

c Sort out the coordinate system & column names
        call gcsynm(chatter, 0, csystem,
     &          col1, col2, dummy8, dummy8, ierr)
	ierr = 0

c Keep the user happy
  	call wtinfo(chatter,5,1,
     :		' Reading COLLRESP extension ')

c --- READ IN THE DATA
c ... The energy grid
c ...... Lower bound
	    if(qenerg) then
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      		If (status.NE.0) THEN
      		  call wtferr(subname,version, status,
     &		    ' ENERG_LO column not present')
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, enunits, dummy20,
     &			nen, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nen,
     &		  enull,energ_lo,anyflg,status)
      		IF (status.NE.0) THEN
      		  call wtferr(subname,version, status,
     &		    ' reading ENERG_LO dataset')
        	  ierr = 1
		  goto 998
      		ENDIF
	    else
		call ftgkye(iunit,'ENERG_LO', energ_lo(1), dummy20,
     &			status)
		if(status.ne.0) then
		   call wtferr(subname, version, status, 
     &			'Reading ENERG_LO keyword')
		   ierr = 1
		   goto 998
		endif
		nen = 1
		enunits = 'keV'
	    endif

c ...... Upper bound
	    if(qenerg) then
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      		If (status.NE.0) THEN
      		  call wtferr(subname,version, status,
     &		    ' ENERG_HI column not present')
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, enunits, dummy20,
     &			nen, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nen,
     &		  enull,energ_hi,anyflg,status)
      		IF (status.NE.0) THEN
      		  call wtferr(subname,version, status,
     &		    ' reading ENERG_HI dataset')
        	  ierr = 1
		  goto 998
      		ENDIF
	    else
		call ftgkye(iunit,'ENERG_HI', energ_hi(1), dummy20,
     &			status)
		if(status.ne.0) then
		   call wtferr(subname, version, status, 
     &			'Reading ENERG_HI keyword')
		   ierr = 1
		   goto 998
		endif
		enunits = 'keV'
		nen = 1
	    endif
c ... The spatial grid
c ...... Coord-1
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,col1,colnum,status)
      		If (status.NE.0) THEN
         	  message = col1(:clenact(col1))// 
     &			' column not present'
      		  call wtferr(subname,version, status, message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, xunit, dummy20,
     &			nix, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nix,
     &		  enull,x,anyflg,status)
      		IF (status.NE.0) THEN
      		  call wtferr(subname,version, status,
     &		    ' reading Coord-1 dataset')
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... Coord-2
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,col2,colnum,status)
      		If (status.NE.0) THEN
         	  message = col2(:clenact(col2))// 
     &			' column not present'
      		  call wtferr(subname,version, status, message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, yunit, dummy20,
     &			niy, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,nix,
     &		  enull,y,anyflg,status)
      		IF (status.NE.0) THEN
		  call wtferr(subname,version, status,
     &			'Reading Coord-2 dataset')
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... The beast itself
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,'COLLRESP',colnum,status)
      		If (status.NE.0) THEN
		  call wtferr(subname,version, status,
     &			' COLLRESP column not present')
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			mat_siz, doubdum,doubdum,enull,dummy20,status)
		if(status.ne.0) then
		  call wtferr(subname,version, status,
     &			'Getting size of COLLRESP array')
         	  ierr = 4
		  goto 998
      		ENDIF

		if(mat_siz.NE.nen*nix*niy) then
		  call wterrm(subname,version,
     &			'incompatible COLLRESP dataset size')
		   write(message,'(a,i12)')
     &			'number of elements in COLLRESP =', mat_siz
		   call wtinfo(chatter,1,1,message)
		   write(dummy20,'(i3,a,i3,a,i3,a,i8)')
     &			nen,'x',nix,'x',niy,'=',nen*nix*niy
		   call crmvblk(dummy20)
		   message = 'expected '//
     &			dummy20(:clenact(dummy20))// ' elements'
		   call wtinfo(chatter,1,2,message)
         	   ierr = 5
		   goto 998
		elseif(nen.GT.maxen) then
		  call wterrm(subname,version,
     &                  ' COLLRESP dataset too large')
                   write(message,'(a,i12)')
     &                  'number of elements in Energy array =', nen
		   call wtinfo(chatter,1,1,message)
                   write(message,'(a,i12)')
     &             'current maximum allowed            =', maxen
		   call wtinfo(chatter,1,2,message)
         	   ierr = 5
		   goto 998
		elseif((nix.GT.maxx).or.(niy.GT.maxy)) then
		  call wterrm(subname,version,
     &                  ' COLLRESP dataset too large')
                   write(message,'(a,i12)')
     &              'number of elements in Coord-1 array =', nix
		   call wtinfo(chatter,1,1,message)
                   write(message,'(a,i12)')
     &              'current maximum allowed             =', maxx
		   call wtinfo(chatter,1,2,message)
                   write(message,'(a,i12)')
     &              'number of elements in Coord-2 array =', niy
		   call wtinfo(chatter,1,1,message)
                   write(message,'(a,i12)')
     &              'current maximum allowed             =', maxy
		   call wtinfo(chatter,1,2,message)
         	   ierr = 5
		   goto 998
		endif
      		call ftgcve(iunit,colnum,frow,felem,mat_siz,
     &		    enull,values,anyflg,status)
		if(status.ne.0) then
		  call wtferr(subname,version, status,
     &			'Reading COLLRESP array')
         	  ierr = 1
		  goto 998
      		ENDIF
		jj = 0
		do j = 1, niy
		  do i = 1, nix
		   do k = 1, nen
		    jj = jj + 1
		    collresp(k,i,j) = values(jj) 
		   enddo
		  enddo 
		enddo

c -----


998     if(ierr.NE.0) then
	  call wterrm(subname, version,
     :                  ' FATAL: Extension NOT successfully read')
        else
	  call wtinfo(chatter,10,1,
     :		' COLLRESP extension successfully read')
        endif

	return
	end

