*+GCSYGD
	subroutine gcsygd(iunit, chatter, 
     &		csysnam,
     &		cnam1, col1, ni1, unit1, lo1, hi1,
     &		cnam2, col2, ni2, unit2, lo2, hi2,
     &		ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer ni1, ni2, col1, col2
	real lo1, hi1, lo2, hi2
	character*(*) csysnam
	character*(*) cnam1, cnam2
	character*(*) unit1, unit2
c 
c Description:
c  Reads the CSYSNAME keyword from the current extension. If found,
c then derives the names of the columns containing the appropriate 
c spatial coordinate grids, and returns bunch of useful info.
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  CSYSNAM         o : Value of the CSYSNAME keyword (UNKNOWN if not found)
c  CNAM1           o : Column name for coord1 in CSYSNAME system
c  COL1            o : Column number of CNAM1 column (or zero, see below)
c  NI1		   o : Number elements in CNAM1 column
c  UNIT1           o : Unit string of CNAM1 column
c  LO1             o : First value in CNAM1 column
c  HI1             o : Last value in CNAM1 column
c  CNAM2           o : Column name for coord2 in CSYSNAME system
c  COL2            o : Column number of CNAM2 column (or zero, see below)
c  NI2		   o : Number elements in CNAM2 column
c  UNIT2           o : Unit string of CNAM2 column
c  LO2             o : First value in CNAM2 column
c  HI2             o : Last value in CNAM2 column
c  IERR            o : Error flag (0 = OK)
c
c Note, that in the case that the one/both of the spatial coord grids 
c       are SINGLE-VALUED and hence stored as a keyword instead of a 
c       column (ie Greenbank Convention), then COL* will be set to zero,
c       and LO* = HI*.
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to STDOUT
c  subroutine FTGBCL     : (FITSIO) Gets BINTABLE info
c  subroutine FTGCNO     : (FITSIO) Gets column number
c  subroutine FTGCVE	 : (FITSIO) Reads BINTABLE dataset
c  subroutine FTGKYx   	 : (FITSIO) Reads keyword value
c  subroutine GCSYSNM    : (CALLIB) Gets CALDB coord system col names
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1995 May 12), original
	character(7) version
	parameter (version = '1.0.0')
*- 
c Internals
	character(20) dummy20
	double precision doubdum
	integer status, mode, frow, felem
	integer enull
	real value
	character(20) dunit1, dunit2
	character(40) comm
	character(80) message
        character(40)  errstr, wrnstr
	logical anyflg
c Initialize
	ierr = 0
	status = 0
        errstr = '** GCSYGD '//version//' ERROR: '
        wrnstr = '** GCSYGD '//version//' WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using GCSYGD '// version
	     call fcecho(message)
        endif

c Search for the CSYSNAME keyword
	csysnam = 'UNKNOWN'
      	status = 0
      	call ftgkys(iunit,'CSYSNAME',csysnam,comm,ierr)
      	IF(ierr.ne.0) THEN
        	message = errstr//' reading CSYSNAME'
        	call wt_ferrmsg(status,message)
		goto 998
      	ENDIF

c Sort out the coordinate system & column names
	mode = 0
	call gcsynm(chatter, mode, csysnam, cnam1, cnam2, 
     &		dunit1, dunit2, ierr)
      	IF(ierr.ne.0) goto 998

c --- READ IN THE INFO FROM THE COLUMNS
c --- Coord-1 -------------------------------------------------
	status = 0
     	call ftgcno(iunit,.false.,cnam1,col1,ierr)
	if(ierr.EQ.0) then
c ........ Standard configuration, column present
	   call ftgbcl(iunit, col1, dummy20, unit1, dummy20,
     &			ni1, doubdum,doubdum,enull,dummy20,ierr)
           IF(ierr.NE.0)THEN
        	message = errstr//' with FTGBCL for Coord1'
        	call wt_ferrmsg(status,message)
		goto 998
	   endif
           frow = 1
      	   enull = 0
c	   ... Get the first value stored for Coord1
      	   felem = 1
	   call ftgcve(iunit,col1,frow,felem,1,
     &		  enull,lo1,anyflg,status)
c	   ... Get the last value stored for Coord1
      	   felem = ni1
	   call ftgcve(iunit,col1,frow,felem,1,
     &		  enull,hi1,anyflg,status)
           IF(status.NE.0)THEN
        	message = errstr//' getting first/last values Coord1'
        	call wt_ferrmsg(status,message)
		ierr = status
		goto 998
	   endif
	else
c ........ Column NOT present, check for Greenbank Convention
	   status = 0
           call ftgkye(iunit,cnam1,value,comm,status)
	   if(status.NE.0) then
        	message = errstr//' Unable to locate Coord1 grid'
        	call fcecho(message)
		message = ' ... '//cnam1(:8)//
     &			'not found/read as column or keyword'
        	call fcecho(message)
		ierr = status
	        goto 998
	   else
		ni1 = 1
		unit1 = dunit1
		lo1 = value
		hi1 = value
		col1 = 0	
	   endif
	endif		    
c --- Coord-2 -------------------------------------------------
	status = 0
     	call ftgcno(iunit,.false.,cnam2,col2,ierr)
	if(ierr.EQ.0) then
c ........ Standard configuration, column present
	   call ftgbcl(iunit, col2, dummy20, unit2, dummy20,
     &			ni2, doubdum,doubdum,enull,dummy20,ierr)
           IF(ierr.NE.0)THEN
        	message = errstr//' with FTGBCL for Coord2'
        	call wt_ferrmsg(status,message)
		goto 998
	   endif
           frow = 1
      	   enull = 0
c	   ... Get the first value stored for Coord2
      	   felem = 1
	   call ftgcve(iunit,col2,frow,felem,1,
     &		  enull,lo2,anyflg,status)
c	   ... Get the last value stored for Coord2
      	   felem = ni2
	   call ftgcve(iunit,col2,frow,felem,1,
     &		  enull,hi2,anyflg,status)
           IF(status.NE.0)THEN
        	message = errstr//' getting first/last values Coord2'
        	call wt_ferrmsg(status,message)
		ierr = status
		goto 998
	   endif
	else
c ........ Column NOT present, check for Greenbank Convention
	   status = 0
           call ftgkye(iunit,cnam2,value,comm,status)
	   if(status.NE.0) then
        	message = errstr//' Unable to locate Coord2 grid'
        	call fcecho(message)
		message = ' ... '//cnam2(:8)//
     &			'not found/read as column or keyword'
        	call fcecho(message)
		ierr = status
	        goto 998
	   else
		ni2 = 1
		unit2 = dunit2
		lo2 = value
		hi2 = value
		col2 = 0	
	   endif
	endif		    
c -----------------------------------------------------------------



998     if(ierr.NE.0) then
                message = errstr //
     :                  ' FATAL: Unable to continue'
                call fcecho(message)
        endif

	return
	end

