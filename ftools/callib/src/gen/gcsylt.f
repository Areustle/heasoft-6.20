*+GCSYLT
	subroutine gcsylt(iunit, chatter, incol, lo, hi, unit, 
     &		ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer incol(2)
	real lo, hi
	character*(*) unit
c 
c Description:
c  Finds & returns the lowest & highest values stored for in the columns 
c specified by INCOL column numbers along with the unit string. 
c NOTE: that INCOL is 2-dimension, allowing (say) both the ENERG_LO and 
c       ENERG_HI columns to be searched with one call.
c
c  CURRENTLY this routine simply returns the first value in the 1st col, 
c            and last value in the 2nd col ...
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  INCOL         i   : Columns Numbers of the 2 columns to be searched 
c  LO              o : Lowest value stored in columns
c  HI              o : Highest value stored in columns
c  UNIT            o : Unit string of columns
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to STDOUT
c  subroutine FTGBCL     : (FITSIO) Gets BINTABLE info
c  subroutine FTGCVE	 : (FITSIO) Reads BINTABLE dataset
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1995 Jul 06), original test version
	character(7) version
	parameter (version = '1.0.0')
*- 
c Internals
	character(20) dummy20
	double precision doubdum
	integer status, frow, felem, clenact
	integer enull, ni1, ni2
	character(20) unit2
	character(80) message
        character(40)  errstr, wrnstr
	logical anyflg
c Initialize
	ierr = 0
	status = 0
        errstr = '** GCSYLT '//version//' ERROR: '
        wrnstr = '** GCSYLT '//version//' WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using GCSYLT '// version
	     call fcecho(message)
        endif


c -----------------------------------------------------------------
c Coord-1
	status = 0
	call ftgbcl(iunit, incol(1), dummy20, unit, dummy20,
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
	call ftgcve(iunit,incol(1),frow,felem,1,
     &		  enull,lo,anyflg,status)
	IF(ierr.NE.0)THEN
        	message = errstr//' with FTGCVE for Coord1'
        	call wt_ferrmsg(status,message)
		goto 998
	endif


c Coord-2
	status = 0
	call ftgbcl(iunit, incol(2), dummy20, unit2, dummy20,
     &			ni2, doubdum,doubdum,enull,dummy20,ierr)
	IF(ierr.NE.0)THEN
        	message = errstr//' with FTGBCL for Coord2'
        	call wt_ferrmsg(status,message)
		goto 998
	endif
	frow = 1
      	enull = 0
c	   ... Get the first value stored for Coord1
      	felem = ni2
	call ftgcve(iunit,incol(2),frow,felem,1,
     &		  enull,hi,anyflg,status)
	IF(ierr.NE.0)THEN
        	message = errstr//' with FTGCVE for Coord2'
        	call wt_ferrmsg(status,message)
		goto 998
	endif

c Checks & Clean-up
	if(ni1.NE.ni2) then
	        message = errstr//' No. elements mismatch'
		call fcecho(message)
		ierr = 1
		goto 998	
	endif

	if(unit.NE.unit2) then
	        message = errstr//' Units mismatch'
		call fcecho(message)
		message = ' ...... discrepant values: "'//
     &			unit(:clenact(unit))//'" & "'//
     &			unit2(:clenact(unit2))//'"'
		call fcecho(message)
		ierr = 1
		goto 998	
	endif


c -----------------------------------------------------------------



998     if(ierr.NE.0) then
                message = errstr //
     :                  ' FATAL: Unable to continue'
                call fcecho(message)
        endif

	return
	end

