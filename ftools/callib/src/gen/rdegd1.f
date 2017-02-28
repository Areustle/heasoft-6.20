*+RDEGD1
        subroutine rdegd1(iunit, chatter,
     &          egridver, 
     &          telescop, instrume, detnam, filter,
     &          ienerg, energ_lo, energ_hi, enunits,
     &          ierr)

        IMPLICIT NONE
        integer chatter, ierr
        integer iunit
        integer ienerg
        real energ_lo(*), energ_hi(*)
        character*(*) enunits
        character*(*) telescop, instrume, detnam, filter, egridver

c Description:
c  Reads an Energy Grid extension conforming to the HDUVERS2 = '1.*.*' fam.
c  Currently the following formats are supported 
c   HDUVERS2 = '1.1.0'
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open input file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  EGRIDVER      i   : String denoting OGIP HDUVERS2 family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  IENERG        i   : No. energy bins
c  ENERG_LO      i   : Array containing lower bound to each energy bin
c  ENERG_HI      i   : Array containing upper bound to each energy bin
c  ENUNITS       i   : String giving physical units of ENERG_LO & HI arrays
c  IERR            o : Error Flag (ierr = 0 on successful completion)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c
c Compilation & Linking
c  link with FITSIO & CALLIB
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (93 Jan 04:1.0.0), original test version
c  
	character(7) version
	parameter (version = '1.0.0')
*- 
c Internals
	character(20) dummy20
	double precision doubdum
	integer status
        integer nfields, irows, ivar, frow, felem, colnum
	integer i, enull
	integer ncols
        parameter (nfields=2)
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(40) comm
	character(80) message
        character(40)  errstr, wrnstr
	logical qen, qcoord1, qcoord2, anyflg
c Initialize
	ierr = 0
	status = 0
        errstr = '** RDEGD1 '//version//' ERROR: '
        wrnstr = '** RDEGD1 '//version//' WARNING: '
	qen = .true.
	qcoord1 = .false.
	qcoord2 = .false.
  
c Give user info if requested
        if(chatter.GE.20) then
                message = ' ... using RDEGD1 '//
     &                  version
		call fcecho(message)
        endif


c
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
     &	  ' ... Sorry, this version will ignore all but 1st row'
        call fcecho(message)
      ENDIF

c EGRDVER ...
      egridver = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',egridver,comm,status)
      message = wrnstr//' reading HDUVERS2'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,message)
      ENDIF
      IF (egridver.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'EGRIDVER',egridver,comm,status)
        message = wrnstr//' reading EGRIDVER'
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
      If (status.EQ.202) THEN
        filter = 'NONE'
      else
        message = wrnstr//' reading FILTER '
        call wt_ferrmsg(status,message)
	message = ' ... assuming no FILTER present'
	call fcecho(message)
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

c Sort out which columns are present
	do i = 1, nfields
	 if(ttype(i).EQ.'ENERG_LO'.OR.ttype(i).EQ.'ENERG_HI') then
		qen = .true.
		enunits = tunits(i)
		call ftgbcl(iunit,i,dummy20,dummy20,dummy20,
     &			ienerg,doubdum,doubdum,enull,dummy20,status)
	 endif
	enddo

c
c --- READ DATA ---
c
	if(qen) then
c ENERG_LO ...
      		frow = 1
      		felem = 1
      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      		If (status.NE.0) THEN
         	  message = errstr//' ENERG_LO column not present'
         	  call fcecho(message)
         	  ierr = 4
		  goto 998
      		ENDIF
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,ienerg,
     &		  enull,energ_lo,anyflg,status)
      		IF (status.NE.0) THEN
        	  message = errstr//' reading ENERG_LO column'
        	  call fcecho(message)
        	  ierr = 1
		  goto 998
      		ENDIF
c ENERG_HI ...
      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      		If (status.NE.0) THEN
         	   message = errstr//' ENERG_HI column not present'
         	   call fcecho(message)
         	   ierr = 4
		   goto 998
      		ENDIF
      		enull = 0
      		call ftgcve(iunit,colnum,frow,felem,ienerg,
     &			enull,energ_hi,anyflg,status)
      		IF (status.NE.0) THEN
        	  message = errstr//' reading ENERG_HI column'
        	  call fcecho(message)
        	  ierr = 1
		  goto 998
      		ENDIF
	else
      		status = 0
		ienerg = 1
		enunits = 'keV'
      		call ftgkye(iunit,'ENERG_LO',energ_lo(1),comm,status)
      		If (status.NE.0) THEN
		 ierr = 1
      		 message = wrnstr//' reading ENERG_LO '
      		 call wt_ferrmsg(status,message)
		 goto 998
      		ENDIF

      		status = 0
      		call ftgkye(iunit,'ENERG_HI',energ_hi(1),comm,status)
      		If (status.NE.0) THEN
		 ierr = 1
      		 message = wrnstr//' reading ENERG_HI '
      		 call wt_ferrmsg(status,message)
		 goto 998
      		ENDIF
	endif

998     if(ierr.NE.0) then
                message = errstr //
     :                  ' FATAL: Extension NOT successfully read'
                call fcecho(message)
        else
          if(chatter.GE.20) then
                message = ' EGRID extension successfully read'
                call fcecho(message)
          endif
        endif

	return
	end
	
