*+WT_EAR1992a
	subroutine wt_ear1992a(ounit, chatter, 
     &		nk_hist, hist, 
     & 		nk_comm, comment,
     &		telescop, instrume, detnam, filter,
     &   	coordsys,
     &		icoord1, coord1, c1units, 
     &		icoord2, coord2, c2units,
     &		ienerg, energ_lo, energ_hi, enunits, 
     &		ear, earunits, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer icoord1, icoord2, ienerg
	integer fcstln
	real energ_lo(ienerg), energ_hi(ienerg)
	real ear(ienerg,icoord1,icoord2)
	real coord1(icoord1), coord2(icoord2)
	character(8) coordsys, c1units, c2units, enunits, earunits
	character(16) telescop, instrume, detnam, filter
	character(80) hist(nk_hist), comment(nk_comm)
c 
c Description:
c  Creates and Writes the EFF AREA extension for an EARVERSN=1992a 
c  Effective Area file. Assumes the FITS file is open and has had the 
c  Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c  Matrix etc written as FIXED length arrays only at present
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open output file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  COORDSYS      i   : String giving (OGIP caldb) coordinate system in use
c  ICOORD1       i   : No. coordinate 1 bins
c  COORD1        i   : Array containing coord 1 bins 
c  C1UNITS       i   : String giving physical units of COORD1 array
c  ICOORD2       i   : No. coordinate 2 bins
c  COORD2        i   : Array containing coord 2 bins 
c  C2UNITS       i   : String giving physical units of COORD2 array
c  IENERG        i   : No. energy bins
c  ENERG_LO      i   : Array containing lower bound to each energy bin
c  ENERG_HI      i   : Array containing upper bound to each energy bin
c  ENUNITS       i   : String giving physical units of ENERG_LO & HI arrays
c  EAR           i   : Array containing the full Eff area matrix (cm**2)
c  EARUNITS      i   : String giving physical units of EAR array
c  IERR            o : Error Flag (ierr = 0 on successful completion)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine CRMVBLK    : (CALTOOLS) Removes blank from a string
c  subroutine CRMVLBK    : (CALTOOLS) Removes leading blanks from a string
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine GT_CSYSNAM : (CALLIB) Returns col names for coord system
c  subroutine WT_FERRMSG : (CALLIB) Writes fitsio error message
c
c Compilation & Linking
c  link with FITSIO & CALLIB
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1993 Jan 04), original
c  Ian M George     (1993 Mar 29), converted to use fcecho etc
	character(7) version
	parameter (version = '0.9.0')
*- 

c Internals
	integer status, kk, decimals
        integer nfields
	integer i, ie, ii, jj, ll
	integer ncols, imax, itemp
	integer maxdim
        parameter (nfields=5, maxdim=1000000)
	real values(maxdim)
	logical qen, qth, qph
	character(8) cnm1, cnm2
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(30) errtxt
	character(80) message
	character(255) wrtstr, wrtstr2
        character(24)  errstr, wrnstr
c Initialize
	ierr = 0
	status = 0
        errstr = '** WT_EAR1992a ERROR: '
        wrnstr = '** WT_EAR1992a WARNING: '
	qen = .true.
	qth = .true.
	qph = .true.
  

c Give user info if requested
        if(chatter.GE.20) then
                message = ' ... using WT_EAR1992a '//
     &                  version
		call fcecho(message)
        endif

c Calc whether our arrays are big enough
	imax = ienerg * icoord1 * icoord2
	if(imax.GT.maxdim) then
		message = errstr // ' maxdim array too small'
		call fcecho(message)
		ierr = 9
		goto 998
	endif


c Get the Coordinate System passed & get the OGIP caldb standard col names
	call gt_csysnms(chatter, coordsys,
     & 		cnm1, cnm2, ierr)

c Create a new extension
	call FTCRHD(ounit,status)
	message = errstr
	call wt_ferrmsg(status, message)
        if(status.NE.0) then
		ierr = 1
		goto 998
        endif
	if(chatter.GE.20) then
		message = ' ... new extension created'
		call fcecho(message)
	endif

c Sort out how many columns we really have...
	ncols = nfields 
	if(ienerg.LE.1) then
		ncols = ncols - 2
		qen = .false.
	endif
	if(ICOORD1.LE.1) then
		ncols = ncols - 1
		qth = .false.
	endif
	if(ICOORD2.LE.1) then
		ncols = ncols - 1
		qph = .false.
	endif
c Check for sillies
	if(ncols.EQ.1) then
	 	message = wrnstr // 
     &		'EAR dataset contains only a single value'
	 	call fcecho(message)
	endif

c Set up the columns n stuff
	kk = 1
	if(qen) then
	  ttype(kk)   = 'ENERG_LO'
          write(wrtstr,'(i12,a)') ienerg,'E'
          call crmvlbk(wrtstr)
	  tform(kk)   = wrtstr(1:10)
	  tunits(kk)  = enunits
	  kk = kk + 1

	  ttype(kk)   = 'ENERG_HI'
	  tform(kk)   = tform(kk-1)
	  tunits(kk)  = enunits
	  kk = kk + 1
	endif

	if(qth) then
	  ttype(kk)   = cnm1
          write(wrtstr,'(i12,a)') icoord1,'E'
          call crmvlbk(wrtstr)
	  tform(kk)   = wrtstr(1:10)
	  tunits(kk)  = c1units
	  kk = kk + 1
	endif

	if(qph) then
	  ttype(kk)   = cnm2
          write(wrtstr,'(i12,a)') icoord2,'E'
          call crmvlbk(wrtstr)
	  tform(kk)   = wrtstr(1:10)
	  tunits(kk)  = c2units
	  kk = kk + 1
	endif

	ttype(kk)   = 'EFFAREA'
        write(wrtstr,'(i12,a)') imax,'E'
        call crmvlbk(wrtstr)
	tform(kk)   = wrtstr(1:10)
	tunits(kk)  = earunits

c Write the required header keywords
	call FTPHBN(ounit,1,nfields,ttype,tform,tunits,
     &		'EFFAREA',0,status)
	message = errstr
	call wt_ferrmsg(status, message)
        if(status.NE.0) then
		ierr = 1
		goto 998
        endif
	if(chatter.GE.20) then
	  message = ' ... written the extension header keywords'
	  call fcecho(message)
	endif

c Add the other (passed) OGIP required keywords
 	call FTPKYS(ounit,'TELESCOP ',
     &		telescop,
     &   	'mission/satellite name',
     &		status)
	
	call FTPKYS(ounit,'INSTRUME ',
     &		instrume,
     &   	'instrument/detector name',
     &		status)

	if(detnam.NE.' ') then
	call FTPKYS(ounit,'DETNAM ',
     &		detnam,
     &   	'specific detector in use',
     &		status)
	endif

	call FTPKYS(ounit,'FILTER ',
     &		filter,
     &   	'filter in use',
     &		status)

	if(.not.qen) then
	  ii = LEN(enunits)
          write(wrtstr,'(a,a,a)') '(',enunits(:ii),')'
          call crmvlbk(wrtstr)
	  wrtstr2 = 'Low Energy bound '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'ENERG_LO ',
     &		energ_lo(1), decimals,
     &		wrtstr2,
     &		status)
	  wrtstr2 = 'High Energy bound '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'ENERG_HI ',
     &		energ_hi(1), decimals,
     &		wrtstr2,
     &		status)
	endif

	if(.not.qth) then
	  ii = LEN(c1units)
          write(wrtstr,'(a,a,a)') '(',c1units(:ii),')'
          call crmvlbk(wrtstr)
	  wrtstr2 = 'Coordinate 1 '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'coord1 ',
     &		coord1(1), decimals,
     &		wrtstr2,
     &		status)
	endif

	if(.not.qth) then
	  ii = LEN(c1units)
          write(wrtstr,'(a,a,a)') '(',c2units(:ii),')'
          call crmvlbk(wrtstr)
	  wrtstr2 = 'Coordinate 2 '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'coord2 ',
     &		coord2(1), decimals,
     &		wrtstr2,
     &		status)
	endif

c The TDIM (matrix ordering) keyword
	write(wrtstr2, '(a,i12)') 'TDIM', ncols
        call crmvblk(wrtstr)

        write(wrtstr,'(a,i12,a,i12,a,i12,a)') 
     & 		'(',ienerg,',',icoord1,',',icoord2,')'
        call crmvblk(wrtstr)

	call FTPKYS(ounit, wrtstr2,
     &		wrtstr,
     &   	'Ordering of n-d array',
     &		status)

c Add other advised keywords
	call FTPKYS(ounit,'EARVERSN ',
     &		'1992a',
     &   	'OGIP classification of FITS format',
     &		status)

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
			call FTPHIS(ounit,'(missing record)', status)
		endif
	enddo
        write(wrtstr,'(2a)') 
     &			' Extension written by WT_EAR1992a ',
     &                   version
	call FTPHIS(ounit,wrtstr,status)
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
			call FTPCOM(ounit,'(missing record)', status)
		endif
	enddo
	message = wrnstr // ' Putting at least one Comment record'
	call wt_ferrmsg(itemp, message)
	if(chatter.GE.20) then
	  message = ' ... written the Comment keywords'
	  call fcecho(message)
	endif
	status = 0


c Define the extension data structure
	call FTBDEF(ounit,nfields,tform,0,ienerg,status)
	message = errstr // ' Defining Data Structure'
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
	kk = 1
	if(qen) then
c 		... the energy bin
		call FTPCLE(ounit, kk, 1, 1, ienerg, energ_lo,status)
		call FTPCLE(ounit, kk+1, 1, 1, ienerg, energ_hi,status)
		kk = kk + 2
	endif
	
	if(qth) then
c		... the coord1 bins
		call FTPCLE(ounit, kk, 1, 1, ICOORD1,coord1,status)
		kk = kk + 1
	endif

	if(qph) then
c		... the coord2 bins
		call FTPCLE(ounit, kk, 1, 1, ICOORD2,coord2,status)
		kk = kk + 1
	endif

c		... the effareas

		ii = 0		
		do ll=1,ICOORD2
		   do jj=1,ICOORD1
			do ie = 1, ienerg
				ii = ii + 1
				values(ii) = ear(ie,jj,ll)
	if(values(ii).LE.0.0) write(*,*) ie, jj, ll, ear(ie,jj,ll)
			enddo
		   enddo
		enddo
		call FTPCLE(ounit, kk, 1, 1, imax,values,status)

c Final check for errors
	if(status.NE.0) then
		call ftgerr(status, errtxt)
                write(5,'(2a)') errstr, ' Due to...'
                write(5,'(a)') errtxt
	endif

998	if(ierr.NE.0) then
		message = errstr //
     :			' FATAL: Extension not successfully written'
		call fcecho(message)
	else
	  if(chatter.GE.20) then
		message = ' EAR extension successfully written'
	   	call fcecho(message)
	  endif
	endif


	return
	end
	
