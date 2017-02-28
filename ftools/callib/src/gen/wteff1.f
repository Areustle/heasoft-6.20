*+WTEFF1
	subroutine wteff1(ounit, chatter, 
     &		nk_hist, hist, nk_comm, comment, effversn, 
     &		telescop, instrume, detnam, filter,
     &   	coordsys, max_en, max_ic1, max_ic2,
     &		icoord1, coord1, c1units, 
     &		icoord2, coord2, c2units,
     &		ienerg, energ_lo, energ_hi, enunits, 
     &		eff, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer icoord1, icoord2, ienerg
	integer fcstln
	integer max_en, max_ic1, max_ic2
	real eff(max_en,max_ic1,max_ic2)
	real energ_lo(*), energ_hi(*)
	real coord1(*), coord2(*)
	character*(*) coordsys, c1units, c2units, enunits
	character*(*) telescop, instrume, detnam, filter, effversn
	character*(*)hist(nk_hist), comment(nk_comm)
c 
c Description:
c  Creates and writes a (detector) Efficiency extension conforming to 
c   the HDUVERS2 = '1.*.*' family.
c  Currently the following formats are supported (see CAL/GEN/92-019)
c   HDUVERS2 = '1.1.0'
c  The requested format is checked, and if belonging to the '1,*.*' family
c  but not included above, the extension is written in the last format listed.
c  	!!! Note !!!! 
c  If ICOORDN=1 and COORDN(1)=-999, then routine will assume efficiency not a 
c      function of that spatial variable.
c  File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open output file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  EFFVERSN      i   : String denoting OGIP HDUVERS2 family
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  COORDSYS      i   : String giving (OGIP caldb) coordinate system in use
c  MAX_EN        i   : Max size of ENERGY dimension of 3-d array
c  MAX_IC1       i   : Max size of Coord-1 dimension of 3-d array
c  MAX_IC1       i   : Max size of Coord-1 dimension of 3-d array
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
c  EFF		 i   : Array containing the full efficiency matrix
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
c  Ian M George     (93 Jul 25:1.0.0), original test version
	character(7) version
	parameter (version = '1.0.0')
*- 

c Internals
	integer status, kk, decimals, clenact
        integer nfields
	integer i, ie, ii, jj, ll
	integer ncols, imax, itemp
	integer maxdim
        parameter (nfields=5, maxdim=1000000)
	real values(maxdim)
	logical qen, qth, qph
        character(5) hduvers2
 	character(8) cnm1, cnm2
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(40) tcomm(nfields)
	character(80) message
	character(255) wrtstr, wrtstr2
        character(40)  errstr, wrnstr
c Initialize
	ierr = 0
	status = 0
        errstr = '** WTEFF1 '//version//' ERROR: '
        wrnstr = '** WTEFF1 '//version//' WARNING: '
	qen = .true.
	qth = .true.
	qph = .true.
  

c Give user info if requested
        if(chatter.GE.20) then
                message = ' ... using WTEFF1 '//
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

c Check we can deal with the format
        if(effversn(1:1).NE.'1') then
          message = wrnstr // ' Format/subroutine mismatch'
          call fcecho(message)
          message =
     &          ' ...... This routine writes only the 1.*.* family' //
     &          ' of formats'
          call fcecho(message)
          message = ' ...... requested format: '// effversn
          call fcecho(message)
          ierr = 15
          goto 998
        endif
c Check we know the format
        if(effversn.EQ.'1.1.0') then
                hduvers2 = effversn
        else
                hduvers2 = '1.1.0'
                message = wrnstr // 'Unknown format: '//effversn
                call fcecho(message)
                message =
     &          '  ...... Resetting format (HDUVERS2) to '//hduvers2
                call fcecho(message)
        endif

c Get the Coordinate System passed & get the OGIP caldb standard col names
	call gt_csysnms(chatter, coordsys,
     & 		cnm1, cnm2, ierr)
	ierr = 0

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
     &		'Eff dataset contains only a single value'
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
	  tcomm(kk)   = 'Lower boundaries of energy bins'
	  kk = kk + 1

	  ttype(kk)   = 'ENERG_HI'
	  tform(kk)   = tform(kk-1)
	  tunits(kk)  = enunits
	  tcomm(kk)   = 'Upper boundaries of energy bins'
	  kk = kk + 1
	endif

	if(qth) then
	  ttype(kk)   = cnm1
          write(wrtstr,'(i12,a)') icoord1,'E'
          call crmvlbk(wrtstr)
	  tform(kk)   = wrtstr(1:10)
	  tunits(kk)  = c1units
	  tcomm(kk)   = 'Spatial coord grid: dimension 1'
	  kk = kk + 1
	endif

	if(qph) then
	  ttype(kk)   = cnm2
          write(wrtstr,'(i12,a)') icoord2,'E'
          call crmvlbk(wrtstr)
	  tform(kk)   = wrtstr(1:10)
	  tunits(kk)  = c2units
	  tcomm(kk)   = 'Spatial coord grid: dimension 2'
	  kk = kk + 1
	endif

	ttype(kk)   = 'DET_EFF'
        write(wrtstr,'(i12,a)') imax,'E'
        call crmvlbk(wrtstr)
	tform(kk)   = wrtstr(1:10)
	tunits(kk)  = ' '
	tcomm(kk)   = 'Detector Efficiency dataset'

c Write the required header keywords
	call FTPHBN(ounit,1,ncols,ttype,tform,tunits,
     &		'DETECTOR EFFICIENCY',0,status)
	message = errstr
	call wt_ferrmsg(status, message)
        if(status.NE.0) then
		ierr = 1
		goto 998
        endif


	if(chatter.ge.20) then
	  message = ' ... written the extension header keywords'
          call fcecho(message)
	endif	
c 
c - Fix up the Comments in the TTYPE keywords
c
	status = 0
	do i = 1, ncols
	  write(wrtstr,'(a5,i2)') 'TTYPE',i
	  call crmvblk(wrtstr)
	  call ftmcom(ounit,wrtstr(:8),tcomm(i),status)
	  if(status.NE.0) then
	   message = wrnstr // 'Problem altering '// wrtstr(:8)
	   call wt_ferrmsg(status,message)
	   status = 0
	  endif
	enddo

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
     & 		'dataset relates to instrument response',
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
     &		'DET_EFF',
     & 		'dataset is a detector efficiency',
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUVERS2 ',
     &		hduvers2,
     &          'Version of format (OGIP memo CAL/GEN/92-025)',
     &		status)
	message = wrnstr // ' Problem putting HDUVERS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

c Add other passed keyword values
 	call FTPKYS(ounit,'CSYSNAME',
     &		coordsys,
     &   	'spatial coord system used in this dataset',
     &		status)
	message = wrnstr // ' Problem putting COORDSYS keyword '
	call wt_ferrmsg(status, message)
	status = 0

 	call FTPKYS(ounit,'TELESCOP ',
     &		telescop,
     &   	'mission/satellite name',
     &		status)
	message = wrnstr // ' Problem putting TELESCOP keyword '
	call wt_ferrmsg(status, message)
	status = 0
	
	call FTPKYS(ounit,'INSTRUME ',
     &		instrume,
     &   	'instrument/detector name',
     &		status)
	message = wrnstr // ' Problem putting INSTRUME keyword '
	call wt_ferrmsg(status, message)
	status = 0

	if(detnam.NE.' ') then
	call FTPKYS(ounit,'DETNAM ',
     &		detnam,
     &   	'specific detector in use',
     &		status)
	  message = wrnstr // ' Problem putting DETNAM keyword '
	  call wt_ferrmsg(status, message)
	  status = 0
	endif

	if(filter.NE.' ') then
	call FTPKYS(ounit,'FILTER ',
     &		filter,
     &   	'filter in use',
     &		status)
	  message = wrnstr // ' Problem putting FILTER keyword '
	  call wt_ferrmsg(status, message)
	  status = 0
	endif

	if(.not.qen) then
	  ii = LEN(enunits)
          wrtstr = '(' // enunits(:ii) // ')'
          call crmvlbk(wrtstr)
	  wrtstr2 = 'Low Energy bound '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'ENERG_LO ',
     &		energ_lo(1), decimals,
     &		wrtstr2,
     &		status)
	message = wrnstr // ' Problem putting ENERG_LO keyword '
	call wt_ferrmsg(status, message)
	status = 0
	  wrtstr2 = 'High Energy bound '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	  call FTPKYF(ounit,'ENERG_HI ',
     &		energ_hi(1), decimals,
     &		wrtstr2,
     &		status)
	message = wrnstr // ' Problem putting ENERG_HI keyword '
	call wt_ferrmsg(status, message)
	status = 0
	endif

	if(.not.qth) then
	  if(coord1(1).NE.-999) then
	    ii = LEN(c1units)
            wrtstr = '(' // c1units(:ii) // ')'
            call crmvlbk(wrtstr)
	    wrtstr2 = 'Coordinate 1 '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	    call FTPKYF(ounit,cnm1,
     &		coord1(1), decimals,
     &		wrtstr2,
     &		status)
	    message = wrnstr // ' Problem putting coord1 keyword '
	    call wt_ferrmsg(status, message)
	    status = 0
	  else
	    wrtstr = 'Dataset assumed to be independent of '// cnm1
	    call FTPCOM(ounit, wrtstr, status)
	  endif
	endif

	if(.not.qph) then
	  if(coord2(1).NE.-999) then
	    ii = LEN(c1units)
            wrtstr = '(' // c2units(:ii) // ')'
            call crmvlbk(wrtstr)
	    wrtstr2 = 'Coordinate 2 '// wrtstr(:fcstln(wrtstr)) //
     &		' - single valued column'
	    call FTPKYF(ounit,cnm2,
     &		coord2(1), decimals,
     &		wrtstr2,
     &		status)
	    message = wrnstr // ' Problem putting coord2 keyword '
	    call wt_ferrmsg(status, message)
	    status = 0
	  else
	    wrtstr = 'Dataset assumed to be independent of '// cnm2
	    call FTPCOM(ounit, wrtstr, status)
	  endif
	endif

c The TDIM (matrix ordering) keyword
	status = 0
	write(wrtstr2, '(a,i12)') 'TDIM', ncols
        call crmvblk(wrtstr2)

	wrtstr = '('
	if(qen) then
		write(wrtstr,'(a,i12)') wrtstr(:clenact(wrtstr)), ienerg
	endif
	if(qth) then
	     if(qen) then
		write(wrtstr,'(a,a,i12)') 
     &			wrtstr(:clenact(wrtstr)), ',', icoord1
	     else
		write(wrtstr,'(a,i12)') wrtstr(:clenact(wrtstr)), icoord1
	     endif	
	endif
	if(qph) then
	     if(qth) then
		write(wrtstr,'(a,a,i12)') 
     &			wrtstr(:clenact(wrtstr)), ',', icoord2
	     else
		write(wrtstr,'(a,i12)') wrtstr(:clenact(wrtstr)), icoord2
	     endif	
	endif
	wrtstr = wrtstr(:clenact(wrtstr))//')'
        call crmvblk(wrtstr)

	call FTPKYS(ounit, wrtstr2,
     &		wrtstr,
     &   	'Ordering of n-d DET_EFF array',
     &		status)

c The vector coordinate keywords
	jj = 0
	if(qen)then
	 jj = jj + 1
	 write(wrtstr2, '(i12,a,i12)') jj,'CTYP', ncols
         call crmvblk(wrtstr2)
	 call FTPKYS(ounit, wrtstr2,
     &		'ENERGY',
     &   	'Axis of 1st dimension of DET_EFF array',
     &		status)
	endif
	if(qth) then
	 jj = jj + 1
	 write(wrtstr2, '(i12,a,i12)') jj,'CTYP', ncols
         call crmvblk(wrtstr2)
	 call FTPKYS(ounit, wrtstr2,
     &		cnm1,
     &   	'Axis of 2nd dimension of DET_EFF array',
     &		status)
	endif
	if(qph) then
	 jj = jj + 1
	 write(wrtstr2, '(i12,a,i12)') jj,'CTYP', ncols
         call crmvblk(wrtstr2)
	 call FTPKYS(ounit, wrtstr2,
     &		cnm2,
     &   	'Axis of 3rd dimension of DET_EFF array',
     &		status)
	endif


c Add other advised keywords
	status = 0
	call FTPKYS(ounit,'EFFVERSN',
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
     &			' Extension written by WTEFF1 ',
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
	call FTBDEF(ounit,ncols,tform,0,1,status)
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

c		... the efficiencies

		ii = 0		
		do ll=1,ICOORD2
		   do jj=1,ICOORD1
			do ie = 1, ienerg
				ii = ii + 1
				values(ii) = eff(ie,jj,ll)
			enddo
		   enddo
		enddo
		call FTPCLE(ounit, kk, 1, 1, imax,values,status)

c Final check for errors
	if(status.NE.0) then
		message = errstr // ' Writing Data'
		call wt_ferrmsg(status,message)
		ierr = 20
		goto 998
	endif

998	if(ierr.NE.0) then
		message = errstr //
     :			' FATAL: Extension not successfully written'
		call fcecho(message)
	else
	  if(chatter.GE.20) then
		message = ' DET_EFF extension successfully written'
	   	call fcecho(message)
	  endif
	endif


	return
	end
	
