*+WTEGD1
	subroutine wtegd1(ounit, chatter, 
     &		nk_hist, hist, nk_comm, comment, egridver, 
     &		telescop, instrume, detnam, filter,
     &		ienerg, energ_lo, energ_hi, enunits, 
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer ienerg
	real energ_lo(ienerg), energ_hi(ienerg)
	character*(*) enunits
	character*(*) telescop, instrume, detnam, filter, egridver
	character*(*)hist(nk_hist), comment(nk_comm)
c 
c Description:
c  Creates and writes an Energy Grid extension conforming to 
c   the HDUVERS2 = '1.*.*' family.
c  Currently the following formats are supported 
c   HDUVERS2 = '1.1.0'
c  The requested format is checked, and if belonging to the '1,*.*' family
c  but not included above, the extension is written in the last format listed.
c  !!! Note !!!! File is left open at the end  
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
c  subroutine CRMVBLK    : (CALTOOLS) Removes blank from a string
c  subroutine CRMVLBK    : (CALTOOLS) Removes leading blanks from a string
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Writes fitsio error message
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
	integer status
        integer nfields
	integer i
	integer ncols, itemp
	integer maxdim
        parameter (nfields=2, maxdim=1000000)
        character(5) hduvers2
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(40) tcomm(nfields)
	character(80) message
	character(255) wrtstr
        character(40)  errstr, wrnstr
c Initialize
	ierr = 0
	status = 0
        errstr = '** WTEGD1 '//version//' ERROR: '
        wrnstr = '** WTEGD1 '//version//' WARNING: '

c Give user info if requested
        if(chatter.GE.20) then
                message = ' ... using WTEGD1 '//
     &                  version
		call fcecho(message)
        endif

c Calc whether our arrays are big enough
	if(ienerg.GT.maxdim) then
		message = errstr // ' maxdim array too small'
		call fcecho(message)
		ierr = 9
		goto 998
	endif

c Check we can deal with the format
        if(egridver(1:1).NE.'1') then
          message = wrnstr // ' Format/subroutine mismatch'
          call fcecho(message)
          message =
     &          ' ...... This routine writes only the 1.*.* family' //
     &          ' of formats'
          call fcecho(message)
          message = ' ...... requested format: '// egridver
          call fcecho(message)
          ierr = 15
          goto 998
        endif
c Check we know the format
        if(egridver.EQ.'1.1.0') then
                hduvers2 = egridver
        else
                hduvers2 = '1.1.0'
                message = wrnstr // 'Unknown format: '//egridver
                call fcecho(message)
                message =
     &          '  ...... Resetting format (HDUVERS2) to '//hduvers2
                call fcecho(message)
        endif

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

c Set up the columns n stuff
	  ttype(1)   = 'ENERG_LO'
c          write(wrtstr,'(i12,a)') ienerg,'E'
c          call crmvlbk(wrtstr)
	  tform(1)   = 'E'
	  tunits(1)  = enunits
	  tcomm(1)   = 'Lower boundaries of energy bins'

	  ttype(2)   = 'ENERG_HI'
	  tform(2)   = tform(1)
	  tunits(2)  = enunits
	  tcomm(2)   = 'Upper boundaries of energy bins'

	ncols = 2	

c Write the required header keywords
	call FTPHBN(ounit,ienerg,ncols,ttype,tform,tunits,
     &		'STANDARD ENERGY GRID',0,status)
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
     &		'ENERGY_GRID',
     & 		'dataset is a standard energy grid',
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUVERS2 ',
     &		hduvers2,
     &          'Version of format',
     &		status)
	message = wrnstr // ' Problem putting HDUVERS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

c Add other passed keyword values
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

c Add other advised keywords
	status = 0
	call FTPKYS(ounit,'EGRIDVER ',
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
     &			' Extension written by WTEGD1 ',
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
	call FTBDEF(ounit,ncols,tform,0,ienerg,status)
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
c 		... the energy bin
	do i = 1, ienerg
		call FTPCLE(ounit, 1, i, 1, 1, energ_lo(i),status)
		call FTPCLE(ounit, 2, i, 1, 1, energ_hi(i),status)
	enddo

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
		message = ' EGRID extension successfully written'
	   	call fcecho(message)
	  endif
	endif


	return
	end
	
