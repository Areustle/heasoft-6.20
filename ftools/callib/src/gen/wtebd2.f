*+WTEBD2
	subroutine wtebd2(ounit, chatter, 
     &		nk_hist, hist, 
     & 		nk_comm, comment,rmfversn,
     &		telescop, instrume, detnam, filter, areascal, 
     &		fchan, iebound, e_min, e_max, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer iebound, fchan
	real areascal
	real e_min(*), e_max(*)
	character*(*) rmfversn
	character*(*) telescop, instrume, detnam, filter
	character*(*) hist(*), comment(*)
c 
c Description:
c  Creates and Writes the EBOUNDS extension for an RMF file one of the formats
c  conforming to the HDUVERS2='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-002a)
c   HDUVERS2 = '1.0.0'
c   HDUCERS2 = '1.1.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format listed.
c  Assumes the FITS is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c This routine differs from wtebd1.f only in so far that the FCHAN parameter
c  is passed which specifies the channel number of the first PHA channel
c  The value of this parameter therefore is usually either 0 or 1
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  RMFVERSN      i   : String denoting OGIP HDUVERS2 family  
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name
c  FILTER        i   : String listing instrument filter in use
c  AREA          i   : Area scaling factor
c  FCHAN         i   : No. ("name") of first channel (usually 0 or 1)
c  IEBOUND       i   : No. channels in the full array
c  E_MIN         i   : Array containing min nominal energy bound to each chan
c  E_MAX         i   : Array containing max nominal energy bound to each chan
c  IERR            o : Error Flag (0=OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) writes to standard o/p unit
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Dumps FITSIO Error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Code mostly hacked from wtebd1.f, which itself was hacked from 
c  within Alan Smale's BBRSP program
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1995 Jun 21), hist & comment made chara*70 
	character(7) version
	parameter (version = '1.0.0')
*- 

c Internals
	integer status, decimals, itemp
        integer npar, nfields
	integer ie, i, ichan
        parameter (npar = 6, nfields=3, decimals=6)
        character(5) hduvers2
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
        character(70) string
	character(80) message
        character(30)  errstr, wrnstr
c Initialization
	ierr = 0
	status = 0 
        errstr = '** WTEBD2 '//version//' ERROR: '
        wrnstr = '** WTEBD2 '//version//' WARNING: '

c Give user info if requested
        if(chatter.GE.15) then
	  message = ' ... using WTEBD2 ' // version
	  call fcecho(message)
        endif


c Check for sillies
	if(rmfversn(1:1).NE.'1') then
	   message = wrnstr // ' Format/subroutine mismatch'
	   call fcecho(message)
	   message = 
     &		' ...... This routine writes only the 1.*.* family' //
     &		' of formats'
	   call fcecho(message)
	   message = 
     &		' ...... requested format: '// rmfversn
	   call fcecho(message)
	   ierr = 15
	   goto 998
	endif
c Check that we know the format
	if((rmfversn.EQ.'1.0.0').OR.(rmfversn.EQ.'1.1.0')) then
	   hduvers2 = rmfversn
	else
	   hduvers2 = '1.1.0'
           IF (chatter.GE.20) THEN
	    message = wrnstr // ' Unknown format: '// rmfversn
	    call fcecho(message)
	    message = 
     &	      ' ...... Resetting format (HDUVERS2) to '//hduvers2
	    call fcecho(message)
           ENDIF
	endif

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
	ttype(1)   = 'CHANNEL'
	tform(1)   = 'J'
	tunits(1)  = ' '
	ttype(2)   = 'E_MIN'
	tform(2)   = 'E'
	tunits(2)  = 'keV'
	ttype(3)   = 'E_MAX'
	tform(3)   = 'E'
	tunits(3)  = 'keV'

c Write the required header keywords
	call FTPHBN(ounit,iebound,nfields,ttype,tform,tunits,
     &		'EBOUNDS',0,status)
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
     & 		'dataset relates to spectral response',
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
     &		'EBOUNDS',
     & 		'nominal energies of PHA chan boundaries',
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'HDUVERS2 ',
     &		hduvers2,
     &          'Version of format (OGIP memo CAL/GEN/92-002a)',
     &		status)
	message = wrnstr // ' Problem putting HDUVERS2 keyword '
	call wt_ferrmsg(status, message)
	status = 0

c Add the other (passed) OGIP required keywords
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
     &          detnam,
     &          'specific detector name in use',
     &          status)
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

	call FTPKYJ(ounit,'DETCHANS ',
     &		iebound,
     &   	'total number of detector channels',
     &		status)
        message = wrnstr // ' Putting DETCHANS keyword '
        call wt_ferrmsg(status, message)
        status = 0

        call FTPKYF(ounit,'EFFAREA ',
     &          areascal, decimals,
     &          'Area scaling factor',
     &          status)
        message = wrnstr // ' Putting EFFAREA keyword '
        call wt_ferrmsg(status, message)
        status = 0

c Add other advised keywords

	call FTPKYS(ounit,'RMFVERSN ',
     &		'1992a',
     &   	'OGIP classification of FITS format',
     &		status)
        message = wrnstr // ' Putting RMFVERSN keyword '
        call wt_ferrmsg(status, message)
        status = 0

        if(chatter.GE.20) then
          message = ' ... written the OGIP required keywords'
          call fcecho(message)
        endif

	call FTPKYJ(ounit,'TLMIN1 ',
     &		fchan,
     &   	'Minimum value legally allowed in column 1',
     &		status)
        message = wrnstr // ' Putting TLMIN1 keyword '
        call wt_ferrmsg(status, message)
        status = 0

	call FTPKYJ(ounit,'TLMAX1 ',
     &		fchan+iebound-1,
     &   	'Maximum value legally allowed in column 1',
     &		status)
        message = wrnstr // ' Putting TLMAX1 keyword '
        call wt_ferrmsg(status, message)
        status = 0


c Add the (passed) history cards, adding one related to this programme
	itemp = 0
        do i = 1, nk_hist
                call FTPHIS(ounit, hist(i), status)
                if(status.NE.0) then
                        itemp = status
                        status = 0
                        call FTPHIS(ounit,
     &          ' - (missing record) fitsio illegal character ?',
     &           status)
                 endif
        enddo
        write(string,'(2a)')
     &		' FITS EBOUNDS extension written by WTEBD2 ',
     &                          version
        call FTPHIS(ounit,string,status)
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
	call FTBDEF(ounit,nfields,tform,0,iebound,status)
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
	do ie = 1, iebound
		ichan = fchan + ie - 1
		call FTPCLJ(ounit, 1, ie, 1, 1, ichan,status)
		call FTPCLE(ounit, 2, ie, 1, 1, e_min(ie),status)
		call FTPCLE(ounit, 3, ie, 1, 1, e_max(ie),status)
	enddo

        if(chatter.GE.20) then
          message = ' ... written the data'
          call fcecho(message)
        endif

c Final check for errors
        message = wrnstr // ' Writing Data '
        call wt_ferrmsg(status, message)

998     if(ierr.NE.0) then
           message = errstr // ' FATAL: Extension not written'
           call fcecho(message)
        endif
 

	return
	end

	
