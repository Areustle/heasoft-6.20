*+WT_FLT1992a
        subroutine wt_flt1992a(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment,
     &          telescop, instrume, detnam, filter,
     &		ienerg, E_lo, E_hi, eunits,
     &		csys, ipos, x, y, punits,
     &          trans, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer ounit, nk_hist, nk_comm
	integer ienerg, ipos
	real E_lo(ienerg), E_hi(ienerg)
	real x(ipos), y(ipos)
	real trans(ienerg, ipos)
	character(8) csys
	character(20) eunits, punits
	character(16) telescop, instrume, detnam, filter
	character(80) hist(nk_hist), comment(nk_comm)
c 
c Description:
c  Creates and Writes the TRANSMISSION extension for an FLTVERSN=1992a file
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : Name of specific detector (if INSTRUME insufficient)
c  FILTER        i   : String listing instrument filter in use
c  IENERG        i   : No. energy bins
c  E_LO          i   : Array containing lower bound to each energy bin 
c  E_HI          i   : Array containing upper bound to each energy bin 
c  EUNITS        i   : Physical units of E_LO & E_HI arrays
c  CSYS          i   : Coordinate system in use
c  I_POS         i   : No. of positional bins
c  X             i   : Array of positions along one axis
c  Y             i   : Array of positions along other axis
c  PUNITS        i   : Physical units of X & Y arrays
c  TRANS         i   : 2-d array of transmission vs energy & position
c  IERR            o : Error status flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTPHBN     : (FITSIO) Writes to BINTABLE header keywords
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYx     : (FITSIO) Writes a keyword
c  subroutine GT_CSYSNMS : (CALLIB) Gets col/key names for coord system
c  subroutine RMVLBK     : (XANLIB) Removes leading blanks from string
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with XANLIB & FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original 
c
c Authors/Modification History:
c  Ian M George     (1993 Feb 22), original
	character(7) version
	parameter (version = '1.0.0')
*- 

c Internals
	integer i, ic, kk, jj, decimals
	integer status, maxfields, nfields, size
	parameter (maxfields=5)
	real values(1000)
	character(16) ttype(maxfields), tform(maxfields)
	character(16) tunits(maxfields)
	character(80) message
        character(8) xnam, ynam
        character(30)  errstr, wrnstr
c Initialize
	status = 0
        errstr = '** WT_FLT1992a ERROR: '
        wrnstr = '** WT_FLT1992a WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using WT_FLT1992a '// version
	     call fcecho(message)
        endif

c Create a new extension
	call FTCRHD(ounit,status)
	message = errstr
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... new extension created'
   	  call fcecho(message)
	endif
	if(status.NE.0) stop

c Get the standard column names of for the coordinate frame
        call gt_csysnms(chatter, csys,
     &          xnam, ynam, ierr)

c Check out whether the coordinate columns are really to exist
	if(ipos.eq.1) then
		nfields = 3
	else
		nfields = 5
	endif

c Set up the easy columns n stuff
	ttype(1)   = 'ENERG_LO'
        write(message,'(i12,a)') ienerg,'E'
        call rmvlbk(message)
	tform(1)   = message(1:10)
	tunits(1)  = eunits

	ttype(2)   = 'ENERG_HI'
	tform(2)   = tform(1)
	tunits(2)  = eunits

	if(nfields.eq.5) then
	  ttype(3)   = xnam
          write(message,'(i12,a)') ipos,'E'
          call rmvlbk(message)
	  tform(3)   = message(1:10)
	  tunits(3)  = punits

	  ttype(4)   = ynam
	  tform(4)   = tform(3)
	  tunits(4)  = punits
	endif

	size = ienerg*ipos
	ttype(nfields)   = 'TRANSMIS'
        write(message,'(i12,a)') size,'E'
        call rmvlbk(message)
	tform(nfields)   = message(1:10)
	tunits(nfields)  = ' '


c Write the required header keywords
	call FTPHBN(ounit,1,nfields,ttype,tform,tunits,
     &		'TRANSMISSION',0,status)
	message = errstr 
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... written the extension header keywords'
   	  call fcecho(message)
	endif
	if(status.NE.0) stop


c Define the extension data structure
	call FTBDEF(ounit,nfields,tform,0,1,status)
	message = errstr // ' Defining Data Structure '
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... defined the extension data structure'
   	  call fcecho(message)
	endif
        if(status.NE.0) stop

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

	if(detnam.EQ.' ') detnam = 'NONE'
	call FTPKYS(ounit,'DETNAM ',
     &		detnam,
     &   	'specific detector name',
     &		status)
	message = wrnstr // ' Putting DETNAM keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(ounit,'FILTER   ',
     &		filter,
     &   	'filter in use',
     &		status)
	message = wrnstr // ' Putting FILTER keyword '
	call wt_ferrmsg(status, message)
	status = 0

c Add other advised keywords
	call FTPKYS(ounit,'FLTVERSN ',
     &		'1992a',
     &   	'OGIP classification of FITS format',
     &		status)
	message = wrnstr // ' Putting FLTVERSN keyword '
	call wt_ferrmsg(status, message)
	status = 0


	if(nfields.eq.5) then
	   call FTPKYS(ounit,'CSYS3 ',
     &		csys,
     &   	'Spatial Coordinate System used (for column 3)',
     &		status)
	   call FTPKYS(ounit,'CSYS4 ',
     &		csys,
     &   	'Spatial Coordinate System used (for column 4)',
     &		status)
	else
	call FTPKYS(ounit,'CSYS ',
     &		csys,
     &   	'Spatial Coordinate System used',
     &		status)
	endif
	message = wrnstr // ' Putting CSYSnnn keyword(s) '
	call wt_ferrmsg(status, message)
	status = 0

	if(chatter.GE.20) then
	  message = ' ... written the OGIP required keywords'
   	  call fcecho(message)
	endif

c Add keywords for the spatial coordinates if neccessary
	if(nfields.eq.3) then
		if(punits(1:6).EQ.'arcmin') then
		  decimals = 6
		  call FTPKYE(ounit, xnam(1:8),
     &		  x(1), decimals,
     &   	  'spatial coordinate (removed from table)',
     &		  status)
		  call FTPKYE(ounit, ynam(1:8),
     &		  y(1), decimals,
     &   	  'spatial coordinate (removed from table)',
     &		  status)
	   	  message = wrnstr // ' Putting Coord keyword '
		  call wt_ferrmsg(status, message)
		  status = 0
		else
		   message = errstr // 
     &			' Only positions in arcmin accepted at present'
		   call fcecho(message)
		   stop
		endif
	endif

c Add the (passed) history cards, adding one related to this programme
	do i = 1, nk_hist
		call FTPHIS(ounit, hist(i), status)
	enddo
        write(message,'(2a)') 
     &		' FITS TRANSMISSION extension written by WT_FLT1992a ',
     &                  version
	call FTPHIS(ounit,message,status)
	message = wrnstr // ' Putting History records'
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... written the history keywords'
   	  call fcecho(message)
	endif
	status = 0

c Add the (passed) comment cards
	do i = 1, nk_comm
		call FTPCOM(ounit, comment(i), status)
		message = wrnstr // ' Putting comments'
		call wt_ferrmsg(status, message)
		status = 0
	enddo
	if(chatter.GE.20) then
	  message = ' ... written the comment header keywords'
   	  call fcecho(message)
	endif


c Write the data
c 	... the energy bin
	call FTPCLE(ounit, 1, 1, 1, ienerg, e_lo,status)
	call FTPCLE(ounit, 2, 1, 1, ienerg, e_hi,status)
	message = wrnstr // ' encounted writing Energy column(s) '
	call wt_ferrmsg(status, message)
	if(status.NE.0) stop

c	... the coordinate pairs
	if(nfields.eq.5) then
		call FTPCLE(ounit, 3, 1, 1, ipos, x,status)
		call FTPCLE(ounit, 4, 1, 1, ipos, y,status)
	    message = wrnstr // ' encounted writing Position column(s) '
		call wt_ferrmsg(status, message)
		if(status.NE.0) stop
	endif

c 	... the Transmission
		
	do kk = 1,size
		values(kk) = 0
	enddo
	kk = 0
	do jj =1, ipos
		  do ic = 1, ienerg
		    kk = kk + 1
		    values(kk) = trans(ic,jj)
		  enddo
	enddo
	call FTPCLE(ounit, nfields, 1, 1, size, values, status)
	message = wrnstr // ' encounted writing Transmision column'
	call wt_ferrmsg(status, message)
	if(status.NE.0) stop

	if(chatter.GE.20) then
	  message = ' ... written the data'
   	  call fcecho(message)
	endif

c Final check for errors

	return
	end
	
