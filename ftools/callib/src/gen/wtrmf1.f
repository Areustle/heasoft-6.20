*+WTRMF1
        subroutine wtrmf1(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment,rmfversn, hduclas3,
     &          telescop, instrume, detnam, filter, areascal,
     &          maxchan, ichan, maxen, ienerg, energ_lo, energ_hi,
     &          imaxgrp, ngrp, F_chan, N_chan,
     &          fmatrix, lo_thresh, ierr)

	IMPLICIT NONE
	integer chatter, ierr,maxchan,maxen
	integer ounit, nk_hist, nk_comm
	integer ichan, ienerg, imaxgrp
	integer ngrp(maxen), F_chan(maxen,*)
	integer N_chan(maxen,*)
	real areascal, lo_thresh
	real energ_lo(maxen), energ_hi(maxen)
	real fmatrix(maxchan,maxen)
	character*(*) rmfversn
	character*(*) telescop, instrume, detnam, filter
	character*(*) hduclas3
	character*(*) hist(*), comment(*)
c 
c Description:
c  Creates and Writes the RMF extension for an RMF file one of the formats 
c  conforming to the HDUVERS2='1.*.*' family.
c Currently the following formats are supported (see OGIP/92-002a)
c   HDUVERS2 = '1.0.0'
c   HDUCERS2 = '1.1.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format listed.
c  Assumes the FITS file is open and has had the Primary Header written
c  !!! Note !!!! File is left open at the end  
c      and  MUST BE CLOSED               by FTCLOS 
c      or   ANOTHER EXTENSION ADDED      by FTCRHD
c  in order to (automatically) write the mandatory END header keyword.
c  For The matrix will be written as a VARIABLE LENGTH ARRAY if the reduction 
c  in storage requirements (ie total number of stroed values) exceeds a 
c  factor 3.0 over that obtained using a FIXED length array.
c
c Passed parameters
c  OUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NK_HIST       i   : No. records to be written as HISTORY records
c  HIST          i   : Array of history strings to be written
c  NK_COMM       i   : No. records to be written as COMMENT records
c  COMMENT       i   : Array of comment strings to be written
c  RMFVERSN      i   : String denoting OGIP HDUVERS2 family
c  HDUCLAS3      i   : String containing HDUCLAS3 value
c  TELESCOP      i   : String listing telescope/mission
c  INSTRUME      i   : String listing instrument/detector
c  DETNAM        i   : String listing specific detector name   
c  FILTER        i   : String listing instrument filter in use
c  AREASCAL      i   : Area scaling factor
c  MAXCHAN       i   : Maximum Channel index array dimension
c  ICHAN         i   : No. channels in the full array
c  MAXEN         i   : Maximum Energy index array dimension
c  IENERG        i   : No. energy bins
c  ENERG_LO      i   : Array containing lower bound to each energy bin
c  ENERG_HI      i   : Array containing upper bound to each energy bin
c  NGRP          i   : Array containing no. channel subsets at each energy
c  IMAXGRP       i   : Max no. grps in any given row
c  F_CHAN        i   : Array containing 1st chan of each subset at each energy
c  N_CHAN        i   : Array containing no. chans within each subset 
c                           at each energy
c  FMATRIX       i   : Array containing the full matrix
c  LO_THRESH     i   : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine CRMVBLK    : (CALLIB) Removes blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine FTBDEF     : (FITSIO) Defines the BINTABLE data structure
c  subroutine FTCRHD     : (FITSIO) Creates a new FITS extension file
c  subroutine FTPHBN     : (FITSIO) Writes the required header keywords
c  subroutine FTPCOM     : (FITSIO) Writes a FITS comment keyword
c  subroutine FTPCLx     : (FITSIO) Writes the data 
c  subroutine FTPHIS     : (FITSIO) Writes a FITS history keyword
c  subroutine FTPKYS     : (FITSIO) Writes a keyword
c  subroutine WT_FERRMSG : (CALLIB) Writes FITSIO error message etc
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Code mostly hacked from within the BBRSP program
c
c Authors/Modification History:
c  Alan Smale       (1992 Sept/Oct), original BBRSP version
c  Ian M George     (1.0.1; 1992 Dec 29), tidied-up version
c  Ian M George     (1.0.2; 1993 Feb 28), minor debugging of History records
c  Ian M George     (1.0.3; 1993 May 19), hist & comment made chara*70
c  Rehana Yusaf     (1.0.4; 1993 July 27), array dimensions changed from
c                                         hard coded to using new arguments
c                                         MAXCHAN and MAXEN
c  Ian M George     (1.1.0; 1993 Jul 30), cosmetics
c  Ian M George     (2.0.0; 1993 Sep 03), added variable length arrays
c  Ian M George     (2.1.0; 1993 Oct 12), added HDUCLASn stuff 
c  Ian M George     (3.0.0: 1993 Oct 13), renamed from wt_rmf1992a & major
c                                       overhaul of HDUCLAS/VERS stuff
c  Ian M George     (3.0.1: 1993 Dec 01), xtra checks before var length array
c  Ian M George     (3.1.0: 1994 Jan 24), added varidat for var length arrays
c  Ian M George     (3.2.0: 1994 Jun 27), made passed chars free form
	character(7) version
	parameter (version = '3.2.0')
*- 

c Internals
	integer status, decimals, itemp
        integer nfields, nvar, sum, nfixed
	integer i, kk, siz_mat, siz_ngrp
	integer ie,jj, ic, varidat
        parameter (nfields=6, decimals=6)
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(5) hduvers2
        character(70) string
	character(255) wrtstr
	character(80) message
        character(40)  errstr, wrnstr
	integer ivalues(10)
	real values(20000)
	logical qvar
c Initialize
	ierr = 0
	status = 0
	qvar = .false.
        errstr = '** WTRMF1 '//version//' ERROR: '
        wrnstr = '** WTRMF1 '//version//' WARNING: '
	

c Give user info if requested
        if(chatter.GE.20) then
             message = ' ... using WTRMF1 '// version
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
	   message = wrnstr // ' Unknown format: '// rmfversn
	   call fcecho(message)
	   message = 
     &	      ' ...... Resetting format (HDUVERS2) to '//hduvers2
	   call fcecho(message)
	endif
c



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
 
c Calculate the necessary dimensions of the arrays (columns)
	siz_ngrp = 0
	siz_mat = 0
	nvar = 0
	do ie = 1, ienerg
	   siz_ngrp = MAX(siz_ngrp,Ngrp(ie))
	   sum = 0
	   do jj = 1, Ngrp(ie)
	      sum = sum + N_chan(ie,jj)
		nvar = nvar + sum 
	      siz_mat = MAX(siz_mat,sum)
	   enddo
	enddo
c Decide whether a variable length array for the matrix column makes 
c sense for the matrix column
c ... no. elements stored in the fixed length case
	nfixed = siz_mat * ienerg
	if((siz_mat.GT.3) .AND. (nvar .NE. nfixed) .AND.
     &		(nvar .LE. 3* nfixed)) then
		qvar = .true.
		varidat = 4*nvar
	else
		qvar = .false.
		varidat = 0
	endif

c Dump to user if requested
	if(chatter.GE.20) then
	  write(message,'(a,i12)') 
     &		' ... Max # elements in F_CHAN & N_CHAN arrays = ',
     &		siz_ngrp
	  call fcecho(message)
	  write(message,'(a,i12)') 
     &	  	' ... Max # elements in MATRIX array = ', siz_mat
	  call fcecho(message)
	  if(qvar) then
		message = ' ... Using Variable length array '//
     &			'for MATRIX column'
	  else
		message = ' ... Using Fixed length array '//
     &			'for MATRIX column'
	  endif
	  call fcecho(message)
	endif

c Set up the columns n stuff
	ttype(1)   = 'ENERG_LO'
	tform(1)   = 'E'
	tunits(1)  = 'keV'
	ttype(2)   = 'ENERG_HI'
	tform(2)   = 'E'
	tunits(2)  = 'keV'
	ttype(3)   = 'N_GRP'
	tform(3)   = 'I'
	tunits(3)  = ' '
	ttype(4)   = 'F_CHAN'
        write(wrtstr,'(i12,a)') siz_ngrp,'I'
        call crmvlbk(wrtstr)
	tform(4)   = wrtstr(1:10)
	tunits(4)  = ' '
	ttype(5)   = 'N_CHAN'
	tform(5)   = tform(4)
	tunits(5)  = ' '
	ttype(6)   = 'MATRIX'
	if(qvar) then
	   wrtstr = 'PE'
	else
           write(wrtstr,'(i12,a)') siz_mat,'E'
           call crmvlbk(wrtstr)
	endif
	tform(6)   = wrtstr(1:10)
	tunits(6)  = ' '

c Write the required header keywords
	if(hduclas3.EQ.'REDIST') then
		string = 'MATRIX'
	else
		string = 'SPECRESP MATRIX'
	endif
	call FTPHBN(ounit,ienerg,nfields,ttype,tform,tunits,
     &		string,varidat,status)
	message = errstr 
	call wt_ferrmsg(status, message)
	if(chatter.GE.20) then
	  message = ' ... written the extension header keywords'
   	  call fcecho(message)
	endif
        if(status.NE.0) then
                ierr = 1
                goto 998
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
     &		'RSP_MATRIX',
     & 		'dataset is a spectral response matrix',
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

	if(hduclas3.EQ.'REDIST') then
		string = 'photon redistribution matrix (only)'
	elseif(hduclas3.EQ.'FULL') then
		string = 'convolved w/ all effects (det + optics)'
	elseif(hduclas3.EQ.'DETECTOR') then
		string = 'convolved w/ detector effects (only)'
	else
		string = 'WARNING This is NOT an OGIP-approved value'
	endif
	call FTPKYS(ounit,'HDUCLAS3 ',
     &		hduclas3,
     & 		string,
     &		status)
	message = wrnstr // ' Problem putting HDUCLAS3 keyword '
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
     &		detnam,
     &   	'specific detector name in use',
     &		status)
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
     &		ichan,
     &   	'total number of detector channels',
     &		status)
	message = wrnstr // ' Putting DETCHANS keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'LO_THRESH ',
     &		lo_thresh,decimals,
     &   	'lower threshold for stored matrix',
     &		status)
	message = wrnstr // ' Putting LO_THRESH keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'EFFAREA ',
     &		areascal, decimals,
     &   	'Area scaling factor',
     &		status)
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


c Add the (passed) history cards, adding one related to this programme
	itemp = 0
	do i = 1, nk_hist
		call FTPHIS(ounit, hist(i), status)
		if(status.NE.0) then
			itemp = status
			status = 0
                        call FTPHIS(ounit,
     &		' - (missing record) fitsio illegal character ?',
     &		 status)
		endif
	enddo
        write(string,'(2a)') 
     &			' FITS RMF extension written by WTRMF1 ',
     &                   version
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
	call FTBDEF(ounit,nfields,tform,varidat,ienerg,status)
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
	do ie = 1, ienerg

c 		... the energy bin
		call FTPCLE(ounit, 1, ie, 1, 1, energ_lo(ie),status)
		call FTPCLE(ounit, 2, ie, 1, 1, energ_hi(ie),status)

c		... the Grping info
		call FTPCLJ(ounit, 3, ie, 1, 1, Ngrp(ie),status)
		do jj=1,Ngrp(ie)
			ivalues(jj) = F_chan(ie,jj)
		enddo
		call FTPCLJ(ounit, 4, ie, 1, Ngrp(ie),ivalues,status)

		do jj=1,Ngrp(ie)
			ivalues(jj) = N_chan(ie,jj)
		enddo
		call FTPCLJ(ounit, 5, ie, 1, Ngrp(ie),ivalues,status)

c		... the matrix
		do kk = 1,siz_mat
			values(kk) = 0
		enddo
		kk = 0
		do jj =1, Ngrp(ie)
		  do ic = F_chan(ie,jj), F_chan(ie,jj) + N_chan(ie,jj) - 1
		    kk = kk + 1
		    values(kk) = fmatrix(ic,ie)
		  enddo
		enddo
	if(qvar) then
		call FTPCLE(ounit, 6, ie, 1, kk, values, status)
	else
		call FTPCLE(ounit, 6, ie, 1, siz_mat, values, status)
	endif
	enddo

	if(chatter.GE.20) then
	  message = ' ... written the data'
   	  call fcecho(message)
	endif

c Final check for errors
	message = wrnstr // ' Writing Data '
	call wt_ferrmsg(status, message)

998	if(ierr.NE.0) then
	   message = errstr // ' FATAL: Extension not written'
	   call fcecho(message)
	endif

	return
	end
	
