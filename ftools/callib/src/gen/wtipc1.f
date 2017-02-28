*+WTIPC1
      subroutine wtipc1(ounit,chatter,nk_hist,hist,nk_comm,
     &			comment,phaversn,instrume,
     &			texpos,areascal,detchans,chantyp,
     &			theta, sorad, arcfrac, bal, balhist,
     &			bal_lo, bal_hi, bal_inc, bal_step, bal_eps,
     &			bal_mean, bal_spat, ierr)

        IMPLICIT NONE
        integer ounit,chatter,nk_hist, nk_comm
	integer detchans, dtype, ierr, bal_step
	real texpos,areascal
	real theta, sorad, arcfrac
	real bal_lo, bal_hi, bal_inc, bal_mean, bal_eps
	real bal(85), balhist(85)
	character(5) phaversn
	character(16) instrume
	character(16) chantyp
	character(20) bal_spat
	character(70) hist(nk_hist), comment(nk_comm)
c Description
c   This subroutine writes an EINSTEIN IPC DETECTOR extension for a PHA file in 
c  PHAVERSN=1992a format. 
c  !!! Note !!! 
c 	 The o/p file is assumed to have been opened, and wound to the desired 
c 	 location. The file is left open at the end of the written DETECTOR 
c 	 extension on return and MUST be closed using FTCLOS or another extn
c 	 written starting with FTCRHD in order that the mandatory END keyword 
c 	 is written            
c The 1992a format for the EINSTEIN IPC consists of a BINTABLE extension, with
c the number of rows equal to the number of non-zero BAL (Bin-Of-Aluminum) 
c steps passed, and the following columns/contents:
c  BAL     - (real) the BAL step
c  BAL_FRAC- (real) the weighting factor for this BAL step
c The following keywords are also written:
c  TELESCOP= 'EINSTEIN'
c  INSTRUME= 'instrument/detector name' (ie 'IPC-1','IPC-2','IPC-3')
c  PHAVERSN='1992a'
c  EXPOSURE- Total exposure (s) for entire integration (incl deadtime etc)
c  AREASCAL- Area scaling factor for PHA dataset
c  DETCHANS- Max no. of detector channels in integration
c  CHANTYPE- Type of PHA channels in use ('PHA'[usual] or 'PI')
c  THETA   - 'Off-axis angle (decimal degrees)'
c  RADIUS  - 'Unsure what this really represents'
c  ARC_FRAC- 'Arcing fraction'
c  BAL_LO  - 'Lowest BAL possible'
c  BAL_HI  - 'Highest BAL possible'
c  BAL_INC - 'BAL step size'
c  BAL_STEP- 'Number of possible BAL steps'
c  BAL_EPS - 'BAL EPS ???????'
c  BAL_MEAN- 'Mean BAL for this dataset'
c  BAL_SPAT- 'BAL SPAT ??????'
c
c Passed Parameters
c  OUNIT	i   : FORTRAN unit number of open PHA file
c  CHATTER      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  NK_HIST      i   : No. records to be written as HISTORY records
c  HIST         i   : Array of history record strings to be written
c  NK_COMM      i   : No. records to be written as COMMENT records
c  COMMENT      i   : Array of comment cards to be written
c  INSTRUME     i   : Name of detector/instrument
c  TEXPOS       i   : Exposure time (in secs), corrected for deadtime etc
c  AREASCAL     i   : Area scaling factor
c  DETCHANS     i   : No of max poss detector channels 
c  CHANTYP      i   : Type of channels (PI, PHA etc)
c  THETA        i   : Mean off-axis angle (in decimal degrees)
c  SORAD        i   : ?????
c  ARCFRAC      i   : Arcing Fraction
c  BAL          i   : Array containing the BAL steps
c  BALHIST      i   : Array containing the BAL weighting factors
c  BAL_LO       i   : Lowest BAL possible
c  BAL_HI       i   : Highest BAL possible
c  BAL_INC      i   : BAL step size (in BAL 'units')
c  BAL_STEP     i   : Total no. of BAL steps possible
c  BAL_EPS      i   : ????
c  BAL_MEAN     i   : Mean BAL for observation
c  BAL_SPAT     i   : ?????
c  IERR           o : Error flag of return (zero is OK)
c
c Called Routines
c
c subroutine FTPCLn	 : (FITSIO)
c subroutine FTPHBN	 : (FITSIO)
c subroutine FTPCOM	 : (FITSIO)
c subroutine FTPHIS	 : (FITSIO)
c subroutine FTPKYn	 : (FITSIO)
c subroutine FCECHO	 : (FTOOLS)
c subroutine FTBDEF	 : (FITSIO)	
c subroutine FTCRHD	 : (FITSIO)
c subroutine WT_FERRMSG  : (CALLIB)
c 
c Authors/Modification History 
c Ian M George (1.0.0; 1993 Jun 20), original
c Ian M George (1.1.0; 1993 Aug 17), deleted unness passed parameters
c Ian M George (1.2.0; 1993 Oct 18), added HDUCLAS stuff
      character(5) version
      parameter (version = '1.2.0')
*-
c Internals 
c
      	integer nfields,status, decimals, itemp
      	parameter (nfields = 2, decimals=6)
      	integer i,key_val,vd, j, nrows
      	integer frow,felem,colnum,tfields
	real dum1(85), dum2(85)
      	character(16) ttype(nfields),tform(nfields),tunit(nfields)
	character(40) errstr, wrnstr
      	character(70) subinfo

c Initialization
	ierr = 0
	status = 0
	errstr = '** WTIPC1 '//version//' ERROR :'
	wrnstr = '** WTIPC1 '//version//' WARNING :'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using WTIPC1 Ver '//version
         call fcecho(subinfo)
      ENDIF

c
c --- SETUP DATA EXTENSION ---
c
c Create data extension

      call ftcrhd(ounit,status)
	subinfo = errstr
	call wt_ferrmsg(status,subinfo)
	if(status.ne.0) then
		ierr = 1
		goto 998
	endif
	if(chatter.ge.20) then
	  subinfo = ' ... new extension created'
	  call fcecho(subinfo)
	endif	

c Setup header keywords
	tfields = 2
      ttype(1) =' BAL'
      tform(1) = 'E'
      tunit(1) = ' '
      ttype(2) =' BAL_FRAC'
      tform(2) = 'E'
      tunit(2) = ' '

c Work out how many rows we're got
	j = 0
	do i = 1, 85
	  if(balhist(i).NE.0.0)then
	  j = j + 1
	    dum1(j) = bal(i)
	    dum2(j) = balhist(i)
	  endif
	enddo
	nrows = j

c --- WRITE THE MAIN HEADER KEYWORDS ---

      status = 0
      vd = 0
      CALL ftphbn(ounit, nrows, tfields, ttype, tform, tunit,
     &            'DETECTOR', vd, status)     
	subinfo = errstr
	call wt_ferrmsg(status,subinfo)
	if(status.NE.0) then
	   ierr = 1
	   goto 998
	endif
	if(chatter.ge.20) then
	  subinfo = ' ... written the extension header keywords'
          call fcecho(subinfo)
	endif	

c
c --- WRITE THE HDUCLASn & HDUVERSn keywords
c
	call FTPKYS(ounit,'HDUCLASS ',
     &		'OGIP',
     & 		'format conforms to OGIP standard',
     &		status)
	subinfo = wrnstr // ' Putting HDUCLASS keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'HDUCLAS1 ',
     &		'SPECTRUM',
     & 		'PHA dataset (OGIP memo OGIP/92-007)',
     &		status)
	subinfo = wrnstr // ' Putting HDUCLAS1 keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'HDUVERS1 ',
     &		phaversn,
     & 		'Version of format (OGIP memo OGIP/92-007a)',
     &		status)
	subinfo = wrnstr // ' Putting HDUVERS1 keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'HDUCLAS2 ',
     &		'DETECTOR',
     & 		'Detector extension to PHA dataset',
     &		status)
	subinfo = wrnstr // ' Putting HDUCLAS2 keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'HDUVERS2 ',
     &		'1.0.0',
     & 		'Version of format (OGIP memo OGIP/93-024))',
     &		status)
	subinfo = wrnstr // ' Putting HDUVERS2 keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0


c
c --- WRITE ADDITIONAL KEYWORDS DESCRIBING DATA
c
	call FTPKYS(ounit,'TELESCOP ',
     &		'EINSTEIN',
     & 		'mission/satellite name',
     &		status)
	subinfo = wrnstr // ' Putting TELESCOP keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'INSTRUME ',
     &		instrume,
     & 		'instrument/detector name',
     &		status)
	subinfo = wrnstr // ' Putting INSTRUME keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0
	    	
	call FTPKYF(ounit,'EXPOSURE ',
     &		texpos,decimals,
     & 		'exposure/live-time (in seconds)',
     &		status)
	subinfo = wrnstr // ' Putting EXPOSURE keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'AREASCAL ',
     &		areascal,decimals,
     & 		'area scaling factor',
     &		status)
	subinfo = wrnstr // ' Putting AREASCAL keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0


	call FTPKYS(ounit,'PHAVERSN ',
     &		'1992a',
     & 		'OGIP classification of FITS format',
     &		status)
	subinfo = wrnstr // ' Putting PHAVERSN keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYJ(ounit,'DETCHANS ',
     &		detchans,
     & 		'total number possible channels',
     &		status)
	subinfo = wrnstr // ' Putting DETCHANS keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'CHANTYPE ',
     &		chantyp,
     & 		'channel type (PHA, PI etc)',
     &		status)
	subinfo = wrnstr // ' Putting CHANTYPE keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0


	call FTPKYF(ounit,'THETA ',
     &		theta,decimals,
     & 		'Off-axis angle (decimal degrees)',
     &		status)
	subinfo = wrnstr // ' Putting THETA keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'RADIUS ',
     &		sorad,decimals,
     & 		'Unsure what this really represents',
     &		status)
	subinfo = wrnstr // ' Putting RADIUS keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'ARC_FRAC ',
     &		arcfrac,decimals,
     & 		'Arcing fraction',
     &		status)
	subinfo = wrnstr // ' Putting ARC_FRAC keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'BAL_LO ',
     &		bal_lo,decimals,
     & 		'Lowest BAL possible',
     &		status)
	subinfo = wrnstr // ' Putting BAL_LO keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'BAL_HI ',
     &		bal_hi,decimals,
     & 		'Highest BAL possible',
     &		status)
	subinfo = wrnstr // ' Putting BAL_HI keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'BAL_INC ',
     &		bal_inc,decimals,
     & 		'BAL step size',
     &		status)
	subinfo = wrnstr // ' Putting BAL_INC keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0


	call FTPKYJ(ounit,'BAL_STEP ',
     &		bal_step,
     & 		'Number of possible BAL steps',
     &		status)
	subinfo = wrnstr // ' Putting BAL_STEP keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'BAL_EPS ',
     &		bal_eps,decimals,
     & 		'BAL EPS ???????',
     &		status)
	subinfo = wrnstr // ' Putting BAL_EPS keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'BAL_MEAN ',
     &		bal_mean,decimals,
     & 		'Mean BAL for this dataset',
     &		status)
	subinfo = wrnstr // ' Putting BAL_MEAN keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'BAL_SPAT ',
     &		bal_spat,
     & 		'BAL SPAT ??????',
     &		status)
	subinfo = wrnstr // ' Putting BAL_SPAT keyword '
	call wt_ferrmsg(status, subinfo)
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
        write(subinfo,'(2a)')
     &		' IPC DETECTOR extension written by WTIPC1 ',
     &                          version
        call FTPHIS(ounit,subinfo,status)
        subinfo = wrnstr // ' Putting at least one History record'
        call wt_ferrmsg(itemp, subinfo)
        if(chatter.GE.20) then
          subinfo = ' ... written the history keywords'
          call fcecho(subinfo)
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
        subinfo = wrnstr // ' Putting at least one Comment record'
        call wt_ferrmsg(itemp, subinfo)
        status = 0
        if(chatter.GE.20) then
          subinfo = ' ... written the comment header keywords'
          call fcecho(subinfo)
        endif

c
c --- DEFINE DATA ---
c
      status = 0
      vd = 0
      call ftbdef(ounit,tfields,tform,vd,nrows,status)
c
c --- WRITE THE ELEMENTS INTO TABLE ---
c
      status = 0
      CALL ftpcle(ounit,1,1,1,nrows,dum1,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing BAL Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,2,1,1,nrows,dum2,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing BAL_FRAC Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif

c ... reassure if everything OK

	if(chatter.GE.5) then
	   subinfo = ' ... written the EINSTEIN IPC DETECTOR extension'
	   call fcecho(subinfo)
	endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not written'
	  call fcecho(subinfo)
	endif


      return
      end
