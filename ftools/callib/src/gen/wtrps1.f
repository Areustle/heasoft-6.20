*+WTRPS1
      subroutine wtrps1(ounit,chatter,nk_hist,hist,nk_comm,
     &			comment,phaversn,instrume,filter,texpos,areascal,
     &			detchans,chantyp,nrad, rad_lo, rad_hi, 
     &			sohist, bohist,	ierr)

        IMPLICIT NONE
        integer ounit,chatter,nk_hist, nk_comm
	integer detchans, dtype, ierr, nrad
	real texpos,areascal
	real rad_lo(*), rad_hi(*)
	real sohist(*), bohist(*)
	character(5) phaversn
	character(16) instrume,filter
	character(16) chantyp
	character(70) hist(nk_hist), comment(nk_comm)
c
c --- DESCRIPTION -------------------------------------------------
c
c This subroutine writes a ROSAT PSPC DETECTOR extension for a PHA file in 
c PHAVERSN=1992a format.
c !!! Note !!! the o/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written DETECTOR extension on return and MUST be closed 
c              using FTCLOS or another extension written starting with FTCRHD
c	       in order that the mandatory END keyword is written              
c The 1992a format for the ROSAT PSPC consists of a BINTABLE extension, with
c the number of rows equal to the number of off-axis angles passed, and 
c the following columns/contents:
c  THET_MIN - (real) theta min for this histogram bin 
c  THET_MAX - (real) theta min for this histogram bin  
c  SOU_FRAC - (real) weighting factor for SOURCE region
c  BKG_FRAC - (real) weighting factor for BKGD region
c The following keywords are also written:
c  TELESCOP= 'ROSAT'
c  INSTRUME= Name of the instrument ('PSPC-B' or 'PSPC-C')
c  FILTER  - Name of filter in use (if any)
c  PHAVERSN='1992a'
c  EXPOSURE- Total exposure (s) for entire integration (incl deadtime etc)
c  AREASCAL- Area scaling factor for PHA dataset
c  DETCHANS- Max no. of detector channels in integration
c  CHANTYPE- Type of PHA channels in use ('PHA' or 'PI'[usual])
c
c Passed Parameters
c  OUNIT	i   : FORTRAN unit number of open PHA file
c  CHATTER      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  NK_HIST      i   : No. records to be written as HISTORY records
c  HIST         i   : Array of history record strings to be written
c  NK_COMM      i   : No. records to be written as COMMENT records
c  {list incomplete}
c                          
c --- CALLED ROUTINES ---------------------------------------------
c  {list incomplete}
c
c 
c --- AUTHORS/MODIFICATION HISTORY --------------------------------
c
c Ian M George (1.0.0:1993 Jun 02), original
c Ian M George (2.0.0:1993 Jun 20), major re-write
c Ian M George (2.1.0:1993 Aug 17), deleted unness passed params
c Lawrence E Brown (2.1.1:1995 Jan 5), removed leading blanks from column names
      character(5) version
      parameter (version = '2.1.1')
*-
c --- INTERNALS ---------------------------------------------------
c
	integer maxradii
	parameter (maxradii = 100)
      	integer nfields,status, decimals, itemp
      	parameter (nfields = 4, decimals=6)
      	integer i,key_val,vd, j, nrows
      	integer frow,felem,colnum,tfields
	real sdata(maxradii), bdata(maxradii)
	real lo_rad(maxradii), hi_rad(maxradii)
      	character(16) ttype(nfields),tform(nfields),tunit(nfields)
	character(30) errstr, wrnstr
      	character(70) subinfo

c Initialization
	ierr = 0
	status = 0
	errstr = '** WTRPS1 ERROR :'
	wrnstr = '** WTRPS1 WARNING :'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using WTRPS1 Ver '//version
         call fcecho(subinfo)
      ENDIF

	if(nrad.GT.maxradii) then
	 subinfo = errstr // 
     &		' Max dimension of Internal MAXRADII array exceeded'
	 call fcecho(subinfo)
	 ierr = 1
	 goto 998
	endif

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
	tfields = 4
      ttype(1) ='THET_MIN'
      tform(1) = 'E'
      tunit(1) = 'arcmin'
      ttype(2) ='THET_MAX'
      tform(2) = 'E'
      tunit(2) = 'arcmin'
      ttype(3) ='SOU_FRAC'
      tform(3) = 'E'
      tunit(3) = ' '
      ttype(4) ='BKG_FRAC'
      tform(4) = 'E'
      tunit(4) = ' '

c Work out how many rows we're got
	j = 0
	do i = 1, nrad
	  if(sohist(i).NE.0.0 .OR. bohist(i).NE.0.0) then
	  j = j + 1
	    lo_rad(j) = rad_lo(i)
	    hi_rad(j) = rad_hi(i)
	    sdata(j) = sohist(i)
	    bdata(j) = bohist(i)
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
     &		'ROSAT',
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
	    	
	call FTPKYS(ounit,'FILTER ',
     &		filter,
     & 		'filter in use',
     &		status)
	subinfo = wrnstr // ' Putting FILTER keyword '
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
     &		' PSPC DETECTOR extension written by WTRPS1 ',
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
      CALL ftpcle(ounit,1,1,1,nrows,lo_rad,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing RAD_LO Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,2,1,1,nrows,hi_rad,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing RAD_LO Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,3,1,1,nrows,sdata,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing RAD_LO Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,4,1,1,nrows,bdata,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing RAD_LO Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif

c ... reassure if everything OK

	if(chatter.GE.5) then
	   subinfo = ' ... written the ROSAT PSPC DETECTOR extension'
	   call fcecho(subinfo)
	endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not written'
	  call fcecho(subinfo)
	endif


      return
      end
