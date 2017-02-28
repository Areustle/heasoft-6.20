*+WTXME1
      subroutine wtxme1(ounit,chatter,nk_hist,hist,nk_comm,
     &			comment, phaversn, texpos,areascal,detchans,
     &			chantyp,h1_off, h2_off,shfstart,shfstop,
     &			ndet,detn, dcol, dare, dexp,dagn,dgco, drco,
     &			ierr)

        IMPLICIT NONE
        integer ounit,chatter,nk_hist, nk_comm
	integer detchans, dtype, ierr, ndet
	real texpos,areascal, h1_off, h2_off
	real dcol(8), dare(8), dexp(8), dagn(8)
	real dgco(8,4), drco(8,8)
	real shfstart, shfstop
	character(5) phaversn
	character(20) detn(8)
	character(16) chantyp
	character(70) hist(nk_hist), comment(nk_comm)
c
c DESCRIPTION 
c   This subroutine writes an EXOSAT ME DETECTOR extension for a PHA file in
c one of the formats conforming to the HDUCLAS1='1.*.*' family.
c Currently the following formats are supported (see OGIP/93-024)
c   HDUCLAS1 = '1.0.0'
c   HDUCLAS1 = '1.1.0'
c The requested format is checked, and if belonging to the '1.*.*' family,
c but not included above, the extension is written in the last format 
c listed.
c !!! Note !!! the o/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written DETECTOR extension on return and MUST be closed 
c              using FTCLOS or another extension written starting with FTCRHD
c	       in order that the mandatory END keyword is written              
c In all cases the 1.*.* family of formats for the EXOSAT ME consists of 
c a BINTABLE extension, with the number of rows equal to the number of 
c sub-detectors included in the accumulation, and the following 
c columns/contents:
c  DETNAM  - (16 char) name of the sub-detector ('DET-A AR'etc)
c  OBSFACT - (real) Obscuration factor (coll resp) for sub-detector
c  GEOAREA - (real) On-axis geometric area for sub-detector
c  ONTIME  - (real) Exposure time for sub-detector (EXCL deadtime etc)
c  DETGAIN - (real) Analogue gain setting for sub-detector
c  GNCOEFF - (real) vector containing 4 gain coeffs for sub-detector
c  RSCOEFF - (real) vector containing 8 resolution coeffs for sub-detector
c The following keywords are also written:
c  HDUCLASS='OGIP'-indicating the format conforms to OGIP standards
c  HDUCLAS1='SPECTRUM' - indicating major class in the heirarchy
c  HDUVERS1- (passed) The phaversn to be written (must in '1.*.*' family)
c  HDUCLAS2='DETECTOR' - indicating this is an instr-specific detector xtens
c  TELESCOP= 'EXOSAT'
c  INSTRUME= 'ME'
c  PHAVERSN='1992a'
c  EXPOSURE  - Total exposure (s) for entire integration (incl deadtime etc)
c  AREASCAL  - Area scaling factor for PHA dataset
c  DETCHANS  - Max no. of detector channels in integration
c  CHANTYPE  - Type of PHA channels in use ('PHA'[usual] or 'PI')
c  HALF1OFF  - Mean Off-set angle for ME Half-1 during integration
c  HALF2OFF  - Mean Off-set angle for ME Half-2 during integration
c  SHFSTART  - SHF key for start of observation
c  SHFSTOP   - SHF key for end of observation
c
c PASSED VARAIABLES
c  OUNIT	i   : FORTRAN unit number of open PHA file
c  CHATTER      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  NK_HIST      i   : No. records to be written as HISTORY records
c  HIST         i   : Array of history record strings to be written
c  NK_COMM      i   : No. records to be written as COMMENT records
c  COMMENT      i   : Array of comment record strings to be written
c  PHAVERSN     i   : String denoting OGIP HDUCLAS2 family 
c  TEXPOS       i   : Exposure time, in seconds (corrected for deadtime etc)
c  AREASCAL     i   : Area scaling factor
c  DETCHANS     i   : Max number of detector channels 
c  CHATYPE      i   : Type of PHA channels ('PHA', 'PI' etc)
c  NDET         i   : Number ME detectors included in this PHA integration
c  H1_OFF  	i   : Mean H1 off-set angle (degrees) during integration
c  H2_OFF  	i   : Mean H2 off-set angle (degrees) during integration
c  SHFSTART     i   : SHF key for start of observation
c  SHFSTOP      i   : SHF key for end of observation
c  DETN         i   : Array of names of detectors included
c  DCOL         i   : Array of collimator response for each incld detector 
c  DARE         i   : Array of assumed onaxis area of each incld detector 
c  DEXP         i   : Array of ontime (active integrat time) for each incld det
c  DAGN         i   : Array of analogue gain setting for each incld detector
c  DGCO         i   : Array of gain coeffs for each incld detector
c  DRCO         i   : Array of resolution coeffs for each incld detector 
c  IERR           o : Return Errror flag (0 = OK)
c                          
c CALLED ROUTINES etc 
c subroutine FCECHO	: (FTOOLS) writes to standard o/p
c subroutine FTBDEF	: (FITSIO) defines FITS header
c subroutine FTCRHD	: (FITSIO) creates new extension
c subroutine FTPCLE	: (FITSIO) writes FITS column
c subroutine FTPCOM	: (FITSIO) writes COMMENT keyword
c subroutine FTPHBN	: (FITSIO) writes mandatory header keywords
c subroutine FTPHIS	: (FITSIO) writes HISTORY keyword
c subroutine FTPKYn	: (FITSIO) writes keyword of type n
c subroutine WT_FERRMSG : (CALLIB) writes standard error messages
c 
c AUTHORS/MODIFICATION HISTORY 
c
c Ian M George (1993 Aug 16), original
c Ian M George (1.0.1; 1993 Sept 02), added SHFSTART & SHFSTOP
c Ian M George (1.1.0; 1993 Oct 08) add HDUCLASn stuff
c Ian M George (2.0.0; 1993 Oct 12) renamed from wt_exome1992a & major
c                                       overhaul of HDUCLAS/VERS stuff 
      character(5) version
      parameter (version = '2.0.0')
*-
c INTERNALS 
c
      	integer nfields,status, decimals, itemp
      	parameter (nfields = 7, decimals=6)
	real values(8)
      	integer i,key_val,vd, j
      	integer frow,felem,colnum,tfields
	character(5) hduvers1
      	character(16) ttype(nfields),tform(nfields),tunit(nfields)
	character(30) errstr, wrnstr
      	character(70) subinfo

c Initialization
	ierr = 0
	status = 0
	errstr = '** WTXME1 ERROR :'
	wrnstr = '** WTXME1 WARNING :'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using WTXME1 Ver '//version
         call fcecho(subinfo)
      ENDIF


c Check for sillies
	if(phaversn(1:1).NE.'1') then
	   subinfo = wrnstr // ' Format/subroutine mismatch'
	   call fcecho(subinfo)
	   subinfo = 
     &		' ...... This routine writes only the 1.*.* family' //
     &		' of formats'
	   call fcecho(subinfo)
	   subinfo = 
     &		' ...... requested format: '// phaversn
	   call fcecho(subinfo)
	   ierr = 15
	   goto 998
	endif
c Check that we know the format
	if(phaversn.EQ.'1.0.0'.OR.phaversn.EQ.'1.1.0') then
	   hduvers1 = phaversn
	else
	   hduvers1 = '1.1.0'
	   subinfo = wrnstr // ' Unknown format: '// phaversn
	   call fcecho(subinfo)
	   subinfo = 
     &	      ' ...... Resetting format (HDUVERS1) to '//hduvers1
	   call fcecho(subinfo)
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
	tfields = 7
      ttype(1) ='DETNAM'
      tform(1) = '16A'
      ttype(2) ='OBSFACT'
      tform(2) = 'E'
      ttype(3) ='GEOAREA'
      tunit(3) =' cm**2  '
      tform(3) = 'E'
      ttype(4) ='ONTIME'
      tform(4) = 'E'
      tunit(4) =' s      '
      ttype(5) ='DETGAIN'
      tform(5) = 'E'
      ttype(6) ='GNCOEFF'
      tform(6) = '4E'
      ttype(7) ='RSCOEFF'
      tform(7) = '8E'

c --- WRITE THE MAIN HEADER KEYWORDS ---

      status = 0
      vd = 0
      CALL ftphbn(ounit, ndet, tfields, ttype, tform, tunit,
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
     &		hduvers1,
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
     &		'EXOSAT',
     & 		'mission/satellite name',
     &		status)
	subinfo = wrnstr // ' Putting TELESCOP keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYS(ounit,'INSTRUME ',
     &		'ME',
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

	call FTPKYF(ounit,'HALF1OFF ',
     &		h1_off,decimals,
     &		'Mean H1 off-set angle (degrees)',
     &		status)
	subinfo = wrnstr // ' Putting HALF1OFF keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'HALF2OFF ',
     &		h2_off,decimals,
     &		'Mean H2 off-set angle (degrees)',
     &		status)
	subinfo = wrnstr // ' Putting HALF2OFF keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0


	call FTPKYF(ounit,'SHFSTART ',
     &		shfstart,decimals,
     &		'SHF key for observation start',
     &		status)
	subinfo = wrnstr // ' Putting SHFSTART keyword '
	call wt_ferrmsg(status, subinfo)
	status = 0

	call FTPKYF(ounit,'SHFSTOP ',
     &		shfstop,decimals,
     &		'SHF key for observation end',
     &		status)
	subinfo = wrnstr // ' Putting SHFSTOP keyword '
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
     &		' EXOSAT ME DETECTOR extension written by WTXME1 ',
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
      call ftbdef(ounit,tfields,tform,vd,ndet,status)
c
c --- WRITE THE ELEMENTS INTO TABLE ---
c
      status = 0
      CALL ftpcls(ounit,1,1,1,ndet,detn,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing DETNAM Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,2,1,1,ndet,dcol,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing OBSFACT Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,3,1,1,ndet,dare,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing GEOAREA Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif
      CALL ftpcle(ounit,4,1,1,ndet,dexp,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing ONTIME Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif

      CALL ftpcle(ounit,5,1,1,ndet,dagn,status)
      	if(status.NE.0) then
		subinfo = wrnstr // ' Writing DETGAIN Data'
		call wt_ferrmsg(status, subinfo)
		goto 998
      	endif

	do i = 1, ndet
	   do j = 1, 4
                    values(j) = dgco(i,j)
	   enddo
           call FTPCLE(ounit, 6, i, 1, 4, values, status)
	   do j = 1, 8
                    values(j) = drco(i,j)
	   enddo
           call FTPCLE(ounit, 7, i, 1, 8, values, status)
        enddo
  

c ... reassure if everything OK

	if(chatter.GE.5) then
	   subinfo = ' ... written the EXOSAT ME DETECTOR extension'
	   call fcecho(subinfo)
	endif

998	if(ierr.NE.0) then
	  subinfo = errstr // 'FATAL: Extension not written'
	  call fcecho(subinfo)
	endif

      return
      end
