*+RDRMF3
        subroutine rdrmf3(iunit, chatter,matext,
     &          telescop, instrume, detnam, filter, areascal,
     &          chantype, flchan, 
     &          ichan, ienerg, energ_lo, energ_hi,
     &          imaxgrp, ngrp, F_chan, N_chan,
     &          fmatrix, lo_thresh, maxchan,maxen,
     &          rmfversn,hduclas3,ierr)

	IMPLICIT NONE
	integer chatter, ierr, maxchan,maxen
	integer iunit, flchan
	integer ichan, ienerg, imaxgrp
	integer ngrp(maxen), F_chan(maxen,*)
	integer N_chan(maxen,*)
	real areascal, lo_thresh
	real energ_lo(maxen), energ_hi(maxen)
	real fmatrix(maxchan,maxen)
        character*(*) chantype
        character*(*) rmfversn
        character*(*) hduclas3
	character*(*) telescop, instrume, detnam, filter
        character*(*) matext

c --- DESCRIPTION -----------------------------------------------------
c
c Reads the RMF extension for an RMFVERSN = 1992a RMF file
c The file is assumed to conform to the HDUVERS2='1.*.*' family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c HDUVERS2 = '1.2.0'
c see OGIP/92-002a
c The HDU CLASS keywords have only been currently introduced thus DO NOT
c have to be present to use this reader.
c
c Assumes the FITS file is open.
c !!! NOTE !!! 
c     - The format definitions prior to HDUVERS2 = '1.2.0' did not require 
c       TLMIN/MAX keywords for the F_CHAN column. This leads to an incorrect
c       indexing for the fmatrix array for detectors whose 1st channel is 
c       numbered zero. This code will be able to correct for this bug for 
c       matrices written using formats prior to HDUVERS2 = '1.2.0' only if 
c       there happens to be a row in the BINTABLE which includes F_CHAN = 0.
c       This is often the case, but cannot be guarenteed.
c     - File is left open at end
c       ... close file using FTCLOS, or
c       ... read another extension
c
c Columns read are ...
c  ENERG_LO      : Low energy bound for row
c  ENERG_HI      : High energy bound for row
c  N_GRP         : Number of channel subsets for row
c  F_CHAN        : Firstchannel in each subset for row
c  N_CHAN        : Number of channels for each subset for row
c  MATRIX        : (non-zero) Matrix element for row
c Keywords read ...
c  TELESCOP      : Mission/Telescop name , if not present set to UNKNOWN
c  INSTRUME      : Instrument/Detector name, if not present set to UNKNOWN 
c  DETNAME       : Specific detector name, if not present set to NONE 
c  FILTER        : Filter in use, if not present set to NONE
c  TLMIN/MAX	 : (for F_CHAN col) for legal min & max for channel numbering
c  EFFAREA       : Effective area, if not present,set to 1
c  CHANTYPE      : Type of detector channel in use (PHA, PI)
c  LO_THRES      : Threshold used to construct the MATRIX, if not present,
c                  set to 0
c  HDUCLAS3      : Values describing data, OGIP approved - REDIST,FULL,
c		   DETECTOR
c  RMFVERSN      : OGIP RMF version
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MATEXT          o : extension name SPECRESP/MATRIX
c  TELESCOP        o : String listing telescope/mission
c  INSTRUME        o : String listing instrument/detector
c  DETNAM          o : String listing specific detector name   
c  FILTER          o : String listing instrument filter in use
c  AREASCAL        o : Area scaling factor
c  CHANTYPE        o : Type of detector channel in use
c  FLCHAN          o : Lowest legal channel for this detector
c  RMFVERSN        o : RMF version
c  HDUCLAS3        o : Hduclas3 keyword value
c  ICHAN           o : No. channels in the full array
c  IENERG          o : No. energy bins
c  ENERG_LO        o : Array containing lower bound to each energy bin
c  ENERG_HI        o : Array containing upper bound to each energy bin
c  NGRP            o : Array containing no. channel subsets at each energy
c  IMAXGRP         o : Max no. grps in any given row
c  F_CHAN          o : Array containing 1st chan of each subset at each energy
c  N_CHAN          o : Array containing no. chans within each subset 
c                           at each energy
c  FMATRIX         o : Array containing the full matrix
c  LO_THRESH       o : The lower threshold used to construct the matrix
c  IERR            o : Error flag (0 = OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FTGCNO            :(FITSIO) returns table column number
c  subroutine FTGCV*            :(FITSIO) reads vector column
c  subroutine FTGKY*            :(FITSIO) reads individual kywds from header
c  subroutine WTERRM            :(CALLIB) writes error message to STDOUT
c  subroutine WTFERR            :(CALLIB) writes error msg (+fitsio) to STDOUT
c  subroutine WTFWRN            :(CALLIB) writes warning msg (+fitsio) to STDOUT
c  subroutine WTINFO            :(CALLIB) writes info message to STDOUT
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c PARENT Routine (wtrmf1.f) history
c 	Rehana Yusaf (1.0.0:93 Jul 26) WT_RMF1992A.F (CALLIB) used as basis
c 	Rehana Yusaf (1.0.1:93 Oct 27) added arguments for rmfversn and 
c					hduclas3, also the name has been 
c				     	changed from rd_rmf1992a. In addition 
c					if extname is not found 
c					HDUCLAS1='RESPONSE' is searched for.
c					and HDUCLAS2='RSP_MATRIX'
c 	Rehana Yusaf (1.0.2:93 Nov 10) HDUVERS2 is read to obtain rmfversn if 
c					HDUVERSN not present then RMFVERSN is 
c					read. Prev' only RMFVERSN read 
c 	Ian M George (1.1.0: 93 Nov 17) Took out searching for correct xtens
c					(this is now responsibilty of main)
c 	Rehana Yusaf (1.1.1:94 Jan 11) Remove mvalues array and read matrix
c					matrix values straight into fmatrix
c 	Rehana Yusaf (1.1.1: 94 Jun 24) Make the routine less verbose, that is 
c					only print warnings at chatter>30 
c PARENT Routine (wtrmf2.f) history
c 	Ian M George (1.0.0:95 Nov 22) Copied from wtrmf1.f (1.1.1), but 
c					chantype added as passed parameter
c 	Ian M George (1.0.1:95 Nov 29) Added wtinfo & friends
c THIS ROUTINE (wtrmf3.f) history
c Ian M George (1.0.0:96 Oct 04) Copied from wtrmf2.f (1.0.1), suuport for
c                             	HDUVERS2 = '1.2.0' added (and flchan added 
c                               as passed parameter
c
c Banashree M Seifert (1.1.0 Oct 10, 1996)
c       . wrtstr was char*80 and made *8
c
c Banashree M Seifert (1.2.0 Nov13, 1996)
c       . flchan needed to be initialised
c --------------------------------------------------------------------
	character(7) version
	parameter (version = '1.2.0')
*-
c Internals
        character(6) subname
        parameter (subname = 'rdrmf3')
	integer status
	integer i, k, siz_mat, siz_ngrp
	integer ie,j, ic, frow,felem,colnum,inull
	character(80) message
        character(8) wrtstr
        character(30) comm, hduvers2
	integer ivalues(10), ioff
	real enull
        logical anyflg, qflchan_prob
c Initialise
      ierr = 0
      status = 0

c User info, if requested
        message = 'using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c Get necessary keywords
c NAXIS2 ...
      status = 0
      call ftgkyj(iunit,'NAXIS2',ienerg,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     &		'reading NAXIS2 keyword')
        ierr = 4
        goto 987
      ENDIF
      IF (ienerg.GT.maxen) THEN
        ierr = 4
	call wtferr(subname,version,status,
     & 		'Energy Array dimension is too small')
        goto 987
      ENDIF

c HDUCLAS3 ...
      hduclas3 = '  '
      status = 0
      call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUCLAS3 keyword')

c HDUVERS2 ...
      hduvers2 = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',hduvers2,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
c If HDUVERS2 < 1.2.0, then we have a potential problem regarding what the 
c first legal channel is for this detector. This will require a check later 
c so set a flag. 
	if((hduvers2.EQ.'1.0.0').or.(hduvers2.EQ.'1.1.0')) then
		qflchan_prob = .true.
	else
		qflchan_prob = .false.
	endif

c RMFVERSN ...
      rmfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',rmfversn,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading HDUVERS2 keyword')
      IF (rmfversn.EQ.'  ') THEN
        status = 0
        call ftgkys(iunit,'RMFVERSN',rmfversn,comm,status) 
        call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading RMFVERSN keyword')
      ENDIF

c TELESCOP ...
      status = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading TELESCOP keyword')
      IF (status.EQ.202) THEN
        telescop = 'UNKNOWN'
      ENDIF 	

c INSTRUME ...
      status = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading INSTRUME keyword')
      IF (status.EQ.202) THEN
        instrume = 'UNKNOWN'
      ENDIF         

c FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading FILTER keyword')
      IF (status.EQ.202) THEN
        filter = 'NONE'
      ENDIF         

c DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading DETNAM keyword')
      IF (status.EQ.202) THEN
        detnam = 'NONE'
      ENDIF

c EFFAREA ...
      status = 0
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading EFFAREA keyword')
      IF (status.NE.0) THEN
        areascal = 1
      ENDIF

c LO_THRES ...
      status = 0
      call ftgkye(iunit,'LO_THRES',lo_thresh,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & 		'Problem reading LO_THRES keyword')
      IF (status.NE.0) THEN
        lo_thresh = 0
      ENDIF

c DETCHANS ...
      status = 0
      call ftgkyj(iunit,'DETCHANS',ichan,comm,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading DETCHANS keyword')
        ierr = 1
        goto 987
      ENDIF
      IF (ichan.GT.maxchan) THEN
        ierr = 4
	call wterrm(subname,version,
     & 		'Channel Array dimension is too small')
        goto 987
      ENDIF

c CHANTYPE
      status = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,status)
      if(status.NE.0) then
        call wtfwrn(subname,version,chatter, 20, status,
     & 		'Problem reading CHANTYPE keyword from RMF')
	call wtinfo(chatter,1, 1,'setting CHANTYPE = UNKNOWN')
        chantype = 'UNKNOWN'
        status= 0
      endif


c Reasure user, if requested
	call wtinfo(chatter,20,1,'read all the keywords')
	call wtinfo(chatter,20,1,'reading the data')

c Get the data (and necessary extras)
c ENERG_LO ...
      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_lo,
     &            anyflg,status)      
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading ENERG_LO column')
        ierr = 1
        goto 987
      ENDIF

c ENERG_HI ...
      status = 0
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding ENERG_LO column')
         ierr = 4
         goto 987
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,energ_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading ENERG_HI column')
        ierr = 1
        goto 987
      ENDIF      

c NGRP ...
      status = 0
      call ftgcno(iunit,.false.,'N_GRP',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding N_GRP column')
         ierr = 4
         goto 987
      ENDIF
      inull = 0
      call ftgcvj(iunit,colnum,frow,felem,ienerg,inull,ngrp,
     &            anyflg,status)
      IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading N_GRP column')
        ierr = 1
        goto 987
      ENDIF    

c F_CHAN ...
      status = 0
      call ftgcno(iunit,.false.,'F_CHAN',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding F_CHAN column')
         ierr = 4
         goto 987
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &            ivalues,anyflg,status)
        IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading F_CHAN column')
          ierr = 1
          goto 987
        ENDIF 
        do j=1,ngrp(i)
           F_chan(i,j) = ivalues(j)
        enddo
      enddo
c OK, if we're reading a dataset created with HDUVERS2 < 1.2.0, then we have 
c a potential problem regarding what the first legal channel is for this 
c detector. Try and check to see if F_CHAN is ever zero, and if so assume 
c that the first legal channel is also zero. THIS WILL OFTEN CATCH THE PROBLEM,
c BUT NOT ALWAYS - although there is little else we can do.
c If we're reading a dataset created with HDUVERS2 >= 1.2.0, then the TLMIN
c keyword should be present, so go read it.
	if(qflchan_prob) then
      	  do i=1,ienerg
	     if(ngrp(i).gt.0 .and. F_chan(i,1).eq.0) then
		flchan = 0
		goto 321
             endif
      	  enddo
          flchan=1
	else
          write(wrtstr, '(a,i1)') 'TLMIN', colnum
          call crmvblk(wrtstr)
      	  status = 0
      	  call ftgkyj(iunit,wrtstr,flchan,comm,status)
      	  if(status.NE.0) then
	    message = 'Problem reading '//wrtstr//' keyword'
            call wtfwrn(subname,version,chatter, 1, status, message)
	    message = 'assuming first legal channel in the detector is '
     &		//'numbered channel 1'
	    call wtinfo(chatter,1, 1,message)
            flchan = 1
            status= 0
          endif
	endif
321	continue

c N_CHAN ...
      status = 0
      call ftgcno(iunit,.false.,'N_CHAN',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding N_CHAN column')
         ierr = 4
         goto 987
      ENDIF
      do i=1,ienerg
        inull = 0
        call ftgcvj(iunit,colnum,i,felem,ngrp(i),inull,
     &              ivalues,anyflg,status)
        IF (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem reading N_CHAN column')
          ierr = 1
          goto 987
        ENDIF  
        do j=1,ngrp(i)
          N_chan(i,j) = ivalues(j)
        enddo
      enddo

c MATRIX ...
c initialise matrix array ...
      do i=1,ichan
        do j=1,ienerg
          fmatrix(i,j) = 0.0
        enddo
      enddo

c imaxgrp ...
      siz_ngrp = 0
      do i=1,ienerg
        siz_ngrp = MAX(siz_ngrp,ngrp(i))         
      enddo
      imaxgrp = siz_ngrp

c read matrix ...
      status = 0
      call ftgcno(iunit,.false.,'MATRIX',colnum,status)
      If (status.NE.0) THEN
	call wtferr(subname,version,status,
     & 		'Problem finding MATRIX column')
         ierr = 4
         goto 987
      ENDIF
c ... first worry about offsets
	if(flchan.EQ.0) then
		ioff = 1
	else
		ioff = 0
	endif
      do ie=1,ienerg
        siz_mat = 0
        do j=1,ngrp(ie)
          siz_mat = siz_mat + N_chan(ie,j)
        enddo
        k = 0
        do j=1,ngrp(ie)
          do ic=F_chan(ie,j),F_chan(ie,j)+N_chan(ie,j) - 1
            k=k+1
            enull = 0
            call ftgcve(iunit,colnum,ie,k,1,enull,
     &              fmatrix(ic+ioff,ie),anyflg,status)
            IF (status.NE.0) THEN
		call wtferr(subname,version,status,
     & 			'Problem reading MATRIX column')
              ierr = 1
              goto 987
            ENDIF
          enddo
        enddo
      enddo


987	if(ierr.NE.0) then
	  call wterrm(subname, version, ' Fatal - aborting')
	else
	  call wtinfo(chatter,20,1, 
     &		'successfully read RSP_MATRIX data')
	endif


      return
      end
c ----------------------------------------------------------------------
c     END OF RDRMF3
c ----------------------------------------------------------------------

  
