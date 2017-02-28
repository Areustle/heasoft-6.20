*+RSP2RMF
	SUBROUTINE RSP2RF
	IMPLICIT NONE
c 
c Description:
c  Program to convert old-style (SF) RSP matrices into new (FITS) RMF format. 
c  NOTE - The par file is assumed to have been opened. 
c         The RMF extension will contain a variable length array for the 
c          "MATRIX" column if (and only if) this leads to a significant 
c   	   saving in disk space.
c
c Passed Paremeters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_RSP2RMF (see below)
c
c Include/Common blocks files
c  common TASK		       : (FTOOLS) standard fatal error message thingy
c 
c Called routines
c  subroutine GP_RSP2RMF       : (below) Gets parameters from XPI par file
c  subroutine CNV_RSP2RMF      : (below) Performs the conversion
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1992 Oct 13), quick & v.dirty more general version
c       ... minor changes ...
c  Ian M George     (2.0.0:1993 Aug 10), wt_rmf1992a inc var length arrays
c  Ian M George     (2.0.1:1993 Oct 12), change to copnrsf
c  Ian M George     (3.0.0:1994 Feb 09), added Dynamic Memory Allocation
c  Ian M George     (3.1.0:1994 Jun 23), improved DMA
c  Rehana Yusaf     (3.1.1: 1994 sept 13), add clobber parameter
c  Rehana Yusaf     (3.1.2: 1994 Dec 12), bug fix in GP_RSP2RMF
c                                         clobber was read as a string
c  Ian M George     (3.1.3: 1994 Dec 12), relaxed max energy condition
c  Ian M George     (3.1.4: 1995 Jun 21), swapped wtebd1.f to wtebd2.f
c  Ian M George     (4.0.0:96 Oct 07) updated to use wtrmf3 & wtebd3
	character(7) version
	parameter (version = '4.0.0')
*- 
c 
c Internals 
        character(40) taskname
	integer chatter, ierr
	real lo_thresh
	character(5) rmfversn
	character(20) origin
	character(20) telescop,instrume, detnam, filter
	character(25) context
	character(80) rspfil, rmffil, message
	logical qregrp, qcif, qrmfcomm, qebdcomm, qoverride,killit
c Initialize
        COMMON/task/taskname
        taskname ='RSP2RMF'//version
	ierr = 0

	message = '** RSP2RMF '//version
	call fcecho(message)


c Get Parameters from the par file
	call gp_rsp2rmf(rspfil, chatter, qregrp, lo_thresh,
     &		qcif, rmffil, rmfversn, origin, qrmfcomm, qebdcomm,
     &     qoverride, telescop, instrume, detnam, filter,killit)


c Do the nasty deed	
	call cnv_rsp2rmf(rspfil, chatter, qregrp, lo_thresh, 
     &		qcif, rmffil, rmfversn, origin, qrmfcomm, qebdcomm,
     &  qoverride, telescop, instrume, detnam, filter, killit,ierr)

c
	if(ierr.NE.0) then
	  context = 'Incomplete Execution'
	  message = '** RSP2RMF '//version//' ERROR : '// context
	  call fcecho(message)
          call fcerr(context)
	else 
	  message = '** RSP2RMF '//version//' Finished'
	  call fcecho(message)
	endif


	return
	end

c -------------------------------------------------------------------------
*+GP_RSP2RMF
	subroutine gp_rsp2rmf(rspfil, chatter, qregrp, lo_thresh,
     &		qcif, rmffil, rmfversn, origin, qrmfcomm, qebdcomm,
     &     qoverride, telescop, instrume, detnam, filter,killit)

	IMPLICIT NONE
	integer chatter
	real lo_thresh
	character(5) rmfversn
	character(20) origin
	character(20) telescop, instrume, detnam, filter
	character(80) rspfil, rmffil
	logical qregrp, qcif, qrmfcomm, qebdcomm, qoverride,killit
c 
c Description:
c  Gets the parameters required by RSP2RMF from the parameter file
c  NOTE - The XPI par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  RSPFILE     - name of i/p RSP (old-style SF) format data file
c		 (a .RSP will be assumed and added later, if not specified here)
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  QREGRP      - Whether regrouping of the RSP file is required
c  LO_THRESH   - if(QREGRP) threshold below which matrix truncated
c  QCIF        - Whether the mandatory CIF keywords are required
c  RMFFILE     - name of o/p RMF (FITS) format data file
c  RMFVERSN    - OGIP version of RMF file required
c  ORIGIN      - Institution which created the file
c  QRMF_COMM   - Whether comments to be added to RMF extension
c  QEBD_COMM   - Whether comments to be added to EBD extension
c  QOVERRIDE   - Whether tele,instr,filt etc keywords from par file are 
c                to be used, rather than those derived from RSP file
c  TELESCOP    - if(QOVERRIDE) Telescope/Mission name to be used
c  INSTRUME    - if(QOVERRIDE) Instrument/Detector name to be used
c  DETNAM      - if(QOVERRIDE) Supplimentary Detector name to be used (if reqd)
c  FILTER      - if(QOVERRIDE) Filter name to be used
c
c Origin:
c  Original
c
c Called Routines
c  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
c  subroutine WT_FERRMSG       : (CALLIB) Writes standard FITSIO message etc
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.1: 1992 Dec 23), Original
c  Ian M George     (1.0.2: 1993 Feb 14) converted to "uclgsi" etc
c  Ian M George     (1.0.3: 1993 Sep 03) changed order in which parameters read
c  Ian M George     (1.1.0:1993 Oct 12) returns rmfversn= '1.1.0' for '1992a'
c  Rehana Yusaf     (1.1.1: 1994 Sept 13) read clobber
c  Rehana Yusaf     (1.1.2: 1994 Dec 12) clobber was read as a string
c                                        now it is read as boolean
	character(7) version
	parameter (version = '1.1.2')
*- 
c Internals
	integer ierr
	character(30)  errstr, wrnstr
	character(80)  message
c Initialize
	errstr = '** GP_RSP2RMF ERROR: '
	wrnstr = '** GP_RSP2RMF WARNING: '
	ierr = 0
	lo_thresh = 0.0
	telescop = 'UNKNOWN'
	instrume = 'UNKNOWN'
	detnam = ' '
	filter = 'NONE'

c Get the name of the old-style SF file
	call uclgst('rspfil',rspfil, ierr)
	message = errstr // 'Getting RSPFIL parameter'
	call wt_ferrmsg(ierr, message)

c Get the name of the o/p FITS file
	call uclgst('rmffil',rmffil, ierr)
	message = errstr // 'Getting RMFFIL parameter'
	call wt_ferrmsg(ierr, message)

c Get the OGIP version number of the RMF file format to be created
	call uclgst('rmfversn',rmfversn, ierr)
	message = errstr // 'Getting RMFVERSN parameter'
	call wt_ferrmsg(ierr, message)
        if(rmfversn.EQ.'1992a') then
                rmfversn='1.1.0'
	endif

c Get the name of the institution producing the file
	call uclgst('origin',origin, ierr)
	message = errstr // 'Getting ORIGIN parameter'
	call wt_ferrmsg(ierr, message)

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
	message = errstr // 'Getting CHATTER parameter'
	call wt_ferrmsg(ierr, message)
	if(ierr.NE.0) then
		ierr = 0 
		message = errstr // 'Setting CHATTER = 10'
		call fcecho(message)
		chatter = 10
	endif	

c Give user info if requested
	if(chatter.GE.20) then	
	 message = ' ... using GP_RSP2RMF ' // version
	 call fcecho(message)
	endif


c Get the regrouping flag
	call uclgsb('qregrp',qregrp, ierr)
	message = errstr // 'Getting QREGRP parameter'
	call wt_ferrmsg(ierr, message)

c Get the lower threshold required for matrix, if regrouping requested
	if(qregrp) then
	  call uclgsr('threshold',lo_thresh, ierr)
	  message = errstr // 'Getting THRESHOLD parameter'
	  call wt_ferrmsg(ierr, message)
	endif	

c Get the CIF flag
	call uclgsb('qcif',qcif, ierr)
	message = errstr // 'Getting QCIF parameter'
	call wt_ferrmsg(ierr, message)

c Get the RMF Comments flag
	call uclgsb('qrmfcomm',qrmfcomm, ierr)
	message = errstr // 'Getting QRMFCOMM parameter'
	call wt_ferrmsg(ierr, message)

c Get the EBD Comments flag
	call uclgsb('qebdcomm',qebdcomm, ierr)
	message = errstr // 'Getting QEBDCOMM parameter'
	call wt_ferrmsg(ierr, message)

c Get the Override flag as to whether Telecop,Instr etc names derived 
c   from the RSP file are to be ignored, and those listed below used
c   instead
	call uclgsb('qoverride',qoverride, ierr)
	message = errstr // 'Getting QOVERRIDE parameter'
	call wt_ferrmsg(ierr, message)

	if(qoverride) then
c 	... OK, we're gonna override, so get the necessary parameters
	  call uclgst('telescop',telescop, ierr)
	  message = errstr // 'Getting TELESCOP parameter'
	  call wt_ferrmsg(ierr, message)

	  call uclgst('instrume',instrume, ierr)
	  message = errstr // 'Getting INSTRUME parameter'
	  call wt_ferrmsg(ierr, message)

	  call uclgst('detnam',detnam, ierr)
	  message = errstr // 'Getting DETNAM parameter'
	  call wt_ferrmsg(ierr, message)

	  call uclgst('filter',filter, ierr)
	  message = errstr // 'Getting FILTER parameter'
	  call wt_ferrmsg(ierr, message)
	endif	
        call uclgsb('clobber',killit, ierr)
        message = errstr // 'Getting CLOBBER parameter'
        call wt_ferrmsg(ierr, message)

	return
	end
c -------------------------------------------------------------------------
*+CNV_RSP2RMF
        subroutine cnv_rsp2rmf(rspfil, chatter, qregrp, lo_thresh,
     &          qcif, rmffil, rmfversn, origin, qrmfcomm, qebdcomm,
     &    qoverride, telescop, instrume, detnam, filter, killit,ierr)

	IMPLICIT NONE
	integer chatter, ierr
	real lo_thresh
	character(5) rmfversn
	character(20) origin
	character(20) telescop, instrume, detnam, filter
	character(80) rspfil, rmffil
	logical qregrp, qcif, qrmfcomm, qebdcomm, qoverride,killit
c 
c Description:
c  Program to convert old-style (SF) RSP matrices into new (FITS) RMF format. 
c  Note: The following options are NOT yet supported:
c  	QCIF = T ...... CIF keywords cannot be automatically added
c	QRMFCOMM = T... Additional of comments to RMF extension
c	QEBDCOMM = T... Additional of comments to EBD extension
c
c User i/ps required (prompted for):
c  None
c
c Passed parameters
c  RSPFIL        i   : name of the SF RSP file to be read in
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly) 
c  QREGRP        i   : Regrp the matrix ?
c  LO_THRESH     i   : Threshold below which matrix truncated
c  QCIF          i   : Include mandatory CIF keywords ?
c  RMFFIL        i   : name of FITS o/p file to be created
c  RMFVERSN      i   : OGIP version no. of RMF format to be created
c  ORIGIN        i   : Name of institution which produced the file
c  QRMFCOMM      i   : Include comments in RMF extension ?
c  QEBDCOMM      i   : Include comments in EBD extension ?
c  QOVERRIDE     i   : Whether tele,instr,filt etc keywords from par file are
c                	to be used, rather than those derived from RSP file
c  TELESCOP      i   : if(QOVERRIDE) Telescope/Mission name to be used
c  INSTRUME      i   : if(QOVERRIDE) Instrument/Detector name to be used
c  DETNAM        i   : if(QOVERRIDE) Supp Detector name to be used (if reqd)
c  FILTER        i   : if(QOVERRIDE) Filter name to be used
c  IERR            o : Error Flag (zero if all OK)
c   
c Called Routines:
c  subroutine CLNSTR     : (CALLIB) Cleans strings for fitsio
c  subroutine CHK_RMF    : (CALLIB) Checks RMF is in order      NOT IMPLEMENTED
c  subroutine FTECHO     : (FTOOLS) Writes to standard o/p
c  subroutine FTVERS     : (FITSIO) Returns version of FITSIO
c  subroutine GRP_RMF    : (CALLIB) Regroups the matrix if reqd 
c  subroutine RD_RSP	 : (CALLIB) Reads the old-style (SF) *.RSP file 
c  subroutine OP_NPA     : (CALLIB) Opens & writes a null P.Header 
c  subroutine WTRMF1     : (CALLIB) Writes the RMF Xtensn (RMFVERSN = 1992a)
c  subroutine WTEBD2     : (CALLIB) Writes the EBOUNDS Xtensn (RMFVERSN = 1992a)
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1992 Oct 13), original
c  Ian M George     (1.0.1:1993 Feb 28), wt_rmf1992a & wt_ebd1992a corrections
c  Ian M George     (1.0.2:1993 May 07), recreated after accidental deletion 
c  Ian M George     (1.0.3:1993 Jul 16), insert clnstr call 2fix illeg char prob
c  Ian M George     (1.0.4:1993 Jul 16), better error handling
c  Ian M George     (1.0.5:1993 Jul 30), new WT_RMF1992a & GRP_RMF calls
c  Ian M George     (1.0.6:1993 Aug 09), added CK_FILE 
c  Ian M George     (2.0.0:1994 Feb 09), added Dynamic Memory Allocation
c  Ian M George     (3.0.0:1994 Jun 23), improved DMA
c  Ian M George     (3.1.0:1994 Dec 12), removed maxchan & maxne (see chkrsp)
c  Ian M George     (3.1.1:1995 Jun 21), swapped wtebd1.f to wtebd2.f
c  Ian M George     (4.0.0:96 Oct 07) updated to use wtrmf3 & wtebd3
        character(7) version
        parameter (version = '4.0.0')
*- 
C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ************************************
c Max Array sizes
	integer maxhist, maxcomm
c	integer maxchan, maxne, maxgrp
	integer maxgrp
	parameter (maxhist=20,maxcomm=20,maxgrp=20)
c	parameter (maxchan=4096,maxne=2048)
c Internals
c ... parameters & static arrays
	integer status, i
	integer nk_hist, nk_comm, ounit
	integer imaxgrp, imsg
        integer ichan, ienerg, iebound, fchan
	real ftsver
        real area
	character(8) key(4)
	character(20) hduclas3
	character(20) old(4), new(4)
	character(20) old_tele, old_inst, old_detn, old_filt
        character(70) hist(maxhist), comment(maxcomm)
	character(30) errstr, wrnstr
	character(80) message, dummystr(1)
	logical qokfil
	integer idummy, nbad
	integer ibad(70)
	integer imaxchan, imaxne, idma, igot
c ... pointers to "arrays" to be dynamically allocated 
	integer p_ngrp, p_matrix, p_F_chan, p_N_chan
	integer p_energ_lo, p_energ_hi, p_e_min, p_e_max
c ... "arrays" to be dynamically allocated 
c       integer ngrp(imaxne)		real matrix(imaxchan,maxne)
c	integer F_chan(imaxne,maxgrp)	integer N_chan(imaxne,maxgrp)
c	real energ_lo(imaxne)		real energ_hi(imaxne)
c       real e_min(imaxchan)		real e_max(imaxchan)


c Initialize
	ierr = 0
	dummystr(1) = ' '
	errstr = '** CNV_RSP2RMF '//version//' ERROR:'	
	wrnstr = '** CNV_RSP2RMF '//version//' WARNING:'	

c Set max size of grouping array 
	imaxgrp = maxgrp

c set first channel to 1
        fchan = 1

c Give user info if requested
        if(chatter.GE.20) then
           message = ' ... using CNV_RSP2RMF '// version
	   call fcecho(message)
	   call ftvers(ftsver)
           write(message,'(a,f6.3)') 
     &		' ... using FITSIO Version ', ftsver
	   call fcecho(message)
        endif

c Check that the o/p file doesn't already exist or is illegal
	call ck_file(rmffil,dummystr, 1, qokfil, killit,chatter)
	if(.NOT.qokfil) then
                message = ' ... Offending file is RMFFIL: '// rmffil
		call fcecho(message)
		ierr = -1
		goto 482
	endif

c Temp error messages
	if(qcif) then
	   message = wrnstr // 'QCIF = T not yet supported (sorry)'
	   call fcecho(message)
	   qcif = .false.
	endif
	if(qrmfcomm) then
	   message = wrnstr // 'QRMFCOMM = T not yet supported (sorry)'
	   call fcecho(message)
	   qcif = .false.
	endif
	if(qebdcomm) then
	   message = wrnstr // 'QEBDCOMM = T not yet supported (sorry)'
	   call fcecho(message)
	   qcif = .false.
	endif

c Store the old keyword values (required below if override requested)
	old_tele = telescop
	old_inst = instrume
	old_detn = detnam
	old_filt = filter


c Go and finger the rsp file to suss out the size of the matrix
	call chkrsp(rspfil,chatter,
     &          imaxchan, imaxne, 
     &		ierr)
	if(ierr.NE.0) then
                message = errstr// ' Problem with RSP file'
		call fcecho(message)
		goto 482
	endif
	
c Calc the DMA requirements
	idma = (3*imaxne + imaxne*imaxchan + 2*imaxne*maxgrp + 
     &		2*imaxchan)*4
	if(chatter.GT.20) then
	   write(message,'(a,i12,a)') 
     &		' ... DMA requirements: ',idma, ' bytes'
	   call fcecho(message)
	endif

c *****
c Allocate dynamic memory

        p_ngrp = 0 
        p_matrix = 0
        p_F_chan = 0
        p_N_chan = 0 
        p_energ_lo = 0
        p_energ_hi = 0
        p_e_min = 0
        p_e_max = 0

		igot = 0
	call udmget(imaxne, 4, p_ngrp, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*4
	call udmget(imaxne*imaxchan, 6, p_matrix, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*imaxchan*4
	call udmget(imaxne*maxgrp, 4, p_F_chan, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*maxgrp*4
	call udmget(imaxne*maxgrp, 4, p_N_chan, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*maxgrp*4
	call udmget(imaxne, 6, p_energ_lo, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*4
	call udmget(imaxne, 6, p_energ_hi, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxne*4
	call udmget(imaxchan, 6, p_e_min, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxchan*4
	call udmget(imaxchan, 6, p_e_max, status)
            	if(status.NE.0) goto 645 
		igot = igot + imaxchan*4
645	if(status.NE.0) then
                message = errstr// ' Failed to allocate Dynamic Memory'
		call fcecho(message)
	   	write(message,'(a,i12,a)') 
     &		' ...... an additional ',idma - igot, 
     &		' bytes of swap space is required'
	   	call fcecho(message)
		ierr = -1
		goto 482
	endif
c *****

c Read in the old-style (SF) file
        call rd_rsp(rspfil,chatter,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, Comment,
     &          telescop, instrume, filter, area,
     &          imaxchan, ichan, imaxne, ienerg, 
     &		MEMR(p_energ_lo), MEMR(p_energ_hi),
     &          imaxgrp, MEMI(p_ngrp), MEMI(p_F_chan), 
     &		MEMI(p_N_chan), MEMR(p_matrix),
     &          iebound, MEMR(P_e_min), MEMR(p_e_max),
     &		status)
	if(status.ne.0) then
		ierr = 1
		message = ' ... RMFFIL not written'
		call fcecho(message)
		goto 482
	endif

c Override if neceesary, informing the debugging user of conflicts
	if(qoverride) then
	      imsg = 0
	      if(telescop.NE.old_tele) then
		imsg = imsg + 1
		key(imsg) = 'TELESCOP'
		old(imsg) = telescop
		new(imsg) = old_tele
		telescop = old_tele
	      endif
	      if(instrume.NE.old_inst) then
		imsg = imsg + 1
		key(imsg) = 'INSTRUME'
		old(imsg) = instrume
		new(imsg) = old_inst
		instrume = old_inst
	      endif
	      if(detnam.NE.old_detn) then
		imsg = imsg + 1
		key(imsg) = 'DETNAM'
		old(imsg) = detnam
		new(imsg) = old_detn
		detnam = old_detn
	      endif
	      if(filter.NE.old_filt) then
		imsg = imsg + 1
		key(imsg) = 'FILTER'
		old(imsg) = filter
		new(imsg) = old_filt
		filter = old_filt
	      endif
	   if(chatter.GE.20) then
	     if(imsg.NE.0) then
	      message = wrnstr // ' Following conflicts found:'
	      call fcecho(message)
	      do i = 1,imsg
		message = key(i) // ' from RSP  = ' // old(i)
		call fcecho(message)
		message = ' ... overwritten with ' // new(i)
		call fcecho(message)
	      enddo
	     endif
	   endif	
	endif

c (Re) Group the matrix
	if(qregrp) then
	     call grprmf(chatter, imaxchan, ichan, imaxne, ienerg, 
     >		MEMR(p_matrix), lo_thresh, maxgrp, fchan,
     >          MEMI(p_ngrp), MEMI(p_F_chan), 
     >		MEMI(p_N_chan), status)
	     if(status.ne.0) then
		ierr = 2
		message = ' ... RMFFIL not written'
		call fcecho(message)
		goto 482
	     endif
	endif


c Clean up any fitsio-illegal characters in the history and comment strings
	imsg = 0
        do i = 1, nk_hist
                call clnstr(chatter, hist(i), 70, hist(i),
     &                  idummy, nbad, ibad, status)
		if(status.ne.0) then
			ierr = 3
			message = ' ... RMFFIL not written'
			call fcecho(message)
			goto 482
		endif
		if(nbad.NE.0) imsg = imsg + 1
	enddo
        do i = 1, nk_comm
                call clnstr(chatter, comment(i), 70, comment(i),
     &                  idummy, nbad, ibad, status)
		if(status.ne.0) then
			ierr = 4
			message = ' ... RMFFIL not written'
			call fcecho(message)
			goto 482
		endif
		if(nbad.NE.0) imsg = imsg + 1
	enddo

	if(imsg.NE.0 .AND. chatter.GE.20) then
	    message = wrnstr // 
     &	       ' FITSIO-Illegal characters replaced by CLNSTR'
	     call fcecho(message)
	     write(message,'(a,i4,a)') 
     &	        ' ... in ', imsg, ' History and/or Comment card(s)'
	     call fcecho(message)
	endif

c Open & Write the FITS file
c ------------------------ PRIMARY HEADER ---------------------------
	if(rmfversn(1:1).EQ.'1')then
c             ... Open the FITS file and write a null primary header
	      call opnpa(rmffil, chatter, ounit, killit,status)
		if(status.ne.0) then
			ierr = 5
			goto 482
		endif
c 	      ... Add additional keywords to Primary Header
	        call FTPKYS(ounit,'CONTENT',
     &  	        'RESPONSE MATRIX',
     &             'RMF & EBOUNDS xtensions',
     &          	status)
c	        call FTPKYS(ounit,'RMFVERSN',
c     &  	        rmfversn	,
c     &  	        'OGIP classification of FITS format style',
c     &  	        status)
        	call FTPKYS(ounit,'ORIGIN',
     &          	origin,
     &       	   	'organization which created this file',
     &          	status)

        else
              message = errstr // 'Unknown format: '// rmfversn
              call fcecho(message)
              ierr = 1
              goto 482
        endif
c ------------------------ finished PRIMARY ---------------------------

	hduclas3 = 'FULL'

c ------------------------ RMF EXTENSION ----------------------------
	if(rmfversn(1:1).EQ.'1')then
c             ... Write the RMF extension within RMF file
        	call wtrmf3(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment,rmfversn,hduclas3,
     &          telescop, instrume, detnam, filter, area,
     &		'PHA', fchan,
     &          imaxchan, ichan, imaxne, ienerg, MEMR(p_energ_lo), 
     &		MEMR(p_energ_hi),
     &          imaxgrp, MEMI(p_ngrp), MEMI(p_F_chan), 
     &		MEMI(p_N_chan),
     &          MEMR(p_matrix), lo_thresh, status)
		if(status.NE.0) goto 876
        else
              message = errstr // 'Unknown format: '// rmfversn
              call fcecho(message)
              ierr = 1
              goto 482
        endif


c 	       ... Add CIF keyswords if required
	
c 	       ... Add comments if required

c ------------------------ finished RMF EXTENSION ----------------------------

876		continue

c ----------------------------- EBOUNDS EXTENSION ----------------------------
	if(rmfversn(1:1).EQ.'1')then
		fchan = 1
c              ... Write the EBOUNDS extension within the RMF file
        	call wtebd3(ounit, chatter,
     &          nk_hist, hist,
     &          nk_comm, comment, rmfversn,
     &          telescop, instrume, detnam, filter, area,
     &		'PHA', fchan,
     &          iebound, MEMR(p_e_min), MEMR(p_e_max), status)

c 	       ... Add CIF keyswords if required

c 	       ... Add comments if required

        else
              message = errstr // 'Unknown format: '// rmfversn
              call fcecho(message)
              ierr = 1
              goto 482
        endif
c ------------------------ finished EBOUNDS EXTENSION ----------------------


c Close the FITS file
        call ftclos(ounit, status) 
	if(status.ne.0) then
		ierr = 6
		goto 482
	endif

c Check for errors
482	if(ierr.ne.0) then
		message = errstr // ' Fatal'
		call fcecho(message)
	endif


c *****
c Free the dynamic Memory
	call udmfre(p_ngrp,4,status)	
            	if(status.NE.0) goto 646 
	call udmfre(p_matrix,6,status)	
            	if(status.NE.0) goto 646 
	call udmfre(p_F_chan, 4, status)
            	if(status.NE.0) goto 646 
	call udmfre(p_N_chan, 4, status)
            	if(status.NE.0) goto 646 
	call udmfre(p_energ_lo, 6, status)
            	if(status.NE.0) goto 646 
	call udmfre(p_energ_hi, 6, status)
            	if(status.NE.0) goto 646 
	call udmfre(p_e_min, 6, status)
            	if(status.NE.0) goto 646 
	call udmfre(p_e_max, 6, status)
            	if(status.NE.0) goto 646 

646	if(status.NE.0) then
                message = wrnstr// 
     &		' Failed to deallocate Dynamic Memory'
		call fcecho(message)
		message = ' ... (only a potential problem '//
     &			'if your NOT running the host version)'
	endif

c *****
	return
	end
c-------------------------------------------------------------------------
*+CHKRSP
	subroutine chkrsp(rspfil,chatter,imaxchan, imaxen,errstat)
	IMPLICIT NONE
	integer imaxchan, imaxen
	integer chatter, errstat
	character*(*) rspfil
c 
c Description:
c  Does a quick inquiry on an old-style (SF) RSP matrix based on the (old) 
c  RDRSP program, to determine the size of the matrix stored. 
c  The routine is useful for allocating dynamic memory prior to calling 
c  the "real" reader of response matrices rd_rsp.f (callib).
c  Note, the RSP file is opened, read & closed within this routine
c
c Passed parameters
c  RSPFIL        i   : name of the SF RSP file to be read in
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  IMAXCHAN        o : Actual Number of PHA channels in full matrix
c  IMAXEN          o : Actual Number of energy bins in full matrix
c  ERRSTAT         o : Error Flag (0 if everything OK)
c
c User i/ps required (prompted for from par file):
c  None
c
c Include files
c  None - responsesf.inc (version 16-OCT-1992) has been hardwired in
c         (for SF response file package definitions) 
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c  subroutine CGETLUN    : (CALLIB) Gets free i/o unit number
c  subroutine CNXPKSF    : (CALLIB) Decodes SF package header
c  subroutine COPNRSF    : (CALLIB) Opens SF RSP file
c  subroutine PCLGST     : (XPI) Gets string from parameter file
c  subroutine PCLPST     : (XPI) Puts string to parameter file
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine CRSUBSF    : (CALLIB) Reads a single subsidiary SF file record 
c
c Compilation & Linking
c  link with CALLIB, FTOOLS & XPI
c
c Origin:
c  RDRSP, BBRSP, then rd_rsp.f 
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1994 May 10), original
c  Ian M George     (2.0.0:1994 Dec 12), removed maxne+maxchan as passed params
	character(7) version
	parameter (version = '2.1.0')
*- 

c Max Array Sizes
	integer*4 maxsize, maxchanen,mxsize4
	parameter (maxchanen=4096,maxsize=32768,mxsize4=8192)
c Internals
	integer iunit, ierrsf
	integer lenbuf
	integer nhist, index, nsubs, infoar(4)
	logical qdone
	character*(12) pkgtyp
	character(80) header, tmplte, message
        character(30)  errstr, wrnstr

c buffers used to read packages

        character(1) buffer(maxsize)
        integer*4 ibuffer(mxsize4)
        equivalence (buffer, ibuffer)

        errstr = '** CHKRSP '//version//' ERROR: '
	wrnstr = '** CHKRSP '//version//' WARNING: '

c START MAIN
	errstat = 0

c Give user info if requested
        if(chatter.GE.20) then
                message = ' ... using CHKRSP '// version
		call fcecho(message)
        endif

c Open the rsp file
	call cgetlun(iunit)
	if(iunit.LE.0) then
		message = errstr// ' Fatal'
		call fcecho(message)
		errstat = 3
		return
	endif

	call copnrsf(rspfil, iunit, 'XSPEC rspnse', header, tmplte,
     &               nhist, ierrsf)
	if(ierrsf.NE.0) then
		errstat = 1
		go to 986
	endif

c -------------------------------------------------------------------------
c Start reading in the packages from the SF RSP file
	qdone = .false.
	do while ( .NOT.qdone)
		ierrsf = 0
		lenbuf = maxsize
		pkgtyp = ' '
		index = 0
		nsubs = 0
		call cnxpksf(iunit, pkgtyp, index, nsubs, 
     &			infoar, buffer,
     & 			lenbuf, .FALSE., ierrsf)
		qdone = ierrsf.EQ.7
		if( .NOT.qdone) then
		   IF(pkgtyp.EQ.'detect info ') then
c -- Detector Info package
			imaxchan = ibuffer(6)
			imaxen = ibuffer(8)
c ... check everything is hunky
			if(imaxchan.LE.0) then
                                write(message,'(2a,i12,a)') errstr,
     &                             'No. chans <= zero'
                                call fcecho(message)
                                errstat = 2
                                return
			elseif(imaxen.LE.0) then
                                write(message,'(2a,i12,a)') errstr,
     &                             'No. energies <= zero'
                                call fcecho(message)
                                errstat = 2
                                return
			endif
c ... check arrays are not too large
c ... IMG took this out 1994 Dec 12 (v3.1.3) since no longer necessary w/DMA
c     (maxne used to be passed & set as 2048}
c		        if(imaxen.GT.maxne) then
c		                write(message,'(2a,i12,a)') errstr,
c     &  		           'No. energy bins > max allowed (',
c     &					maxne,')'
c				call fcecho(message)
c				errstat = 2
c				return
c		        elseif(imaxchan.GT.maxchan) then
		        if(imaxchan.GT.maxchanen) then
		                write(message,'(2a,i12,a)') errstr,
     &  		           'No. PHA channels > max allowed (',
     &					maxchanen,')'
				call fcecho(message)
				errstat = 2
				return
			endif
		   endif
	        endif
	enddo 

c Close the RSP file
	close(iunit)

986	if(errstat.ne.0) then
	  message = errstr // 'Incomplete execution'
	  call fcecho(message)	  
	endif

	return
	end

c -------------------------------------------------------------------------
