*+MARFRMF
	SUBROUTINE MARFRF

	IMPLICIT NONE
c 
c Description:
c  Program to multiply/divide a detector redistribution matrix through by an 
c Ancillary Response file or scalar
c  NOTE - The par file is assumed to have been opened. 
c         The RMF extension will contain a variable length array for the 
c          "MATRIX" column if (and only if) this leads to a significant 
c   	   saving in disk space.
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_MARFRMF (see below)
c
c Include/Common blocks files
c  common TASK		       : (FTOOLS) standard fatal error message thingy
c 
c Called routines
c  subroutine GP_MARFRMF       : (below) Gets parameters from XPI par file
c  subroutine DO_MARFRMF      : (below) Performs the conversion
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1992 Nov 15), quick & v.dirty 
c  Ian M George     (1.0.1:1994 Jan 10), reduced max array sizes
c  Ian M George     (2.0.0:1994 Mar 07), added Dynamic Memory Allocation
c  Ian M George     (2.1.0:1994 Jun 27), DMA requirements read from i/p file
c  Rehana Yusaf     (2.1.1:1994 Sep 13), minor changes, clobber read
c                                         ck_file has additional argument
c  Ian M George     (2.1.2:1995 Mar 14), bugettes fixed in gp_marfrmf
c  Ian M George     (2.2.0:1995 Apr 08), added scalar & qdivide parameters
c  Ian M George     (2.3.0:1995 Jun 21), DO_MARFRMF calls wtebd2 not wtebd1
c  Ian M George     (2.3.1:1995 Jun 22), reads chantyp from RMF & writes to o/p
c  Ian M George     (2.3.2:1995 Aug 21), minor additional error checking
c  Ian M George     (2.3.3:1995 Nov 22), DO_MARFRMF calls rdrmf2 & wtrmf2 now
c  Ian M George     (2.3.4:1995 Nov 29) Added wtinfo, wterrm, wtwarm etc
c  Ian M George     (2.3.5:1995 Dec 18) Added wtbegm & wtendm changes
c  Ian M George (3.0.0:96 Oct 04) updated the depths to use rdrmf3,rdebd3,
c                                                                   wtrmf3
c                               & wtebd3 subroutines, which try their best to
c                               not to get the indexing of the matrix incorrect
c                               for detectors whose first channel is numbered
c                               channel zero
c				Fixed bug whereby EBOUNDS extension always read
c 				from rmfil irrespective of value of ebfil param
c  Ian M George (3.0.1:96 Nov 05) tweak so rmfversn=1.2.0 if not read from par
c  Banashree M Seifert (3.1.0)
c           . only in gp_marfrmf subroutine, when asks for input files
c             first it asks for ARF then RMF file name.  Before (v3.0.1)
c             it was otherway round.  User didnot like it
c
c  Banashree M Seifert (3.2.0, Mar 12, 1997)
c           . initialised ierr in do_mathpha
c  Peter D Wilson (3.2.1, Feb 20, 1998)
c           . Copy HISTORY keywords from input RMF and EBOUNDS extensions
c             to the new output file
c  Peter D Wilson (3.2.2, Aug 11, 1999)
c           . Call wterrm instead of wtferr when rdrmf4 fails
c  Ning Gan  (3.2.3, Aug 30, 1999)
c           . The  imaxarf was set to the wrong value ( read from the 
c             wrong extension if the arf extension is not first one).
C           . Correct the bug in rdrmf4.   
c  Ning Gan  (3.2.4, Oct 14, 1999)
C           . Switched the order of reading parameter for filename of arf
C             and rmf.
c  M Tripicco (3.2.5, Oct 31, 2000)
c           . upped history/comment string lengths to 72 to match CFITSIO
c             default and fcrhky() which starts copying from col 9 now.
c  kaa       (3.2.6, Jul 18, 2005)
c             added (hidden) arfcol option so ARF can be read from a column 
c             other than SPECRESP.
c ------------------------------------------------------------------
	character(7) version
	parameter (version = '3.2.6')
*- 
c Internals 
        character(40) taskname
	integer chatter, ierr
	real scalar
	character(5) rmfversn
	character(20) telescop,instrume, detnam, filter, arfcol
	character(80) rmfil, arfil, outfil, ebfil
	logical qoverride,killit, qdivide
c Initialize
        taskname ='marfrmf'
	ierr = 0

c Get Parameters from the par file
	call gp_marfrmf(rmfil, ebfil, arfil, outfil, chatter, rmfversn, 
     &     qoverride, telescop, instrume, detnam, filter, killit,
     &	   scalar, qdivide, arfcol, ierr)
	if(ierr.NE.0) goto 148

c Start-up MAIN
	call wtbegm(taskname, version, chatter)

c Do the nasty deed	
	call do_marfrmf(taskname, version, 
     &		rmfil, ebfil, arfil, outfil, chatter, rmfversn, 
     &     	qoverride, telescop, instrume, detnam, filter, killit,
     &		scalar, qdivide, arfcol, ierr)

c Finish-Off
148	continue
	call wtendm(taskname, version, ierr,chatter)

	return
	end

c -------------------------------------------------------------------------
*+GP_MARF
	subroutine gp_marfrmf(rmfil, ebfil, arfil, outfil, chatter, 
     &  rmfversn,qoverride, telescop, instrume, detnam, filter, killit,
     &  scalar, qdivide, arfcol, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	real scalar
	character*(*) rmfversn
	character*(*) telescop, instrume, detnam, filter, arfcol
	character*(*) rmfil, arfil, outfil, ebfil
	logical qoverride,killit, qdivide
c 
c Description:
c  Gets the parameters required by MARFRMF from the parameter file
c  NOTE - The par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  RMFIL       - file name of i/p RSP_MATRIX extn (redistribution matrix) 
c  EBFIL       - file name of i/p EBOUNDS extn (nom chan-energy boundaries)
c  ARFIL       - file name of i/p SPECRESP extn (ancillary response dataset)
c  OUTFIL      - name of o/p RMF (redistribution matrix) to be created
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  RMFVERSN    - OGIP version of RMF file required
c  QOVERRIDE   - Whether tele,instr,filt etc keywords from par file are 
c                to be used, rather than those derived from RSP file
c  TELESCOP    - if(QOVERRIDE) Telescope/Mission name to be used
c  INSTRUME    - if(QOVERRIDE) Instrument/Detector name to be used
c  DETNAM      - if(QOVERRIDE) Supplimentary Detector name to be used (if reqd)
c  FILTER      - if(QOVERRIDE) Filter name to be used
c  KILLIT      - Something FTOOLS Grp made me add
c  SCALAR      - if(ARFIL=NONE) Scalar value by which RSP_MATRIX multiplied
c  QDIVIDE     - Flag whether * or / performed (qdivide = T for /)
c  ARFCOL      - Option to use a column other than SPECRESP for the ARF data
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
c  Ian M George     (1.0.0: 1992 Dec 23), Original
c  Ian M George     (1.0.1: 1992 Dec 23), Fixed error handling
c  Rehana yusaf     (1.0.2: 1994 Sep 13), clobber read
c  Ian M George     (1.1.0: 1995 Mar 14), clobber read as 'b' rather than 's'
c  Ian M George     (2.0.0: 1995 Apr 08), added scalar & qdivide parameters
c  Ian M George     (2.0.1: 1995 Nov 29) Added wtinfo, wterrm, wtwarm etc
c
c  Banashree M Seifert (2.1.0)
c           . when asks for input files first it asks for ARF then RMF 
c             file name.  Before (v2.0.1) it was otherway round.  
c             User didnot like it
c  kaa  (2.2.0) added arfcol parameter
c -----------------------------------------------------------------------
	character(7) version
	parameter (version = '2.2.0')
*- 
c Internals
	character(10) subname
	parameter (subname = 'gp_marfrmf')
	character(80)  message
	character(80) tmpfile
c Initialize
	ierr = 0
	telescop = 'UNKNOWN'
	instrume = 'UNKNOWN'
	detnam = ' '
	filter = 'NONE'
	scalar = 0.0

c Get the name of the RMF
	call uclgst('rmfil',rmfil, ierr)
	if(ierr.ne.0) then
	  call wterrm(subname, version,
     &                'Problem getting RMFIL parameter')
	  goto 999
	endif

c Get the name of the ARF
	call uclgst('arfil',arfil, ierr)
	if(ierr.ne.0) then
	  call wterrm(subname, version, 
     &		'Problem getting ARFIL parameter')
	  goto 999
	endif
        call crmvlbk(arfil)

c Check for a null filename, implying a scalar will be entered
	tmpfile = arfil
        call ftupch(tmpfile)
	if((tmpfile(1:1).EQ.' ').OR.
     &		(tmpfile(1:4).EQ.'NONE')) then
	 message = 'Assuming you wish to simply operate using '//
     &		'a scalar'
	 call wtinfo(chatter,1,1,message)
	 call uclgsr('scalar',scalar, ierr)
	 if(ierr.ne.0) then
	  call wterrm(subname, version, 
     &		'Problem getting SCALAR parameter')
	  goto 999
	 else
	  arfil = 'NONE'
	 endif
	 if(scalar.EQ.1.0) then
	  call wtwarm(subname, version, chatter, 1, 'SCALAR = 1.0')
	  call wtinfo(chatter,1,1, 
     &		' The requested output file identical to '//
     &		'input file')
	 elseif(scalar.EQ.0.0) then
	  call wterrm(subname, version, 
     &		'SCALAR = 0.0 is meaningless')
	  goto 999
	 elseif(scalar.LT.0.0) then
	  call wtwarm(subname, version, chatter, 1,'SCALAR < 0.0')
	 call wtinfo(chatter,1,1, 'I will try, '//
     &          'but have no idea what it means')
	 endif
	endif



c Get the name of the o/p file
	call uclgst('outfil',outfil, ierr)
	if(ierr.ne.0) then
	  call wterrm(subname, version, 
     &		'Problem getting OUTFIL parameter')
	  goto 999
	endif

c Get the name of file containing EBOUNDS extn
	call uclgst('ebfil',ebfil, ierr)
	if(ierr.NE.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'Problem getting EBFIL parameter')
	  call wtinfo(chatter,1,1, 'setting EBFIL = RMFIL')
	  ebfil = rmfil
	  ierr = 0
	elseif(ebfil(:1).EQ.'%')then
	  ebfil = rmfil
	endif

c Get the OGIP version number of the RMF file format to be created
	call uclgst('rmfversn',rmfversn, ierr)
	if(ierr.ne.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'Problem getting RMFVERSN parameter')
          rmfversn='1.2.0'
	  message = 'setting RMFVERSN = '//rmfversn
	  call wtinfo(chatter,1,1,message)
	  ierr = 0
        elseif(rmfversn.EQ.'1992a') then
                rmfversn='1.2.0'
	endif

c Get the column to be used to read the ARF
	call uclgst('arfcol',arfcol, ierr)
	if(ierr.ne.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'Problem getting ARFCOL parameter, assuming SPECRESP')
	  ierr = 0
	  arfcol = 'SPECRESP'
	endif

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
	if(ierr.NE.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'Problem getting CHATTER parameter')
		ierr = 0 
	        call wtinfo(chatter,1,1, 'setting CHATTER = 10')
		chatter = 10
	endif	

c Give user info if requested
         message = ' using '//subname//' '//version
	 call wtinfo(chatter,20,1,message)

c Check whether the RSP_MATRIX extension is to be multiplied or 
c divided by the SPECRESP dataset (or scalar)
c ... qdivide = T for DIVIDE
	call uclgsb('qdivide',qdivide, ierr)
	if(ierr.NE.0) then
	  call wterrm(subname, version, 
     &		'Problem getting QDIVIDE parameter')
	  call wtinfo(chatter,1,1, ' Too dangerous to continue')
	  goto 999
	endif	


c Get the Override flag as to whether Telecop,Instr etc names derived 
c   from the RMF file are to be ignored, and those listed below used
c   instead
	call uclgsb('qoverride',qoverride, ierr)
	if(ierr.NE.0) then
	  call wtwarm(subname, version, chatter, 1,
     &		'Problem getting QOVERRIDE parameter')
		ierr = 0 
	  call wtinfo(chatter,1,1,' setting QOVERRIDE = FALSE')
		qoverride = .false.
	endif	

	if(qoverride) then
c 	... OK, we're gonna override, so get the necessary parameters
	  call uclgst('telescop',telescop, ierr)
	  if(ierr.ne.0) then
	    call wterrm(subname, version, 
     &		'Problem getting TELESCOP parameter')
	    goto 999
	  endif

	  call uclgst('instrume',instrume, ierr)
	  if(ierr.ne.0) then
	    call wterrm(subname, version, 
     &		'Problem getting INSTRUME parameter')
	    goto 999
	  endif

	  call uclgst('detnam',detnam, ierr)
	  if(ierr.ne.0) then
	    call wterrm(subname, version, 
     &		'Problem getting DETNAM parameter')
	    goto 999
	  endif

	  call uclgst('filter',filter, ierr)
	  if(ierr.ne.0) then
	    call wterrm(subname, version, 
     &		'Problem getting FILTER parameter')
	    goto 999
	  endif
	endif	

        call uclgsb('clobber',killit, ierr)
        if(ierr.ne.0) then
	    call wtwarm(subname, version, chatter, 1,
     &		'Problem getting CLOBBER parameter')
	    ierr = 0 
	    message = ' setting CLOBBER = FALSE'
	    call wtinfo(chatter,1,1,message)
	    killit = .false.
        endif

c Check for errors
999	if(ierr.ne.0) then
	  call wterrm(subname, version, ' unable to continue')
	endif

	return
	end
c -------------------------------------------------------------------------
*+DO_MARFRMF
	subroutine do_marfrmf(tname,tvers, 
     &		rmexp, ebexp, arexp, outfil, chatter, 
     &     	rmfversn, qoverride, telescop, instrume, detnam, filter, 
     &		killit, scalar, qdivide, arfcol, ierr)

	IMPLICIT NONE

C - Alex: added new variables numgrp, order, qorder, isorder
c
	integer chatter, ierr, numgrp 
	real scalar
	character*(*) rmfversn, tname, tvers
	character*(*) telescop, instrume, detnam, filter, arfcol
	character*(*) rmexp, arexp, outfil, ebexp
	logical qoverride,killit, qdivide, qorder, isorder
c 
c Description:
c  Program to multiply an RMF redistribution matrix through by an Ancillary 
c response file (or scalar).
c
c User i/ps required (prompted for):
c  None
c
c Passed parameters
c  RMEXP       - filename[ext] of i/p RSP_MATRIX extn (redistribution matrix) 
c  EBEXP       - filename[ext] of i/p EBOUNDS extn (nom chan-energy boundaries)
c  AREXP       - filename[ext] of i/p SPECRESP extn (ancillary response dataset)
c  OUTFIL      - name of o/p RMF (redistribution matrix) to be created
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  RMFVERSN    - OGIP version of RMF file required
c  QOVERRIDE   - Whether tele,instr,filt etc keywords from par file are 
c                to be used, rather than those derived from RSP file
c  TELESCOP    - if(QOVERRIDE) Telescope/Mission name to be used
c  INSTRUME    - if(QOVERRIDE) Instrument/Detector name to be used
c  DETNAM      - if(QOVERRIDE) Supplimentary Detector name to be used (if reqd)
c  FILTER      - if(QOVERRIDE) Filter name to be used
c  SCALAR      - if(ARFIL=NONE) Scalar value by which RSP_MATRIX multiplied
c  QDIVIDE     - Flag whether * or / performed (qdivide = T for /)
c  ARFCOL      - Column to use to get ARF data (standard is SPECRESP)
c  IERR            o : Error Flag (zero if all OK)
c   
c Called Routines:
c  subroutine CGETLUN    : (CALLIB) Gets a free logical unit
c  subroutine CK_FILE    : (CALLIB) Checks if file exists, deletes if reqd
c  subroutine CLNSTR     : (CALLIB) Cleans strings for fitsio
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p
c  subroutine FNDEXT	 : (CALLIB) Finds xtens based on EXTNAME value
c  subroutine FNDHDU	 : (CALLIB) Finds xtens based on HDUCLASn values
c  subroutine FTCLOS	 : (FITSIO) Closes a FITS file
c  subroutine FTMAHD	 : (FITSIO) Move to an absolute xtens no.
c  subroutine FTMRHD	 : (FITSIO) Move a relative no. xtens rel to CDU
c  subroutine FTOPEN	 : (FITSIO) Open a FITS file
c  subroutine FTPKYx     : (FITSIO) writes a keyword of type "x"
c  subroutine FTVERS     : (FITSIO) Returns version of FITSIO
c  subroutine OP_NPA     : (CALLIB) Opens & writes a null P.Header 
c  subroutine RDARF1     : (CALLIB) Reads an SPECRESP xtens
c  subroutine RDEBD1     : (CALLIB) Reads an EBOUNDS xtens
c  subroutine RDRMF1     : (CALLIB) Reads an RSP_MATRIX xtens
c  subroutine REMAP	 : (CALLIB) Remaps 1-d array onto new grid
c  subroutine WT_FERRMSG : (CALLIB) Writes standard FITSIO error message
c  subroutine WTRMF1     : (CALLIB) Writes the RMF Xtensn (RMFVERSN = 1992a)
c  subroutine WTEBD2     : (CALLIB) Writes the EBOUNDS Xtensn (RMFVERSN = 1992a)
c  subroutine RDRMF4     : (CALLIB) Reads an RSP_MATRIX xtens(RMFVERSN = 1999a)
c  subroutine WTRMF4     : (CALLIB) Writes the RMF Xtensn (RMFVERSN = 1999a)
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Oct 20), original
c  Ian M George     (1.0.1:1994 Jan 10), reduced max array sizes
c  Ian M George     (2.0.0:1994 Mar 01), added Dynamic Memory Allocation
c  Rehana Yusaf     (2.0.1:1994 sep 13), added killit argument
c  Ian M George     (3.0.0:1995 Apr 08), added scalar & qdivide parameters
c  Ian M George     (3.0.1:1995 Jun 21), call wtebd2 instead of wtebd1
c  Ian M George     (3.0.2:1995 Jun 22), reads chantyp from RMF & writes to o/p
c  Ian M George     (3.0.3:1995 Aug 21), minor additional error checking
c  Ian M George     (3.0.4:1995 Nov 22), rdrmf1->rdrmf2 & wtrmf1->wtrmf2
c  Ian M George (4.0.0:96 Oct 04) updated to use rdrmf3,rdebd3,wtrmf3
c                               & wtebd3 subroutines, which try their best to
c                               not to get the indexing of the matrix incorrect
c                               for detectors whose first channel is numbered
c                               channel zero
c				Fixed bug whereby EBOUNDS extension always read
c 				from rmfil irrespective of value of ebfil param
c  M Tripicco        (4.0.1:97 Jan 03) added initialization of status
c
c Banashree M Seifert (4.1.0, Mar 12, 1997)
c            . error status initialised (line #1203)
c              ( Mar 18, 1997) maxgrp made 10 instead of 5 (for HXTE data)
c Banashree M Seifert (4.2.0, June6, 1997)
c            . warning messages when looking for new HDUCLAS are made
c              vissible at high chatter since otherwise it confuses
c              users
c Peter D Wilson (4.2.1, Feb 20, 1998)
c            . Copy HISTORY keywords from RMF and EBOUNDS extensions
c Alex M. (4.2.2, Feb 8, 1999) Incorporated new subroutines RDRMF4 and 
c                              WTRMF4 written by Dr. Keith A. Arnaud. 
c                              Modified subroutines multmatrix and 
c                              divmatrix. Fixed bug in 'call fndhdu()'
c                              associated with passing a wrong parameter
c                              (iunit1 instead of iunix3).
c  Ning Gan  (4.2.3, Aug 30, 1999)
c           . The  imaxarf was set to the wrong value ( read from the 
c             wrong extension if the arf extension is not first one). 
c  kaa (4.3.0) Added arfcol option to read ARF from column other than SPECRESP
c ------------------------------------------------------------------------
        character(7) version
        parameter (version = '4.3.0')
*- 
	character(10) subname
	parameter (subname = 'do_marfrmf')
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

c Maximum Array sizes ( Alex -- added maxelt, numelt )
c ---------------------------------------------------------------------------
        integer maxchan, maxne, maxgrp, maxelt, numelt
	integer maxhist, maxcomm, maxextn
	parameter (maxhist=100,maxcomm=20,maxextn=99)
c ---------------------------------------------------------------------------

c Commons
        character(40) taskname
        COMMON/task/taskname
c Internals
c ... parameters & statis arrays
	integer block, imove, i, status, jj, htype
	integer ninstr, nsearch, nfound, clenact
	integer rmextn,ebextn,arextn, fchan
	integer next(maxextn)
	character(8) dummy8
	character(20) dummy20
	character(20) instr(9)
	character(20) outhdu(9,maxextn), outver(9,maxextn)
	character(20) extnam(maxextn)
	character(20) rmfhdu(9), outhdu3
	character(20) atelescop, ainstrume, adetnam, afilter
        character(30) rchantyp, echantyp
	character(80) rmfil, arfil, ebfil
	character(80) rmf_rmfversn, ebd_rmfversn
	integer nk, nk_prhist, nk_rmfhist, nk_ebhist, nk_comm
	integer ounit, iunit1, iunit2, iunit3
	integer imaxgrp, imsg, rflchan
        integer ichan, iebound, icol
	integer rien, aien
	real ftsver
        real area, lo_thresh
	character(5) arfversn
	character(8) key(4)
	character(20) old(4), new(4)
	character(20) old_tele, old_inst, old_detn, old_filt
c upped hist/comm strings to 72 since CFITSIO starts them in col 9 now
        character(72) prhist(maxhist), rmfhist(maxhist), ebhist(maxhist)
	character(72) comment(maxcomm), newhist
	character(30) comm
	character(80) message, dummystr(1)
        character(80) rmf_extname,rmf_extcomm
        character(80) ebd_extname,ebd_extcomm
	logical qokfil, anyflg
	integer idma, igot
	integer imaxne, imaxchan, imaxelt, imxarf
c ... pointers to "arrays" to be dynamically allocated
c --- Alex added p_order
	integer p_ngrp, p_F_chan, p_N_chan, p_matrix, p_re_lo, p_order
	integer p_re_hi, p_ae_lo, p_ae_hi, p_e_min, p_e_max, p_sprsp
	integer p_factor, p_chan
c ... "arrays" to be dynamically allocated
c       integer ngrp(imaxne)		integer F_chan(imaxne,maxgrp)
c	integer N_chan(imaxne,maxgrp)	real matrix(imaxelt)
c	real re_lo(imaxne)		real re_hi(imaxne)
c	real ae_lo(imxarf)		real ae_hi(imxarf)
c       real e_min(imaxchan) 		real e_max(imaxchan)
c	real sprsp(imxarf)		real factor(imaxne)
c	integer chan(imaxchan)          integer order(maxgrp)
c ... Other
	logical qscalar

c Initialize
	status = 0
	echantyp = 'UNKNOWN'
	rchantyp = 'UNKNOWN'
	qscalar = .false.
	ierr = 0
	dummystr(1) = ' '
c -- Alex ---------------
        isorder = .false.
	qorder  = .false.
c ------------------------

c Check out whether an ARF or a scalar has been requested
	if((arexp.EQ.'NONE').AND.(scalar.NE.0.0)) then
		qscalar = .true.
	else
		qscalar = .false.
	endif

c Give user info if requested
        message = ' using '//subname//' '//version
	call wtinfo(chatter,20,1,message)
	call ftvers(ftsver)
        write(message,'(a,f6.3)') 
     &		' using FITSIO Version ', ftsver
	call wtinfo(chatter,20,2,message)

c Check that the o/p file doesn't already exist or is illegal
	call ck_file(outfil,dummystr, 1, qokfil, killit,chatter)
	if(.NOT.qokfil) then
                message = 'OUTFIL parameter found offensive to '// 
     &			subname//' '//version
	  	call wtinfo(chatter,1,1,message)
		ierr = -1
		goto 482
	endif

c Store the old keyword values (required below if override requested)
	old_tele = telescop
	old_inst = instrume
	old_detn = detnam
	old_filt = filter

c Parse the supplied filenames, stripping off incld extension numbers
        call fcpars(rmexp,rmfil,rmextn,status)
	if(status.NE.0) then
          message = ' Problem passing rmfil expression'
          call wtfwrn(subname, version, chatter, 1, status, message)
	  call wtinfo(chatter,1,1,' will search all extensions')
	  rmextn = -99
	endif	

        call fcpars(ebexp,ebfil,ebextn,status)
	if(status.NE.0) then
	  message = ' Problem passing ebfil expression'
          call wtfwrn(subname, version, chatter, 1, status, message)
	  call wtinfo(chatter,1,1, 'will search all extensions')
	  ebextn = -99
	endif	
	if(ebexp.EQ.rmexp) ebextn = -99

	if(.not.qscalar) then

C --- Alex: initialized arextn
        arextn = 0
        call fcpars(arexp,arfil,arextn,status)
	if(status.NE.0) then
	  message = ' Problem passing arfil expression'
          call wtfwrn(subname, version, chatter, 1,status, message)
	  call wtinfo(chatter,1,1,'will search all extensions')
	  arextn = -99
	endif	
	endif

c Open i/p RSP_MATRIX file
        status = 0
      	call cgetlun(iunit1)
      	call ftopen(iunit1,rmfil,0,block,status)
	if(status.ne.0) then
	    message = ' opening RMF file: '//rmfil(:20)
            call wtferr(subname, version, status, message)
	    ierr = 1
	    goto 482
      	endif


c Read HISTORY keywords from primary header of RSP_MATRIX file
	nk_prhist = 0
      	call fcrhky(iunit1,maxhist-1,nk_prhist,prhist,status)
	if(status.ne.0) then
	    message = ' reading RMF HISTORY keywords'
            call wtferr(subname, version, status, message)
	    ierr = 1
	    goto 482
      	endif

c Open i/p EBOUNDS file, if necessary
	if(ebfil.NE.rmfil) then
          status = 0
      	  call cgetlun(iunit2)
      	  call ftopen(iunit2,ebfil,0,block,status)
	  if(status.ne.0) then
	    message = ' opening EBOUNDS file: '//ebfil(:20)
            call wtferr(subname, version, status, message)
	    ierr = 1
	    goto 482
      	  endif
	else
	  iunit2 = iunit1
	endif


c Open i/p ARF file and get the number of energy bins
	if(.not.qscalar) then
      	  call cgetlun(iunit3)
      	  call ftopen(iunit3,arfil,0,block,status)
	  if(status.ne.0) then
	    message = ' opening ARF file: '//arfil(:20)
            call wtferr(subname, version, status, message)
	    ierr = 1
	    goto 482
      	  endif
	endif

c Fill in a few history & comment records
	newhist = 'RSP_MATRIX dataset convolved with SPECRESP array'//
     &		' by MARFRMF '//tvers
	nk_prhist = nk_prhist + 1
	prhist(nk_prhist) = newhist
	comment(1) = 'MARFRMF '//tvers//'Summary:'
	comment(2) = '  I/p RMF file:     '//rmexp
	if(ebexp.EQ.rmexp) then
	 comment(3) = '  I/p EBOUNDS file: '//ebfil
	else
	 comment(3) = '  I/p EBOUNDS file: '//ebexp
	endif
	comment(4) = '  I/p ARF file:     '//arexp
	nk_comm = 4
	if(qscalar) then
	 nk_comm = nk_comm+1
	 write(comment(nk_comm),'(a,g12.6)') 
     &		'  Scalar factor:    ', scalar
	endif

	nk_comm = nk_comm+1
	if(qdivide) then
	  if(qscalar) then
     	     comment(nk_comm) = 
     &		'  ... I/p RMF dataset DIVIDED by scalar'
	  else
     	     comment(nk_comm) = 
     &		'  ... I/p RMF dataset DIVIDED by ARF dataset'
	  endif
	else
	  if(qscalar) then
     	     comment(nk_comm) = 
     &		'  ... I/p RMF dataset MULTIPLIED by scalar'
	  else
     	     comment(nk_comm) = 
     &		'  ... I/p RMF dataset MULTIPLIED by ARF dataset'
	  endif
	endif

c -------------------------- RSP_MATRIX extension --------------------
c Find the RSP_MATRIX extension in the RMF file 
c - Extension number NOT given as part of rmexp (search for HDUCLAS/EXTNAM)
	if(rmextn.LT.0) then
	  ninstr = 2
	  instr(1) = 'RESPONSE'
	  instr(2) = 'RSP_MATRIX'
	  nsearch = maxextn
          call fndhdu(chatter, iunit1, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
c 	  ... check for old-style EXTNAME values if no OK HDUCLASn values found

	  if(nfound.LE.0) then
            message = ' Ext w/ allowed HDUCLASn keywrds not found'
            call wtwarm(subname, version, chatter, 20,message)
	    message = ' offending file: '//rmfil
	    call wtinfo(chatter,20,1,message)
	    message = ' searching for extnsion with EXTNAME = MATRIX'
	    call wtinfo(chatter,20,2,message)
            call fndext(chatter, iunit1, 'MATRIX',
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	  endif

c  - Extension number IS given as part of rmfil 
	else
	   call ftmahd(iunit1,rmextn+1,htype,status)
	     message = ' Problem moving to specified xtens'
             call wtferr(subname, version, status, message)
c 	   ... grab the HDUCLAS values for reference
	   ninstr = 1
	   instr(1) = '*'
	   nsearch = 1
           call fndhdu(chatter, iunit1, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	   nfound = 1
	   next(1) = 0
	endif

c - sort out what we've got
	if(nfound.GT.1) then
	  call wterrm(subname, version, 
     &		'RMFIL contains >1 RSP_MATRIX datasets')
	    write(message,'(i12,a)') nfound,' extensions found:'
	    call wtinfo(chatter,1,1,message)
	    do i = 1, nfound
		write(message,'(a,i12,a)') 'Ext ',next(i),':'	
	        call wtinfo(chatter,1,2,message)
		write(message,'(a,a)') 'EXTNAME = ', extnam(i)
	        call wtinfo(chatter,1,3,message)
		do jj = 1, 4
		  write(message,'(a,i2,2a)') 
     &			'HDUCLAS',jj,' = ',outhdu(i,jj)
		  call wtinfo(chatter,1,3,message)
		enddo
	    enddo
	    message = 
     &		' ... Extension number must be specified via rmfil param'
	    call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	elseif(nfound.LE.0) then
            message = ' Unable to locate an RSP_MATRIX extension'
            call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	else
	 do i = 1, 9
	   rmfhdu(i) = outhdu(i,1)
	 enddo
	 outhdu3 = 'FULL'
	endif

c Move to the Extension if not already there

	if(next(1).GT.0) then
	   imove = next(1)
	   status = 0
	   call ftmrhd(iunit1,imove,htype,status)
	     message = ' Problem moving to RSP-MATRIX xtens'
             call wtferr(subname, version, status, message)
	endif

c Alex: Get the values of RMF arrays sizes maxne, maxgrp, and maxelt

        maxne  = 0
        maxgrp = 0
        maxelt = 0

        call rmfsz( iunit1, chatter, maxne, maxgrp, maxelt, status)
	if(status.ne.0) then
	    message = ' reading RMF array sizes maxne,maxgrp, maxelt'
            call wtferr(subname, version, status, message)
	    ierr = 1
	    goto 482
      	endif

c Set max sizes of arrays
        imaxne  = maxne 
	imaxgrp = maxgrp
        imaxelt = maxelt

c Read the dimensions of the RMF file

        maxchan = 0       
	
        call ftgkyj(iunit1,'DETCHANS',maxchan,comm,status)
	if(status.NE.0) then
            call wtferr(subname, version, status, 
     &		' reading DETCHANS')
	    ierr = 1
	    goto 482
	endif

        imaxchan = maxchan

c Calc the DMA requirements
        idma = (4*imaxne + imaxne*imaxchan + 2*imaxne*maxgrp +
     &          3*imaxchan)*4
        IF ( .NOT.qscalar ) idma = idma + 12*imxarf
	write(message,'(a,i12,a)')
     &          'DMA requirements: ',idma, ' bytes'
	call wtinfo(chatter,20,1,message)


c *****
c Allocate dynamic memory

        p_ngrp = 0
        p_matrix = 0
        p_F_chan = 0
        p_N_chan = 0
        p_order = 0
        p_re_lo = 0
        p_re_hi = 0
        p_e_min = 0
        p_e_max = 0
        p_factor = 0
        p_chan = 0

c -- Alex -- added memory allocatio for order array
		igot = 0
        call udmget(imaxne, 4, p_ngrp, status)
		if(status.NE.0) goto 645
		igot = igot + imaxne*4
        call udmget(imaxelt, 6, p_matrix, status)
		if(status.NE.0) goto 645
		igot = igot + imaxelt*4
        call udmget(imaxgrp, 4, p_F_chan, status)
		if(status.NE.0) goto 645
		igot = igot + imaxgrp*4
        call udmget(imaxgrp, 4, p_N_chan, status)
		if(status.NE.0) goto 645
		igot = igot + imaxgrp*4
        call udmget(imaxgrp, 4, p_order, status)
		if(status.NE.0) goto 645
		igot = igot + imaxgrp*4
        call udmget(imaxne, 6, p_re_lo, status)
		if(status.NE.0) goto 645
		igot = igot + imaxne*4
        call udmget(imaxne, 6, p_re_hi, status)
		if(status.NE.0) goto 645
		igot = igot + imaxne*4
        call udmget(imaxchan, 6, p_e_min, status)
		if(status.NE.0) goto 645
		igot = igot + imaxchan*4
        call udmget(imaxchan, 6, p_e_max, status)
		if(status.NE.0) goto 645
		igot = igot + imaxchan*4
	call udmget(imaxne, 6, p_factor, status)
		if(status.NE.0) goto 645
		igot = igot + imaxne*4
	call udmget(imaxchan, 4, p_chan, status)
		if(status.NE.0) goto 645
		igot = igot + imaxchan*4
645     if(status.NE.0) then
            call wterrm(subname, version, 
     &		' Failed to allocate Dynamic Memory')
                write(message,'(a,i12,a)')
     &          ' an additional ',idma - igot,
     &          ' bytes of swap space is required'
	        call wtinfo(chatter,1,1,message)
                ierr = -1
                goto 482
        endif
c *****
c Read in the RMF history keywords
	nk_rmfhist = 0
	call fcrhky(iunit1,maxhist-1,nk_rmfhist,rmfhist,status)
	nk_rmfhist = nk_rmfhist+1
	rmfhist(nk_rmfhist) = newhist

c Read in the RMF data ( Alex -- replaced rdrmf3 version by rdrmf4 )
c Read in the RMF data ( Ziqin -- replaced rdrmf4 version by rdrmf5 )

        rmf_extname = ' '
        rmf_extcomm =' '
	call rdrmf5(
     & iunit1,chatter,rmf_extname,rmf_extcomm,
     & qorder,imaxne,maxgrp,maxelt,rmf_rmfversn,
     & dummy20,telescop,instrume,detnam,filter,area,rchantyp,
     & rflchan,ichan,rien,imaxgrp,numelt,MEMR(p_re_lo), 
     & MEMR(p_re_hi),MEMI(p_ngrp),MEMI(p_F_chan),MEMI(p_N_chan),
     & isorder,MEMI(p_order),MEMR(p_matrix),lo_thresh,ierr)
      
	if(ierr.NE.0) then
	  message = ' Problem reading RSP_MATRIX xtens'
          call wterrm(subname, version, message)
	  goto 482
	endif
c -------------------------------------------------------------------------

c Checks & Warnings
	if(qscalar) then
	    message = ' Setting HDUCLAS3 of o/p same as '//
     &		' that for i/p RSP_MATRIX'
	    call wtinfo(chatter,1,1,message)
	    outhdu3 = rmfhdu(3)
	    message = ' (ie '//outhdu3(:clenact(outhdu3))//')'
	    call wtinfo(chatter,1,2,message)
	else	
	 if(rmfhdu(3).EQ.'FULL') then
	    if(qdivide) then
	      message = 
     &		' HDUCLAS3 in o/p RSP_MATRIX set to REDIST'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'REDIST'
	    else
	      message = ' i/p RSP_MATRIX is a FULL matrix '
              call wtwarm(subname, version, chatter, 1,message)
	      message = 
     &		' ...... Unsure what multiplication by an ARF gives'
	      call wtinfo(chatter,1,2,message)
	      message = 
     &		' ... HDUCLAS3 in o/p RSP_MATRIX set to UNKNOWN'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'UNKNOWN'
	    endif
	 elseif(rmfhdu(3).EQ.'REDIST') then
	    if(qdivide) then
	      message = ' i/p RSP_MATRIX is a REDIST matrix '
              call wtwarm(subname, version, chatter, 1,message)
	      message = 
     &		'Unsure what division by an ARF gives'
	      call wtinfo(chatter,1,2,message)
	      message = 
     &		' HDUCLAS3 in o/p RSP_MATRIX set to UNKNOWN'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'UNKNOWN'
	    else
	      message = 
     &		' HDUCLAS3 in o/p RSP_MATRIX set to FULL'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'FULL'
	    endif
	 elseif(rmfhdu(3).EQ.'DETECTOR') then
	    if(qdivide) then
	      message = ' i/p RSP_MATRIX is a DETECTOR matrix '
              call wtwarm(subname, version, chatter, 1, message)
	      message = 
     &		' Assuming division by an ARF '//
     &		'gives a REDIST matrix'
	      call wtinfo(chatter,1,2,message)
	      message = 
     &		' HDUCLAS3 in o/p RSP_MATRIX set to REDIST'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'REDIST'
	    else
	      message = 
     &		' HDUCLAS3 in o/p RSP_MATRIX set to FULL'
	      call wtinfo(chatter,1,1,message)
	      outhdu3 = 'FULL'
	    endif
	 else
	    message = ' i/p RSP_MATRIX of unknown type'
            call wtwarm(subname, version, chatter, 1, message)
	    message = 'HDUCLAS3 = '// rmfhdu(3)
	    call wtinfo(chatter,1,2,message)
	    message = ' HDUCLAS3 in o/p RSP_MATRIX set to UNKNOWN'
	    call wtinfo(chatter,1,1,message)
	    outhdu3 = 'UNKNOWN'
	 endif
	endif

c Rewind the RMF file
	status = 0
	call ftmahd(iunit1,1,htype, status)
	if(status.NE.0) then
	   message = ' Problem rewinding RMF file'
           call wtferr(subname, version, status, message)
	endif

c -------------------------- EBOUNDS extension --------------------
c Find the EBOUNDS extension in the EBFIL file 
c - Extension number NOT given as part of ebfil (search for HDUCLAS/EXTNAM)
	if(ebextn.LT.0) then
	  ninstr = 2
	  instr(1) = 'RESPONSE'
	  instr(2) = 'EBOUNDS'
	  nsearch = maxextn
          call fndhdu(chatter, iunit2, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
c - check for old-style EXTNAME values if no acceptable HDUCLASn values found
	  if(nfound.LE.0) then
            message = ' Ext w/ allowed HDUCLASn keywrds not found'
            call wtwarm(subname, version, chatter, 20,message)
	    message = 'offending file: '//ebfil
	    call wtinfo(chatter,20,1,message)
	    message = 'searching for extnsion with EXTNAME = EBOUNDS'
	    call wtinfo(chatter,20,1,message)
            call fndext(chatter, iunit2, 'EBOUNDS',
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	  endif
c  - Extension number IS given as part of ebexp
	else
	   call ftmahd(iunit2,ebextn+1,htype,status)
	     message = ' Problem moving to specified xtens'
             call wtferr(subname, version, status, message)
c 	   ... grab the HDUCLAS values for reference
	   ninstr = 1
	   instr(1) = '*'
	   nsearch = 1
           call fndhdu(chatter, iunit1, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	   nfound = 1
	   next(1) = 0
	endif

c - sort out what we've got
	if(nfound.GT.1) then
            call wterrm(subname, version,
     &		' EBFIL contains >1 EBOUNDS dataset')
	    write(message,'(i12,a)') nfound,' extensions found:'
	    call wtinfo(chatter,1,1,message)
	    do i = 1, nfound
		write(message,'(a,i12,a)') ' Ext ',next(i),':'	
	        call wtinfo(chatter,1,2,message)
		write(message,'(a,a)') 'EXTNAME = ', extnam(i)
	        call wtinfo(chatter,1,2,message)
		do jj = 1, 4
		  write(message,'(a,i2,2a)') 
     &			'HDUCLAS',jj,' = ',outhdu(i,jj)
	  		 call wtinfo(chatter,1,3,message)
		enddo
	    enddo
	    message = 
     &		' ... Extension number must be specified via ebfil param'
	    call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	elseif(nfound.LE.0) then
            message = ' Unable to locate an EBOUNDS extension'
            call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	endif

c Move to the Extension if necessary
	if(next(1).GT.0) then
	   imove = next(1)
	   status = 0
	   call ftmrhd(iunit2,imove,htype,status)
	     message = ' Problem moving to EBOUNDS xtens'
             call wtferr(subname, version, status, message)
	endif

c *****
c Read in the EBOUNDS history keywords
	nk_ebhist = 0
	call fcrhky(iunit1,maxhist-1,nk_ebhist,ebhist,status)
	nk_ebhist = nk_ebhist+1
	ebhist(nk_ebhist) = newhist

c Read in the EBOUNDS data
        ebd_extname = ' '
        ebd_extcomm = ' '
        call rdebd4(iunit2,chatter, ebd_extname, ebd_extcomm,
     &          imaxchan,
     &          telescop,instrume,detnam,filter,area,
     &		echantyp,fchan,
     &          iebound,MEMI(p_chan),MEMR(p_e_min),MEMR(p_e_max),
     &		ebd_rmfversn,ierr)
	if(ierr.NE.0) then
	  message = ' Problem reading EBOUNDS xtens'
          call wterrm(subname, version, message)
	  goto 482
	endif
c Determine what the first channel number is in the EBOUNDS extension
c	call fixebd(chatter, MEMI(p_chan), fchan, ierr)

c Read additional info required below
c	call ftgkys(iunit1,'CHANTYPE',echantyp,comm,status)
c      	IF (chatter.GE.30) THEN
c          message = ' reading CHANTYPE from EBOUNDS file '
c	  call wtferr(subname, version, status, message)
c      	ENDIF

c Perform checks
	if(rchantyp.NE.echantyp) then
	  message = ' CHANTYPE mis-match'
          call wtwarm(subname, version, chatter, 1,message)
	  message = ' CHANTYPE from RMF extn:     '//rchantyp
	  call wtinfo(chatter,1,1,message)
	  message = ' CHANTYPE from EBOUNDS extn: '//echantyp
	  call wtinfo(chatter,1,1,message)
	  message = ' continuing assuming you know what '//
     &		'you are doing'
	  call wtinfo(chatter,1,2,message)
	  message = ' o/p file will have CHANTYPE: '//
     &		rchantyp
	  call wtinfo(chatter,1,2,message)
	endif

	if(rflchan.ne.fchan) then
	  message = ' Apparent discrepancy in channel numbering between'
     &		//' input RMF & ARF extns'
	  call wtwarm(subname, version, chatter, 1,message) 
	  write(message,'(a,i12)') 'RMF appears to start with channel: ',
     &		rflchan
	  call wtinfo(chatter,1,2,message) 
	  write(message,'(a,i12)')
     &          'EBOUNDS appears to start with channel: ',
     &		fchan
	  call wtinfo(chatter,1,2,message) 
	  message = subname//version//' unsure what to do'
	  call wtinfo(chatter,1,3,message)
	  ierr = 10
	  goto 482
	endif

c Close the RSP_MATRIX & EBOUNDS files

	status = 0

        call ftclos(iunit1, status) 

	if(iunit2.NE.iunit1) call ftclos(iunit2, status) 

        if(.NOT.qscalar) then

c -------------------------- SPECRESP extension --------------------
c Find the SPECRESP extension in the ARF file 
c - Extension number NOT given as part of arexp (search for HDUCLAS/EXTNAM)

	if(arextn.LT.0) then

	  ninstr = 2
	  instr(1) = 'RESPONSE'
	  instr(2) = 'SPECRESP'
	  nsearch = maxextn

          call fndhdu(chatter, iunit3, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)

c - check for old-style EXTNAME values if no acceptable HDUCLASn values found
	  if(nfound.LE.0) then
            message = ' Ext w/ allowed HDUCLASn keywrds not found'
            call wtwarm(subname, version, chatter, 20,message)
	    message = ' offending file: '//arfil
	    call wtinfo(chatter,20,1,message)
	    message = ' searching for extnsion with EXTNAME = SPECRESP'
	    call wtinfo(chatter,20,1,message)
            call fndext(chatter, iunit3, 'SPECRESP',
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	  endif

c  - Extension number IS given as part of arexp
	else

	   call ftmahd(iunit3,arextn+1,htype,status)
	   message = 'Problem moving to specified xtens'
	   call wtferr(subname, version, status, message)
c 	   ... grab the HDUCLAS values for reference

	   ninstr = 1
	   instr(1) = '*'
	   nsearch = 1
           ierr = 0
           call fndhdu(chatter, iunit3, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)

	   nfound = 1
	   next(1) = 0
	endif

c - sort out what we've got
	if(nfound.GT.1) then
            call wterrm(subname, version,
     &		' ARFIL contains >1 SPECRESP datasets')
	    write(message,'(i12,a)') nfound,' extensions found:'
	    call wtinfo(chatter,1,1,message)
	    do i = 1, nfound
		write(message,'(a,i12,a)') 'Ext ',next(i),':'	
	  	call wtinfo(chatter,1,2,message)
		write(message,'(a,a)') 'EXTNAME = ', extnam(i)
	        call wtinfo(chatter,1,2,message)
		do jj = 1, 4
		  write(message,'(a,i2,2a)') 
     &			'HDUCLAS',jj,' = ',outhdu(i,jj)
	  	  call wtinfo(chatter,1,2,message)
		enddo
	    enddo
	    message = 
     &		' ... Extension number must be specified via arfil param'
	    call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	elseif(nfound.LE.0) then
            message = ' Unable to locate an SPECRESP extension'
            call wterrm(subname, version, message)
	    ierr = 2
	    goto 482
	endif


c Move to the Extension if necessary
	if(next(1).GT.0) then
	   imove = next(1)
	   status = 0
	   call ftmrhd(iunit3,imove,htype,status)
	     message = ' Problem moving to SPECRESP xtens'
             call wtferr(subname, version, status, message)
	endif

C       read the imxarf.
        CALL ftgkyj(iunit3, 'NAXIS2', imxarf, comm, status)
        IF ( status .NE. 0 ) THEN
            call wtferr(subname, version, status, 
     &                  ' reading NAXIS2 for SPECRESP')
	    ierr = 1
	    goto 482
        ENDIF 

C       allocate the p_ae_lo, p_ae_hi, p_sprsp
        p_ae_lo = 0
        p_ae_hi = 0
        p_sprsp = 0
	if(.NOT.qscalar) then
            call udmget(imxarf, 6, p_ae_lo, status)
		if(status.NE.0) goto 645
		igot = igot + imxarf*4
            call udmget(imxarf, 6, p_ae_hi, status)
		if(status.NE.0) goto 645
		igot = igot + imxarf*4
	    call udmget(imxarf, 6, p_sprsp, status)
		if(status.NE.0) goto 645
		igot = igot + imxarf*4
        endif
c Read in the SPECRESP data

        call rdarf1(iunit3,chatter,
     &          atelescop,ainstrume,adetnam,afilter,
     &          aien, MEMR(p_ae_lo), MEMR(p_ae_hi), MEMR(p_sprsp), 
     &		arfversn,ierr)
	if(ierr.NE.0) then
	  message = ' Problem reading SPECRESP xtens'
          call wterrm(subname, version, message)
	  goto 482
	endif

c if arfcol is not SPECRESP then try to read the column

	IF ( arfcol .NE. 'SPECRESP' ) THEN
	   CALL ftgcno(iunit3, .false., arfcol, icol, ierr)
	   IF ( ierr .NE. 0 ) THEN
	      message = ' Cannot find '//arfcol
	      call wterrm(subname, version, message)
	      goto 482
	   ENDIF
	   CALL ftgcve(iunit3, icol, 1, 1, aien, 0.0, MEMR(p_sprsp),
     &                 anyflg, ierr)
	   IF ( ierr .NE. 0 ) THEN
	      message = ' Cannot read '//arfcol
	      call wterrm(subname, version, message)
	      goto 482
	   ENDIF
	ENDIF

c Close the ARF file
        call ftclos(iunit3, status) 

	endif
c ------- End of data collection -------------------
c ------- Check RSP_MATRIX vs SPECRESP when FILE entered -------
	if(.NOT.qscalar) then
c Check/Warn if there could be trouble
c ... Trouble with the energy grids 
	      call chkgrid(chatter, rien,aien,MEMR(p_re_lo), 
     &		MEMR(p_re_hi),
     &		MEMR(p_ae_lo), MEMR(p_ae_hi), comment, nk_comm)
c ... Trouble with the mission/instrument etc 
	      imsg = 0
	      if(telescop.NE.atelescop) then
		imsg = imsg + 1
		key(imsg) = 'TELESCOP'
		old(imsg) = telescop
		new(imsg) = atelescop
	      endif
	      if(instrume.NE.ainstrume) then
		imsg = imsg + 1
		key(imsg) = 'INSTRUME'
		old(imsg) = instrume
		new(imsg) = ainstrume
	      endif
	      if(detnam.NE.adetnam) then
		imsg = imsg + 1
		key(imsg) = 'DETNAM'
		old(imsg) = detnam
		new(imsg) = adetnam
	      endif
	      if(filter.NE.afilter) then
		imsg = imsg + 1
		key(imsg) = 'FILTER'
		old(imsg) = filter
		new(imsg) = afilter
	      endif
	      if(imsg.NE.0) then
	        message = ' Following conflicts found:'
                call wtwarm(subname, version, chatter, 1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	        do i = 1,imsg
		  message = key(i) // ' from RMF  = ' // old(i)
	  	  call wtinfo(chatter,1,1,message)
		  nk_comm = nk_comm + 1
		  comment(nk_comm) = message
		  message = key(i) // ' from ARF  = ' // new(i)
	  	  call wtinfo(chatter,1,1,message)
		  nk_comm = nk_comm + 1
		  comment(nk_comm) = message
	        enddo
	        if(.NOT.qoverride) then
	          message = 'values from RMF will be used in o/p'
		  call wtinfo(chatter,1,2,message)
		  nk_comm = nk_comm + 1
		  comment(nk_comm) = message
	        endif
	      endif

c Remap the SPECRESP dataset onto the energy grid supplied via RSP_MATRIX
		call remap(chatter,aien, MEMR(p_ae_lo), 
     &		 MEMR(p_ae_hi), MEMR(p_sprsp), 
     &		 rien, MEMR(p_re_lo), MEMR(p_re_hi), MEMR(p_factor), 
     &		 ierr)
		if(ierr.GT.0) then
	          message = ' Problem with the Remapping'
          	  call wterrm(subname, version, message)
	          goto 482
	         elseif(ierr.LT.0) then
	          message = ' Carrying on regardless '
                  call wtwarm(subname, version, chatter, 1,message)
	        endif
	endif

c ------- Fix Variables & Pop SPECRESP array when SCALAR entered -------
        ierr=0
	if(qscalar) then
	   aien = rien
	   call scalfill(rien, scalar, MEMR(p_factor), ierr)
	   atelescop = telescop
	   ainstrume = instrume
	   adetnam = detnam
	   afilter = filter
	   if(qdivide) then
	      lo_thresh = lo_thresh/scalar
	   else
	      lo_thresh = lo_thresh*scalar
	   endif
	endif

c --------- Do The Maths ----------------------------------------------
c Perfom the multiplication (RSP_MATRIX * SPECRESP)
c Alex - modified the calls of divmatrix and multmatrix subroutines

	if(qdivide) then

	   call divmatrix(imaxelt,imaxgrp,imaxne,MEMI(p_ngrp),
     & MEMI(p_N_chan),MEMR(p_matrix),MEMR(p_factor))
	else
	   call multmatrix(imaxelt,imaxgrp,imaxne,MEMI(p_ngrp),
     & MEMI(p_N_chan),MEMR(p_matrix),MEMR(p_factor))
	endif
c Override if necessary, informing the debugging user of conflicts
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
	      message = ' Following conflicts found:'
              call wtwarm(subname, version, chatter, 1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	      do i = 1,imsg
		message = key(i) // ' from RSP  = ' // old(i)
	  	call wtinfo(chatter,1,1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
		message = ' overwritten with ' // new(i)
	  	call wtinfo(chatter,1,2,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	      enddo
	     endif
	   endif	
	endif

c Open & Write the FITS file
c ------------------------ PRIMARY HEADER ---------------------------
	if(rmfversn(1:1).EQ.'1')then
c             ... Open the FITS file and write a null primary header
	      call opnpa(outfil, chatter, ounit, killit,status)
		if(status.ne.0) then
			ierr = 5
			goto 482
		endif
c 	      ... Add additional keywords to Primary Header
	        call FTPKYS(ounit,'CREATOR',
     &  	        taskname,
     &             's/w task which wrote this dataset',
     &          	status)

	        call FTPKYS(ounit,'CONTENT',
     &  	        'RESPONSE MATRIX',
     &             'RMF & EBOUNDS xtensions',
     &          	status)
c	        call FTPKYS(ounit,'RMFVERSN',
c     &  	        rmfversn	,
c     &  	        'OGIP classification of FITS format style',
c     &  	        status)
		do 132 nk=1,nk_prhist
		   call FTPHIS(ounit,prhist(nk),status)
 132		continue
        else
            message = 'Unknown format: '// rmfversn
            call wterrm(subname, version, message)
            ierr = 1
            goto 482
        endif
c ------------------------ finished PRIMARY ---------------------------
c ------------------------ RMF EXTENSION ----------------------------
      IF (ebfil.EQ.rmfil) THEN
      IF( rmextn.gt. ebextn) THEN
          goto 504
      ENDIF
      ENDIF
502     continue
        
	if(rmfversn(1:1).EQ.'1')then
c             ... Write the RMF extension within RMF file
c -- Alex: replaced the wtrmf3 version by wtrmf4 
c -- Ziqin: replaced the wtrmf4 version by wtrmf5 
 
        call wtrmf5(ounit,chatter,rmf_extname,rmf_extcomm,
     & nk_rmfhist,rmfhist,
     & nk_comm,comment,rmfversn,outhdu3,telescop,instrume,
     & detnam,filter,area,rchantyp,rflchan,numelt,ichan,rien,
     & imaxgrp,MEMR(p_re_lo),MEMR(p_re_hi),MEMI(p_ngrp), 
     & MEMI(p_F_chan),MEMI(p_N_chan),qorder,MEMI(p_order), 
     & MEMR(p_matrix),lo_thresh,status)

		if(status.NE.0) goto 876

	        call FTPKYS(ounit,'CREATOR',
     &  	        taskname,
     &             's/w task which wrote this dataset',
     &          	status)


        else
            message = 'Unknown format: '// rmfversn
            call wterrm(subname, version, message)
            ierr = 1
            goto 482
        endif

c ------------------------ finished RMF EXTENSION ----------------------------
      IF (ebfil.EQ.rmfil) THEN
      IF( rmextn.gt. ebextn) THEN
          goto 505
      ENDIF
      ENDIF

876		continue

504   continue
c ----------------------------- EBOUNDS EXTENSION ----------------------------
	if(rmfversn(1:1).EQ.'1')then
c              ... Write the EBOUNDS extension within the RMF file
        	call wtebd4(ounit, chatter,ebd_extname,ebd_extcomm,
     &          nk_ebhist, ebhist,
     &          nk_comm, comment, rmfversn,
     &          telescop, instrume, detnam, filter, area,
     &		echantyp, fchan,
     &          iebound, MEMR(p_e_min), MEMR(p_e_max), status)

	        call FTPKYS(ounit,'CREATOR',
     &  	        taskname,
     &             's/w task which wrote this dataset',
     &          	status)

		call ftpkys(ounit,'CHANTYPE',
     &		   	rchantyp,
     &		   'Detector Channel Type in use (PHA or PI)',
     &			status)
          	call wtferr(subname, version, status, 
     &			' Problem writing CHANTYPE keyword')
        else
            message = 'Unknown format: '// rmfversn
            call wterrm(subname, version, message)
            ierr = 1
            goto 482
        endif
      IF (ebfil.EQ.rmfil) THEN
      IF( rmextn.gt. ebextn) THEN
          goto 502
      ENDIF
      ENDIF
505   continue 
c ------------------------ finished EBOUNDS EXTENSION ----------------------
c Close the FITS file

	status = 0
        call ftclos(ounit, status) 
	if(status.ne.0) then
          call wtferr(subname, version, status, 
     &		' Problem closing o/p file')
	  ierr = 6
	  goto 482
	endif
c Check for errors
 482	if(ierr.ne.0) then
	  call wterrm(subname, version, ' Fatal - aborting')
	endif

c Free the allocated memory:

        call udmfre(p_ngrp, 4, status)
                if(status.NE.0) goto 646
        call udmfre(p_matrix, 6, status)
                if(status.NE.0) goto 646
        call udmfre(p_F_chan, 4, status)
                if(status.NE.0) goto 646
        call udmfre(p_N_chan, 4, status)
                if(status.NE.0) goto 646
        call udmfre(p_order, 4, status)
                if(status.NE.0) goto 646
        call udmfre(p_re_lo, 6, status)
                if(status.NE.0) goto 646
        call udmfre(p_re_hi, 6, status)
                if(status.NE.0) goto 646
        call udmfre(p_e_min, 6, status)
                if(status.NE.0) goto 646
        call udmfre(p_e_max, 6, status)
                if(status.NE.0) goto 646
	call udmfre(p_factor, 6, status)
                if(status.NE.0) goto 646
	call udmfre(p_chan, 4, status)
                if(status.NE.0) goto 646
	if(.not.qscalar) then
            call udmfre(p_ae_lo, 6, status)
                if(status.NE.0) goto 646
            call udmfre(p_ae_hi, 6, status)
                if(status.NE.0) goto 646
	    call udmfre(p_sprsp, 6, status)
                if(status.NE.0) goto 646
	endif
646     if(status.NE.0) then
                message = ' Failed to deallocate Dynamic Memory'
                call wtwarm(subname, version, chatter, 1,message)
                message = ' (only a potential problem '//
     &                  'if your NOT running the host version)'
	  	call wtinfo(chatter,1,1,message)
        endif

c --------------------------------------------------------------------
	return
	end
c -----------------------------------------------------------
*+ MULTMATRIX

C - old:	subroutine multmatrix(maxchan,rien,ichan,matrix,factor)
C - old:	integer rien, ichan, maxchan, numelt
C - old:	real matrix(maxchan, *), factor(*)

	SUBROUTINE MULTMATRIX(IMAXELT,IMAXGRP,IMAXEN,NGRP,
     &    NCHAN,MATRIX,FACTOR)

	IMPLICIT NONE

	INTEGER IMAXELT,IMAXGRP,IMAXEN,NGRP(IMAXEN),NCHAN(IMAXGRP)
        REAL MATRIX(IMAXELT),FACTOR(IMAXEN)

c  Description
c  Multiplies the matrix by the energy-dependent vector factor
c  Required to make use of dynamic memory alloaction routines
c
c Author/Modification history
c  Ian M George
c  Alex         modified the matrix multiplication to take into account
c                        that the matrix is now 1-D array
*-
c Internals 
	INTEGER I,J,IGRP,IMTRX,ICH, jj
        IGRP  = 0
        IMTRX = 0

        DO I = 1, IMAXEN
           DO J = 1, NGRP(I)
              IGRP = IGRP + 1
                 DO ICH = 1, NCHAN(IGRP)
                    IMTRX = IMTRX + 1
                    if(matrix(imtrx).lt.1.0e-20) then 
                       MATRIX(IMTRX) = 0.0 
                    else 
                       MATRIX(IMTRX) = MATRIX(IMTRX)*FACTOR(I)
                    endif
                 ENDDO
           ENDDO
        ENDDO

CC ------------------------- old version ---------------------
CC	do i = 1, rien
CC	  do jj = 1, ichan
CC		matrix(jj,i) = matrix(jj,i) * factor(i)	    
CC	  enddo
CC	enddo
CC -----------------------------------------------------------

	return
	end
c -----------------------------------------------------------
*+ DIVMATRIXC
C - old:	subroutine divmatrix(maxchan, rien, ichan, matrix, factor)
C - old:	integer rien, ichan, maxchan
C - old:	real matrix(maxchan, *), factor(*)

	SUBROUTINE DIVMATRIX(IMAXELT,IMAXGRP,IMAXEN,NGRP,
     &    NCHAN,MATRIX,FACTOR)

	IMPLICIT NONE

	INTEGER IMAXELT,IMAXGRP,IMAXEN,NGRP(IMAXEN),NCHAN(IMAXGRP)
        REAL MATRIX(IMAXELT),FACTOR(IMAXEN)

c Description
c  Divides the matrix by the energy-dependent vector factor
c  Required to make use of dynamic memory alloaction routines
c
c Author/Modification history
c  Ian M George
c  Alex         modified the matrix division to take into account
c                        that the matrix is now 1-D array
*-
c Internals 

	INTEGER I,J,IGRP,IMTRX,ICH
        IGRP  = 0
        IMTRX = 0

        DO I = 1, IMAXEN
           DO J = 1, NGRP(I)
              IGRP = IGRP + 1
                 DO ICH = 1, NCHAN(IGRP)
                    IMTRX = IMTRX + 1
                      MATRIX(IMTRX) = MATRIX(IMTRX)/FACTOR(I)
                 ENDDO
           ENDDO
        ENDDO

C -----------------  old version ---------------------------
C	do i = 1, rien
C	  do jj = 1, ichan
C		matrix(jj,i) = matrix(jj,i) / factor(i)	    
C	  enddo
C	enddo
C ----------------------------------------------------------

	return
	end
c -----------------------------------------------------------
*+ CHKGRID
	subroutine chkgrid(chatter, rien,aien,re_lo, re_hi,
     &		ae_lo, ae_hi, comment, nk_comm)

	IMPLICIT NONE
	integer rien, aien, nk_comm
	integer chatter
	real re_lo(rien), re_hi(rien)
	real ae_lo(aien), ae_hi(aien)
	character*(*) comment(*)
c Description
c  Checks the energy grid of the RMF & ARF files
c  Required to make use of dynamic memory alloaction routines
c
c Author/Modification history
c  Ian M George
        character(7) version
        parameter (version = '1.0.0')
*-
c Internals 
	character(10) subname
	parameter (subname = 'chkgrid')
	character(80) message

	if(rien.NE.aien .OR. re_lo(1).NE.ae_lo(1) .OR. 
     &		re_hi(rien).NE.ae_hi(aien)) then
	   message = ' RMF/ARF energy grid mismatch'
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	   call wtwarm(subname, version, chatter, 1,message)
	   write(message,'(a,i12,a,f10.5,a,f10.5,a)') 'RMF: ',
     &		rien,' bins between ', re_lo(1), ' & ', re_hi(rien),' keV'
	   call wtinfo(chatter,1,1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	   write(message,'(a,i12,a,f10.5,a,f10.5)') 'ARF: ',
     &		aien,' bins between ', ae_lo(1), ' & ', ae_hi(aien)
	   call wtinfo(chatter,1,1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	   message = 'ARF dataset will be remapped onto RMF grid'
	   call wtinfo(chatter,1,1,message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
	endif

	return
	end

c -----------------------------------------------------------
*+ SCALFILL
	subroutine scalfill(rien, scalar, factor, ierr)

	IMPLICIT NONE
	integer rien, ierr
	real scalar
	real factor(*)

c Description
c  Fills the factor array with the value of scalar
c
c Author/Modification history
c  Ian M George (1.0.0: 1995 Apr 09) original
*-
c Internals 
	integer i

	do i = 1, rien
	   factor(i) = scalar 
	enddo
	ierr = 0

	return
	end
c -----------------------------------------------------------


