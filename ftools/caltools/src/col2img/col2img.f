*+COL2IMG
	subroutine col2ig

	IMPLICIT NONE
c 
c Description:
c     Task to produce a 2-D image in the FITS Primary Array of an
c input Collimator response dataset. The input dataset must be in 
c an OGIP-approved format, and can consist of one of the following:
c   	- an OBSFACT dataset (fn of X & Y)
c	- a COLLRESP dataset (fn of E, X & Y)
c where X & Y represent the spatial coordinates, and E an energy coordinate
c     In the case of energy-dependent datasets, the image is constructed 
c  	by adding the collimator responses over a user-defined energy range
c     In the case of datasets which are not stored on a uniform Cartesian 
c       spatial grid, user-defined parameters control whether the image 
c  	is constructed using the stored grid (and hence will be "distorted"),
c 	or is remapped/rebinned onto a user-defined regular grid.
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None ... isolated GP_COL2IMG (below)
c
c COMMONS/INCLUDES etc
c  common TASK		       : (FTOOLS) standard fatal error message thingy
c 
c Called routines
c  subroutine GP_COL2IMG      : (below) Gets parameters from XPI par file
c  subroutine DO_COL2IMG      : (below) Performs the conversion
c  
c Compilation:
c  subroutines require XPI, CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (0.9.0:1994 Mar 10), quick & v.dirty 
c  Ian M George     (1.0.0:1994 Aug 05), Test version for XTE
c  Ian M George     (2.0.0:1994 Oct 14), Added support for COLLRESP datasets 
c  Ian M George     (2.1.0:1994 Oct 25), Bug fix to COLLRESP energy remapping
c  Ian M George     (2.1.1:1994 Dec 14), cleaned up for the public
c  Rehana Yusaf     (2.1.2:1994 Dec 22), fixed variables that are multiply
c				         defined. Also fixed warnings that
c                                        appeared on legacy, due to
c                                        message = errstr//'...etc
c                                        message and errstr were both defined
c                                        as character(80)
c  Ian M George     (2.2.0:1995 Jun 27), DMA bug-fix in do_col2img & esquish
c  Ian M George     (2.2.1;1995 Jun 28), minor stuff in CK_OBSFACT & CK_COLRESP
c  Ian M George     (2.2.2:1995 Jun 30), added fnd* calls to DO_COL2IMG
c  Ian M George     (2.2.3:1996 Feb 03), added wtinfo & friends
	character(7) version
	parameter (version = '2.2.3')
*- 
c Internals 
        character(40) taskname
	integer chatter, ierr
	character(10) emin, emax
	character(80) infil, outfil
	integer nxbins, nybins
	real xmin, xmax, ymin, ymax
	logical remap, qextrap, clobber
c Initialize
	ierr = 0
	taskname = 'col2img'

c Get Parameters from the par file
	call gp_col2img(infil, outfil, remap, clobber, chatter, 
     &  	xmin, xmax, nxbins, ymin, ymax, nybins, qextrap,
     &		emin, emax,ierr)

c Start-up Main
        call wtbegm(taskname, version, chatter)

c Do the nasty deed	
	call do_col2img(taskname,infil, outfil, remap, clobber, chatter, 
     &  	xmin, xmax, nxbins, ymin, ymax, nybins, qextrap,
     &		emin, emax, version,ierr)
        if(ierr.NE.0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version,ierr,chatter)

	return
	end

c -------------------------------------------------------------------------
*+GP_COL2IMG
	subroutine gp_col2img(infil, outfil, remap, clobber, chatter, 
     &  	xmin, xmax, nxbins, ymin, ymax, nybins, qextrap,
     &     	emin, emax, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nxbins, nybins
	real xmin, xmax, ymin, ymax
	character*(*) infil, outfil, emin, emax
	logical remap, qextrap, clobber
c 
c Description:
c  Gets the parameters required by COL2IMG from the parameter file
c  NOTE - The par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  INFIL       - Input File name/expression
c  OUTFIL      - name of o/p Image file required
c  CLOBBER     - Clobber flag (required by FTOOLS team)
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  IERR	       - Error Flag (zero = OK)
c
c Origin:
c  Original
c
c Called Routines
c  subroutine WT*              : (CALLIB) CALDB-standard writers
c  subroutine UCLG*            : (XPI) returns parameters
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (0.9.0:1994 Mar 10), Limited capability version
c  Ian M George     (1.0.0:1994 Sep 22), Clobber read from Par file
c  Ian M George     (1.1.0:1994 Oct 14), Emin, Emax read from Par file
c  Ian M George     (1.1.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.1.1')
*- 
c Internals
        character(10) subname
        parameter (subname = 'gp_col2img')
        character(50) contxt
c Initialize
	ierr = 0

c Get the name of the input file
	call uclgst('infil',infil, ierr)
        if(ierr.ne.0) then
        	call wterrm(subname, version,
     &            'Problem getting INFIL parameter')
                goto 999
        endif

c Get the name of the output (Image) file
	call uclgst('outfil',outfil, ierr)
	if(ierr.ne.0) then
        	call wterrm(subname, version,
     &            'Problem getting OUTFIL parameter')
                goto 999
	endif

c Get the min & max energies 
	call uclgst('emin',emin, ierr)
	if(ierr.ne.0) then
          call wtwarm(subname, version,1,1,
     &            'Problem getting EMIN parameter')
	  ierr = 0 
	  call wtinfo(1,5,1, 'will use lower boundary stored')
	  emin = '%'	  
	elseif(emin.EQ.' ') then
	  emin = '%'	  
	endif

	call uclgst('emax',emax, ierr)
	if(ierr.ne.0) then
          call wtwarm(subname, version,1,1,
     &            'Problem getting EMAX parameter')
	  ierr = 0
	  call wtinfo(1,5,1, 'will use upper boundary stored')
	  emax = '%'	  
	elseif(emax.EQ.' ') then
	  emax = '%'	  
	endif

c Read in clobber
        call uclgsb('clobber',clobber,ierr)
        if (ierr.NE.0) then
          clobber = .false.
          call wtwarm(subname, version,1,1,
     &            'Problem getting CLOBBER parameter')
	  ierr = 0
	  call wtinfo(1,5,1, 'setting CLOBBER = False')
        endif

c Get the remapping Flag
	call uclgsb('remap',remap, ierr)
	if(ierr.NE.0) then
          call wtwarm(subname, version,1,1,
     &            'Problem getting REMAP parameter')
	  ierr = 0
	  call wtinfo(1,5,1, 'setting REMAP = False')
    	  remap = .false.
	endif	

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
        if(ierr.NE.0) then
          call wtwarm(subname, version, 1, 1,
     &          'Problem getting CHATTER parameter')
                ierr = 0
                call wtinfo(1,1,1, 'setting CHATTER = 10')
                chatter = 10
        endif

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

c Get the extra stuff required when remap reuested
	if(remap) then
	  call uclgsb('extrap',qextrap, ierr)
	  if(ierr.NE.0) then
            call wtwarm(subname, version,1,1,
     &            'Problem getting EXTRAP parameter')
	    ierr = 0
	    call wtinfo(1,5,1, 'setting EXTRAP = False')
	    qextrap = .false.
	  endif	

	call uclgsr('xmin',xmin, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting XMIN parameter')
                goto 999
	endif	
	call uclgsr('xmax',xmax, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting XMAX parameter')
                goto 999
	endif	

	call uclgsi('nxbins',nxbins, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting NXBINS parameter')
                goto 999
	endif	

	call uclgsr('ymin',ymin, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting YMIN parameter')
                goto 999
	endif	
	call uclgsr('ymax',ymax, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting YMAX parameter')
                goto 999
	endif	

	call uclgsi('nybins',nybins, ierr)
	if(ierr.NE.0) then
        	call wterrm(subname, version,
     &            'Problem getting NYBINS parameter')
                goto 999
	endif	

	endif	

999     if(ierr.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

	return
	end

c -------------------------------------------------------------------------
*+DO_COL2IMG
	subroutine do_col2img(taskname, inexp, outfil, remap, clobber,
     &       chatter, xmin, xmax, nxbins, ymin, ymax, nybins, qextrap,
     &       eminstr, emaxstr, mnver,ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nxbins, nybins
	real xmin, xmax, ymin, ymax
	character*(*) inexp, outfil, eminstr, emaxstr,mnver, taskname
	logical remap, qextrap, clobber
c 
c Description:
c  Does the business for COL2IMG
c
c User i/ps required (prompted for):
c  INEXP       - Input File name/expression
c  OUTFIL      - name of o/p Image file required
c  REMAP       - Whether spatial remapping is requested ?
c  CLOBBER     - Clobber flag
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  IERR	       - Error Flag (zero = OK)
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
c  Ian M George     (0.9.0:1994 Mar 10), Limited capability version
c  Ian M George     (1.0.0:1994 Sep 22), Added clobber flag
c  Ian M George     (2.0.0:1994 Oct 14), Added support for COLLRESP datasets 
c  Rehana Yusaf     (2.0.1:1994 Dec 22), mnver passed to this subroutine,
c                                        so that mnver is used for COL2IMG
c                                        version in HISTORY comment
c  Ian M George     (2.0.2:1995 Jun 27), fixes with DMA pointers
c  Ian M George     (2.1.0:1995 Jun 30), inserted fnd* calls
c  Ian M George     (2.1.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '2.1.1')
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
	integer maxextn, maxen
	parameter (maxhist=30,maxcomm=100)
	parameter (maxextn = 10, maxen = 100)
c Internals 
        character(10) subname
        character(160) message
        parameter (subname = 'do_col2img')
	integer decimals, clenact
	integer inextn, status, itemp
	integer iunit, ounit, block
	integer i, htype
	integer nk_hist, nk_comm
	integer bitpix, naxis, gcount, pcount
	integer naxes(2), icoord1, icoord2, ienerg
	integer maxenerg, maxcoord1, maxcoord2
	logical simple, extend
	real emin, emax
	character(5) obfversn,crspvers
	character(20) telescop, instrume, detnam, filter
        character(70) hist(maxhist), comment(maxcomm)
        character(80) dummystr(1)
	character(80) infil, string
	character(20) cunit1, cunit2, eunits
	character(20) cname1, cname2
        integer ninstr, nsearch, nfound
        integer next(maxextn)
        character(20) instr(9), ftype
        character(20) outhdu(9,maxextn), outver(9,maxextn)
	character(20) hduclas(9), hduvers(9)
        character(20) extnam(maxextn)
	logical qokfil, qxlin, qylin
	real delx, dely, ratio

c ... pointers to "arrays" to be dynamically allocated
        integer p_x, p_y, p_energ_lo, p_energ_hi, p_ework
        integer p_x_out, p_y_out
        integer p_collresp, p_image, p_img_out
c ... "arrays" to be dynamically allocated
c	real x(maxpos), 		real	y(maxpos)
c	real energ_lo(maxen)		real 	energ_hi(maxen)
c	real x_out(maxpos), 		real 	y_out(maxpos)
c	real image(maxpos,maxpos), 	real	img_out(maxpos,maxpos)
c	real collresp(maxen,maxpos,maxpos)

c Initialize
	ierr = 0
	status = 0
	decimals = 5

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c Check that the o/p file doesn't already exist or is illegal
        call ck_file(outfil,dummystr, 1, qokfil, clobber, chatter)
        if(.NOT.qokfil) then
                message = 'Offending file is OUTFIL: '// outfil
                call wtinfo(chatter,1,2,message)
                ierr = -1
                goto 482
        endif



c Fill in a few history & comment records
	hist(1) = ' IMAGE constructed by COL2IMG '//mnver
	nk_hist = 1
	comment(1) = ' COL2IMG '//mnver//'Summary:'
	comment(2) = '  I/p file:     '//inexp
	nk_comm = 2
	if(remap) then
	  comment(nk_comm+1) = '  Spatial remapping requested by user'
	  nk_comm = nk_comm+1
	  if(qextrap) then
	    comment(nk_comm+1) = '  (extrapolation beyond spatial '//
     &		'grid allowed by user'
	    nk_comm = nk_comm+1
	  endif
	else
	  comment(nk_comm+1) = 
     &		'  No spatial remapping requested by user'
	  nk_comm = nk_comm+1
	endif


c Translate the name of the input file (taking off any extension)
        call fcpars(inexp,infil,inextn,status)
	if(status.NE.0) then
          call wtwarm(subname, version, chatter,1,
     &		' Problem passing infil expression')
	  call wtinfo(chatter,1, 'will search all extensions')
	  inextn = -99
	endif	

c Open i/p file
       status = 0
      	call cgetlun(iunit)
      	call ftopen(iunit,infil,0,block,status)
      	IF (status.NE.0) THEN
		message = ' opening file: '//infil
        	call wtferr(subname,version, status,message)
		ierr = 1        
		goto 482
      	ENDIF


c GO FIND THE EXTENSION
c  - Extension number IS given as part of infil 
	if(inextn.GE.0) then
	   call ftmrhd(iunit,inextn,htype,status)
	   IF (status.NE.0) THEN
	      call wtferr(subname,version, status,
     &		' Problem moving to specified xtens')
	      ierr = 1
	      goto 482
	   ENDIF
c 	   ... grab the HDUCLAS values for reference
	   ninstr = 1
	   instr(1) = '*'
	   nsearch = 1
           call fndhdu(chatter, iunit, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
	   nfound = 1
	   next(1) = 0
	   ftype = outhdu(2,1)
	   goto 200
	else
c  - Extension SEARCHING required 
c .... Search for OBFFACTOR
	   nsearch = 99
	   call fndobf(chatter, iunit, nsearch,
     &          obfversn, hduclas, hduvers,
     &          ierr)
	   if(ierr.EQ.0) then
	    ftype = hduclas(2)
	    goto 200
	   elseif(ierr.GT.0) then
	    goto 482
	   else
	    ierr = 0	
	   endif
c .... Search for COLLRESP 
	   call fndcol(chatter, iunit, nsearch,
     &          crspvers, hduclas, hduvers,
     &          ierr)
	   if(ierr.EQ.0) then
	    ftype = hduclas(2)
	   else 
	    call wterrm(subname,version,
     &                  ' Unable to locate acceptable extn')
	    goto 482
	   endif
	endif


200	continue


c Check out the input file, getting array sizes
	if(ftype.EQ.'OBSFACTOR') then
	    call ck_obsfact(iunit, chatter, 
     &		obfversn,
     &		icoord1, icoord2, ierr) 
	elseif(ftype.EQ.'COLLRESP') then
	   call ck_colresp(iunit, chatter, 
     &		crspvers,
     & 		ienerg, icoord1, icoord2, ierr) 
	else
	   call wterrm(subname,version,
     &                  ' Unable to locate acceptable extn')
	   ierr = 5
	   goto 482
	endif

c Allocate dynamic memory
        p_x_out = 0
        p_y_out = 0
        p_img_out = 0
        p_x = 0
        p_y = 0
        p_image = 0
        p_energ_lo = 0
        p_energ_hi = 0
        p_ework = 0
        p_collresp = 0

	if(remap) then
		maxcoord1 = MAX(icoord1,nxbins) 
		maxcoord2 = MAX(icoord2,nybins) 
		maxcoord1 = MAX(maxcoord1,50) 
		maxcoord2 = MAX(maxcoord2,50) 
        	call udmget(maxcoord1, 6, p_x_out, status)
        	  if(status.NE.0) goto 765
        	call udmget(maxcoord2, 6, p_y_out, status)
        	  if(status.NE.0) goto 765
        	call udmget(maxcoord1*maxcoord2, 6, p_img_out, status)
        	  if(status.NE.0) goto 765
	else
		maxcoord1 = icoord1
		maxcoord2 = icoord2
	endif
	call udmget(maxcoord1, 6, p_x, status)
	          if(status.NE.0) goto 765
	call udmget(maxcoord2, 6, p_y, status)
	          if(status.NE.0) goto 765
	call udmget(maxcoord1*maxcoord2, 6, p_image, status)
        	  if(status.NE.0) goto 765
	if(ftype.EQ.'COLLRESP') then
	  maxenerg = MAX(ienerg,50)
	  call udmget(maxenerg, 6, p_energ_lo, status)
            if(status.NE.0) goto 765
	  call udmget(maxenerg, 6, p_energ_hi, status)
            if(status.NE.0) goto 765
	  call udmget(maxenerg, 6, p_ework, status)
            if(status.NE.0) goto 765
	  call udmget(maxenerg*maxcoord1*maxcoord2,6, p_collresp,status)
            if(status.NE.0) goto 765
	endif

765       if(status.NE.0) then
	        call wterrm(subname,version,
     &                  ' Failed to allocate Dynamic Memory')
                ierr = -1
                goto 482
	   elseif(chatter.GE.20) then
	      call wtinfo(chatter,10,1,
     &		'Dynamic Memory successfully allocated')
          endif
c *****

c --------------------- Start of DATA reading -------------------
c OBSFACTOR
	if(ftype.EQ.'OBSFACTOR') then
	  call rdobf1(iunit, chatter,
     &          obfversn,
     &          telescop, instrume, detnam, filter, 
     &		cname1, cname2,
     &		maxcoord1,maxcoord2,
     &          icoord1, icoord2, MEMR(p_X), MEMR(p_Y), 
     &		cunit1, cunit2, MEMR(p_image), 
     &		ierr)
	  if(ierr.NE.0) then
	    call wterrm(subname,version,
     &                  ' Reading OBSFACTOR dataset')
	    ierr = 2
	    goto 482
	  endif
	  call post_obf(chatter, 
     &		nk_comm, comment,
     &		cname1, cname2,
     &		cunit1, cunit2,
     &		icoord1, icoord2, MEMR(p_x), MEMR(p_y),
     &		ierr)
	  if(ierr.NE.0) then
	    call wterrm(subname,version,
     &                  ' Manipulating Dataset')
	    ierr = 2
	    goto 482
	  endif

c COLLRESP
	elseif(ftype.EQ.'COLLRESP') then
          call rdcol1(iunit, chatter,
     &          crspvers,
     &          telescop, instrume, detnam, filter,
     &          cname1, cname2,
     &          maxenerg, maxcoord1, maxcoord2,
     &          ienerg, MEMR(p_energ_lo), MEMR(p_energ_hi), eunits,
     &          icoord1, icoord2, MEMR(p_x), MEMR(p_y), 
     &		cunit1, cunit2, MEMR(p_collresp), ierr)
	  if(ierr.NE.0) then
	    call wterrm(subname,version,
     &                  ' Reading COLLRESP dataset')
	    ierr = 2
	    goto 482
	  endif

	  call post_col(chatter, 
     &		nk_comm, comment,
     &		ienerg, MEMR(p_energ_lo), MEMR(p_energ_hi), eunits,
     &		cname1, cname2,
     &		cunit1, cunit2,
     &		icoord1, icoord2, MEMR(p_x), MEMR(p_y),
     &		ierr)
	  if(ierr.NE.0) then
	    call wterrm(subname,version,
     &                  ' Manipulating Dataset')
	    ierr = 2
	    goto 482
	  endif
	else
	  call wterrm(subname,version,
     &          ' Unrecognized/Unsupported HDUCLAS2 value')
	  ierr = 10
	  goto 482
	endif

c --------------------- End of DATA reading -------------------


c Close the FITS file
        call ftclos(iunit, status) 
	if(status.ne.0) then
           call wtferr(subname,version, status,
     &           'Problem closing i/p data file')
	   ierr = 6
	   goto 482
	endif

c Spatial Grid Check out
	  ratio = 1.E-03
c ... Loop around to see whether the spatial grid is regular
	  call ck_sgrid(chatter, 
     &		cname1, cname2,
     &		icoord1, icoord2, MEMR(p_x), MEMR(p_y),
     &		ratio, qxlin, qylin,
     &		delx, dely,
     &		ierr)
	    if(ierr.NE.0) then
	    	  goto 482
	    endif
	   if(qxlin.AND.qylin) then
	     message = ' (linear binning along both axes)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	   elseif(qxlin) then
	     message = ' (linear binning along axis-1)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	     message = 
     &		' (non-linear binning along axis-2)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	   elseif(qylin) then
	     message = 
     &		' (non-linear binning along axis-1)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	     message = ' (linear binning along axis-2)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	   else
	     message = 
     &		' (non-linear binning along both axes)'
	       comment(nk_comm+1) = message 
	       nk_comm = nk_comm + 1
	       call wtinfo(chatter,9,2,message)
	   endif

c Energy Grid check out and compression
c Trap out the OBSFACTOR case & warn users if necessary
	if(ftype.EQ.'OBSFACTOR') then
	  if((eminstr.NE.'%') .AND. (emaxstr.NE.'%')) then
             call wtwarm(subname,version, chatter,1,
     &          'Dataset is an OBSFACTOR')
	     call wtinfo(chatter,1,2,'(No energy dependence)')
	     call wtinfo(chatter,1,2,
     &		'User-defining energy range ignored')
	     nk_comm = nk_comm + 1
	     comment(nk_comm) = message
	     eminstr = '%'
	     emaxstr = '%'
	   endif
	else
	   call ck_egrid(chatter,
     &		nk_comm, comment,
     &		ienerg, MEMR(p_energ_lo), MEMR(p_energ_hi), eunits,
     &		eminstr, emaxstr, emin, emax,
     &		ierr)
	   if(ierr.NE.0) then
	        goto 482
	   endif

	   call esquish(chatter,
     &		maxenerg, ienerg, MEMR(p_energ_lo), MEMR(p_energ_hi), 
     &		MEMR(p_ework),
     &          maxcoord1, maxcoord2, icoord1, icoord2, 
     &		MEMR(p_collresp), 
     &		emin, emax, MEMR(p_image),
     &		ierr)
	   if(ierr.NE.0) then
	        goto 482
	   endif

c ... Add a few comments
	nk_comm = nk_comm+1 
 	comment(nk_comm) = ' I/p dataset was collapsed in '//
     &		'energy-space to construct this image'
	nk_comm = nk_comm+1 
 	write(comment(nk_comm),'(a,f10.5,a,a)')
     &		'    lower energy boundary: ',emin,
     &		' ', eunits
	nk_comm = nk_comm+1 
 	write(comment(nk_comm),'(a,f10.5,a,a)')
     &		'    upper energy boundary: ',emax,
     &		' ', eunits

c ... Dump stuff to screen if requested
	     write(message,'(a,f10.5,a,a)')
     &		' Lower energy boundary in o/p image: ',emin,
     &		' ', eunits
	     call wtinfo(chatter,15,1,message)
	     write(message,'(a,f10.5,a,a)')
     &		' Upper energy boundary in o/p image: ',emax,
     &		' ', eunits
	     call wtinfo(chatter,15,1,message)

	endif



655	continue
c -------------------- Start of Remapping --------------------------------
	if(remap) then
	     call wtinfo(chatter,9,1,
     &		'Spatial Remapping requested')
c	... set up the desired grids 
		call pre_remap(chatter, 
     &			xmin, xmax, nxbins,
     &			ymin, ymax, nybins,
     &			MEMR(p_x_out), MEMR(p_y_out),
     &			ierr)
	    	if(ierr.NE.0) then
	    	  goto 482
	    	endif

c	... perform the remapping 
        	call sremap(chatter,
     &          	maxcoord1, maxcoord2,
     &          	icoord1, icoord2, 
     &			MEMR(p_x), MEMR(p_y), MEMR(p_image),
     &          	maxcoord1, maxcoord2,
     &          	nxbins, nybins, 
     &			MEMR(p_x_out), MEMR(p_y_out), 
     &			MEMR(p_img_out),
     &          	qextrap, ierr)
c	... error out if necessary
	    	if(ierr.NE.0) then
	    	  goto 482
	    	endif
c 	... Fix up all the values for output
		call post_remap(chatter, 
     &			maxcoord1, maxcoord2,
     &			MEMR(p_x_out), MEMR(p_y_out), 
     &			MEMR(p_img_out),
     & 			maxcoord1, maxcoord2, 
     & 			nxbins, nybins, 
     &			MEMR(p_x), MEMR(p_y), MEMR(p_image),    
     &			ierr)
	  if(ierr.NE.0) then
            call wterrm(subname,version,
     &                  'Remapping Dataset')
	    ierr = 2
	    goto 482
	  endif
		icoord1 = nxbins
		icoord2 = nybins
          	qxlin = .true.
          	qylin = .true.
          	delx = (xmax - xmin)/nxbins
        	dely = (ymax - ymin)/nybins

c ... Add a few comments concerning the remapping
	  	comment(nk_comm+1) = 
     &	     	    ' Input dataset was spatially remapped '//
     &	     		'to construct this image'
	  	nk_comm = nk_comm + 1
	  	write(message,'(a,g15.5)')
     &          '    axis-1 min:            ', xmin 
	  	comment(nk_comm+1) = message
	  	write(message,'(a,g15.5)')
     &          '    axis-1 max:            ', xmax
	  	comment(nk_comm+2) = message
	  	write(message,'(a,i4)')
     &          '    # (new) axis-1 bins:   ', nxbins
	  	comment(nk_comm+3) = message
	  	nk_comm = nk_comm + 3
	  	write(message,'(a,g15.5)')
     &          '    axis-2 min:            ', ymin 
	  	comment(nk_comm+1) = message
	  	write(message,'(a,g15.5)')
     &          '    axis-2 max:            ', ymax
	  	comment(nk_comm+2) = message
	  	write(message,'(a,i4)')
     &          '    # (new) axis-2 bins:   ', nybins
	  	comment(nk_comm+3) = message
	  	nk_comm = nk_comm + 3
	  	if(qextrap) then
	  	  comment(nk_comm+1) = 
     &		'    NOTE: extrapolation was enabled'
	  	else
	  	  comment(nk_comm+1) = 
     &		'    NOTE: extrapolation was disabled'
	  	endif
	  	nk_comm = nk_comm + 1

c 	Write user info
	   message = ' output image: '// 
     &		cname1(:MIN(10,clenact(cname1))) //
     &		' vs '//
     &		cname2(:MIN(10,clenact(cname2)))
	   call wtinfo(chatter,9,1,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		Xmin, ':', xmax
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &		cname1(:MIN(10,clenact(cname1))), ' has ',
     &		nxbins, ' elements (', string(:clenact(string)),
     &		') ', cunit1
	   call wtinfo(chatter,9,2,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		ymin, ':', Ymax
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &	  	cname2(:MIN(10,clenact(cname2))), ' has ',
     &		nybins, ' elements (', string(:clenact(string)),
     &		') ', cunit2
	   call wtinfo(chatter,9,2,message)
	   call wtinfo(chatter,9,2,
     &		'(linear binning along both axes)')

	endif
c -------------------- End of Remapping --------------------------------



c --------------------- Start OUTPUT File ------------------------------


c OPEN THE IMAGE FILE
	block = 2880
	call cgetlun(ounit)
        call opfits(ounit,outfil,clobber,chatter,status)
	if(status.NE.0) then
          call wtferr(subname,version, status,
     &                  ' Opening outfile')
	  ierr = 9
	  goto 482
	endif
	call wtinfo(chatter,20,1,'Opened the Output File')

	simple = .true.
	bitpix = -32
	naxis = 2
	naxes(1) = icoord1
	naxes(2) = icoord2
	pcount = 0
	gcount = 1
	extend = .true.
	call ftphpr(ounit,simple,bitpix,naxis,naxes,pcount,
     &		gcount,extend,status)
	if(status.NE.0) then
          call wtferr(subname,version, status,
     &                  ' With FITSIO/FTPHPR call')
	  ierr = 9
	  goto 482
	endif
	call wtinfo(chatter,20,1,
     &		' Defined the Primary Header Data structure')

c Write a bunch of keywords
	status = 0
	call ftpkys(ounit,'CREATOR',taskname,
     &		's/w task which wrote this dataset',
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing CREATOR keyword')
	  status = 0	   
	endif

c ... The Coordinate Keywords
	call wt_ckys(chatter, ounit,
     &		cname1, cunit1, qxlin, delx, MEMR(p_x),
     &		cname2, cunit2, qylin, dely, MEMR(p_y),
     &		ierr)
	
c ... The Mission/Instrument Keywords
	status = 0 
	call ftpkys(ounit,'TELESCOP',telescop,
     &		'Mission/Satellite Name', 
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing TELESCOP keyword')
	  status = 0	   
	endif

	call ftpkys(ounit,'INSTRUME',instrume,
     &		'Instrument Name', 
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing INSTRUME keyword')
	  status = 0	   
	endif

	if(detnam.NE.' '.OR.detnam.NE.'NONE') then
	   call ftpkys(ounit,'DETNAM',detnam,
     &		'Sub-Instrument Name', 
     &		status)
	   if(status.ne.0) then
             call wtwarm(subname,version, chatter,1,
     &          'problem writing DETNAM keyword')
	     status = 0	   
	   endif
	endif

c ... The HDUCLAS/VERS Keywords
	status = 0 
	call ftpkys(ounit,'HDUCLASS','OGIP',
     &		'Format conforms to OGIP Standards',
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing HDUCLASS keyword')
	  status = 0	   
	endif

	status = 0
	call ftpkys(ounit,'HDUCLAS1','IMAGE',
     &		'Dataset is an Image',
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing HDUCLAS1 keyword')
	  status = 0	   
	endif

	status = 0
	call ftpkys(ounit,'HDUVERS1','1.0.0',
     &		'Version of family of formats',
     &		status)
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem writing HDUVERS1 keyword')
	  status = 0	   
	endif

c Add the energy bounds if appropriate
	if(ftype.EQ.'COLLRESP') then
	  status = 0
	  string = 'Lower energy bound of image ('//
     &		eunits(:clenact(eunits))//')'
	  call ftpkye(ounit,'ENERG_LO',emin, decimals,
     &		string,
     &		status)
	  if(status.ne.0) then
             call wtwarm(subname,version, chatter,1,
     &          'problem writing ENERG_LO keyword')
	     status = 0	   
	  endif
	  string = 'Upper energy bound of image ('//
     &		eunits(:clenact(eunits))//')'
	  call ftpkye(ounit,'ENERG_HI',emax, decimals,
     &		string,
     &		status)
	  if(status.ne.0) then
             call wtwarm(subname,version, chatter,1,
     &          'problem writing ENERG_HI keyword')
	     status = 0	   
	  endif
	endif


c Add the (passed) history cards
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
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem putting at least one HISTORY record')
	  status = 0	   
	endif
	call wtinfo(chatter,20,1, 'written the history keywords')
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
	if(status.ne.0) then
          call wtwarm(subname,version, chatter,1,
     &          'problem putting at least one COMMENT record')
	  status = 0	   
	endif
	call wtinfo(chatter,20,1, 'written the comment keywords')
        status = 0


c Define the data structure
	status = 0
	call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
	if(status.NE.0) then
          call wtferr(subname,version, status,
     &                  ' With FITSIO/FTPDEF call')
	  ierr = 9
	  goto 482
	endif
	call wtinfo(chatter,20,1,
     &		' Data structure has been defined')

c Write the Data
	status = 0
	call ftp2de(ounit,0,icoord1,naxes(1),naxes(2), 
     &		MEMR(p_image), status)
	if(status.NE.0) then
	  call wtferr(subname,version, status,
     &                  ' Writing the data')
	  ierr = 9
	  goto 482
	endif
	call wtinfo(chatter,20,1,
     &          ' data written successfully')

c Close the FITS file
	status = 0
	call ftclos(ounit,status)
	if(status.ne.0) then
           call wtferr(subname,version, status,
     &           'Problem closing o/p data file')
	   ierr = 6
	   goto 482
	endif
	
c --------------------- End of OUTPUT File ------------------------------


c Check for errors
482	if(ierr.ne.0) then
		call wterrm(subname, version,'Fatal')
	endif

c DeAllocate the dynamic memory

	if(remap) then
        	call udmfre(p_x_out, 6, status)
        	  if(status.NE.0) goto 766
        	call udmfre(p_y_out, 6, status)
        	  if(status.NE.0) goto 766
        	call udmfre(p_img_out, 6, status)
        	  if(status.NE.0) goto 766
	endif
        call udmfre(p_x, 6, status)
          if(status.NE.0) goto 766
        call udmfre(p_y, 6, status)
          if(status.NE.0) goto 766
        call udmfre(p_image, 6, status)
          if(status.NE.0) goto 766

	if(ftype.EQ.'COLLRESP') then
	  call udmfre(p_energ_lo, 6, status)
            if(status.NE.0) goto 766
	  call udmfre(p_energ_hi, 6, status)
            if(status.NE.0) goto 766
	  call udmfre(p_ework, 6, status)
            if(status.NE.0) goto 766
	  call udmfre(p_collresp, 6, status)
            if(status.NE.0) goto 766
	endif

766       if(status.NE.0) then
		call wterrm(subname, version,
     &          ' Failed to deallocate Dynamic Memory')
                ierr = 99
	  else
		call wtinfo(chatter,20,1,
     &		' ... Dynamic Memory successfully deallocated')
          endif
c *****


	return
	end

c -------------------------------------------------------------------------
*+CK_OBSFACT
	subroutine ck_obsfact(iunit, chatter, 
     &		obfversn,
     &		nix, niy, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer nix, niy
	character*(*) obfversn
c 
c Description:
c  Checks out an OBSFACT extension in one of the formats 
c  conforming to the HDUVERS2='1.*.*' family (for DMA purposes)
c  !!! Note !!!! File is left open at the end  
c
c Passed parameters
c  iunit         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  OBFVERSN      i   : String denoting OGIP HDUVERS2 family
c  NIX		 i   : Number of values for coordinate-1
c  NIY		 i   : Number of values for coordinate-2
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine FCECHO		(FTOOLS) Writes to standard o/p
c  subroutine FTGBCL		(FITSIO)
c  subroutine FTGCNO		(FITSIO)
c  subroutine FTGHBN		(FITSIO)
c  subroutine FTGKYS		(FITSIO)
c  subroutine GT_CSYSNMS	(CALLIB) Gets coordinate sytem column names
c  subroutine WT_FERRMSG	(CALLIB) Standard error string writer
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0;94 Oct 17), original
c  Ian M George     (1.0.1;95 Jun 28), replaced GT_CSYSNMS w/ GCSYNM
c  Ian M George     (1.0.2:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.2')
*- 
c Internals
        character(10) subname
        parameter (subname = 'ck_obsfact')
	character(20) dummy20
	double precision doubdum
	integer status, decimals, nfields
	integer irows, ncols, ivar
	integer colnum, enull, clenact
        parameter (nfields=3, decimals=6)
	character(8) dummy8
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) csystem, col1, col2
	character(40) comm
	character(80) message
c Initialize
	ierr = 0
	status = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c --- READING KEYWORD VALUES ---

      status = 0
      call ftghbn(iunit,nfields,irows,ncols,ttype,tform,tunits,
     &            comm,ivar,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version, status,
     &          ' reading binary header info')
        ierr = 2
        goto 998
      ENDIF

c OBFVERSN ...
      obfversn = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',obfversn,comm,status)
      IF(status.ne.0) then
	 call wtferr(subname,version, status,
     &          ' reading HDUVERS2 keyword') 
	 status = 0
      ENDIF
      IF (obfversn.EQ.'  ') THEN
        call ftgkys(iunit,'OBFVERSN',obfversn,comm,status)
	if(status.ne.0) then
	 call wtferr(subname,version, status,
     &          ' reading OBFVERSN keyword') 
	 status = 0
        ENDIF
      ENDIF

c Coordinate system in use
      status = 0
      call ftgkys(iunit,'CSYSNAME',csystem,comm,status)
      call wtferr(subname,version, status,
     &          ' reading CSYSNAME keyword')
      IF (status.EQ.202) THEN
         csystem = 'XMA_POL'
         call wtinfo(chatter,20,2,'assuming CSYSNAME = XMA_POL')
      elseif(status.ne.0) then
         csystem = 'XMA_POL'
        call wtwarm(subname,version, chatter,1,
     &          'Setting CSYSNAME = XMA_POL')
      ENDIF

c Sort out the coordinate system & column names
        call gcsynm(chatter, 0, csystem,
     &          col1, col2, dummy8, dummy8, ierr)
        IF (ierr.NE.0) goto 998

c --- READ IN THE DATA
c ... The spatial grid
c ...... Coord-1
      		status = 0
      		call ftgcno(iunit,.false.,col1,colnum,status)
                If (status.NE.0) THEN
                  message = col1(:clenact(col1))//
     &                  ' column not present'
                  call wtferr(subname,version, status, message)
                  ierr = 4
                  goto 998
                ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			nix, doubdum,doubdum,enull,dummy20,status)
      		IF (status.NE.0) THEN
                  call wtferr(subname,version, status,
     &              ' reading Coord-1 dataset (FITSIO/FTGBCL)')
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... Coord-2
      		status = 0
      		call ftgcno(iunit,.false.,col2,colnum,status)
      		If (status.NE.0) THEN
         	  message = col2(:clenact(col2))// 
     &			' column not present'
                  call wtferr(subname,version, status, message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			niy, doubdum,doubdum,enull,dummy20,status)
      		IF (status.NE.0) THEN
                  call wtferr(subname,version, status,
     &              ' reading Coord-2 dataset (FITSIO/FTGBCL)')
        	  ierr = 1
		  goto 998
      		ENDIF


c -----
998     if(ierr.NE.0) then
          call wterrm(subname, version,' Punting')
        endif

	return
	end
c -------------------------------------------------------------------------
*+CK_COLRESP
	subroutine ck_colresp(iunit, chatter, 
     &		crspvers,
     & 		nen, nix, niy, ierr) 

	IMPLICIT NONE
	integer chatter, ierr
	integer iunit
	integer nix, niy, nen
	character*(*) crspvers
c 
c Description:
c  Checks a COLLRESP extension in one of the formats 
c  conforming to the HDUVERS2='1.*.*' family (for DMA)
c  !!! Note !!!! File is left open at the end  
c
c Passed parameters
c  iunit         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  CRSPVERS        o : String denoting OGIP HDUVERS2 family
c  NEN		   o : Number of values in energy arrays
c  NIX		   o : Number of values for coordinate-1
c  NIY		   o : Number of values for coordinate-2
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine FCECHO		(FTOOLS) Writes to standard o/p
c  subroutine FTGBCL		(FITSIO)
c  subroutine FTGCNO		(FITSIO)
c  subroutine FTGHBN		(FITSIO)
c  subroutine FTGKYS		(FITSIO)
c  subroutine GT_CSYSNMS	(CALLIB) Gets coordinate sytem column names
c  subroutine WT_FERRMSG	(CALLIB) Standard error string writer
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0;1994 Oct 17), original
c  Ian M George     (1.0.1;1995 Jun 28), replaced GT_CSYSNMS w/ GCSYNM
c  Ian M George     (1.0.2:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.2')
*- 
c Internals
        character(10) subname
        parameter (subname = 'ck_colresp')
	character(20) dummy20
	double precision doubdum
	integer status, decimals, nfields
	integer irows, ncols, ivar
	integer colnum, enull, clenact
        parameter (nfields=3, decimals=6)
	character(8) dummy8
	character(16) ttype(nfields), tform(nfields), tunits(nfields)
	character(20) csystem, col1, col2
	character(40) comm
	character(80) message
c Initialize
	ierr = 0
	status = 0

c Give user info if requested
        message = ' using '//subname//' '//version
        call wtinfo(chatter,20,1,message)

c --- READING KEYWORD VALUES ---

      status = 0
      call ftghbn(iunit,nfields,irows,ncols,ttype,tform,tunits,
     &            comm,ivar,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version, status,
     &          ' reading binary header info')
        ierr = 2
        goto 998
      ENDIF
      IF (irows.GT.1) then
        call wtwarm(subname, version, 1, 1,
     &          ' Dataset contains more than one row')
      ENDIF

c CRSPVERS ...
      crspvers = '  '
      status = 0
      call ftgkys(iunit,'HDUVERS2',crspvers,comm,status)
      IF(status.ne.0) then
	 call wtferr(subname,version, status,
     &          ' reading HDUVERS2 keyword') 
	 status = 0
      ENDIF
      IF (crspvers.EQ.'  ') THEN
        call ftgkys(iunit,'CRSPVERS',crspvers,comm,status)
	if(status.ne.0) then
	 call wtferr(subname,version, status,
     &          ' reading CRSPVERS keyword') 
	 status = 0
        ENDIF
      ENDIF

c Coordinate system in use
      status = 0
      call ftgkys(iunit,'CSYSNAME',csystem,comm,status)
      call wtferr(subname,version, status,
     &          ' reading CSYSNAME keyword')
      IF (status.EQ.202) THEN
         csystem = 'XMA_POL'
         call wtinfo(chatter,20,2,'assuming CSYSNAME = XMA_POL')
      elseif(status.ne.0) then
         csystem = 'XMA_POL'
        call wtwarm(subname,version, chatter,1,
     &          'Setting CSYSNAME = XMA_POL')
      ENDIF

c Sort out the coordinate system & column names
        call gcsynm(chatter, 0, csystem,
     &          col1, col2, dummy8, dummy8, ierr)
        IF (ierr.NE.0) goto 998

c --- READ IN THE DATA
c ... The energy grid
c ...... Lower bound
      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      		If (status.NE.0) THEN
		  call wtinfo(chatter,15,2,
     &	'ENERG_LO column not present (will assume its a keyword)')
		  nen = 1
		  goto 123
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			nen, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		IF (status.NE.0) THEN
		  call wterrm(subname,version,
     &			'problem reading ENERG_LO dataset')
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... Upper bound
123      		status = 0
      		call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      		If (status.NE.0) THEN
		  call wtinfo(chatter,15,2,
     &	'ENERG_HI column not present (will assume its a keyword)')
		  nen = 1
	  	  goto 124
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			nen, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		IF (status.NE.0) THEN
		  call wterrm(subname,version,
     &			'problem reading ENERG_HI dataset')
        	  ierr = 1
		  goto 998
      		ENDIF
c ... The spatial grid
c ...... Coord-1
124      	status = 0
      		call ftgcno(iunit,.false.,col1,colnum,status)
      		If (status.NE.0) THEN
         	  message = col1(:clenact(col1))// 
     &			' column not present'
         	  call wtferr(subname,version, status, message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			nix, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		IF (status.NE.0) THEN
                  call wtferr(subname,version, status,
     &              ' reading Coord-1 dataset (FITSIO/FTGBCL)')
        	  ierr = 1
		  goto 998
      		ENDIF

c ...... Coord-2
      		status = 0
      		call ftgcno(iunit,.false.,col2,colnum,status)
      		If (status.NE.0) THEN
         	  message = col2(:clenact(col2))// 
     &			' column not present'
         	  call wtferr(subname,version, status, message)
         	  ierr = 4
		  goto 998
      		ENDIF
		call ftgbcl(iunit, colnum, dummy20, dummy20, dummy20,
     &			niy, doubdum,doubdum,enull,dummy20,status)
      		enull = 0
      		IF (status.NE.0) THEN
                  call wtferr(subname,version, status,
     &              ' reading Coord-2 dataset (FITSIO/FTGBCL)')
        	  ierr = 1
		  goto 998
      		ENDIF

c -----


998     if(ierr.NE.0) then
          call wterrm(subname, version,' Punting')
        endif

	return
	end

c -------------------------------------------------------------------------
*+SREMAP
	subroutine sremap(chatter, 
     &		msizx_in, msizy_in,
     &		nx_in, ny_in, x_in, y_in, img_in,
     &		msizx_out, msizy_out,
     &		nx_out, ny_out, x_out, y_out, img_out,
     &		qextrap, ierr)
	IMPLICIT NONE
	integer chatter, ierr
	integer msizx_in, msizy_in
	integer msizx_out, msizy_out
	integer nx_in, ny_in
	integer nx_out, ny_out
	real x_in(*), y_in(*)
	real img_in(msizx_in,msizy_in)
	real x_out(*), y_out(*)
	real img_out(msizx_out,msizy_out)
	logical qextrap	

c Description
c  Remaps spatial coordinate grid onto a new (linear) grid - extracted 
c from DO_COL2IMG for dynamic memory allocation reasons
c  
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MSIZX_IN	 i   : max size of x-dimensions of input arrays
c  MSIZY_IN	 i   : max size of y-dimensions of input arrays
c  NX_IN	 i   : No elements in x-dimensions of input arrays
c  NY_IN	 i   : No elements in y-dimensions of input arrays
c  X_IN		 i   : Input X-coord grid points array (for IMG_IN)
c  Y_IN		 i   : Input Y-coord grid points array (for IMG_IN)
c  IMG_IN	 i   : Input Image
c  MSIZX_OUT	 i   : max size of x-dimensions of output arrays
c  MSIZY_OUT	 i   : max size of y-dimensions of output arrays
c  NX_OUT	 i   : No elements in x-dimensions of output arrays
c  NY_OUT	 i   : No elements in y-dimensions of output arrays
c  X_OUT	 i   : Requested remapped X-coord grid points array (4 IMG_OUT)
c  Y_OUT	 i   : Requested remapped Y-coord grid points array (4 IMG_OUT)
c  IMG_OUT	   o : Remapped Output Image
c  QEXTRAP       i   : Flag as to whether to extrapolate oustide X_IN,Y_IN 
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c  subroutine BILINT		(CALLIB) performs bilinear interpolation
c  subroutine CLENACT 		(CALLIB) returns actual length of a string
c  subroutine CRMVBLK		(CALLIB) removes blanks from a string
c  subroutine FCECHO		(FTOOLS) writes to Standard Output
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1994 Oct 17), original
c  Ian M George     (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*- 
c Internals
        character(6) subname
        parameter (subname = 'sremap')
	integer i,j, clenact
	real value
	character(20) string
	character(80) message
	logical qprob
c Initialize
	ierr = 0
	qprob = .false.
	
c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c Check for sillies
c ... Funny inputs
	if((nx_in.GT.msizx_in).or.(ny_in.GT.msizy_in))then
	   call wtwarm(subname, version, chatter,1,
     &		' Incompatible array sizes')
           write(string,'(i12,a,i12)') msizx_in,'x',msizy_in
	   call crmvblk(string)
	   message = ' I/p image dimensions     : '//
     &		string(:clenact(string))
	   call wtinfo(chatter,1,2,message)
           write(string,'(i12,a,i12)') nx_in,'x',ny_in
	   call crmvblk(string)
	   message = ' I/p array grid dimensions: '//
     &		string(:clenact(string))
	   call wtinfo(chatter,1,2,message)
	   call wtinfo(chatter,1,1,
     &		' Attempting to continue using reduced grid')
	   if(nx_in.GT.msizx_in) nx_in = msizx_in
	   if(ny_in.GT.msizy_in) ny_in = msizy_in
	endif
c ... Funny outputs
	if((nx_out.GT.msizx_out).or.(ny_out.GT.msizy_out))then
	   call wterrm(subname,version,
     &		' Requested dimensions too big')
           write(string,'(i12,a,i12)') msizx_out,'x',msizy_out
	   call crmvblk(string)
	   message = ' Max allowed O/p image dimensions: '//
     &		string(:clenact(string))
	   call wtinfo(chatter,1,1,message)
           write(string,'(i12,a,i12)') nx_out,'x',ny_out
	   call crmvblk(string)
	   message = ' Requested O/p image dimensions  : '//
     &		string(:clenact(string))
	   call wtinfo(chatter,1,2,message)
	   ierr = 1 
	   goto 999
	endif

c ... Perform the re-mapping
	    do j = 1, ny_out
		do i = 1, nx_out
		   call bilint(chatter, x_out(i), y_out(j), 
     &			msizx_in, msizy_in,
     &          	nx_in, ny_in, X_in, Y_in, img_in, 
     &			qextrap, value, ierr)
		   if(ierr.EQ.0) then
			img_out(i,j) = value
		   else
			img_out(i,j) = -99
			if(ierr.NE.-1) then
			    qprob = .true.
		        endif
		   endif
	  	enddo
	    enddo
	
c Dump info if desired
	  if(qprob) call wtwarm(subname,version,chatter,2,
     &		'At least one problem with remapping')

	  if(.NOT.qprob) then
	    ierr = 0
	    call wtinfo(chatter,15,1,
     &		'successfully remapped the dataset')
	  endif

c Error checking
999	if(ierr.NE.0) then
          call wterrm(subname, version,'Aborting')
        endif

	return
	end
c -------------------------------------------------------------------------
*+PRE_REMAP
	subroutine pre_remap(chatter, 
     &		xmin, xmax, nxbins,
     &		ymin, ymax, nybins,
     &		x_out, y_out,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nxbins, nybins
	real xmin, xmax, ymin, ymax
	real x_out(*), y_out(*)

c Description 
c  Pathetic little guy to populate the X & Y grids 
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  XMIN		 i   : Min X-coord of remapped grid
c  XMAX		 i   : Max X-coord of remapped grid
c  NXBINS	 i   : No X-coord elements  remapped grid
c  YMIN		 i   : Min Y-coord of remapped grid
c  YMAX		 i   : Max Y-coord of remapped grid
c  NYBINS	 i   : No Y-coord elements  remapped grid
c  X_OUT	   o : Requested remapped X-coord grid points array (4 IMG_OUT)
c  Y_OUT	   o : Requested remapped Y-coord grid points array (4 IMG_OUT)
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  None
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0:1994 Oct 18) original
c  Ian M George  (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(9) subname
        parameter (subname = 'pre_remap')
	integer i
	real delx_tmp, dely_tmp
	character(80) message
c Initialize
	ierr = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Just do it
	delx_tmp = (xmax - xmin)/nxbins
        dely_tmp = (ymax - ymin)/nybins
        do i = 1, nxbins
            x_out(i) = xmin + (i-1)*delx_tmp
        enddo
        do i = 1, nybins
            y_out(i) = ymin + (i-1)*dely_tmp
        enddo

	return
	end


c -------------------------------------------------------------------------
*+POST_REMAP
	subroutine post_remap(chatter, 
     &		msizx_in, msizy_in,
     &		x_in, y_in, img_in,
     &		msizx_out, msizy_out,
     & 		nx_out, ny_out, 
     &		x_out, y_out, img_out,    
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer msizx_in, msizy_in
	integer msizx_out, msizy_out
	integer nx_out, ny_out
	real x_in(*), y_in(*)
	real img_in(msizx_in,msizy_in)
	real x_out(*), y_out(*)
	real img_out(msizx_out,msizy_out)

c Description 
c  Pathetic little guy to populate the X & Y grids 
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  MSIZX_IN	 i   : max size of x-dimensions of input arrays
c  MSIZY_IN	 i   : max size of y-dimensions of input arrays
c  X_IN		 i   : Input X-coord grid points array (for IMG_IN)
c  Y_IN		 i   : Input Y-coord grid points array (for IMG_IN)
c  IMG_IN	 i   : Input Image
c  MSIZX_OUT	 i   : max size of x-dimensions of output arrays
c  MSIZY_OUT	 i   : max size of y-dimensions of output arrays
c  NX_OUT	 i   : No elements in x-dimensions of output arrays
c  NY_OUT	 i   : No elements in y-dimensions of output arrays
c  X_OUT	 i   : Requested remapped X-coord grid points array (4 IMG_OUT)
c  Y_OUT	 i   : Requested remapped Y-coord grid points array (4 IMG_OUT)
c  IMG_OUT	   o : Remapped Output Image
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  None
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0:1994 Oct 18) original
c  Ian M George  (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(10) subname
        parameter (subname = 'post_remap')
	integer i, j
	character(80) message
c Initialize
	ierr = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Just do it
          do i = 1, nx_out
          	X_out(i) = x_in(i)
          enddo
          do j = 1, nx_out
          	Y_out(j) = y_in(j)
          enddo
          do j = 1, ny_out
          	do i = 1, nx_out
          		img_out(i,j) = img_in(i,j)
          	enddo
          enddo


	return
	end
c -------------------------------------------------------------------------
*+POST_OBF
	subroutine post_obf(chatter, 
     &		nk_comm, comment,
     &		cname1, cname2,
     &		cunit1, cunit2,
     &		icoord1, icoord2, x, y,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nk_comm
	integer icoord1, icoord2
	real x(*), y(*)
	character*(*) comment(*)
	character*(*) cname1, cname2
	character*(*) cunit1, cunit2

c Description 
c  Pathetic little guy to dump some junk to screen & comment cards
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  subroutine CLENACT 		(CALLIB) returns actual length of a string
c  subroutine FCECHO		(FTOOLS) writes to Standard Output
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 18) original
c  Ian M George  (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(8) subname
        parameter (subname = 'post_obf')
	integer clenact
	character(80) message, string

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c 	Write user info
	   message = 'input image: '// 
     &		cname1(:MIN(10,clenact(cname1))) //
     &		' vs '//
     &		cname2(:MIN(10,clenact(cname2))) //
     &		' (OBSFACTOR dataset)'
	   call wtinfo(chatter,9,1,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		X(1), ':', X(icoord1)
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &		cname1(:MIN(10,clenact(cname1))), ' has ',
     &		icoord1, ' elements (', string(:clenact(string)),
     &		') ', cunit1
	   call wtinfo(chatter,9,2,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		y(1), ':', Y(icoord2)
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &		cname2(:MIN(10,clenact(cname2))), ' has ',
     &		icoord1, ' elements (', string(:clenact(string)),
     &		') ', cunit2
	   call wtinfo(chatter,9,2,message)

c 	Add a few comments
	  comment(nk_comm+1) = 
     &		' COL2IMG identified i/p dataset as an OBSFACTOR'
	  nk_comm = nk_comm + 1
	  comment(nk_comm+1) = 
     &          '    axis-1 label:   '// cname1 
	  comment(nk_comm+2) = 
     &          '    axis-1 units:   '// cunit1 
	  write(message,'(g15.5,a,g15.5)')
     & 		X(1), ':', X(icoord1)
	  call crmvblk(message)
	  comment(nk_comm+3) = 
     & 		'    axis-1 range:   '//message 
	  nk_comm = nk_comm + 3
	  comment(nk_comm+1) = 
     &          '    axis-2 label:   '// cname2 
	  comment(nk_comm+2) = 
     &          '    axis-2 units:   '// cunit2 
	  write(message,'(g15.5,a,g15.5)')
     & 		Y(1), ':', X(icoord2)
	  call crmvblk(message)
	  comment(nk_comm+3) = 
     & 		'    axis-2 range:   '//message 
          nk_comm = nk_comm + 3

	return
	end
c -------------------------------------------------------------------------
*+POST_COL
	subroutine post_col(chatter, 
     &		nk_comm, comment,
     &		ienerg, energ_lo, energ_hi, eunits,
     &		cname1, cname2,
     &		cunit1, cunit2,
     &		icoord1, icoord2, x, y,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nk_comm
	integer icoord1, icoord2, ienerg
	real x(*), y(*)
	real energ_lo(*), energ_hi(*)
	character*(*) comment(*)
	character*(*) cname1, cname2
	character*(*) cunit1, cunit2, eunits

c Description 
c  Pathetic little guy to dump some junk to screen & comment cards
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  subroutine CLENACT 		(CALLIB) returns actual length of a string
c  subroutine FCECHO		(FTOOLS) writes to Standard Output
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 18) original
c  Ian M George  (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(8) subname
        parameter (subname = 'post_col')
	integer clenact
	character(80) message, string

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c 	Write user info
	   message = ' input image: '// 
     &		cname1(:MIN(10,clenact(cname1))) //
     &		' vs '//
     &		cname2(:MIN(10,clenact(cname2))) //
     &		' (COLLRESP dataset)'
	   call wtinfo(chatter,9,1,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		X(1), ':', X(icoord1)
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &		cname1(:MIN(10,clenact(cname1))), ' has ',
     &		icoord1, ' elements (', string(:clenact(string)),
     &		') ', cunit1
	   call wtinfo(chatter,9,2,message)

	   write(string,'(g15.5,a,g15.5)')
     & 		y(1), ':', Y(icoord2)
	   call crmvblk(string)
	   write(message,'(a,a, i4,a,a,a,a)')
     &		cname2(:MIN(10,clenact(cname2))), ' has ',
     &		icoord1, ' elements (', string(:clenact(string)),
     &		') ', cunit2
	   call wtinfo(chatter,9,2,message)

c 	Add a few comments
	  comment(nk_comm+1) = 
     &		' COL2IMG identified i/p dataset as a COLLRESP'
	  nk_comm = nk_comm + 1

	  write(message,'(g15.5,a,g15.5)')
     & 		energ_lo(1), ':', energ_hi(ienerg)
	  call crmvblk(message)
	  comment(nk_comm+1) = 
     & 		'    energy range:   '//message//' '//
     &		eunits(:clenact(eunits))
	  nk_comm = nk_comm + 1

	  comment(nk_comm+1) = 
     &          '    axis-1 label:   '// cname1 
	  comment(nk_comm+2) = 
     &          '    axis-1 units:   '// cunit1 
	  write(message,'(g15.5,a,g15.5)')
     & 		X(1), ':', X(icoord1)
	  call crmvblk(message)
	  comment(nk_comm+3) = 
     & 		'    axis-1 range:   '//message 
	  nk_comm = nk_comm + 3

	  comment(nk_comm+1) = 
     &          '    axis-2 label:   '// cname2 
	  comment(nk_comm+2) = 
     &          '    axis-2 units:   '// cunit2 
	  write(message,'(g15.5,a,g15.5)')
     & 		Y(1), ':', X(icoord2)
	  call crmvblk(message)
	  comment(nk_comm+3) = 
     & 		'    axis-2 range:   '//message 
          nk_comm = nk_comm + 3


	return
	end
c -------------------------------------------------------------------------
*+CK_SGRID
	subroutine ck_sgrid(chatter, 
     &		cname1, cname2,
     &		icoord1, icoord2, x, y,
     &		ratio, qxlin, qylin,
     &		delx, dely,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer icoord1, icoord2
	real x(*), y(*)
	real ratio
	character cname1, cname2
	logical qxlin, qylin

c Description 
c  Pathetic little guy to checkout the spatial grid
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  None
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 18) original
c  Ian M George  (2.0.0: 94 Dec 14) addede delx & dely as passed parameters
c  Ian M George  (2.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '2.0.1')
*-
c Internals
        character(8) subname
        parameter (subname = 'ck_sgrid')
	integer i, clenact
	real delta, delx, dely
	character(80) message
c Initialization 
	ierr = 0
	qxlin = .true.
	qylin = .true.

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Do it
	  if(icoord1.LE.1) then
	    message = 'Only 1 element along ' // 
     &			cname1(:clenact(cname1))
	    call wtwarm(subname,version,chatter,1,message)
	    goto 654
	  endif
	  delx = X(2) - X(1)
	  do i = 2, icoord1
		delta = X(i) - X(i-1)
		if(MOD(delta,delx).GT.(delx/ratio)) then
		   qxlin = .false.
		   goto 654
		endif		     
	  enddo

654	  if(icoord2.LE.1) then
	    message = 'Only 1 element along ' // 
     &			cname2(:clenact(cname2))
	    call wtwarm(subname,version,chatter,1,message)
	    goto 655
	  endif
	  dely = Y(2) - Y(1)
	  do i = 2, icoord2
		delta = Y(i) - Y(i-1)
		if(MOD(delta,dely).GT.(dely/ratio)) then
		   qylin = .false.
		   goto 655
		endif		     
	  enddo

655	continue
	return
	end
c -------------------------------------------------------------------------
*+ WT_CKYS
	subroutine wt_ckys(chatter, ounit,
     &		cname1, cunit1, qxlin, delx, x,
     &		cname2, cunit2, qylin, dely, y,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr, ounit
	real delx, dely
	real x(*), y(*)
	character*(*) cname1, cname2, cunit1, cunit2
	logical qxlin, qylin

c Description 
c  Pathetic little guy to write a few keywords to the output file
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  None
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 18) original
c  Ian M George  (1.0.1:1996 Feb 04), added wtinfo & friends
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(7) subname
        parameter (subname = 'wt_ckys')
	integer status, clenact
	real inreal
	integer decimals
	character(80) message, string
c Initialization 
	ierr = 0
	decimals = 6

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Do it
	status = 0
	call ftpkys(ounit,'CTYPE1',cname1,
     &		'Name of 1st coordinate axis',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CTYPE1 keyword')

	status = 0
	call ftpkys(ounit,'CUNIT1',cunit1,
     &		'Units of 1st coordinate axis',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CUNIT1 keyword')

	if(qxlin) then
	  status = 0
	  string = 'Pixel Size of 1st coordinate axis (' // 
     &		cunit1(:clenact(cunit1))//')'
	  call ftpkye(ounit,'CDELT1',delx, decimals,
     &		string,
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CDELT1 keyword')

	  status = 0
	  inreal = 1.0
	  call ftpkye(ounit,'CRPIX1',inreal,decimals,
     &		'Axis-1 coord of Reference Pixel',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CRPIX1 keyword')

	  status = 0
	  string = cname1(:clenact(cname1)) // 
     &		' Value at Reference Pixel ('//
     &		cunit1(:clenact(cunit1))//')'
	  inreal = X(1)
	  call ftpkye(ounit,'CRVAL1',inreal, decimals,
     &		string,
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CRVAL1 keyword')
	endif

	status = 0
	call ftpkys(ounit,'CTYPE2',cname2,
     &		'Name of 2nd coordinate axis',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CTYPE2 keyword')

	status = 0
	call ftpkys(ounit,'CUNIT2',cunit2,
     &		'Units of 2nd coordinate axis',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CUNIT2 keyword')

	if(qylin) then
	  status = 0
	  string = 'Pixel Size of 2nd coordinate axis (' // 
     &		cunit2(:clenact(cunit2))//')'
	  call ftpkye(ounit,'CDELT2',dely, decimals,
     &		string,
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CDELT2 keyword')

	  status = 0
	  inreal = 1.0
	  call ftpkye(ounit,'CRPIX2',inreal,decimals,
     &		'Axis-2 coord of Reference Pixel',
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CRPIX2 keyword')

	  status = 0
	  string = cname2(:clenact(cname2)) // 
     &		' Value at Reference Pixel ('//
     &		cunit2(:clenact(cunit2))//')'
	  inreal = Y(1)
	  call ftpkye(ounit,'CRVAL2',inreal, decimals,
     &		string,
     &		status)
	call wtferr(subname,version,status,	
     &		' problem writing CRVAL2 keyword')
	endif

	return
	end
c -------------------------------------------------------------------------
*+ CK_EGRID
	subroutine ck_egrid(chatter,
     &		nk_comm, comment,
     &		ienerg, energ_lo, energ_hi, eunits,
     &		eminstr, emaxstr, emin, emax, 
     &		ierr)
	IMPLICIT NONE
	integer chatter, ierr
	integer nk_comm, ienerg
	integer maxcomm
	parameter (maxcomm=100)
	real emin, emax
	real energ_lo(*), energ_hi(*)
	character*(*) eminstr, emaxstr
	character*(*) comment(maxcomm), eunits

c Description 
c  Check-out routine for energy grid
c  (necessary for the Dynamic Memory Allocation in calling routine)
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  None
c
c Compilation & Linking
c  No special requirements
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 19) original
c  Ian M George  (1.0.1: 1996 Feb 04) add wtinfo & friends
	character(7) version
	parameter (version=' 1.0.1 ')
*-
c Internals
        character(8) subname
        parameter (subname = 'ck_egrid')
	integer clenact
	character(80) message
c Initialization 
	ierr = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Translate the user-defined energy ranges
c ... lower bound
	if(eminstr.EQ.'%') then
	   emin = energ_lo(1)
	else
	   read(eminstr,*,iostat=ierr) emin
	   if(ierr.NE.0) then
	     call wtferr(subname,version,
     &		' Unable to translate energy bound ')
	     message = ' EMIN = '// eminstr(:clenact(eminstr))
	     call wtinfo(chatter,1,1,message)
	     goto 986
	   endif
	endif
c ... upper bound
	if(emaxstr.EQ.'%') then
	   emax = energ_hi(ienerg)
	else
	   read(emaxstr,*,iostat=ierr) emax
	   if(ierr.NE.0) then
	     call wtferr(subname,version,
     &		' Unable to translate energy bound ')
	     message = ' EMAX = '// emaxstr(:clenact(emaxstr))
	     call wtinfo(chatter,1,1,message)
	     goto 986
	   endif
	endif

c Final error check
986	if(ierr.NE.0) then
          call wterrm(subname, version,' Punting')
        endif

987	continue

	return
	end
c -------------------------------------------------------------------------
*+ ESQUISH
	subroutine esquish(chatter,
     &		maxenerg, ienerg, energ_lo, energ_hi, ework,
     &          maxcoord1, maxcoord2, icoord1, icoord2, collresp, 
     &		emin, emax, image,
     &		ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer maxenerg, maxcoord1, maxcoord2
	integer ienerg, icoord1, icoord2
	real emin, emax
	real energ_lo(*), energ_hi(*), ework(*)
	real collresp(maxenerg, maxcoord1, maxcoord2)
	real image(maxcoord1, maxcoord2)

c Description 
c  Compresses a COLLRESP dataset in the energy dimension
c
c Passed Parameters
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  {incomplete}
c  IERR            o : Error flag (0 = OK)
c
c Called Routines
c  {incomplete}
c
c Compilation & Linking
c  CALLIB, FTOOLS
c
c Origin:
c  Original
c 
c Author/Modification History
c  Ian M George  (1.0.0: 94 Oct 19) original
c  Ian M George  (1.1.0: 94 Oct 25), Bug fix to energy remapping
c  Ian M George  (2.0.0: 95 Jun 27), Major overhaul
c  Ian M George  (2.0.1: 1996 Feb 04) add wtinfo & friends
	character(7) version
	parameter (version=' 2.0.1 ')
*-
c Internals
        character(8) subname
        parameter (subname = 'ck_egrid')
	integer i,j,k
	real acc, value(1)
	character(80) message
c Initialization 
	acc = 1.E-3
	ierr = 0

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,30,1,message)

c Do the stuff
	do k = 1, icoord2
	   do j = 1, icoord1
	      do i = 1, ienerg
	         ework(i) = collresp(i,j,k)
	      enddo
	      call rmap1d(chatter, 
     &		ienerg, energ_lo, energ_hi, ework,
     &		1,emin, emax, value,
     &		3, acc, ierr)
	      if(ierr.NE.0) then
		goto 953
	      endif
	      image(j,k) = value(1)
	   enddo
	enddo


c Final error check
953	if(ierr.NE.0) then
          call wterrm(subname, version,'Aborting')
        endif

	return
	end
c -------------------------------------------------------------------------
