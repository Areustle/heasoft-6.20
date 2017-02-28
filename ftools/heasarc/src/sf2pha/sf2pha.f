*+SF2PHA
	SUBROUTINE SF2PHA
	IMPLICIT NONE
c 
c Description:
c  Program to convert old-style (SF) PHA files into new (FITS) PHA format. 
c  NOTE - The par file is assumed to have been opened.
c
c Passed Paremeters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_SF2PHA (see below)
c
c Include files
c  None
c 
c Called routines
c  subroutine GP_SF2PHA        : (below) Gets parameters from XPI par file
c  subroutine CNV_SF2PHA       : (below) Performs the conversion
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 May 20), original version
c  Ian M George     (1.1.0:1993 Jun 21), beta-test version
c  Ian M George     (1.2.0:1993 Oct 07), beta-test version for geoff crew
c  Ian M George     (1.2.1:1993 Oct 19), minor OFSP required changes 
c  Ian M George     (1.3.0:1993 Oct 22), major change to wtpha1
c  Ian M George     (1.3.1:1994 Jan 05), EXOSAT MHDREF & TIMESYS change
c  Ian M George     (1.3.2:1994 Feb 04), fixed bugette in rd_sfgrp
c  Ian M George     (1.4.0:1994 Feb 22), added EXOSAT TGS
c  Ian M George     (2.0.0:1994 Mar 08), added Dynamic Memory Allocation
c  Ian M George     (2.1.0:1994 Mar 29), fixed bug in FIXPHA, TLMIN=1 default
c  Rehana Yusaf     (2.1.1:1994 Sept 13), minor change, ck_file has
c                                         additional argument, and clobber is
c                                         read
c  Ning Gan         (2.1.2:1998 Jul 7),   minor change, the date string
c					  are 68 character long, and 
c					  use cfitsio routine to parse 
c				 	  date/time string.
c  Ning Gan         (2.1.3:1998 Jul 21),  replace the cshftime with 
c                                         timka in xanlib, and change 
c                                         the timarr from integer*2 
c                                         to integer*4.
c  Ning Gan         (2.1.4:1999 Mar 2),   fixed bugs caused by y2k. 
c                                         Take out the leading space 
c                                         in column names. 
        character(7) version
        parameter (version = '2.1.4')
*- 
c Internals 
	integer chatter, ierr
	character(5) phaversn
	character(20) origin
	character(60) telescop,instrume, detnam, filter
        character(25) context
        character(40) taskname
	character(80) infil, outfil, message
	logical qoverride,killit

        COMMON/task/taskname
        taskname ='SF2PHA'//version
 
        message = '** SF2PHA '//version
        call fcecho(message)

c Get Parameters from the par file
	call gp_sf2pha(infil, chatter, outfil, phaversn, origin,
     &    	qoverride,telescop,instrume,detnam,filter,killit)


c Do the nasty deed	
	call cnv_sf2pha(infil, chatter, outfil, phaversn, origin,
     &     qoverride, telescop, instrume, detnam,filter,killit,ierr)

c
        if(ierr.NE.0) then
          context = 'Incomplete Execution'
          message = '** SF2PHA '//version//' ERROR : '// context
          call fcecho(message)
          call fcerr(context)
        else
          message = '** SF2PHA '//version//' Finished'
          call fcecho(message)
        endif

	return
	end




c -------------------------------------------------------------------------
*+GP_SF2PHA
	subroutine gp_sf2pha(infil, chatter, outfil, phaversn, origin,
     &     	qoverride, telescop, instrume, detnam, filter,killit)

	IMPLICIT NONE
	integer chatter
	character*(*) phaversn
	character*(*) origin
	character*(*) telescop, instrume, detnam, filter
	character*(*) infil, outfil
	logical qoverride,killit
c 
c Description:
c  Gets the parameters required by SF2PHA from the parameter file
c  NOTE - The XPI par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  INFILE      - name of i/p PHA (old-style SF) format data file
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  OUTFILE     - name of o/p PHA (FITS) format data file
c  PHAVERSN    - OGIP version of PHA file required
c  ORIGIN      - Institution which created the file
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
c  Ian M George     (1.0.0:1993 May 21), original
c  Ian M George     (1.1.0:1993 Oct 12) returns phaversn= '1.1.0' for '1992a'
c Rehana Yusaf      (1.1.1: 1994 Sept 13) read in clobber
	character(7) version
	parameter (version = '1.1.1')
*- 
c Internals
	integer ierr
	character(30)  errstr, wrnstr
	character(80)  message
c Initialize
	errstr = '** GP_SF2PHA ERROR: '
	wrnstr = '** GP_SF2PHA WARNING: '
	ierr = 0
	telescop = 'UNKNOWN'
	instrume = 'UNKNOWN'
	detnam = ' '
	filter = 'NONE'


c Get the name of the old-style SF file
	call uclgst('infil',infil, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting INFIL parameter'
		call fcecho(message)
		return
	endif	

c Get the name of the o/p FITS file
	call uclgst('outfil',outfil, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting OUTFIL parameter'
		call fcecho(message)
		return
	endif	

c Get the OGIP version number of the PHA file format to be created
	call uclgst('phaversn',phaversn, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting PHAVERSN parameter'
		call fcecho(message)
		ierr = 0 
		message = wrnstr // 'Setting PHAVERSN=1.1.0'
		call fcecho(message)
		phaversn='1.1.0'
	endif	
	if(phaversn.EQ.'1992a') then
		phaversn='1.1.0'
	elseif(phaversn.EQ.'1.0.0') then
           phaversn = '1.1.0'
           message = wrnstr // ' Task no longer supports this format'
           call fcecho(message)
           message =
     &        ' ...... Resetting format (HDUVERS1) to '//phaversn
           call fcecho(message)
	endif

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting CHATTER parameter'
		call fcecho(message)
		ierr = 0 
		message = wrnstr // 'Setting CHATTER = 10'
		call fcecho(message)
		chatter = 10
	endif	

c Give user info if requested
	if(chatter.GE.20) then	
	 message = ' ... using GP_SF2PHA ' // version
	 call fcecho(message)
	endif

c Get the name of the institution producing the file
	call uclgst('origin',origin, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting ORIGIN parameter'
		call fcecho(message)
		ierr = 0 
		message = wrnstr // 'Setting ORIGIN = UNKNOWN'
		call fcecho(message)
		origin = 'UNKNOWN'
	endif	

c Get the Override flag as to whether Telecop,Instr etc names derived 
c   from the SF file are to be ignored, and those listed below used
c   instead
	call uclgsb('qoverride',qoverride, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting QOVERRIDE parameter'
		call fcecho(message)
		ierr = 0 
		message = wrnstr // 'Setting QOVERRIDE = FALSE'
		call fcecho(message)
		qoverride = .false.
	endif	

	if(qoverride) then
c 	... OK, we're gonna override, so get the necessary parameters
	  call uclgst('telescop',telescop, ierr)
	  if(ierr.NE.0) then
	  	message = errstr // 'Getting TELESCOP parameter'
		call fcecho(message)
		return
	  endif	

	  call uclgst('instrume',instrume, ierr)
	  if(ierr.NE.0) then
	  	message = errstr // 'Getting INSTRUME parameter'
		call fcecho(message)
		return
	  endif	

	  call uclgst('detnam',detnam, ierr)
	  if(ierr.NE.0) then
	  	message = errstr // 'Getting DETNAM parameter'
		call fcecho(message)
		return
	  endif	

	  call uclgst('filter',filter, ierr)
	  if(ierr.NE.0) then
	  	message = errstr // 'Getting FILTER parameter'
		call fcecho(message)
		return
	  endif	
	endif	

c get clobber

        call uclgsb('clobber',killit,ierr)
          if(ierr.NE.0) then
                message = errstr // 'Getting CLOBBER parameter'
                call fcecho(message)
                return
          endif

	return
	end
c -------------------------------------------------------------------------
*+CNV_SF2PHA
	subroutine cnv_sf2pha(infil, chatter, outfil, phaversn, origin,
     &     	qoverride, telescop, instrume, detnam, filter,killit,ierr)

	IMPLICIT NONE
	integer chatter, ierr
	character*(*) phaversn
	character*(*) origin
	character*(*) telescop, instrume, detnam, filter
	character*(*) infil, outfil
	logical qoverride,killit
c 
c 
c Description:
c  Program to convert old-style (SF) PHA file into new (FITS) PHA format. 
c
c User i/ps required (prompted for):
c  INFILE      - name of i/p PHA (old-style SF) format data file
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  OUTFILE     - name of o/p PHA (FITS) format data file
c  PHAVERSN    - OGIP version of RMF file required
c  ORIGIN      - Institution which created the file
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
c  Ian M George     (1993 May 21), original
c  Ian M George     (1.1.0:1993 Aug 10), callib updates + big push
c  Ian M George     (1.2.0:1993 Oct 06), wt_*radec changes
c  Ian M George     (1.2.1:1994 Jan 10) reduced max array sizes
c  Ian M George     (1.2.2:1994 Feb 15) changed me_detnam
c  Ian M George     (2.0.0:1994 Feb 22) added EXOSAT TGS
c  Ian M George     (3.0.0:1994 Mar 09), added Dynamic Memory Allocation
c  Ian M George     (3.0.1:1994 Apr 01), made fchan = 1 default
c  rehana Yusaf     (3.0.2:1994 Sept 13), add killit to ck_file call
        character(7) version
        parameter (version = '3.0.2')
*- 
c Commons
        character(40) taskname
        COMMON/task/taskname
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
	integer maxchan, maxgrp
	parameter (maxhist=50,maxcomm=200,maxgrp=5)
	parameter (maxchan=2048)
c Internals
c ... parameters & static arrays
	integer status,   iunit, ounit,i
	integer ihumm, fchan
	integer nk_hist, nk_comm, lc, fcstln
	integer imsg, detchans, nchans
        integer dtype, gcards
	real ftsver
        real areascal, live_time, backscal,corrscal,indices(3)
	character(8) key(4)
	character(16) chantyp
	character(60) old(4), new(4)
	character(60) old_tele, old_inst, old_detn, old_filt
        character(70) hist(maxhist), comment(maxcomm)
	character(30) errstr, wrnstr
	character(80) message
	character(80) backfil, respfil,corrfil, ancrfil
	logical qfinfo,qass, qdet_xten 
	logical qerror, qsys, qqual, qgroup, qgrpd
c ...... HDUCLAS stuff
	character(20) hduclas2
c ...... Source info writing routine
	character(70) object, obs_id, obs_pi, seqno, observer
	character(70) proc_sys, proc_ver, proc_loc, proc_date
	character(70) proc_time
c ...... Source Info package reading routine
        real shfstart, shfstop, obsepoch
	double precision mjdobs
        character(68) date_obs, time_obs, date_end, time_end
        logical qsinfo
c ...... Celestial positions
	real equinox, ra_obj, dec_obj
c	real ra_pnt, dec_pnt, pa_pnt
c  	real ra_pnte, dec_pnte, pa_pnte
	real pa_obj
	real ra_obje, dec_obje
	real pa_obje
	character(70) radecsys
	logical qrasys, qscraftx,qscrafty,qscraftz
c ...... Clnstr stuff
        character(80) dummystr(1)
        logical qokfil
        integer idummy, nbad
        integer ibad(70)
c ...... Selectors stuff
        integer nsel, nseld(10), nselr(10), nselc(10)
        double precision seld(10,10)
        real selr(10,10)
        character(70) seltype(10), selnm(10,10,10), selc(10,10)
c ...... full timing stuff
        real tierrela, tierabso, ontime, deadcorr, vigncorr
	real telapse,livetime
        double precision mjdref, tstart, tstop, tzero
        character(20) timesys, timeunit, timeref
        character(20) tassign
        logical deadapp, vignapp, clockapp
c ...... EXOSAT ME specific stuff
        integer ndets
        real h1_off, h2_off
        real scx(3), scy(3), scz(3)
        real dcol(8), dare(8), dexp(8), dgan(8)
        real dcog(8,4), dcor(8,8)
	character(2) dgas
        character(20) detstr(8)
c ...... EXOSAT GSPC specific stuff
	integer blw_start, blw_stop
	integer linchan(4)
	real nomgain, gaincor, gsdcol,gsdarea
        logical qlinchan(4), qgaincor
c ...... EXOSAT TGS specific stuff
	real x0, y0, angle, sscorr
	integer width

c ... pointers to "arrays" to be dynamically allocated
	integer p_channel, p_qualty, p_grping, p_qual_in
	integer p_grp_in, p_pha_data, p_pha_errors, p_syserr
	integer p_pha_din, p_pha_ein, p_pha_sin
c ... "arrays" to be dynamically allocated
c	integer channel(maxchan)	integer qualty(maxchan)
c	integer grping(maxchan)		integer qual_in(maxchan)
c	integer grp_in(maxchan)		real pha_data(maxchan)
c 	real pha_errors(maxchan)	real syserr(maxchan)
c	real pha_din(maxchan)		real pha_ein(maxchan)
c	real pha_sin(maxchan)


c Initialize
	ierr = 0
        dummystr(1) = ' '
	errstr = '** CNV_SF2PHA ERROR:'	
	wrnstr = '** CNV_SF2PHA WARNING:'	
c Set up the defaults
	vignapp = .false.
	vigncorr = 0.0
	deadapp  = .false.
	deadcorr = 0.0
        date_obs = ' '
	time_obs = ' '
	date_end = ' '
	time_end = ' ' 
	fchan = 1
	nk_hist = 1
	hist(1) = ' '
	nk_comm = 1
	comment(1) = ' '
	detchans = 1 	
	chantyp = 'UNKNOWN'
	ancrfil = ' '
	nchans = 1
	dtype = 1
	object = ' '
	obs_id = ' '
	obs_pi = ' '
	seqno = ' '
	observer = ' '
	radecsys = ' '
	proc_sys = ' '
	proc_ver = ' ' 
	proc_loc = ' '
	proc_date = ' '
	proc_time = ' '

c Give user info if requested
        if(chatter.GE.10) then
                message = ' ... using CNV_SF2PHA '// version
		call fcecho(message)
           	call ftvers(ftsver)
           	write(message,'(a,f6.3)')
     &          ' ... using FITSIO Version ', ftsver
           	call fcecho(message)
        endif

c Check that the o/p file doesn't already exist or is illegal
        call ck_file(outfil,dummystr,1,qokfil,killit,chatter)
        if(.NOT.qokfil) then
                message = ' ... Offending file is OUTFIL: '// outfil
                call fcecho(message)
                ierr = -1
                goto 482
        endif

c *****
c Allocate dynamic memory

        p_channel = 0
        p_qualty = 0
        p_grping = 0
        p_qual_in = 0
        p_grp_in = 0
        p_pha_data = 0
        p_pha_errors = 0
        p_syserr = 0
        p_pha_din = 0
        p_pha_ein = 0
        p_pha_sin = 0

        call udmget(maxchan, 4, p_channel, status)
        call udmget(maxchan, 4, p_qualty, status)
        call udmget(maxchan, 4, p_grping, status)
        call udmget(maxchan, 4, p_qual_in, status)
        call udmget(maxchan, 4, p_grp_in, status)
        call udmget(maxchan, 6, p_pha_data, status)
        call udmget(maxchan, 6, p_pha_errors, status)
        call udmget(maxchan, 6, p_syserr, status)
        call udmget(maxchan, 6, p_pha_din, status)
        call udmget(maxchan, 6, p_pha_ein, status)
        call udmget(maxchan, 6, p_pha_sin, status)
        if(status.NE.0) then
                message = errstr// ' Failed to allocate Dynamic Memory'
                call fcecho(message)
                ierr = -1
                goto 482
        endif
c *****




c Store the old keyword values (required below if override requested)
	old_tele = telescop
	old_inst = instrume
	old_detn = detnam
	old_filt = filter

c Open the SF file
        call rd_sfopn(infil,chatter,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          iunit, ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
c Check through the history strings for useful info
	do i = 1, nk_hist
	   ihumm = INDEX(hist(i), 'Deadtime')
	   if(ihumm.NE.0) then
	         ihumm = INDEX(hist(i), 'applied')
	         if(ihumm.NE.0) then
		     deadapp = .true.
		     goto 391
		 endif
	   	 ihumm = INDEX(hist(i), '=')
		 if(ihumm.NE.0) then
		     read(hist(i)(ihumm+1:ihumm+7),'(g7.0)') deadcorr
			deadcorr = 1./deadcorr
		     goto 391
		  endif
	    endif
391	enddo


c Read in the file info stuff from the old-style (SF) file
        call rd_sfinfo(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          qfinfo, instrume, nchans, detchans, live_time, areascal,
     &          backscal, corrscal,indices,
     &          qass, backfil, respfil,corrfil,
     &          ierr)
	if(ierr.NE.0)then
	   goto 482
	endif


c - Break up the SF-supplied instrument name into 2 halves, in an
c   attempt to decouple the telescope/mission from the detector
        call crmvlbk(instrume)
        lc = fcstln(instrume)
        if(lc.GT.0) then
          do i = 1, lc
                if(instrume(i:i).EQ.' ') then
                        lc = i-1
                        goto 765
                endif
          enddo
765       telescop = instrume(1:lc)
          instrume = instrume(lc+1:)
          call crmvlbk(instrume)
        else
                telescop = ' '
                instrume = ' '
        endif



c Read in the source info stuff from the old-style (SF) file
        call rd_sfsinfo(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          obsepoch, shfstart, date_obs, time_obs,
     &          shfstop, date_end, time_end, mjdobs,
     &          equinox, ra_obj, dec_obj, pa_obj, object,
     &          qsinfo, ierr)
	if(ierr.NE.0)then
	   goto 482
	endif


c Check/Read Database info
        call rd_sfdbase(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &		radecsys, equinox,
     &          ra_obj, dec_obj, pa_obj, shfstart, shfstop,
     &          ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
c Set associated arrays
	ra_obje = 0.0
	dec_obje= 0.0
	pa_obje =0.0


c Get any selectors
        call rd_sfslct(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          nsel, seltype, nseld, nselr, nselc,
     &          selnm, seld, selr, selc,
     &          ierr)
	if(ierr.NE.0)then
	   goto 482
	endif

c Read the PHA data 
        call rd_sfpha(iunit,chatter,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          dtype, nchans, MEMR(p_pha_din), qerror, 
     &		MEMR(p_pha_ein),ierr)
	if(ierr.NE.0)then
	   goto 482
	endif

c Read the systematic errors info
        call rd_sfsys(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          nchans, qsys, MEMR(p_pha_sin), ierr)
	if(ierr.NE.0)then
	   goto 482
	endif

c Read the grouping info
        call rd_sfgrp(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          detchans, qgroup, gcards, MEMI(p_grp_in), 
     &		qqual, MEMI(p_qual_in),ierr)
	if(ierr.NE.0)then
	   goto 482
	endif

c ... Check to see if original data has been grouped
c     (This is now done in fixpha below)

c... fix up the PHA channel numbers with data, and with quality & grouping
c card. This is necessary since the grouping (& hence quality) flags are 
c read in using full (detchans) channel range, whilst data,errors etc are 
c read in using a 'false' scale (giving total of nchans) which ignores any 
c channels defined to be '.' in original SF grouping card.
	call fixpha(chatter,detchans,nchans,qgrpd, comment, nk_comm,
     &		MEMI(p_grp_in), MEMI(p_qual_in), MEMI(p_channel),
     &		MEMR(p_pha_data), MEMR(p_pha_din), MEMR(p_pha_errors),
     &		MEMR(p_pha_ein), MEMR(p_syserr), MEMR(p_pha_sin), 
     &		MEMI(p_qualty), MEMI(p_grping), ierr)
 	if(ierr.NE.0)then
	   goto 482
	endif

c Clean up any fitsio-illegal characters in the history and comment strings
	imsg = 0
        do i = 1, nk_hist
                call clnstr(chatter, hist(i), 70, hist(i),
     &                  idummy, nbad, ibad, status)
		if(status.ne.0) status = 0
		if(nbad.NE.0) imsg = imsg + 1
	enddo
        do i = 1, nk_comm
                call clnstr(chatter, comment(i), 70, comment(i),
     &                  idummy, nbad, ibad, status)
		if(status.ne.0) status = 0
		if(nbad.NE.0) imsg = imsg + 1
	enddo
        if(imsg.NE.0 .AND. chatter.GE.20) then
            message = wrnstr //
     &         ' FITSIO-Illegal characters replaced by CLNSTR'
             call fcecho(message)
             write(message,'(a,i4,a)')
     &          ' ... in ', imsg, ' History and/or Comment card(s)'
             call fcecho(message)
        endif

c Fix up the HDUCLAS stuff
c	... work out whether this PHA data is bkgd-subtracted
	if(backfil.NE.'NONE'.OR.backfil.NE.' ')then
		hduclas2 = 'NET'
	else
		hduclas2 = 'TOTAL'
	endif

c Sort out the detector-specific packages and mission/detector-specific info
c ... EXOSAT (Main)
	if(telescop(:6).EQ.'EXOSAT') then
		mjdref  = 44239.00
		tstart 	= shfstart
		tstop 	= shfstop
		tzero = shfstart
		telapse = tstop - tstart
		timesys = '1980 1 1 00:00:00'
		timeunit= 's'
		clockapp = .false.
		timeref  = 'LOCAL'
		tassign  = 'SATELLITE'
		tierrela = 0.0
		tierabso = 0.0
		fchan = 1
c		... fix up the s/c coords systems
			qscraftx = .true.
			qscrafty = .true.
			qscraftz = .false.
c ... EXOSAT ME
	    if(INSTRUME(:2).EQ.'ME') then
              call cnv_exome(chatter, iunit, maxhist, nk_hist, hist,
     &          maxcomm,nk_comm,comment,proc_ver,proc_date,proc_time,
     &          equinox, scx, scy, h1_off, h2_off,
     &          ndets, detstr,dcol,dare,dexp,dgan,
     &          dcog, dcor,dgas,ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
	      		chantyp = 'PHA'
	      		vignapp = .true.	
	      		vigncorr= 0.0
c			... Note, deadcorr is searched for in the history 
c			    records, however, even if not found, it has 
c			    been applied for EXOSAT ME according to Nick White
			deadapp = .true.
c			... Fix up the timing stuff
	      		ontime  = tstop - tstart
			if(deadcorr.GT.0.0) then
			   livetime = telapse * deadcorr
			else
			   livetime = 0.0
			endif
c 			... strip off any crap on end of instrume
			instrume = 'ME'
c 			... try and sort out what the detnam will be
        		call me_detnam(chatter,ndets,detstr,dgas, 
     &				detnam,status)
c ... EXOSAT GSPC
	    elseif(INSTRUME(:4).EQ.'GSPC') then
              call cnv_exogs(chatter, iunit, maxhist, nk_hist, hist,
     &          maxcomm,nk_comm,comment,blw_start,blw_stop,
     &		nomgain, gaincor,proc_ver,proc_date,proc_time,
     &          equinox, scx, scy, 
     &          gsdcol,gsdarea,qlinchan,linchan, qgaincor,
     &          ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
	      		chantyp = 'PHA'
	      		vignapp = .true.	
	      		vigncorr= 0.0
c			... Note, deadcorr is searched for in the history 
c			    records, however, even if not found, I'm 
c			assuming it has been applied.
			deadapp = .true.
c			... Fix up the timing stuff
	      		ontime  = tstop - tstart
c 			... I'm guessing that the stored deadcorr is upside-down
			if(deadcorr.GT.0.0) then
			   deadcorr = 1./deadcorr
			   livetime = telapse * deadcorr
			else
			   livetime = 0.0
			endif
c 			... strip off any crap on end of instrume
			instrume = 'GSPC'
c ... EXOSAT TGS
	    elseif(INSTRUME(:5).EQ.'GRATE') then
      	call cnv_exotgs(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		instrume, detnam,filter,
     &		proc_ver, proc_date, proc_time,
     &		x0, y0, width, angle, deadcorr, sscorr,
     &		ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
	      		chantyp = 'PHA'
	      		vignapp = .true.	
	      		vigncorr= 0.0
			deadapp = .true.
c			... Fix up the timing stuff
	      		ontime  = tstop - tstart
			livetime = telapse * deadcorr
c ... EXOSAT CMA
	    elseif(INSTRUME(:3).EQ.'CMA') then
c              call cnv_exocma(chatter, iunit, maxhist, nk_hist, hist,
c     &          maxcomm,nk_comm,comment,proc_ver,proc_date,proc_time,
c     &          equinox, scx, scy, h1_off, h2_off,
c     &          ndets, detstr,dcol,dare,dexp,dgan,
c     &          dcog, dcor,dgas,ierr)
	if(ierr.NE.0)then
	   goto 482
	endif
	      		chantyp = 'PHA'
	      		vignapp = .true.	
	      		vigncorr= 0.0
c			... Note, deadcorr is searched for in the history 
c			    records, however, even if not found, it has 
c			    been applied for EXOSAT ME according to Nick White
c CHECK THIS !!!!
			deadapp = .true.
c			... Fix up the timing stuff
	      		ontime  = tstop - tstart
			if(deadcorr.GT.0.0) then
			   livetime = telapse * deadcorr
			else
			   livetime = 0.0
			endif
c 			... strip off any crap on end of instrume
c 			... try and sort out what the detnam will be
        		call cma_detnam(chatter,instrume, instrume, filter,
     &				status)
	    endif
c ... ROSAT (Main)
	elseif(telescop(:5).EQ.'ROSAT') then
		if(instrume(:6).EQ.'PSPC-C') instrume = 'PSPCC'
		if(instrume(:6).EQ.'PSPC-B') instrume = 'PSPCB'
                if(radecsys.EQ.' ') then
			radecsys = 'FK5'
                	equinox = 2000.0
                	mjdref = 48043.879745364201881
                	timesys = '1980.00'
                	timeunit = 's'
                	tstart = 0.0
                	tstop = 0.0
                	tzero = 0.0
                	timeref = 'LOCAL'
                	tassign = ' '
                	tierrela = 0.0
                	tierabso = 0.0
		else
			tstart 	= shfstart
			tstop 	= shfstop
			tzero = shfstart
		endif
		telapse = tstop - tstart
	endif 

c Dump all the history & comment cards if user really wants them
	if(chatter.GT.20) then
	  message = ' ... History Records prior to opening o/p file:'
	  call fcecho(message)
	  do i = 1, nk_hist
	   call fcecho(hist(i))
	  enddo
	  message = ' ... Comment Cards prior to opening o/p file:'
	  call fcecho(message)
	  do i = 1, nk_comm
	   call fcecho(comment(i))
	  enddo
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
                message = key(i) // ' from PHA  = ' // old(i)
                call fcecho(message)
                message = ' ... overwritten with ' // new(i)
                call fcecho(message)
              enddo
             endif
           endif
	else
	  if(chatter.ge.20) then
	   message = ' ... attempted to decouple mission/inst'
	   call fcecho(message)
	   message = ' ...... TELESCOP = ' // telescop
	   call fcecho(message)
	   message = ' ...... INSTRUME = ' // instrume
	   call fcecho(message)
	  endif
        endif

c Open & Write the FITS file
	status = 0
c ------------------------ PRIMARY HEADER ---------------------------
	if(phaversn(1:1).EQ.'1') then

c             ... Open the FITS file and write a null primary header
	      call opnpa(outfil, chatter, ounit, killit,status)
		if(status.NE.0) then
			ierr = status
			goto 481
		endif

c 	      ... Add additional keywords to Primary Header
	        call FTPKYS(ounit,'CONTENT',
     &  	        'PHA SPECTRUM',
     &             'SPECTRUM xtens (at least) present',
     &          	status)
        	call FTPKYS(ounit,'ORIGIN',
     &          	origin,
     &       	   	'organization which created this file',
     &          	status)
	endif
c ------------------------ finished PRIMARY ---------------------------


c ------------------------ SPECTRUM EXTENSION ----------------------------
	if(phaversn(1:1).EQ.'1') then
      		call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &                  comment,telescop,instrume,detnam,filter,
     &			phaversn,hduclas2,fchan,
     &                  live_time,areascal,backfil,backscal,corrfil,
     &                  corrscal,respfil,ancrfil,detchans,chantyp,
     &                  MEMI(p_channel),MEMR(p_pha_data),dtype,
     &			qerror,MEMR(p_pha_errors),qsys,
     &                  MEMR(p_syserr),qqual,MEMI(p_qualty),qgroup,
     &			MEMI(p_grping),nchans,ierr)
		if(ierr.NE.0) goto 481
	else
	      message = errstr // 'Unknown format: '// phaversn
	      call fcecho(message)
	      ierr = 1
	      goto 481
	endif
c       ... Add additional keywords to Header
                call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)

c       ... Insert the source/observation info
        	call wt_obsinfo(chatter, ounit,
     &          object, obs_id, obs_pi, seqno, observer,
     &          proc_sys, proc_ver, proc_loc, proc_date, proc_time,
     &		ierr)

c       ... Insert the source/observation timing info
                call wttobs(chatter, ounit,
     &          date_obs, time_obs, date_end, time_end,
     &          mjdobs, ierr)

c 	... Insert the serious time info 
       		call wtftim(chatter, ounit,
     &  	mjdref, tstart, tstop, tzero,
     &  	timesys,timeunit,clockapp, timeref,tassign,
     &  	tierrela, tierabso, telapse, ontime, deadapp, deadcorr,
     &  	livetime, vignapp, vigncorr, ierr)
 
	qrasys = .true.
c       ... Insert the pointing position in celestial coords
c        COMMENTED OUT SINCE NEVER BELIEVED TO BE KNOWN FROM SF FILE
c                call wt_pntradec(chatter, ounit,
c     &          qrasys, radecsys, equinox, 
c     &		ra_pnt, dec_pnt, pa_pnt,
c     &		ra_pnte, dec_pnte, pa_pnte,
c     &          ierr)
c       ... Insert the Object position in celestial coords
                call wt_objradec(chatter, ounit,
     &          qrasys, radecsys, equinox, 
     &		ra_obj, dec_obj, ra_obje, dec_obje,
     &          ierr)

c ------------------------ finished SPECTRUM EXTENSION -------------------

	qdet_xten = .false.
c ------------------------ DETECTOR EXTENSION ----------------------------
	if(phaversn(1:1).EQ.'1') then
	    if(telescop(:6).EQ.'EXOSAT' .AND. INSTRUME(:2).EQ.'ME') then
		qdet_xten = .true.
      		call wtxme1(ounit,chatter,nk_hist,hist,nk_comm,
     &                  comment,phaversn,live_time,areascal,detchans,
     &			chantyp,h1_off, h2_off,shfstart, shfstop,
     &          	ndets, detstr,dcol,dare,dexp,dgan,
     &          	dcog, dcor,ierr)
	    elseif(telescop(:6).EQ.'EXOSAT'
     &			.AND.INSTRUME(:4).EQ.'GSPC')then
      		call wtxgs1(ounit,chatter, blw_start, blw_stop,
     &          nomgain, gaincor, gsdcol,gsdarea,
     &          qlinchan, linchan, qgaincor, ierr)
	    elseif(telescop(:6).EQ.'EXOSAT'
     &			.AND.INSTRUME(:3).EQ.'TGS')then
      		call wtxtg1(ounit,chatter, 
     &		x0, y0, width, angle, sscorr,
     &		ierr)
	    else
	      message = wrnstr // 'Unknown/supported Mission/instrument'
	      call fcecho(message)
	      message = ' ... Detector Extension NOT written'
	      call fcecho(message)
	      message = ' ...... (but PHA extension WILL read into XSPEC)'
	      call fcecho(message)
		goto 481
	    endif
	else
	      message = errstr // 'Unknown format'
	      call fcecho(message)
		ierr = 1
		goto 481
	endif

	if(qdet_xten) then
c       ... Add additional keywords to Header
                call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)

c               ... Insert the source/observation info
                call wt_obsinfo(chatter, ounit,
     &          object, obs_id, obs_pi, seqno, observer,
     &          proc_sys, proc_ver, proc_loc, proc_date, proc_time,
     &		ierr)

c               ... Insert the source/observation timing info
                call wttobs(chatter, ounit,
     &          date_obs, time_obs, date_end, time_end,
     &          mjdobs, ierr)

c 		.... Insert the serious time info 
       		call wtftim(chatter, ounit,
     &  	mjdref, tstart, tstop, tzero,
     &  	timesys,timeunit,clockapp, timeref,tassign,
     &  	tierrela, tierabso, telapse, ontime, deadapp, deadcorr,
     &  	livetime, vignapp, vigncorr, ierr)
 
	qrasys = .true.
c       ... Insert the pointing position in celestial coords
c        COMMENTED OUT SINCE NEVER BELIEVED TO BE KNOWN FROM SF FILE
c                call wt_pntradec(chatter, ounit,
c     &          qrasys, radecsys, equinox, 
c     &		ra_pnt, dec_pnt, pa_pnt,
c     &		ra_pnte, dec_pnte, pa_pnte,
c     &          ierr)
c       ... Insert the s/c position in celestial coords
                call wt_scradec(chatter, ounit,
     &          qrasys, qscraftx, qscrafty,qscraftz,
     &		radecsys, equinox, 
     &		scx(1), scx(2), scx(3),
     &		scy(1), scy(2), scy(3),
     &		scz(1), scz(2), scz(3),
     &		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     &		0.0, 0.0, 0.0, ierr)

c       ... Insert the Object position in celestial coords
                call wt_objradec(chatter, ounit,
     &          qrasys, radecsys, equinox, 
     &		ra_obj, dec_obj, ra_obje, dec_obje,
     &          ierr)
	endif

c ------------------------ finished DETECTOR EXTENSION -------------------


c ------------------------ CLOSE O/P FILE ----------------------------
c Close the FITS file
481	call ftclos(ounit, status)

c Check for errors
482     if(ierr.ne.0) then
                message = errstr // ' Fatal'
                call fcecho(message)
        endif

c *****
c Free the dynamic Memory
        call udmfre(p_channel, 4, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_qualty, 4, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_grping, 4, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_qual_in, 4, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_grp_in, 4, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_pha_data, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_pha_errors, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_syserr, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_pha_din, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_pha_ein, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
        call udmfre(p_pha_sin, 6, status)
        if(status.NE.0) then
                message = wrnstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
		goto 999
        endif
c *****

999 	return
	end
c ----------------------------------------------------------------------
*+ CNV_EXOME
      	subroutine cnv_exome(chatter, iunit, maxhist, nk_hist, hist,
     &		maxcomm,nk_comm,comment,proc_ver,proc_date,proc_time,
     &		equinox, scx, scy, h1_off, h2_off,
     &		ndets, detstr,dcol,dare,dexp,dgan,
     &		dcog, dcor,dgas,ierr)

	IMPLICIT NONE
	integer ierr, iunit
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer ndets
	real h1_off, h2_off, equinox
	real scx(3), scy(3)
	real dcol(8), dare(8), dexp(8), dgan(8)
	real dcog(8,4), dcor(8,8)
	character(2) dgas
	character*(*) comment(maxcomm), hist(maxhist)
	character*(*) detstr(8), proc_ver, proc_date, proc_time
c 
c Description:
c  Reads an EXOSAT ME package from an SF files and does some minor 
c fiddling around
c
c Authors/Modification History
c Ian M George (1993 Aug 16), orginal
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals
	integer i
	real deq
	real dscx(3), dscy(3)
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	rewind(iunit)
	ierr = 0
	errstr = '** CNV_EXOME ERROR:'
	wrnstr = '** CNV_EXOME WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using CNV_EXOME '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' EXOSAT ME conversion by CNV_EXOME',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Read the package from the SF file
      	call rd_sfexome(chatter,iunit, maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,proc_ver,proc_date,proc_time,
     &		deq, dscx, dscy, h1_off, h2_off, ndets,
     &          detstr,dcol,dare,dexp,dgan,dcog,dcor,dgas,ierr)
	if(ierr.NE.0) goto 482

c Convert the H1 & H2 offsets from decimal arcmin to degrees
	h1_off = h1_off/60.0
	h2_off = h2_off/60.0

c SOrt out & check the celestial coordinate info
	if(equinox.EQ.0.0) then
		equinox = deq
	elseif(deq.EQ.0.0) then
		goto 376
	elseif(deq.NE.equinox) then
	  message = wrnstr // ' equinox mismatch '
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'... value read from EXOSAT ME package of i/p file = ', 
     &		deq
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'... value (passed) from elsewhere in i/p file     =',
     &		equinox
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'...... assuming EQUINOX =', equinox
	  call fcecho(message)
376	endif

c Check the s/c axes 
c Currently unsure what to do if non-zero values found !!!!
	do i = 1, 3
	   if(scx(i).GT.0.0)then
		message = errstr // 'Non-zero s/c coordinate'
		call fcecho(message)
		message = ' ... CNV_EXOME does not know what to do'
		call fcecho(message)
		write(message,'(a,i2, 1x, f10.5)') 
     &			'... SCX coord ', i, scx(i)
		call fcecho(message)
	   endif
	   if(scy(i).GT.0.0)then
		message = errstr // 'Non-zero s/c coordinate'
		call fcecho(message)
		message = ' ... CNV_EXOME does not know what to do'
		call fcecho(message)
		write(message,'(a,i2, 1x, f10.5)') 
     &			'... SCY coord ', i, scy(i)
		call fcecho(message)
	   endif
	enddo

c Error check
482	if(ierr.NE.0) then
	   message = errstr // 'Aborting'
	   call fcecho(message)
	endif

      return
      end
c ----------------------------------------------------------------------
*+ CNV_EXOGS
      	subroutine cnv_exogs(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          blw_start, blw_stop, nomgain, gaincor,
     &          proc_ver, proc_date, proc_time,
     &          equinox, scx, scy,
     &          dcol,darea,qlinchan,linchan, qgaincor,
     &          ierr)

        IMPLICIT NONE
        integer iunit, ierr
        integer chatter, maxhist, maxcomm
        integer nk_hist, nk_comm
        integer blw_start,blw_stop
	integer linchan(4)
        real nomgain, gaincor, equinox
        real scx(3), scy(3)
        real dcol, darea
        logical qlinchan(4), qgaincor
        character*(*) comment(maxcomm), hist(maxhist)
        character*(*) proc_ver, proc_date, proc_time
c
c Description:
c  Reads an EXOSAT GSFC package from an SF files and does some minor 
c fiddling around
c
c Authors/Modification History
c Ian M George (1993 Oct 13), orginal
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals
	integer i
	real deq
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	rewind(iunit)
	ierr = 0
	errstr = '** CNV_EXOGS ERROR:'
	wrnstr = '** CNV_EXOGS WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using CNV_EXOGS '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' EXOSAT GSPC conversion by CNV_EXOGS',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Read the package from the SF file
	call rd_sfexogs(chatter,iunit,
     &          maxhist, nk_hist, hist,
     &          maxcomm, nk_comm, comment,
     &          blw_start, blw_stop, nomgain, gaincor,
     &          proc_ver, proc_date, proc_time,
     &          deq, scx, scy,
     &          dcol,darea,qlinchan,linchan, qgaincor,
     &          ierr)
	if(ierr.NE.0) goto 482

c SOrt out & check the celestial coordinate info
	if(equinox.EQ.0.0) then
		equinox = deq
	elseif(deq.EQ.0.0) then
		goto 376
	elseif(deq.NE.equinox) then
	  message = wrnstr // ' equinox mismatch '
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'... value read from EXOSAT ME package of i/p file = ', 
     &		deq
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'... value (passed) from elsewhere in i/p file     =',
     &		equinox
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		'...... assuming EQUINOX =', equinox
	  call fcecho(message)
376	endif

c Check the s/c axes 
c Currently unsure what to do if non-zero values found !!!!
	do i = 1, 3
	   if(scx(i).GT.0.0)then
		message = errstr // 'Non-zero s/c coordinate'
		call fcecho(message)
		message = ' ... CNV_EXOGS does not know what to do'
		call fcecho(message)
		write(message,'(a,i2, 1x, f10.5)') 
     &			'... SCX coord ', i, scx(i)
		call fcecho(message)
	   endif
	   if(scy(i).GT.0.0)then
		message = errstr // 'Non-zero s/c coordinate'
		call fcecho(message)
		message = ' ... CNV_EXOGS does not know what to do'
		call fcecho(message)
		write(message,'(a,i2, 1x, f10.5)') 
     &			'... SCY coord ', i, scy(i)
		call fcecho(message)
	   endif
	enddo

c Error check
482	if(ierr.NE.0) then
	   message = errstr // 'Aborting'
	   call fcecho(message)
	endif

      return
      end
c ----------------------------------------------------------------------
*+ CNV_EXOTGS
      	subroutine cnv_exotgs(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		instrume, detnam,filtstr,
     &		proc_ver, proc_date, proc_time,
     &		x0, y0, width, angle, dtcorr, sscorr,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	real x0, y0, angle
	real dtcorr,sscorr
	integer width
	character*(*) instrume, detnam, filtstr
	character*(*) comment(maxcomm), hist(maxhist)
	character*(*) proc_ver, proc_date, proc_time
c
c Description:
c  Reads an EXOSAT TGS package from an SF files and does some minor 
c fiddling around
c
c Authors/Modification History
c Ian M George (1994 Feb 22), orginal
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	rewind(iunit)
	ierr = 0
	errstr = '** CNV_EXOTGS ERROR:'
	wrnstr = '** CNV_EXOTGS WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using CNV_EXOTGS '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' EXOSAT TGS conversion by CNV_EXOTGS',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Read the package from the SF file
      	call rd_sfexotgs(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		instrume, detnam,filtstr,
     &		proc_ver, proc_date, proc_time,
     &		x0, y0, width, angle, dtcorr, sscorr,
     &		ierr)
	if(ierr.NE.0) goto 482

c Error check
482	if(ierr.NE.0) then
	   message = errstr // 'Aborting'
	   call fcecho(message)
	endif

      return
      end
c ----------------------------------------------------------------------
*+ RD_SFOPN
      	subroutine rd_sfopn(sffil,chatter,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		iunit, ierr)

	IMPLICIT NONE
	integer chatter, maxhist, maxcomm, ierr
	integer iunit, nk_hist, nk_comm
	character*(*) sffil
	character*(*) hist(maxhist), comment(maxcomm)
c 
c Description:
c  Opens an SF file & reads the history records
c
c Authors/Modification History
c  Ian M George    (1993 Jun 21), original
	character(7) version
	parameter (version = '1.0.0')
*-
c Max Array Sizes
        integer*4 maxsize, maxchanen
        parameter (maxchanen=4096,maxsize=32768)
c Internals 
	integer ierrsf, nhist, irec, lr, lc, i
	integer fcstln
        byte buffer(maxsize)
	character(30) errstr, wrnstr
	character(80) message, header, txtstr, tmplte
        character(255) wrtstr, cbuf
c Initialize
	ierrsf = 0
	ierr = 0
	errstr = '** RD_SFOPN ERROR:'
	wrnstr = '** RD_SFOPN WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFOPN '// version
		call fcecho(message)
	endif

c Get a free logical unit & open the SF file
      	CALL cgetlun(iunit)
      	CALL copnrsf(sffil, iunit, 'XSPEC data', header, tmplte, 
     &		nhist, ierrsf)
	if(ierrsf.NE.0) then
		write(message,'(2a,i12)') errstr,
     &			'COPNRSF ierrsf = ', ierrsf
		call fcecho(message)
		ierr = 1
		return
	endif

c Dump initial info into history array
	write(txtstr,'(2a)') ' SF file opened by RD_SFOPN ',
     &                          version
	hist(1) = txtstr
	write(hist(2),'(a)')
     &		' (SF) PHA file history records'
	write(txtstr,'(2a)') ' - PHA file header:',
     &		header(27:fcstln(header))
	hist(3) = txtstr
	write(txtstr,'(a,i12)') ' - No. of history records: ', nhist
	hist(4) = txtstr
	nk_hist = 4

c Dump (old) SF history records to the user & into (new) history array
c --- This code taken almost directly from PTXTSF -------------------------
        if(nhist.NE.0) then
                nk_hist = nk_hist + nhist
                irec = 0
                do while(irec.NE.nhist)
                  read(iunit) lr,(buffer(i),i=1,min(maxsize,lr))
                  if(lr.gt.0) then
                    irec=irec+1
                    lc=min(maxsize,lr)
                    if(lr.gt.lc) then
                        if(chatter.GE.10) then
                                write(message,'(2a,i12,a)') wrnstr,
     &                                  'Record too full:',lr,
     &                                  'some text lost'
                                call fcecho(message)
                        endif
                    endif
                    cbuf = ' '
                    do i = 1, lc
                        write(cbuf(i+1:i+1),'(a1)') buffer(i)
                    enddo
                    write(wrtstr,'(a2,a)') ' -',cbuf(:lc+1)
                    hist(2+irec) = wrtstr(:80)
                  else
                    irec = nhist
                        if(chatter.GE.10) then
                                write(message,'(2a)') wrnstr,
     &                                  'Badly terminated package'
                                call fcecho(message)
                        endif
                  endif
                enddo
        endif

c Give User info if requested
	if(chatter.ge.10) then
		do i = 1, nk_hist
			write(message,'(a)') hist(i)
			call fcecho(message)
		enddo
	endif
c Add additional history record relating to this programme
	write(txtstr,'(2a)') ' SF file opened by RD_SFOPN ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = txtstr

	return
	end

c ----------------------------------------------------------------------
*+ RD_SFINFO
      	subroutine rd_sfinfo(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     & 		qfinfo, instrume, ipha, ichan, expos, area,
     &		bscale, cscale,indices, 
     &		qass, backfil, respfil,corrfil,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer ipha, ichan
	real expos,area, bscale, cscale
	real indices(3)
	character*(*) instrume
	character*(*) hist(maxhist), comment(maxcomm)
	character*(*) backfil, respfil, corrfil
	logical qfinfo, qass
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1.0.0:1993 Aug 16), took out unnecessary structures
	character(7) version
	parameter (version = '1.0.0')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'phasf.inc' --------------------------------------------------
c	include file PHASF
c	defines structures for use with the PHA SF files

	character(16) detid
	integer*4 len_file_info
	parameter(len_file_info=100)
	structure /file_info/
	    character(16) detid
	    integer*4	nbin, nchan
	    real*4	t,a,del,cor,index(3)
	    byte	spare(48)
	end structure
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/file_info/finfo
             endmap
	   end union
	end structure
	record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4     ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4),  nstrot, 
     &          i
      CHARACTER*(72) strar(50)
      character(12) pkgtyp
      LOGICAL*4 qdone

	integer ip
	character(80) message
	character(20) errstr, wrnstr

c
c Inintialize
	rewind(iunit)
	nhist = 0
	ierrsf = 0
	qfinfo = .false.
	qass = .false.
	errstr = '** RD_SFINFO ERROR:'
	wrnstr = '** RD_SFINFO WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFINFO '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF File Info package scanned by RD_SFINFO ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
	 ip = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
	    ip = ip + 1
c File/Detector Info
            IF (pkgtyp.EQ.'file info ') THEN
		qfinfo = .true.
		detid = all.finfo.detid
		instrume = detid
		ipha     = all.finfo.nbin
		ichan    = all.finfo.nchan
		expos    = all.finfo.t
		area 	 = all.finfo.a
		bscale	 = all.finfo.del
		cscale 	 = all.finfo.cor
		do i = 1, 3
			indices(i)  = all.finfo.index(i)
		enddo
c ... write o/p in same style as RDPHA produced into comment array
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		comment(nk_comm+1) = message
		write(comment(nk_comm+2),'(2a)')
     &		  ' Detector Identity      :', detid
		write(comment(nk_comm+3),'(a,i12)')
     &		  ' No. of data bins       :', ipha
		write(comment(nk_comm+4),'(a,i12)')
     &		  ' Unbinned data channels :', ichan
		write(comment(nk_comm+5),'(a,f9.2)')
     &		  ' Integration time(secs) :', expos
		write(comment(nk_comm+6),'(a,f9.2)')
     &		  ' On source area (cm**2) :', area
		write(comment(nk_comm+7),'(a,f11.3)')
     &		  ' Background scale factor:', bscale
		write(comment(nk_comm+8),'(a,f11.3)')
     &		  ' Correction scale factor:', cscale
		write(comment(nk_comm+9),'(a,3f15.7)')
     &		  ' Indices                :',indices
		nk_comm = nk_comm + 9
            ELSEIF (pkgtyp.EQ.'ass. files ') THEN
	       qass = .true.
               nstrot = 3
               CALL crstrsf(iunit, strar, 3, nstrot, ierrsf)
c ... write o/p in same style as RDPHA produced into comment array
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		comment(nk_comm+1) = message
		backfil = strar(1) 
		respfil = strar(2)
		corrfil = strar(3)

		write(comment(nk_comm+2),'(2a)')
     &		  ' Background Filename    :', backfil(:45)
		write(comment(nk_comm+3),'(2a)')
     &		  ' Response Filename      :', respfil(:45)
		write(comment(nk_comm+4),'(2a)')
     &		  ' Correction Filename    :', corrfil(:45)
		nk_comm = nk_comm + 4

	    ENDIF
	   ENDIF
	ENDDO

	return
	end
c ----------------------------------------------------------------------
*+ RD_SFSINFO
      	subroutine rd_sfsinfo(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     & 		obsepoch, shfstart, date_obs, time_obs,
     & 		shfstop, date_end, time_end, mjdobs,
     &		sfrm, ra_vec, dec_vec, roll, sname,
     &		qsinfo, ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	real shfstart, shfstop
	real obsepoch, ra_vec, dec_vec, roll, sfrm
	double precision mjdobs
	character*(*) date_obs, time_obs, date_end, time_end
	character*(*) hist(maxhist), comment(maxcomm)
	character*(*) sname
	logical qsinfo
c 
c Description:
c   Read the standard SF source info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal 
	character(7) version
	parameter (version = '1.0.1')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c from 'phasf.inc' --------------------------------------------------
	integer*4 len_source_info
	parameter(len_source_info=72)
	structure /source_info/
	    real*8	epoch,start,stop
	    integer*4	system
	    real*8	vect(3)
	    character(20)	name
	end structure

c ----------------------------------------------------------------------
c from 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/source_info/sinfo
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      	integer ierrsf, nhist, lenbuf, index, nsubs, infoar(4)
	integer fcstln
	integer ip, iy,im,id,ih,is
      	INTEGER*4 key
      	INTEGER*4 timarr(5)
	character(68) date,time
      	character(12) pkgtyp
      	LOGICAL*4 qdone
	character(80) message
	character(20) errstr, wrnstr
	double precision ss_is
        integer ydum,mdum,ddum
c
c Inintialize
	rewind(iunit)
	nhist = 0
	ierrsf = 0
	qsinfo = .false.
	errstr = '** RD_SFSINFO ERROR:'
	wrnstr = '** RD_SFSINFO WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFSINFO '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF Source Info package scanned by RD_SFSINFO ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
	 ip = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
	    ip = ip + 1
c File/Detector Info
            IF (pkgtyp.EQ.'source info') THEN
		qsinfo   = .true.
		obsepoch = all.sinfo.epoch
		shfstart = all.sinfo.start
		key = shfstart
		call cshfdmy(key,date_obs, time_obs, ierrsf)
		shfstop  = all.sinfo.stop
		key = shfstop
		call cshfdmy(key,date_end, time_end, ierrsf)
		if(all.sinfo.system.EQ.0) then
			sfrm = 1950.0
		else
			sfrm = all.sinfo.system
		endif
		ra_vec 	     =  all.sinfo.vect(1)*360.*1.E-9
		dec_vec	     =  all.sinfo.vect(2)*360.*1.E-9
		roll 	     =  all.sinfo.vect(3)
		sname    = all.sinfo.name
c ... write o/p in same style as RDPHA produced into comment array
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		comment(nk_comm+1) = message
		write(comment(nk_comm+2),'(a,g18.8)')
     &		  ' Observation Epoch      :', obsepoch

		key = shfstart
               CALL timka(key, timarr)
               WRITE (comment(nk_comm+3), '(a,1pg18.8,0p,a,2i4,3i3,a)')
     &                 ' SHF   start time:', shfstart, '  (',
     &                timarr, ')'

		key = shfstop
               CALL timka(key, timarr)
               WRITE (comment(nk_comm+4), '(a,1p,g18.8,0p,a,2i4,3i3,a)')
     &                 ' SHF     end time:', shfstop, '  (',
     &                timarr, ')'
		nk_comm = nk_comm + 4

               IF (all.sinfo.system.EQ.0) THEN
		  nk_comm = nk_comm + 1
                  WRITE (comment(nk_comm), '(a)') 
     &			' 1950 RA and DEC and Roll'
               ENDIF

               WRITE (comment(nk_comm+1), '(a,1p3g18.8)') 
     &			' source vector: ', all.sinfo.vect

               WRITE (comment(nk_comm+2), '(2a)') ' source name  : ',
     &                         sname(:fcstln(sname))
		nk_comm = nk_comm + 2

c Calculate the mean MJD of the observation (wot a bloody yawn)
		key = (shfstop + shfstart)/2
		call cshfdmy(key,date, time, ierrsf)
		ierrsf = 0
		call fts2dt(date,iy,im,id,ierrsf)
c		 read(date(1:2),'(i)') id
c		 read(date(4:5),'(i)') im
c		 read(date(7:8),'(i)') iy
		ierrsf = 0
		call ccaldj(iy,im,id,mjdobs, ierrsf)
		ierrsf = 0
		call fts2tm(time,ydum,mdum,ddum,ih,im,ss_is,ierrsf)
		is = int(ss_is)
		ierrsf = 0
c		 read(time(1:2),'(i)') ih
c		 read(time(4:5),'(i)') im
c		 read(time(7:8),'(i)') is
		mjdobs = mjdobs + (ih/24.) + (im/(24.*60)) 
     &				+ (is/(24.*3600))
	    ENDIF
	   ENDIF
	ENDDO





	return
	end
c ----------------------------------------------------------------------
*+ RD_SFDBASE
      	subroutine rd_sfdbase(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		radecsys, equinox,
     &		ra, dec, roll, shf_start, shf_stop,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	real ra, dec, roll, shf_start, shf_stop, equinox
	character*(*) radecsys
	character*(*) hist(maxhist), comment(maxcomm)
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1993 Jul 18), split Grping bit from PHA bit
	character(7) version
	parameter (version = '0.9.0')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'phasf.inc' --------------------------------------------------

	integer*4 len_data_base
	parameter(len_data_base=40)
	structure /data_base/
	    integer*4 source_ra,source_dec
	    integer*4 roll
	    integer*4 shf_start,shf_end
	    character(6) name
	    byte      spare(14)
	end structure
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/data_base/dbase
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4     ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4)  
      character(12) pkgtyp
      LOGICAL*4 qdone
c      REAL*4 psum, perr, ra, dec
c      INTEGER*4 key
c      INTEGER*4 timarr(5)

	real ra_db, dec_db
	character(80) message
	character(20) errstr, wrnstr
c (G_string must stretch to length = maxchan)

c
c Inintialize
	nhist = 0
	rewind(iunit)
	ierr = 0
	errstr = '** RD_SFDBASE ERROR:'
	wrnstr = '** RD_SFDBASE WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFDBASE '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF data base package scanned by RD_SFDBASE ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c Grouping Card
            IF (pkgtyp.EQ.'data base ') THEN
c ... write o/p in same style as RDPHA produced into comment array
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
               ra_db = all.dbase.source_ra*360.*1.E-9
               dec_db = all.dbase.source_dec*90.*1.E-9
                    write(message, '(a,2f12.5)')
     &                  ' Source RA and DEC: ', ra_db, dec_db
                    nk_comm = nk_comm + 1
                    comment(nk_comm) = message
                    write(message, '(a,i4)')
     &                  ' Roll angle: ', all.dbase.roll
                    nk_comm = nk_comm + 1
                    comment(nk_comm) = message
                    write(message, '(a,2i16)')
     &                  ' SHF start and stop: ', 
     &			all.dbase.shf_start, all.dbase.shf_end
                    nk_comm = nk_comm + 1
                    comment(nk_comm) = message
		if(ra.EQ.0.0 .AND. dec.EQ.0.0 .AND. roll.EQ.0.0) then
		   ra = ra_db
		   dec = dec_db
		   roll = all.dbase.roll
		   goto 123
	        else
		  if(ra_db.NE.ra) then
		    message = wrnstr // 'Database RA mismatch'
		    call fcecho(message)
		    write(message,'(a,f12.5,a,f12.5)') 
     &		    ' ...  found RA = ', ra_db, 
     &		    	'; expected RA = ', ra
		    call fcecho(message)
		    ierr = 1
		    goto 482
	  	  endif
		  if(dec_db.NE.dec) then
		    message = wrnstr // 'Database DEC mismatch'
		    call fcecho(message)
		    write(message,'(a,f12.5,a,f12.5)') 
     &		    ' ...  found dec = ', dec_db, 
     &		    	'; expected dec = ', dec
		    call fcecho(message)
		    ierr = 1
		    goto 482
	  	  endif
		  if(all.dbase.roll.NE.roll) then
		    message = wrnstr // 'Database ROLL mismatch'
		    call fcecho(message)
		    write(message,'(a,f12.5,a,f12.5)') 
     &		    ' ...  found roll = ', all.dbase.roll, 
     &		    	'; expected roll = ', roll
		    call fcecho(message)
		    ierr = 1
		    goto 482
	  	  endif
		endif
123		if(all.dbase.shf_start.NE.shf_start) then
		  message = wrnstr // 'Database SHF start mismatch'
		  call fcecho(message)
		  write(message,'(a,f12.5,a,f12.5)') 
     &		  ' ...  found SHF start = ', all.dbase.shf_start, 
     &		    	'; expected SHF start = ', shf_start
		  call fcecho(message)
		  ierr = 1
		  goto 482
	  	endif
		if(all.dbase.shf_end.NE.shf_stop) then
		  message = wrnstr // 'Database SHF stop mismatch'
		  call fcecho(message)
		  write(message,'(a,f12.5,a,f12.5)') 
     &		  ' ...  found SHF stop = ', all.dbase.shf_end, 
     &		    	'; expected SHF stop = ', shf_stop
		  call fcecho(message)
		  ierr = 1
		  goto 482
	  	endif
	endif
	endif
	enddo

c Add the Celestial Coordinate system stuff, if not already known
	if(equinox.EQ.0.0)then
		equinox = 1950.0
	endif
	if(radecsys.EQ.' ')then
		radecsys = 'FK4' 
c ... or is it 'FK4-NO-E' 
c alternative is 'FK5'
	endif

482	if(ierr.NE.0) then
		message = ' ... skipping rest of package'
		call fcecho(message)
	else
		message = ' ... Database package confirmed OK'
	endif

	return
	end
c ----------------------------------------------------------------------
*+ RD_SFSLCT
      	subroutine rd_sfslct(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		nsel, seltype, nseld, nselr, nselc,
     &		selnm, seld, selr, selc, 
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer nsel, nseld(10), nselr(10), nselc(10)
	double precision seld(10,10)
	real selr(10,10)
	character*(*) seltype(10), selnm(10,10,10), selc(10,10)
	character*(*) hist(maxhist), comment(maxcomm)
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1993 Jul 18), split Grping bit from PHA bit
	character(7) version
	parameter (version = '1.1.0')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c ----------------------------------------------------------------------
c INCLUDE 'selectorsf.inc' ---------------------------------------------
c	SELECTORSF.INC
c		Include file for the definitions of the SF Selector records
	integer*4 len_selectors
	parameter(len_selectors=10)
	structure /selectors/
	    character(10) type
        end structure

	integer*4 len_time_sel
	parameter(len_time_sel=16)
	structure /time_selectors/
	    real*8 min
c                  start time of time window (units as start and stop 
c                  times in 'source info' package.
	    real*8 max
c                  end time of time window
        end structure

	integer*4 len_phase_sel
	parameter(len_phase_sel=28)
	structure /phase_selectors/
	    real*4 phmin
	    real*4 phmax
c                  phase range
	    real*8 period
c                  folding period in sec
	    real*8 phase0
c                  phase 0 (units as start and stop times)
	    integer*4 nphase
c                     total no of phase bins
        end structure

	integer*4 len_intensity_sel
	parameter(len_intensity_sel=37)
	structure /intensity_selectors/
	    real*4 rmin
	    real*4 rmax
c                  intensity range in counts per sec
	    integer*4 startchan
	    integer*4 endchan
c                     channels used for count rates
	    real*4 tbin
c                  time resolution used for count rates (in sec)
	    logical*1 bgsub
c                     flag to indicate if count rates are background 
c                     subtracted.
	    character(16) detid 
c                        Detectors used for count rates (can be SAME)
        end structure
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
c selector packages:
             map
                record/selectors/sel
             endmap	
             map
                record/time_selectors/tsel
             endmap
             map
                record/phase_selectors/psel
             endmap
             map
                record/intensity_selectors/isel
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4     ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4), length,
     &          i 
      CHARACTER*(72) strar(50)
      character(12) pkgtyp
      LOGICAL*4 qdone
c      REAL*4 psum, perr, ra, dec
c      INTEGER*4 key
c      INTEGER*4 timarr(5)

	integer ip, fcstln
	character(80) message
	character(20) errstr, wrnstr

c
c Inintialize
	rewind(iunit)
	nhist = 0
	ierr = 0
	errstr = '** RD_SFSLCT ERROR:'
	wrnstr = '** RD_SFSLCT WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFSLCT '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF selectors package scanned by RD_SFSLCT ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
	 ip = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c The 'selectors'
            IF (pkgtyp.EQ.'selectors  ') THEN
		ip = ip + 1
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
		    seltype(ip) = all.sel.type
                    write(message, '(2a)')
     &                  ' Selector of type ', all.sel.type
                    nk_comm = nk_comm + 1
                    comment(nk_comm) = message
               IF (nsubs.GT.0) THEN
                  IF (all.sel.type.EQ.'time') THEN
		     nseld(ip) = 2
		     nselr(ip) = 0
		     nselc(ip) = 0
                     DO i = 1, nsubs
                        length = len_time_sel
                        CALL crsubsf(iunit, all, length, ierrsf)
			selnm(ip,1,1) = 'Time window min'
			seld(ip,1) = all.tsel.min
			selnm(ip,2,1) = 'Time window max'
			seld(ip,2) = all.tsel.max
                        write(message, '(a, D10.5,1x,D10.5)')
     &                  ' Time window: ', seld(ip,1), seld(ip,2)
                        nk_comm = nk_comm + 1
                        comment(nk_comm) = message
                     ENDDO
                  ELSEIF (all.sel.type.EQ.'phase') THEN
		     nseld(ip) = 2
		     nselr(ip) = 3
		     nselc(ip) = 0
                     DO i = 1, nsubs
                        length = len_phase_sel
                        CALL crsubsf(iunit, all, length, ierrsf)
			selnm(ip,1,2) = 'Phase window min'
			selr(ip,1) = all.psel.phmin
			selnm(ip,2,2) = 'Phase window max'
			selr(ip,2) = all.psel.phmax
                          write(message, '(a, 2f10.5)')
     &                    ' Phase window: ', selr(ip,1), selr(ip,2)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
			selnm(ip,1,1) = 'Phase Fold period'
			seld(ip,1) = all.psel.period
                          write(message, '(a, D10.5)')
     &                    ' Folding Period: ', seld(ip,1)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
			selnm(ip,2,1) = 'Phase zero'
			seld(ip,2) = all.psel.phase0
                          write(message, '(a, D10.5)')
     &                    ' Phase 0: ', seld(ip,2)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
			selnm(ip,3,2) = 'No. Phase bins'
			selr(ip,3) = all.psel.nphase
                          write(message, '(a, f10.2)')
     &                    ' Number of Phase bins: ', selr(ip,3)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                     ENDDO
                  ELSEIF (all.sel.type.EQ.'intensity') THEN
		     nseld(ip) = 0
		     nselr(ip) = 5
		     nselc(ip) = 2
                     DO i = 1, nsubs
                        length = len_intensity_sel
                        CALL crsubsf(iunit, all, length, ierrsf)
			selnm(ip,1,2) = 'Intensity window min'
			selr(ip,1) = all.isel.rmin
			selnm(ip,2,2) = 'Intensity window max'
			selr(ip,2) = all.isel.rmax
                          write(message, '(a, 2f10.5)')
     &                    ' Intensity window: ', selr(ip,1), selr(ip,2)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
			selnm(ip,3,2) = 'Intensity window, chan min'
			selr(ip,3) = all.isel.startchan
			selnm(ip,4,2) = 'Intensity window, chan max'
			selr(ip,4) = all.isel.endchan
			selnm(ip,5,2) = 'Intensity window, time res'
			selr(ip,5) = all.isel.tbin
			selnm(ip,1,3) = 'Intensity window, detectors'
			selc(ip,1) = all.isel.detid
			selnm(ip,2,3) = 'Intensity window, bkgdsub'
                        IF (all.isel.bgsub) THEN
			   selc(ip,2) = 'TRUE'
			ELSE
			   selc(ip,2) = 'FALSE'
			ENDIF 
			message = ' The following criteria '//
     &                         'have been used for the count rates: '
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                          write(message, '(a, 2f10.5)')
     &                    ' Channels: ', selr(ip,3), selr(ip,4)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                          write(message, '(a, f10.5)')
     &                    ' Time resolution: ', selr(ip,5)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                          write(message, '(2a)')
     &                    ' Detectors: ', selc(ip,1)
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                          IF (all.isel.bgsub) THEN
			   message = ' Background subtracted'
                        ELSE
			   message = ' Not background subtracted'
                        ENDIF
                          nk_comm = nk_comm + 1
                          comment(nk_comm) = message
                     ENDDO
                  ENDIF
               ENDIF
c The 'filters'
            ELSEIF (pkgtyp.EQ.'filters') THEN
	       ip = ip + 1
		seltype(ip) = 'filters'
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
               CALL crstrsf(iunit, strar, 50, nsubs, ierrsf)
		nseld(ip) = 0	
		nselr(ip) = 0
		nselc(ip) = nsubs
               DO i = 1, nsubs
		   selnm(ip,i,3) = ' '
		   selc(ip,i) =  strar(i)(:fcstln(strar(i)))
		   message = ' Filter : '// strar(i)(:fcstln(strar(i)))
                   nk_comm = nk_comm + 1
                   comment(nk_comm) = message
		   if(chatter.GE.10) call fcecho(message)
               ENDDO
	endif
	endif
	enddo

482	if(ierr.NE.0) then
		message = ' ... skipping rest of package'
		call fcecho(message)
	endif

	nsel = ip

	return
	end
c ----------------------------------------------------------------------
*+ RD_SFPHA
      	subroutine rd_sfpha(iunit,chatter,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		phatyp, npha, phadat, qerr, phaerr,
     &		ierr)
	
	IMPLICIT NONE
	integer chatter, maxhist, maxcomm, ierr, iunit
	integer nk_hist, nk_comm, npha, phatyp
	real phadat(*), phaerr(*)
	logical qerr
	character*(*) hist(maxhist), comment(maxcomm)
c 
c Description:
c
c Authors/Modification History
	character(7) version
	parameter (version = '0.8.0')
*-




c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                real*4 pha(maxchan)
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4  ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4),
     &          i
      character(12) pkgtyp
      LOGICAL*4 qdone
      REAL*4 err(maxchan)

	character(80) message
	character(20) errstr, wrnstr

c
c Inintialize
	rewind(iunit)
	qerr = .false.
	phatyp = 0
	nhist = 0
	ierrsf = 0
	errstr = '** RD_SFPHA ERROR:'
	wrnstr = '** RD_SFPHA WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFPHA '// version
		call fcecho(message)
	endif

c Add additional history record relating to this routine
	write(message,'(2a)') 
     &		' SF PHA data package scanned by RD_SFPHA ',
     &			version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message     

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c Simple PHA counts data (no errors)
            IF (pkgtyp.EQ.'pha data ') THEN
		write(message,'(3a)') ' ***** Package "',pkgtyp,
     &			'" ***** (located)'
		phatyp = 1
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
               	   DO i = 1, npha
			phadat(i) = all.pha(i)
			phaerr(i) = 0.0
		   ENDDO 
c PHA data with errors
            ELSEIF (pkgtyp.EQ.'pha per sec ' .OR. 
     &          pkgtyp.EQ.'pha per cm2s ') THEN
		write(message,'(3a)') ' ***** Package "',pkgtyp,
     &			'" ***** (located)'
		if(chatter.GE.10) call fcecho(message)
		phatyp = 2
		qerr = .true.
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
                CALL cnxpksf(iunit, 'pha errors', index, nsubs, infoar, 
     &                     err, lenbuf, .FALSE., ierrsf)
               	DO i = 1, npha
			phadat(i) = all.pha(i)
			phaerr(i) = err(i)
		ENDDO 
            ENDIF
         ENDIF
      ENDDO

	return
	end
c ----------------------------------------------------------------------
*+ RD_SFSYS
      	subroutine rd_sfsys(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		npha, qsys, sys, ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer npha
	integer sys(*)
	character*(*) hist(maxhist), comment(maxcomm)
	logical qsys
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1993 Jul 18), split Grping bit from PHA bit
	character(7) version
	parameter (version = '0.9.0')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                real*4 sysfrac(maxchan)
             endmap	
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4  ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4),
     &          i
      character(12) pkgtyp
      LOGICAL*4 qdone

	integer nsys
	character(80) message
	character(20) errstr, wrnstr
c
c Initialize
	rewind(iunit)
	nhist = 0
	ierrsf = 0
	qsys = .false.
	errstr = '** RD_SFSYS ERROR:'
	wrnstr = '** RD_SFSYS WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFSYS '// version
		call fcecho(message)
	endif


c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c Grouping Card
            IF (pkgtyp.EQ.'systematics') THEN
c ... Add additional history record relating to this programme
c ... and write o/p in same style as RDPHA produced into comment array
	        write(message,'(2a)') 
     &		 ' SF Systematics package scanned by RD_SFSYS ',
     &				version
	        nk_hist = nk_hist + 1
	        hist(nk_hist + nhist) = message
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
		nsys = lenbuf
		if(nsys.NE.npha) then
		  message = wrnstr // 'Systematics/channels mismatch'
		  call fcecho(message)
		  write(message,'(a,i12,a,i12)') 
     &		  ' ... number of sys values = ', nsys, 
     &		    	'; number of channels = ', npha
		  call fcecho(message)
		  message = ' ... skipping the package ' //
     &			'(Grouping will NOT be set by RD_SFGRP)'
		  call fcecho(message)
		  return
	        else
		  qsys = .true.
		  do i = 1, nsys 
		     sys(i) = all.sysfrac(i)
	 	  enddo 
		endif
	endif
	endif
	enddo

	return
	end

c ----------------------------------------------------------------------
*+ RD_SFGRP
      	subroutine rd_sfgrp(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		npha, qgrp, gcards, grpg, qqual, qual,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer npha, gcards
	integer grpg(*), qual(*)
	character*(*) hist(maxhist), comment(maxcomm)
	logical qgrp, qqual
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1993 Jul 18), split Grping bit from PHA bit
c Ian M George (1994 Feb 04), fixed error so that grp flag F when no grping
	character(7) version
	parameter (version = '1.0.0')
*-

c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)

       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                character(4096) grouping
c grouping must have length = maxchan
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
      INTEGER*4  ierrsf, nhist, 
     &          lenbuf, index, nsubs, infoar(4),
     &          i, j, k
      character(12) pkgtyp
      LOGICAL*4 qdone

	character(80) message
	character(20) errstr, wrnstr
        character(4096) G_string
c (G_string must stretch to length = maxchan)

c
c Inintialize
	rewind(iunit)
	nhist = 0
	ierrsf = 0
	qgrp = .false.
	errstr = '** RD_SFGRP ERROR:'
	wrnstr = '** RD_SFGRP WARNING:'

c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFGRP '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF Grouping package scanned by RD_SFGRP ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist + nhist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c Grouping Card
            IF (pkgtyp.EQ.'grouping   ') THEN
		qgrp = .true.
c ... write o/p in same style as RDPHA produced into comment array
		write(message,'(3a)')' ***** Package "',pkgtyp,
     &				'" *****'
		if(chatter.GE.10) call fcecho(message)
		nk_comm = nk_comm + 1
		comment(nk_comm) = message
		G_string = all.grouping
		gcards = lenbuf
		j = 0
		do i = 1, gcards
		  j = j+1 
		  if(j .EQ. 51) then
		    j = 1
		    write(message, '(5(2x,5a1,1x,5a1))')
     &			(G_string(k:k), k = i-50, i-1)
		    nk_comm = nk_comm + 1
		    comment(nk_comm) = message
		  endif
		enddo
		if(gcards.NE.npha) then
		  message = wrnstr // 'Grouping card/channels mismatch'
		  call fcecho(message)
		  write(message,'(a,i12,a,i12)') 
     &		  ' ... number of grp cards = ', gcards, 
     &		    	'; number of channels = ', npha
		  call fcecho(message)
		  message = ' ... skipping the package ' //
     &			'(Grouping will NOT be set by RD_SFGRP)'
		  call fcecho(message)
		  goto 194
	        endif 
	endif
	endif
	enddo

c Jump out if not grouped
	if(.NOT.qgrp) then
	  message = ' ... Grouping card not defined for SF data'
	  if(chatter.GE.20) call fcecho(message)
	  nk_comm = nk_comm + 1
	  comment(nk_comm) = message
	  goto 194
	endif


c Pass the grouping card
	do i = 1, gcards
	  qual(i) = 0
	  if(G_string(i:i) .EQ. '+') then
		grpg(i) = 1
	  elseif(G_string(i:i) .EQ. '-') then
		grpg(i) = -1
	  elseif(G_string(i:i) .EQ. '*') then
		grpg(i) = 0
		qual(i) = 5
	  elseif(G_string(i:i) .EQ. '.') then
		grpg(i) = 0
		qual(i) = 1
	  else
		message = errstr // 'Unknown character in grpg card'
		call fcecho(message)
	  	nk_comm = nk_comm + 1
	  	comment(nk_comm) = message
		message = ' ... character = ' //
     &			G_string(i:i)
		call fcecho(message)
		message = ' ... skipping the package ' //
     &			'(Grouping will NOT be set by RD_SFGRP)'
		call fcecho(message)
	  	nk_comm = nk_comm + 1
	  	comment(nk_comm) = message
		qgrp = .false.
		goto 194
	  endif
	enddo


c Successful completion
	  qgrp = .true.
	  message = ' ... Grouping card OK by RD_SFGRP'
	  if(chatter.GE.10) call fcecho(message)
	  nk_comm = nk_comm + 1
	  comment(nk_comm) = message


c Set quality flag
194	qqual = qgrp	

	return
	end
c ----------------------------------------------------------------------
*+ RD_SFEXOME
      	subroutine rd_sfexome(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		proc_ver, proc_date, proc_time,
     &		equinox, scx, scy, h1_off, h2_off,
     &		ndets, detstr,dcol,dare,dexp,dagn,
     &		dgco, drco,dgas,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer ndets
	real h1_off, h2_off, equinox
	real scx(3), scy(3)
	real dcol(8), dare(8), dexp(8), dagn(8)
	real dgco(8,4), drco(8,8)
	character(2) dgas
	character*(*) comment(maxcomm), hist(maxhist)
	character*(*) detstr(8)
	character*(*) proc_ver, proc_date, proc_time
c 
c Description:
c   Read the standard SF file info package
c
c Authors/Modification History
c Ian M George (1993 Jun 20), orginal hacked version
c Ian M George (1.0.0:1993 Aug 16), took out unnecessary structures
c Ian M George (1.0.1:1994 Feb 22), cosmetic to proc_ver comment
	character(7) version
	parameter (version = '1.0.1')
*-
c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'detectorsf.inc' --------------------------------------------------
c		Include file used to describe detector information in
c		PHA and RSP files
c
      integer*4 ndettype
      parameter (ndettype=7)

* EXOSAT detectors :
      integer*4 len_exosat_me
      parameter(len_exosat_me=108)
      integer*4 len_exosat_me_sub
      parameter(len_exosat_me_sub=100)
      integer*4 max_det_head_len
      parameter(max_det_head_len=120)

	structure/exosat_me/
	    character(2) gas
	    logical*1 incres,incgn
	    integer*4 revis,shf
	    real*4 h1ang,h2ang
	    logical*1 incdet(8)
	    integer*4 system
	    real*8   scxaxis(3),scyaxis(3)
	    byte      spare(28)
	    end structure

	structure/exosat_me_sub/
	    integer*4 detnum
	    real*4 colrsp,assar,ag,gcoef(4),resol(8),usedrsparea
	    real*4 detintt
	    byte spare(28)
	    end structure
c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)
       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/exosat_me/meinfo
             endmap
             map
                record/exosat_me_sub/mesub
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
        INTEGER i, j,k,  ierrsf, nsubs, lent
        character(12) pkgtyp
	integer key, lenbuf, index, infoar(4)
	integer*4 timarr(5)
	logical qdone, qfound
	character(8) substr
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	qfound = .false.
	rewind(iunit)
	ierrsf = 0
	errstr = '** RD_SFEXOME ERROR:'
	wrnstr = '** RD_SFEXOME WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFEXOME '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF EXOSAT ME package scanned by RD_SFEXOME ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c File/Detector Info
      IF (pkgtyp.EQ.'EXOSAT ME ') THEN
	   qfound = .true.
	   dgas = all.meinfo.gas
	   nk_comm = nk_comm + 1
	   comment(nk_comm) = 
     & 		' Detector type (AX=both argon & xenon) - '// dgas
	   write(proc_ver,'(i12)') all.meinfo.revis 
	   nk_comm = nk_comm + 1
	   comment(nk_comm) = 
     &           ' revision level of info used in production of '
     &           //'file'// proc_ver
           key = all.meinfo.shf
           call timka(key, timarr)
	   call cshfdmy(key, proc_date, proc_time, ierrsf)   
	   nk_comm = nk_comm + 1
           write(comment(nk_comm), '(a,i12,a,2i4,3i3,a)')
     &           ' shf key of creation date', all.meinfo.shf,
     &            ' (', timarr, ')'
	 h1_off = all.meinfo.h1ang
	   nk_comm = nk_comm + 1
           write(comment(nk_comm), '(a,f10.5)') 
     &             ' offset angle of first half(average for multiple'
     &             //' offsets', all.meinfo.h1ang
	 h2_off = all.meinfo.h2ang
	   nk_comm = nk_comm + 1
           write(comment(nk_comm), '(a,f10.5)') 
     &		' offset angle of second half', all.meinfo.h2ang

c S/c coordinates
         IF (all.meinfo.system.EQ.0) THEN
	    equinox = 1950.0
	 ELSE
	    equinox = all.meinfo.system
         ENDIF
	 do i = 1, 3	 
	 scx(i) = all.meinfo.scxaxis(i)
	 scy(i) = all.meinfo.scyaxis(i)
	 enddo

c List detectors included 
	 k = 0
         DO i = 1, 8
            IF (all.meinfo.incdet(i)) THEN
	       nk_comm = nk_comm + 1
               WRITE (comment(nk_comm), '(a,i12,a)') ' detector no.', i, 
     &                'included in accumulation'
	       if(chatter.GE.25)then
		   message = comment(nk_comm)
		   call fcecho(message)
	       endif
		k = k + 1
	    	if(dgas.EQ.'AX') then
			substr = 'AR+XE'
		 else 
			substr = dgas
		 endif
		 write(detstr(k),'(a4,a,a,a8)')
     &			'DET-',CHAR(i+64),' ', substr
            ENDIF
         ENDDO
c Error Check
	if(.NOT.qfound) then
	   ierr = 1
	   return
	endif	

	 ndets = nsubs
         lent = maxsize
         DO i = 1, nsubs
            CALL crsubsf(iunit, all.mesub, lent, ierrsf)
c		... collimator response
	    dcol(i) = all.mesub.colrsp
c		... (assumed) on-axis area
	    dare(i) = all.mesub.assar
c 		... active integration time
	    dexp(i) = all.mesub.detintt
c		... analogue gain setting
	    dagn(i) = all.mesub.ag
c		... gain coeffs
	    do j = 1, 4
	       dgco(i,j) = all.mesub.gcoef(j)
	    enddo
c		... resolution coeffs
	    do j = 1, 8
	       drco(i,j) = all.mesub.resol(j)
	    enddo
         ENDDO
        ENDIF
        ENDIF
	ENDDO


      return
      end
c ----------------------------------------------------------------------
*+ RD_SFEXOGS
      	subroutine rd_sfexogs(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		blw_start, blw_stop, nomgain, gaincor,
     &		proc_ver, proc_date, proc_time,
     &		equinox, scx, scy, 
     &		dcol,darea,qlinchan,linchan,qgaincor,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	integer blw_start,blw_stop
	integer linchan(4) 
	real nomgain, gaincor, equinox
	real scx(3), scy(3)
	real dcol, darea
	logical qgaincor, qlinchan(4)
	character*(*) comment(maxcomm), hist(maxhist)
	character*(*) proc_ver, proc_date, proc_time
c 
c Description:
c   Read the standard SF EXOSAT GSPC package
c
c Authors/Modification History
c Ian M George (1.0.0:1993 Oct 13), original
c Ian M George (1.0.1:1994 Feb 22), cosmetic to proc_ver comment
	character(7) version
	parameter (version = '1.0.1')
*-
c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'detectorsf.inc' --------------------------------------------------
c		Include file used to describe detector information in
c		PHA and RSP files
c
      integer*4 ndettype
      parameter (ndettype=7)

* EXOSAT detectors :
      integer*4 len_exosat_gs
      parameter(len_exosat_gs=120)
      integer*4 max_det_head_len
      parameter(max_det_head_len=120)

	structure/exosat_gs/
	    integer*2 blwstart,blwend
	    real*4 nominalgain,gaincor
	    logical*1 useline(4),usegaincor
	    integer*4 linechan(4),revis,shf,system
	    real*8 scxaxis(3),scyaxis(3)
	    real*4 colrsp,assar,usedrsparea
	    integer*2 flgcolrsp
	    byte spare(13)
	    end structure

c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)
       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/exosat_gs/gsinfo
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
        INTEGER i, ierrsf, nsubs
        character(12) pkgtyp
	integer key, lenbuf, index, infoar(4)
	integer*4 timarr(5)
	logical qdone, qfound
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	qfound = .false.
	rewind(iunit)
	proc_date = ' '
	proc_time = ' '
	ierrsf = 0
	errstr = '** RD_SFEXOGS ERROR:'
	wrnstr = '** RD_SFEXOGS WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFEXOGS '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF EXOSAT GSFC package scanned by RD_SFEXOGS ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN
c File/Detector Info
         IF (pkgtyp.EQ.'EXOSAT GSPC ') THEN
	 qfound = .true.
c ... write o/p in same style as RDPHA produced into comment array
                write(message,'(3a)')' ***** Package "',pkgtyp,
     &                          '" *****'
                comment(nk_comm+1) = message
         if(chatter.GE.10) then
                write(message,'(3a)')' ***** Package "',pkgtyp,
     &                          '" ***** (located)'
		call fcecho(message)
	 endif
	 blw_start = all.gsinfo.blwstart
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,i12)')
     &		' Burst length window start ', blw_start
	 blw_stop = all.gsinfo.blwend
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,i12)')
     &		' Burst length window end   ', blw_stop
	 nomgain = all.gsinfo.nominalgain
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,f10.3)')
     &		' Nominal gain              ', nomgain
	 gaincor = all.gsinfo.gaincor
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,f10.3)')
     &		' Gain correction factor    ', gaincor
	 write(proc_ver,'(i12)') all.gsinfo.revis 
	 nk_comm = nk_comm + 1
	 comment(nk_comm) = 
     &           ' revision level of info used in production of '
     &           //'file'// proc_ver
           key = all.gsinfo.shf
           call timka(key, timarr)
	   nk_comm = nk_comm + 1
           write(comment(nk_comm), '(a,i12,a,2i4,3i3,a)')
     &           ' shf key of obs date', all.gsinfo.shf,
     &            ' (', timarr, ')'
c S/c coordinates
         IF (all.gsinfo.system.EQ.0) THEN
	    equinox = 1950.0
	 ELSE
	    equinox = all.gsinfo.system
         ENDIF
	 do i = 1, 3	 
	  scx(i) = all.gsinfo.scxaxis(i)
	  scy(i) = all.gsinfo.scyaxis(i)
	 enddo

	 dcol = all.gsinfo.colrsp
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,f10.3)')
     &		' Collimator response       ', dcol
	 darea = all.gsinfo.assar
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,f10.3)')
     &		' On-axis area              ', darea

         DO i = 1, 4
	    qlinchan(i) = all.gsinfo.useline(i)
	    linchan(i) = all.gsinfo.linechan(i)
            IF (qlinchan(i)) THEN
	       nk_comm = nk_comm + 1
	        write(comment(nk_comm),'(a,i12,a,i12)')
     &		' line channel ', i, ' = ', linchan(i)
            ENDIF
         ENDDO

	qgaincor = all.gsinfo.usegaincor
	 nk_comm = nk_comm + 1
         IF (qgaincor) then
	   comment(nk_comm) = ' use gain correction factor'
         ELSE
	   comment(nk_comm) = ' use not gain correction factor'
         ENDIF

	ENDIF
        ENDIF
	ENDDO


      return
      end
c ----------------------------------------------------------------------
*+ RD_SFEXOTGS
      	subroutine rd_sfexotgs(chatter,iunit,
     &		maxhist, nk_hist, hist,
     &		maxcomm, nk_comm, comment,
     &		instrume, detnam,filtstr,
     &		proc_ver, proc_date, proc_time,
     &		x0, y0, width, angle, dtcorr, sscorr,
     &		ierr)

	IMPLICIT NONE
	integer iunit, ierr
	integer chatter, maxhist, maxcomm
	integer nk_hist, nk_comm
	real x0, y0, angle
	real dtcorr,sscorr
	integer width
	character*(*) instrume, detnam, filtstr
	character*(*) comment(maxcomm), hist(maxhist)
	character*(*) proc_ver, proc_date, proc_time
c 
c Description:
c   Read the standard SF EXOSAT TGS package
c
c Authors/Modification History
c Ian M George (1.0.0:1994 Nov 22), original
	character(7) version
	parameter (version = '1.0.0')
*-
c Included Files 
c   	- remember, none of them are my doing !!!! [IMG]
c INCLUDE 'detectorsf.inc' --------------------------------------------------
c		Include file used to describe detector information in
c		PHA and RSP files
c
      integer*4 ndettype
      parameter (ndettype=7)

* EXOSAT detectors :
      integer*4 len_exosat_grating
      parameter(len_exosat_grating=100)
      integer*4 max_det_head_len
      parameter(max_det_head_len=120)

        structure/exosat_grating/
            integer*2 leid
c  LE detector number (1 or 2)
            character(3) det
c  Detector type (CMA or PSD)
            integer*2 filter
c  Filter used (number)
            integer*4 revis
c  Revision number of software
            integer*4 shf
c  SHF key of extraction date
            real*4 zerox,zeroy
c  Location in pixels of the zero point
            integer*2 nx
c  Strip width (in pixels)
            real*4 angle
c  Strip angle from Y axis (hopefully 0.0)
            real*4 sample
c  Sample deadtime correction
            real*4 sumsig
c  Sum-signal correction
            real*4 norm
c  Spare normalization factor
            byte spare(59)
            end structure


c ----------------------------------------------------------------------
c INCLUDE 'phaunion.inc' -----------------------------------------------
      integer*4 maxsize, maxchan
      parameter (maxchan=4096)
      parameter (maxsize=32768)
       structure/all_structure/
          union
             map
                byte buffer(maxsize)
             endmap
             map
                record/exosat_grating/grinfo
             endmap
          end union
       end structure
       record/all_structure/all
c ----------------------------------------------------------------------
c End of included files
c Internals
        INTEGER  ierrsf, nsubs
        character(12) pkgtyp
	integer key, lenbuf, index, infoar(4)
	integer*4 timarr(5)
	integer aninteger
	logical qdone, qfound
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	qfound = .false.
	rewind(iunit)
	proc_date = ' '
	proc_time = ' '
	ierrsf = 0
	errstr = '** RD_SFEXOTGS ERROR:'
	wrnstr = '** RD_SFEXOTGS WARNING:'
c START MAIN
c Give the user info if requested
	if(chatter.GE.20) then
		message = ' ... using RD_SFEXOTGS '// version
		call fcecho(message)
	endif

c Add additional history record relating to this programme
	write(message,'(2a)') 
     &		' SF EXOSAT TGS package scanned by RD_SFEXOTGS ',
     &				version
	nk_hist = nk_hist + 1
	hist(nk_hist) = message

c Start reading in the packages from the SF PHA file
	qdone = .false.
      	DO WHILE (.NOT.qdone)
         ierrsf = 0
         lenbuf = maxsize
         pkgtyp = ' '
         index = 0
         CALL cnxpksf(iunit, pkgtyp, index, nsubs, infoar, all.buffer, 
     &               lenbuf, .FALSE., ierrsf)
         qdone = ierrsf.NE.0
         IF (.NOT.qdone) THEN


c File/Detector Info
         IF (pkgtyp.EQ.'EXOSAT GR ') THEN
	 qfound = .true.
c ... write o/p in same style as RDPHA produced into comment array
                write(message,'(3a)')' ***** Package "',pkgtyp,
     &                          '" *****'
                comment(nk_comm+1) = message
         if(chatter.GE.10) then
                write(message,'(3a)')' ***** Package "',pkgtyp,
     &                          '" ***** (located)'
		call fcecho(message)
	 endif

	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,i12)')
     &		' Telescope ID              ', all.grinfo.leid
	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,a)')
     &		' Detector ID               ', all.grinfo.det(:3)
	 write(instrume,'(a,i12)') 'TGS-',all.grinfo.leid
	 call crmvblk(instrume)
	 write(detnam,'(a,a,i12)') all.grinfo.det(:3),'-',
     &                             all.grinfo.leid
	 call crmvblk(detnam)

	 nk_comm = nk_comm + 1
	 write(comment(nk_comm),'(a,i12)')
     &		' Filter ID                 ', all.grinfo.filter
	 aninteger = all.grinfo.filter
	 call cma_filter(chatter,aninteger,filtstr,ierr)

	 write(proc_ver,'(i12)') all.grinfo.revis 

	 nk_comm = nk_comm + 1
	 comment(nk_comm) = 
     &           ' Revision level of info used in production of '
     &           //'file'// proc_ver

           key = all.grinfo.shf
           call timka(key, timarr)
	   call cshfdmy(key, proc_date, proc_time, ierrsf)   
	   nk_comm = nk_comm + 1
           write(comment(nk_comm), '(a,i12,a,2i4,3i3,a)')
     &           ' shf key of extractn', all.grinfo.shf,
     &            ' (', timarr, ')'

	   nk_comm = nk_comm + 1
	   write(comment(nk_comm),'(a,2f15.7)')
     &		'  location 0 point (pixels) ',
     &                            all.grinfo.zerox, all.grinfo.zeroy 

	   nk_comm = nk_comm + 1
	   write(comment(nk_comm),'(a,i5)')
     &		'  strip width (pixels) ', all.grinfo.nx

	   nk_comm = nk_comm + 1
	   write(comment(nk_comm),'(a,f15.7)')
     &		'  strip angle from Y axis ', all.grinfo.angle

	   x0 = all.grinfo.zerox
	   y0 = all.grinfo.zeroy
	   width = all.grinfo.nx
	   angle = all.grinfo.angle

	   nk_comm = nk_comm + 1
	   write(comment(nk_comm),'(a,f15.7)')
     &		'  sample deadtime correction ', all.grinfo.sample
	   dtcorr = all.grinfo.sample

	   nk_comm = nk_comm + 1
	   write(comment(nk_comm),'(a,f15.7)')
     &		'  sum-signal correction ',all.grinfo.sumsig
	   sscorr = all.grinfo.sumsig

	endif
	endif
	enddo

      return
      end
c ----------------------------------------------------------------------
*+ CMA_FILTER
	subroutine cma_filter(chatter,filtid,filtstr,ierr)
	
	IMPLICIT NONE
	integer chatter, filtid, ierr
	character*(*) filtstr

	if(filtid.EQ.1) then
		filtstr = 'CLOSED'
	elseif(filtid.EQ.2) then
		filtstr = 'PPL'
	elseif(filtid.EQ.3) then
		filtstr = '4Lx'
	elseif(filtid.EQ.4) then
		filtstr = 'OPEN'
	elseif(filtid.EQ.5) then
		filtstr = 'Fe Cal'
	elseif(filtid.EQ.6) then
		filtstr = 'AL/P'
	elseif(filtid.EQ.7) then
		filtstr = '3Lx'
	elseif(filtid.EQ.8) then
		filtstr = 'Bor'
	elseif(filtid.EQ.9) then
		filtstr = 'UV'
	else
		filtstr = 'UNKNOWN'
	endif
	return
	end
c ----------------------------------------------------------------------
*+ FIXPHA
	subroutine fixpha(chatter,detchans,nchans, qgrpd, 
     &		comment, nk_comm,
     &		grp_in, qual_in, channel,
     &		pha_data, pha_din, pha_errors,
     &		pha_ein, syserr, pha_sin, 
     &		qualty, grping,
     & 		ierr)

	IMPLICIT NONE
	integer detchans, nk_comm, ierr, nchans, chatter
	integer grp_in(*), qual_in(*), channel(*)
	integer qualty(*), grping(*)
	real pha_data(*), pha_din(*), pha_errors(*)
	real pha_ein(*), syserr(*), pha_sin(*) 
	character*(*) comment(*)
	logical qgrpd
c
c Description
c... fix up the PHA channel numbers with data, and with quality & grouping
c card. This is necessary since the grouping (& hence quality) flags are 
c read in using full (detchans) channel range, whilst data,errors etc are 
c read in using a 'false' scale (giving total of nchans) which ignores any 
c channels defined to be '.' in original SF grouping card.
c This chunk of code was taken out of the main, for DMA reasons
c
c Author/Modification History
c  Ian M George (1994 Mar 09), original
c  Ian M George (1994 Mar 29), bug fixed associated with pre-grouped datasets
*-
c Internals
	integer i, j, kk, ii, imissing, istart
	real mod
	integer iratio, nchans2
	character(80) message
	character(30) wrnstr, errstr
c Initialize
	imissing = 0
	ierr = 0
	iratio = 1
	kk = 0
	errstr = '** FIXPHA ERROR:'	
	wrnstr = '** FIXPHA WARNING:'	

c ... Check to see if original data has been grouped
	do i = 1, detchans
	  if(grp_in(i).EQ.-1) then
		qgrpd = .true.
		goto 829
	  endif
	enddo

829	j = 0
c ---------------------- PreGrouped Data ----------------------------
	if(qgrpd) then
c 	... The case where original data was grouped 
	  message = wrnstr // 'SF data was already grouped'
	  call fcecho(message)
          comment(nk_comm+1) = message
	  message = ' ... UNABLE to reconstruct original PHA dataset'
	  call fcecho(message)
          comment(nk_comm+2) = message
	  message = ' ... OR compressed OBC mode operating'
	  call fcecho(message)
          comment(nk_comm+3) = message
	  message = ' ...... this may restrict the functionality of '//
     &		'some downstream s/w'
	  call fcecho(message)
          comment(nk_comm+4) = message
	  message = ' ...... but is not a cause for alarm'
	  call fcecho(message)
          comment(nk_comm+5) = message
          nk_comm = nk_comm + 5
c Translation table between old grouping card and array values
c    character     grp_in value		quality value
c	+		1		     {undef}	
c	-	       -1		     {undef}
c	*		0			5
c	.		0			1
c	   ... Calculate how many 'dead channels' are at the start
	  do i = 1, detchans
	    if((grp_in(i).EQ.0).AND.(qual_in(i).EQ.1)) then
		imissing = imissing + 1
	    else
		goto 345
	    endif
	  enddo
c	   ... try and calculate the compression factor
345	  do i = imissing+1,detchans
		if(grp_in(i).EQ.1) then
			do kk = i+1,detchans
			  if(grp_in(kk).EQ.1) then
			    iratio = kk - i 
			    istart = i
			    goto 845
			  endif
	  		enddo
		endif
	   enddo
c 	   ... check out whether a linear compression has been applied
c	   bombing out if this is not the case
845	   if(MOD(detchans,nchans).NE.0) then
	      nchans2 = nchans + imissing/iratio
	   else
	      nchans2 = nchans
	   endif
	   if((MOD(detchans,nchans2).NE.0) 
     &	.OR. ((imissing.GT.0).AND.(MOD(imissing,iratio).NE.0))) then
                  message = errstr// ' Nonlinear Channel compression'
                  call fcecho(message)
                  write(message,'(a,i12)')
     &             ' ... Total number det channels      : ', detchans
                  call fcecho(message)
                  write(message,'(a,i12)')
     &             ' ... Number stored det channels     : ', nchans
                  call fcecho(message)
                  write(message,'(a,i12)')
     &             ' ... Calculated compression ratio   : ', iratio
                  call fcecho(message)
                  write(message,'(a,i12)')
     &             ' ... Number "dead" channels at start: ', imissing
                  call fcecho(message)
  	          if(MOD(detchans,nchans2).NE.0) then
		    message = ' ...... Detchans not a multiple '//
     &				'of stored + missing chans'
		    call fcecho(message)
		  elseif(MOD(imissing,iratio).NE.0) then
		    message = ' ...... Missing chans not a multiple '//
     &						'of calcd ratio'
		    call fcecho(message)
		  endif
		  message = ' ...... Unable to proceed'
		  call fcecho(message)
		  ierr = 1
		  goto 482
	  endif

c 	   ... think we have a linear compression, but check we really know
c		what we're doing
	  do kk = istart,detchans,iratio
		  if(grp_in(i).EQ.-1) then 
                	message = errstr// 
     &			' Remapping pre-grouped Channel numbers'
                	call fcecho(message)
			write(message,'(a,i12)')
     &			' ... Total number det channels : ', detchans
                	call fcecho(message)
			write(message,'(a,i12)')
     &			' ... Number stored det channels: ', nchans
                	call fcecho(message)
			write(message,'(a,i12)')
     &			' ... Expected grouping factor  : ', iratio
                	call fcecho(message)
			write(message,'(2a,i12)')
     &			' ... Non-linear grouping detected,'
     &			// ' starting at chan: ', i
                	call fcecho(message)
		    	ierr = 1 
		   	goto 482
		  endif
	  enddo	

c 	  ... rest the no. raw detector channels
	  detchans = detchans/iratio

c 	  ... inform user
	  write(comment(nk_comm+1),'(a,i12)') 
     &	   ' ... SF dataset has had Linear grouping applied; factor: ',
     &			iratio
	  if(chatter.gt.5) call fcecho(comment(nk_comm+1))
	  write(comment(nk_comm+2),'(a,i12)') 
     &		' ...... No. of "raw" detector channels reset to: ',
     &			detchans
	  if(chatter.gt.5) call fcecho(comment(nk_comm+2))
	  nk_comm = nk_comm+ 2
	  if(imissing.GT.0) then
	  nk_comm = nk_comm + 1
	  write(comment(nk_comm),'(a,i12)') 
     &		' ...... No. of "raw" channels ignored at start : ',
     &			imissing
	  if(chatter.gt.5) call fcecho(comment(nk_comm))
	  endif

c	  ... apply the remapping
	   kk = 0
	   do i = 1,detchans*iratio
		if(qual_in(i).NE.1) then
			j = j + 1
			channel(j) = kk/iratio + j
			pha_data(j) = pha_din(j)
			pha_errors(j) = pha_ein(j)
			syserr(j) = pha_sin(j)
			qualty(j) = 0
			do ii = kk+(j-1)*iratio + 1,kk+j*iratio
			  if(qual_in(ii).NE.0) then
				qualty(j) = qual_in(ii)
				goto 153
			  endif
			enddo
153			grping(j) = 1
		else
			kk = kk + 1
		endif
	   enddo
	else	
c ---------------------- Non PreGrouped Data ----------------------------
c 	... The case where original data was ungrouped
	   do i = 1,detchans
		if(qual_in(i).NE.1) then
			j = j + 1
			channel(j) = i
			pha_data(j) = pha_din(j)
			pha_errors(j) = pha_ein(j)
			syserr(j) = pha_sin(j)
			qualty(j) = qual_in(i)
			grping(j) = grp_in(i)
		endif
	   enddo
	endif
c ----------------------- End of Grouping Fun ----------------------------


c	... Final check that everything computes
	if(j.NE.nchans*iratio) then
            message = errstr// ' Remapping of Channel numbers'
            call fcecho(message)
	    write(message,'(a,i12)')
     &	       	' ... Total number det channels : ', detchans
            call fcecho(message)
	    write(message,'(a,i12)')
     &		' ... Number stored det channels: ', nchans*iratio
            call fcecho(message)
            write(message,'(a,i12)')
     &          ' ... Number "dead" channels at start: ', imissing
            call fcecho(message)
	    write(message,'(a,i12)')
     &		' ... Final Remapping index     : ', j
            call fcecho(message)
	    ierr = 1
	endif

482	return
	end
