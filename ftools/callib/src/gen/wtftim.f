*+ WTFTIM
	subroutine wtftim(chatter, ounit,
     &	mjdref,	tstart, tstop, tzero,
     &	timesys,timeunit,clockapp, timeref,tassign,
     &  tierrela, tierabso, telapse, ontime, deadapp, deadcorr,
     &	livetime, vignapp, vignet, ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	real tierrela, tierabso, ontime, deadcorr, vignet
	real telapse, livetime
	double precision mjdref, tstart, tstop, tzero
	character(20) timesys, timeunit, timeref
	character(20) tassign
	logical deadapp, vignapp, clockapp
c
c Description
c   Writes the details of the time of the observation to the current
c  FITS header unit
c  The following keywords can be written (assuming valid values)
c  MJDREF  - 'Mission reference time (in MJD)'
c  TIMESYS - 'Time frame system in use'
c  TIMEUNIT- 'Units for TSTART, TSTOP & TIMEZERO keywords'
c  TSTART  - 'Start time in TIMESYS frame in TIMEUNIT units'
c  TSTOP   - 'Stop time in TIMESYS frame in TIMEUNIT units'
c  TIMEZERO- 'Time zero off-set in TIMESYS frame & TIMEUNIT units'
c  CLOCKAPP- 'Corrections applied for s/c clock ?'
c  TIMEREF - 'Reference frame used for times'
c  TASSIGN - 'Location where time assignment performed'
c  TIERRELA- 'relative errors expressed as a rate'
c  TIERABSO- 'timing precision (seconds)'
c  TELAPSE - 'total time between TSTART & TSTOP (seconds)
c  ONTIME  - 'total time on source (excl deadtime corrections)'
c  DEADAPP - 'Deadtime correction applied ?'
c  DEADCORR- 'Deadtime correction factor'
c  LIVETIME- 'ONTIME corrected for Deadtime effects'
c  VIGNAPP - 'Vignetting correction applied ?'
c  VIGNCORR- 'Vignetting correction factor'
c
c Passed Parameters
c  CHATTER	i   : Chattiness flag (<5 quite, 10=normal,>20 silly)
c  OUNIT        i   : FORTRAN logical unit of o/p file
c  MJDREF  	i   : Mission reference time (in MJD)
c  TSTART  	i   : Start time in TIMESYS frame in TIMEUNIT units
c  TSTOP   	i   : Stop time in TIMESYS frame in TIMEUNIT units
c  TZERO	i   : Time zero off-set in TIMESYS frame & TIMEUNIT units
c  TIMESYS 	i   : Time frame system in use
c  TIMEUNIT	i   : Units for TSTART, TSTOP & TIMEZERO keywords
c  CLOCKAPP     i   : Logical as whether corrections applied for s/c clock
c  TIMEREF      i   : Reference frame used for times
c  TASSIGN      i   : Location where time assignment performed
c  TIERRELA     i   : relative errors (expressed as a rate)
c  TIERABSO     i   : timing precision (seconds)
c  TELAPSE      i   : TSTOP - TSTART (in seconds)
c  ONTIME  	i   : total seconds on source (excl deadtime corrections)
c  DEADAPP 	i   : Logical as whether deadtime correction applied
c  DEADCORR     i   : Deadtime correction factor
c  LIVETIME     i   : 'ONTIME corrected for Deadtime effects'
c  VIGNAPP 	i   : Logical as whether vignetting correction applied
c  VIGNET       i   : Vignetting correction factor
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c  subroutine FTPCOM            : (FITSIO) writes a comment keyword
c  subroutine FTPHIS            : (FITSIO) writes a history keyword
c  subroutine FTPKYn            : (FITSIO) writes a keyword of type n
c  subroutine WT_FERRMSG        : (CALLIB) writes FITSIO error message etc
c
c Origin
c   An IMG original
c
c Authors/Modification History
c  Ian M George    (1993 Jun 20), original
c  Ian M George    (1.0.1:1993 Oct 07) TIMVERSN keyword NOT written
c  Ian M George    (1.0.2:1994 Apr 01) DEADAPP replaced by COMMENT
c		when DEADAPP = .false. & DEADCORR = 0.0 (ie default value)
c			ditto VIGNCORR & VIGNAPP
c  Jeff Guerber (1.0.3 1998-02-25) telapse is real so use ftpkyf not ftpkyd
c
	character(7) version
	parameter (version = '1.0.3')
*-

c Internals
	integer status, decimals, ddecimals
	parameter (decimals = 6, ddecimals=12)
	character(30) errstr, wrnstr
	character(80) message

c Initialize
	errstr = '** WTFTIM '//version//' ERROR: '
	wrnstr = '** WTFTIM '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WTFTIM '// version
		call fcecho(message)
	endif

c Add a history record to this effect
	message = ' Timing Info written by WTFTIM '// version
	call FTPHIS(ounit, message, status)
	message = wrnstr // ' Problem writing History record'
	call wt_ferrmsg(status,message)
	status = 0

c Lob in the reference
c	   call FTPKYS(ounit, 'TIMVERSN',
c     &		'OGIP/93-003',
c     &		'OGIP Memo number where convention used',
c     &		status)
c	   message = wrnstr // ' Putting TIMVERSN keyword'
c	   call wt_ferrmsg(status,message)
c	   status = 0

c On-time
	if(ontime.NE.0.0) then
	   call FTPKYF(ounit, 'ONTIME',
     &		ontime, decimals,
     &		'Total time on source (seconds)',
     &		status)
	    message = wrnstr // ' Putting ONTIME keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif

c Deadtime corrections
	if(.NOT.deadapp) then
		if(deadcorr.EQ.0.0) then
		  message = wrnstr//
     &			' No knowledge of deadtime'
		  call FTPCOM(ounit, message, status)
		  status = 0
		  goto 123
		endif
	endif
	   call FTPKYL(ounit, 'DEADAPP',
     &		deadapp,
     &		'Deadtime correction applied ?',
     &		status)
	    message = wrnstr // ' Putting DEADAPP keyword'
	    call wt_ferrmsg(status,message)
	    status = 0

	if(deadcorr.NE.0.0) then
	   call FTPKYF(ounit, 'DEADC',
     &		deadcorr, decimals,
     &		'Deadtime correction factor',
     &		status)
	    message = wrnstr // ' Putting DEADC keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif
	if(livetime.NE.0.0) then
	   call FTPKYF(ounit, 'LIVETIME',
     &		livetime, decimals,
     &		'Total time on source corrected for deadtime (seconds)',
     &		status)
	    message = wrnstr // ' Putting LIVETIME keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif

c Vignetting corrections
123	if(.NOT.vignapp) then
		if(vignet.EQ.0.0) then
		  message = wrnstr//
     &			' No knowledge of vignetting'
		  call FTPCOM(ounit, message, status)
		  status = 0
		  goto 456
		endif
	endif
	   call FTPKYL(ounit, 'VIGNAPP',
     &		vignapp,
     &		'Vignetting correction applied ?',
     &		status)
	    message = wrnstr // ' Putting VIGNAPP keyword'
	    call wt_ferrmsg(status,message)
	    status = 0

	if(vignet.NE.0.0) then
	   call FTPKYF(ounit, 'VIGNCORR',
     &		vignet, decimals,
     &		'Vignetting correction factor',
     &		status)
	    message = wrnstr // ' Putting VIGNCORR keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif

c The MJDREF date
456	if(mjdref.NE.0.0) then
	   call FTPKYD(ounit, 'MJDREF',
     &		mjdref, ddecimals,
     &		'Mission reference time (in MJD)',
     &		status)
	   message = wrnstr // ' Putting MJDREF keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	else
	  ierr = 1
	  goto 159
	endif

c Timing coord system and unit
	if(timesys.EQ.' ') timesys = 'UNKNOWN'
	   call FTPKYS(ounit, 'TIMESYS',
     &		timesys,
     &		'Time frame system in use',
     &		status)
	   message = wrnstr // ' Putting TIMESYS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(timeunit.EQ.' ') timeunit = 'UNKNOWN'
	   call FTPKYS(ounit, 'TIMEUNIT',
     &		timeunit,
     &		'Units for TSTART, TSTOP & TIMEZERO keywords',
     &		status)
	   message = wrnstr // ' Putting TIMEUNIT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c The start, stop & zero time
	if(tstart.NE.0.0) then
	   call FTPKYD(ounit, 'TSTART',
     &		tstart, ddecimals,
     &		'Start time in TIMESYS frame in TIMEUNIT units',
     &		status)
	   message = wrnstr // ' Putting TSTART keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	else
	  ierr = 1
	  goto 159
	endif
	if(tstop.NE.0.0) then
	   call FTPKYD(ounit, 'TSTOP',
     &		tstop, ddecimals,
     &		'Stop time in TIMESYS frame in TIMEUNIT units',
     &		status)
	   message = wrnstr // ' Putting TSTOP keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   call FTPKYF(ounit, 'TELAPSE',
     &		telapse, decimals,
     &		'Total elapsed time between TSTART & TSTOP in seconds',
     &		status)
	   message = wrnstr // ' Putting TELAPSE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	else
	  ierr = 1
	  goto 159
	endif

	   call FTPKYD(ounit, 'TIMEZERO',
     &		tzero, ddecimals,
     &		'Time zero off-set in TIMESYS frame & TIMEUNIT units',
     &		status)
	   if(status.NE.0) then
	    message = wrnstr // ' Putting TIMEZERO keyword'
	    call wt_ferrmsg(status,message)
	    ierr = 1
	    goto 159
	   endif

	   call FTPKYL(ounit, 'CLOCKAPP',
     &		clockapp,
     &		'Clock correction applied ?',
     &		status)
	    message = wrnstr // ' Putting CLOCKAPP keyword'
	    call wt_ferrmsg(status,message)
	    status = 0


c Barycentric correction reference frame
	if(timeref.EQ.' ') timeref = 'UNKNOWN'
	   call FTPKYS(ounit, 'TIMEREF',
     &		timeref,
     &		'Reference frame used for times',
     &		status)
	    message = wrnstr // ' Putting TIMEREF keyword'
	    call wt_ferrmsg(status,message)
	    status = 0

c Location of Time assignment
	if(tassign.NE.' ') then
	   call FTPKYS(ounit, 'TASSIGN',
     &		tassign,
     &		'Location where time assignment performed',
     &		status)
	    message = wrnstr // ' Putting TASSIGN keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
c GEOLAT, GEOLING & ALTITUDE keywords should be added
	endif

c Optional timing precision keyword
	if(tierrela.NE.0.0) then
	   call FTPKYF(ounit, 'TIERRELA',
     &		tierrela, decimals,
     &		'relative errors expressed as a rate',
     &		status)
	    message = wrnstr // ' Putting TIERRELA keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif
	if(tierabso.NE.0.0) then
	   call FTPKYF(ounit, 'TIERABSO',
     &		tierabso, decimals,
     &		'timing precision (seconds)',
     &		status)
	    message = wrnstr // ' Putting TIERABSO keyword'
	    call wt_ferrmsg(status,message)
	    status = 0
	endif




159	if(ierr.NE.0) then
	  message = wrnstr // ' Timing keyword outside nominal range'
	  call fcecho(message)
	  call FTPCOM(ounit, message, status)
	  message = ' ... aborting adding timing keywords'
	  call fcecho(message)
	  call FTPCOM(ounit, message, status)
	endif


	Return
	End
