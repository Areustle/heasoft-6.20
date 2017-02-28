*+ WTTOBS
	subroutine wttobs(chatter, ounit,
     &  date_obs, time_obs, date_end, time_end,
     &	mjdobs,ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	double precision mjdobs
	character*(*) date_obs, time_obs, date_end, time_end
c
c Description
c   Writes the details of the time of the observation to the current
c  FITS header unit
c  The following keywords can be written (assuming valid values)
c   DATE-OBS -'Nominal UT date of obs start (yyyy-mm-dd)'
c   TIME-OBS -'Nominal UT time of obs start (hh:mm:ss)'
c   DATE-END -'Nominal UT date of obs end (yyyy-mm-dd)'
c   TIME-END -'Nominal UT time of obs end (hh:mm:ss)'
c
c Passed Parameters
c CHATTER      i   : chattiness flag for o/p (5 quite, 10 norm, >19 silly)
c OUNIT        i   : FORTRAN unit number of open File
c DATE_OBS     i   : Date of observation start (yyyy-mm-dd)
c TIME_OBS     i   : Time of observation start (hh:mm:ss) on day DATA_OBS
c DATE_END     i   : Date of observation end (yyyy-mm-dd)
c TIME_END     i   : Time of observation end (hh:mm:ss) on day DATA_END
c MJDOBS       i   : Mean MJD of observation
c IERR           o : Error flag on return (zero = OK)
c
c Called Routines
c subroutine FTPHIS	  : (FITSIO) writes a history keyword
c subroutine FTPKYm	  : (FITSIO) writes key, values,comment
c subroutine FCECHO	  : (FTOOLS) writes to standard o/p
c subroutine WT_FERRMSG   : (CALLIB) writes standard fitsio error message
c
c Origin
c   An IMG original
c
c Authors/Modification History
c  Ian M George    (1.0.1: 1993 Jun 20), first 'proper' version
c  Jeff Guerber    (1.0.2: 1998-07-07), make args char*(*) instead of *8
c                    and fix keyword comments, for new date format
c
	character(7) version
	parameter (version = '1.0.2')
*-

c Internals
	integer status, ddecimals
	parameter (ddecimals = 12)
	character(40) errstr, wrnstr
	character(80) message

c Initialize
	errstr = '** WTTOBS '//version//' ERROR: '
	wrnstr = '** WTTOBS '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WTTOBS '// version
		call fcecho(message)
	endif

c Add a history record to this effect
	message = ' Observation Date/Time Info written by WTTOBS '
     &			// version
	call FTPHIS(ounit, message, status)
	message = wrnstr // ' Problem writing History record'
	call wt_ferrmsg(status,message)
	status = 0

	if(date_obs.NE.' ') then
	   call FTPKYS(ounit, 'DATE-OBS',
     &		date_obs,
     &		'Nominal UT date of obs start (yyyy-mm-dd)',
     &		status)
	   message = wrnstr // ' Putting DATE-OBS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	else
	   call FTPKYS(ounit, 'DATE-OBS',
     &		date_obs,
     &		'Nominal UT date of obs start (UNKNOWN)',
     &		status)
	   status = 0
	endif


	if(time_obs.NE.' ') then
	   call FTPKYS(ounit, 'TIME-OBS',
     &		time_obs,
     &		'Nominal UT time of obs start (hh:mm:ss)',
     &		status)
	   message = wrnstr // ' Putting TIME-OBS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(date_end.NE.' ') then
	   call FTPKYS(ounit, 'DATE-END',
     &		date_end,
     &		'Nominal UT date of obs end (yyyy-mm-dd)',
     &		status)
	   message = wrnstr // ' Putting DATE-END keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	else
	   call FTPKYS(ounit, 'DATE-END',
     &		date_end,
     &		'Nominal UT date of obs end (UNKNOWN)',
     &		status)
	   status = 0
	endif

	if(time_end.NE.' ') then
	   call FTPKYS(ounit, 'TIME-END',
     &		time_end,
     &		'Nominal UT time of obs end (hh:mm:ss)',
     &		status)
	   message = wrnstr // ' Putting TIME-END keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(mjdobs.NE.0.0) then
	   call FTPKYD(ounit, 'MJD-OBS',
     &		mjdobs, ddecimals,
     &		'Mean MJD of observation',
     &		status)
	   message = wrnstr // ' Putting MJD-OBS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif


	Return
	End
