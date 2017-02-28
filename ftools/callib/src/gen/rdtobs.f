*+ RDTOBS
	subroutine rdtobs(chatter, iunit, 
     &  date_obs, time_obs, date_end, time_end,
     &	mjdobs,ierr)

	IMPLICIT NONE
	integer chatter, iunit, ierr
	double precision mjdobs
	character*(*) date_obs, time_obs, date_end, time_end
c
c Description
c   Reads the details of the time of the observation from the current 
c  FITS header unit
c  The following keywords can be read 
c   DATE-OBS -'Nominal UT date of obs start (dd/mm/yy)'
c   TIME-OBS -'Nominal UT time of obs start (hh:mm:ss)'
c   DATE-END -'Nominal UT date of obs end (dd/mm/yy)'
c   TIME-END -'Nominal UT time of obs end (hh:mm:ss)'
c 
c Passed Parameters
c CHATTER      i   : chattiness flag for o/p (5 quite, 10 norm, >19 silly)
c iunit        i   : FORTRAN unit number of open File
c DATE_OBS     i   : Date of observation start (dd/mm/yy)
c TIME_OBS     i   : Time of observation start (hh:mm:ss) on day DATA_OBS
c DATE_END     i   : Date of observation end(dd/mm/yy)
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
c   An IMG original, based upon wttobs (v1.0.1)
c
c Authors/Modification History
c  Ian M George    (1.0.1: 1994 Feb 02), original
c
	character(7) version
	parameter (version = '1.0.0')
*-

c Internals
	integer status
	character(40) errstr, wrnstr
	character(70) comm
	character(80) message

c Initialize	
	errstr = '** RDTOBS '//version//' ERROR: '
	wrnstr = '** RDTOBS '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using RDTOBS '// version
		call fcecho(message)
	endif

	   call FTGKYS(iunit, 'DATE-OBS', 
     &		date_obs, 
     &		comm,
     &		status)
	if(status.NE.0) then
	   date_obs = ' '
	   status = 0
	endif

	   call FTGKYS(iunit, 'TIME-OBS', 
     &		time_obs, 
     &		comm,
     &		status)
	if(status.NE.0) then
	   time_obs = ' '
	   status = 0
	endif

	   call FTGKYS(iunit, 'DATE-END', 
     &		date_end, 
     &		comm,
     &		status)
	if(status.NE.0) then
	   date_end = ' '
	   status = 0
	endif

	   call FTGKYS(iunit, 'TIME-END', 
     &		time_end, 
     &		comm,
     &		status)
	if(status.NE.0) then
	   time_end = ' '
	   status = 0
	endif

	   call FTGKYD(iunit, 'MJD-OBS', 
     &		mjdobs, 
     &		comm,
     &		status)
	if(status.NE.0) then
	   mjdobs = 0.0
	   status = 0
	endif

	Return
	End
