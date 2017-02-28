*+ RD_OBSINFO
	subroutine rd_obsinfo(chatter, iunit, 
     &  object, obs_id, obs_pi, seqno, observer,
     &	proc_sys, proc_ver,proc_loc, proc_date, proc_time,
     &	ierr)

	IMPLICIT NONE
	integer chatter, iunit, ierr
	character*(*) object, obs_id, obs_pi, seqno, observer
	character*(*) proc_sys, proc_ver, proc_loc, proc_date
	character*(*) proc_time
c
c Description
c   Reads the Observation details to the current FITS header unit
c The following keywords are read 
c	(assuming found; blank strings returned otherwise)
c  OBJECT  - 'Object name'
c  OBS_ID  - 'Observation ID code'
c  OBS_PI  - 'Principal Investigator of observation'
c  SEQNO   - 'Sequence Number of Observation'
c  OBSERVER- 'Name or ref code for Observer/Duty Scientist'
c  PROC_SYS- 'Science Data Processing System used'
c  PROC_VER- 'Revision level of Processing System',
c  PROC_LOC- 'Location at which Processing System run'
c  PROC_DAT- 'Date on which Processing System run'
c  PROC_TIM- 'Time at which Processing System run (on PROC_DAT)'
c 
c Passed Parameters
c  CHATTER	i   : Chattiness flag (<5=quite,10=normal,>20=silly)
c  IUNIT        i   : FORTRAN logical unit number of i/p file
c  OBJECT       i   : (Astronomical) Target/object name
c  OBS_ID	i   : Observation ID
c  OBS_PI       i   : Name of Principal Investigator of Observation
c  SEQNO        i   : (Scheduling) sequence number of observation
c  OBSERVER     i   : Name or ref code for Observer/Duty Scientist
c  PROC_SYS     i   : Name of data processing system 
c  PROC_VER     i   : Revision level of Processing System
c  PROC_LOC     i   : Location at which Processing System run
c  PROC_DAT     i   : Date on which Processing System run
c  PROC_TIM     i   : Time at which Processing System run (on PROC_DAT)
c  IERR           o : Return Error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO		: (FTOOLS) writes to standard o/p
c  subroutine FTPHIS		: (FITSIO) writes a history keyword
c  subroutine FTPKYn		: (FITSIO) writes a keyword of type n
c  subroutine WT_FERRMSG	: (CALLIB) writes FITSIO error message etc
c
c Origin
c   An IMG original, based on wt_obsinfo.f (v1.1.1)
c
c Authors/Modification History
c  Ian M George    (1.0.0:1994 Feb 02), original
c
	character(7) version
	parameter (version = '1.0.0')
*-

c Internals
	integer status
	character(30) errstr, wrnstr
	character(70) comm
	character(80) message

c Initialize	
	errstr = '** RD_OBSINFO ERROR: '
	wrnstr = '** RD_OBSINFO WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using RD_OBSINFO '// version
		call fcecho(message)
	endif

	   call FTGKYS(iunit, 'OBJECT', 
     &		object, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  object = ' '
	  status = 0 
	endif


	   call FTGKYS(iunit, 'OBS_ID', 
     &		obs_id, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  obs_id = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'OBS_PI', 
     &		obs_pi, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  obs_pi = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'SEQNO', 
     &		seqno, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  seqno = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'OBSERVER', 
     &		observer, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  observer = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'PROC_SYS', 
     &		proc_sys, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  proc_sys = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'PROC_VER', 
     &		proc_ver, 
     &		comm,
     &		status)
	if(status.NE.0) then
	  proc_ver = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'PROC_LOC', 
     &		proc_loc, 
     &		'Location at which Processing System run',
     &		status)
	if(status.NE.0) then
	  proc_loc = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'PROC_DAT', 
     &		proc_date, 
     &		'Date on which Processing System run',
     &		status)
	if(status.NE.0) then
	  proc_date = ' '
	  status = 0 
	endif

	   call FTGKYS(iunit, 'PROC_TIM', 
     &		proc_time, 
     &		'Time at which Proc System run (on PROC_DAT)',
     &		status)
	if(status.NE.0) then
	  proc_time = ' '
	  status = 0 
	endif

	Return
	End
