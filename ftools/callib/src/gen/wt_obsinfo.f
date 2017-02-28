*+ WT_OBSINFO
	subroutine wt_obsinfo(chatter, ounit, 
     &  object, obs_id, obs_pi, seqno, observer,
     &	proc_sys, proc_ver,proc_loc, proc_date, proc_time,
     &	ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	character(70) object, obs_id, obs_pi, seqno, observer
	character(70) proc_sys, proc_ver, proc_loc, proc_date
	character(70) proc_time
c
c Description
c   Writes the Observation details to the current FITS header unit
c The following keywords can be written (assuming valid value)
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
c  OUNIT        i   : FORTRAN logical unit number of o/p file
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
c   An IMG original
c
c Authors/Modification History
c  Ian M George    (1.0.01:1993 Jun 10), first 'proper' version
c  Ian M George    (1.0.2:1993 Jun 20), times & celestial coords taken out
c  Ian M George    (1.1.0:1993 Aug 16), add proc_sys,_ver,_loc etc etc
c  Ian M George    (1.1.1:1993 Aug 27), added OBJECT key clnstr
c
	character(7) version
	parameter (version = '1.1.1')
*-

c Internals
	integer status, decimals, i
        integer nbad, ibad(80)
 	parameter (decimals = 6)
	character(30) errstr, wrnstr
	character(70) clean
	character(80) message

c Initialize	
	errstr = '** WT_OBSINFO ERROR: '
	wrnstr = '** WT_OBSINFO WARNING: '
	nbad = 0
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WT_OBSINFO '// version
		call fcecho(message)
	endif

c Add a history record to this effect
	message = ' Observation Info written by WT_OBSINFO '// version
	call FTPHIS(ounit, message, status)
	message = wrnstr // ' Problem writing History record'
	call wt_ferrmsg(status,message)
	status = 0

	if(object.EQ.' ') then
	   object = 'UNKNOWN'
	else
	   call clnstr(chatter, object(:70), 70, 
     &			clean, i, nbad, ibad, status)
 	   if(nbad.NE.0) then
		write(message,'(a,i2,a)')  wrnstr, nbad,
     &		' illegal characters removed from OBJECT name:'
		call fcecho(message)
		message = '     "'//clean(:i)//'"'
		call fcecho(message)
		message = 
     &		' ... this cleaned string will be written & returned'
		call fcecho(message)
	  	object = clean
	    endif
	endif
	   call FTPKYS(ounit, 'OBJECT', 
     &		object, 
     &		'Object name',
     &		status)
	   message = wrnstr // ' Problem writing OBJECT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(obs_id.NE.' ') then
	   call FTPKYS(ounit, 'OBS_ID', 
     &		obs_id, 
     &		'Observation ID code',
     &		status)
	   message = wrnstr // ' Problem writing OBS_ID keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(obs_pi.NE.' ') then
	   call FTPKYS(ounit, 'OBS_PI', 
     &		obs_pi, 
     &		'Principal Investigator of observation',
     &		status)
	   message = wrnstr // ' Problem writing OBS_PI keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(seqno.NE.' ') then
	   call FTPKYS(ounit, 'SEQNO', 
     &		seqno, 
     &		'Sequence Number of Observation',
     &		status)
	   message = wrnstr // ' Problem writing SEQNO keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(observer.NE.' ') then
	   call FTPKYS(ounit, 'OBSERVER', 
     &		observer, 
     &		'Name or ref code for Observer/Duty Scientist',
     &		status)
	   message = wrnstr // ' Problem writing OBSERVER keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(proc_sys.NE.' ') then
	   call FTPKYS(ounit, 'PROC_SYS', 
     &		proc_sys, 
     &		'Science Data Processing System used',
     &		status)
	   message = wrnstr // ' Problem writing PROC_SYS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(proc_ver.NE.' ') then
	   call FTPKYS(ounit, 'PROC_VER', 
     &		proc_ver, 
     &		'Revision level of Processing System',
     &		status)
	   message = wrnstr // ' Problem writing PROC_VER keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(proc_loc.NE.' ') then
	   call FTPKYS(ounit, 'PROC_LOC', 
     &		proc_loc, 
     &		'Location at which Processing System run',
     &		status)
	   message = wrnstr // ' Problem writing PROC_LOC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(proc_date.NE.' ') then
	   call FTPKYS(ounit, 'PROC_DAT', 
     &		proc_date, 
     &		'Date on which Processing System run',
     &		status)
	   message = wrnstr // ' Problem writing PROC_DAT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(proc_time.NE.' ') then
	   call FTPKYS(ounit, 'PROC_TIM', 
     &		proc_time, 
     &		'Time at which Proc System run (on PROC_DAT)',
     &		status)
	   message = wrnstr // ' Problem writing PROC_TIM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	Return
	End
