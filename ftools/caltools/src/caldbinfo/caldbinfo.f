*+CALDBINFO
      subroutine caldbo()
      implicit none

c Description: 
c  Checks out and reports upon the status of the CALDB available to 
c a user, with the level of checking & information controlled by the 
c mode & chatter parameters.
c
c Passed parameters:
c  None
c
c Origin: 
c  Written for the Calibration Database, based largely upon pre-existing 
c CALLIB subroutines
c
c Authors/Modification History:
c  Ian M George (1.0.0: 95 Dec 13), original version
c  Ian M George (1.0.1: 96 Feb 03), deleted unused variable
	character(7) version
	parameter (version = '1.0.2')
*-
c Internals
	character(40) taskname
        character(10)  missn, instr, mode
	integer ierr, chatter
	character(20)  cnfgvar, caldbvar
c Initialize
	taskname = 'caldbinfo'
	ierr = 0
	cnfgvar = 'CALDBCONFIG'
	caldbvar = 'CALDB'

c Get Parameters from the par file
	call gpcaldbinfo(mode,chatter,missn,instr,ierr)
	if(ierr.NE.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

c Do the job 
	call infocaldb(mode,chatter,missn,instr,ierr)
	if(ierr.NE.0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version,ierr,chatter)

	return
	end

C-----------------------------------------------------------------------
*+GPCALDBINFO
	subroutine gpcaldbinfo(mode,chatter,missn,instr,ierr)

	implicit none
        character*(*) missn,instr
        character*(*) mode
	integer chatter, ierr

c Description: 
c  Gets the parameters required by the CALTOOLS task CALDBINFO
c
c Passed parameters:
c  mode              r : The mode os invetigation
c  chatter           r : Chattiness flag
c  missn             r : Mission string    
c  instr             r : Instrument string
c  ierr		     r : Error flag (zero if all OK)
c
c Origin: 
c  Written for the Calibration Database, based largely upon pre-existing 
c CALLIB subroutines
c
c Authors/Modification History:
c  Ian M George (1.0.0: 95 Dec 13), original version
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals
        character(11) subname
        parameter (subname = 'gpcaldbinfo')
        character(50) contxt
c Initialize
        ierr = 0

C Get mode parameter
        call uclgst('infomode', mode, ierr)
        if(ierr.ne.0) then
          call wterrm(subname, version,
     &          'Problem getting MODE parameter')
                ierr = 0
                call wtinfo(1,1,1, 'setting MODE = BASIC')
                mode = 'BASIC'
        else
           call ftupch(mode)
        endif
	

C Get mission and instrument parameters if requested
	if (mode(1:4).eq.'INST') then
             call uclgst('mission', missn, ierr)
        	if(ierr.ne.0) then
          		call wterrm(subname, version,
     &    	      		'Problem getting MISSION parameter')
          		goto 999
        	endif
             call uclgst('instrument', instr, ierr)
        	if(ierr.ne.0) then
          		call wterrm(subname, version,
     &    	      		'Problem getting INSTRUMENT parameter')
          		goto 999
        	endif
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

999	if(ierr.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif   

        return
        end

C---------------------------------------------------------------------
*+INFOCALDB
        subroutine infocaldb(mode,chatter,missn,instr,ierr)

        implicit none
        character*(*) missn,instr
        character*(*) mode
	integer chatter, ierr


c Description: 
c  Checks out and reports upon the status of the CALDB available to 
c a user, with the level of checking & information controlled by the 
c mode & chatter parameters.
c
c Passed parameters:
c  mode              r : The mode os invetigation
c  chatter           r : Chattiness flag
c  missn             r : Mission string    
c  instr             r : Instrument string
c  ierr		     r : Error flag (zero if all OK)
c
c Origin: 
c  Written for the Calibration Database, based largely upon pre-existing 
c CALLIB subroutines
c
c Authors/Modification History:
c  Ian M George (1.0.0: 95 Dec 13), original version
c  M. F. Corcoran (1.0.1: 04 Dec 14) convert missn, instr to upper case 
c                 before passing to caldb_info to solve swift xrt problem
c
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
        character(11) subname
	character(160) message
        parameter (subname = 'infocaldb')

c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)
	 
C Convert missn, instr into uppercase
	call ftupch(missn)
	call ftupch(instr)


c Check that the CALDB is defined & available to the user
	call caldb_info(chatter, mode, missn, instr, ierr)
	if(ierr.NE.0) then
	   message = 'CALDB not defined/available'
	   call wterrm(subname, version,message)
	   message = 'Task requires CALDB to be both defined '//
     &		'& available in order to run'
	   call wtinfo(chatter,1,1, message)
           goto 148
	endif

148     if(ierr.ne.0) then
           call wterrm(subname, version,'Aborting')
	else
	   message = 'Local CALDB appears to be set-up & accessible'
	   call wtinfo(chatter,1,1,message)
	endif

        return
        end



