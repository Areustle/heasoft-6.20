*+MKCALB
	subroutine mkcalb

	implicit none

C-----------------------------------------------------------------------
C Description: Creates a calibration database.  The current working
C              directory is assumed to be the top level directory of the
C              Caldb, and the caldbconfig file is assumed to be in the
C              subdirectory 'software/tools'.  The environment variables
C              (or logicals) CALDB and CALDBCONFIG are set using these
C              assumptions.  After, the appropriate caldbinit file is 
C              generated along with the Caldb directories specified in 
C              the caldbconfig file.  Index files are generated in the
C              locations specified by the caldbconfig file.  Finally, 
C              any calibration files specified by the user are moved 
C              into the Caldb.
C
C Arguments:   NONE
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar    (1.0.0: Sep 28, 1994) Original Version
C  Ian M George  (1.1.0: 94 Dec 21) added wtinfo & friends
	character(7) version
        parameter (version = '1.1.0')
C-----------------------------------------------------------------------
*-
c Internals
	character(40) taskname
	integer errstat, chatter
	character(160) files, configdir
c Initialize
	taskname = 'mkcaldb'
	errstat = 0

c Go get the parameters
	call gpmcb(files,configdir,chatter,errstat)
        if(errstat.NE.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

c Just do it
	call mkcldb(files,configdir,chatter,errstat)
        if (errstat .ne. 0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version, errstat, chatter)

	return
	end

c -----------------------------------------------------------------
*+GPMCB
	subroutine gpmcb(files,configdir,chatter,status)

	implicit none
	character*(*) files, configdir
	integer status, chatter

C-----------------------------------------------------------------------
C Description: Gets the parameters from the mkcaldb par file
C
C Arguments:
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar    (1.1.0: Sep 28 1994) -- Original Version
c  Ian M George  (1.1.1: 95 Dec 21) changed o/p to include wtinfo etc
c                                   and replaced verbose w/ chatter 
        character(7) version
        parameter (version = '1.1.1')
C-----------------------------------------------------------------------
*-
c Internals
        character(5) subname
        parameter (subname = 'gpmcb')
	integer errstat
	character(160) contxt
c Initialize
	status = 0

c Get the files
	call uclgst('files',files,errstat)
        if(errstat.ne.0) then
          call wterrm(subname, version,
     &          'Problem getting FILES parameter')
          status = 1
          goto 999
        endif

c Get the configdir
	call uclgst('configdir',configdir,errstat)
        if(errstat.ne.0) then
          call wterrm(subname, version,
     &          'Problem getting CONFIGDIR parameter')
          status = 1
          goto 999
        endif

c Get the chattiness flag
        call uclgsi('chatter',chatter, errstat)
        if(errstat.NE.0) then
          call wtwarm(subname, version, 1, 1,
     &          'Problem getting CHATTER parameter')
                errstat = 0
                call wtinfo(1,1,1, 'setting CHATTER = 10')
                chatter = 10
        endif

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)


999     if(status.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

	return
	end
c -----------------------------------------------------------------
*+MKCLDB
	subroutine mkcldb(files, configdir, chatter, errstat)

	implicit none
	character*(*) files, configdir
	integer chatter, errstat

C-----------------------------------------------------------------------
C Description: Creates a calibration database.  The current working
C              directory is assumed to be the top level directory of the
C              Caldb, and the caldbconfig file is assumed to be in the
C              subdirectory 'software/tools'.  The environment variables
C              (or logicals) CALDB and CALDBCONFIG are set using these
C              assumptions.  After the appropriate caldbinit file is 
C              generated along with the Caldb directories and index 
C              files using the caldbconfig file, any calibration files 
C              specified by the user are moved into the Caldb.
C
C Arguments:   NONE
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar     (1.0.0: Sep 28, 1994) -- Original Version
c  Ian M George  (1.1.0: 95 Dec 21) changed o/p to include wtinfo etc
c                                   and replaced verbose w/ chatter
        character(7) version
        parameter (version = '1.1.0')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'mkcldb')
	integer fcstln,index,cwdlen, status, wrnstat
	character(160) cwd,drdmy1,drdmy2,drdmy3,cnfg,contxt
	character(161) cwddmy
	character(20) cnfgvar,calvar
	logical mkcifs, verbose

C Initialize
	cnfgvar = 'CALDBCONFIG'
	calvar = 'CALDB'
	verbose = .false.
	errstat = 0
	wrnstat = 0
	status = 0

C Set the 'make CIFs?' flag to true
	mkcifs = .true.

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

C Get the current directory
        call gtdir(cwd)

C Parse the current directory into disk, dir, and file components
        call ppthnm(cwd,drdmy1,drdmy2,drdmy3)

C Append the file argument to the directory argument.  
        if (fcstln(drdmy3).ne.0) then
             drdmy2 = drdmy2(:fcstln(drdmy2))//'/'//
     &       drdmy3(:fcstln(drdmy3))
             drdmy3 = ' '
        endif

C Append the configdir subdirectory to the dir component
        drdmy2 = drdmy2(:fcstln(drdmy2))//
     &		configdir(:fcstln(configdir))

C Construct the system dependent path to the caldbconfig file
        cnfg = 'caldb.config'
        call cpthnm(drdmy1,drdmy2,cnfg,errstat)
        if (errstat.ne.0) then
           call wterrm(subname, version,
     &          'problem constructing path to caldbconfig file')
           call wtinfo(chatter,5,2, 'Components are as follows:') 
	   contxt = 'drdmy1 : '//drdmy1(:fcstln(drdmy1))
           call wtinfo(chatter,5,2,contxt) 
	   contxt = 'drdmy2 : '//drdmy2(:fcstln(drdmy2))
           call wtinfo(chatter,5,2,contxt) 
	   contxt = 'cnfg   : '//cnfg(:fcstln(cnfg))
           call wtinfo(chatter,5,2,contxt) 
	   status = 1
           go to 482
	else
	   contxt = 'CALDBCONFIG set to: '//cnfg(:fcstln(cnfg))
           call wtinfo(chatter,2,2,contxt)        	   
        endif

C Set the CALDBCONFIG environment variable
	call stlog(cnfgvar,cnfg)

C If we're on a VMS machine, rework the current working directory
C so that it ends in '.]'
        cwddmy = cwd
        if (index(cwd,']').ne.0) then
             cwdlen = fcstln(cwddmy)
             cwddmy(cwdlen:cwdlen+1)='.]'
	endif
	contxt = 'CALDB set to: '//cwddmy(:fcstln(cwddmy))
        call wtinfo(chatter,2,2,contxt)        	   

C Set the CALDB environment variable
	call stlog(calvar,cwddmy)

C Give the user some info if they want it
	call wtinfo(chatter,2,1,'Creating the caldbinit file...')

C Create the caldbinit file
	call mkcint(configdir, chatter, errstat)
	if (errstat.ne.0) then
           call wtwarm(subname, version, chatter,1,
     &          'problem creating caldbinit file')
	   call wtinfo(chatter,1,1,
     &		'You may have to create a caldbinit file by hand')
	   call wtinfo(chatter,1,2,
     &		'continuing regardless')
	   wrnstat = 1
	endif

C More info for the user if they want it
	call wtinfo(chatter,2,1,'Creating the Caldb directories ...')

C Create the Caldb directories
	call mkcdir(chatter,mkcifs,errstat)
	if (errstat.ne.0) then
           call wtwarm(subname, version, chatter,1,
     &          'problem creating Caldb directories')
           call wtinfo(chatter,1,1,
     &          'Additional errors may occur during storage')
           call wtinfo(chatter,1,2,
     &          'continuing regardless')
	   wrnstat = 1
	endif

C See if there are any files which the user wants to store, and if so 
c store them
	if (fcstln(files).ne.0) then
	     call wtinfo(chatter,2,1,
     &		'Storing the calibration files ...')
	     if(chatter.GT.9) verbose = .true.	
	     call strcal(files,verbose,errstat)
	     if (errstat.ne.0) then
           	call wterrm(subname, version, 
     &               'problem storing calibration files')
	   	call wtinfo(chatter,1,1,
     &		   'You will have to correct cause of error by hand')
	   	call wtinfo(chatter,1,2,
     &		   'and store remaining files using stcal')
	        wrnstat = 1
		go to 482
	     endif
	endif

C Report overall success to stderr
482     if (status .ne. 0) then
          call wterrm(subname, version, ' Fatal - aborting')
        elseif(wrnstat.NE.0) then
	  contxt = 'Partial execution'
	  status = 0
          call wtinfo(chatter,1,1,contxt)
        endif

	return
	end
