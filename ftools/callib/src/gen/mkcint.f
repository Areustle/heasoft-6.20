*+MKCINT
	subroutine mkcint(configdir, chatter,status)

	implicit none
	character*(*) configdir
	integer status, chatter

C-----------------------------------------------------------------------
C Description: Creates a caldbinit file which contains the system 
C              specific commands required to set up the CALDB and 
C              CALDBCONFIG environment variables or logicals.
C
C              The caldbinit file is called caldbinit.<sys> where
C              <sys> is either 'unix' if running on a UNIX or ULTRIX
C              machine, or 'vms' if running on a VMS machine.
C              The caldbinit file is created in the subdirectory 
C              'software/tools'.  
C
C              This program assumes that the current directory is the 
C              top level directory of the Caldb and that the caldbconfig
C              file is located in the 'software/tools' directory and 
C              called 'caldb.config'
C
C Arguments:    chatter(i): chattiness flag
c		status (r): the success status of this routine.  Non-zero
C                          status values indicate failure to create
C                          a caldbinit file.
C
C Origin:      Written for the Calibration Database
C 
C Authors/Modification History:
C Ron Zellar     (1.0.0: Sep 20 1994) -- Original version
c  Ian M George  (2.0.0: 95 Dec 21) changed o/p to include wtinfo etc
c                                   and replaced verbose w/ chatter
        character(7) version
        parameter (version = '2.0.0')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'mkcint')
	character(4) ext
	character(160) cwd,cnfg,cint,line,drdmy1,drdmy2,drdmy3,contxt
	character(161) cwddmy
	integer errstat,unit,fcstln,cwdlen
	logical exist

C Initialize the internal and external status flags
	exist = .false.
	status = 0
	errstat = 0

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

C Append the subdirectory 'software/tools' to the dir component
	drdmy2 = drdmy2(:fcstln(drdmy2))//'/'//
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
           call wtinfo(chatter,15,3,contxt)
	endif

C If there's a '[' in the current directory, then this must be
C a VMS machine, so set the filename extension
	if (index(cwd,'[').ne.0) then
	     ext = 'vms'
	else 
	     ext = 'unix'
	endif

C Construct the system dependent path to the caldbinit file
	cint = 'caldbinit.'//ext(:fcstln(ext))
	call cpthnm(drdmy1,drdmy2,cint,errstat)
        if (errstat.ne.0) then
           call wterrm(subname, version,
     &          'problem constructing path to caldbinit file')
           call wtinfo(chatter,5,2, 'Components are as follows:')
           contxt = 'drdmy1 : '//drdmy1(:fcstln(drdmy1))
           call wtinfo(chatter,5,2,contxt)
           contxt = 'drdmy2 : '//drdmy2(:fcstln(drdmy2))
           call wtinfo(chatter,5,2,contxt)
           contxt = 'cint   : '//cint(:fcstln(cint))
           call wtinfo(chatter,5,2,contxt)
           status = 2
           go to 482
        else
           contxt = 'CINT set to: '//cint(:fcstln(cint))
           call wtinfo(chatter,15,3,contxt)
	endif

C Get a free logical unit number
	call cgetlun(unit)

C remove the caldbinit file, if it already exists
	call clobber(cint,errstat)
	errstat = 0

C Open the caldbinit file
	call faopnw(unit,cint,3,-1,errstat)
	if (errstat.ne.0) then
	       status = 3
               call wterrm(subname, version,
     &          'problem opening caldbinit file')
               contxt = 'offending file: '//cint(:fcstln(cint))
               call wtinfo(chatter,1,1, contxt)
               go to 482
	endif

C If we're on a VMS machine, rework the current working directory
C so that it ends in '.]'
	if (ext.eq.'vms') then
           call wtinfo(chatter,10,2,'setting up for VMS')
	   cwddmy = cwd
	   cwdlen = fcstln(cwddmy)
	   cwddmy(cwdlen:cwdlen+1)='.]'
C ........ Write the commands to set logicals CALDB and CALDBCONFIG
	   line='define/trans=conceal CALDB '//cwddmy(:fcstln(cwddmy))
	   write(unit,'(A)',iostat=errstat)line(:fcstln(line))
	   line='define/trans=conceal CALDBCONFIG '//cnfg(:fcstln(cnfg))
	   write(unit,'(A)',iostat=errstat)line(:fcstln(line))
	else
           call wtinfo(chatter,10,2,'setting up for unix/ultrix/osf')
C ........ Write the commands to set Env-vars CALDB and CALDBCONFIG
	   line='setenv CALDB '//cwd(:fcstln(cwd))
	   write(unit,'(A)',iostat=errstat)line(:fcstln(line))
	   line='setenv CALDBCONFIG '//cnfg(:fcstln(cnfg))
	   write(unit,'(A)',iostat=errstat)line(:fcstln(line))
	endif

c Perform a few checks on what we've just done
C ......Check to see whether CALDB directory exists
	if (ext.eq.'vms') then
	   call direx(cwddmy(:fcstln(cwddmy)),exist)
	   if(.not.exist) then
	      contxt = 'directory specified by the CALDB logical does '//
     &           'not currently exist'
	      call wtwarm(subname, version, chatter,1, contxt)
	   endif
	else
	   call direx(cwd(:fcstln(cwd)),exist)
	   if(.not.exist) then
	      contxt = 'directory specified by the CALDB '//
     &           'environment variable does not currently exist'
	      call wtwarm(subname, version, chatter,1, contxt)
	   endif
	endif
C ......Check to see whether CALDBCONFIG file exists
	inquire(file=cnfg(:fcstln(cnfg)),exist=exist)
	if(.not.exist) then
           contxt = 'file specified by the CALDB '//
     &           'env-var/logical does not currently exist'
           call wtwarm(subname, version, chatter,1, contxt)
	endif

C Report overall success to stderr
482     if (status .ne. 0) then
          call wterrm(subname, version, 'Caldbinit file NOT created')
        else
          contxt='Caldbinit file created successfully'
          call wtinfo(chatter,10,2,contxt)
        endif

C Close the caldbinit file
	close(unit)

C Free the logical unit number
	call cfrelun(unit)


	return
	end
