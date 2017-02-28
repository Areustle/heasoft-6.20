*+RCNFIG
	subroutine rcnfig(mission,instr,chatter,cif,instdir,status)

	implicit none
	character*(*) mission,instr,cif,instdir
	integer status, chatter

C-----------------------------------------------------------------------
C Description: Opens and reads the file specified by the CALDBCONFIG
C              environment variable (UNIX systems) or logical
C              (VMS systems), matches the line pertaining to the mission
C              and instr argument values, and returns the complete
C              system dependent path to the Calibration Index File valid
C              for the specified mission and instrument, along with the
C              top level directory under which all calibration files 
C              reside for that mission and instrument.
C
C              For example, if mission = 'ASCA' and instr = 'GIS2' then
C              then the returned cif value might be something like
C              '/caldb/data/asca/gis/caldb.indx' on UNIX systems while
C              it would be 'CALDB:[DATA.ASCA.GIS]CALDB.INDX' on VMS 
C              systems.  The returned instdir values would be
C              '/caldb/data/asca/gis' and 'CALDB:[DATA.ASCA.GIS]'
C              respectively.  
C
C              A non-zero value will be returned to status should no
C              entry be found in the CALDBCONFIG file which matches the
C              mission and instr argument values, or if an error 
C              occurred during the execution of this routine.
C
C              The format of the file being read is as follows:
C              Lines beginning with '#' are ignored as are blank lines.
C              Lines without '#' characters at the beginning should have 
C              7 tokens.  They are mission, instrument, cifdev, cifdir, 
C              cif insdev, insdir, where 
C
C              mission is the name of the mission (in uppercase)
C              instrument is the name of the instrument (in uppercase)
C              cifdev is the name of the environment variable which
C                   points to the top directory of the Caldb
C              cifdir is the directory path beneath cifdev in which the
C                   Calibration Index File resides
C              cif is the name of the Calibration Index File which is 
C                   valid for the mission and instrument
C              insdev is the name of the environment variable which
C                   points to the top of the Caldb
C              insdir is the directory path beneath insdev in which 
C                   all files for the mission and instrument reside
C
C              Each token is separated by one or more white spaces.
C
C              When an entry is found which matches the mission and 
C              instr arguments, the cifdev, cifdir, and cif columns are
C              combined to form the system dependent path to the 
C              Calibration Index File.  The insdev, and insdir columns
C              are combined to form the system dependent path which
C              points at the top level directory in which all files
C              for the mission and instrument reside.
C
C Arguments:   mission  (i): The name of the mission for which you need
C                            a Calibration Index File
C              instr    (i): The name of the instrument for which you 
C                            need a Calibration Index File
C              chatter  (i): Chattiness flag
C              cif      (r): The system dependent path to the 
C                            Calibration Index File
C              instdir  (r): The system dependent path to the top level
C                            directory where all calibration files 
C                            reside for the mission and instrument
C              status   (r): the success status of this routine
C
C Origin:      Based upon (and replaces) Ron Zellar's rdcnfg routine
C
C Authors/Modification History:
C  Ron Zellar    (1.0.0: 94 Aug 08) -- Original version of rdcnfg
c  Ian M George  (2.0.0: 95 Dec 21) changed o/p to include wtinfo etc
c                                   and replaced quiet w/ chatter
c  Ian M George  (2.0.1: 96 Jun 03) Tweaks to internal chatter settings
        character(7) version
        parameter (version = '2.0.1')
*-
C-----------------------------------------------------------------------
c Internals
        character(6) subname
        parameter (subname = 'rcnfig')
	integer linenum
	character(4) cval
	character(20) envar
	character(512) contxt
	character(160) config,misval,insval,cifdev,cifdir,insdev,insdir
	integer fcstln,envlen,configlen,unit,errstat

C Initialize the internal and external status flags
	status = 0
	errstat = 0
	linenum = 0

C Set the name of the environment variable used in this routine
	envar = 'CALDBCONFIG'
	instdir = ' '
	
c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,25,1,contxt)

C Convert arguments to uppercase
	call ftupch(mission)
	call ftupch(instr)

C Translate the caldbconfig environment variable
	envlen = fcstln(envar)
	call ctrlog(envar,envlen,config,configlen)

C Make sure that the environment variable was defined
	if (configlen .eq. 0) then
           call wterrm(subname, version,
     &          'CALDBCONFIG environ-var/logical not set')
           contxt = 'The env-var/logical '//
     &		'CALDBCONFIG must be set to point your local Caldb '//
     &          'configuration file'
           call wtinfo(chatter,5,1, contxt)
           contxt = 'See the Caldb Users Guide (CAL/GEN/94-002)'//
     &          ' for details'
           call wtinfo(chatter,5,2,contxt)
           status = 10
           goto 999
	endif

C Get a free logical unit number
	call cgetlun(unit)

C Open the caldbconfig file
	call ocnfg(unit,chatter,config,errstat)
        if (errstat.ne.0) then
             status = 20
             call wterrm(subname, version,
     &          'problem opening caldbconfig file')
             goto 999
        else
             call wtinfo(chatter,25,3,'Opened the caldbconfig file')
        endif

C Read the file 
100	continue
	call rcnfgl(unit,chatter,linenum,misval,insval,cifdev,cifdir,
     &	cif,insdev,insdir,errstat)
	if ((errstat.ne.0).and.(errstat.ne.-1)) then
	     status = 30
             call wterrm(subname, version,
     &          'problem reading caldbconfig file')
	     write(cval,'(I4)')linenum
	     contxt='offending line: '//cval
	     call wtinfo(chatter,1,1,contxt)
	     call ccnfg(unit)
	     call cfrelun(unit)
	     goto 999
	elseif(errstat.eq.-1) then
c .......... (end of file)
	     errstat = 0
	     goto 1000
	else
	     contxt = 'read '//misval(:fcstln(misval))//' '//
     &		insval(:fcstln(insval))
	     call wtinfo(chatter,25,3,contxt)
	endif

C compare mission and instrument values
	if (misval .eq. mission) then
	     if (insval .eq. instr) then
	          call cpthnm(cifdev,cifdir,cif,errstat)
		  if (errstat.ne.0) then
	     		status = 40
			contxt = 'problem constructing '//
     &			   'cif arg from cpthnm'
             		call wterrm(subname, version, contxt)
	     		write(cval,'(I4)')linenum
	     		contxt='offending line: '//cval
	     		call wtinfo(chatter,1,2,contxt)
	                call ccnfg(unit)
		        goto 999
		  endif
	          call cpthnm(insdev,insdir,instdir,errstat)
		  if (errstat.ne.0) then
	     		status = 50
			contxt = 'problem constructing '//
     &			   'instdir arg from cpthnm'
             		call wterrm(subname, version, contxt)
	     		write(cval,'(I4)')linenum
	     		contxt='offending line: '//cval
	     		call wtinfo(chatter,1,2,contxt)
	                call ccnfg(unit)
		        goto 999
		  endif
	          contxt = 'found the entry for '//
     &		    mission(:fcstln(mission))//' '//
     &		    instr(:fcstln(instr))//' '//
     &		    'within the caldbconfig file'
	          call wtinfo(chatter,25,3,contxt)
		  status = 0
		  goto 999
	     endif
	endif

	goto 100

1000	continue

C If I got to here then I couldn't find a valid entry
	contxt='Unable to find a valid entry for the '//
     &		mission(:fcstln(mission))//' '//
     &          instr(:fcstln(instr))//' '//
     & 		'in the Caldb configuration file '//
     &		config(:fcstln(config))
	call wterrm(subname, version, contxt)
	status = 50


999     if(status.ne.0) then
           call wterrm(subname, version, 'Aborting')
	else
           call wtinfo(chatter,25,3,'Closing the caldbconfig file')
        endif


	call ccnfg(unit)
	call cfrelun(unit)

        return
        end
C-----------------------------------------------------------------------

