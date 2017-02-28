	subroutine mkcdir(chatter,mkcifs,status)

	implicit none
	logical mkcifs
	integer status,chatter

C-----------------------------------------------------------------------
C Description: Reads the file indicated by the value of the CALDBCONFIG
C              environment variable or logical and creates the 
C              instrument directories found there.  In addition the 
C              subdirectories bcf and cpf are created beneath the 
C              instrument directories.  If the mkcifs argument is true
C              then the Caldb index files are also generated from the
C              specifications in the caldbconfig file.
C
C Arguments:   chatter  (i): Whether or not the task should tell the
C                            user what it is doing
C              mkcifs   (i): Whether or not the task should create the
C                            Caldb index files
C              status   (r): The success status of this routine.  
C                            Non-zero values indicate failure to create
C                            one or more Caldb directories.
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar  (1.0.0: Sep 13 1994) -- Original Version
C Ron Zellar  (2.0.0: Sep 28 1994) -- Added mkcifs argument and feature
C Ron Zellar  (2.0.1: Oct 13 1994) -- Added errstat reinitialization at top of loops
c Ian M George(3.0.0: 95 Dec 21) changed o/p to include wtinfo etc
c                                   and replaced verbose w/ chatter
c Ian M George(3.0.1: 95 Dec 29) cosmetics
        character(7) version
        parameter (version = '3.0.1')
C
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'mkcdir')
	integer errstat,unit,linenum,fcstln,i
	character(4) cval
	character(3) subdir(2)
	character(160) misval,insval,cifdev,cifdir,cif
	character(160) insdev,insdir,insdmy,instdir,contxt,config
	logical exist
C Initialize variables
	status = 0
	errstat = 0
	linenum = 0
	subdir(1) = 'bcf'
	subdir(2) = 'cpf'

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)


C Get a free logical unit number
	call cgetlun(unit)

C Open the CALDBCONFIG file
	call ocnfg(unit,chatter,config,errstat)
	if (errstat.ne.0) then
	     status = 1
             call wterrm(subname, version,
     &          'problem opening caldbconfig file')
	     goto 482
	else 
	     call wtinfo(chatter,25,3,'Opened the caldbconfig file')
	endif

C ----- Top of the CALDBCONFIG reading loop ------
100	continue

	errstat = 0
C Read the next line from the config file
	call rcnfgl(unit,chatter,linenum,misval,insval,cifdev,cifdir,
     &	cif,insdev,insdir,errstat)
	if ((errstat.ne.0).and.(errstat.ne.-1)) then
	     status = 2
             call wterrm(subname, version,
     &          'problem reading caldbconfig file')
	     write(cval,'(I4)')linenum
	     contxt='offending line: '//cval
	     call wtinfo(chatter,1,1,contxt)
	     goto 482
	elseif(errstat.eq.-1) then
c .......... (end of file)
	     errstat = 0
	     goto 1000
	else
	     contxt = 'doing '//misval(:fcstln(misval))//' '//
     &		insval(:fcstln(insval))
	     call wtinfo(chatter,15,3,contxt)
	endif

C For each instrument directory, create a bcf and cpf subdirectory
	do 200 i=1,2
		errstat = 0
C .............	Append the subdirectory name to the instrument directory
		insdmy = insdir(:fcstln(insdir))//'/'//subdir(i)
C..............	Construct the system dependent directory path
		instdir = ' '
		call cpthnm(insdev,insdmy,instdir,errstat)
		if (errstat.ne.0) then
	     		status = 3
			contxt = 'problem constructing '//
     &			   'pathname from tokens caldbconfig file'
             		call wterrm(subname, version, contxt)
	     		write(cval,'(I4)')linenum
	     		contxt='offending line: '//cval
	     		call wtinfo(chatter,1,2,contxt)
		endif

C .............. If the directory already exists, don't bother to create it
		call direx(instdir,exist)
		if (exist) then
	     		contxt='Will reuse existing directory:   '//
     &				instdir(:fcstln(instdir))
			call wtinfo(chatter,10,1,contxt)
			goto 200
		else
	     		contxt='Will try to create new directory: '//
     &				instdir(:fcstln(instdir))
			call wtinfo(chatter,10,1,contxt)
		endif

C .............. Create the directory
		call makdir(instdir)

C .............. See if the directory was created successfully
		call direx(instdir,exist)
		if (.not.exist) then
	     		status = 4
			contxt = 'Unable to create the directory: '//
     &				instdir(:fcstln(instdir))
             		call wterrm(subname, version, contxt)
			goto 482
		endif

C ----	Bottom of the loop over the subdirs
200	continue

C If needed create the Caldb index files
	if (mkcifs) then
	     call cpthnm(cifdev,cifdir,cif,errstat)
             if (errstat.ne.0) then
                  status = 5
		  contxt = 'problem constructing CIF path '//
     &				'from tokens caldbconfig file'
             		call wterrm(subname, version, contxt)
	     		write(cval,'(I4)')linenum
	     		contxt='offending line: '//cval
	     		call wtinfo(chatter,1,2,contxt)
	     endif
C	     If the Index file already exists, don't bother to create
C            it and skip on to the next config entry
	     inquire(file=cif,exist=exist)
	     if (exist) then
	        contxt='Pre-existing CIF: '//cif(:fcstln(cif))//
     &		  ' not overwritten'
	        call wtinfo(chatter,10,1,contxt)
		goto 100
	     else
	       contxt='Creating the CIF: '//cif(:fcstln(cif))
	       call wtinfo(chatter,10,1,contxt)
	     endif

	     call crtcif(chatter, cif, errstat)
	     if (errstat.ne.0) then
	          status = 6
		  contxt = 'Unable to create CIF: '//
     &				cif(:fcstln(cif))
             	  call wterrm(subname, version, contxt)
		  goto 482
             endif
	endif

C	Bottom of the config reading loop
	goto 100

C	This is what happens at the end of the config file
1000	continue

C Report overall success to stderr
482     if (status .ne. 0) then
          call wterrm(subname, version, 
     &		'Caldb directory structure NOT created')
        else
          contxt='Caldb directory structure created successfully'
          call wtinfo(chatter,10,2,contxt)
        endif
 


C	Close the config file
	close(unit)

C	Free the logical unit number I was using
	call cfrelun(unit)

	return
	end
