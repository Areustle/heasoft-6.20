
*+STRCAL
	subroutine strcal(files,verbose,status)

	implicit none
	character*(*) files
	logical verbose
	integer status

C-----------------------------------------------------------------------
C Description: Moves the calibration files listed in the files argument
C              to the appropriate instrument directory as indicated by 
C              the caldbconfig file.
C
C              If the first character of the files argument is an '@'
C              character the remainder of the files argument is 
C              interpreted as the name of a file containing the
C              filenames to be accessed.
C
C              Each calibration file is searched for the TELESCOP,
C              INSTRUME, and CTYP0001 keywords.  The values of the 
C              TELESCOP and INSTRUME keywords are used when consulting
C              the caldbconfig file to learn the location of the top 
C              level instrument directory.  The caldbconfig file is 
C              found through the CALDBCONFIG environment variable. The
C              user should have this variable set to point at the 
C              caldbconfig file.  The value of the CCLS0001 keyword
C              determines whether the file is placed in the 'bcf' or
C              'cpf' subdirectory beneath the instrument directory.
C
C Arguments:   files   (i): the names of the calibration files to move.
C                           If the first character is an '@' symbol the
C                           remainder of the files argument is
C                           interpreted as the name of a file containing
C                           the names of the calibration files.
C              verbose (i): toggles informational messages on/off
C              status  (r): the success status of this routine. Non-zero
C                           status indicates that an error occurred
C                           while trying to store one or more cal files.
C                           Note that most errors will not cause this 
C                           process to exit.  It merely moves on to the 
C                           next file in the files list.
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar Sep 16 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	integer errstat,funit,cunit,sp,index,fcstln,blksz,hdutp,clen
	integer extno,i
	logical flist,exist
	character(4) subdir,cval
	character(30) telval,comm,insval,typval
	character(80) calfile
	character(160) contxt,cif,insdir,drdmy1,drdmy2,drdmy3,target
	character(12) subname

C	This is the name of this subroutine
	subname='stcal'

C	initialize the variables
	status = 0
	errstat = 0
	flist = .false.
	sp = 0

C	If the first character of files is '@', then the rest must be
C       the name of the file containing the list.  So set the flist 
C       flag to indicate that we are reading from a file and open the
C       file.  Otherwise, the entire list is contained in the files
C       argument, so make sure there are no preceeding spaces in the
C       list.

	if (files(1:1).eq.'@') then
	     flist = .true.
	     files=files(2:)
	     call cgetlun(funit)
	     open(funit,file=files,iostat=errstat,status='OLD')
	     if (errstat.ne.0) then
	          contxt=subname(:fcstln(subname))//' : '//
     &	          'Cannot open the file:'
	          call fcerr(contxt)
	          call fcerr(files)
	          status = 1
	          return
	     endif
	else
	     call crmvlbk(files)
	endif

C	Top level of the file list loop
100	continue

C	If we're reading from a file, read the next record into
C       the calfile variable.  Otherwise, find the next filename
C       in the files argument, write it to calfile, and remove it
C       from the files argument.  If the end of either list is 
C       encountered, goto 1000.
	if (flist) then
	     read(funit,'(A)',iostat=errstat,end=1000)calfile
	     if (errstat.ne.0) then
	          contxt=subname(:fcstln(subname))//' : '//
     &	          'Unable to read next record in file:'
	          call fcerr(contxt)
	          call fcerr(files)
	          status = 2
	          return
	     endif
	else
	     sp = index(files,' ')
	     if ((sp.eq.0).or.(sp.eq.1)) goto 1000
	     calfile = files(:sp-1)
	     files = files(sp+1:)
	endif

C	get a free logical unit number
	call cgetlun(cunit)

C	Open the calibration file
	call ftopen(cunit,calfile,0,blksz,errstat)
	if (errstat.ne.0) then
	     errstat=0
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Unable to open the file:'
	     call fcerr(contxt)
	     call fcerr(calfile)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     call cfrelun(cunit)
	     goto 100
	endif

C	Initialize the extno variable for searching the cal file
	extno = 0

C	Top of the cal file searching loop
200	continue

C	reset the errstat flag in case an error occurred
	errstat = 0

C	Increment the extno variable and move to that extension.
C       If end of file encountered (107), then we must have searched
C       the entire file without finding the TELESCOP or INSTRUME
C       keywords.

	extno = extno + 1
	call ftmahd(cunit,extno,hdutp,errstat)
	if ((errstat.ne.0).and.(errstat.ne.107)) then
	     errstat=0
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &	     'Error searching calibration file:'
	     call fcerr(contxt)
	     call fcerr(calfile)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     call ftclos(cunit,errstat)
	     call cfrelun(cunit)
	     goto 100
	else if (errstat.eq.107) then
	     errstat=0
	     contxt=subname(:fcstln(subname))//' : '//
     &	     'Cannot find TELESCOP and INSTRUME keywords in file:'
	     call fcerr(contxt)
	     call fcerr(calfile)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     call ftclos(cunit,errstat) 
             call cfrelun(cunit)
             goto 100
	endif

C	Try getting the TELESCOP, INSTRUME, and CCLS0001 keywords.
C       If they are not present (202), move on to the next extension.
C       If an error occurred, move on to the next file.

	call ftgkys(cunit,'TELESCOP',telval,comm,errstat)
	call ftgkys(cunit,'INSTRUME',insval,comm,errstat)
	call ftgkys(cunit,'CCLS0001',typval,comm,errstat)
	if ((errstat.ne.0).and.(errstat.ne.202)) then
	     errstat = 0
	     status = 3
C	     reset the extno variable so that it matches the ftools
C            convention.
	     extno = extno - 1
	     write(cval,'(I4)')extno
	     contxt=subname(:fcstln(subname))//' : '// 
     &       'Error reading TELESCOP, INSTRUME or CCLS0001 keyword'
	     call fcerr(contxt)
	     contxt='in extension '//cval//' of file: '
     &	     //calfile(:fcstln(calfile))
	     call fcerr(contxt)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     call ftclos(cunit,errstat)
	     call cfrelun(cunit)
	     goto 100
	else if (errstat.eq.202) then
	     errstat = 0
	     goto 200
	endif

C	Close the cal file and free the logical unit number.
	call ftclos(cunit,errstat)
	if (errstat.ne.0) then
	     errstat = 0
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Unable to close the calibration file: '
	     call fcerr(contxt)
	     call fcerr(calfile)
	endif
	call cfrelun(cunit)

C	Determine the correct subdirectory for the cal file based
C       on the value of the CCLS0001 keyword.  If a valid CCLS0001
C       value is not present, move on to next file.
	if (typval.eq.'BCF') then
	     subdir = '/bcf'
	else if (typval.eq.'CPF') then
	     subdir = '/cpf'
	else
	     status = 3
	     write(cval,'(I4)')extno
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Illegal value for CCLS0001 keyword in extension '//cval
	     call fcerr(contxt)
	     contxt='of file: '//calfile(:fcstln(calfile))
	     call fcerr(contxt)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     goto 100
	endif

C	Find out the location of the top level instrument directory
C       from the caldbconfig file.
	call rdcnfg(telval,insval,.false.,cif,insdir,errstat)
	if (errstat.ne.0) then
	     errstat = 0
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Error reading caldbconfig file'
	     call fcerr(contxt)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     goto 100
	endif

C	Append the bcf or cpf subdirectory, along with the cal file name
C       to the instrument directory.  First, split the instrument 
C       directory into it's disk, dir, and file components.  Then append 
C       the subdirectory onto the file component.  Append the modified
C       file component onto the dir component, and reset the file
C       component as the name of the cal file.  Finally, reassemble the 
C       pieces into the new system dependent path.  The new path contains
C       the location to which the cal file will be moved.

C	Break up the instrument directory into its components
	call ppthnm(insdir,drdmy1,drdmy2,drdmy3)

C	If drdmy3 is not empty, we're on a UNIX system and we need
C	to reassemble the directory components.
	if (fcstln(drdmy3).ne.0) then
	     drdmy2 = drdmy2(:fcstln(drdmy2))//'/'//
     &	     drdmy3(:fcstln(drdmy3))
	     drdmy3 = ' '
	endif

C	Append the subdirectory onto the directory component
	drdmy2 = drdmy2(:fcstln(drdmy2))//subdir(:fcstln(subdir))

C	If the calibration file is specified using an absolute or
C       relative path, strip off the filename.  Otherwise, use the 
C	cal file name as is.
	if ((index(calfile,'[').ne.0).or.(index(calfile,'/').ne.0)) then
	     clen = fcstln(calfile)
	     Do 300 i=1,clen
	          if ((calfile(clen+1-i:clen+1-i).eq.'/').or.
     &	          (calfile(clen+1-i:clen+1-i).eq.']')) then
	               target=calfile(clen+2-i:)
	          endif
300	     continue
	else
	     target=calfile
	endif

C	Reassemble the components in a system dependent way to get
C       the location to which the cal file will be moved.
	call cpthnm(drdmy1,drdmy2,target,errstat)
	if (errstat.ne.0) then
	     errstat = 0
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &	     'Error constructing target pathname'
	     call fcerr(contxt)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     goto 100
	endif

C	See if the cal file is really there
	inquire(file=calfile,exist=exist)
	if (.not.exist) then
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &       'File does not exist:'
	     call fcerr(contxt)
	     call fcerr(calfile)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     goto 100
	endif

C	See if the target location is already occupied by a file of
C       the same name.
	inquire(file=target,exist=exist)
	if (exist) then
	     status = 3
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Target file already exists:'
	     call fcerr(contxt)
	     call fcerr(target)
	     contxt='skipping to next file'
	     call fcerr(contxt)
	     goto 100
	endif

C	If the user wants to know whats going on, tell him/her.
	if (verbose) then
	     contxt='Moving '//calfile(:fcstln(calfile))//' to '//
     &	     target(:fcstln(target))
	     call fcecho(contxt)
	endif

C	Move the cal file to it's new home (sniff sniff)
	call mvfile(calfile,target)

C	Check to see that it got there alright
	inquire(file=target,exist=exist)
	if (.not.exist) then
	     status = 4
	     contxt=subname(:fcstln(subname))//' : '//
     &       'Unable to store the file: '//calfile(:fcstln(calfile))
	     call fcerr(contxt)
	     contxt='in the location: '//target(:fcstln(target))
	     call fcerr(contxt)
	     contxt='Does the directory exist?'
	     call fcerr(contxt)
	endif

C	Move on to the next file
	goto 100

C	This is were we end up at the end of the files list
1000	continue

C	If we were reading from a file, close it and free up the
C       logical unit number we were using.
	if (flist) then
	     close(funit)
	     call cfrelun(funit)
	endif

	return
	end
