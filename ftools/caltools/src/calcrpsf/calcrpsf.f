*+CALCRPSF
	SUBROUTINE CALCRF
	IMPLICIT NONE
c 
c Description:
c  Wrapper program to run (spawn) some/all of the following tasks
c     ST2RPSF - convert from o/p of STWFITS to OGIP RPSF FITS format
c     RBNRPSF - rebins an RPSF dataset
c     PCRPSF  - generate a theoretical RPSF for the ROSAT PSPC 
c     HRIRPSF - generate a theoretical RPSF for the ROSAT HRI
c     RPSFQDP - dumps an RPSF dataset to a QDP file
c
c User i/ps required (prompted for):
c  None here, isolated in GP_CALRPSF (see below), and prompted for as 
c  necessary by the individual tasks.
c
c  PROGRAMMERS !!: When adding new theoretical generators, remember to update
c                  the qpred "help" in GP_CALCRPSF.
c
c Called routines
c  subroutine FCECHO           : (FTOOLS) write to standard i/o
c  subroutine FCERR	       : (FTOOLS) write to standard error
c  subroutine GP_CALCRPSF      : (below) gets parameters from the par file
c  subroutine DO_CALCRPSF      : (below) the 'main' routine
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 94 Jan 31), original
c  Ian M George     (1.1.0: 94 Feb 03), clean up properly
c  Ian M George     (1.2.0: 94 Feb 14), added ROSAT HRI
c  Ian M George     (1.3.0: 94 Mar 08), cleaned up parameter getting routine
c  Ian M George     (1.3.1: 94 Sep 12), minor stuff in parameter getting routine
	character(7) version
	parameter (version = '1.3.1')
*- 
c Internals 
        character(40) taskname
	integer chatter, schatter, ierr
	character(16) telescop,instrume
	character(25) context
	character(80) infil, outfil, message
	logical qst2rpsf, qrbnrpsf, qpred, qrpsfqdp,killit

c Initialize
        COMMON/task/taskname
        taskname ='CALCRPSF '//version
        ierr = 0

        message = '** CALCRPSF '//version
        call fcecho(message)

c Get Parameters from the par file
	call gp_calcrpsf(infil, outfil, telescop, instrume, 
     &		chatter, schatter, 
     &		qst2rpsf, qrbnrpsf, qpred, qrpsfqdp,killit,ierr)
	if(ierr.NE.0) goto 926

c Do the nasty deed(s)
        call do_calcrpsf(infil, outfil, telescop, instrume, 
     &		chatter, schatter, 
     &		qst2rpsf, qrbnrpsf, qpred, qrpsfqdp, killit,ierr)

c Inform user of failure, if necessary
926     if(ierr.NE.0) then
          context = 'Fatal'
          message = '** CALCRPSF '//version//' ERROR : '// context
          call fcecho(message)
          call fcerr(context)
        else
          message = '** CALCRPSF '//version//' Finished'
          call fcecho(message)
        endif

	return
	end



c -------------------------------------------------------------------------
*+GP_CALCRPSF
	subroutine gp_calcrpsf(infil, outfil, telescop, instrume, 
     &		chatter, schatter, 
     &		qst2rpsf, qrbnrpsf, qpred, qrpsfqdp, killit,ierr)

	IMPLICIT NONE
	integer chatter, schatter, ierr
	character*(*) telescop, instrume
	character*(*) infil, outfil
	logical qst2rpsf, qrbnrpsf, qpred, qrpsfqdp,killit
c 
c Description:
c  Gets the parameters required by CALCRPSF from the parameter file
c  NOTE - The par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  INFILE      - name of i/p FITS SOURCE file (produced by stwfits)
c ??? incomplete
c
c Origin:
c  Original
c
c Called Routines
c  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
c  subroutine WT_FERRMSG       : (CALLIB) Writes standard FITSIO message etc
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1994 Jan 31), Original
c  Ian M George     (1.1.0: 1994 Mar 08), Added infil=NONE option
	character(7) version
	parameter (version = '1.1.0')
*- 
c Internals
	character(40)  errstr, wrnstr
	character(40) string
	character(80)  message
c Initialize
	errstr = '** GP_CALCRPSF '//version//' ERROR: '
	wrnstr = '** GP_CALCRPSF '//version//' WARNING: '
	qpred = .true.
	ierr = 0
	telescop = 'UNKNOWN'
	instrume = 'UNKNOWN'

c Go get 'em coyboy....

c Get the name of the i/p file (from stwfits)
	call uclgst('infil',infil, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting INFIL parameter'
		call fcecho(message)
		return
	endif

c Get the name of the o/p FITS file in OGIP-standard format
	call uclgst('outfil',outfil, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting OUTFIL parameter'
		call fcecho(message)
		return
	endif
	if(outfil.EQ.' ') then
	        message = errstr // 'No OUTFIL entered'
	        call fcecho(message)
		ierr = 1
		return
	endif

c Get the chattiness flag
	call uclgsi('chatter',chatter, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting CHATTER parameter'
		call fcecho(message)
		ierr = 0 
		message = errstr // 'Setting CHATTER = 10'
		call fcecho(message)
		chatter = 9
	endif	

c Give user info if requested
	if(chatter.GE.20) then	
	 message = ' ... using GP_CALCRPSF ' // version
	 call fcecho(message)
	endif

c get clobber parameter ...

       call uclgsb('clobber',killit, ierr)
        if(ierr.NE.0) then
                message = errstr // 'Getting CLOBBER parameter'
                call fcecho(message)
                ierr = 0
        endif

c Get the chattiness flag (for spawned tasks)
	call uclgsi('schatter',schatter, ierr)
	if(ierr.NE.0) then
		message = errstr // 'Getting SCHATTER parameter'
		call fcecho(message)
		ierr = 0 
		message = errstr // 'Setting SCHATTER = 10'
		call fcecho(message)
		schatter = 9
	endif	

c Skip to qpred if no i/p file
	call crmvlbk(infil)
	if(infil(:4).EQ.'NONE'.or.infil(:4).EQ.'none'
     &		.or.infil.EQ.' ')then
	   qst2rpsf = .false.
	   qrbnrpsf = .false.	   
	   goto 564
	endif

c Get the logicals indicating which tasks are to be spawned
c ... st2rpsf
	call uclgsb('qst2rpsf',qst2rpsf, ierr)
	if(ierr.NE.0) then
	  message = errstr // 'Getting QST2RPSF parameter'
	  call fcecho(message)
	  return
	endif
c ... rbnrpsf
	call uclgsb('qrbnrpsf',qrbnrpsf, ierr)
	if(ierr.NE.0) then
	  message = errstr // 'Getting QRBNRPSF parameter'
	  call fcecho(message)
	  return
	endif
c ... the prediction routines
564	continue
	call uclgst('qpred',string, ierr)
	if(ierr.NE.0) then
	  message = errstr // 'Getting QPRED parameter'
	  call fcecho(message)
	  return
	endif
	call crmvlbk(string)
	if(string(:1).eq.'T'.or.string(:1).eq.'t'
     &		.or.string(:2).eq.'.T'.or.string(:2).eq.'.T'
     &		.or.string(:1).eq.'y'.or.string(:1).eq.'Y') then
		qpred = .true.
	     if((.not.qst2rpsf) .AND. (.not.qrbnrpsf)) then
		call uclgst('telescop',telescop, ierr)
		if(ierr.NE.0) then
		  message = errstr // 'Getting TELESCOP parameter'
		  call fcecho(message)
		  return
	        endif
		call uclgst('instrume',instrume, ierr)
		if(ierr.NE.0) then
		  message = errstr // 'Getting INSTRUME parameter'
		  call fcecho(message)
		  return
	        endif
	      endif
	elseif(string(:1).eq.'F'.or.string(:1).eq.'f'
     &		.or.string(:2).eq.'.F'.or.string(:2).eq.'.F'
     &		.or.string(:1).eq.'N'.or.string(:1).eq.'n') then
		qpred = .false.
	elseif(string(:1).eq.'?'.or.string(:4).eq.'help'
     &		.or.string(:4).eq.'HELP') then
	    message = ' ... PSF datasets can be generated for '//
     &		'the following instruments:'
	    call fcecho(message)
	    message = '     ROSAT HRI       (using hrirpsf)'
	    call fcecho(message)
	    message = '     ROSAT PSPC      (using pcrpsf)'
	    call fcecho(message)
	    goto 564
	else
	    message = errstr//' Unrecognized answer'
	    call fcecho(message)
	    message = ' ... allowed values: yes/no, true/false, or ?' 
	    call fcecho(message)
	    goto 564
	endif

c ... rpsfqdp
	call uclgsb('qrpsfqdp',qrpsfqdp, ierr)
	if(ierr.NE.0) then
	  message = errstr // 'Getting QRPSFQDP parameter'
	  call fcecho(message)
	  return
	endif

	return
	end
c -------------------------------------------------------------------------
*+DO_CALCRPSF
	subroutine do_calcrpsf(infil, outfil, telescop, instrume, 
     &		chatter, schatter, 
     &		qst2rpsf, qrbnrpsf, qpred, qrpsfqdp, killit,ierr)

	IMPLICIT NONE
	integer chatter, schatter, ierr
	character*(*) telescop, instrume
	character*(*) infil, outfil
	logical qst2rpsf, qrbnrpsf, qpred, qrpsfqdp,killit
c 
c Description:
c  Program which actually spawns all the necessary standalones
c
c Passed parameters
c ???
c
c Origin:
c  Original
c
c Called Routines
c Task  STW2PHA		: (CALTOOLS) Converts for STWFITS o/p to OGIP standard
c Task  MATHPHA		: (HEASARC) Performs maths on PHA datasets
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1994 Jan 31), original... ROSAT PSPC only
c  Ian M George     (1.1.0: 94 Feb 14), added ROSAT HRI
c  Ian M George     (1.1.1: 1994 Sep 12), infil=infile etc 
        character(7) version
        parameter (version = '1.1.1')
*- 
c Internals
	integer status, lspawn, fcstln, clenact
	integer nstrs, lstr(20), i
	real ftsver
	character(80) str(20), dummy
	character(80) message, blank
	character(512) cbuf
	character(40) errstr, wrnstr
        character(80) dummystr(1), ill_fil(3)
        logical qokfil, qmore, exist
c Initialize
	status = 0
	ierr = 0
        dummystr(1) = ' '
	blank = ' '
	qmore = .false.
	errstr = '** DO_CALCRPSF '//version//' ERROR:'	
	wrnstr = '** DO_CALCRPSF '//version//' WARNING:'	

c Give user info if requested
        if(chatter.GE.20) then
           message = ' ... using DO_CALCRPSF '// version
	   call fcecho(message)
           call ftvers(ftsver)
           write(message,'(a,f6.3)')
     &          ' ... using FITSIO Version ', ftsver
           call fcecho(message)
        endif

c The Host-interface considers all characters after a ! to be a comment,
c   even if its enclosed within double quotes. Thus files specified to 
c   be overwritten should be removed (under unix/ultrix) here, in main, 
c   rather than passing a filename precceded by a "!" to the spawned 
c   tasks (ho, hum, yawn, yawn)
	ill_fil(1) = 'st2rpsf.tmp'
	ill_fil(2) = 'rbnrpsf.tmp'
	ill_fil(3) = 'rpsfpred.tmp'
	call ck_file(outfil,ill_fil,3,qokfil,killit,chatter) 
	if(.NOT.qokfil) then
	  message = errstr // 'OUTFIL has an illegal name'
	  call fcecho(message)
	  message = ' ...... (name is reserved for a temporary file)'
	  call fcecho(message)
	  ierr = 1
	  goto 482
	endif

c Check that the o/p file doesn't already exist or is otherwise illegal
        call ck_file(outfil,dummystr, 1, qokfil, killit,chatter)
        if(.NOT.qokfil) then
                message = '... Offending file is OUTFIL: '// outfil(:50)
                call fcecho(message)
                ierr = -1
                goto 482
        endif

c Clean up any 'old' temporary files which might be around
        INQUIRE(FILE='st2rpsf.tmp',EXIST=exist)
	  if(exist) call delfil('st2rpsf.tmp')
        INQUIRE(FILE='rbnrpsf.tmp',EXIST=exist)
	  if(exist) call delfil('rbnrpsf.tmp')
        INQUIRE(FILE='rpsfpred.tmp',EXIST=exist)
	  if(exist) call delfil('rpsfpred.tmp')

c --------------------------- ST2RPSF -------------------------
c ... construct the command string and spawn ST2RPSF task if necessary

	if(qst2rpsf) then
		if(qrbnrpsf.OR.qpred.OR.qrpsfqdp) then
			qmore = .true.
		else
			qmore = .false.
		endif
		call crmvblk(infil)
		call crmvblk(outfil)
		write(str(1),'(a,i12)') 'chatter=',schatter
		str(2) = ' infil="'// infil(:clenact(infil))//'"'
		if(qmore) then
		   str(3) = ' outfil='//'st2rpsf.tmp'
		else
		   str(3) = ' outfil='//outfil
		endif
		nstrs = 3
		do i = 1, nstrs
		  call crmvblk(str(i))
		  lstr(i) = fcstln(str(i))
		enddo

		cbuf = 'st2rpsf'
		lspawn = fcstln(cbuf)
		do i = 1, nstrs
		  dummy = str(i)
		  cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
		  lspawn = fcstln(cbuf)
		enddo
		 call fcecho(blank)
		message = ' *** spawning ST2RPSF '//
     &				'to convert o/p from STWFITS:'
		call fcecho(message)
		if(lspawn.lt.80) then
		   message = cbuf(:lspawn)
		else
		   message = cbuf(:70) // ' (etc...)'
		endif
		call fcecho(message)
	
		status = 0
		call cspawn(cbuf,lspawn,status)
		if(status.NE.0) then
			message = errstr // ' Problem with ST2RPSF spawn'
			call fcecho(message)
			write(message,'(a,i12)') 
     &			' ... CSPAWN Error flag = ', status
			call fcecho(message)
			ierr = 1
			goto 482
		endif
	 	message = ' *** completed spawn to ST2RPSF'
	 	call fcecho(blank)
		if(qmore) then
			infil = 'st2rpsf.tmp'
		endif
	endif
c ------------------------------------------------------------

c --------------------------- RBNRPSF -------------------------
c ... construct the command string and spawn RBNRPSF task if necessary

	if(qrbnrpsf) then
		if(qpred.OR.qrpsfqdp) then
			qmore = .true.
		else
			qmore = .false.
		endif
		call crmvblk(infil)
		call crmvblk(outfil)
		write(str(1),'(a,i12)') 'chatter=',schatter
		str(2) = ' infile="'// infil(:clenact(infil))//'"'
		if(qmore) then
		   str(3) = ' outfile='//'rbnrpsf.tmp'
		else
		   str(3) = ' outfile='//outfil
		endif
		nstrs = 3
		do i = 1, nstrs
		  call crmvblk(str(i))
		  lstr(i) = fcstln(str(i))
		enddo

		cbuf = 'rbnrpsf'
		lspawn = fcstln(cbuf)
		do i = 1, nstrs
		  dummy = str(i)
		  cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
		  lspawn = fcstln(cbuf)
		enddo
		 call fcecho(blank)
		message = ' *** spawning RBNRPSF '//
     &				'to convert rebin RPSF dataset:'
		call fcecho(message)
		if(lspawn.lt.80) then
		   message = cbuf(:lspawn)
		else
		   message = cbuf(:70) // ' (etc...)'
		endif
		call fcecho(message)
	
		status = 0
		call cspawn(cbuf,lspawn,status)
		if(status.NE.0) then
			message = errstr // ' Problem with RBNRPSF spawn'
			call fcecho(message)
			write(message,'(a,i12)') 
     &			' ... CSPAWN Error flag = ', status
			call fcecho(message)
			ierr = 1
			goto 482
		endif
	 	message = ' *** completed spawn to RBNRPSF'
	 	call fcecho(blank)
		if(qmore) then
			infil = 'rbnrpsf.tmp'
		endif
	endif

c ------------------------------------------------------------

c --------------------------- PREDICTED guys -------------------------
c ... construct the command string and spawn tasks to generate predicted 
c RPSF dataset (if requested)

	if(qpred) then
		if(qrpsfqdp) then
			qmore = .true.
		else
			qmore = .false.
		endif
c ... Go get the mission & instruments from the file
	if((qst2rpsf) .OR. (qrbnrpsf)) then
	  call gt_misinst(chatter, infil, telescop, instrume, ierr)
	  if(ierr.NE.0) goto 482
	endif

c ... check we can handle the telescop/instrument
		call crmvblk(telescop)
	   if(telescop(:5).NE.'ROSAT') then
		  message = errstr//' Unsupported Mission/Satellite'
		  call fcecho(message)
		  ierr = 1
		  goto 482
	   endif
	   call crmvblk(instrume)
c .... ROSAT PSPC
	   if(instrume(:4).EQ.'PSPC') then
		call crmvblk(infil)
		call crmvblk(outfil)
		write(str(1),'(a,i12)') 'chatter=',schatter
		str(2) = ' infile="'// infil(:clenact(infil))//'"'
		if(qmore) then
		   str(3) = ' outfile='//'rpsfpred.tmp'
		else
		   str(3) = ' outfile='//outfil
		endif
		nstrs = 3
		if((qst2rpsf).OR.(qrbnrpsf))then
		   str(4) = 'bkgd = %'
		   nstrs = nstrs + 1
		endif
		do i = 1, nstrs
		  call crmvblk(str(i))
		  lstr(i) = fcstln(str(i))
		enddo

		cbuf = 'pcrpsf'
		lspawn = fcstln(cbuf)
		do i = 1, nstrs
		  dummy = str(i)
		  cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
		  lspawn = fcstln(cbuf)
		enddo
		 call fcecho(blank)
		message = ' *** spawning PCRPSF '//
     &				'to generate prediced RPSF dataset:'
		call fcecho(message)
		if(lspawn.lt.80) then
		   message = cbuf(:lspawn)
		else
		   message = cbuf(:70) // ' (etc...)'
		endif
		call fcecho(message)
	
		status = 0
		call cspawn(cbuf,lspawn,status)
		if(status.NE.0) then
			message = errstr // ' Problem with PCRPSF spawn'
			call fcecho(message)
			write(message,'(a,i12)') 
     &			' ... CSPAWN Error flag = ', status
			call fcecho(message)
			ierr = 1
			goto 482
		endif
	 	message = ' *** completed spawn to PCRPSF'
	 	call fcecho(blank)
		if(qmore) then
			infil = 'rpsfpred.tmp'
		endif
c .... ROSAT HRI
	   elseif(instrume(:3).EQ.'HRI') then
		call crmvblk(infil)
		call crmvblk(outfil)
		write(str(1),'(a,i12)') 'chatter=',schatter
		str(2) = ' infile="'// infil(:clenact(infil))//'"'
		if(qmore) then
		   str(3) = ' outfile='//'rpsfpred.tmp'
		else
		   str(3) = ' outfile='//outfil
		endif
		nstrs = 3
		if((qst2rpsf).OR.(qrbnrpsf))then
		   str(4) = 'bkgd = %'
		   nstrs = nstrs + 1
		endif
		do i = 1, nstrs
		  call crmvblk(str(i))
		  lstr(i) = fcstln(str(i))
		enddo

		cbuf = 'hrirpsf'
		lspawn = fcstln(cbuf)
		do i = 1, nstrs
		  dummy = str(i)
		  cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
		  lspawn = fcstln(cbuf)
		enddo
		 call fcecho(blank)
		message = ' *** spawning HRIRPSF '//
     &				'to generate prediced RPSF dataset:'
		call fcecho(message)
		if(lspawn.lt.80) then
		   message = cbuf(:lspawn)
		else
		   message = cbuf(:70) // ' (etc...)'
		endif
		call fcecho(message)
	
		status = 0
		call cspawn(cbuf,lspawn,status)
		if(status.NE.0) then
			message = errstr // ' Problem with HRIRPSF spawn'
			call fcecho(message)
			write(message,'(a,i12)') 
     &			' ... CSPAWN Error flag = ', status
			call fcecho(message)
			ierr = 1
			goto 482
		endif
	 	message = ' *** completed spawn to HRIRPSF'
	 	call fcecho(blank)
		if(qmore) then
			infil = 'rpsfpred.tmp'
		endif
	   else
		  message = errstr//' Unsupported Detector'
		  call fcecho(message)
		  ierr = 1
		  goto 482
	   endif
	endif

c ------------------------------------------------------------

c --------------------------- RPSFQDP -------------------------
c ... construct the command string and spawn RPSFQDP task if necessary

	if(qrpsfqdp) then
		call crmvblk(infil)
		call crmvblk(outfil)
		write(str(1),'(a,i12)') 'chatter=',schatter
		str(2) = ' datafile="'// infil(:clenact(infil))//'"'
		str(3) = ' outfile='//outfil
		nstrs = 3
		do i = 1, nstrs
		  call crmvblk(str(i))
		  lstr(i) = fcstln(str(i))
		enddo

		cbuf = 'rpsfqdp'
		lspawn = fcstln(cbuf)
		do i = 1, nstrs
		  dummy = str(i)
		  cbuf = cbuf(:lspawn) //' '// dummy(:lstr(i))
		  lspawn = fcstln(cbuf)
		enddo
		 call fcecho(blank)
		message = ' *** spawning RPSFQDP '//
     &				'to convert RPSF dataset to QDP:'
		call fcecho(message)
		if(lspawn.lt.80) then
		   message = cbuf(:lspawn)
		else
		   message = cbuf(:70) // ' (etc...)'
		endif
		call fcecho(message)
	
		status = 0
		call cspawn(cbuf,lspawn,status)
		if(status.NE.0) then
			message = errstr // ' Problem with RPSFQDP spawn'
			call fcecho(message)
			write(message,'(a,i12)') 
     &			' ... CSPAWN Error flag = ', status
			call fcecho(message)
			ierr = 1
			goto 482
		endif
	 	message = ' *** completed spawn to RPSFQDP'
	 	call fcecho(blank)
	endif

c ------------------------------------------------------------

c Clean up the temporary files, if any
c Clean up any 'old' temporary files which might be around
        INQUIRE(FILE='st2rpsf.tmp',EXIST=exist)
	  if(exist) call delfil('st2rpsf.tmp')
        INQUIRE(FILE='rbnrpsf.tmp',EXIST=exist)
	  if(exist) call delfil('rbnrpsf.tmp')
        INQUIRE(FILE='rpsfpred.tmp',EXIST=exist)
	  if(exist) call delfil('rpsfpred.tmp')

c Check of errors
482	if(ierr.ne.0) then
		message = errstr // ' Fatal'
		call fcecho(message)
	endif

	return
	end


c -----------------------------------------------------------
*+ GT_MISINST
	subroutine gt_misinst(chatter, inexp, telescop, 
     &		instrume, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	character*(*) inexp, telescop, instrume
c
c Description
c  Opens, reads & returns the telescope & instrument keyword values 
c from infil
c
c Author/Modification History
c  Ian M George (1.0.0:1994 Feb 03), quick'n'dirty orginal
	character(7) version
	parameter (version = '1.0.0')
*-
c Internals 
	integer status, iunit,block, extn, htype
	integer clenact
	character(40) wrnstr,errstr
	character(80) infil, comm, message
c Initialize
	status = 0
	ierr = 0	
        errstr = '** GT_MISINST '//version//' ERROR:'
        wrnstr = '** GT_MISINST'//version//' WARNING:'


c Parse the supplied filename, stripping off incld extension numbers
        call fcpars(inexp,infil,extn,status)
        if(status.NE.0) then
          message = errstr // ' Problem parsing the expression:'
          call fcecho(message)
          message = ' ......    '//inexp(:MIN(50,clenact(inexp)))
          call fcecho(message)
	  ierr = 1
	  return
        endif

	if(extn.LE.0) extn = 1

c Open the file
        call cgetlun(iunit)
        call ftopen(iunit,infil,0,block,status)
        IF (status.NE.0) THEN
                message = errstr//' opening file: '//infil(:20)
                call wt_ferrmsg(status,message)
                ierr = 1
                return
        ENDIF

c Move to correct extension (first extension),
          call ftmahd(iunit,extn+1,htype,status)


c Read the keys 
        call ftgkys(iunit,'TELESCOP',telescop,comm,status)
        if(status.NE.0) then
		telescop = ' '
        endif
        call ftgkys(iunit,'INSTRUME',instrume,comm,status)
        if(status.NE.0) then
		instrume = ' '
        endif


c Close the file
        call ftclos(iunit, status)


	return
	end



