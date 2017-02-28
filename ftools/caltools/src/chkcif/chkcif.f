*+CHKCIF
      subroutine chkcif()
      implicit none

C-----------------------------------------------------------------------
C Description: Checks a Calibration Index File (CIF) by extracting
C              each entry which is listed as ONLINE, opening
C              the corresponding calibration file and advancing to
C              the correct extension. Any problems in reading/advancing
C              are reported to the file named in the outfile parameter.
C
C Arguments:   none
C
C Origin:      Written for the Calibration Database. Based in large
C              part on routines written by Ron Zellar (qzcif, gtcal)
C
C Authors/Modification History:
C  Mike Tripicco (1.1.0: 95 Jul 24), original version
C  Ian M George  (1.1.1: 95 Dec 13), minor mods to o/p
c  Ian M George  (1.1.2: 95 Dec 18), added chatter as passed in wtbegm & wtendm
c			             and added call caldb_info
c  Ian M George  (1.1.3: 95 Dec 28), rdcnfg replaced with rcnfig
c  Jeff Guerber (1.1.4 1999-02-23) can't call ftgstm with a constant arg
c
	character(7) version
	parameter (version = '1.1.4')
C-----------------------------------------------------------------------
*-
c Internals
	character(40) taskname
	character(160) outfil,ciffil,junkdir
	character(160) message
	character(20)  cnfgvar, caldbvar, mode
	character(10)  missn, instr
	integer errstat, chatter
        logical page

c Initialize
	taskname = 'chkcif'
	errstat = 0
	cnfgvar = 'CALDBCONFIG'
	caldbvar = 'CALDB'
c ... following is a temporary place-holder
	mode = 'INST'

c Get Parameters from the par file
	call gpckcf(missn,instr,outfil,ciffil,chatter, page,errstat)
	if(errstat.NE.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

c Check that the CALDB is defined & available to the user
	call caldb_info(chatter, mode, missn, instr, errstat)
	if(errstat.NE.0) then
	   message = 'CALDB not defined/available'
	   call wterrm(taskname, version,message)
	   message = 'Task requires CALDB to be both defined '//
     &		'available in order to run'
	   call wtinfo(chatter,1,1, message)
           goto 148
	endif

C Read the Configuration file to locate the correct CIF
	if (ciffil(1:5) .eq. 'CALDB') then
	     call rcnfig(missn,instr,chatter,ciffil,junkdir,errstat)
	     if (errstat .ne. 0) goto 148
	endif

c Check the CIF and report problem file/extensions to outfile
        call chcif(version,ciffil,outfil,chatter,page,errstat)
	if (errstat .ne. 0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version, errstat, chatter)

	return
	end

C-----------------------------------------------------------------------
*+GPCKCF
	subroutine gpckcf(missn,instr,outfil,ciffil,
     &		chatter, page,status)

	implicit none
        character*(*) missn,instr,outfil,ciffil
	logical page
	integer chatter, status

C---------------------------------------------------------------------
C Description:  Gets the parameters for CHKCIF from the parameter
C               file.  If ciffil param is default ("CALDB") then ask
C               for and use the mission/instrument params (plus
C               environment variables) to specify the caldb.config file.
C               If a "non-standard" ciffil is specified, use it and
C               ignore the mission & instrument params.
C
C Arguments:    missn    : The value of the 'mission' param
C               instr    : The value of the 'instrument' param
C               outfil   : The value of the 'outfile' param
C               ciffil   : The value of the 'ciffile' param
C               page     : The value of the 'page' param
C               status   : The status of the subroutine
C                              = 0  --> OK
C                              = 1  --> Problem getting parameters
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C  Mike Tripicco (1.1.0: 95 July 24), original version
c  Ian M George  (1.1.1: 95 Dec 13) changed o/p to include wtinfo etc
        character(7) version
        parameter (version = '1.1.1')
C---------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'gpckcf')
        character(50) contxt
        integer errstat
c Initialize
        status = 0
        errstat = 0

C Get ciffile parameter
        call uclgst('ciffile', ciffil, errstat)
        if(errstat.ne.0) then
          call wterrm(subname, version,
     &          'Problem getting CIFFILE parameter')
	  status = 1
          goto 999
        endif

C Get mission and instrument parameters unless a non-standard CIF was named
c in the ciffile parameter and return on error
	if (ciffil(1:5) .eq. 'CALDB') then
             call uclgst('mission', missn, errstat)
        	if(errstat.ne.0) then
          		call wterrm(subname, version,
     &    	      		'Problem getting MISSION parameter')
	  		status = 1
          		goto 999
        	endif
             call uclgst('instrument', instr, errstat)
        	if(errstat.ne.0) then
          		call wterrm(subname, version,
     &    	      		'Problem getting INSTRUMENT parameter')
	  		status = 1
          		goto 999
        	endif
        endif

C Get outfile parameter, return on error
        call uclgst('outfile', outfil, errstat)
         if(errstat.ne.0) then
          	call wterrm(subname, version,
     &    		'Problem getting OUTFILE parameter')
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

C Get page parameter, return on error
        call uclgsb('page', page, errstat)
         if(errstat.ne.0) then
                call wtwarm(subname, version, 1, 1,
     &                  'Problem getting PAGE parameter')
		call wtinfo(chatter,1,1,
     &		        'Setting PAGE = NO')
		page = .false.
		status = 0
		errstat = 0
         endif

999	if(status.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

        return
        end

C---------------------------------------------------------------------
*+CHCIF
        subroutine chcif(taskversion,ciffil,outfil,chatter,page,status)

        implicit none
        character*(*) outfil,ciffil, taskversion
        logical page
        integer status, chatter

C---------------------------------------------------------------------
C Description:  Opens the CIF and outfile (if not STDOUT) then
C               for each ONLINE entry in the CIF open the calibration
C               file and advance to the proper extension. Report
C               any files which are not online, cannot be opened,
C               or cannot be advanced to the extension to outfile.
C
C Arguments:    outfil   : The value of the 'outfile' param
C               ciffil   : The value of the 'ciffile' param
C		chatter  : chattiness flag
C               page     : The value of the 'page' param
C               status   : The status of the subroutine
C                              = 0    --> OK
C                              = 1    --> FTOOLS subr error
C                              = xxx  --> FITSIO error
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C  Mike Tripicco (1.0.0: 95 July 24), original version
c  Ian M George  (2.0.0: 95 Dec 13) added wtinfo etc +
c  Lorraine Breedon (2.0.1: 98 Jun 23) replace gtdati and gttime with ftgstm
c  Jeff Guerber (2.0.2 1999-02-23) can't call ftgstm with a constant arg

        character(7) version
        parameter (version = '2.0.2')
*-
c Internals
        character(10) subname
        parameter (subname = 'chcif')
	character(4) cval,cval1, cval2
	character(20) caldbvar
	character(30) sjunk, online
	character(70) dirval
	character(160) file
        character(240) contxt,commentnew
	integer iunit, ounit, ijunk, nax2val, fcstln, errstat
	integer devcol, dircol, filcol, extcol, i, extno, pgfstat
	integer cunit, j, lastslash, timeref
      	logical ljunk, outopen
        character(30) timestr

c Initialization
	caldbvar = 'CALDB'
	outopen = .false.
	errstat = 0
	pgfstat = 0
	status = 0
        timeref = 0

C Open the output (log) file
	call cgetlun(ounit)
	if (outfil(1:6) .ne. 'STDOUT') then
	     call faopen (ounit, outfil, 2, -80, errstat)
	     if (errstat .ne. 0) then
	       call wterrm(subname, version,
     &		'problem opening output file')
	       contxt = 'offending file: '//outfil
	       call wtinfo(chatter,1,1, contxt)
	       call wtinfo(chatter,1,2, 'does it already exist ?')
	       status = 1
	       go to 482
	     else
	       outopen = .true.
	     endif
             call ftgstm(timestr, timeref, errstat)

	     contxt = '# Results from chkcif '//taskversion//' run on '//
     &		timestr//' (system clock)'
	     call pgfout(ounit,outopen,contxt,pgfstat)
             if (pgfstat .ne. 0) then
	             call ftclos(cunit,errstat)
	             call cfrelun(cunit,errstat)
	             goto 110
	     endif
	     contxt = '# ... problems w/ following files in CIF: '//
     &		ciffil(:fcstln(ciffil))
	     call pgfout(ounit,outopen,contxt,pgfstat)
	     if (pgfstat .ne. 0) goto 110
	else
C Enable paging in pgfout
	     if (page) ounit = - ounit
	endif

C Open the CIF
        call cgetlun(iunit)
        call ftopen(iunit, ciffil, 0, ijunk, errstat)
	if (errstat .ne. 0) then
          contxt = ' Problem opening CIF'
          call wtferr(subname, version, errstat, contxt)
          status = errstat
          goto 110
        endif

C Move to the first extension
        call ftmahd(iunit, 2, ijunk, errstat)
	if (errstat .ne. 0) then
          contxt = ' Problem moving to correct extension'
          call wtferr(subname, version, errstat, contxt)
          status = errstat
          goto 110
        endif

C Find out how many rows the CIF contains
        call ftgkyj(iunit, 'NAXIS2', nax2val, sjunk, errstat)
	if(errstat .ne. 0) then
            call wtferr(subname, version, errstat,
     &          ' reading NAXIS2 keyword')
            status = errstat
            goto 110
        endif
        if(nax2val.eq.0)then
	     call wterrm(subname, version, 'Zero rows in the CIF')
             status = 1
             goto 110
        endif

C Get the column numbers of all the columns to be read
        call ftgcno(iunit,.true.,'CAL_DEV',devcol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_DEV column in CIF')
            status = errstat
            goto 110
        endif

        call ftgcno(iunit,.true.,'CAL_DIR',dircol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_DIR column in CIF')
            status = errstat
            goto 110
        endif

        call ftgcno(iunit,.true.,'CAL_FILE',filcol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_FILE column in CIF')
            status = errstat
            goto 110
        endif

        call ftgcno(iunit,.true.,'CAL_XNO',extcol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_XNO column in CIF')
            status = errstat
            goto 110
        endif

C --------------------- START OF DO-LOOP -----------------------
C Read each row and assemble full pathname and ext.num
	do 100 i=1, nax2val
	     errstat = 0
C ...........Get directory value
	     call ftgcvs(iunit,dircol,i,1,1,' ',dirval,ljunk,errstat)
             if(errstat.ne.0)then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_DIR column of CIF')
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')dircol
	        contxt='error occurs in row = '//cval1//
     &			' column = '//cval2
		call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C ...........Get filename value
	     call ftgcvs(iunit,filcol,i,1,1,' ',file,ljunk,errstat)
	     if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_FILE column of CIF')
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')filcol
	        contxt='error occurs in row = '//cval1//
     &			' column = '//cval2
		call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C ...........Reparse filename so that the path parts get put into dirval
C            so cpthnm will work properly on VMS (it only VMS-parses
C            the "dirval" argument)
             lastslash = 0
             do 200 j=1,fcstln(file)
               if (file(j:j) .eq. '/') lastslash=j
200          continue
             if (lastslash .ne. 0) then
               dirval = dirval(:fcstln(dirval)) //
     &           '/' // file(:lastslash-1)
               file=file(lastslash+1:)
             endif

C ...........Construct system dependent pathname (and put back in file)
	     call cpthnm(caldbvar,dirval,file,errstat)
	     if (errstat .ne. 0) then
		contxt = 'unable to construct path name from '//
     &			'values in CIF'
		call wterrm(subname, version, contxt)
	        write(cval,'(I4)')i
	        contxt='error occurs in row = '//cval
		call wtinfo(chatter,1,1,contxt)
	        status = 2
	        goto 110
	     endif

C ...........Get extension number
             call ftgcvj(iunit,extcol,i,1,1,0,extno,ljunk,errstat)
	     if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_XNO column of CIF')
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')extcol
	        contxt='error occurs in row = '//cval1//
     &			' column = '//cval2
		call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C ...........Get online/offline value
             call ftgcvs(iunit,devcol,i,1,1,' ',online,ljunk,errstat)
	     if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_XNO column of CIF')
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')devcol
	        contxt='error occurs in row = '//cval1//
     &			' column = '//cval2
		call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C ...........Only attempt to open calibration files listed as ONLINE
	     if (online(1:6) .ne. 'ONLINE') then
	       write(cval,'(I4)')i
	       contxt= file(:fcstln(file))//' # file OFFLINE '//
     &			' (CIF row '//cval//')'
	       call pgfout (ounit, outopen, contxt, pgfstat)
               if (pgfstat .ne. 0) goto 110
	     else
C ...........Open the calibration file
	       call cgetlun(cunit)
	       call ftopen(cunit,file,0,ijunk,errstat)
	       if (errstat .ne. 0) then
	           write(cval,'(I4)')i
	           contxt= file(:fcstln(file))//' # cannot be opened'//
     &			' (CIF row '//cval//')'
	            status = errstat
                   call pgfout (ounit, outopen, contxt, pgfstat)
                   if (pgfstat .ne. 0) then
	             call ftclos(cunit,errstat)
	             call cfrelun(cunit,errstat)
	             goto 110
	           endif
	           goto 101
	       endif
C .............Scroll to desired extension
	       call ftmahd(cunit, extno+1, ijunk, errstat)
	       if (errstat .ne. 0) then
	           write(cval,'(I4)')i
	           write(cval1,'(I4)')extno
	           contxt= file(:fcstln(file))//
     &		   ' # cannot be scrolled to extn '//cval1//
     &			' (CIF row '//cval//')'
	            status = errstat
                   call pgfout (ounit, outopen, contxt, pgfstat)
                   if (pgfstat .ne. 0) then
	             call ftclos(cunit,errstat)
	             call cfrelun(cunit,errstat)
	             goto 110
	           endif
		   goto 101
	       endif
101	       call ftclos(cunit,errstat)
	       call cfrelun(cunit,errstat)
	     endif

100     continue
C --------------------- END OF DO-LOOP -----------------------

C Close the CIF
110	errstat = 0
	call ftclos(iunit,errstat)
	if (errstat .ne. 0) then
             call wtferr(subname, version, errstat,
     &            ' closing CIF')
	     status = errstat
	else
	     call cfrelun(iunit)
	endif

C       Close the LOG (unless outfile is STDOUT)
	if (outopen) then
	     close(ounit)
	     call cfrelun(ounit)
	endif


C Report overall success to stderr
482 	if (status .ne. 0) then
          call wterrm(subname, version, ' Fatal - aborting')
          call wtinfo(chatter,1,1,contxt)
	else
	  contxt='Contents of CIF '//ciffil(:fcstln(ciffil))//
     &       ' verified'
	  call wtinfo(chatter,1,1,contxt)
        endif
                 commentnew='See ' // outfil // ' for more information'
                 call wtinfo(chatter,1,1,commentnew)

	return
	end
