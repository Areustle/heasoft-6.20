
*+LSTGOOD
      subroutine lstgod
      implicit none

C-----------------------------------------------------------------------
C Description:  Lists all calibration files flagged as good quality
C               in a Calibration Index File
C
C Arguments:    none
C
C Origin:       Written for the Calibration Database. Based in large
C               part upon caldb routines written by Ron Zellar.
C
C Authors/Modification History:
C  Mike Tripicco (1.1.0: 95 Jul 24), original version
c  Ian M George  (1.1.1: 95 Dec 18), added chatter as passed in wtbegm & wtendm
c                                    and added call caldb_info
c  Ian M George  (1.1.2: 95 Dec 28), replaced rdcnfg with rcnfig
c  Lorraine Breedon (1.1.3: 97 Jan 19) concatenation of [#ext] onto filenames
c  Lorraine Breedon (1.1.4: 98 Jun 23) replace gtdati and gttime with ftgstm
c  Jeff Guerber (1.1.5 1999-02-23) can't call ftgstm with a constant arg.
c

        character(7) version
        parameter (version = '1.1.5')
C-----------------------------------------------------------------------
*-
c Internals
        character(40) taskname

	character(10) missn, instr
	character(160) outfil,ciffil,junkdir
        character(160) message
	character(20) cnfgvar, caldbvar, mode
	integer errstat
	integer chatter
        logical page
c Initialize
	taskname = 'lstgood'
	errstat = 0
	cnfgvar = 'CALDBCONFIG'
	caldbvar = 'CALDB'
c ... following is a temporary place-holder
        mode = 'INST'


C Get the parameters from the par file
	call glstgd(missn,instr,outfil,ciffil,page,chatter, errstat)
        if(errstat.NE.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

c Check that the CALDB is defined & available to the user
        call caldb_info(chatter, mode, missn, instr, errstat)
        if(errstat.NE.0) then
           message = 'CALDB not defined/available'
           call wterrm(taskname, version,message)
           message = 'Task requires CALDB to be both defined '//
     &          'available in order to run'
           call wtinfo(chatter,1,1, message)
           goto 148
        endif

C Read the Configuration file to locate the correct CIF
	if (ciffil(1:5) .eq. 'CALDB') then
	     call rcnfig(missn,instr,chatter,ciffil,junkdir,errstat)
	     if (errstat .ne. 0) goto 148
	endif

c Do the job
        call dolsgd(version,outfil,ciffil,instr,page,chatter,errstat)
        if (errstat .ne. 0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version, errstat, chatter)

	return
	end

C-----------------------------------------------------------------------
*+GLSTGD
	subroutine glstgd(missn,instr,outfil,ciffil,page,chatter,status)

	implicit none
        character*(*) missn,instr,outfil,ciffil
	logical page
	integer status, chatter

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
C                              = 1  --> Problem getting prameters
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C  Mike Tripicco (1.1.0:95 July 24), original version
c  Ian M George  (1.1.1: 95 Dec 19) changed o/p to include wtinfo etc
c				    and made chatter a passed parameter

        character(7) version
        parameter (version = '1.1.1')

C---------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'glstgd')
        character(50) contxt
        integer errstat
c Set Status flag to 'no problem!'
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
c in the ciffile parameter or error out

        if (ciffil(1:5) .eq. 'CALDB') then
             call uclgst('mission', missn, errstat)
                if(errstat.ne.0) then
                        call wterrm(subname, version,
     &                          'Problem getting MISSION parameter')
                        status = 1
                        goto 999
                endif
             call uclgst('instrument', instr, errstat)
                if(errstat.ne.0) then
                        call wterrm(subname, version,
     &                          'Problem getting INSTRUMENT parameter')
                        status = 1
                        goto 999
                endif
        endif

C Get outfile parameter
        call uclgst('outfile', outfil, errstat)
         if(errstat.ne.0) then
                call wterrm(subname, version,
     &                  'Problem getting OUTFILE parameter')
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


C Get page parameter
        call uclgsb('page', page, errstat)
         if(errstat.ne.0) then
                call wtwarm(subname, version, 1, 1,
     &                  'Problem getting PAGE parameter')
                call wtinfo(chatter,1,1,
     &                  'Setting PAGE = NO')
                page = .false.
                status = 0
                errstat = 0
         endif

999     if(status.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

        return
        end

C---------------------------------------------------------------------
*+DOLSGD
        subroutine dolsgd(taskversion,outfil,ciffil,instr,
     &                    page,chatter,status)

        implicit none
        character*(*) outfil,ciffil, taskversion,instr
        logical page
        integer status, chatter

C---------------------------------------------------------------------
C Description:  Opens the CIF and outfile (if not STDOUT) then
C               writes the full pathname of each calibration file
C               flagged as ONLINE and "good" to outfile
C
C Arguments:    outfil: Output file or STDOUT
C               ciffil: Calibration Index File to use
C               page:   Whether to page though STDOUT
C               status: Error status (0 if OK)
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C  Mike Tripicco (1.0.0: 95 July 24), original version
c  Ian M George  (2.0.0: 95 Dec 13) added wtinfo etc +
c  Ian M George  (2.0.1: 95 Dec 28) cosmetics
c  Lorraine Breedon (2.0.2: 97 Jan 19) concatenation of [#ext] onto filenames
C                                       + code to search only those files
C                                          for a given instrument alias
c  Lorraine Breedon (2.0.3: 98 Jun 23) replace gtdati and gttime with ftgstm
c  Jeff Guerber (2.0.4 1999-02-23) can't call ftgstm with a constant arg.
c

        character(7) version
        parameter (version = '2.0.4')
C---------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'dolsgd')
	character(1) chextno1,bracket1,bracket2
        character(2) chextno2
	character(4) cval, cval1, cval2
	character(20) caldbvar
        character(10) instval
	character(30) sjunk, online,errstr
        character(70) dirval
	character(160) file,contxt,calfile,calfile1,calfile2,calfile3,
     &                message
	integer iunit, ounit, ijunk, nax2val, fcstln, errstat
	integer devcol, dircol, filcol, qualcol, i, qual, pgfstat
	integer j, lastslash, ifound,xnocol,extno,lencal,instcol
        integer counter, timeref
	logical ljunk, outopen
        character(30) timestr

c Initialize
	caldbvar = 'CALDB'
	outopen = .false.
        file=' '
        calfile=' '
        calfile1=' '
        calfile2=' '
        calfile3=' '
	errstat = 0
	pgfstat = 0
	ifound = 0
        counter=0
        timeref = 0

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

         errstr = ' ** DOLSGD '//version//' ERROR: '

C Open the output (log) file
        call cgetlun(ounit)
        if (outfil(1:6) .ne. 'STDOUT') then
             call faopen (ounit, outfil, 2, 80, errstat)
             if (errstat .ne. 0) then
               call wterrm(subname, version,
     &          'problem opening output file')
               contxt = 'offending file: '//outfil
               call wtinfo(chatter,1,1, contxt)
               call wtinfo(chatter,1,2, 'does it already exist ?')
               status = 1
               go to 482
             else
               outopen = .true.
             endif
             call ftgstm(timestr,timeref,errstat)
             contxt = '# Results from lstgdq '//taskversion//
     &          ' run on '//timestr//' (system clock)'
             call pgfout(ounit,outopen,contxt,pgfstat)
             if (pgfstat .ne. 0) then
                     call ftclos(ounit,errstat)
                     call cfrelun(ounit)
                     goto 110
             endif
             contxt = '#..good quality files for '//instr//' in CIF: '//
     &          ciffil(:fcstln(ciffil))
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

        call ftgcno(iunit,.true.,'INSTRUME',instcol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding INSTRUME column in CIF')
            status = errstat
            goto 110
        endif



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

        call ftgcno(iunit,.true.,'CAL_XNO',xnocol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_XNO column in CIF')
            status = errstat
            goto 110
        endif


        call ftgcno(iunit,.true.,'CAL_QUAL',qualcol,errstat)
        if(errstat.ne.0)then
            call wtferr(subname, version, errstat,
     &          ' finding CAL_QUAL column in CIF')
            status = errstat
            goto 110
        endif

C --------------------- START OF DO-LOOP -----------------------
C Read each row and assemble full pathname and ext.num
	do 100 i=1, nax2val
	     errstat = 0
C ...........Get instrument value
	     call ftgcvs(iunit,instcol,i,1,1,' ',instval,ljunk,errstat)
             if(errstat.ne.0)then
                call wtferr(subname, version, errstat,
     &            ' reading INSTRUME column of CIF')
                write(cval1,'(I4)')i
                write(cval2,'(I4)')instcol
                contxt='error occurs in row = '//cval1//
     &                  ' column = '//cval2
                call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif


C ...........Get directory value
	     call ftgcvs(iunit,dircol,i,1,1,' ',dirval,ljunk,errstat)
             if(errstat.ne.0)then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_DIR column of CIF')
                write(cval1,'(I4)')i
                write(cval2,'(I4)')dircol
                contxt='error occurs in row = '//cval1//
     &                  ' column = '//cval2
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
     &                  ' column = '//cval2
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
200	     continue
	     if (lastslash .ne. 0) then
	       dirval = dirval(:fcstln(dirval)) //
     &           '/' // file(:lastslash-1)
	       file=file(lastslash+1:)
	     endif

C ...........Construct system dependent pathname (and put back in file)
	     call cpthnm(caldbvar,dirval,file,errstat)
             if (errstat .ne. 0) then
                contxt = 'unable to construct path name from '//
     &                  'values in CIF'
                call wterrm(subname, version, contxt)
                write(cval,'(I4)')i
                contxt='error occurs in row = '//cval
                call wtinfo(chatter,1,1,contxt)
                status = 2
                goto 110
             endif

C ...........Get Extension Number
             call ftgcvj(iunit,xnocol,i,1,1,0,extno,ljunk,errstat)
             if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_XNO column of CIF')
                write(cval1,'(I4)')i
                write(cval2,'(I4)')xnocol
                contxt='error occurs in row = '//cval1//
     &                  ' column = '//cval2
                call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C Concatenate [#ext] onto cal filename
             calfile=file
	     lencal=fcstln(calfile)
	     bracket1='['
             bracket2=']'
	     if (extno .lt. 10) then
                write (chextno1,fmt='(I1)') extno
                calfile1=calfile(1:lencal)//bracket1
                calfile2=calfile1(1:lencal+1)//chextno1
                calfile3=calfile2(1:lencal+2)//bracket2
             else
                write (chextno2,fmt='(I2)') extno
                if (extno .le. 99) then
                   calfile1=calfile(1:lencal)//bracket1
                   calfile2=calfile1(1:lencal+1)//chextno2
                   calfile3=calfile2(1:lencal+3)//bracket2
                else
                   message=errstr//calfile
	           call fcecho(message)
                   message='extension number greater than 99!'
	           call fcecho(message)
                   status=errstat
                   goto 110
                endif
	     endif

             file=calfile3


C ...........Get Quality Flag
             call ftgcvj(iunit,qualcol,i,1,1,0,qual,ljunk,errstat)
             if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_QUAL column of CIF')
                write(cval1,'(I4)')i
                write(cval2,'(I4)')qualcol
                contxt='error occurs in row = '//cval1//
     &                  ' column = '//cval2
                call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

C ...........Get online/offline value
             call ftgcvs(iunit,devcol,i,1,1,' ',online,ljunk,errstat)
             if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &            ' reading CAL_DEV column of CIF')
                write(cval1,'(I4)')i
                write(cval2,'(I4)')devcol
                contxt='error occurs in row = '//cval1//
     &                  ' column = '//cval2
                call wtinfo(chatter,1,1,contxt)
                status = errstat
                goto 110
             endif

	     if (online(1:6) .eq. 'ONLINE') then
	       ifound = ifound + 1
	       if (qual .eq. 0) then
                 if (instval(:fcstln(instval))
     &                   .eq.instr(:fcstln(instr))) then
                    counter=counter+1
	            contxt=file(:fcstln(file))
                    call pgfout (ounit, outopen, contxt, pgfstat)
                    if (pgfstat .ne. 0) goto 110
                 endif
	       endif
	     endif

100     continue
C --------------------- END OF DO-LOOP -----------------------

C       close the CIF
110	call ftclos(iunit,errstat)
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
482     if (status .ne. 0) then
          call wterrm(subname, version, ' Fatal - aborting')
        else
          write(contxt,'(i12,a)') counter,' Good entries found in CIF '
          contxt = contxt // ciffil(:fcstln(ciffil))
          call wtinfo(chatter,1,1,contxt)
        endif


	return
	end
