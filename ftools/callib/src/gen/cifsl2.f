*+CIFSL2
        subroutine cifsl2(chatter,cif,tele,instr,detnam,filt,codenam,
     &	startreftime, stopreftime, expr,maxret,file,extno,online,
     &	nfound,status)
     
	implicit none
        character*(*) cif, tele, instr, codenam, expr
        double Precision startreftime, stopreftime
	integer       maxret, nfound, chatter
        character*(*) detnam,filt
        character*(*) file(maxret),online(maxret)
        integer       extno(maxret), status
        
C
C Description: 
C  Opens the Calibration Index File specified in the CIF argument, and 
C searches the TELESCOP, INSTRUME, DETNAM, FILTER, and CAL_CNAM columns for 
C values matching those found in TELE, INSTR, DETNAM, FILT, and CODENAM 
C passed parameters.  If the DETNAM or FILT argument is passed as a '-' then
C the DETNAM and FILTER columns are not searched.  A value of 'NONE' in the 
C DETNAM or FILTER column will match any value in DETNAM or FILT.  
C The REF_TIME column is searched for the largest value which is also less 
C than the STARTREFTIME argument.  The EXPR string is parsed and formed into 
C Calibration boundary strings which are then compared to the CAL_CBD column 
C values.  If EXPR is '-', then the CAL_CBD column is not searched.
C
C When a row in the CIF is found that meets the above criteria, the values of 
C the CALDB environment variable (logical) the CAL_DIR and CAL_FILE columns
C are formed into a system dependent path.  This path points at the 
C calibration file having the selection characteristics specified above, and 
C is returned in the FILE argument.
C
C When a valid file is found, the CAL_DEV column value also gets recorded to 
C the ONLINE argument.
C
C If no rows are found matching the input arguments then a STATUS of 1 is 
C returned.
C
C Arguments:   
C chatter     	(i) : Chattiness flag
C cif         	(i) : the path and name of the calibration index file.
C tele    	(i) : the value of the TELESCOP column to be selected
C instr   	(i) : the value of the INSTRUME column to be selected
C detnam  	(i) : the value of the DETNAM column to be selected
C filt    	(i) : the value of the FILTER column to be selected
C codenam 	(i) : the value of the CAL_CNAM column to be selected
C startreftime 	(i) : the value which is compared to the REF_TIME column
C stopreftime 	(i) : the value which is compared to the REF_TIME column
C expr    	(i) : boolean expression used to search CAL_CBD column
C maxret  	(i) : the maximum number of entries to return 
C file    	(r) : an array containing the system dependent path to the 
C			calibration file
C extno   	(r) : an array containing the extension numbers of the 
C			calibration datasets found in the CAL_XNO column
C online  	(r) : array specifying on-line/off-line status of files 
C			in file array
C nfound  	(r) : the total number of entries which met the selection 
C			criteria
C status  (r) : success flag for this routine
C                            0 = everything is OK
C                            1 = no rows found matching input arguments
C                            2 = cannot open cif
C                            3 = problem reading CIF
C Origin:      
C  Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar (1.0.0:93 Apr 7), original version
C  Ron Zellar (2.0.0:94 May 31) Added detnam and filt arguments
C                               Enhanced if block selection so if detnam or 
C				filt = '-', then not used in selection.
C  Ron Zellar (3.0.0:94 Jul 18) Enhanced if block selection so if cif 
C				gives 'NONE' for DETNAM or FILTER then 
C				comparison is TRUE.  Added expr parser and
C                               comparison.  Modified file and extno 
C				arguments to be arrays. Added nfound argument
C                               and maxret argument, removed dev and dir 
C				arguments.
C  Ron Zellar (3.1.0:94 Jul 20) RENBAMED cifsel to cifsl
C  Ron Zellar (4.0.0:94 Aug  3) Added online argument
c  Ian George (5.0.0:96 Feb 01) Added CHATTER & STOPREFTIME as passed params
C				Removed QUIET as passed, plus cosmetics
c  Ian George (5.0.1:96 Jun 03) Initialized tmp_sum to 0 on entry
c  Lorraine Breedon (5.0.2:97 Mar 01) Increased size of temporary array
c  Mike Tripicco (5.0.3:98 Jan 16) Changed nulval arg in ftgcvd from 0 to 0.0d0
c  Peter D Wilson(5.0.4:00 Jun 26) Suppress file-not-found error
C                                  when chatter<0
C  Mike Corcoran (5.0.5 06-Jun-20) compare CBD values to expr only for
C                               those rows that match televal, insval,
C                               detval,filtval, codval and qulval
C
C MFC (5.1 June 2011) - now determine mission matching from CALDB.CONFIG file;
C   		remove matching mission string to TELESCOP in the 
C		caldb.indx file.  This makes it easier to handle mission
C 		aliases/renaming (GLAST vs. Fermi, for example). 
C		This requires that all the mission data are stored in  
C 		subdirectories of the main directory which stores the caldb.indx
C		file (for eg. all fermi data should be under
C		$CALDB/data/glast), as has been the convention
C		Added a warning if the TELESCOP value in the caldb.indx file
C		does not match the mission string if chatter >0
C                               
        character(7) version
        parameter (version = '5.1')

C---------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'cifsl2')
	character(4) cval1, cval2
        character(10) telval, insval, filtval
        character(20) codval, devval, detval, caldbvar
        character(70) dirval
        character(30) sjunk
        character(160) contxt,
     &                errstr
        integer unit, errstat, ijunk, i,j,nax2val, qulval
        integer telcol, inscol, codcol, refcol, qulcol, devcol, 
     &          dircol, filcol, extcol, tellen, inslen, codlen,
     &          fcstln, detcol, filtcol, detlen, filtlen,
     &          errstlen
	integer retcnt, cbdcol
        double precision refval, maxref
        logical ljunk,cbdres
	integer tmp_sum, tmp_max
c	parameter (tmp_max = 100)
	parameter (tmp_max = 500)
	integer tmp_row(tmp_max)
	double precision tmp_tim(tmp_max)
	logical tmp_log(tmp_max) 
        
C Set the name of the environment variable used in this routine
        caldbvar = 'CALDB'
         
C Initialize 
	tmp_sum = 0
        status  = 0
	errstat = 0
	nfound  = 0
	devval  = ' '
	dirval  = ' '
	retcnt  = 0
	errstlen = 0
	do j=1,maxret
	     file(j) = ' '
	enddo
        
c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

C open the CIF
        call cgetlun(unit)
        call ftopen(unit, cif, 0, ijunk, errstat)
        if(errstat.ne.0)then
           contxt = 'Unable to open CIF'
           if(chatter.ge.0) 
     &       call wtferr(subname, version, errstat, contxt)
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
        endif
        
C move to the first extension
        call ftmahd(unit, 2, ijunk, errstat)
        if(errstat.ne.0)then
           contxt = 'Problem reading CIF'
           call wtferr(subname, version, errstat,contxt)
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

C find out how many rows the CIF contains
        call ftgkyj(unit, 'NAXIS2', nax2val, sjunk, errstat)
        if(errstat.ne.0)then
           contxt = 'Problem reading NAXIS2 of CIF'
           call wtferr(subname, version, errstat,contxt)
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif
        if(nax2val.eq.0)then
	     contxt = 'CIF appears to be empty'
	     call wterrm(subname, version, contxt) 
             contxt = 'offending file: '//cif(:fcstln(cif))
             call wtinfo(chatter,1,1,contxt)
	     status = 1
	     goto 999
        endif
        
C get the column numbers of all the columns to be read

        call ftgcno(unit,.true.,'TELESCOP', telcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find TELESCOP column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'INSTRUME', inscol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find INSTRUME column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'DETNAM',   detcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find DETNAM column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'FILTER',filtcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find FILTER column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'CAL_CNAM', codcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_CNAM column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif
	
        call ftgcno(unit,.true.,'REF_TIME', refcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find REF_TIME column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif
	
        call ftgcno(unit,.true.,'CAL_QUAL', qulcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_QUAL column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'CAL_DEV',  devcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_DEV column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif
	
        call ftgcno(unit,.true.,'CAL_DIR',  dircol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_DIR column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'CAL_FILE', filcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_FILE column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'CAL_XNO',  extcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_XNO column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif

        call ftgcno(unit,.true.,'CAL_CBD',  cbdcol,errstat)
        if(errstat.ne.0)then
           call wtferr(subname, version, errstat,
     &		'Unable to find CAL_CBD column')
	   contxt = 'offending file: '//cif(:fcstln(cif))
	   call wtinfo(chatter,1,1,contxt)
           status = 2
           goto 999
	endif
        
	
C read each row and check against input arguments.  If row matches input 
C arguments then read returned values and write them to returned arguments.
        do 100 i=1, nax2val
            call ftgcvs(unit,telcol,i,1,1,' ',telval,ljunk,errstat)
	     if (errstat .ne. 0) then
           	call wtferr(subname, version, errstat,
     &			'Error reading CIF')
	   	contxt = 'filename: '//cif(:fcstln(cif))
	  	call wtinfo(chatter,1,1,contxt)
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')telcol
	        contxt='problematic row = '//cval1//', col = '//cval2
	   	call wtinfo(chatter,1,2,contxt)
           	status = 2
           	goto 999
	     endif

             call ftgcvs(unit,inscol,i,1,1,' ',insval,ljunk,errstat)
	     if (errstat .ne. 0) then
           	call wtferr(subname, version, errstat,
     &			'Error reading CIF')
	   	contxt = 'filename: '//cif(:fcstln(cif))
	  	call wtinfo(chatter,1,1,contxt)
	        write(cval1,'(I4)')i
	        write(cval2,'(I4)')inscol
	        contxt='problematic row = '//cval1//', col = '//cval2
	   	call wtinfo(chatter,1,2,contxt)
           	status = 2
           	goto 999
	     endif

C ... Since DETNAM is not required to make a unique selection it is removed 
c     from the selection criteria by making all detval values equal to the 
c     input value (a space '-').

             if (detnam .eq. '-') then
                  detval = '-'
             else
                  call ftgcvs(unit,detcol,i,1,1,' ',detval,
     &                 ljunk,errstat)
	          if (errstat .ne. 0) then
           		call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	   		contxt = 'filename: '//cif(:fcstln(cif))
	  		call wtinfo(chatter,1,1,contxt)
	        	write(cval1,'(I4)')i
	        	write(cval2,'(I4)')detcol
	        	contxt='problematic row = '//cval1//
     &				', col = '//cval2
	   		call wtinfo(chatter,1,2,contxt)
           		status = 2
           		goto 999
	          endif
             endif

C	As for DETNAM, so it is for FILTER...

	     if (filt .eq. '-' ) then
	          filtval = '-'
	     else
                  call ftgcvs(unit,filtcol,i,1,1,' ',filtval,
     &                 ljunk,errstat)
	          if (errstat .ne. 0) then
           		call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	   		contxt = 'filename: '//cif(:fcstln(cif))
	  		call wtinfo(chatter,1,1,contxt)
	        	write(cval1,'(I4)')i
	        	write(cval2,'(I4)')filtcol
	        	contxt='problematic row = '//cval1//
     &				', col = '//cval2
	   		call wtinfo(chatter,1,2,contxt)
           		status = 2
           		goto 999
	          endif
             endif
             
             call ftgcvs(unit,codcol,i,1,1,' ',codval,ljunk,errstat)
	     if (errstat .ne. 0) then
           		call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	   		contxt = 'filename: '//cif(:fcstln(cif))
	  		call wtinfo(chatter,1,1,contxt)
	        	write(cval1,'(I4)')i
	        	write(cval2,'(I4)')codcol
	        	contxt='problematic row = '//cval1//
     &				', col = '//cval2
	   		call wtinfo(chatter,1,2,contxt)
           		status = 2
           		goto 999
	     endif

             call ftgcvd(unit,refcol,i,1,1,0.0d0,refval,ljunk,errstat)
c     changed 0 to 0.0d0 in line above (mjt/16jan98)
	     if (errstat .ne. 0) then
           		call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	   		contxt = 'filename: '//cif(:fcstln(cif))
	  		call wtinfo(chatter,1,1,contxt)
	        	write(cval1,'(I4)')i
	        	write(cval2,'(I4)')refcol
	        	contxt='problematic row = '//cval1//
     &				', col = '//cval2
	   		call wtinfo(chatter,1,2,contxt)
           		status = 2
           		goto 999
	     endif

             call ftgcvj(unit,qulcol,i,1,1,0,qulval,ljunk,errstat)
	     if (errstat .ne. 0) then
           		call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	   		contxt = 'filename: '//cif(:fcstln(cif))
	  		call wtinfo(chatter,1,1,contxt)
	        	write(cval1,'(I4)')i
	        	write(cval2,'(I4)')qulcol
	        	contxt='problematic row = '//cval1//
     &				', col = '//cval2
	   		call wtinfo(chatter,1,2,contxt)
           		status = 2
           		goto 999
	     endif

             
C get the lengths of the strings read in
             tellen = fcstln(telval)
             inslen = fcstln(insval)
             codlen = fcstln(codval)
             filtlen= fcstln(filtval)
             detlen = fcstln(detval)

C MFC Addition 201106 version 5.1
C
C Prior to version 5.1: Because missions are often renamed 
C after launch, trying to match the TELESCOP name
C in the caldb.indx file will result in a mis-match if the post-launch mission 
C name is used (for eq. searching for Fermi data won't match files where
C TELESCOP='GLAST    '
C
C Version 5.1 change: The caldb for a given mission 
C is stored in bcf and cpf subdirectories
C of the directory where the caldb.indx file is located, so that the 
C mission data is effectively determined by the location of the caldb.indx file.
C So we REQUIRE that the specified mission name match the name of the directory 
C in which the caldb.indx is stored (as has been the convention)
C and therefore we don't need to require that the mission name string equal the 
C value of the TELESCOP keyword stored in the CALDB.
C However we should compare the mission string to the TELESCOP value and 
C warn the user of mismatches (which could indicate a problem in the 
C caldb.indx file)
C 
C  MFC
	      if(telval(:tellen).ne.tele) then 
     	       errstr='Mission name '//tele(:fcstln(tele))//' Does not'
     &         //' Match TELESCOP= '//telval(:tellen)
               errstr=errstr//telval(:tellen)
               contxt=errstr(:fcstln(errstr)) 
	       call wtwarm(subname, version, chatter, 1, contxt)
	       endif
C  MFC           if((telval(:tellen).eq.tele).and.
C  the read values match the input values read in the returned values
                if((insval(:inslen).eq.instr).and.
     &            ( (detval(:detlen).eq.detnam) .or.
     &	           (detval(:detlen).eq.'NONE') ).and.
     &              ( (filtval(:filtlen).eq.filt) .or.
     &	             (filtval(:filtlen).eq.'NONE') ).and.
     &                (codval(:codlen).eq.codenam).and.
     &                 (qulval.eq.0)) then
     
C Check to see if this row is valid for expr
	             if (expr.eq.'-') then
	                cbdres = .true.
	             else
	               call evexp2(chatter, unit,i,cbdcol,expr,
     &                         cbdres,errstat)
	               if (errstat .ne. 0) then
           	      	call wtferr(subname, version, errstat,
     &		      		'Error reading CIF')
	   	      	contxt = 'filename: '//cif(:fcstln(cif))
	  	      	call wtinfo(chatter,1,1,contxt)
	             	write(cval1,'(I4)')i
	             	write(cval2,'(I4)')cbdcol
	             	contxt='problematic row = '//cval1//
     &		      		', col = '//cval2
	   	      	call wtinfo(chatter,1,2,contxt)
           	      	status = 2
           	      	goto 999
	               endif 
	            endif 

              if (cbdres) then 

C ..... OK, this means we've found a calibration dataset of the requested 
C       type which satisfies all the limitations EXCEPT perhaps the date/time
C       Lob these into temporary arrays
		tmp_sum = tmp_sum + 1
		if(tmp_sum.gt.tmp_max) then
		   call wtwarm(subname,version,chatter,1,
     &			'Internal Array exceeded (param tmp_max)')
		   call wtinfo(chatter,1,1,
     &			'Some files may have been skipped')
	           contxt = 'Suggest more conservative '//
     &			'selection criteria'
		   call wtinfo(chatter,1,2,contxt)
		   goto 669		
		endif ! tmp_sum if
		tmp_row(tmp_sum) = i
		tmp_tim(tmp_sum) = refval
		tmp_log(tmp_sum) = .true.
             endif 
	   endif  
100	continue					
669	continue
C Now loop through the temp stuff twice, once finding the latest 
C validity start date/time BEFORE startreftime, then flagging the 
C earlier (unwanted) datasets to be ignored.
C If startreftime = -99, then we have no constraints in early files 
C and want all files starting at the year dot up until stopreftime
	maxref = 0.
	if(startreftime.EQ.-99) then
	   goto 666	
	else
	   do i = 1, tmp_sum
	     if((tmp_tim(i).le.startreftime).and.
     &	       (tmp_tim(i).gt.maxref)) maxref =  tmp_tim(i)
	   enddo
	   do i = 1, tmp_sum
	     if(tmp_tim(i).lt.maxref) tmp_log(i) = .false.
	   enddo
	endif

666	continue

c Now loop through flagging all datasets after the stopreftime
C If stopreftime = -99, then we have no constraints on late files
C and want all files after startreftime
	if(stopreftime.EQ.-99) then
	   goto 667	
	else
	   do i = 1, tmp_sum
	     if(tmp_tim(i).gt.stopreftime) tmp_log(i) = .false.
	   enddo
	endif

667	continue
c So now we're ready for the final pass through the CIF, reading all 
c the other keywords etc for the datasets within the requested time
c window.	
	do i = 1, tmp_sum
	   if(.NOT.tmp_log(i)) goto 668
	   nfound = nfound + 1
	   if (retcnt.ge.maxret) goto 668
	   retcnt = retcnt + 1
	            
C ........ Get directory value
    	   call ftgcvs(unit,dircol,tmp_row(i),1,1,' ',
     &                 dirval,ljunk,errstat)
	   if (errstat .ne. 0) then
              call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	      contxt = 'filename: '//cif(:fcstln(cif))
	      call wtinfo(chatter,1,1,contxt)
	      write(cval1,'(I4)')tmp_row(i)
	      write(cval2,'(I4)')dircol
	      contxt='problematic row = '//cval1//
     &				', col = '//cval2
	      call wtinfo(chatter,1,2,contxt)
              status = 2
              goto 999
	   endif

C ........ Get filename value
           call ftgcvs(unit,filcol,tmp_row(i),1,1,' ',
     &                 file(retcnt),ljunk,errstat)
	   if (errstat .ne. 0) then
              call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	      contxt = 'filename: '//cif(:fcstln(cif))
	      call wtinfo(chatter,1,1,contxt)
	      write(cval1,'(I4)')tmp_row(i)
	      write(cval2,'(I4)')filcol
	      contxt='problematic row = '//cval1//
     &				', col = '//cval2
	      call wtinfo(chatter,1,2,contxt)
              status = 2
              goto 999
	   endif

C ........ construct system dependent path
	   call cpthnm(caldbvar,dirval,file(retcnt),errstat)
	   if (errstat .ne. 0) then
              call wtferr(subname, version, errstat,
     &				'Error constructing path')
	      contxt = 'filename: '//cif(:fcstln(cif))
	      call wtinfo(chatter,1,1,contxt)
	      write(cval1,'(I4)')tmp_row(i)
	      write(cval2,'(I4)')detcol
	      contxt='problematic row = '//cval1
	      call wtinfo(chatter,1,2,contxt)
              status = 2
              goto 999
	   endif
	            
C ........ Get extension number 
 	   call ftgcvj(unit,extcol,tmp_row(i),1,1,0,
     &                 extno(retcnt),ljunk,errstat)
	   if (errstat .ne. 0) then
              call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	      contxt = 'filename: '//cif(:fcstln(cif))
	      call wtinfo(chatter,1,1,contxt)
	      write(cval1,'(I4)')tmp_row(i)
	      write(cval2,'(I4)')extcol
	      contxt='problematic row = '//cval1//
     &				', col = '//cval2
	      call wtinfo(chatter,1,2,contxt)
              status = 2
              goto 999
	   endif

C ........ Get onliine/offline value
           call ftgcvs(unit,devcol,tmp_row(i),1,1,' ',
     &                 online(retcnt),ljunk,errstat)
	   if (errstat .ne. 0) then
              call wtferr(subname, version, errstat,
     &				'Error reading CIF')
	      contxt = 'filename: '//cif(:fcstln(cif))
	      call wtinfo(chatter,1,1,contxt)
	      write(cval1,'(I4)')tmp_row(i)
	      write(cval2,'(I4)')devcol
	      contxt='problematic row = '//cval1//
     &				', col = '//cval2
	      call wtinfo(chatter,1,2,contxt)
              status = 2
              goto 999
	   endif
668	enddo

C if maxref is zero then no datasets matched the input values
C        if(maxref.eq.0)status=1	     



C Close the Index File
	errstat = 0
	call ftclos(unit,errstat)
	if (errstat .ne. 0) then
          call wtferr(subname, version, errstat,
     &				'Problem closing CIF')
	  status = 7
	  goto 999
	endif

C Free up the logical unit number I was using
	call cfrelun(unit)

c Final error checking
999     if ((errstat.ne.0).or.(status.gt.0)) then
          call wterrm(subname, version, ' Fatal - aborting')
        endif

        return
        end
