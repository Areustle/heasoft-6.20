
*+CALDBFLAG
      subroutine caldbg
      implicit none

C Description:  For each calibration file (in an input ASCII list), switches all 
C required flgs in the CIF to a requested value (provided file is listed in the CIF!)
C 
C                             ******NB*********
C required file format in ASCII list is path/filename[extension number]
C
C              
C Passed parameters : none
C
C User i/ps required (prompted for): none
C
C called routines : 
C  subroutine GPARAMS      : (CALLIB) gets parameters from caldflg.par file
C  subroutine CALDB_INFO   : (CALLIB) checks local CALDB available to user
C                                     for required mission/instrument 
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C  subroutine CHFLAGS      : (CALLIB) does the business
C 
C compilation & linking :
C  link with CALLIB & FITSIO
C
C Origin: Written for the Calibration Database.  
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Jul 5) original version
C  Lorraine Breedon (1.1.0:97 Jan 25) code for comparing filenames in ASCII 
C                                     list with those in the CIF adapted to 
C                                     account for file extensions
C  Lorraine Breedon (1.2.0:97 Jun 09) code to take into account the extra
C                                     CAL_ORIG column (if present).
C  Lorraine Breedon (1.3.0:97 Dec 12) include option to switch DETNAM value
C  Lorraine Breedon (1.4.0:98 Jun 23) y2k adaptions
C  Lorraine Breedon (1.4.1:98 Aug 28) replace ftdt2s with fttm2s


	character(7) version
        parameter (version = '1.4.1')
*-

C internals
        integer arrsz
        parameter (arrsz=3000)
	character(20) missn, instr,chextno,swit13,alias,swit14
	character(80) message,swit1,swit2,swit3,swit4
        character(8) swit8
        character(10) swit7
	character(160) contxt,calfexp,inpfil, junkdir,
     &                 lclfile(arrsz),lclf,swit12
        character(160) name(arrsz), compfile
        character(160) path(arrsz), finalf3, localf, finalf, finalf2,
     &                fext,fileandext(arrsz),caldbpth

        character(30) errstr,warnstr
	integer errstat,kunit,status,chatter,n,m,flen,flen1
	integer fcstln,nfiles,swit10,ext(arrsz),caldblen,
     &            extno,lastslash,dirstr,flen2,fl1,fl2
        logical there, flg1,flg2,flg3,flg4,flg7,flg8,
     &          flg10,flg12,bcfile,cpfile,flg13,flg14,origin
        
C initialise
	character(40) taskname
	common /task/ taskname

	taskname = 'CALDBFLAG '//version
	errstat = 0
	nfiles  = 0
        do 5 n=1,arrsz
              name(n)=' '
              lclfile(n)=' '
              path(n)=' '
              fileandext(n)=' '
              ext(n)=0
5       continue
	

	message = '** CALDBFLAG '//version
        call fcecho(message)
        errstr = '** CALDBFLAG '//version//' ERROR: '
        warnstr = '** CALDBFLAG '//version//' WARNING: '

C Get the parameters from the par file
	call gparams_flag(missn,instr,alias,inpfil,calfexp,flg1,swit1,
     &               flg2,swit2,flg3,swit3,flg4,swit4,
     &               flg7,swit7,flg8,swit8,
     &               flg10,swit10,flg12,swit12,bcfile,cpfile,
     &               origin,flg13,swit13,flg14,swit14,
     &               chatter,errstat)
    
	call ftupch(calfexp)

C If there's an error getting parameters then return
	if (errstat .ne. 0) return

C see if the user has set the environment variable specified in the 
C calfexp arguement
	call ctrlog(calfexp,fcstln(calfexp),caldbpth,caldblen)
	if (caldblen .eq. 0) then
	   contxt = 'Environment variable not set'
    	   message = errstr//contxt
	   call fcecho(message)
    	   message = 'The environment variable or logical "'
     &     //calfexp(:fcstln(calfexp))//'"'
	   call fcecho(message)
           message= 'must be set to point at the top of the Caldb'
           call fcecho(message)
           message= 'see the Caldb Users Guide for details'
           call fcecho(message)
	   errstat=2
	   return
	endif

C Open ASCII file 
	     call cgetlun(kunit)	     
	     open (kunit,file=inpfil,iostat=errstat,status='OLD')
    	     if (errstat .ne. 0) then
                inquire (file=inpfil,exist=there)
                   if (there .neqv. .true.) then
                     contxt = 'cant find ASCII file'
                     message = errstr//contxt
                     goto 110
		   endif  
		contxt = ' can find but cant open ASCII file '
    	        message = errstr//contxt
110             continue
	        call fcecho(message)
		status=20
		return
	     endif

100	continue

C get cal files from ASCII file
	   	     
	     read (kunit,'(A)',iostat=errstat,end=1000) lclf
	     if (errstat .ne. 0 ) then
	     	contxt = 'error reading input ASCII file '
	        message = errstr//contxt
	        call fcecho(message)
		status=errstat
	     	return
	     endif

C skip blank lines or lines beginning with '#' characters to obtain 
c calfilenames[#ext]
	     if ((lclf(1:1).ne.'#').and.(fcstln(lclf).ne.0)) then
	          nfiles = nfiles + 1
	          lclfile(nfiles) = lclf
c                  type *, nfiles, lclfile(nfiles)
             else
                  goto 100
	     endif
             goto 100
	
1000	continue
	
        if (nfiles .eq. 0) then
            contxt = 'input ASCII file empty !! '
	    message = errstr//contxt
	    call fcecho(message)
            status=-1
	    return
	endif

C close the ASCII file 

	close (kunit)
	call cfrelun(kunit)

C Get calibration filenames from special ASCII file

	do 1100 m=1,nfiles
                localf=lclfile(m)
C strip off [#ext], write filename (including path) to 'compfile'.
C get actual ext no and convert to integer
                flen=fcstln(localf)
                compfile=' '
                flen1=flen-1
                flen2=0
                fl1=0
                fl2=0
                do 1150 n=0,flen1
                          if (localf(flen-n:flen-n).eq.']') fl1=flen-n 
                          if (localf(flen-n:flen-n).eq.'[') fl2=flen-n-1
1150            continue
	        compfile=localf(1:fl2)
	        chextno=localf(fl2+2:fl1-1)
                if (fcstln(chextno) .eq. 1) then
                    read (chextno(1:1),fmt='(I1)') extno
	        else
                    read (chextno(1:2),fmt='(I2)') extno 
	        endif
	        ext(m)=extno                   
c	        type *, 'ext(m)=',ext(m)                

c create COMPLETE filename using 'compfile' info and the environment variable
C specified in the calfexp arguement (NB strip off $CALDB characters first)

C 		compf(m)=caldbpth(:fcstln(caldbpth))//localf(7:fl2)
c	        type *,'compf(m)=',compf(m) 

C now strip filenames of their paths, write the actual filename
C to 'finalf2' and the path to 'finalf3' (ignoring the initial $CALDB/
C characters).
C Then concatenate [#extn] onto filename
                finalf2=' '
                finalf3= ' '
                flen=fcstln(compfile)
                lastslash=0
                dirstr=0
                flen1=flen-1
                do 1200 n=0,flen1 
			finalf=compfile(flen-n:flen)
                        if (dirstr .eq. 1) then
c                           finalf3=compfile(8:flen2-1)
                            finalf3=compfile(1:flen2-1)

                        endif
                        if (compfile(flen-n:flen-n).eq.'/') then
                           lastslash=lastslash+1
                           dirstr=1
                           if (lastslash .eq. 1) then
                              flen2=flen-n
                              finalf2=compfile(flen-n+1:flen)
                              fext=localf(flen-n+1:fl1)
                           endif
                        endif
1200            continue 
	        name(m)=finalf2
	        path(m)=finalf3
	        fileandext(m)=fext
c	        type *, 'name=',name(m)
c                type *, 'path=',path(m)
c                type *, 'fext=',fileandext(m)

1100	continue

c now check that the calibration files exist.

	do 1300 m=1,nfiles
c	          inpfil=compf(m)
	          inpfil=compfile
                  inquire (file=inpfil,exist=there)
                  if (.not.there) then
c                  if (there .neqv. .true.) then
                     contxt = 'cant find '//inpfil
                     message = warnstr//contxt
	             call fcecho(message)
                  endif
1300	continue

C get the caldb.config file

	if ((calfexp(1:5).eq.'caldb').or.(calfexp(1:5).eq.'CALDB')) then
	    call caldb_info(chatter,'INST',missn,alias,errstat)
	    if (errstat.ne.0) then
               message='CALDB not defined/available'
               call fcecho(message)
	       message='task requires CALDB to be both defined '//
     &             '& available in order to run'
               call wt_ferrmsg(errstat,message)
	       goto 999
	    endif
	    errstat=0
C read config file to determine path to apprpriate CIF
	    call rdcnfg(missn,alias,.false.,calfexp,junkdir,errstat)

            if (errstat .ne. 0) return
        endif

	
C before passing switch arguements, convert to uppercase those which havent 
C been already
       
	call ftupch(swit7)
	call ftupch(swit8)
	call ftupch(swit13)
	


C now do the business	

        call chflgs(calfexp,arrsz,lclfile,path,name,
     &               fileandext,nfiles,flg1,swit1,
     &               flg2,swit2,flg3,swit3,flg4,swit4,
     &               flg7,swit7,flg8,swit8,
     &               flg10,swit10,flg12,swit12,bcfile,cpfile,
     &               flg13,swit13,flg14,swit14,chatter,errstat)

 
	
	message=' '
        call fcecho(message)
	message = '** CALDBFLAG '//version//' finished **'
        call fcecho(message)

	return
999	continue
	end

C----------End of CALDBG subroutine-----------------------------------

C-----------------------------------------------------------------------

*+GPARAMS_FLAG
	subroutine gparams_flag(missn,instr,alias,inpfil,calfexp,flg1,
     &               swit1,flg2,swit2,flg3,swit3,flg4,swit4,
     &               flg7,swit7,flg8,swit8,
     &               flg10,swit10,flg12,swit12,bcfile,cpfile,
     &               origin,flg13,swit13,flg14,swit14,chatter,
     &               status)

	implicit none
        character*(*) missn,instr,alias, inpfil,calfexp,
     &                swit1,swit2,swit3,swit4,swit7,
     &                swit8,swit12,swit13,swit14
	integer status,chatter,swit10
        logical flg1,flg2,flg3,flg4,flg7,flg8,
     &          flg10,flg12,bcfile,cpfile,flg13,flg14,origin


C Description:  
C  Gets the parameters from the parameter 
C  file.  If calfexp param is default ("CALDB") then ask 
C  for and use the mission/instrument params (plus 
C  environment variables) to specify the caldb.config file. 

C
C passed parameters:
C  CALFEXP    :    value of environment variable
C  STATUS     :    error flg (0=OK)
C
C user i/ps (prompted for):
C  MISSN      :   the value of the 'mission' param
C  INSTR      :   the value of the 'instrument 'param
C  ALIAS      :   the value of the 'instrument alias ' param
C  INPFIL     :   the name of the input ASCII file containing list of 
C                 calibration files whose CIF flg status is to be changed 
C  FLG1-14    :   the name of the required CIF flg to change
C  SWIT1-14   :   the new flg status
C  BCFILE     :   logical defining cal file as a Basic Calibration File
C  CPFILE     :   logical defining cal file as a Calibration Product File
C  ORIGIN     :   logical to determine whether CAL_ORIGIN column present in CIF
C 
C Called routines : 
C  subroutine UGLGST       : (XPI) gets parameter values from CALDBFLAG.par
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C       
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Jul 5) original version
C  Lorraine Breedon (1.1.0:98 Jun 23) y2k adaptions



        character(7) version
        parameter (version='1.1.0')
*-
C Internals

        character(50) contxt
        character(80) message
        integer errstat

C initialise
C Set Status flg to 'no problem!'
        status = 0
        errstat = 0

C Get calfexp parameter
        call uclgst('calfexp', calfexp, errstat)
C If there's an error getting calfexp, return
	if(errstat.ne.0) then
             contxt = 'cant get calfexp parameter'	     
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

	

	if ((calfexp(1:5) .eq. 'CALDB').or.(calfexp(1:5) .eq. 'caldb'))
     &       then
C Get mission parameter
             call uclgst('mission', missn, errstat)
C If there's an error getting mission, return
             if(errstat.ne.0)then
                contxt = 'cant get mission parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif

C Get instrument parameter
             call uclgst('instrument', instr, errstat)
C If there's an error getting instrument, return
             if(errstat.ne.0)then
                contxt = 'cant get instrument parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)               
                status = 1
                return
             endif

C Get alias parameter
             call uclgst('alias', alias, errstat)
C If there's an error getting instrument alias, return
             if(errstat.ne.0)then
                contxt = 'cant get instrument alias parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)               
                status = 1
                return
             endif


        endif


C Get inpfile parameter
        call uclgst('inpfile', inpfil, errstat)
C If there's an error getting inpfile, return
        if(errstat.ne.0)then
           contxt = 'cant get inpfile parameter'
	   message = '**UCLGST '//version//' ERROR : '//contxt
	   call fcecho(message)             
           status = 1
           return
        endif

C Get the chatter parameter
        call uclgsi('chatter', chatter, errstat)
	 if(errstat.ne.0)then
             contxt = 'cant get chatter parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
             call fcecho(message)
	     chatter=9	
	     message = ' setting chatter=9'
	     call fcecho(message)
             status = 0
        endif
	

C Get the flag (and switch) parameters.......

C Get flg1 parameter
	
        call uclgsb('flag1', flg1, errstat)
C If there's an error getting flg1 return
        if(errstat.ne.0)then
             contxt = 'cant get flg1 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg1 .eqv. .true.) then 
C Get switch1 parameter
             call uclgst('switch1', swit1, errstat)
C If there's an error getting switch1, return
             if(errstat.ne.0)then
                contxt = 'cant get switch1 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
C Check that switch1 entry is OGIP-approved
	     call ftupch(swit1) 
	     call caldev(swit1,chatter,errstat)
             if (errstat.ne.0) then
                status=1
                return
             endif
          endif

C Get flg2 parameter
        call uclgsb('flag2', flg2, errstat)
C If there's an error getting flg2 return
        if(errstat.ne.0)then
             contxt = 'cant get flg2 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg2 .eqv. .true.) then 
C Get switch2 parameter
             call uclgst('switch2', swit2, errstat)
C If there's an error getting switch2, return
             if(errstat.ne.0)then
                contxt = 'cant get switch2 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
C Check that switch2 entry is OGIP-approved
	     call ftupch(swit2) 
	     call calclas(swit2,chatter,errstat)
             if (errstat.ne.0) then
                status=1
                return
             endif

          endif

C Get flg3 parameter
        call uclgsb('flag3', flg3, errstat)
C If there's an error getting flg3 return
        if(errstat.ne.0)then
             contxt = 'cant get flg3 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg3 .eqv. .true.) then 
C Get switch3 parameter
             call uclgst('switch3', swit3, errstat)
C If there's an error getting switch3, return
             if(errstat.ne.0)then
                contxt = 'cant get switch3 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
C Check that switch3 entry is OGIP-approved
	     call ftupch(swit3) 
	     call caldtyp(swit3,chatter,errstat)
             if (errstat.ne.0) then
                status=1
                return
             endif

          endif

C Get flg4 parameter
        call uclgsb('flag4', flg4, errstat)
C If there's an error getting flg4 return
        if(errstat.ne.0)then
             contxt = 'cant get flg4 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg4 .eqv. .true.) then 
C Get switch4 parameter
             call uclgst('switch4', swit4, errstat)
C If there's an error getting switch4, return
             if(errstat.ne.0)then
                contxt = 'cant get switch4 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
C Check that switch4 entry is OGIP-approved
	     call ftupch(swit4) 
	     call calcnam(swit4,chatter,bcfile,cpfile,errstat)
             if (errstat.ne.0) then
                status=1
                return
             endif

          endif


C Get flg7 parameter
        call uclgsb('flag7', flg7, errstat)
C If there's an error getting flg7 return
        if(errstat.ne.0)then
             contxt = 'cant get flg7 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg7 .eqv. .true.) then 
C Get switch7 parameter
10             call uclgst('switch7', swit7, errstat)
C If there's an error getting switch7, return
             if(errstat.ne.0)then
                contxt = 'cant get switch7 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
         
C check that input date is in correct yyyy-mm-dd format
             if (swit7(5:5) .ne. '-') then
                contxt = 'input date is not in yyyy-mm-dd format '
	        message = '**UCLGST '//version//' WARNING : '//contxt
	        call fcecho(message)
                message = '......please enter again '
                call fcecho(message)
                goto 10
              endif
         endif

C Get flg8 parameter
        call uclgsb('flag8', flg8, errstat)
C If there's an error getting flg8 return
        if(errstat.ne.0)then
             contxt = 'cant get flg8 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg8 .eqv. .true.) then 
C Get switch8 parameter
             call uclgst('switch8', swit8, errstat)
C If there's an error getting switch8, return
             if(errstat.ne.0)then
                contxt = 'cant get switch8 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
          endif


C Get flg10 parameter
        call uclgsb('flag10', flg10, errstat)
C If there's an error getting flg10 return
        if(errstat.ne.0)then
             contxt = 'cant get flg10 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
         if (flg10 .eqv. .true.) then 
C Get switch10 parameter
             call uclgsi('switch10', swit10, errstat)
C If there's an error getting switch10, return
             if(errstat.ne.0)then
                contxt = 'cant get switch10 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
C Check that switch10 entry is OGIP-approved
	     call calqual(swit10,chatter,errstat)
             if (errstat.ne.0) then
                status=1
                return
             endif

          endif

	
C Get flg12 parameter
        call uclgsb('flag12', flg12, errstat)
C If there's an error getting flg12 return
        if(errstat.ne.0)then
             contxt = 'cant get flg12 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
        if (flg12 .eqv. .true.) then 
C Get switch12 parameter
             call uclgst('switch12', swit12, errstat)
C If there's an error getting switch12, return
             if(errstat.ne.0)then
                contxt = 'cant get switch12 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
        endif

C Get origin parameter
        call uclgsb('origin', origin, errstat)
C If there's an error getting origin return
        if(errstat.ne.0)then
             contxt = 'cant get origin parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif

        if (origin .eqv. .true.) then 
C CIF possesses a CAL_ORIGIN column..therefore user may change the value
C in this column
  
C Get flg13 parameter
              call uclgsb('flag13', flg13, errstat)
C If there's an error getting flg13 return
              if(errstat.ne.0)then
                contxt = 'cant get flg13 parameter'
	         message = '**UCLGST '//version//' ERROR : '//contxt
	         call fcecho(message)
                 status = 1
                 return
              endif
              if (flg13 .eqv. .true.) then 
C Get switch12 parameter
                 call uclgst('switch13', swit13, errstat)
C If there's an error getting switch13, return
                 if(errstat.ne.0)then
                   contxt = 'cant get switch13 parameter'
	           message = '**UCLGST '//version//' ERROR : '//contxt
	           call fcecho(message)
                   status = 1
                   return
                endif
             endif
	endif 
  
C Get flg14 parameter
        call uclgsb('flag14', flg14, errstat)
C If there's an error getting flg14 return
        if(errstat.ne.0)then
             contxt = 'cant get flg14 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
            status = 1
            return
         endif
        if (flg14 .eqv. .true.) then 
C Get switch12 parameter
             call uclgst('switch14', swit14, errstat)
C If there's an error getting switch14, return
             if(errstat.ne.0)then
                contxt = 'cant get switch14 parameter'
	        message = '**UCLGST '//version//' ERROR : '//contxt
	        call fcecho(message)
                status = 1
                return
             endif
        endif



        return
        end

C-----------------------end of GPARAMS subroutine----------------------

C---------------------------------------------------------------------
*+CHFLAGS
        subroutine chflgs(calfexp,arrsz,lclfile,path,
     &               name,fileandext,nfiles,flg1,swit1,
     &               flg2,swit2,flg3,swit3,flg4,swit4,
     &               flg7,swit7,flg8,swit8,
     &               flg10,swit10,flg12,swit12,bc_file,cp_file,
     &               flg13,swit13,flg14,swit14,chatter,status)

        implicit none
        integer arrsz
        character*(*) calfexp,lclfile(arrsz),path(arrsz),
     &                name(arrsz),fileandext(arrsz), 
     &                swit1,swit2,swit3,swit4,swit7,
     &                swit8,swit12,swit13,swit14
        
	integer status,chatter,nfiles,swit10
        logical flg1,flg2,flg3,flg4,flg7,flg8,
     &          flg10,flg12,bc_file,cp_file,flg13,flg14
                  
C Description:  
C  Opens the CIF (appropriate for the given mission and instrument),
C  and for a given calibration file (filename[extn]) listed in both the CIF and the input
C  ASCII file, the required CIF flgs are switched to the requested values 

C passed parameters:
C  CALFEXP    :    value of environment variable
C  STATUS     :    error flg (0=OK)
C  LCLFILE    :    the full name of each calibration file (path/filename[extn]) in input 
C                  ASCII list
C  PATH       :    the path to each calibration file (given in ASCII list)
C  NAME       :    the name only, of each calibration file
C  FILEANDEXT :    the name[extn] of each calibration file
C  NFILES     :    the number of calibration files in the input ASCII file
C  FLG1-14    :    the name of the required CIF flg to change
C  SWIT1-14   :    the new flg status
C  BC_FILE     :   logical defining cal file as a Basic Calibration File
C  CP_FILE     :   logical defining cal file as a Calibration Product File
C   
C
C user i/ps (prompted for):
C none
C
C Called routines : 
C  subroutine UGLGST       : (XPI) gets parameter values from CALDBFLAG.par
C  subroutine FCECHO       : (FITSIO) write to STDOUT
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages if necessary
C  subroutine FTOPEN       : (FITSIO) opens CIF
C  subroutine FTMAHD       : (FITSIO) moves to 1st extension in FITS file
C  subroutine FTGKYJ       : (FITSIO) determines no. rows in CIF
C  subroutine FTGCNO       : (FITSIO) gets column nos of all columns to be read
C  subroutine FTGCVS       : (FITSIO) gets cal file name from CIF 
C  subroutine FTPCLS       : (FITSIO) puts flg status into 
C                                     appropriate column of bin table in CIF
C compilation & linking :
C  link with XPI, FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.  
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Jul 5) original version
	character(7) version
        parameter (version = '1.0.0')


*-
C Internals
	character(1) chextno1,bracket1,bracket2
        character(2) chextno2
        character(20) cal_orig,cal_det
	character(30) sjunk,errstr,wrnstr
        character(80) online, cal_clas, cal_dtyp, cal_cnam
        character(68) cal_vsdd
	character(10) cal_vsd
        character(8) cal_vst
	character(160) file,calif, calif1,calif2,calif3,
     &           direct, cifline,
     &           cal_desc,current,fle,pth,message
        integer iyear,imon,iday,ihour,imin,decimals
        integer iunit, ijunk, nax2val,fcstln,errstat,stat
	integer devcol,dircol,filcol,clascol,dtypcol,cnamcol,
     &          vsdcol,vstcol,reftcol,qualcol,desccol,origcol
	integer i,m,flen,nn,len,exist(3000),mm,lencal,
     &          flen2,len1, cal_qual,extcol,extno,detcol
	double precision swit9,second
c        double precision ref_time
	logical ljunk

C initialise and set up defaults
	errstat = 0
        status=0
	flen=0
        len=0
        flen2=0
        calif=' '
        calif1=' '
        calif2=' '
        calif3=' '        
        do 25 m=1,arrsz
                exist(m)=0
25      continue
        i=0
        m=0
        iyear=0
        imon=0
        iday=0
        ihour=0
        imin=0
        second=0.0
        decimals=0



        errstr = '** CHFLAGS '//version//'ERROR: '
        wrnstr = '** CHFLAGS '//version//'WARNING: '

 

C Open the CIF
        call cgetlun(iunit)
        call ftopen(iunit, calfexp, 1, ijunk, errstat)
        if(errstat.ne.0)then
           message=errstr// ' opening the CIF'
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif
     
C Move to the first extension
        call ftmahd(iunit, 2, ijunk, errstat)
        if(errstat.ne.0)then
            message=errstr// ' moving to 1st extension'
	    call wt_ferrmsg(errstat,message)
	    status = errstat
	    return
	endif

C Find out how many rows the CIF contains
        call ftgkyj(iunit, 'NAXIS2', nax2val, sjunk, errstat)
        if(nax2val.eq.0)then
             status = errstat
             return
        endif
        
C Get the column numbers of all the columns to be read
	call ftgcno(iunit,.true.,'CAL_FILE',filcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_FILE column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif

	call ftgcno(iunit,.true.,'CAL_XNO',extcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_XNO column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif


	if (flg1 .eqv. .true.) then
            call ftgcno(iunit,.true.,'CAL_DEV',devcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_DEV column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	
	    call ftgcno(iunit,.true.,'CAL_DIR',dircol,errstat)
                 if(errstat.ne.0)then
                   message=errstr// ' getting CAL_DIR column number'
	           call wt_ferrmsg(errstat,message)
	           status = errstat
	           return
	    endif
	endif

	if ((flg2 .eqv. .true.) .or. (flg4 .eqv. .true.)) then
            call ftgcno(iunit,.true.,'CAL_CLAS',clascol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_CLAS column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
            call ftgcno(iunit,.true.,'CAL_CNAM',cnamcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_CNAM column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif

	if (flg3 .eqv. .true.) then
            call ftgcno(iunit,.true.,'CAL_DTYP',dtypcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_DTYP column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif

	
	if ((flg7 .eqv. .true.) .or. (flg8 .eqv. .true.))  then
            call ftgcno(iunit,.true.,'CAL_VSD',vsdcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_VSD column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	

	    call ftgcno(iunit,.true.,'CAL_VST',vstcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_VST column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	

	    call ftgcno(iunit,.true.,'REF_TIME',reftcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting REF_TIME column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif

	if (flg10 .eqv. .true.) then
            call ftgcno(iunit,.true.,'CAL_QUAL',qualcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_QUAL column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif


	if (flg12 .eqv. .true.) then
            call ftgcno(iunit,.true.,'CAL_DESC',desccol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_DESC column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif

	if (flg13 .eqv. .true.) then
            call ftgcno(iunit,.true.,'CAL_ORIG',origcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting CAL_ORIG column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif

	if (flg14 .eqv. .true.) then
            call ftgcno(iunit,.true.,'DETNAM',detcol,errstat)
            if(errstat.ne.0)then
               message=errstr// ' getting DETNAM column number'
	       call wt_ferrmsg(errstat,message)
	       status = errstat
	       return
	    endif
	endif


C Read each row and assemble full pathname and ext.num
	do 100 i=1,nax2val
	   errstat = 0

		
C Get filename value
	   call ftgcvs(iunit,filcol,i,1,1,' ',file,ljunk,errstat)
	   if (errstat .ne. 0) then
	       message=errstr// ' getting cal file name '
	       call wt_ferrmsg(errstat,message)
               status = errstat
               return
	   endif

C Get extension no  value
	   call ftgcvj(iunit,extcol,i,1,1,0,extno,ljunk,errstat)
	   if (errstat .ne. 0) then
	       message=errstr// ' getting extension number '
	       call wt_ferrmsg(errstat,message)
               status = errstat
               return
	   endif

	
C Get cal filename in the CIF independant of path
           len=fcstln(file)
           len1=len-1
    	   do 120 nn=0,len1
	          if (file(len-nn:len-nn).eq.'/') goto 150
                  calif=file(len-nn:len)
120           continue
		
150        continue

C Concatenate [#ext] onto cal filename
	   lencal=fcstln(calif)
	   bracket1='['
           bracket2=']'
	   if (extno .lt. 10) then
              write (chextno1,fmt='(I1)') extno
              calif1=calif(1:lencal)//bracket1
              calif2=calif1(1:lencal+1)//chextno1
              calif3=calif2(1:lencal+2)//bracket2
           else
              write (chextno2,fmt='(I2)') extno
              if (extno .le. 99) then
                 calif1=calif(1:lencal)//bracket1
                 calif2=calif1(1:lencal+1)//chextno2
                 calif3=calif2(1:lencal+3)//bracket2
              else
                 message=errstr//calif
	         call fcecho(message)
                 message='extension number greater than 99!'
	         call fcecho(message)
                 status=1
                 return
              endif
	   endif

	   calif = calif3

              
     

c Compare the cal filename in the CIF with those in the input ASCII file
C If filenames match, switch the appropriate flg to the required value
C....but warn if flg value in CIF is already equal to the required value
           m=0
           do 200 m=1,nfiles
	      if (calif .eq. fileandext(m)) then
                 cifline=fileandext(m)
C If filenames match, cal file in ASCII list obviously exists in CIF !
		 exist(m)=1
	         if (flg2 .eqv. .true.) then
C Get the old and write the new CAL_CLAS value in CIF
                    call ftgcvs(iunit,clascol,i,1,1,
     &                           ' ',cal_clas,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_CLAS flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_clas(1:3) .eq. swit2(1:3)) then
                       message=wrnstr//' cal file: '// cifline
	               call fcecho(message)
		       message='CAL_CLAS in CIF already at input value!'
		       call fcecho(message)
	            endif
                    call ftpcls(iunit,clascol,i,1,1,swit2,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr//' writing CAL_CLAS to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
	         endif

	         if (flg3 .eqv. .true.) then
C Get the old and write the new CAL_DTYP value in CIF
                    call ftgcvs(iunit,dtypcol,i,1,1,
     &                         ' ',cal_dtyp,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_DTYP flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_dtyp(1:3) .eq. swit3(1:3)) then
                       message=wrnstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='CAL_DTYP in CIF already at input value!'
		       call fcecho(message)
	            endif
	                  
	            call ftpcls(iunit,dtypcol,i,1,1,swit3,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing CAL_DTYP to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif

	         if (flg4 .eqv. .true.) then
C Get the old and write the new CAL_CNAM value in CIF
C But first get CAL_CLAS value in CIF also and check wether new CAL_CNAM entry is 
C appropriate for that class
                   call ftgcvs(iunit,cnamcol,i,1,1,
     &					' ',cal_cnam,ljunk,errstat)
	           if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_CNAM flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	           endif

                         
                   if (cal_cnam(1:20) .eq. swit4(1:20)) then
                       message=wrnstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='CAL_CNAM in CIF already at input value!'
		       call fcecho(message)
	           endif
	                  
                   call ftgcvs(iunit,clascol,i,1,1,
     &                           ' ',cal_clas,ljunk,errstat)
	           if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_CLAS flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	           endif

	           if ((bc_file .eqv. .false.)
     &                      .and. (cp_file .eqv. .true.)
     &                      .and. (cal_clas(1:3) .eq. 'BCF')) then 
C CAL_CNAM entry is present in table7 of the memo CAL/GEN/92-011
                       message=errstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='new CAL_CNAM entry is NOT appropriate'
     	               call fcecho(message)
	  	       message=
     &                   'for the OGIP class ..BCF.. of this cal file'
                       call fcecho(message)
                       message= 'see memo (CAL/GEN/92-011)'
                       call fcecho(message)
	               status=1
                       return
                   endif

	           if ((bc_file .eqv. .true.)
     &                .and.(cp_file .eqv. .false.)
     &                .and. (cal_clas(1:3) .eq. 'CPF')) then 
C CAL_CNAM entry is present in either of tables 3-5 of the memo CAL/GEN/92-011
                       message=errstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='new CAL_CNAM entry is NOT appropriate'
     	               call fcecho(message)
	  	       message=
     &                   'for the OGIP class ..CPF.. of this cal file'
                       call fcecho(message)
                       message= 'see memo (CAL/GEN/92-011)'
                       call fcecho(message)
	               status=1
                       return
                   endif

	           call ftpcls(iunit,cnamcol,i,1,1,swit4,errstat)
	     	   if (errstat .ne. 0) then
	       	      message=errstr// ' writing CAL_CNAM to CIF '
	              call wt_ferrmsg(errstat,message)
                      status = errstat
	              return
	           endif
                 endif

	         if ((flg7 .eqv. .true.) .or. (flg8 .eqv. .true.)) then
C Get the old and write the new CAL_VSD value in CIF
                    call ftgcvs(iunit,vsdcol,i,1,1,
     &                         ' ',cal_vsd,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_VSD flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif


C ensure that old CAL_VSD is converetd to yyyy-mm-dd format, if not already
                    if (fcstln(cal_vsd) .ne. 10) then

                     call fts2dt(cal_vsd,iyear,imon,iday,errstat)
                     if (errstat .ne. 0) then
                          message='problem in fts2dt '
                          call fcecho(message)
                          return
                     endif


                     call fttm2s(iyear,imon,iday,ihour,
     &                    imin,second,decimals,cal_vsdd,errstat)
                     if (errstat .ne. 0) then
                        message='problem in fttm2s '
                       call fcecho(message)
                       return
                     endif

                    endif
         
                    cal_vsd=cal_vsdd(1:10)


 

	            call ftpcls(iunit,vsdcol,i,1,1,swit7,errstat)
	     	    if (errstat .ne. 0) then
	       	          message=errstr// ' writing CAL_VSD to CIF '
	                  call wt_ferrmsg(errstat,message)
                          status = errstat
	                  return
	            endif
                         

C Get the old and write the new CAL_VST value in CIF
                    call ftgcvs(iunit,vstcol,i,1,1,
     &                               ' ',cal_vst,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_VST flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif

	            if (flg8 .eqv. .true.) then
                       if (cal_vst(1:8) .eq. swit8(1:8)) then
                         message=wrnstr// 'cal file: '//cifline
	                 call fcecho(message)
			 message=
     &                     'CAL_VST in CIF already at input value!'
			 call fcecho(message)
	               endif
	                  
	               call ftpcls(iunit,vstcol,i,1,1,swit8,errstat)
	     	       if (errstat .ne. 0) then
	       	          message=errstr//' writing CAL_VST to CIF '
	                  call wt_ferrmsg(errstat,message)
                          status = errstat
	                  return
	               endif
	            endif

C write the new REF_TIME value in CIF
c                    call ftgcvd(iunit,reftcol,i,1,1,
c     &                             0,ref_time,ljunk,errstat)
c	            if (errstat .ne. 0) then
c	               message=errstr// ' getting REF_TIME flg in CIF '
c	               call wt_ferrmsg(errstat,message)
c                       status = errstat
c	               return
c	            endif
		    if (flg8 .eqv. .false.) swit8=cal_vst
	            if (flg7 .eqv. .false.) swit7=cal_vsd

c		          type *, 'reftime in CIF=',ref_time
C Calculate the reference time (from swit7 & swit8) for input into the REF_TIME column
c	                  type *, '** entering calcrt **'
C REMEMBER - the new calcrt takes input date as yyyy-mm-dd
	            call calcrt(swit7,swit8,swit9,errstat)
		    if (errstat .ne. 0) then
	     	       message=errstr//
     &                   ' Error calculating REF_TIME value'
    	     	       call fcecho(message)
                       message='Check CAL_VSD and CAL_VST entries'
                       call fcecho(message)
	               status = errstat
	               return
	            endif
 


	                  
	            call ftpcld(iunit,reftcol,i,1,1,swit9,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing REF_TIME to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif
	         if (flg10 .eqv. .true.) then
C Get the old and write the new CAL_QUAL value in CIF
                    call ftgcvj(iunit,qualcol,i,1,1,
     &                           0,cal_qual,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_QUAL flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_qual .eq. swit10) then
                       message=wrnstr//' cal file: '//cifline 
		       call fcecho(message)
		       message='CAL_QUAL in CIF already at input value!'
		       call fcecho(message)
	            endif

	            call ftpclj(iunit,qualcol,i,1,1,swit10,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing CAL_QUAL to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif
	         if (flg12 .eqv. .true.) then
C Get the old and write the new CAL_DESC value in CIF
                    call ftgcvs(iunit,desccol,i,1,1,
     &                           ' ',cal_desc,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_DESC flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_desc(1:45) .eq. swit12(1:45)) then
                       message=wrnstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='CAL_DESC in CIF already at input value!'
		       call fcecho(message)
	            endif
	            call ftpcls(iunit,desccol,i,1,1,swit12,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing CAL_DESC to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif
	         if (flg13 .eqv. .true.) then
C Get the old and write the new CAL_ORIG value in CIF
                    call ftgcvs(iunit,origcol,i,1,1,
     &                           ' ',cal_orig,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_ORIG flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_orig(1:7) .eq. swit13(1:7)) then
                       message=wrnstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='CAL_ORIG in CIF already at input value!'
		       call fcecho(message)
	            endif
	            call ftpcls(iunit,origcol,i,1,1,swit13,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing CAL_ORIG to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif

	         if (flg14 .eqv. .true.) then

C Get the old and write the new DETNAM value in CIF
                    call ftgcvs(iunit,detcol,i,1,1,
     &                           ' ',cal_det,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting DETNAM flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
		    if (cal_det(1:20) .eq. swit14(1:20)) then
                       message=wrnstr// 'cal file: '//cifline
	               call fcecho(message)
		       message='DETNAM in CIF already at input value!'
		       call fcecho(message)
	            endif
	            call ftpcls(iunit,detcol,i,1,1,swit14,errstat)
	     	    if (errstat .ne. 0) then
	       	       message=errstr// ' writing DETNAM to CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif
                 endif


                 if (flg1 .eqv. .true.) then

C************************************************************************************
C                    ***** loads more work here!! *******
C************************************************************************************
C get current working directory 
	                  
	            call gtdir(current)
c	            type *, '*** cwd *** is ',current
	                  
C Get online/offline value in CIF
                    call ftgcvs(iunit,devcol,i,1,1,
     &                               ' ',online,ljunk,errstat)
	            if (errstat .ne. 0) then
	               message=errstr// ' getting CAL_DEV flg in CIF '
	               call wt_ferrmsg(errstat,message)
                       status = errstat
	               return
	            endif

C If On,off line status requested for a cal file in ASCII list is DIFFERENT to
C the value in the CIF, then do the stuff!
                    if (swit1 .ne. online(1:7)) then

C write cal filename (independant of path) to the CIF
                       call ftpcls(iunit,filcol,i,1,1,
     &                                name(m),errstat)
	     	       if (errstat .ne. 0) then
	       	          message=errstr//' writing cal filename to CIF'
	                  call wt_ferrmsg(errstat,message)
                          status = errstat
	                  return
	               endif
                       if ((online(1:7) .eq.'ONLINE') .and.
     &                         (swit1 .eq. 'OFFLINE')) then
	                  online(1:7)=swit1
C if previously ONLINE and switching to OFFLINE,
C write blank directory value to CIF and write new OFFLINE status
                          direct=' '
			  call ftpcls(iunit,dircol,i,1,1,direct,errstat)
	                  if (errstat .ne. 0) then
	       	             message=errstr// 'writing path to CIF'
	                     call wt_ferrmsg(errstat,message)
                             status = errstat
	                     return
	                  endif
	                  call ftpcls(iunit,devcol,i,1,1,online,errstat)
	     	          if (errstat .ne. 0) then
	       	              message=errstr//' writing CAL_DEV to CIF'
	                      call wt_ferrmsg(errstat,message)
                              status = errstat
	                      return
	                  endif
		       elseif ((online(1:7) .eq. 'OFFLINE') .and.
     &                                (swit1 .eq. 'ONLINE')) then
C If previously OFFLINE and switching to ONLINE
C Check that cal file exists on current working directory.
C If it exists write new pathname and new ONLINE status to CIF ; warn if
c environment pathname and pathname given in ASCII file disagree. 
c If cal file does not exist on cwd, do nothing

	                  fle=name(m)
		          pth=path(m)
		          call flechk(current,fle,pth,stat)
                          if (stat.ne.2) then
                             online(1:7)=swit1
			     call ftpcls(iunit,dircol,i,1,1,current,
     &                                   errstat)
	                     if (errstat .ne. 0) then
	       	                message=errstr// 'writing path to CIF'
	                        call wt_ferrmsg(errstat,message)
                                status = errstat
	                        return
	                     endif
			     call ftpcls(iunit,devcol,i,1,1,online,errstat)
	     	             if (errstat .ne. 0) then
	       	                message=errstr// 'writing CAL_DEV to CIF'
	                        call wt_ferrmsg(errstat,message)
                                status = errstat
	                        return
	                     endif                                
	                  endif

	               endif
C                      end if ((online(1:7) .eq. 'ONLINE') .and....

		    else	

C New on,off line status requested for a given cal file in the ASCII list is the 
C SAME as the value in the CIF ! Therefore do nothing to CIF and warn user
                        message=wrnstr//' file: '//name(m)
                        call fcecho(message)
                        message='is already '//swit1
                        call fcecho(message)
C                   end if (swit1 .ne. online(1:7)) :
                    endif
C                end if (flg1 .eqv. .true.) :
                 endif
C             end of if (calif .eq. fileandext(m)) :
              endif			
200           continue
100	continue
	                  

C Warn if cal filename in ASCII list does not exist in the CIF
        do 300 mm=1,nfiles
               if (exist(mm).eq.0) then
		   message=' '
                   call fcecho(message)
                   message=wrnstr//'cal file in ASCII list: '
                   call fcecho(message)
                   message=lclfile(mm)
                   call fcecho(message)
                   message='does NOT exist in the CIF !!'
                   call fcecho(message)
               endif
300     continue


C       close the CIF
110	call ftclos(iunit,errstat)
	if (errstat .ne. 0) then
	     status = errstat
	else
	     call cfrelun(iunit)
	endif



	return
	end




C-----------------------end of CHFLAG subroutine----------------------

C---------------------------------------------------------------------

*+FLECHK
        subroutine flechk(envfile,lfile2,lfile3,status)
 
        implicit none
        character*(*) envfile,lfile2,lfile3
        integer status
	
C Description: 
C checks that cal file given in ASCII file as placed back ONLINE, does
C exist on default directory. 
C Also checks that environment pathname to cal file agrees with that in 
C the ASCII file 
C 
C passed parameters:
C  ENVFILE     :    environment path
C  LFILE2      :    cal filename
C  LFILE3      :    path given in ASCII file
C
C user i/ps (prompted for):
C none 
C 
C Called routines : 
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C  subroutine CPTHNM       : (CALLIB) constructs a system dependant path
C                             from passed arguements
C       
C compilation & linking :
C  link with FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.  
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:95 Jul 5)

        character(7) version
        parameter (version='1.0.0')
*-
C Internals

        character(30) wrnstr
        character(80) message
	character(320) filepathn
        integer errstat
	logical here

C initialise
        status=0
        errstat=0
        filepathn=' '
     	wrnstr= ' ** FLECHK ' //version//' WARNING: '

C create complete file pathname from current directory
 	call cpthnm(envfile,lfile2,filepathn,errstat)
        if (errstat.ne.0) then
            message=wrnstr//' error creating path '
            call fcecho(message)
            message='for '//lfile2
            call fcecho(message)
            status=errstat
        return
        endif

C check that file exists on current working directory
        inquire (file=filepathn,exist=here)
        if (here .neqv. .true.) then
           message=' '
           call fcecho(message)
           message = wrnstr//' file: '//lfile2
           call fcecho(message)
           message= ' which was requested to be placed back'
           call fcecho(message)
           message='ONLINE, cannot be found on your current'
           call fcecho(message)
           message='working directory. Hence no modifications'
           call fcecho(message)
           message='have been made to the CIF'
           call fcecho(message)
           status=2
         else
C check that environment pathname is the same as the path given in ASCII file
	   if (envfile.ne.lfile3) then
              message=' '
              call fcecho(message)
              message=wrnstr//' file: '//lfile2
              call fcecho(message)
	      message='is ONLINE on directory '//envfile
              call fcecho(message)
              message='although path given in ASCII file is  '//lfile3
              call fcecho(message)
	      status=1
           endif
         endif
         return
         end

C-----------------------end of FLECHK subroutine----------------------

C---------------------------------------------------------------------

