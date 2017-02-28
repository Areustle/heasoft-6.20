*+CIFCADD
        subroutine cifcad
        implicit none
                       
C Description:  
C  Opens a CIF (appropriate for a given mission and instrument).
C  Creates a new CIF called cif.tmp and copies all data from old CIF (above)
C  to new. Then adds a new column CAL_ORIG to new CIF (default value
C  changed to 'HEASARC')...if works OK copy new CIF to old one.
 

C passed parameters: none
C
C user i/ps (prompted for):
C none
C
C Called routines : 
C  subroutine FCECHO       : (FITSIO) write to STDOUT
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine CFRELUN      : (CALLIB) free FORTRAN logical unit no
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages if necessary
C  subroutine WTINFO       : (CALLIB) dumps error messages if necessary
C  subroutine FTOPEN       : (FITSIO) opens FITS file
C  subroutine FTCLOS       : (FITSIO) closes FITS file
C  subroutine FTMAHD       : (FITSIO) moves to 1st extension in FITS file
C  subroutine RDCNFG       : (CALLIB) reads the caldb.config file for 
C                                     given mission/instrument

C compilation & linking :
C  link with XPI, FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.  
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:97 Jun 7) original version
C  Lorraine Breedon (1.1.0:98 Apr 23) removed crtcif routine due to
C                                     problems with ftcopy not
C                                     clobbering 1st extn data in 
C                                     cif.tmp
	character(7) version
        parameter (version = '1.1.0')


*-
C Internals
        character(160) calfexp,message
        character(20) mission, instru,alias,tform,origval
        character(30) sjunk
        character(6) subname
        character(8) ttype
        parameter (subname='cifcad')
        logical copen,oopen
 	character(160) junkdir,context,cif,ciftmp,caldbpth
        character(30) errstr,warnstr
        integer fcstln,errstat,hdutype,ijunk,nax2val
	integer blcksz,j,caldblen,origcol
        integer ierr,chatter,ounit,cunit,i
        logical simple,extend
        integer bitpix,naxis,naxes(2),pcount,gcount

C initialise and set up defaults
        ierr = 0
        ounit=0
        cunit=0
        errstat=0
        copen=.false.
        oopen=.false.
        calfexp=' '

	message = '** CIFCADD '//version
        call fcecho(message)
        errstr = '** CIFCADD '//version//' ERROR: '
        warnstr = '** CIFCADD '//version//' WARNING: '

C Get the parameters from the par file
	call gparms2(mission,instru,alias,calfexp,chatter,errstat)

    

C If there's an error getting parameters then return
	if (errstat .ne. 0) return

C see if the user has set the environment variable specified in the 
C calfexp arguement
	call ctrlog(calfexp,fcstln(calfexp),caldbpth,caldblen)
	if (caldblen .eq. 0) then
	   context = 'Environment variable not set'
    	   message = '**CIFCADD '//version//' ERROR : '//context
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

C get and read the caldb.config file to get path to CIF


C NOTE : caldb_info .... the instru argument
C MUST be the 'instrument alias' rather then the 'instrument'
	if ((calfexp(1:5).eq.'caldb').or.(calfexp(1:5).eq.'CALDB')) then
            
	    call caldb_info(chatter,'INST',mission,alias,errstat)
	    if (errstat.ne.0) then
               message='CALDB not defined/available'
               call fcecho(message)
	       message='task requires CALDB to be both defined '//
     &             '& available in order to run'
               call wt_ferrmsg(errstat,message)
	       goto 1000
	    endif
	    errstat=0
	    call rdcnfg(mission,alias,.false.,calfexp,junkdir,errstat)

            if (errstat .ne. 0) return
        endif


  	call cgetlun(cunit)
        call cgetlun(ounit)
        cif=calfexp

        


c See whether we have the CIF open. If not, then open it, and make 
c a "back-up" copy (which is actually the guys we'll be working in)
c ------------------------------------------------------------

C ........ Open the old calibration index file and move to the first extension.
	   call ftopen(cunit,cif(:fcstln(cif)),1,blcksz,errstat)
	   if (errstat .eq. 0) then
		copen = .true.
	   else
		context='Cannot open the CIF'
		call wtferr(subname,version,errstat,context) 
     		context = 'offending file: '//cif
		call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999		
	   endif
	   call ftmahd(cunit,2,hdutype,errstat)
	   If (errstat .ne. 0) then
	        context='Cannot move within the CIF'
		        call wtferr(subname,version,errstat,context) 
     		context = 'offending file: '//cif
		call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999
	   endif



C .........For safety, create a new cif called 'cif.tmp' which will contain 
C          all the entries -- old and new.
	   call wtinfo(chatter,10,1,'Making back-up copy of CIF')
	   j = index(cif,'caldb.indx')
	   ciftmp = cif(:j-1)//'cif.tmp   '
	     simple = .true.
	     blcksz = 0
	     bitpix = 8
	     naxis = 0
	     naxes(1) = 0
	     naxes(2) = 0
	     pcount = 0
	     gcount = 0
	     extend = .true.

	     call ftinit(ounit,ciftmp,blcksz,errstat)
	     if (errstat .eq. 0) oopen = .true.
	     if (errstat .eq. 105) then
	          context='Please remove the file cif.tmp'
	          goto 999
	     endif

	     call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &          pcount,gcount,extend,errstat)
	     call ftpdef(ounit,bitpix,naxis,naxes,pcount,
     &          gcount,errstat)


C ......... Copy all the data from the old cif to the new cif
	     call ftcrhd(ounit,errstat)
	     if (errstat .ne. 0) then
		context='Cannot create new FITS header'//
     &			' in back-up copy'
		call wtferr(subname,version,errstat,context) 
     		context = 'offending file: '//ciftmp
			call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999		
	     endif
  
	     call ftcopy(cunit,ounit,0,errstat)

	     if (errstat .ne. 0) then
		  errstat = 0
		  if (oopen) then
		       call ftclos(ounit,errstat)
		       oopen = .false.
	               call delfil(ciftmp)
	          endif
	          context='Cannot copy data from CIF to temp file'
	          goto 999
	     endif
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) copen = .false.


C now add extra column to backup copy ...the default value in this
C column will be `INDEF'...so will need to change this to `HEASARC'
             ttype='CAL_ORIG'
             tform='20A'
             call fticol(ounit,19,ttype,tform,errstat)
             if (errstat .ne. 0) then
                context='Cannot add extra column to: cif.tmp'
                call wtferr(subname,version,errstat,context)
                call wtinfo(chatter,1,1,context)
                oopen=.true.
                ierr=1
                goto 999
             endif
C now move to 1st ext of backup copy

            call ftmahd(ounit, 2, ijunk, errstat)
            if(errstat.ne.0)then
               context=' moving to 1st extension'
               call wtferr(subname,version,errstat,context)
                call wtinfo(chatter,1,1,context)
                oopen=.true.
                ierr=1
                goto 999

            endif
C Find out how many rows the CIF contains
           call ftgkyj(ounit, 'NAXIS2', nax2val, sjunk, errstat)
           if(nax2val.eq.0)then
             ierr=1
             goto 999             
           endif

C ..replace `INDEF' value with `HEASARC' in the CAL_ORIG column

        origval='HEASARC'

	do 100 i=1,nax2val
C Get the column number of the column to be read
               call ftgcno(ounit,.true.,'CAL_ORIG',origcol,errstat)
                if(errstat.ne.0)then
                   context=' getting CAL_ORIG column number '
                    call wtferr(subname,version,errstat,context)
                    call wtinfo(chatter,1,1,context)
                    oopen=.true.
                    ierr=1
                   goto 999
                endif

              call ftpcls(ounit,origcol,i,1,1,origval,errstat)
              if (errstat .ne. 0) then
                 context=' changing CAL_ORIG default value to HEASARC  '
                 call wtferr(subname,version,errstat,context)
                 call wtinfo(chatter,1,1,context)
                 oopen=.true.
                 ierr=1
                goto 999
              endif

100	continue
C close the new calibration index file
            call ftclos(ounit, errstat)
            if (errstat .ne. 0) then
               context='Cannot close index file: cif.tmp'
               call wtferr(subname,version,errstat,context)
                call wtinfo(chatter,1,1,context)
                oopen=.true.
                ierr=1
                goto 999
            else
              call cfrelun(ounit)
              oopen = .false.
            endif


C Move the new cal index file onto the old one
           call mvfile(ciftmp,cif)

C Close the old calibration index file
 
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		context='Cannot close index file: caldb.indx'
		call wtferr(subname,version, errstat,context)
		copen = .true.
 		ierr=1
                goto 999
	     else
                call cfrelun(cunit)
		copen = .false.
	     endif
   
999	if(ierr.ne.0) then
	  context = 'Unable to continue '//
     &		' -- attempting to shut down & clean up'
	  call wtinfo(chatter,1,1,context)
	 


C	close the new calibration index file
	  if (oopen) then
	     call ftclos(ounit, errstat)
	     if (errstat .ne. 0) then
	          context='Cannot close index file: cif.tmp'
		  call wtferr(subname,version, errstat,context) 
	          errstat = 0
	     endif
             call cfrelun(ounit)
	  endif

C	Close the old calibration index file
		  if (copen) then
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close index file: caldb.indx'
		  call wtferr(subname,version, errstat,context) 
		  errstat = 0
	     endif
             call cfrelun(cunit)
	  endif
	endif

        message=' '
        call fcecho(message)
         message = '** CIFCADD '//version//' finished **'
        call fcecho(message)
        return
1000	continue
        end

C-----------------------------------------------------------------------

*+GPARMS2
	subroutine gparms2(mission,instru,alias,calfexp,chatter,
     &               status)

	implicit none
        character*(*) mission,instru,alias,calfexp
     	integer status,chatter


C Description:  
C  Gets the parameters for CIFCADD from the parameter 
C  file.  

C
C passed parameters:
C  CALFEXP     :    value of environment variable
C  STATUS      :    error flg (0=OK)
C  CHATTER     :    chattiness flag for o/p (5 low,10 normal,15 high)
C
C user i/ps (prompted for):
C  MISSION        :   the name of the mission
C  INSTRU         :   the name of the instrument
C  ALIAS          :   the name of the instrument alias
C 
C Called routines : 
C  subroutine UGLGST       : (XPI) gets parameter values from mudcif.par
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C       
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:97 Jun 06) original version
        character(7) version
        parameter (version='1.0.0')

*-
C Internals

        character(50) contxt
        character(80) message
        integer errstat

C initialise
C Set Status flg to 'no problem!'
        status = 0
        errstat = 0



C Get mission parameter
        call uclgst('mission', mission, errstat)
C If there's an error getting mission, return
        if(errstat.ne.0)then
           contxt = 'cant get mission parameter'
	   message = '**UCLGST '//version//' ERROR : '//contxt
	   call fcecho(message)             
           status = 1
           return
        endif

C Get instrument parameter
        call uclgst('instrument', instru, errstat)
C If there's an error getting instrument, return
        if(errstat.ne.0)then
           contxt = 'cant get instrument parameter'
	   message = '**UCLGST '//version//' ERROR : '//contxt
	   call fcecho(message)             
           status = 1
           return
        endif

C Get instrument alias parameter
        call uclgst('instru_alias',alias, errstat)
C If there's an error getting instru_alias, return
        if(errstat.ne.0)then
           contxt = 'cant get instru_alias parameter'
	   message = '**UCLGST '//version//' ERROR : '//contxt
	   call fcecho(message)             
           status = 1
           return
        endif


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
	

 

        return
        end
C-----------------------end of GPARMS2 subroutine----------------------

