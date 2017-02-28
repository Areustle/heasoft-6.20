C FTOOLs Id:
C  $Header: /headas/headas/ftools/einstein/src/sssarf/sssarf.f,v 3.12 2013/05/21 19:08:14 irby Exp $
C
*+SSSARF
      subroutine sssarf
      implicit none

C Description:  Creates an ARF for SSS Einstein PHA data :
C	        Reads in observational data from PHA file + 
C		response matrix (raw or corrected for odd/even effect).
C               Effects of absorption by water ice (which built
C               up at the front of the detector during the mission) are 
C		included in an ARF. 
C              
C Passed parameters : none
C
C User i/ps required (prompted for): none
C
C called routines : 
C  subroutine GTPARM       : (CALLIB) gets parameters from sssarf.par file
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine FCECHO       : (FITSIO) write to standard o/p
c  subroutine FTMAHD	   : (FITSIO) Move to an absolute xtens no.
c  subroutine FTOPEN	   : (FITSIO) Open a FITS file
c  subroutine FTGKYx       : (FITSIO) gets a keyword of type "x"
C  subroutine CALDB_INFO   : (CALLIB) checks local CALDB available to user
C                                     for required mission/instrument 
C  subroutine GTCALF       : (CALLIB) finds specific type of cal data set
C  subroutine FCPARS       : (CALLIB) parses a cal filename
C  subroutine MATRICES     : (CALLIB) does the job
C 
C compilation & linking :
C  link with CALLIB & FITSIO
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files into standard FITS format before use.
C         Based in part on VIMAT routines (Nick White et al 1988)
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:95 Dec 01) original version
C  Ning Gan (1.0.0:98 Aug 01) Changed the length of date keyword string
C			      from 20 to 68.

	character(7) version
        parameter (version = '1.0.1')
*-
C Commons
	character(40) taskname
	common/task/taskname

C internals
	character(80) message
	character(30) online,sjunk,errstr,ianl(1), calexpr
	character(20) telescop, instrume
	character(68) dateobs,timeobs
	character(160) phafil,outfile1,calfexp,calfilename,ianf(1)
	character(5) arfversn
	integer errstat,icef,chatter,calextno,nret,nfound,phalun,
     &           ijunk, iane(1)
	logical odd_even

	        
C initialise
	errstat = 0
	taskname='SSSARF '//version

	message = '** SSSARF '//version
        call fcecho(message)
        errstr = '** SSSARF '//version//' ERROR: '

       

C Get the parameters from the SSSARF.par file
	call gtparm(phafil,calfexp,arfversn,outfile1,
     &              icef,odd_even,chatter,errstat)

C If there's an error getting parameters then return
	if (errstat.ne. 0) return


C Open the PHA file
	call cgetlun(phalun)
	call ftopen(phalun,phafil,1,ijunk,errstat)
	if(errstat.ne.0) then
           message=errstr// ' opening the PHAFILE'
           call wt_ferrmsg(errstat,message)
           return
        endif

C Move to 1st extension
       call ftmahd(phalun, 2, ijunk, errstat)
        if(errstat.ne.0)then
            message=errstr// ' moving to 1st extension of PHAfile'
            call wt_ferrmsg(errstat,message)
             return
        endif

C Check that the PHA file is Einstein SSS data
	call ftgkys(phalun, 'TELESCOP', telescop, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the TELESCOP parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            return
        endif
	
	call ftgkys(phalun, 'INSTRUME', instrume, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the INSTRUME parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            return
        endif
	if ((telescop(1:8).ne.'EINSTEIN').or.
     &       (instrume(1:3).ne.'SSS')) then
	     message=errstr//' PHA file is NOT Einstein SSS data'
	     call fcecho(message)
             errstat = 1
             return
        endif

C obtain observation date and time from PHA file
	call ftgkys(phalun, 'DATE-OBS', dateobs, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the DATE-OBS parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            return
        endif
	
	call ftgkys(phalun, 'TIME-OBS', timeobs, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the TIME-OBS parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            return
        endif

C check whether CALDB access software is to be used
	if ((calfexp(1:5).eq.'CALDB').or.(calfexp(1:5).eq.'caldb')) then
	    call caldb_info(chatter,'INST',telescop,instrume,errstat)
	    if (errstat.ne.0) then
               message='CALDB not defined/available'
               call fcecho(message)
	       message='task requires CALDB to be both defined '//
     &             '& available in order to run'
               call wt_ferrmsg(errstat,message)
	       goto 999
	    endif

	    errstat=0
C now call the caldb access software
	    calexpr = 'ODDEVEN.eq."true"'
c MJT 03July96 changed to .eqv. from .eq
            if (odd_even .eqv. .true.) then
	    call gtcalf(chatter,telescop,instrume, '-', '-',
     &             'SPECRESP MATRIX', 
     &            '-','-','now','now',calexpr,1,
     &             ianf, iane,ianl,nret,nfound,errstat)
	       calfilename = ianf(1)
	       calextno = iane(1)
	       online = ianl(1)
	       if (errstat.ne.0) then
                   message = 'problem obtaining valid MATRIX dataset'//
     &             'accounting for the odd,even effect'
                   call fcecho(message)
                   goto 999
               elseif (nfound .gt. 1) then
                   message = 'more than one valid MATRIX dataset'//
     &             '(with odd/even effect) has been found in the CALDB'
                   call fcecho(message)
	           message = 'unable to determine which to use -- '//
     &             'aborting'
	           errstat=1
                   call wt_ferrmsg(errstat,message)
                   goto 999
	       endif
	    else
	       calexpr = 'ODDEVEN.eq."false"'
	       call gtcalf(chatter,telescop,instrume, '-', '-', 
     &		 'SPECRESP MATRIX', 
     &            '-','-','now','now',calexpr,1,
     &             ianf, iane,ianl,nret,nfound,errstat)
	       calfilename = ianf(1)
	       calextno = iane(1)
	       online = ianl(1)
	       if (errstat.ne.0) then
                   message = 'problem getting valid raw MATRIX dataset'
                   call fcecho(message)
                   goto 999
               elseif (nfound .gt. 1) then
                   message = 'more than one valid raw MATRIX dataset'//
     &             ' has been found in the CALDB'
                   call fcecho(message)
	           message = 'unable to determine which to use -- '//
     &             'aborting'
	           errstat=1
                   call wt_ferrmsg(errstat,message)
                   goto 999
	       endif
	     endif

C..local CALDB is not to be used, rather the user has apparently entered the 
C pathname of the calibration file themselves - so translate its name 
C (removing any extension specified by the user)
	else
	     call fcpars(calfexp,calfilename,calextno,errstat)
c..calfilename contains the name of the calibration file to be opened 
c  calextno contains ext# of dataset (or-99 indicating search required) 
	     if (errstat.ne.0) then
                message='problem parsing calfexp expression'
                call fcecho(message)
                goto 999
             endif
        endif

C do the job

        call matrices(phafil,phalun,calfexp,calfilename,calextno,
     &             arfversn,
     &             outfile1,icef,chatter,errstat)

 	message=' '
        call fcecho(message)
	message = '** SSSARF '//version//' finished '
        call fcecho(message)

	return
999	continue
	end

C----------End of SSSRSP subroutine-----------------------------------

C-----------------------------------------------------------------------

*+GTPARM
	subroutine gtparm(phafil,calfexp,arfversn,outfile1,
     &                    icef,odd_even,chatter,status)

	implicit none
        character*(*) phafil,outfile1,arfversn,calfexp
	integer status,icef,chatter
	logical odd_even
	

C Description:  
C  Gets the parameters from the parameter file.
C
C passed parameters:
C  STATUS     :    error flag (0=OK)
C  ICEF       :    user-defined ice-fraction (default=1)
C  ODD_EVEN   :    odd-even effect applied to raw matrix (if "true")
C  CHATTER    :    chattiness flag for o/p (5 low,10 normal,15 high,>20 silly)
C
C user i/ps (prompted for):
C  PHAFIL     :   source PHA file from Einstein SSS
C  CALFEXP    :   name of calibration information to be used
C  OUTFILE1   :   output ARF file including effects of ice absorption
C  ARFVERSN   :   OGIP version of ARF file required
C  
C
C 
C Called routines : 
C  subroutine UGLGS(T,I,R,B) : (XPI) gets parameter values from SSSRESP.par
C  subroutine FCECHO         : (FITSIO) write to standard o/p

C       
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files into standard FITS format before use.
C         Based in part on VIMAT routines (Nick White et al 1988)
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:95 Dec 01) original version


        character(7) version
        parameter (version='1.0.0')
*-
C Internals

        character(50) contxt
        character(80) message
        integer errstat

C initialise
C Set Status flag to 'no problem!'
        status = 0
        errstat = 0

C Get phafile parameter
        call uclgst('phafile', phafil, errstat)
C If there's an error getting phafile, return
        if(errstat.ne.0) then
             contxt = 'cant get phafile parameter'	     
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


C Get the OGIP version number of the ARF file format to be created
	call uclgst('arfversn',arfversn, errstat)
	if(errstat.ne.0) then
	  contxt= 'Problem getting ARFVERSN parameter'
	  message = '**UCLGST '//version//' ERROR : '//contxt
	  call fcecho(message)
	  arfversn='1.1.0'
	  message = 'setting ARFVERSN = '//arfversn
	  call fcecho(message)
	  status=0
	elseif(arfversn.EQ.'1992a') then
          arfversn='1.1.0'
	endif


C Get outfile1 parameter
        call uclgst('outfile1', outfile1, errstat)
C If there's an error getting outfile1, return
        if(errstat.ne.0)then
             contxt = 'cant get outfile1 parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
             call fcecho(message)            
             status = 1
             return
        endif


C Get the user-defined ice fraction parameter
        call uclgsi('icef', icef, errstat)
C If there's an error getting icef, return
	 if(errstat.ne.0)then
             contxt = 'cant get icef parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
             call fcecho(message)
             status = 1
             return
        endif

	if ((calfexp(1:5).eq.'CALDB').or.(calfexp(1:5).eq.'caldb')) then
C Get the parameter to determine whether odd-even effect is 
C to be applied to matrix
            call uclgsb('oddeven', odd_even, errstat)
C If there's an error getting oddeven, return
            if(errstat.ne.0)then
               contxt = 'cant get oddeven parameter'
               message = '**UCLGST '//version//' ERROR : '//contxt
               call fcecho(message)
               status = 1
               return
            endif
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

C-----------------------end of GTPARM subroutine----------------------

C---------------------------------------------------------------------
*+MATRICES
       subroutine matrices(phafil,phalun,calfexp,calfilename,calextno,
     &             arfversn,
     &             outfile1, icef,chatter,status)


        implicit none
	character*(*) phafil, outfile1,arfversn,calfilename,calfexp
	integer status,icef,chatter,calextno,phalun
	

C Description:  
C  Opens the EINSTEIN SSS PHA file.
C  Reads the necessary keywords from the 1st and 2nd xtensions
C  of the PHA file + observational data.
C  Calculates the amount of ice on the front of the SSS at a given epoch.
C  Reads in the (RMF) Einstein SSS response matrix (raw OR corrected for the 
C  odd/even effect)
C  Creates an ARF (modified by ice-absorption).   
C   
C passed parameters:
C  PHAFIL       :   source PHA file from Einstein SSS
C  PHALUN       :   logical unit number for PHA file
C  CALFEXP      :   type of calibration info to be used
C  CALFILENAME  :   name of calibration information to be used
C  CALEXTNO     :   extn of cal file
C  OUTFILE1     :   output ARF file
C  ARFVERSN     :   OGIP version of RMF file required
C  STATUS       :    error flag (0=OK)
C  ICEF         :    user-defined ice-fraction (default = 1)
C  CHATTER      :    chattiness flag for o/p (5 low,10 normal,15 high,>20 silly)
C   
C user i/ps (prompted for):
C none
C
C Called routines : 
C  subroutine FCECHO     : (FITSIO) write to standard o/p
C  subroutine CGETLUN    : (CALLIB) get free FORTRAN logical unit no
C  subroutine WT_FERRMSG : (CALLIB) dumps error messages
c  subroutine CK_FILE    : (CALLIB) Checks if file exists, deletes if req'd
c  subroutine FTCLOS	 : (FITSIO) Closes a FITS file
c  subroutine FTMAHD	 : (FITSIO) Move to an absolute xtens no.
c  subroutine MVER       : (CALLIB) searches for the appropriate extn
c  subroutine FTOPEN	 : (FITSIO) Open a FITS file
c  subroutine FTGKYx     : (FITSIO) gets a keyword of type "x"
c  subroutine FTPKYx     : (FITSIO) writes a keyword of type "x"
C  subroutine FTPCOM     : (FITSIO) writes comments to a FITS file xtension
C  subroutine WTARF1     : (CALLIB) writes spectral extension within ARF file
C  subroutine FTGCNO     : (FITSIO) gets column no of columns in binary table
c  subroutine OPNPA      : (CALLIB) opens FITS file and writes new primary hdr
C  subroutine INT2MJD    : (CALLIB) convert year:month:day:time to mjd    
C  subroutine NEW_ICE    : (VIMAT)  calculates amount of ice @ given epoch
c  subroutine SSS_ICE_ABS : (VIMAT) calculates ice absorption @ given energy for
C                                   given amount of ice
C  
C compilation & linking :
C  link with FITSIO & CALLIB
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files into standard FITS format before use.
C         Based in part on VIMAT routines (Nick White et al 1988)
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:95 Dec 01) original version


        character(7) version
        parameter (version='1.0.0')

*-
C Commons
	character(40) taskname
	common/task/taskname
C Internals
	integer maxen
        integer nk_hist, nk_comm
        integer ienerg, frow, felem, colnum, ierr, enull
        real areascal
        character(20) telescop, instrume, detnam, filter, mice, mepoch
	character(160) infil
	character(70) hhist(1), comment(9) 
	character(30) sjunk,errstr,wrnstr,chantype,comm
	character(80) message,dummystr(1)
	integer ounit1,lun,ijunk,i,ii,j,
     &     jmax,nobs,errstat, nax2val, 
     &     sss_maxobs, sss_nkev,sss_npha,fcstln	
	real sss_area
	parameter (sss_maxobs=100, sss_nkev=150, sss_npha=128,    
     &     sss_area=180.0)
	real energ_lo(sss_nkev), energ_hi(sss_nkev)
        real sprsp(sss_nkev)

	integer nsearch, ninstr
	parameter (nsearch = 10)
	integer next(nsearch),extnum
	character(20) instr(3),extnames(nsearch),outhdu(3,nsearch),
     &      outver(nsearch)
	character(8) extname
	
	real epoch(sss_maxobs),clumps(sss_maxobs),time(sss_maxobs),
     &     fraction(sss_maxobs),ice_abs,sss_ice_abs, ee, e(sss_nkev),t, 
     &     tt,ti,te, mean_epoch,mean_ice
	
	integer startcol, stopcol, day, month, year , hour, minute
	real startmjd, stopmjd
	double precision start, stop, mjdref, mjd, second

	parameter (year=1978,month=1,day=1,hour=0,minute=0,second=0.0)

	logical killit, qokfil,anyflg, ljunk	

C initialise and set up defaults
	
	killit=.true.
	dummystr(1)=' '
	errstat = 0
        status=0
	i=0
	ii=0
	j=0
	t=0
	tt=0
	te=0
	ti=0
	mean_epoch=0.0
	mean_ice=0.0
	do i=1,sss_maxobs
              epoch(i)=0.0
              clumps(i)=0.0
	      time(i)=0.0
              fraction(i)=0.0
   	end do 
	do i=1,sss_nkev
	     sprsp(i)=0.0
      	end do

        errstr = '** MATRICES '//version//' ERROR: '
        wrnstr = '** MATRICES '//version//' WARNING: '


C*********************obtain observational data from PHA file******************
	
C Rewind PHA file
	rewind(phalun)

C Move to 1st extension
       call ftmahd(phalun, 2, ijunk, errstat)
        if(errstat.ne.0)then
            message=errstr// ' moving to 1st extension of PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif

C Get other necessary keywords
	call ftgkys(phalun, 'CHANTYPE', chantype, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the CHANTYPE parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
	if (chantype.ne.'PHA') then
           message=errstr// ' CHANTYPE is not PHA '
           call fcecho(message)
           status=1
           return
        endif 
	

	call ftgkye(phalun, 'AREASCAL', areascal, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the AREASCAL parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
	if (areascal.ne.1.0) then
           message=wrnstr//'AREASCAL isnt = 1.0, check PHA file'
           call fcecho(message)
        endif

C move to the 2nd extension
       call ftmahd(phalun, 3, ijunk, errstat)
        if(errstat.ne.0)then
            message=errstr// ' moving to 2nd extension of PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif

C Get reference MJD 
	call ftgkyd(phalun, 'MJDREF', mjdref, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr// ' getting the MJDREF parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif

C Find out how many rows (observations) the PHA file contains
        call ftgkyj(phalun, 'NAXIS2', nax2val, sjunk, errstat)
        if(nax2val.eq.0)then
	    message=errstr// ' getting the NAXIS2 parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif

        
C Get the column numbers of all the columns to be read

        call ftgcno(phalun,.true.,'START',startcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting START column number in PHAfile'
           call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif
        
        call ftgcno(phalun,.true.,'STOP',stopcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting STOP column number in PHAfile'
           call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C Read each row and calculate the epochs and exposure times
	nobs = nax2val
        do 100 i=1,nobs
             errstat = 0

C Get start time value (in seconds)
             call ftgcvd(phalun,startcol,i,1,1,0.D0,start,ljunk,errstat)
             if (errstat .ne. 0) then
                 message=errstr// ' getting start time in PHAfile'
                 call wt_ferrmsg(errstat,message)
	         status=errstat
                 return
             endif

C Get stop time value (in seconds)
             call ftgcvd(phalun,stopcol,i,1,1,0.D0,stop,ljunk,errstat)
             if (errstat .ne. 0) then
                 message=errstr// ' getting stop time in PHAfile'
                 call wt_ferrmsg(errstat,message)
	         status=errstat
                 return
             endif

C calculate the exposure time (secs) and mean epoch (decimal day of 
C 1978) for each obs :
	     time(i) = real(stop) - real(start)
	      	     
C for mean epoch calc. convert 1978 Jan 01 OO:OO UT to mjd
	     errstat=0
	     call int2mjd(year,month,day,hour,minute,second,mjd,errstat)
	     if (errstat.ne.0) then
		message=errstr// ' calculating mjd for Jan 01 1978'
	        call fcecho(message)
                status=errstat
	        return
             endif
		        
C start time(mjd) = mjdref + start time (days)....86400 secs in 1 day
C stop time(mjd) = mjdref + stop time (days)
C mean epoch(mjd)= (start time(mjd) + stop time(mjd))/2.0  
C mean epoch(decimal day of 1978) = mean epoch(mjd) - mjd(1978 Jan 01 00:00)

	     startmjd = real(mjdref) + real(start)/86400.0
	     stopmjd = real(mjdref) + real(stop)/86400.0 
       	     epoch(i) = (startmjd + stopmjd)/2.0 - real(mjd)

100	continue

C*************** get EINSTEIN SSS RMF and read in energy data **************

        errstat=0
	call cgetlun(lun)
        call ftopen(lun,calfilename,0,ijunk,errstat)
	if (errstat.ne.0) then
                message='Problem opening'//
     &                   calfilename(:fcstln(calfilename))
                call wt_ferrmsg(errstat,message)
                status=errstat
                return
        endif
	if ((calfexp(1:5).eq.'CALDB').or.(calfexp(1:5).eq.'caldb')) then
C Move to calextno extension
c increment the cal ext by 1 to agree with fitsio ext convention
	   calextno = calextno + 1
	   call ftmahd(lun, calextno, ijunk, errstat)
	   if(errstat.ne.0)then
              message=errstr//' moving to 1st extension of calfile'
              call wt_ferrmsg(errstat,message)
              status = errstat
              return
           endif
	else
c search for the apprpriate extension
	   extnum = calextno
	   ninstr = 3
	   instr(1) = 'RESPONSE'
	   instr(2) = 'RSP_MATRIX'
	   instr(3) = 'REDIST'
	   extname = 'MATRIX'
	   call mver(lun,extnum,ninstr,instr,nsearch,
     &                 next,outhdu,extnames,outver,
     &                 extname,errstat,chatter)
	   
        endif
 
     	infil=calfilename

                 
C Check that the RMF is Einstein SSS data
	call ftgkys(lun, 'TELESCOP', telescop, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the TELESCOP parameter in calfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif


	call ftgkys(lun, 'INSTRUME', instrume, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the INSTRUME parameter in calfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
	if ((telescop(1:8).ne.'EINSTEIN').or.
     &       (instrume(1:3).ne.'SSS')) then
	     message=errstr//' calfile is NOT Einstein SSS data'
	     call fcecho(message)
             status = 1
             return
        endif

	maxen = sss_nkev
        call ftgkyj(lun,'NAXIS2',ienerg,comm,status)
        message = errstr//' reading NAXIS2 value '
        if (status.ne.0) then
          status = 4
          call wt_ferrmsg(status,message)
          return
        endif
        if (ienerg.gt.maxen) then
           status = 4
           message = errstr//' Energy Array dimension is too small !'
           call fcecho(message)
           return
        endif

c --- READ DATA ---
c
      
c     ENERG_LO ...

	frow = 1
        felem = 1
        status = 0
        call ftgcno(lun,.false.,'ENERG_LO',colnum,status)
        if (status.ne.0) then
         message = errstr//' ENERG_LO column not present !'
         call fcecho(message)
         ierr = 4
         return
        endif
        enull = 0
        call ftgcve(lun,colnum,frow,felem,ienerg,enull,energ_lo,
     &            anyflg,status)      
        if (status.ne.0) then
           message = errstr//' reading ENERG_LO column'
           call fcecho(message)
           ierr = 1
           return
        endif

c     ENERG_HI ...
         
	status = 0
        call ftgcno(lun,.false.,'ENERG_HI',colnum,status)
        if (status.ne.0) then
         message = errstr//' ENERG_HI column not present !'
         call fcecho(message)
         ierr = 4
         return
        endif
        enull = 0
        call ftgcve(lun,colnum,frow,felem,ienerg,enull,energ_hi,
     &            anyflg,status)
        if (status.ne.0) then
          message = errstr//' reading ENERG_HI column'
          call fcecho(message)
          ierr = 1
          return
        endif      

C close input RMF 
	status=0
	errstat = 0
        call ftclos(lun, errstat) 
	if(errstat.ne.0) then
	   message=errstr// ' Problem closing input RMF'
           call wt_ferrmsg(errstat,message)
	   status = errstat 
	   return         	
	endif




C ******************Creating the ARF ******************************* 

C check that the ARF file doesn't already exist
	call ck_file(outfile1,dummystr,1,qokfil,killit,chatter)
	if(.NOT.qokfil) then
	     message = '**MATRICES '//version//' ERROR : '//
     &                    'OUTFIL1 parameter already exists'
             call fcecho(message)            
             status = 1
             return
	endif

C Open a FITS file and write a null primary header
	call opnpa(outfile1,chatter,ounit1,killit,errstat)
	if (errstat.ne.0) then
	     message=errstr// ' creating ARF'
	     call fcecho(message)
             status = errstat
             return
        endif

C add additional info keywords to primary header
	call ftpkys(ounit1,'CREATOR',taskname, 
     &             's/w task which wrote this dataset', status)
	call ftpkys(ounit1,'CONTENT','ANCILLIARY RESPONSE', 
     &              'ARF ', status)

C set up input arguements for routines and code to create an ARF 

	filter = ' '
	detnam = ' '
	hhist(1) = ' ARF file created from SSS Einstein data'//
     &           ' by SSSARF '//version
	nk_hist = 1
	comment(1) = ' SSSARF '//version//'summary:'
	comment(2) = ' I/p SSS matrix file : ' //infil
	nk_comm = 2
	maxen = sss_nkev
	ienerg = sss_nkev
	
C include additional effect of absorption (due to water ice depositing
C on the window surface) 
C calculate the amount of ice at the given epochs for the observations
	do 25 i=1, nobs
		call new_ice(epoch(i), clumps(i))
25 	continue

C calculate fractional exposures and thicknesses
	if (nobs.gt.0) then
            jmax=nobs
            do j=1,jmax
		  tt=tt+time(j)
            end do
	    do j=1,jmax
	       fraction(j)= time(j)/tt
	      
	    end do
        else
            jmax=1
            fraction(1)=1.0
            clumps(1)=0.0
	endif


C transform energy bounds read from input RMF back to original range (present 
C in raw VIMAT matrix) for use in ice_abs calculation
	e(1) = (energ_lo(1) + energ_hi(1))/2.0
	e(2) = 0.5*(3.0*energ_hi(1) - energ_lo(1))
	
	do i=3, maxen
	     e(i) = 2.0*energ_hi(i-1) - e(i-1)
        end do

C calculate the ice absorption & multiply through by SSS geometric area
	do i=1,maxen
	     status=0
	     ee=e(i)
	     ice_abs=0.0
             do j=1,jmax
                    clumps(j)=clumps(j)*icef
                    ice_abs=ice_abs + 
     &                      fraction(j)*sss_ice_abs(clumps(j),ee,status)
	     end do

	     sprsp(i)=ice_abs*sss_area
	    
        end do    

C write the SPECRESP extension within the ARF file
       	call wtarf1(ounit1, chatter,
     &          nk_hist, hhist,
     &          nk_comm, comment,arfversn,phafil,
     &          telescop, instrume, detnam, filter,
     &          maxen, ienerg, energ_lo, energ_hi,
     &          sprsp, status)

C add additional key words to ARF extension
	call ftpkys(ounit1,'CREATOR',
     &  	        taskname,
     &             's/w task which wrote this dataset',
     &          	status)

c Close the FITS file
	errstat=0
        call ftclos(ounit1, errstat) 
	if(errstat.ne.0)then
            message=errstr// ' problem closing ARF file'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif

C***********************finished ARF file*************************************

C **********************..update PHA file *************************


C write the required keywords
	call ftpkys(phalun, 'RESPFILE',infil, 
     &                     'Redistribution matrix file (RMF)', errstat)
	if (errstat.ne.0) then
	    message=wrnstr//
     &              ' problem writing RESPFILE keyword to PHA file'
            call wt_ferrmsg(errstat,message)
       	endif

	errstat=0
	call ftpkys(phalun, 'ANCRFILE',outfile1,
     &                     'Ancilliary response file (ARF)', errstat)
	if (errstat.ne.0) then
	   message=wrnstr//
     &            ' problem writing ANCRFILE keyword to PHA file'
           call wt_ferrmsg(errstat,message)
	endif
       	
C calculate the mean_epoch and mean_ice values
	tt=0
	if (nobs.gt.0) then
	   do i=1,nobs
                 t=time(i)
                 tt=tt+t
                 te=te+t*epoch(i)
                 ti=ti+t*clumps(i)
           end do
	   mean_epoch=te/tt
           mean_ice=ti/tt
        else
           mean_epoch=0.0
           mean_ice=clumps(1)
	endif
	          
C indicate these values in the 1st extension as comments
	comment(1) = ' SSSARF '//version//' summary:'
	comment(2) = ' produced the ARF (corrected for ice-absorption) '
	comment(3) = ' for this PHA data '
        comment(4) = ' SSSARF also calculates the mean epoch '
        comment(5) = ' (day of 1978) of n observations & the '
	comment(6) = ' associated amount of ice which accumulated '
	comment(7) = ' on the front of the instrument up to this time '
	write (mepoch,fmt='(F12.7)') mean_epoch
	write (mice,fmt='(F12.7)') mean_ice
	comment(8) = ' mean epoch = '//mepoch
	comment(9) = ' mean ice value = '//mice
	nk_comm = 9
	errstat = 0
	do i=1,nk_comm
	      call ftpcom(phalun,comment(i),errstat)
	      if (errstat.ne.0) then
	         status=0
                 call ftpcom(phalun,' - (missing record) fitsio illegal
     &                      character ?',status)
	      endif
	end do
	message=wrnstr//
     &         ' Putting at least one comment record in PHA file'
        call wt_ferrmsg(status,message)

C close the PHA file
	status=0
	errstat = 0
        call ftclos(phalun, errstat) 
	if(errstat.ne.0) then
	   message=errstr// ' Problem closing PHA file'
           call wt_ferrmsg(errstat,message)
	   status = errstat          	
	endif
	return
	end



C-----------------------end of MATRICES subroutine----------------------

C---------------------------------------------------------------------------
C Append Nick Whites Vimat subroutines/functions used to calc. ice parameter
C----------------------------------------------------------------------------

*+NEW_ICE       
	 subroutine new_ice(time,blobs)
c
c       returns number of BLOBS on the sss at TIME
c       N.E. White   July 89
c
c
        real time               
c  time in 1978 day no ******!!!
        real blobs              
c  calculated number of blobs
        real time_0             
c  start epoch 
        real ice_0              
c  initial value of ice
        real rate_0             
c  e-folding time of 2nd component
        real rate_1             
c  amount on detector on 639
        real loss_0             
c  fraction that returns after a defrost
        real loss_1     
c  residual amount on the detector on day 639
        real rate_2     
c  e-folding of return of 1st component
        real frac       
c  fraction reduction in return of 2nd component
        real day        
c  start day for linear decline in 2nd component 
        real rate_3     
c  fraction of longer return after day 338
        real extra      
c  reduction in loss_0 after day 338
c
c original ice model from July 1989 by Nick
c       time_0=1.0
c       ice_0=63.83
c       rate_0=12.47
c       rate_1=0.25
c       loss_0=0.4137
c       loss_1=0.1067
c       rate_2=0.1087
c       frac=0.2177
c       day=575.0
c       rate_3=15.0
c       extra=1.0
c
c revised ice model Dec 1990, by Damian, Jean and Nick
c
        time_0=1.0
        ice_0=40.10
        rate_0=15.35
        rate_1=0.0041
        loss_0=0.487
        loss_1=0.114
        rate_2=0.1462
        frac=0.0128
        day=577.0
        rate_3=15.0
        extra=1.24
c
c
c
        call icey
     *  (blobs,time,time_0,ice_0,rate_0,rate_1,loss_0,loss_1,rate_2,
     *  frac,day,rate_3,extra)
c
c       
        return
        end

C-----------------------end of NEW_ICE subroutine-----------------------------

C---------------------------------------------------------------------------   
*+ICEY    

	subroutine 
     *icey(ice_time,time,time_0,ice_0,rate_0,rate_1,loss_0,loss_1
     *,rate_2,frac,day,rate_4,extra)
c
cc  to predict SSS ice as a function of time
cc  N.E.White 29/6/89
c
* Import :
      real time               
c  time of interest
* Local constants :
      integer ndef                    
c  no of sss defrostings
      parameter (ndef=31)
* Local data :
      real time_0               
c  starting epoch 
      real ice_0                
c  intial ice value 
      real rate_0       
c  rate of return of 2nd component
      real rate_1       
c  dc component on the detector
      real rate_2       
c  rate of return of 1st component
      real loss_0       
c  fraction that returns after a defrost
       real loss_1       
c  total loss per defrost 
      real frac         
c  fraction of loss of 1st component at 650
      real day          
c  day no when 1st component starts to decrease
      real ice_back     
c  amount of ice in 1st component
      real ice_return   
c  amount that returns after a defrost
      real ice_time     
c  ice at time time
      real ice_start    
c  ice in 2nd component
      real t_average    
c  average time in defrost interval
      real dt           
c  time after defrost
c      real rate         
c  local rate of outgas at t_average
      real ice_def      
c  amount of ice at defrost
      real ice_post_def 
c  total amount of ice after defrost
c
      real rate_4, extra, rate_x, const, clump_true
      integer n
      real t_defrost(ndef)            
c  defrost times
c     real t_start                
c  timeof start of mission
* Local variables :
      integer i                       
c  no of defrosts to time
*
c data statements:
      data t_defrost/333.21 ,337.80 ,341.71 ,343.62 ,345.39 ,
     *               373.85 ,381.62 ,389.75 ,395.75 ,405.76 ,
     *               413.70 ,424.70 ,433.70 ,442.80 ,447.72 ,
     *               453.71 ,460.76 ,468.73 ,477.72 ,493.74 ,
     *               510.75 ,517.60 ,537.69 ,560.759,571.726,
     *               585.712,595.662,610.684,624.859,638.700,650.700/
c     data t_start/330.0/
c
c
c find out no of defrosts up to time
c
      if(time.le.t_defrost(1))then
         i=0
      else
         i=1
         do while((time.gt.t_defrost(i)).and.(i.le.ndef))
            i=i+1
         end do
         i=i-1
        endif
c
c
        ice_start=ice_0
        ice_post_def=ice_start
        rate_x=rate_2
c
c
c before the first defrost
c
        if(i.eq.0.)then
                dt=time-time_0
                if(dt.lt.0.0)dt=0.0
                t_average=(time+time_0)*0.5
                ice_post_def=ice_start
                ice_back=0.0
c
c after first defrost
c
        else
c
                t_average=(t_defrost(1)+time_0)*0.5
                dt=t_defrost(1)-time_0
        ice_def=ice_return(rate_0,dt,0.0,ice_start,rate_x)
	
	        ice_back=ice_def*loss_0
                ice_post_def=ice_post_def-ice_def*loss_1
                ice_start=ice_post_def-ice_back
                if(ice_start.le.0.0)ice_start=0.0       
                if(ice_post_def.le.0.0)ice_post_def=0.0                 
c               write(*,*)ice_post_def
c
c after second defrost if i>=2
c
                n=2
c
        do while(n.le.i)
c
                t_average=(t_defrost(n)+t_defrost(n-1))*0.5
                dt=t_defrost(n)-t_defrost(n-1)
                rate_x=rate_2
c
c
c       extra return time because of longer defrost
c
                if(n.eq.3)then
                rate_x=rate_4*rate_2
                endif
c
c	
	
        ice_def=ice_return(rate_0,dt,ice_back,ice_start,rate_x)
c
c       extra return time because of longer defrost
c
                rate_x=rate_2
                if(n.eq.2)then
                rate_x=rate_4*rate_2
                endif
                ice_back=ice_def*loss_0
c
c  extra loss to account for decrease in last 100 days
c
                if(t_defrost(n).gt.day)then
        const=frac+(1.0-frac)*(638.7-t_defrost(n))/(638.7-day)
c
c               const=exp(-(t_defrost(n)-day)**2.0/frac)
                        ice_back=ice_back*const
                endif
                if(n.eq.2)then
                ice_back=ice_back*extra
                endif
c
c
	        
                ice_post_def=ice_post_def-ice_def*loss_1
                ice_start=ice_post_def-ice_back
                if(ice_start.le.0.0)ice_start=0.0       
                if(ice_post_def.le.0.0)ice_post_def=0.0                 
                n=n+1
        end do
	        dt=time-t_defrost(i)
                t_average=(time+t_defrost(i))*0.5
       endif
c
c
c correct for ice thickness variation with number of clumps
c


      ice_time=ice_return(rate_0,dt,ice_back,ice_start,rate_x)

c
c rate_1 is the amount of ice on the detector on day 639
c
	
        ice_time=clump_true(ice_time)
        ice_time=ice_time-rate_1
	if (ice_time.lt.0.0) then
	    ice_time=0.0
	endif	
	
        return
        end

C-----------------------end of ICEY subroutine-----------------------------

C-------------------------------------------------------------------------

*+ICE_RETURN
       function 
     *ice_return(rate_0,dt,start,total,rate_2)
c
c
c calculates the amount of ice return dt after a defrost
c
c
        real*4 ice_return 
c  amount of ice dt after defrost
        real*4 dt               
c  time from defrost
        real*4 start            
c  1st component ice value after defrost
        real*4 total            
c  2nd component ice value after defrost
        real*4 rate_0           
c  rate of return of 2nd component
        real rate_2             
c
c  rate of return of 1st component
c
c  New model that assumes we are dealing with a fixed amount
c  of water in the dewar, which returns exponentially and 
c  at every defrost a small fraction is lost.
c
c  this is intial return after defrost
c
        if (dt.ge.0.2)then
        if(rate_2.lt.0.01)rate_2=0.01
        if(rate_2.gt.1.0)rate_2=1.0
        ice_return=start*(1.0-exp(-((dt-0.2)/rate_2)**1.0))
c
c  this is the return of the rest from the rest of the cryostat
c
        if(rate_0.lt.0.1)rate_0=0.1
        if(rate_0.gt.1000.)rate_0=1000.
        ice_return=total*(1.0-exp(-dt/rate_0))+ice_return
        else
        ice_return=0.0
        endif
c
       return
       end

C-----------------------end of ICE_RETURN function--------------------------

C--------------------------------------------------------------------

*+CLUMP_TRUE

        real function clump_true(clump)
        real clump
c
c calculate true number of clumps for variable thickness
c
        real thick_min,creal,clump1,thickness
        integer n


        clump_true=0.0
        if(clump.le.0.05)return
        thick_min=0.8e-04
        creal=clump
        n=0
        do while(abs(creal-clump1).gt.0.01.and.n.lt.20)
                n=n+1
                clump1=creal
	        creal=clump*thick_min/thickness(clump1)
        end do
        clump_true=creal
	
        return
        end

C-----------------------end of CLUMP_TRUE function--------------------------


C---------------------------------------------------------------------------
*+THICKNESS

      real function thickness(clumps)
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  1 September 1988 : original clumpy model from Nick White
* Global constants :
* Import :
      real clumps                     
c  no of clumps
* Status :

	
      thickness=0.82e-4+1.35e-4*clumps
	

      end
C-----------------------end of THICKNESS function--------------------------


C---------------------------------------------------------------------------


*+SSS_ICE_ABS
	real function sss_ice_abs(clumps,keV,status)
* Description :
*  code lifted from GSFC software for use in sss spectrum modelling of
*  the absorption caused by ice condensed on the front of the instrument
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  14 December 1987 : original
*  1 September 1988 : clumpy model devised by Nick White
c      implicit none
* Global constants :
c      include 'usr:[andy.par]status.codes'
      integer ok__       
c                            ! success code
      parameter (ok__=0)
      integer error__
c                            ! general error code
      parameter (error__=1)
      integer eof__
c                            ! end-of-file code
      parameter (eof__=-1)

* Import :
      real clumps
c                       ! no of clumps of thick ice
      real clump2
c                       ! no of thin ice clumps
      real keV
c                       ! energy
* Status :
      integer status
* Local constant :
      real oxygen_edge
       parameter (oxygen_edge=0.5317)
c                                      ! keV
* Local variables :
      real t1,t2,v1,v2,z
* External reference :
*-
      sss_ice_abs=0.0
      if(status.ne.ok__)return

      if(keV.le.oxygen_edge)then
         z= 213.*keV**(-2.84)
      else
        z=4860.*keV**(-2.84)
      endif
c
c       t1=(0.82+1.35*clumps)*1.0e-04
c      t2=t1/16.6
c      clump2=clumps*1.27
c
c new parameters from Damian
c
      t1=(1.165+1.944*clumps)*1.0e-04
      t2=t1/26.68
      clump2=clumps*3.02
      v1=z*t1
      v2=z*t2
      sss_ice_abs=exp(-clumps*(1.-exp(-v1)))
      sss_ice_abs=sss_ice_abs*exp(-clump2*(1.-exp(-v2)))
c
      return
      end
C-----------------------end of SSS_ICE_ABS function--------------------------

C--------------------------------------------------------------------------- 
