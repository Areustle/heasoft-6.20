*+SSSBCK
	subroutine sssbck
	implicit none

C Description: Builds the Einstein SSS background files for a given source
C              observation by reading the user's PHA file and background
C              data 
C
C Passed parameters:   None
C
C User i/ps required (prompted for): none
C
C called routines : 
C  subroutine GPS3BK       : (CALLIB) gets parameters from sssbck.par file
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine FCECHO       : (FITSIO) write to standard o/p
c  subroutine FTMAHD	   : (FITSIO) Move to an absolute xtens no.
c  subroutine FTOPEN	   : (FITSIO) Open a FITS file
c  subroutine FTGKYx       : (FITSIO) gets a keyword of type "x"
C  subroutine CALDB_INFO   : (CALLIB) checks local CALDB available to user
C                                     for required mission/instrument 
C  subroutine GTCALF       : (CALLIB) finds specific type of cal data set
C  subroutine S3WTKY       : (FITSIO) modifies or creates keyword
C  subroutine MKS3BK       : (CALLIB) does the job
C 
C compilation & linking :
C  link with CALLIB & FITSIO
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files and bkgd files into standard FITS format before
C         use. Based on VIMAT routines (Nick White et al 1988). 

C Authors/Modification History:
C  Ron Zellar (1.0.0 :Apr 7, 1994 ) -- Original version
C  Lorraine Breedon (2.0.0: 96 Jan 16) improved and CALLIB-ized
C  Ning Gan(2.0.1: 98 Jul 1) changed the length of dateobs and timeobs
C  string
C  Peter Wilson (2.0.2: 99 Aug 16) Replaced gtcal with gtcalf

	character(7) version
        parameter (version = '2.0.2')
*-
C Commons
	character(40) taskname
	common/task/taskname

C internals

	character(160) phafile,linfile,varfile,
     &              bckfil,corfil,linfexp,varfexp
	character(80) comm,message
	character(30) sjunk,errstr,wrnstr,online
	integer status,errstat,phalun, chatter,
     &          calextno1, calextno2,nret,nfound,ijunk
 	real exptim,jcg,areascal,jcgparm
	character(20) telescop, instrume, chantype
        character(68) dateobs,timeobs
    
	character(160) ianf(1)
	character(30) ianl(1)
	integer iane(1)
	logical ianw
	
C initialise
	errstat = 0
	taskname='SSSBCK '//version

	message = '** SSSBCK '//version
        call fcecho(message)
	
        errstr = '** SSSBCK '//version//' ERROR: '
        wrnstr = '** SSSBCK '//version//' WARNING: '

C Get the parameters from the SSSBCK.par file
	call gps3bk(phafile,jcgparm,linfexp,varfexp,bckfil,
     &                corfil,chatter,errstat)

C If there's an error getting parameters then return
	if (errstat.ne. 0) return
	
C Open the PHA file
	call cgetlun(phalun)
	call ftopen(phalun,phafile,1,ijunk,errstat)
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

C ************************************************************************
C check whether CALDB access software is to be used for linear bkgd spectrum
C *************************************************************************
	if ((linfexp(1:5).eq.'CALDB').or.(linfexp(1:5).eq.'caldb')) then
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
	    ianw = .false.
C now call the caldb access software for linear bkgd spectrum
	    call gtcalf(chatter,telescop,instrume, '-', '-',
     &                  'STD_BKGD_SPEC', dateobs, timeobs,
     &                  dateobs,timeobs,'-',1,
     &                  ianf, iane,ianl,nret,nfound,errstat)
	    linfile = ianf(1)
	    calextno1 = iane(1)
	    online = ianl(1)
            if (errstat.ne.0) then
                   message = 'problem obtaining valid STD_BKGD dataset'
                   call fcecho(message)
                   goto 999
            elseif (nfound .gt. 1) then
                   message = 'more than one valid STD_BKGD dataset'//
     &             'has been found in the CALDB'
                   call fcecho(message)
	           message =
     &                'unable to determine which to use -- aborting'
	           errstat=1
                   call wt_ferrmsg(errstat,message)
                   goto 999
	    endif

C..local CALDB is not to be used, rather the user has apparently entered the 
C pathname of the calibration file themselves - so translate its name 
C (removing any extension specified by the user)
	else
	     call fcpars(linfexp,linfile,calextno1,errstat)
c..calfilename contains the name of the calibration file to be opened 
c  calextno contains ext# of dataset (or-99 indicating search required) 
	     if (errstat.ne.0) then
                message='problem parsing linfexp expression'
                call fcecho(message)
                goto 999
             endif
        endif


C ************************************************************************
C check whether CALDB access software is to be used for var bkgd spectrum
C *************************************************************************

	if ((varfexp(1:5).eq.'CALDB').or.(varfexp(1:5).eq.'caldb')) then
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
C now call the caldb access software for variable bkgd spectrum
	    call gtcalf(chatter,telescop,instrume, '-', '-',
     &                  'VAR_BKGD_SPEC', dateobs, timeobs,
     &                  dateobs,timeobs,'-',1,
     &                  ianf, iane,ianl,nret,nfound,errstat)
	    varfile = ianf(1)
	    calextno2 = iane(1)
	    online = ianl(1)
	    if (errstat.ne.0) then
                   message = 'problem obtaining valid VAR_BKGD dataset'
                   call fcecho(message)
                   goto 999
            elseif (nfound .gt. 1) then
                   message = 'more than one valid STD_BKGD dataset'//
     &             'has been found in the CALDB'
                   call fcecho(message)
	           message =
     &                'unable to determine which to use -- aborting'
	           errstat=1
                   call wt_ferrmsg(errstat,message)
                   goto 999
	    endif

C..local CALDB is not to be used, rather the user has apparently entered the 
C pathname of the calibration file themselves - so translate its name 
C (removing any extension specified by the user)
	else
	     call fcpars(varfexp,varfile,calextno2,errstat)
c..calfilename contains the name of the calibration file to be opened 
c  calextno contains ext# of dataset (or-99 indicating search required) 
	     if (errstat.ne.0) then
                message='problem parsing varfexp expression'
                call fcecho(message)
                goto 999
             endif
        endif



C*********************obtain observational data from PHA file******************

C rewind the PHA file
	rewind(phalun)

C Get other necessary keywords
	call ftgkys(phalun, 'CHANTYPE', chantype, sjunk, errstat)
	if(errstat.ne.0)then
            message=errstr//' getting the CHANTYPE parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
	if (chantype.ne.'PHA') then
           message=errstr//' CHANTYPE is not PHA '
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
	


C Get the EXPOSURE and JCGPARM keywords from the pha file
C EXPOSURE contains the deadtime corrected exposure and
C JCGPARM/100 is the fraction of bad counts which is used 
C to scale the background spectrum

	call ftgkye(phalun,'EXPOSURE',exptim,comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//' getting the EXPOSURE parameter in PHAfile'
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
		     
C	call ftgkye(phalun,'JCGPARM',jcg,comm,errstat)
C	if (errstat .ne. 0) then
C	    message=errstr//' getting the JCGPARM parameter in PHAfile'
C            call wt_ferrmsg(errstat,message)
C            status = errstat
C            return
C        endif

C ***********finished getting data from PHA file *****************************
	     
	jcg = jcgparm

C Generate the scaled linear background file using the linfile 
C background info
	call mks3bk(bckfil,linfexp,linfile,calextno1,telescop,instrume,
     +              exptim,jcg,chatter,errstat)

		

C If no errors generating background file, write BACKFILE
C keyword to the phafile
	if (errstat .eq. 0) then
	   comm = 'Scaled linear background file for this obsn'
	   call s3wtky(phalun,'BACKFILE',bckfil,comm,errstat)
	   if (errstat .ne. 0) then
	      message='There was an error searching for, modifying,'
	      call fcecho(message)
	      message='or adding the BACKFILE keyword to the phafile'
	      call fcecho(message)
	      message='The BACKFILE keyword may not exist or may '//
     &	      'have an incorrect value'
	      call fcecho(message)
	      errstat = 0
	   endif
C Reset errstat in case error occurred generating background file
	   errstat = 0
	endif
C Generate the background correction using the varfile 
C background info
	call mks3bk(corfil,varfexp,varfile,calextno2,telescop,instrume,
     +             exptim,jcg,chatter,errstat)
C If no errors generating background correction file, write CORRFILE
C keyword to the phafile
	if (errstat .eq. 0) then
	   comm = 'Scaled variable background file for this obsn'
	   call s3wtky(phalun,'CORRFILE',corfil,comm,errstat)
	   if (errstat .ne. 0) then
	          message='There was an error searching for, modifying,'
	          call fcecho(message)
	          message='or adding the CORRFILE keyword to the phafile'
	          call fcecho(message)
	          message='The CORRFILE keyword may not exist or may '//
     &	          'have an incorrect value'
	          call fcecho(message)
	          errstat = 0
	    endif
C Reset errstat in case error occurred generating correction file
	    errstat = 0
	endif

C Reset errstat so that ftclos can close the phafile
	errstat = 0
	call ftclos(phalun,errstat)
	if (errstat .ne. 0) then
       	   message=errstr// ' Problem closing PHA file'
           call wt_ferrmsg(errstat,message)
	   status = errstat 
	   return         	
	endif

	message=' '
        call fcecho(message)
	message = '** SSSBCK '//version//' finished '
        call fcecho(message)

	return 
999 	continue 
	end

C----------End of SSSBCK subroutine-----------------------------------

C-----------------------------------------------------------------------

*+GPS3BK
	subroutine gps3bk(phafile,jcgparm,linfexp,varfexp,bckfile,
     +               corfile,chatter, status)

	implicit none
	character*(*) phafile,bckfile,corfile,linfexp,varfexp
	integer status, chatter
	real jcgparm

C Description:  
C  Gets the parameters from the parameter file.
C
C passed parameters:
C  STATUS     :    error flag (0=OK)
C  CHATTER    :    chattiness flag for o/p (5 low,10 normal,15 high,>20 silly)
C
C user i/ps (prompted for):
C  PHAFILE      :   source PHA file from Einstein SSS
C  JCGPARM      :   number of bad counts in PHA file"
C  LINFEXP      :   name of calibration information to be used 
C	            for linear background
C  VARFEXP      :   name of calibration information to be used 
C	            for linear background
C  BCKFILE       :   scaled linear background file
C  CORFILE       :   scaled variable background file
C
C 
C Called routines : 
C  subroutine UGLGS(T,I,R,B) : (XPI) gets parameter values from SSSBCK.par
C  subroutine FCECHO         : (FITSIO) write to standard o/p

C       
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files and bkgd files into standard FITS format before
C         use. Based on VIMAT routines (Nick White et al 1988). 

C Authors/Modification History:
C  Ron Zellar (1.0.0 :Apr 7, 1994 ) -- Original version
C  Lorraine Breedon (2.0.0: 96 Jan 16) improved and CALLIB-ized

	character(7) version
        parameter (version = '2.0.0')

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
        call uclgst('phafile', phafile, errstat)
C If there's an error getting phafile, return
        if(errstat.ne.0) then
             contxt = 'cant get phafile parameter'	     
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

C Get jcgparm parameter
        call uclgsr('jcgparm', jcgparm, errstat)
C If there's an error getting jcgparm, return
        if(errstat.ne.0) then
             contxt = 'cant get jcgparm parameter'	     
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

C Get linfexp parameter
        call uclgst('linfexp', linfexp, errstat)
C If there's an error getting linfexp, return
        if(errstat.ne.0) then
             contxt = 'cant get linfexp parameter'	     
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

C Get varfexp parameter
        call uclgst('varfexp', varfexp, errstat)
C If there's an error getting varfexp, return
        if(errstat.ne.0) then
             contxt = 'cant get varfexp parameter'	     
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

C Get backfile parameter
       call uclgst('bckfile', bckfile, errstat)
C If there's an error getting bckfile, return
       if(errstat.ne.0) then
               contxt = 'cant get bckfile parameter'	     
               message = '**UCLGST '//version//' ERROR : '//contxt
	       call fcecho(message)
               status = 1
               return
        endif

C Get corfile parameter
       call uclgst('corfile', corfile, errstat)
C If there's an error getting corfile, return
       if(errstat.ne.0) then
              contxt = 'cant get corfil parameter'	     
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

C-----------------------end of GPS3BK subroutine----------------------

C---------------------------------------------------------------------


*+S3WTKY
	subroutine s3wtky(lun,keywrd,keyval,comm,status)

	implicit none
	integer lun,status
	character*(*) keywrd,keyval,comm

C Description: Checks to see if the keyword given in keywrd exists
C              in the extension having logical unit lun.  If it exists
C              it is modified to have the value keyval.  If it does
C              not exist, it is created with the value keyval.
C
C passed parameters:
C
C LUN          : The logical unit number of FITS extension
C KEYWRD       : The keyword to be modified/created
C KEYVAL       : The value which keywrd will have
C COMM         : Information comment appropriate to keyword
C STATUS       : The success status of this routine 
C                          0 = OK
C                          1 = error searching file (message given)
C                          2 = error modifying/appending keywrd (no msg)
C
  
C user i/ps (prompted for):
C none
C 
C Called routines : 
C  subroutine WT_FERRMSG : (CALLIB) dumps error messages
c  subroutine FTPKYx     : (FITSIO) writes a keyword of type "x"
c  subroutine FTGKYx     : (FITSIO) gets a keyword of type "x"
C  subroutine FTMKYx     : (FITSIO) modifies a keyword of type "x"
C       
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files and bkgd files into standard FITS format before
C         use. Based on VIMAT routines (Nick White et al 1988). 

C Authors/Modification History:
C  Ron Zellar (1.0.0 :Apr 7, 1994 ) -- Original version
C  Lorraine Breedon (1.1.0: 96 Jan 16) improved and CALLIB-ized


	character(7) version
        parameter (version = '1.1.0')

*-
C internals
	character(10) keydmy,comdmy
	character(30) errstr
	character(80) message
	integer errstat

C initialize 
	status = 0
	errstat = 0
        errstr = '** S3WTKY '//version//' ERROR: '
      

C See if the keyword exists already
	call ftgkys(lun,keywrd,keydmy,comdmy,errstat)
C Error = 0 means keyword exists, so modify it
	if (errstat .eq. 0) then
	     call ftmkys(lun,keywrd,keyval,'&',errstat)
C Error = 202 means keyword doesn't exist, so add it
	else if (errstat .eq. 202) then
	   errstat = 0
	   call ftpkys(lun,keywrd,keyval,comm,errstat)
C Other error means problem searching file
	else
            message=errstr//' searching file for keyword '//keywrd
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif


C See if there was an error modifying or appending keyword
	if (errstat .ne. 0) status = 2
	return

	end

C-----------------------end of S3WTKY subroutine----------------------

C---------------------------------------------------------------------


*+MKS3BK
	subroutine mks3bk(outfil,calfexp,calfil,calext,telescop,
     +                    instrume,exptim,jcg,chatter,status)

	implicit none
	character*(*) outfil,calfil,telescop,instrume,calfexp
	integer calext,status,chatter
	real exptim,jcg

C Description: Makes the Einstein SSS background spectrum extension in
C              outfile.  The background spectrum is created by modifying 
C              the unscaled background spectrum found in the calext
C              extension of the FITS file calfil, using the deadtime
C              corrected exposure time (exptim)  and the jcg value. 
C	       (jcg/100 is the fraction of bad counts.)
C
C passed parameters:
C  OUTFIL      : the name of the output FITS file
C  CALFIL      : the name of the FITS file containing the 
C                unscaled background spectrum
C  CALEXT      : the extension of calfil which contains the
C                unscaled background spectrum
C  TELESCOP    : mission 
c  INSTRUME    : instrument
C  EXPTIM      : the deadtime corrected exposure time
C  JCG         : fraction of bad counts * 100
C   
C user i/ps (prompted for):
C none
C
C Called routines : 
C  subroutine FCECHO     : (FITSIO) write to standard o/p
C  subroutine CGETLUN    : (CALLIB) get free FORTRAN logical unit no
C  subroutine WT_FERRMSG : (CALLIB) dumps error messages
c  subroutine FTCLOS	 : (FITSIO) Closes a FITS file
c  subroutine FTMAHD	 : (FITSIO) Move to an absolute xtens no.
c  subroutine MVER       : (CALLIB) searches for the appropriate extn
c  subroutine FTOPEN	 : (FITSIO) Open a FITS file
c  subroutine FTPKYx     : (FITSIO) writes a keyword of type "x"
c  subroutine FTGKYx     : (FITSIO) gets a keyword of type "x"
c  subroutine FTGCNO     : (FITSIO) gets column nos of all columns to be read
C  subroutine FTINIT     : (FITSIO) opens and initialises a new empty FITS file
C  subroutine FTPHPR     : (FITSIO) puts primary header into the CHU
c  subroutine FTPDEF     : (FITSIO) defines structure of primary array
c  subroutine FTCRHD     : (FITSIO) appends new empty HDU after previously
C                                   accessed extension
C  subroutine FTPHBN     : (FITSIO) puts binary table header keywords into CHU
c  subroutine FTGCVx     : (FITSIO) gets elements (type x) from binary table 
C                                   column
C  subroutine FTPCLx     : (FITSIO) puts values (type x) into a binary table 
C                                   column
C  subroutine FTPCOM     : (FITSIO) writes comments to a FITS file xtension
C  
C compilation & linking :
C  link with FITSIO & CALLIB
C
C Origin: Written for the FTOOLS library. Requires conversion of original
C         source SSS PHA files and bkgd files into standard FITS format before
C         use. Based on VIMAT routines (Nick White et al 1988). 

C Authors/Modification History:
C  Ron Zellar (1.0.0 :Apr 7, 1994 ) -- Original version
C  Lorraine Breedon (2.0.0: 96 Jan 16) improved and CALLIB-ized


	character(7) version
        parameter (version = '2.0.0')
*-

C internals

	integer maxdim
	parameter (maxdim=3)

	integer errstat,bitpix,naxis,naxes(maxdim),pcount,gcount
	integer nrows,tfields,varidat,rwmode,blcksz,hdutyp,row
	integer outlun,callun,nk_comm
	integer ichcol,ictcol,oercol,chanval,cntval,i
	integer ochcol,ortcol
	real    errval,tval
	real    bkg,b,bkgrnd
	character(8) ttype(maxdim),tform(maxdim),tunit(maxdim),extname
	character(30) errstr, wrnstr
	character(180) comm, message
	character(80) dummystr(1)
	character(70) comment(6)
	logical simple,exact,anyf,extend,killit,qokfil

	integer nsearch, ninstr
	parameter (nsearch = 10)
	integer next(nsearch),extnum
	character(20) instr(3),extnames(nsearch),outhdu(3,nsearch),
     &      outver(nsearch)
C	character(8) extname


C Initialize variables
	dummystr(1)=' '
	killit=.true.
	simple   = .true.
	bitpix   = 8
	naxis    = 0
	naxes(1) = 0
	naxes(2) = 0
	pcount   = 0
	gcount   = 1
	extend   = .true.
	varidat  = 0
	rwmode   = 0
	exact    = .true.

        errstr = '** MKS3BK '//version//' ERROR: '
        wrnstr = '** MKS3BK'//version//' WARNING: '


C Open the calibration background spectrum file
	call cgetlun(callun)
	call ftopen(callun,calfil,rwmode,blcksz,errstat)
	if (errstat .ne. 0) then
           message=errstr// ' opening bkgd file '//calfil
           call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

	if ((calfexp(1:5).eq.'CALDB').or.(calfexp(1:5).eq.'caldb')) then
C Move to calextno extension
c increment the cal ext by 1 to agree with fitsio ext convention
	   calext = calext + 1
	   call ftmahd(callun, calext, hdutyp, errstat)
	   if(errstat.ne.0)then
              message=errstr//' moving to 1st extension of calfile'
              call wt_ferrmsg(errstat,message)
              status = errstat
              return
           endif
	   
	else
c search for the apprpriate extension
	   extnum = calext
	   ninstr = 3
	   instr(1) = 'SPECTRUM'
	   instr(2) = 'BACKGROUND'
	   instr(3) = 'COUNT'
	   extname = 'SPECTRUM'
	   call mver(callun,extnum,ninstr,instr,nsearch,
     &                 next,outhdu,extnames,outver,
     &                 extname,errstat,chatter)
	   	   
        endif
	   
C Get info from the header keywords
	call ftgkyj(callun,'NAXIS2',nrows,comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//' getting NAXIS2 from bkgd file '//calfil
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
  	
C Get the EXPOSURE keyword from the calfile
	call ftgkye(callun,'EXPOSURE',tval,comm,errstat)
	if (errstat .ne. 0) then
            message=errstr//' getting EXPOSURE from bkgd file '//calfil
            call wt_ferrmsg(errstat,message)
            status = errstat
            return
        endif
  
C Find the column numbers of the input column names
	call ftgcno(callun,exact,'CHANNEL',ichcol,errstat)
	if (errstat .ne. 0) then
	   message=errstr//
     &             ' getting CHANNEL col. from bkgd file '//calfil
           call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

	call ftgcno(callun,exact,'COUNTS',ictcol,errstat)
	if (errstat .ne. 0) then
 	   message=errstr//
     &             ' getting COUNTS col. from bkgd file '//calfil
           call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif
		    
C check that the output FITS file doesn't already exist
        call ck_file(outfil,dummystr,1,qokfil,killit,chatter)
        if(.NOT.qokfil) then
             message = '**MATRICES '//version//' ERROR : '//
     &                    'OUTFIL1 parameter already exists'
             call fcecho(message)            
             status = 1
             return
        endif

C Open a FITS file and write a null primary header
	call opnpa(outfil,chatter,outlun,killit,errstat)
	if (errstat.ne.0) then
	     message=errstr// ' creating FITS file'
	     call fcecho(message)
  	call cgetlun(outlun)
           status = errstat
             return
        endif
	

C Create the extension for the output data
	call ftcrhd(outlun,errstat)
	if (errstat .ne. 0) then
	   message=errstr//
     &             ' ftcrhd: creating extn for output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C assign the ttype, tform, tunit, etc keywords
	tfields = 3
	ttype(1) = 'CHANNEL'
	ttype(2) = 'RATE'
	ttype(3) = 'STAT_ERR'
	tform(1) = 'I'
	tform(2) = 'E'
	tform(3) = 'E'
	tunit(1) = ' '
	tunit(2) = 'counts/s'
	tunit(3) = 'counts/s'
	extname  = 'SPECTRUM'
	varidat  = 0

	call ftphbn(outlun,nrows,tfields,ttype,tform,tunit,extname,
     &	varidat,errstat)
	if (errstat .ne. 0) then
	   message=errstr//
     &             ' ftphbn: creating extn for output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif


C Write the TELESCOP keyword to the output file
	comm = 'Name of mission'
	call ftpkys(outlun,'TELESCOP',telescop,comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing TELESCOP keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif


C Write the INSTRUME keyword to the output file
	comm = 'Name of instrument'
	call ftpkys(outlun,'INSTRUME',instrume,comm,errstat)
	if (errstat .ne. 0) then
	   message=errstr//
     &             'writing INSTRUME keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C Write the EXPOSURE keyword to output file
C Value came from the calibration file
	comm = 'Integration time (seconds)'
	call ftpkye(outlun,'EXPOSURE',tval,7,comm,errstat)
	if (errstat .ne. 0) then
	   message=errstr//
     &             'writing EXPOSURE keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C Write the AREASCAL keyword to the output file
	comm = 'Area scaling factor'
	call ftpkye(outlun,'AREASCAL',1.0,1,comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing AREASCAL keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C Write the PHAVERSN keyword to the output file
	comm = 'Version of PHA format'
	call ftpkys(outlun,'PHAVERSN','1992a',comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing PHAVERSN keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

	  
C Write the CHANTYPE keyword to the output file
	comm = 'Type of channels'
	call ftpkys(outlun,'CHANTYPE','PHA',comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing CHANTYPE keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif


C Write the DETCHANS keyword to the output file
	comm = 'Number of detector channels'
	call ftpkyj(outlun,'DETCHANS',nrows,comm,errstat)
  	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing DETCHANS keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
       endif

	call FTPKYS(outlun,'BACKFILE ',
     &		'NONE',
     & 		'associated background filename',
     &		status)
	message = wrnstr // ' Problem putting BACKFILE keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(outlun,'BACKSCAL ',
     &		1.0, 6,
     & 		'background file scaling factor',
     &		status)
	message = wrnstr // ' Problem putting BACKSCAL keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYS(outlun,'CORRFILE ',
     &		'NONE',
     & 		'associated correction filename',
     &		status)
	message = wrnstr // ' Problem putting CORRFILE keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(outlun,'CORRSCAL ',
     &		1.0, 6,
     & 		'correction file scaling factor',
     &		status)
	message = wrnstr // ' Problem putting CORRSCAL keyword '
	call wt_ferrmsg(status, message)
	status = 0

         CALL ftpkyl(outlun, 'POISSERR', .FALSE.,
     &               'Poissonian errors not applicable', status)
	message = wrnstr // ' Problem putting POISSER keyword '
	call wt_ferrmsg(status, message)
	status = 0

      CALL ftpkyj(outlun, 'GROUPING', 0,
     &               'no grouping of the data has been defined',
     &               status)
	message = wrnstr // ' Problem putting GROUPING keyword '
	call wt_ferrmsg(status, message)
	status = 0

         CALL ftpkyj(outlun, 'QUALITY', 0,
     &               'no data quality information specified', status)
	message = wrnstr // ' Problem putting QUALITY keyword '
	call wt_ferrmsg(status, message)
	status = 0



C	Write the JCG keyword to the output file
	comm = 'Background prediction parameter'
	call ftpkye(outlun,'JCGPARM',jcg,5,comm,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'writing JCGPARM keyword to output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
       endif

	call ftbdef(outlun,tfields,tform,varidat,nrows,errstat)
	if (errstat .ne. 0) then
	    message=errstr//
     &              'ftbdef: creating extn for output file '//outfil
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
       endif


C	...

C	Read the unscaled background file, scale the COUNTS value, and write
C	to the output file one row at a time

	ochcol = 1
	ortcol = 2
	oercol = 3

	Do 100 row=1,nrows
	     call ftgcvj(callun,ichcol,row,1,1,0,chanval,anyf,errstat)
	     if (errstat .ne. 0) then
	        message=errstr//
     &                  'getting value from CHANNEL col '//outfil
	        call wt_ferrmsg(errstat,message)
	        status = errstat
	        return
             endif

	     call ftgcvj(callun,ictcol,row,1,1,0,cntval,anyf,errstat)
	     if (errstat .ne. 0) then
	        message=errstr//
     &                  'getting value from COUNTS col '//outfil
	        call wt_ferrmsg(errstat,message)
	        status = errstat
	        return
             endif

	     bkg=1.+(jcg/100.-0.7)*0.3
	     b=cntval*bkg*exptim/tval
	     if(cntval.gt.0.)then
	          errval=b*b/cntval
	     else
	          errval=0.
	     endif
	     bkgrnd=b/exptim
	     errval=sqrt(errval)/exptim

	     call ftpclj(outlun,ochcol,row,1,1,chanval,errstat)
	     if (errstat .ne. 0) then
	        message=errstr//'writing value to CHANNEL col '//outfil
	        call wt_ferrmsg(errstat,message)
	        status = errstat
	        return
             endif

	     call ftpcle(outlun,ortcol,row,1,1,bkgrnd,anyf,errstat)
	     if (errstat .ne. 0) then
	        message=errstr//'writing value to RATE col '//outfil
	        call wt_ferrmsg(errstat,message)
	        status = errstat
	        return
             endif

	     call ftpcle(outlun,oercol,row,1,1,errval,anyf,errstat)
	     if (errstat .ne. 0) then
	        message=errstr//'writing value to ERR_STAT col '//outfil
	        call wt_ferrmsg(errstat,message)
	        status = errstat
	        return
             endif


100	continue

C give info about the new FITS file in 1st extension as comments
        comment(1) = ' SSSBCK '//version//' summary:'
        comment(2) = ' this background file : '//outfil
	comment(3) = ' is a scaled version of the calibration data'
	comment(4) = calfil
        comment(5) = ' Scaling uses the relative exposures of the '
        comment(6) = ' input PHA file and the calibration data' 
        nk_comm = 6
        errstat = 0
        do i=1,nk_comm
              call ftpcom(outlun,comment(i),errstat)
              if (errstat.ne.0) then
                 status=0
                 call ftpcom(outlun,' - (missing record) fitsio illegal
     &                      character ?',status)
              endif
        end do
        message=wrnstr//' Putting comment records in bkgd file'
        call wt_ferrmsg(status,message)



C Close the output file
	status=0
	errstat = 0
        call ftclos(outlun, errstat) 
	if(errstat.ne.0) then
	   message=errstr//' Problem closing output file'//outfil
           call wt_ferrmsg(errstat,message)
	   status = errstat 
	   return         	
	endif

C close the unscaled background file
	status=0
	errstat = 0
        call ftclos(callun, errstat) 
	if(errstat.ne.0) then
	   message=errstr//' Problem closing unscaled bkgd file'//calfil
           call wt_ferrmsg(errstat,message)
	   status = errstat 
	   return         	
	endif
	

	return

	end

C-----------------------end of MKS3BK subroutine----------------------
C----------------------------------------------------------------------
