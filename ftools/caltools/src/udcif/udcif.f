*+UDCIF
        subroutine udcif()
        implicit none

C-----------------------------------------------------------------------
C $Id: udcif.f,v 3.27 2013/05/21 19:08:14 irby Exp $
C
C $Log: udcif.f,v $
C Revision 3.27  2013/05/21 19:08:14  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.26  2010/07/30 21:42:37  irby
C Oops, amend the previous commit: leave 'comment' & 'cclsval' at 80 chars.
C
C Revision 3.25  2010/07/30 21:32:16  irby
C String 'context' must be large enough (240 char) to hold the contents
C of 'dummystring'.  Was leading to an "End of record" Fortran runtime
C error for long directory paths.
C
C Revision 3.24  2006/07/11 18:56:45  irby
C Submitted by M.Corcoran:
C If a telescope has more than one instrument, udcif won't allow the
C caldb.indx file to be updated (when it finds the first value of instrume
C in the caldb.indx which doesn't match the INSTRUME keyword the execution
C is halted).  This revision should fix the problem.
C
C Revision 3.23  2006/06/16 17:43:50  irby
C Updated documentation from M.Corcoran.
C
C Revision 3.22  2006/03/29 21:08:21  irby
C Submitted by Mike Corcoran - UDCIF VERSION 3.0:
C
C Mike Corcoran Nov 2005 - Substantially changed the logic by which calibration
C conflicts are determined.  Now for "base conflicts" (where cnam, reftime,
C detnam, telescop, filter all match) it will check all the boundary keywords
C for overlaps.  If a parameter name is missing it will alert the user.  If a
C parameter name matches but the parameter has different UNITS, it will alert
C the user.  For "base conflicts", the boundary parameter names are checked.
C If for any matching boundary parameter name, the values overlap, then there's
C a CONFLICT.  If none of the matching boundary parameter names have overlapping
C values, then there's NO CONFLICT.
C
C Also updates checksum and datasums in 1st extension of caldb.indx file.
C
C Also added check to make sure calibration files TELESCOP values
C match all those in the CIF, to avoid the "suzaku" vs. "sazuku" problem
C
C Jan 2006 - Removed option of including "duplicate" data files (added in
C CVS rev. 3.18) since the CBD block is now correctly checked.
C
C Revision 3.21  2005/08/26 19:36:31  irby
C Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C involving fixes to lines longer than 72 chars that were being truncated,
C but in the case of the CGRO code, also moving misplaced (tabbed) line
C continuation characters to their appropriate position in column 6.

C Revision 3.20  2005/08/08 13:30:53  irby
C Version 2.6.1, submitted by Mike Corcoran 2005/08/05:
C Moved CASE conversion implemented in 3.19 before the check for duplication
C done by subroutine ckcif - this is necessary to compare "ASTRO-E2" to
C "ASTRO-E2", not "Astro-E2' to 'ASTRO-E2'.
C
C Revision 3.19  2005/06/07 20:08:21  irby
C Submitted by Mike Corcoran: version 2.6
C Mike Corcoran, Jun 3, 2005 -- Remove case sensitivity from
C CALDB retrieval/query actions by changing the case of televal,
C detval, filtval, ccnmval, cbdval to uppercase.  Needed since
C quzcif changes inputs into upper case before checking the CIF
C for a match, so that a value of TELESCOP='Astro-E2' in the
C CIF would not match %quzcif mission='Astro-E2' otherwise.
C
C Revision 3.18  2004/07/23 18:48:07  irby
C Submitted by Mike Corcoran July 16 2004:
C Allowed user option of including a file to be indexed in the CALDB even
C if it seems to be a "duplicate" of another entry in the caldb index file.
C This is useful since the determination of a duplicate does not currently
C check for differences in the CBD block, which can be used to distinguish files.
C
C Revision 3.17  2003/08/26 20:40:42  irby
C Fixes submitted by M. Corcoran for the following bugs:
C
C - CIFs have a 40 character limit on the CAL_FILE column; however,
C   udcif seems to have no such limit (it apparently accepts file names
C   up to 160 characters); thus if you update a cif with a file whose
C   name is more than 40 characters the filename (in the CAL_FILE column)
C   is truncated in the cif, and thus the file can't be found in the
C   CALDB.  At minimum udcif should warn the user that the file name is
C   being truncated.
C
C - udcif seems to require that the alias_config.fits file needs to be
C   under $CALDB no matter what value the CALDBALIAS variable contains.
C
C Revision 3.16  2001/11/23 19:21:15  irby
C Fix uninitialized variables to prevent warnings.
C
C Revision 3.15  1999/09/27 16:53:21  pwilson
C Strengthen restrictions on duplicate entries
C
C Revision 3.14  1999/02/24 03:02:11  guerber
C Can't call ftgstm with a constant arg.
C
C Revision 3.13  1998/08/20 18:13:20  breedon
C replace ftdt2s with fttm2s
C
C Revision 3.12  1998/07/27 21:39:35  breedon
C y2k changes ...
C
c Revision 3.10  1998/03/11  19:04:25  pwilson
c Same old 0 -> 0.D0 oedipus problem!
c
C Revision 3.9  1997/12/19 16:45:28  peachey
C Moved ckcbd routine to callib to remove redundancy with mudcif
C
C Revision 3.8  1997/12/18 21:30:39  breedon
C calcrt duplicity problem
C
C Revision 3.7  1996/08/07 10:47:37  oneel
C More uninited varables, again loses on Irix and g77
C
c Revision 3.6  1996/08/06  18:03:41  oneel
c Forgot to init a status variable before calling uclgst.  This loses on
c Irix (and probably g77 as well).
c
C ============================================================================
C
C Description:  Adds an entry to a Calibraiton Index File, for each
C               dataset found in the input FITS file.
C
C Arguments:    NONE
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C               Ron Zellar (1993 June 8), Original fortran version
C                    adapted from the caldb IRAF script udcif.cl
C              Ron Zellar May 23, 1994 -- Added Filter and Detnam check
C              Ron Zellar May 24, 1994 -- Added cmpcbd routine, modified
C                                         ckcif to call cmpcbd
C              Ron Zellar May 25, 1994 -- Added error checking to ckcif
C                                         and wtcif, and improved
C                                         comments in ckcif.  Added CBD
C                                         check to updcif using ckcbd.
C              Ron Zellar Jun 13, 1994 -- Modified fcecho text and if
C                                         blocks so that single good
C                                         quality entry is no longer
C                                         required in CIF
C
C              Lorraine Breedon 23 Jun 1998 - modifications for y2k
C                                             problem
C version 1.1
C version 1.1.2 Lorraine Breedon 20 Aug 1998 - replace ftdt2s with fttm2s
C 
C version 2.4   PDW 27 Sep 1999 - Enforce better duplicates control
C                                 Delete cif.tmp when erroring out
C
C version 2.5   Mike Corcoran, Aug 26, 2003 -- get CALDBALIAS value from
C               CALDBALIAS environment variable; Warn user if cal_file
C               value is being truncated and stop update; also translate
C               caldb alias file name directly from CALDBALIAS
C               environment variable
C
C version 2.5.1 Mike Corcoran July 16 2004 - Allowed user option of including
C               a file to be indexed in the CALDB even if it seems to be a
C               "duplicate" of another entry in the caldb index file.  This
C               is useful since the determination of a duplicate does not
C               currently check for differences in the CBD block, which can
C               be used to distinguish files.
C
C version 2.6   Mike Corcoran June 3 2005 - Remove case sensitivity from
C               CALDB retrieval/query actions by changing the case of televal,
C               detval, filtval, ccnmval, cbdval to uppercase.  Needed since
C               quzcif changes inputs into upper case before checking the CIF
C               for a match, so that a value of TELESCOP='Astro-E2' in the
C               CIF would not match %quzcif mission='Astro-E2' otherwise.
C
C version 2.6.1 Mike Corcoran Aug 8 2005 - Moved CASE conversion implemented
C               in 3.19 before the check for duplication done by subroutine
C               ckcif - this is necessary to compare "ASTRO-E2" to "ASTRO-E2",
C               not "Astro-E2' to 'ASTRO-E2'.
C
C version 3.0   Mike Corcoran Nov 2005 - Substantially changed the logic by
C               which calibration conflicts are determined.  Now for "base
C               conflicts" (where cnam, reftime, detnam, telescop, filter all
C               match) it will check all the boundary keywords for overlaps. 
C               If a parameter name is missing it will alert the user.  If a
C               parameter name matches but the parameter has different UNITS,
C               it will alert the user.
C
C               For "base conflicts", if either boundary parameter array is
C               completely filled with NONE, then there's a boundary overlap
C               and the files conflict: CONFLICT.EQ.TRUE
C
C               For "base conflicts", the boundary parameter names are checked.
C               If all the matching parameter names have overlapping values
C               then conflict.eq.true; otherwise there's at least one parameter
C               who's values don't overlap which means conflict=.false.
C
C               Also updates checksum and datasums in 1st extension
C               of caldb.indx file
C
C               Also added check to make sure calibration files TELESCOP
C               and INSTRUME values match all those in the CIF, to avoid
C               the "suzaku" vs. "sazuku" problem
C
C               Added CHATTER parameter
C
C               Jan 2006 - Removed option of including "duplicate" data
C               files (added in CVS rev. 3.18) since the CBD block is now
C               correctly checked.

*-Version 3.0

        character(160) infile, cif, envar
        integer errstat, chatter
        character(40) taskname
        common /task/ taskname

        taskname = 'udcif3.0'

	envar = 'CALDB'

C       Get the parameters from the par file
        errstat = 0
        call gpudcf(infile, cif, chatter, errstat)

C       if there's a problem getting parameters, return
        if(errstat.ne.0)return

C       Update the CIF
        call updcif(infile, cif, envar, chatter)

        return
        end

C------END of subroutine UDCIF------------------------------------------

*+GPUDCF
C 
                subroutine gpudcf(infile, cif, chatter, status)
        implicit none
        character*(*) infile, cif
        integer status, chatter

C-----------------------------------------------------------------------
C Description: Gets the parameters from the udcif par file and returns
C              them to the calling program.
C
C Arguments:   infile (r): the value of the infile parameter
C              cif    (r): the value of the cif parameter
C              chatter(r): the value of the chatter parameter
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar (1993 June 8), original version
C-----------------------------------------------------------------------
*-Version 1.0
        integer errstat

C       Set the status flag to OK
        status = 0

C       get the infile paramter
        errstat = 0
        call uclgst('infile', infile, errstat)

C       if there's an error getting the infile param, return
        if(errstat.ne.0)then
             call fcerr('Cannot get infile parameter')
             status = 1
             return
        endif

C       get the cif parameter
        call uclgst('cif', cif, errstat)

C       if there's an error getting the cif param, return
        if(errstat.ne.0)then
             call fcerr('Cannot get cif parameter')
             status = 1
             return
        endif
        
C       get the chatter paramter
        errstat = 0
        call uclgsi('chatter', chatter, errstat)
C       if there's an error getting the chatter param, return
        if(errstat.ne.0)then
             call fcerr('Cannot get chatter parameter')
             status = 1
             return
        endif        

        return
        end
                         
C-------End of the GPUDCF subroutine------------------------------------ 
C
*+UPDCIF
C 
        subroutine updcif(infile, cif, envar,chatter)
        implicit none
        character*(*) infile, cif,envar
                                                                                                          
C-----------------------------------------------------------------------
C Description: Accesses the file, infile, and searches for OGIP Caldb
C              datasets.  When one is found, an entry for it is made in
C              the Calibration Index File, cif.
C
C Arguments:   infile  (i): the name of the FITS file containing a
C                           Calibration dataset
C              cif     (i): the name of the Calibration Index File
C              envar   (i): Environment variable pointing at the top
C                           directory of the Caldb
C
C Origin:      Written for the OGIP Calibration Database
C
C Authors/Modification History:
C              Ron Zellar (1993 June 9) Original version
C              Ron Zellar (1994 Aug  8) Added envar variable for cgdr
*-Version 1.0

C-----------------------------------------------------------------------

C              Lorraine Breedon 23 Jun 1998 - modifications for y2k
C                                             problem
C version 1.1
C           Lorraine Breedon 20 Aug 1998 - replace ftdt2s with fttm2s 
C           etc
C version 1.1.1


C-----------------------------------------------------------------------

        character(7) version
        parameter (version='1.1.1')

        character(240) context
        character(80) comment,cclsval
        character(160) alsfil,ciftmp
        integer fillen, ciflen, fcstln,errstat,hdutype,extnum,iunit
        character(240) dummystring
        integer cunit,telecol,instcol,filtcol,devcol,dircol,detcol
        integer filecol,cbdcol,xnocol,aunit,chatter
        integer refcol,qualcol,datecol,cdescol
        integer ounit,oldrows,newrows,qualval,ntrans
        integer blcksz,bitpix,naxis,naxes(2),pcount,gcount,extval,i,j
        integer cclscol,cdtpcol,ccnmcol,cvsdcol,cvstcol,row,width,size
        integer fillenlimit, dirlenlimit
        parameter (fillenlimit=40)
        parameter (dirlenlimit=70)
        
        character(10) televal,instval,trans(10)
c       character(2)  dd,mm
        character(4)  cdtpval,cval
c       character(4)  yy
        character(20) ccnmval,filtval,detval,devval
        character(10)  cvsdval,date
        character(68)  cvsdatval
        character(8)   cvstval
        character(70) cdesval, cbdval(9),dirval
        character(40) fileval
        double precision reftime,second
        logical exact,copen,simple,extend,aopen,iopen,oopen,result
        logical modcif
        character(160) message
        integer iyear,imon,iday,ihour,imin,decimals, utmode
        character(30) timestr
C       character(1) answer


C	Initialize the variables
	extnum = 0
	fillen = 0
	iunit = 34
	ounit = 48
	cunit = 29
	aunit = 56
	copen = .false.                               
	aopen = .false.
	iopen = .false.
	oopen = .false.
	modcif= .false.
	oldrows = 0
	newrows = 0
        iyear=0
        imon=0
        iday=0
        ihour=0
        imin=0
        second=0.0
        decimals=0
        utmode = 0

C       Open the configuration file containing the telescop alias
C       values
	alsfil = 'alias_config.fits'
        errstat = 0
C	call cpthnm(envar,'software/tools',alsfil,errstat)
	    call getenv('CALDBALIAS',alsfil)
C	    write(*,*) 'CALDBALIAS =',alsfil
         errstat = 0
	call ftopen(aunit,alsfil,0,blcksz,errstat)
	if (errstat .ne. 0) then
	     context = 'Cannot open the config file: '//
     &          alsfil(:fcstln(alsfil))
	     goto 999
	endif
	aopen = .true.

C       Get length of the cal filename and the cif filename
        fillen = fcstln(infile)
        ciflen = fcstln(cif)

C	open the calibration file
	call ftopen(iunit,infile(:fillen),0,blcksz,errstat)
	If (errstat .ne. 0) then
	     context='Cannot open the input file: '//
     &	        infile(:fillen)
	     goto 999
	endif


C	Top of the cal file extension searching loop
1000    continue

	errstat = 0

C	keep track of the extension number
	extnum = extnum + 1
	extval = extnum - 1

C	move to the extension given by extnum
	call ftmahd(iunit,extnum,hdutype,errstat)

C	If errstat does not = 0, then end of file
	if ( errstat .ne. 0) then
	     if (modcif) goto 9000
	     context='No datasets indexed'
	     goto 999
	endif

C	Look in the cal file for the CCLS0001 keyword
	call ftgkys(iunit,'CCLS0001',cclsval,comment,errstat)
	if (errstat .ne. 0) goto 1000

C	Get the required calibration keywords

	call ftgkys(iunit,'CCNM0001',ccnmval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('CCNM0001',extnum)
	     goto 1000
	endif

        write(*,*)' '
        context = 'Dataset:     '//ccnmval
        call fcecho(context)

	call ftgkys(iunit,'TELESCOP',televal,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('TELESCOP',extnum)
	     goto 1000
	endif

	call ftgkys(iunit,'INSTRUME',instval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('INSTRUME',extnum)
	     goto 1000
	endif

	call ftgkys(iunit,'CDTP0001',cdtpval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('CDTP0001',extnum)
	     goto 1000
	endif

	call ftgkys(iunit,'CVSD0001',cvsdatval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('CVSD0001',extnum)
	     goto 1000
 	endif
        if (fcstln(cvsdatval) .ne. 10) then

           message='WARNING: CVSD0001 keyword in calibration'//
     &                 ' file is in dd/mm/yy format'
           call fcecho(message)
           message='WARNING: this date will be written in'//
     &                ' yyyy-mm-dd format in the caldb.indx file '

           call fcecho(message)
           call fts2dt(cvsdatval,iyear,imon,iday,errstat)
           if (errstat .ne. 0) then
              message='problem in fts2dt '
              call fcecho(message)
              goto 1000
           endif


           call fttm2s(iyear,imon,iday,ihour,
     &                 imin,second,decimals,cvsdatval,errstat)
           if (errstat .ne. 0) then
              message='problem in fttm2s '
              call fcecho(message)
              goto 1000
           endif

        endif

        cvsdval=cvsdatval(1:10)


	call ftgkys(iunit,'CVST0001',cvstval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('CVST0001',extnum)
	     goto 1000
	endif

C	Calculate the reference time for input into the
C       REF_TIME column
	call calcrt(cvsdatval,cvstval,reftime,errstat)
	if (errstat .ne. 0) then
	     write(cval,'(I4)')extval
	     context='Error calculating the REF_TIME value'//
     &               ' in extension:'//cval
	     call fcerr(context)
	     goto 1000
	endif

	call ftgkys(iunit,'CDES0001',cdesval,comment,errstat)
	if (errstat .ne. 0) then
	     call rqkyer('CDES0001',extnum)
	     goto 1000
	endif

C	Get the non-required keywords
	call ftgkys(iunit,'FILTER',filtval,comment,errstat)
	if (errstat .ne. 0) filtval = 'NONE'
	errstat = 0

	call ftgkys(iunit,'DETNAM',detval,comment,errstat)
	if (errstat .ne. 0) detval = 'NONE'
	errstat = 0

C	Get all the CBDnxxxx keywords from the calibration file
	call ftgkys(iunit,'CBD10001',cbdval(1),comment,errstat)
	if (errstat .ne. 0) cbdval(1)='NONE'
	call ftgkys(iunit,'CBD20001',cbdval(2),comment,errstat)
	if (errstat .ne. 0) cbdval(2)='NONE'
	call ftgkys(iunit,'CBD30001',cbdval(3),comment,errstat)
	if (errstat .ne. 0) cbdval(3)='NONE'
	call ftgkys(iunit,'CBD40001',cbdval(4),comment,errstat)
	if (errstat .ne. 0) cbdval(4)='NONE'
	call ftgkys(iunit,'CBD50001',cbdval(5),comment,errstat)
	if (errstat .ne. 0) cbdval(5)='NONE'
	call ftgkys(iunit,'CBD60001',cbdval(6),comment,errstat)
	if (errstat .ne. 0) cbdval(6)='NONE'
	call ftgkys(iunit,'CBD70001',cbdval(7),comment,errstat)
	if (errstat .ne. 0) cbdval(7)='NONE'
	call ftgkys(iunit,'CBD80001',cbdval(8),comment,errstat)
	if (errstat .ne. 0) cbdval(8)='NONE'
	call ftgkys(iunit,'CBD90001',cbdval(9),comment,errstat)
	if (errstat .ne. 0) cbdval(9)='NONE'

	errstat = 0

C	Check the CBD values to see if the format is readable
	call ckcbd(cbdval,result)
	if (.not. result) then
	     context='Cannot read the CBD values for this dataset'
	     call fcerr(context)
	     goto 1000
	endif

C	Assign input file name to variable written to CIF
	fileval = infile
	
C
C make sure the file name is less than the 40 character limit for
C cal_file in the CIF
C
	if (fillen.gt.fillenlimit) then
                dummystring='Calibration file name '//infile
                write(context,'(A)') dummystring
                call fcerr(context)
                context='Greater than the 40 character limit '
                call fcerr(context)
                goto 1000
	endif

C	Get dir part of system independent specification for the
C	current working directory
        call cgdr(envar,dirval,errstat)
	if (errstat .ne. 0) then
             dummystring='Cannot get directory from CGDR;'
             write(*,*) dummystring
             dummystring='is $CALDB defined?'
             write(*,*) dummystring
	     goto 999
	endif
        fillen=fcstln(dirval)
        if (fillen.gt.fillenlimit) then
		dummystring='Calibration directory name '//dirval
		write(*,*) dummystring
		context='Greater than the 70 character limit '
		call fcerr(context)
		goto 1000
	endif

C	Use the CAL_DEV column to store the online/offline info
C	Since this dataset must be online for me to access it
C	write 'ONLINE' to the CAL_DEV column.
	devval = 'ONLINE'

C	Get the system date
c	call gtdats(dd,mm,yy,date)
c new FITS standard date
        call ftgstm(timestr,utmode,errstat)
        date=timestr(1:10)

C	Get the quality value for this dataset
	call uclgsi('quality',qualval,errstat)
	if (errstat .ne. 0) then
	     context = 'Cannot get quality parameter'
	     call fcerr(context)
	     goto 1000
	endif

C	Make a new entry in the index file containing the keyword
C	values gotten above.

	if (.not. oopen) then
C	     open the old calibration index file and move to the
C	     first extension.
	     call ftopen(cunit,cif(:ciflen),1,blcksz,errstat)
	     if (errstat .eq. 0) copen = .true.
	     call ftmahd(cunit,2,hdutype,errstat)

11     CONTINUE
C	     create a new cif called 'cif.tmp' which will
C            contain all the entries -- old and new.

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
C	    MFC
	     endif

	     call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &          pcount,gcount,extend,errstat)
	     call ftpdef(ounit,bitpix,naxis,naxes,pcount,
     &          gcount,errstat)

C	     Copy all the data from the old cif to the new cif
	     call ftcrhd(ounit,errstat)
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

C	     Get the column numbers from the index file
	     exact = .true.
	     call ftgcno(ounit,exact,'TELESCOP',telecol,errstat)
	     call ftgcno(ounit,exact,'INSTRUME',instcol,errstat)
	     call ftgcno(ounit,exact,'DETNAM',detcol,errstat)
	     call ftgcno(ounit,exact,'FILTER',filtcol,errstat)
	     call ftgcno(ounit,exact,'CAL_DEV',devcol,errstat)
	     call ftgcno(ounit,exact,'CAL_DIR',dircol,errstat)
	     call ftgcno(ounit,exact,'CAL_FILE',filecol,errstat)
	     call ftgcno(ounit,exact,'CAL_CLAS',cclscol,errstat)
	     call ftgcno(ounit,exact,'CAL_DTYP',cdtpcol,errstat)
	     call ftgcno(ounit,exact,'CAL_CNAM',ccnmcol,errstat)
	     call ftgcno(ounit,exact,'CAL_CBD',cbdcol,errstat)
	     call ftgcno(ounit,exact,'CAL_XNO',xnocol,errstat)
	     call ftgcno(ounit,exact,'CAL_VSD',cvsdcol,errstat)
	     call ftgcno(ounit,exact,'CAL_VST',cvstcol,errstat)
	     call ftgcno(ounit,exact,'REF_TIME',refcol,errstat)
	     call ftgcno(ounit,exact,'CAL_QUAL',qualcol,errstat)
	     call ftgcno(ounit,exact,'CAL_DATE',datecol,errstat)
	     call ftgcno(ounit,exact,'CAL_DESC',cdescol,errstat)

	     if (errstat .ne. 0) then
	          context='Cannot find index file columns'
	          goto 999
	     endif

C	     get the number of rows in the cif
	     call ftgkyj(ounit,'NAXIS2',oldrows,comment,errstat)

	endif

C	Search config file for the extension which has the EXTNAME
C	keyword equal to televal
	call mvxnam(aunit,televal,errstat)
	if (errstat .eq. -1) then
	     context = 'Error searching alias config file'
	     goto 999
	endif

        if (errstat .eq. 1) then
	   errstat = 0

C	   Keep track of the number of new entries made
	   newrows = newrows + 1
	   row = newrows + oldrows

           call wtcif(ounit,row,telecol,televal,instcol,instval,
     &     detcol,detval,
     &     filtcol,filtval,devcol,devval,dircol,dirval,filecol,fileval,
     &     cclscol,cclsval,cdtpcol,cdtpval,ccnmcol,ccnmval,cbdcol,
     &     cbdval,xnocol,extval,cvsdcol,cvsdval,cvstcol,cvstval,refcol,
     &     reftime,qualcol,qualval,datecol,date,cdescol,cdesval,
     &     chatter, errstat)

	   if (errstat .ne. 0) then
	      errstat = 0
	      newrows = newrows -1
	      row = row - 1
	   else
	      modcif = .true.
	   endif

	else

	   call ckinst(instval,aunit,ntrans,trans)
	   if (ntrans .eq. 0) then

	      ntrans = 1
	      trans(1) = instval

	   endif

	   Do 2000 i=1,ntrans
	      instval = trans(i)

C	      Keep track of the number of new entries made
	      newrows = newrows + 1
	      row = newrows + oldrows

              call wtcif(ounit,row,telecol,televal,instcol,instval,
     &        detcol,detval,
     &        filtcol,filtval,devcol,devval,dircol,dirval,filecol,
     &        fileval,cclscol,cclsval,cdtpcol,cdtpval,ccnmcol,
     &        ccnmval,cbdcol,cbdval,xnocol,extval,cvsdcol,cvsdval,
     &        cvstcol,cvstval,refcol,reftime,qualcol,qualval,
     &        datecol,date,cdescol,cdesval,chatter, errstat)

	      if (errstat .ne. 0) then
	           errstat = 0
	           newrows = newrows - 1
	           row = row - 1
	      else
	           modcif = .true.
	      endif

2000       continue
	endif


C	Search for the next dataset
	goto 1000

C	Bottom of extension searching loop
9000	continue

C	close the calibration file
	errstat = 0
	call ftclos(iunit, errstat)
	if ( errstat .ne. 0 ) then
	     context = 'Cannot close calibration file: '
     &	        //infile(:fillen)
	     call fcerr(context)
	endif

C       Close the config file
	errstat = 0
	call ftclos(aunit, errstat)
	if ( errstat .ne. 0 ) then
	     context = 'Cannot close configuration file '
     &	        //alsfil(:fcstln(alsfil))
	     call fcerr(context)
	endif

C	reset the data definition keywords
	call ftmkyj(ounit,'NAXIS2',row,'&',errstat)
	call ftgkyj(ounit,'NAXIS1',width,comment,errstat)
	size = width * row
	call ftddef(ounit,size,errstat)

C       update the CHECKSUM and DATASUM keywords
        call ftpcks(ounit,errstat)
	if (errstat .ne. 0) then
	     context='Cannot update the CHECKSUM and DATASUM: cif.tmp'
	     call fcerr(context)
	     errstat = 0
	endif
        

C	close the new calibration index file
	call ftclos(ounit, errstat)
	if (errstat .ne. 0) then
	     context='Cannot close index file: cif.tmp'
	     call fcerr(context)
	     errstat = 0
	endif

C	Move the new cal index file onto the old one
	call mvfile(ciftmp,cif)

C       Write 'newrows' into the .par file
        call uclpsi('newentries',newrows,errstat)
        if (errstat .ne. 0) then
           context='Cannot record newentries into par file'
           call fcerr(context)
           errstat = 0
        endif

	return

999	continue

	call fcerr(context)

	errstat = 0

C	Close the instrument alias configuration file
	if (aopen) then
	     call ftclos(aunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close config file'
		  call fcerr(context)
		  errstat = 0
	     endif
	endif

C	close the calibration file
	if (iopen) then
	     call ftclos(iunit, errstat)
	     if (errstat .ne. 0) then
	          context='Cannot close calibration file: '//
     &	             infile(:fillen)
	          call fcerr(context)
	          errstat = 0
	     endif
	endif

C	close the new calibration index file
	if (oopen) then
	     call ftclos(ounit, errstat)
	     if (errstat .ne. 0) then
	          context='Cannot close index file: cif.tmp'
	          call fcerr(context)
	          errstat = 0
	     endif
             call unlink(ciftmp)
	endif

C	Close the old calibration index file
	if (copen) then
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close index file: caldb.indx'
		  call fcerr(context)
		  errstat = 0
	     endif
	endif

        return
        end
C-------End of subroutine UPDCIF----------------------------------------

*+RQKYER
C 
		subroutine rqkyer(keyword,extnum)

	implicit none
	character(8) keyword
	integer extnum

C-----------------------------------------------------------------------
C Description: Sends an error message to stderr using fcerr.  Used when
C              a required keyword is missing from a CIF.
C
C Arguments:   keyword (i) : the keyword which is missing
C              extnum  (i) : the extnum in which the keyword is missing
C
C Origin:      Written for the calibration database
C
C Authors/Modification History:
C              Ron Zellar 8 Sept, 1993 -- Original Version
C----------------------------------------------------------------------
*-Version 1.0

	character(80) context
	character(4)  cval

	write(cval,'(I4)')extnum
	context='Cannot find '//keyword//' keyword in ext: '//cval
	call fcerr(context)
	return
	end

C------------------------End of RQKYER subroutine---------------------- 

*+MVXNAM
C 
		subroutine mvxnam(lun,extname,status)

	implicit none
	integer lun,status
	character*(*) extname

C-----------------------------------------------------------------------
C Description: Will move through a fits file until the extension which
C              contains the EXTNAME keyword with the value extname is
C              found.
C
C Arguments:   lun      (i) : the logical unit number which the file
C                             was opened with
C              extname  (i) : the EXTNAME keyword value to look for
C              status   (r) : the success status of this routine
C                             0 = OK
C                             1 = extname not found in file
C                             -1 = error when moving through file
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C              Ron Zellar Oct 1, 1993 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	integer errstat,hdutype,fcstln
	character(80) comment,value

	status = 0
        errstat = 0

	call ftgkys(lun,'EXTNAME',value,comment,errstat)
	if ((errstat.eq.0).and.
     &  (value(:fcstln(value)).eq.extname(:fcstln(extname))))then
	     return
	endif

	errstat = 0

	call ftmahd(lun,1,hdutype,errstat)
	if (errstat .ne. 0) then
	     status = -1
	     return
	endif
1000	continue

	call ftgkys(lun,'EXTNAME',value,comment,errstat)
	if ((errstat.eq.0).and.
     &  (value(:fcstln(value)).eq.extname(:fcstln(extname))))then
	     return
	endif

	errstat = 0

	call ftmrhd(lun,1,hdutype,errstat)
	if (errstat.eq.0) then
	     goto 1000
	else
	     status = 1
	     return
	endif

	end

C-----------------End of MVXNAM subroutine------------------------------ 

*+CKINST
C 
		subroutine ckinst(instval,aunit,ntrans,trans)

	implicit none
	character*(*)instval
	character(10) trans(10)
	integer ntrans,aunit

C-----------------------------------------------------------------------
C Description: Checks the alias config file in the current HDU for an
C              alias value matching the instval.  If one is found, trans
C              is returned with the translations for instval and
C              ntrans is the number of translations for instval.
C              If instval does not have any translations, ntrans is
C              returned as 0.
C
C Arguments:   televal (i) : the value for which translations are made
C              aunit   (i) : the logical unit number for the previously
C                            opened configuration file
C              ntrans  (r) : the number of translations for instval
C              trans   (r) : the translations for instval
C
C Origin:      Written for the Caldb
C
C Authors/Modifiction History:
C              Ron Zellar Oct 1, 1993 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	character(10) alias
	character(80) comment
	logical anyf
	integer errstat,i,nalias,fcstln

	ntrans = 0
        errstat = 0

	call ftgkyj(aunit,'NAXIS2',nalias,comment,errstat)
	Do 1000 i=1,nalias
	   call ftgcvs(aunit,1,i,1,1,' ',alias,anyf,errstat)
	   if(alias(:fcstln(alias)).eq.instval(:fcstln(instval)))then
	      call ftgcvj(aunit,2,i,1,1,0,ntrans,anyf,errstat)
	      call ftgcvs(aunit,3,i,1,ntrans,' ',trans,anyf,errstat)
	   endif
1000	continue
	return
	end

*+WTCIF
	subroutine wtcif(ounit,row,telecol,televal,instcol,instval,
     &  detcol,detval,
     &  filtcol,filtval,devcol,devval,dircol,dirval,filecol,fileval,
     &  cclscol,cclsval,cdtpcol,cdtpval,ccnmcol,ccnmval,cbdcol,cbdval,
     &  xnocol,extval,cvsdcol,cvsdval,cvstcol,cvstval,refcol,reftime,
     &  qualcol,qualval,datecol,date,cdescol,cdesval,chatter, status)

	implicit none
     	integer ounit,row,status
	integer extval,qualval
	double precision reftime
     	character*(*)televal,instval,detval,filtval,devval,dirval,
     &  fileval,cclsval,cdtpval,ccnmval,cbdval(9),cvsdval,cvstval,date,
     &  cdesval
	integer telecol,instcol,detcol,filtcol,devcol,dircol,filecol,
     &  cclscol,cdtpcol,ccnmcol,cbdcol,xnocol,cvsdcol,cvstcol,refcol,
     &  qualcol,datecol,cdescol

C-----------------------------------------------------------------------
C Description: Writes Calibration Index File row to the file opened
C              unit ounit to the row given by 'row'.
C
C Arguments:   ounit (i) : the logical unit number for the cif
C              row   (i) : the row number where data will be written
C              errstat (r) : the success status of this routine
C
C Origin:      written for the caldb
C
C Authors/Modification History
C              Ron Zellar Oct 1, 1993 -- Original version
C              Ron Zellar May 24, 1994 -- Modified ckcif call to include
C                                         cbd values
C              Ron Zellar May 25, 1994 -- Added error checking
C              Mike Corcoran Feb 9 2006 -- convert to upper case;
C                                          Version 1.2
C-----------------------------------------------------------------------
*- Version 1.2

        integer nrows,errstat
        character(256) context
        character(4) cval
        character(3) extvalchar
        integer fcstln,chatter

C	Initialize the errstat and status variables
         errstat = 0
         status = 0
         extvalchar='***'
         write(extvalchar,101) extval
  101    format(i2)
        
C        Convert to upper case
C         write(*,*) 'Converting to upper case BEFORE check'
	call ftupch(televal)
	call ftupch(instval)
	call ftupch(ccnmval)
	call ftupch(detval)
	call ftupch(filtval)
	call ftupch(cbdval)

C	Check the other entries to make sure that
C       there are no conflicting datasets
        nrows = row - 1

        call ckcif(ounit,nrows,telecol,televal,instcol,instval,
     &       refcol,reftime,ccnmcol,ccnmval,qualcol,qualval,filtcol,
     &       filtval,detcol,detval,cbdcol,cbdval,dircol,dirval,
     &       filecol,fileval,xnocol,extval,cdescol,chatter,errstat)

        if (errstat .ne. 0) then
           status = 1
           context='DATASET ' // fileval(:fcstln(fileval)) //
     &       '[' // extvalchar //']' // ' not written to CIF'
C           write(*,*) context
           call fcerr(context)
           return
           else 
           context='DATASET ' // fileval(:fcstln(fileval)) //
     &       '[' // extvalchar //']' // ' written to CIF'
           call fcerr(context)             
	endif
C	make necessary fitsio calls to add the data

	call ftpcls(ounit,telecol,row,1,1,televal,errstat)
	call ftpcls(ounit,instcol,row,1,1,instval,errstat)
	call ftpcls(ounit,detcol,row,1,1,detval,errstat)
	call ftpcls(ounit,filtcol,row,1,1,filtval,errstat)
	call ftpcls(ounit,devcol,row,1,1,devval,errstat)
	call ftpcls(ounit,dircol,row,1,1,dirval,errstat)
	call ftpcls(ounit,filecol,row,1,1,fileval,errstat)
	call ftpcls(ounit,cclscol,row,1,1,cclsval,errstat)
	call ftpcls(ounit,cdtpcol,row,1,1,cdtpval,errstat)
	call ftpcls(ounit,ccnmcol,row,1,1,ccnmval,errstat)
	call ftpcls(ounit,cbdcol,row,1,9,cbdval,errstat)
	call ftpclj(ounit,xnocol,row,1,1,extval,errstat)
	call ftpcls(ounit,cvsdcol,row,1,1,cvsdval,errstat)
	call ftpcls(ounit,cvstcol,row,1,1,cvstval,errstat)
	call ftpcld(ounit,refcol,row,1,1,reftime,errstat)
	call ftpclj(ounit,qualcol,row,1,1,qualval,errstat)
	call ftpcls(ounit,datecol,row,1,1,date,errstat)
	call ftpcls(ounit,cdescol,row,1,1,cdesval,errstat)

	if (errstat .ne. 0) then
	     write(cval,'(I4)')errstat
	     context='WTCIF error: returned value = '//cval
	     call fcerr(context)
	     context='Unable to write entry to CIF'
	     call fcerr(context)
	     status = 1
	endif

	return
	end
C
*+CKCIF
C 
        subroutine ckcif(lun,nrows,telecol,televal,instcol,instval,
     &  refcol,reftime,ccnmcol,ccnmval,qualcol,qualval,filtcol,filtval,
     &	detcol,detval,cbdcol,cbdval,dircol,dirval,filecol,fileval,
     &  xnocol,extval,descol,chatter,status)

	implicit none
	integer lun,nrows,telecol,instcol,refcol,ccnmcol,qualcol,qualval
	integer filtcol,detcol,filecol,descol,cbdcol,status
        integer xnocol, extval, dircol,chatter
	character(10) televal,instval,filtval
	character(20) ccnmval,detval
	character*(*) cbdval(9), dirval, fileval
	double precision reftime

C-----------------------------------------------------------------------
C Description: Examines the first nrows of a CIF for rows which have the
C              quality value qualval and which contain the values
C              televal, instval, ccnmval, reftime, filtval, detval and
C              the values cbdval.  If a row which
C              meets these criteria is found then the user is asked to
C              input a different quality value for the row which is
C              found or for the row being added.  If he or she chooses
C              to change the quality value of the row being added, this
C              new quality value is returned to qualval provided it is
C              not the same as the original value.  If he or she
C              chooses to change the quality value in the row which was
C              found, this subroutine will overwrite that row's quality
C              value provided the new value is different from the old
C              value.  If the quality value being edited is not
C              different from its original value, the user will be
C              reprompted.
C
C Arguments:   lun       (i) : The logical unit number for the CIF
C              nrows     (i) : The number of rows in the CIF excluding
C                              the row about to be added
C              telecol   (i) : the col number of the TELESCOP col
C              televal   (i) : the TELESCOP col value
C              instcol   (i) : the col number of the INSTRUME col
C              instval   (i) : the INSTRUME col value
C              refcol    (i) : the col number of the REF_TIME col
C              reftime   (i) : the REF_TIME col value
C              ccnmcol   (i) : the col number of the CAL_CNAM col
C              ccnmval   (i) : the CAL_CNAM col value
C              qualcol   (i) : the col number of the CAL_QUAL col
C              qualval   (r) : the quality value
C              filtcol   (i) : the col number of the FILTER col
C              filtval   (i) : the FILTER col value
C              detcol    (i) : the col number of the DETNAM col
C              detval    (i) : the DETNAM col value
C              cbdcol    (i) : the col number of the CAL_CBD col
C              cbdval    (i) : the CAL_CBD col values
C              filecol   (i) : the col number of the CAL_FILE col
C              fileval   (i) : the CAL_FILE col value
C              descol    (i) : the col number of the CAL_DESC col
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C      Ron Zellar Oct 2, 1993 -- Original Version
C      Ron Zellar May 23, 1994 -- Added FILTER and DETNAM search
C      Ron Zellar May 24, 1994 -- Added CBD search, cmpcbd call
C      Ron Zellar May 25, 1994 -- Added error checking and
C                                 improved comments
C      Ron Zellar Jun 13, 1994 -- Modified fcecho text and if
C                                 blocks so that single good
C                                 quality entry is no longer
C                                 required
C      PD Wilson  Sep 01, 1999 -- Merge the many if-blocks together
C                                 Enforce stricter duplicate control...
C                                 Cannot change current quality, but
C                                 can force old record to qual=5.
C-----------------------------------------------------------------------
*- Version 1.2

	integer qval,errstat,i,extnval,fcstln
	character(4) crow,cerr
	character(10) tval,ival,fval
	character(20) cval,dval
	character(40) file
	character(80) desc,text,bval(9),context, dir
	double precision rval
	logical anyf,ans,duplicate,samefile
         integer pmatch(9)
         logical valtest(9),unitstest(9)

        errstat = 0

	Do 5000 i=1,nrows

C       Get Dir/File/Extn of this record
        call ftgcvs(lun,dircol,i,1,1,' ',dir,anyf,errstat)
        call ftgcvs(lun,filecol,i,1,1,' ',file,anyf,errstat)
	call ftgcvj(lun,xnocol,i,1,1,0,extnval,anyf,errstat)

C	Get quality value from CIF and
	call ftgcvj(lun,qualcol,i,1,1,0,qval,anyf,errstat)

C       Get codename value from CIF and
        call ftgcvs(lun,ccnmcol,i,1,1,' ',cval,anyf,errstat)

C	Get instrument value from CIF and
        call ftgcvs(lun,instcol,i,1,1,' ',ival,anyf,errstat)

C	Get telescope value from CIF and
        call ftgcvs(lun,telecol,i,1,1,' ',tval,anyf,errstat)

C	Get reftime value from CIF and
        call ftgcvd(lun,refcol,i,1,1,0.D0,rval,anyf,errstat)

C	Get filter value from CIF and
        call ftgcvs(lun,filtcol,i,1,1,' ',fval,anyf,errstat)

C	Get detnam value from CIF and
	call ftgcvs(lun,detcol,i,1,1,' ',dval,anyf,errstat)

C       Get boundary values from CIF and
        call ftgcvs(lun,cbdcol,i,1,9,' ',bval,anyf,errstat)

        if (errstat .ne. 0) then
           write(cerr,'(I4)')errstat
           write(crow,'(I4)')i
           context='CKCIF error: returned value = '//cerr
           call fcerr(context)
           context='Unable to get info from row '//crow
           call fcerr(context)
           status = 1
           return
        endif

C       Check if this file has already been added

        if(  dir(1:fcstln(dir)).eq.dirval(1:fcstln(dirval)) .and.
     &       file(1:fcstln(file)).eq.fileval(1:fcstln(fileval)) .and.
     &       extval.eq.extnval ) then
           samefile = .true.
        else
           samefile = .false.
        endif

C Warn user if telescop values are not the same
             if (TVAL.ne.TELEVAL) then 
               WRITE(*,*) 'TELESCOPE VALUE in FILE NOT THE SAME as CIF 
     &          VALUES'
	       write(*,*) ' CIF TELESCOP =',TVAL
	       write(*,*) 'FILE TELESCOP =',TELEVAL
	       write(*,*) 'RETURNING'
               STATUS=1
	       return
	       endif
	       
               
C       Compare this row's values to the dataset
	if ( cval .eq. ccnmval .and.
     &       ival .eq. instval .and.
     &       tval .eq. televal .and.
     &       rval .eq. reftime .and.
     &       fval .eq. filtval .and.
     &       dval .eq. detval ) then



C        If all the above are matched, then check the boundary values
C        using cbdcma2 to see if there's a conflict
         call cbdcma(bval,9,cbdval,9,chatter,duplicate,pmatch,valtest,
     &       unitstest,errstat)

        if (errstat .ne. 0) then
              write(cerr,'(I4)')errstat
              write(crow,'(I4)')i
              context='Unable to compare boundary info from '//
     &             'row '//crow
              call fcerr(context)
              status = 1
              return
           endif
        else
           duplicate = .false.
        endif

        if( duplicate .and. samefile ) then

           call fcecho(' ')
           text = '*********************************************'//
     &          '****************************'
           call fcecho(text)
           text = 'This record is already present in the CIF '
           call fcecho(text)
           write(text,'(a,i4,a)') '      File: '
     &          //file(:fcstln(file))//'[',extnval,']'
           call fcecho(text)
           text = 'Instrument: '//ival
           call fcecho(text)
           text = ' Code Name: '//ccnmval
           call fcecho(text)
           write(text,'(a,i2)') '   Quality: ',qval
           call fcecho(text)
           text = 'If this file''s flags have changed, use caldbflag'
     &          // ' to modify the CIF entry'
           call fcecho(text)
           text = '*********************************************'//
     &          '****************************'
           call fcecho(text)
           call fcecho(' ')
           status = 1
           return

        else if ( duplicate .and.
     &          qval .eq. qualval .and. qval .eq. 0 ) then

C     If all the comparisons above are true then
C     there is a conflict which the user needs to
C     know about

              call ftgcvs(lun,descol,i,1,1,' ',desc,anyf,
     &             errstat)
              call fcecho('   ')
              text='Another dataset has been found which '//
     &             'is valid for ALL the same '
              call fcecho(text)
              text='conditions as the dataset being indexed'
              call fcecho(text)
              call fcecho('   ')
              text='The conflicting dataset: '
              call fcecho(text)
              text='----------------------------------------'
     &             //'--------------------'
              call fcecho(text)
              text=' Instrument: '//ival
              call fcecho(text)
              text='  Code Name: '//ccnmval
              call fcecho(text)
              write(text,'(a,i4,a)') '       File: '//
     &             file(:fcstln(file))//'[',extnval,']'
              call fcecho(text)
              call fcecho('Description:')
              call fcecho(desc)
              text='----------------------------------------'
     &             //'--------------------'
              call fcecho(text)
              call fcecho('   ')
C           Ask if the duplicate should be flagged as bad
              call uclgsb('editc',ans,errstat)
              if (ans) then
C                Change quality of old record to 5
                 qval = 5
                 call ftpclj(lun,qualcol,i,1,1,qval,
     &                errstat)
              else
              status=1
C   MFC:	Give option to include new record, leaving old record valid too
C	 write(*,'(A$)') 'Include new data file in CALDB.INDX (Y/N)> '
c	 read(*,*) answer
c	 if (answer.eq.'Y')then
C        call uclgsb('overide',ans,errstat)
C        if (ans) then
C		      status=0
C	         else
C              Leave old record as valid, skip new entry
C               status = 1
C		 endif
                 return
              endif
             call fcecho('   ')

        endif

 5000   continue
 5010   continue
        return
        end 
	
