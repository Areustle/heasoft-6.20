*+DO_ADDCOM
        subroutine do_addcom(chatter, ascfil, fitfil, extname)

	IMPLICIT NONE
	integer chatter, ierr
	character*(*) ascfil, fitfil, extname
c
c Description
c  Finds & opens fitfil, locates desired extension, then opens & reads
c  ascfil, a simple ASCII file, the contents of which it adds to fitfil
c  as COMMENT keywords.
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quiet,10 normal,>20 silly)
c  ASCFIL        i   : Input ASCII filename
c  FITFIL        i   : Input FITS filename
c  EXTNAME       i   : name of xtension to which keywords are to be added
c  IERR            o : Error flag (0 for OK)
c
c User i/ps required:
c  None
c
c Include files
c  None
c
c Called routines
c  subroutine CGETLUN         : (CALLIB) Gets a free FORTRAN unit number
c  subroutine CRMVBLK         : (CALLIB) Removes blanks from a string
c
c Compilation
c  link with XANLIB, FTOOLS & FITSIO
c
c Origin
c  Original
c
c Authors/Modification History:
c  Ian M George    (1992 Nov 04), original
c  Ian M George    (1993 Feb 18), minor i/o modifications
c  Jeff Guerber (1999-02-17)  Make string args assumed-length.  Compare full
c       extension-name strings which will be more reliable.
c
	character(7) version
	parameter (version = '1.0.1')
*-
c Internals
	integer ounit, iunit, blcksize, maxcom, ncom
	integer extno, i
	integer keyadd, keysexist
	parameter (maxcom = 999)
	character(70) string, crap, hdutype
	character(70) stuff(maxcom)
	character(30) errstr
	character(30) wrnstr
	character(80) wrtstr

c Initialize
	errstr = '** DO_ADDCOM ERROR: '
	wrnstr = '** DO_ADDCOM WARNING: '
	ierr = 0

c Give info if requested
	if(chatter.GE.20) then
	  write(wrtstr,'(2A)') ' ... using DO_ADDCOM ', version
 	  call fcecho(wrtstr)
	endif
	if(chatter.GE.5) then
	  wrtstr = wrnstr // 'Operates on i/p file directly'
 	  call fcecho(wrtstr)
	endif

c Open the FITS file
	call cgetlun(ounit)
	call FTOPEN(ounit, fitfil, 1, blcksize, ierr)
	wrtstr = errstr
	call wt_ferrmsg(ierr, wrtstr)
	if(ierr.NE.0) stop

c Remove leading blanks from requested extension name
	call rmvlbk(extname)

c Check to see that Primary is not requested extension
	if(extname(:7).EQ.'PRIMARY') then
		extno = 0
		goto 876
	endif

c Find the requested extension
	do i = 1, 99999
	   call FTMRHD(ounit, 1, hdutype, ierr)
	   if(ierr.NE.0) then
		wrtstr = errstr
		call wt_ferrmsg(ierr, wrtstr)
		stop
	   endif
	   call FTGKYS(ounit,'EXTNAME ', string, crap, ierr)
	   if(ierr.NE.0) then
		wrtstr = errstr
		call wt_ferrmsg(ierr, wrtstr)
		stop
	   endif
	   call crmvlbk(string)
	   if(extname .EQ. string) then
		extno = i
		goto 876
	   endif
	enddo

c Dump some reassurance if requested
876	if(chatter.GE.20) then
	 write(wrtstr,'(A,I12)')' ... Extension located; Ext # = ',extno
	 call fcecho(wrtstr)
	endif

c Open the ASCII file
	call cgetlun(iunit)
	open(unit=iunit, file=ascfil,status = 'OLD')

	do i = 1, maxcom
		read(iunit,'(a)',END=186) stuff(i)
		ncom = i
	enddo
186	if(chatter.GE.10) then
	  write(wrtstr,'(a,i12)') ' ... # COMMENTS to be added = ', ncom
	  call fcecho(wrtstr)
	endif

c Checkout the space available in this header unit
	ierr = 0
	call FTGHSP(ounit, keysexist, keyadd, ierr)
	wrtstr = errstr
	call wt_ferrmsg(ierr, wrtstr)

	if(keyadd.LT.ncom) then
	   if(chatter.GE.20) then
		wrtstr = wrnstr //
     &		   ' Insufficient space for reqd additional keywords'
		call fcecho(wrtstr)
		wrtstr = ' ... relying on FITSIO to automatically' //
     &				' add further header records'
		call fcecho(wrtstr)
	   endif
	endif

c Bung in the buggers
	do i = 1, ncom
		string = stuff(i)
		call FTPCOM(ounit,string, ierr)
		call wt_ferrmsg(ierr, wrnstr)
	enddo

c Close the header n stuff
	call ftclos(ounit, ierr)
	close(iunit)

	return
	end
