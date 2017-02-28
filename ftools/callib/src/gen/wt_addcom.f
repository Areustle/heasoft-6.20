*+WT_ADDCOM
	subroutine wt_addcom(chatter, ounit, ascfil, ierr)

        IMPLICIT NONE
	integer ounit, ierr
	integer chatter
	character*(*) ascfil
c
c Description
c  Program to add the contents of a simple ASCII i/p file as COMMENT
c  keywords in Current FITS Header Unit. A max of 999 comments are
c  currently allowed in a single sitting.
c  !!! NOTE !!!
c    - the FITS file must be closed after return using FTCLOS etc
c
c Passed parameters
c  CHATTER             i  : Chatter flag (5 low, 20 silly)
c  OUNIT               i  : FORTRAN unit of open FITS file
c  ASCFIL              i  : Name of ASCII file containing comments
c  IERR                 o : Error Flag (0=OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called routines
c  subroutine FCECHO         : (FTOOLS) Writes to standard o/p device
c  subroutine FTGHSP         : (FITSIO) Gets space available in CDU
c  subroutine FTPCOM         : (FITSIO) Inserts a COMMENT card into CDU
c  subroutine CGETLUN        : (CALLIB) Gets a free FORTRAN unit
c  subroutine WT_FERRMSG     : (CALLIB) Writes FITSIO error string n stuff
c
c Compilation
c  requires CALLIB,  FITSIO, & FTOOLS
c
c Origin
c  Original
c
c Authors/Modification History:
c  Ian M George    (1993 Feb 18), original
c  Jeff Guerber (1999-02-17)  Made ascfil arg char*(*).
c
        character(7) version
        parameter (version = '1.0.1')
*-

c Internals
	integer iunit, i, ncom
	integer keysexist, keyadd
	integer maxcom
	parameter (maxcom = 999)
	character(80) wrtstr, string
	character(70) stuff(maxcom)
	character(30) wrnstr
	character(30) errstr
c Initialize
	wrnstr = ' ** WT_ADDCOM WARNING: '
	errstr = ' ** WT_ADDCOM ERROR: '
	ierr = 0

c Dump rubbish if daft
       if(chatter.GE.20) then
           wrtstr = ' ... using WT_ADDCOM ' // version
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
	wrtstr = wrnstr//' Problem writing the comment'
	do i = 1, ncom
		string = stuff(i)
		call FTPCOM(ounit,string, ierr)
		call wt_ferrmsg(ierr, wrtstr)
		ierr = 0
	enddo


	return
	end
