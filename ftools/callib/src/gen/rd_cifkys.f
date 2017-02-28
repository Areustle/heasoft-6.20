*+RD_CIFKYS
	subroutine rd_cifkys(ounit, chatter, datano,
     &		telescop, instrume, filter, detnam,
     &		ccls, ccnm, cdtp,
     &		cvsd, cvst, cdes, nbd, cbd, ierr)

	IMPLICIT NONE
	integer ounit, nbd, chatter, ierr
	character*(*) datano
	character*(*) telescop, instrume, filter, detnam
	character*(*) ccls,ccnm,cdtp,cvsd,cvst
	character*(*) cdes, cbd(9)
c
c Description:
c  Gets the mandatory CIF keywords from the current FITS header unit
c  A max of 9 cbd (parameter boundary strings) allowed
c  Returns 'noexist' as value for non-existent strings, or if any other
c  FITSIO errors are encounted during reading
c  !!! NOTE !!!
c  - there is NO checking that the strings have legal values
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (5 quiet,10 normal,>20 silly)
c  DATANO        i   : calibration dataset number (ie 0001, 0002, etc)
c  TELESCOP        o : name of telescope/mission
c  INSTRUME        o : name of instrument/detector
c  FILTER          o : name of filter in use (if applicable)
c  DETNAM          o : name of specific detector (if INSTRUME insufficient)
c  CCLS	           o : OGIP class of calibration file to be written
c  CCNM            o : OGIP codename for calibration file to be written
c  CDTP            o : Type of data file to be written (Real, task etc)
c  CVSD            o : Validity start date (UTC) for dataset
c  CVST            o : Validity start time (UTC on CVSD) for dataset
c  CDES            o : Descriptive string for dataset
c  NBD             o : Number of CBD strings read in & passed back
c  CBD             o : Array of Parameter boundary strings
c  IERR            o : Number of keywords not found (= Error flag; 0 for OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine FCECHO        : (FTOOLS) writes to standard o/p device
c  subroutine FTGKYS        : (FITSIO) Reads (string) keyword
c  subroutine CRMVBLK       : (CALLIB) Removes blanks from a string
c
c Compilation & Linking
c  require XANLIB, FITSIO & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1993 Feb 13), original
c  Jeff Guerber (1998-07-07) Made arg strings char*(*); fix '(i)' formats
c      (i alone is non-std extension, usu means i7 so CBD read was broken)
c
	character(7) version
	parameter (version = '1.0.1')
*-

c Internals
	integer i, status
	character(30) errstr
	character(30) wrnstr
	character(80) wrtstr, noexist, crap

c Initial Setup
	errstr = '** RD_CIFKYS ERROR: '
	wrnstr = '** RD_CIFKYS WARNING: '
	noexist = 'noexist'
	ierr = 0
	status = 0

c Give user info if requested
	if(chatter.GE.20) then
	   wrtstr = ' ... using RD_CIFKYS ' // version
	   call fcecho(wrtstr)
	endif

c Get the CIF keyword values
	status = 0
	call FTGKYS(ounit,'TELESCOP', telescop, crap, status)
	if(status.NE.0) then
		telescop = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'INSTRUME', instrume, crap, status)
	if(status.NE.0) then
		instrume = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'FILTER', filter, crap, status)
	if(status.NE.0) then
		filter = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'DETNAM', detnam, crap, status)
	if(status.NE.0) then
		detnam = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CCLS'//datano(1:4), ccls, crap, status)
	if(status.NE.0) then
		ccls = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CCNM'//datano(1:4), ccnm, crap, status)
	if(status.NE.0) then
		ccnm = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CDTP'//datano(1:4), cdtp, crap, status)
	if(status.NE.0) then
		cdtp = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CVSD'//datano(1:4), cvsd, crap, status)
	if(status.NE.0) then
		cvsd = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CVST'//datano(1:4), cvst, crap, status)
	if(status.NE.0) then
		cvst = noexist
		ierr = ierr + 1
	endif

	status= 0
	call FTGKYS(ounit,'CDES'//datano(1:4), cdes, crap, status)
	if(status.NE.0) then
		cdes = noexist
		ierr = ierr + 1
	endif

	do i = 1,9
	  nbd = i
	  write(wrtstr,'(a,i1)') 'CBD',i
	  call crmvblk(wrtstr)
	  status = 0
	  call FTGKYS(ounit,wrtstr(1:4)//datano(1:4),cbd(i),crap,status)
	  if(status.NE.0) then
		cbd(i) = noexist
		ierr = ierr + 1
	  endif
	enddo

c Final diagnostic
	if(chatter.ge.20) then
	   if(ierr.gt.0) then
		write(wrtstr,'(2A,I7)') wrnstr,
     &			'# keywords not found = ', ierr
		call fcecho(wrtstr)
	   endif
	endif


c yawn, yawn
	return
	end
