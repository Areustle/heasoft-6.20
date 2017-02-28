*+WT_CIFKYS
	subroutine wt_cifkys(ounit, chatter, datano, telescop,
     &		instrume, filter, detnam, ccls,
     &		ccnm, cdtp, cvsd, cvst, cdes, nbd, cbd, ierr)

	IMPLICIT NONE
	integer chatter, nbd, ierr, ounit
	character*(*) datano
	character*(*) telescop, instrume, filter, detnam
	character*(*) ccls,ccnm,cdtp,cvsd,cvst
	character*(*) cdes, cbd(*)
c
c Description:
c  Writes the keywords & values mandatory for CIF purposes
c  Blank string passed in the filter field results in a corresponding keyword
c  value of 'NONE' being written to the file. Blank string passed in the
c  detnam field results in keyword not being written.
c
c  !!! Note !!! (currently) NO checking of the FITS file is performed.
c               ... the file is assumed to be open within the correct
c                   extension header
c
c Passed parameters
c  OUNIT          i  : FORTAN unit number for o/p
c  CHATTER        i  : chattiness flag for o/p (5 quiet,10 normal,>20 silly)
c  DATANO         i  : dataset number string (ie '0001', '0002' etc)
c  TELESCOP       i  : name of telescope/mission
c  INSTRUME       i  : name of instrument/detector
c  DETNAM         i  : name of detector (if INSTRUME insufficient)
c  FILTER         i  : name of filter in use (if applicable)
c  CCLS	          i  : OGIP class of calibration file to be written
c  CCNM           i  : OGIP codename for calibration file to be written
c  CDTP           i  : Type of data file to be written (Real, task etc)
c  CVSD           i  : Validity start date (UTC) for dataset
c  CVST           i  : Validity start time (UTC on CVSD) for dataset
c  CDES           i  : Descriptive string for dataset
c  NDB            i  : Number of CBD strings to be written
c  CBD            i  : Array of parmeter boundary strings
c  IERR            o : Error flag (0 for OK)
c
c User i/ps required (prompted for):
c  None
c
c Include files
c  None
c
c Called Routines:
c  subroutine CRMVBLK    : (CALLIB) Removes blanks from string
c  subroutine FCECHO     : (FTOOLS) WRites to standard o/p device
c  subroutine FTGERR     : (FITSIO) Gets Error Text
c  subroutine FTPKYS     : (FITSIO) writes a character string keyword
c  subroutine WT_FERRMSG : (CALLIB) writes standard error text string
c
c Compilation:
c  link with FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1992 Dec 29), original
c  Ian M George     (1993 Feb 14) telescop, instrume, filter & detnam added
c  Jeff Guerber  (1998-07-07) Made args char*(*), use datano(1:4), fix '(i)'
c     formats (w/o width spec) -- nonstd extension meaning i7 (so CBD broken)
c
	character(7) version
	parameter (version = '1.1.1')
*-

c Internals
	integer i
	character(69) wrtstr
	character(21) errstr

c Initial setup
	ierr = 0
	errstr = '** WT_CIFKYS ERROR:  '

c Main

c Give info if required
	if(chatter.GE.20) then
		write(wrtstr,'(2a)') '  ... using WT_CIFKYS ',
     &			version
		call fcecho(wrtstr)
	endif

c TELESCOP
           call FTPKYS(ounit,'TELESCOP',telescop,
     &          'Name of Mission/Telescope',
     &          ierr)

c INSTRUME
           call FTPKYS(ounit,'INSTRUME',instrume,
     &          'Name of Instrument/Detector',
     &          ierr)

c DETNAM
	if(detnam.NE.' ') then
           call FTPKYS(ounit,'DETNAM',detnam,
     &          'Specific Detector(s)',
     &          ierr)
	endif
c FILTER
	if(filter.EQ.' ') filter = 'NONE'
           call FTPKYS(ounit,'FILTER',filter,
     &          'Filter in use',
     &          ierr)

c CCLS
	   write(wrtstr,'(2a)') 'ccls',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),ccls,
     &          'OGIP class of calibration file',
     &          ierr)

c CCNM
	   write(wrtstr,'(2a)') 'ccnm',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),ccnm,
     &          'OGIP codename for this type of cal file',
     &          ierr)

c CBD
	do i = 1, nbd
	   write(wrtstr,'(a,i1,a)') 'cbd',i, datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit, wrtstr(1:8),cbd(i),
     &          'dataset parameter boundary',
     &          ierr)
	enddo

c CDTP
	   write(wrtstr,'(2a)') 'cdtp',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),cdtp,
     &          'OGIP type of dataset (DATA, TASK etc)',
     &          ierr)

c CVSD
	   write(wrtstr,'(2a)') 'cvsd',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),cvsd,
     &          'Dataset validity start date (UTC)',
     &          ierr)

c CVST
	   write(wrtstr,'(2a)') 'cvst',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),cvst,
     &          'Dataset validity start time (UTC, of day CVSD)',
     &          ierr)

c CDES
	   write(wrtstr,'(2a)') 'cdes',datano(1:4)
	   call crmvblk(wrtstr)
           call FTPKYS(ounit,wrtstr(1:8),cdes,
     &          ' ',
     &          ierr)



c Last check for error
	call wt_ferrmsg(ierr,errstr)

	return
	end
