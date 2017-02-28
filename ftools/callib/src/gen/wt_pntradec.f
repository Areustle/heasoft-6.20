*+ WT_PNTRADEC
	subroutine wt_pntradec(chatter, ounit, 
     &  qsys, radecsys, equinox, 
     &	ra_pnt, dec_pnt, pa_pnt,
     &	ra_pnte, dec_pnte, pa_pnte,
     &	ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	real equinox	
	real ra_pnt, dec_pnt, pa_pnt
	real ra_pnte, dec_pnte, pa_pnte
	character(70) radecsys
	logical qsys

c
c Description
c   Writes the (mean) RA, dec & roll angle of the pointing direction
c and any associated errors into current FITS header unit. 
c
c The following keywords can be written (depending upon flags/passed values)
c  RADECSYS- 'Stellar Reference frame used for EQUINOX'
c  EQUINOX - 'Equinox of all RA,dec specifications (AD year)'
c  RA_PNT  - 'RA of pointing (degrees)'
c  DEC_PNT - 'dec of pointing (degrees)'
c  PA_PNT  - 'Roll angle (degrees) from North towards East (degrees)'
c  RA_PNTE - 'Error on RA_PNT (degrees)'
c  DEC_PNTE- 'Error on DEC_PNT (degrees)'
c  PA_PNTE - 'Error on PA_PNT (degrees)'
c 
c NOTE: A logical flag is used to indicate whether the RADECSYS & EQUINOX
c       keywords are written. These keywords are mandatory if any of the 
c       RA,dec keywords are written. Thus the only time when this flag should
c       be set false is when RADECSYS & EQUINOX have already been written 
c	to the current header.
c
c Passed Parameters
c  CHATTER	i   : Chatter flag (<5=quite,10=normal,>20=silly)
c  OUNIT        i   : FORTRAN logical unit no. of o/p file
c  QSYS         i/r : Logical flag whether RADECSYS & EQUINOX to be written
c  RADECSYS     i   : Reference frame for ALL RA & decs
c  EQUINOX      i   : Equinox for ALL RA & decs
c  RA_PNT       i   : RA of pointing direction ("Optical axis")
c  DEC_PNT      i   : dec of pointing direction ("Optical axis")
c  PA_PNT	i   : Roll angle (degrees) from North
c  RA_PNTE      i   : Error on RA_PNT
c  DEC_PNTE     i   : Error on DEC_PNT 
c  PA_PNTE	i   : Error on PA_PNT
c  IERR	          o : Return error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c  subroutine FTPHIS            : (FITSIO) writes a history keyword
c  subroutine FTPKYn            : (FITSIO) writes a keyword of type n
c  subroutine WT_FERRMSG        : (CALLIB) writes FITSIO error message etc
c
c Origin
c   Hacked from the old "WT_RADEC" subroutine
c
c Authors/Modification History
c  Ian M George    (1.0.0: 1993 Oct 04), original
c  Ian M George    (1.0.1: 1993 Oct 22), moved RA,DEC = 0.0 warning message 
	character(7) version
	parameter (version = ' 1.0.1 ')
*-

c Internals
	integer status, decimals
	parameter (decimals = 6)
	character(40) errstr, wrnstr
	character(80) message

c Initialize	
	errstr = '** WT_PNTRADEC '//version//' ERROR: '
	wrnstr = '** WT_PNTRADEC '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WT_PNTRADEC '// version
		call fcecho(message)
	endif

c Stick in the necessary stellar reference frame is any RA & decs to be listed
c ... adding a history record too
	if(qsys) then
	   qsys = .false.
	   message = 
     &     ' RADECSYS & EQUINOX keywords written by WT_PNTRADEC '
     &		// version
	   call FTPHIS(ounit, message, status)
	   message = wrnstr // ' Problem writing History record'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYS(ounit, 'RADECSYS', 
     &		radecsys, 
     &		'Stellar Reference frame used for EQUINOX',
     &		status)
	   message = wrnstr // ' Putting RADECSYS keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'EQUINOX', 
     &		equinox, decimals,
     &		'Equinox of all RA,dec specifications (AD year)',
     &		status)
	   message = wrnstr // ' Putting EQUINOX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	   message = ' Pointing Info written by WT_PNTRADEC '
     &		// version
	   call FTPHIS(ounit, message, status)
	   message = wrnstr // ' Problem writing History record'
	   call wt_ferrmsg(status,message)
	   status = 0


c Pointing direction
	   call FTPKYF(ounit, 'RA_PNT', 
     &		ra_pnt, decimals,
     &		'(mean) RA of pointing direction (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_PNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'DEC_PNT', 
     &		dec_pnt, decimals,
     &		'(mean) dec of pointing direction (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_PNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'PA_PNT', 
     &		pa_pnt, decimals,
     &		'(mean) roll angle (degrees) from North towards East',
     &		status)
	   message = wrnstr // ' Putting PA_PNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c Quick little check & warning if necessary
	if((ra_pnt.EQ.0.0).AND.(dec_pnt.EQ.0.0).AND.(pa_pnt.EQ.0.0))then
	   message = wrnstr //
     &		' RA_PNT,DEC_PNT,PA_PNT zero'
	   call FTPCOM(ounit, message, status)
	   call FCECHO(message)
c	   message = ' ... possible error '
c	   call FTPCOM(ounit, message, status)
c	   call FCECHO(message)
	   message = wrnstr // ' Problem writing Comment record'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

c Errors on Pointing direction
	if(ra_pnte.GT.0) then
	   call FTPKYF(ounit, 'RA_PNTE', 
     &		ra_pnte, decimals,
     &		'1-sigma Error on RA_PNT (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_PNTE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(dec_pnte.GT.0) then
	   call FTPKYF(ounit, 'DEC_PNTE', 
     &		dec_pnte, decimals,
     &		'1-sigma Error on DEC_PNT (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_PNTE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(pa_pnte.GT.0) then
	   call FTPKYF(ounit, 'PA_PNTE', 
     &		pa_pnte, decimals,
     &		'1-sigma Error on PA_PNTE (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_PNTE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif


	Return
	End
