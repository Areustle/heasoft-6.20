*+ WT_SCRADEC
	subroutine wt_scradec(chatter, ounit, 
     &  qsys, qscraftx,qscrafty,qscraftz,
     &  radecsys, equinox, 
     &  ra_scx, dec_scx, pa_scx,
     &  ra_scy, dec_scy, pa_scy,
     &  ra_scz, dec_scz, pa_scz,
     &  ra_scxe, dec_scxe, pa_scxe,
     &  ra_scye, dec_scye, pa_scye,
     &  ra_scze, dec_scze, pa_scze,
     &	ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	real equinox	
	real ra_scx, dec_scx, ra_scy, dec_scy, ra_scz, dec_scz
	real pa_scx, pa_scy, pa_scz
	real ra_scxe, dec_scxe, ra_scye, dec_scye, ra_scze, dec_scze
	real pa_scxe, pa_scye, pa_scze
	character(70) radecsys
	logical qsys, qscraftx,qscrafty,qscraftz

c
c Description
c   Writes the RA & dec of the s/c X-, Y-, &/or Z-axes to the 
c current FITS header unit
c
c Description
c   Writes the Observation details to the current FITS header unit
c The following keywords can be written (depending upon flags/passed values)
c  RADECSYS- 'Stellar Reference frame used for EQUINOX'
c  EQUINOX - 'Equinox of all RA,dec specifications (AD year)'
c  RA_SCX  - 'RA of s/c X-axis (degrees)'
c  DEC_SCX - 'Dec of s/c X-axis (degrees)'
c  PA_SCX  - 'Roll angle of s/c X-axis (degrees)
c  RA_SCY  - 'RA of s/c y-axis (degrees)'
c  DEC_SCY - 'Dec of s/c Y-axis (degrees)'
c  PA_SCY  - 'Roll angle of s/c Y-axis (degrees)
c  RA_SCZ  - 'RA of s/c Z-axis (degrees)'
c  DEC_SCZ - 'Dec of s/c Z-axis (degrees)'
c  PA_SCZ  - 'Roll angle of s/c Z-axis (degrees)
c  RA_SCXE -  Error on RA_SCX (degrees)
c  DEC_SCXE-  Error on DEC_SCX (degrees)
c  PA_SCXE -  Error on PA_SCX (degrees)
c  RA_SCYE -  Error on RA_SCY (degrees)
c  DEC_SCYE-  Error on DEC_SCY (degrees)
c  PA_SCYE -  Error on PA_SCY (degrees)
c  RA_SCZE -  Error on RA_SCZ (degrees)
c  DEC_SCZE-  Error on DEC_SCZ (degrees)
c  PA_SCZE -  Error on PA_SCZ (degrees)
c 
c Passed Parameters
c  CHATTER	i   : Chatter flag (<5=quite,10=normal,>20=silly)
c  OUNIT        i   : FORTRAN logical unit no. of o/p file
c  QSYS 	i/r : Logical flag whether coord sys info to be written
c  QSCRAFTX     i   : Logical flag whether s/c X-axis info to be written
c  QSCRAFTY     i   : Logical flag whether s/c Y-axis info to be written
c  QSCRAFTZ     i   : Logical flag whether s/c Z-axis info to be written
c  RADECSYS     i   : Reference frame for ALL RA & decs
c  EQUINOX      i   : Equinox for ALL RA & decs
c  RA_SCX       i   : RA along direction of s/c X-axis 
c  DEC_SCX      i   : dec along direction of s/c X-axis 
c  PA_SCX       i   : position angle (roll) of s/c X-axis
c  RA_SCY       i   : RA along direction of s/c Y-axis 
c  DEC_SCY      i   : dec along direction of s/c Y-axis 
c  PA_SCY       i   : position angle (roll) of s/c Y-axis
c  RA_SCZ       i   : RA along direction of s/c Z-axis 
c  DEC_SCZ      i   : dec along direction of s/c Z-axis 
c  PA_SCZ       i   : position angle (roll) of s/c Z-axis
c  RA_SCXE 	i   : Error on RA_SCX (degrees)
c  DEC_SCXE	i   : Error on DEC_SCX (degrees)
c  PA_SCXE      i   : Error on PA_SCX (degrees)
c  RA_SCYE 	i   : Error on RA_SCY (degrees)
c  DEC_SCYE	i   : Error on DEC_SCY (degrees)
c  PA_SCYE      i   : Error on PA_SCY (degrees)
c  RA_SCZE 	i   : Error on RA_SCZ (degrees)
c  DEC_SCZE     i   : Error on DEC_SCZ (degrees)
c  PA_SCZE      i   : Error on PA_SCZ (degrees)
c  IERR	          o : Return error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c  subroutine FTPHIS            : (FITSIO) writes a history keyword
c  subroutine FTPKYn            : (FITSIO) writes a keyword of type n
c  subroutine WT_FERRMSG        : (CALLIB) writes FITSIO error message etc
c
c Origin
c   Hacked from old general routine "WT_RADEC" which grew too big
c
c Authors/Modification History
c  Ian M George    (1.0.0: 1993 Oct 04), original hacked version
c  Ian M George    (1.0.1: 1993 Oct 22), moved RA,DEC = 0.0 warning messages
	character(7) version
	parameter (version = ' 1.0.1 ')
*-

c Internals
	integer status, decimals
	parameter (decimals = 6)
	character(40) errstr, wrnstr
	character(80) message

c Initialize	
	errstr = '** WT_SCRADEC '//version//' ERROR: '
	wrnstr = '** WT_SCRADEC '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WT_SCRADEC '// version
		call fcecho(message)
	endif

c Stick in the necessary stellar reference frame is any RA & decs to be listed
c ... adding a history record too
	if(qscraftx.OR.qscrafty.OR.qscraftz) then

	message = ' Spacecraft Celstial Info written by WT_SCRADEC '
     &		// version
	call FTPHIS(ounit, message, status)
	message = wrnstr // ' Problem writing History record'
	call wt_ferrmsg(status,message)
	status = 0
	endif

	if(qsys.AND.(qscraftx.OR.qscrafty.OR.qscraftz)) then
           qsys = .false.
           message=' RADECSYS & EQUINOX keywords written by WT_SCRADEC '
     &          // version
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

c Spacecraft Orientation
c ... s/c X-axis
	if(qscraftx) then	
	   call FTPKYF(ounit, 'RA_SCX', 
     &		ra_scx, decimals,
     &		'RA of s/c X-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'DEC_SCX', 
     &		dec_scx, decimals,
     &		'Dec of s/c X-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c 	..... Quick little check & warning if necessary
		if((ra_scx.EQ.0.0).AND.(dec_scx.EQ.0.0)) then
	   	  message = wrnstr //
     &			' RA_SCX,DEC_SCX zero'
	   	  call FTPCOM(ounit, message, status)
	   	  call FCECHO(message)
	   	  call wt_ferrmsg(status,message)
	   	  status = 0
		endif

	   if(pa_scx.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCX', 
     &		pa_scx, decimals,
     &		'Position angle (N thru E) of s/c X-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif


	   if(ra_scxe.GT.0) then
	   call FTPKYF(ounit, 'RA_SCXE', 
     &		ra_scxe, decimals,
     &		'Error on RA_SCX (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCXE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(dec_scxe.GT.0) then
	   call FTPKYF(ounit, 'DEC_SCXE', 
     &		dec_scxe, decimals,
     &		'Error on DEC_SCX (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCXE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(pa_scxe.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCXE', 
     &		pa_scxe, decimals,
     &		'Error on PA_SCX (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCXE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif
	endif

c ... s/c Y-axis
	if(qscrafty)then
	   call FTPKYF(ounit, 'RA_SCY', 
     &		ra_scy, decimals,
     &		'RA of s/c y-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCY keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'DEC_SCY', 
     &		dec_scy, decimals,
     &		'Dec of s/c Y-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCY keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c 	..... Quick little check & warning if necessary
		if((ra_scy.EQ.0.0).AND.(dec_scy.EQ.0.0)) then
	   	  message = wrnstr //
     &			' RA_SCY,DEC_SCY zero'
	   	  call FTPCOM(ounit, message, status)
	   	  call FCECHO(message)
	   	  call wt_ferrmsg(status,message)
	   	  status = 0
		endif

	   if(pa_scy.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCY', 
     &		pa_scy, decimals,
     &		'Position angle (N thru E) of s/c Y-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCY keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(ra_scye.GT.0) then
	   call FTPKYF(ounit, 'RA_SCYE', 
     &		ra_scye, decimals,
     &		'Error on RA_SCY (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCYE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(dec_scye.GT.0) then
	   call FTPKYF(ounit, 'DEC_SCYE', 
     &		dec_scye, decimals,
     &		'Error on DEC_SCY (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCYE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(pa_scye.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCYE', 
     &		pa_scye, decimals,
     &		'Error on PA_SCY (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCYE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif
	endif

c ... s/c Z-axis
	if(qscraftz) then
	   call FTPKYF(ounit, 'RA_SCZ', 
     &		ra_scz, decimals,
     &		'RA of s/c Z-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCZ keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'DEC_SCZ', 
     &		dec_scZ, decimals,
     &		'Dec of s/c Z-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCY keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c 	..... Quick little check & warning if necessary
		if((ra_scy.EQ.0.0).AND.(dec_scy.EQ.0.0)) then
	   	  message = wrnstr //
     &			' RA_SCY,DEC_SCY zero'
	   	  call FTPCOM(ounit, message, status)
	   	  call FCECHO(message)
	   	  call wt_ferrmsg(status,message)
	   	  status = 0
		endif

	   if(pa_scz.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCZ', 
     &		pa_scz, decimals,
     &		'Position angle (N thru E) of s/c Z-axis (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCZ keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(ra_scze.GT.0) then
	   call FTPKYF(ounit, 'RA_SCZE', 
     &		ra_scze, decimals,
     &		'Error on RA_SCZ (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_SCZE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(dec_scze.GT.0) then
	   call FTPKYF(ounit, 'DEC_SCZE', 
     &		dec_scze, decimals,
     &		'Error on DEC_SCZ (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_SCZE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   if(pa_scze.GT.0.0) then
	   call FTPKYF(ounit, 'PA_SCZE', 
     &		pa_scze, decimals,
     &		'Error on PA_SCZ (degrees)',
     &		status)
	   message = wrnstr // ' Putting PA_SCZE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	endif


	Return
	End
