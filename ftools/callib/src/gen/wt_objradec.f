*+ WT_OBJRADEC
	subroutine wt_objradec(chatter, ounit, 
     &  qsys, radecsys, equinox, 
     &	ra_obj, dec_obj, ra_obje, dec_obje, 
     &	ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	real equinox	
	real ra_obj, dec_obj, ra_obje, dec_obje
	character(70) radecsys
	logical qsys

c
c Description
c   Writes the RA & dec of the object/target
c and any associated errors into current FITS header unit. 
c
c The following keywords can be written (depending upon flags/passed values)
c  RADECSYS- 'Stellar Reference frame used for EQUINOX'
c  EQUINOX - 'Equinox of all RA,dec specifications (AD year)'
c  RA_OBJ  - 'RA of object (degrees)'
c  DEC_OBJ - 'dec of object (degrees)'
c  RA_OBJE - 'Error on RA_OBJ (degrees)'
c  DEC_OBJE- 'Error on DEC_OBJ (degrees)'
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
c  QSYS         i/r : Logical flag whether pointing info to be written
c  RADECSYS     i   : Reference frame for ALL RA & decs
c  EQUINOX      i   : Equinox for ALL RA & decs
c  RA_OBJ       i   : RA of object/target 
c  DEC_OBJ      i   : dec of object/target
c  RA_OBJE      i   : Error on RA_OBJ
c  DEC_OBJE     i   : Error on DEC_OBJ 
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
	errstr = '** WT_OBJRADEC '//version//' ERROR: '
	wrnstr = '** WT_OBJRADEC '//version//' WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WT_OBJRADEC '// version
		call fcecho(message)
	endif

c Stick in the necessary stellar reference frame is any RA & decs to be listed
c ... adding a history record too
	if(qsys) then
	   qsys = .false.
	   message = 
     &     ' RADECSYS & EQUINOX keywords written by WT_OBJRADEC '
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

	   message = ' Pointing Info written by WT_OBJRADEC '
     &		// version
	   call FTPHIS(ounit, message, status)
	   message = wrnstr // ' Problem writing History record'
	   call wt_ferrmsg(status,message)
	   status = 0


c Object Position
	   call FTPKYF(ounit, 'RA_OBJ', 
     &		ra_obj, decimals,
     &		'RA of object (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_OBJ keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'DEC_OBJ', 
     &		dec_obj, decimals,
     &		'dec of object (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_OBJ keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

c Quick little check & warning if necessary
	if((ra_obj.EQ.0.0).AND.(dec_obj.EQ.0.0)) then
	   message = wrnstr //
     &		' RA_OBJ,DEC_OBJ zero'
	   call FTPCOM(ounit, message, status)
	   call FCECHO(message)
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

c Errors on Pointing direction
	if(ra_obje.GT.0) then
	   call FTPKYF(ounit, 'RA_OBJE', 
     &		ra_obje, decimals,
     &		'1-sigma Error on RA_OBJ (degrees)',
     &		status)
	   message = wrnstr // ' Putting RA_OBJE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(dec_obje.GT.0) then
	   call FTPKYF(ounit, 'DEC_OBJE', 
     &		dec_obje, decimals,
     &		'1-sigma Error on DEC_OBJ (degrees)',
     &		status)
	   message = wrnstr // ' Putting DEC_OBJE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif


	Return
	End

