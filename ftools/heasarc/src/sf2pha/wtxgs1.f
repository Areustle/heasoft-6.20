*+WTXGS1
      subroutine wtxgs1(ounit,chatter, blw_start, blw_stop,
     & 		nomgain, gaincor, gsdcol,gsdarea,
     &		qlinchan, linchan, qgaincor, ierr)
 
        IMPLICIT NONE
        integer ounit, chatter, ierr, blw_start, blw_stop
	integer linchan(4)
        real nomgain, gaincor, gsdcol,gsdarea
        logical qlinchan(4), qgaincor
c
c DESCRIPTION 
c   This subroutine writes an EXOSAT GSPC-specific keywords to the 
c **CURRENT** header unit of a PHA file (No Detector extension defined
c or necessary for the GSPC).
c !!! Note !!! the o/p file is assumed to have been opened, and wound to the 
c              desired location. The file is left open at the end of the 
c              newly written keywords on return and MUST be closed 
c              using FTCLOS or another extension written starting with FTCRHD
c	       in order that the mandatory END keyword is written              
c The following keywords are also written:
c  ... incomplete ...
c
c PASSED VARAIABLES
c  OUNIT	i   : FORTRAN unit number of open PHA file
c  CHATTER      i   : chattiness flag for o/p (5 quite,10 norm,>19 silly)
c  ... incomplete ...
c  IERR           o : Return Errror flag (0 = OK)
c                          
c CALLED ROUTINES etc 
c  ... incomplete ...
c 
c AUTHORS/MODIFICATION HISTORY 
c
c Ian M George (1.0.0: 1993 Oct 13), original
      character(5) version
      parameter (version = '1.0.0')
*-
c INTERNALS 
c
	integer status, decimals, i
	parameter (decimals=6)
	character(8) keywrd
	character(30) errstr, wrnstr
      	character(70) message, string
	logical qgaincapp

c Initialization
	ierr = 0
	status = 0
	errstr = '** WTXGS1 ERROR :'
	wrnstr = '** WTXGS1 WARNING :'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         message = ' ... using WTXGS1 Ver '//version
         call fcecho(message)
      ENDIF

c Add history card related to this programme
        write(message,'(2a)')
     &		' EXOSAT GSPC keywords written by WTXGS1 ',
     &                          version
        call FTPHIS(ounit,message,status)
        message = wrnstr // ' Putting HISTORY record'
        call wt_ferrmsg(status, message)
        status = 0


c --- WRITE KEYWORDS 
c

	call FTPKYJ(ounit,'BLMIN ',
     &		blw_start,
     & 		'burst length window start',
     &		status)
	message = wrnstr // ' Putting BLMIN keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYJ(ounit,'BLMAX ',
     &		blw_stop,
     & 		'burst length window stop',
     &		status)
	message = wrnstr // ' Putting BLMAX keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'GAIN_NOM ',
     &		nomgain,decimals,
     & 		'nominal detector gain',
     &		status)
	message = wrnstr // ' Putting GAIN_NOM keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'GAINC ',
     &		gaincor,decimals,
     & 		'gain correction factor',
     &		status)
	message = wrnstr // ' Putting GAINC keyword '
	call wt_ferrmsg(status, message)
	status = 0

	qgaincapp = .false.
           call FTPKYL(ounit, 'GAINCAPP',
     &          qgaincapp,
     &          'Gain Correction applied ?',
     &          status)
            message = wrnstr // ' Putting GAINCAPP keyword'
            call wt_ferrmsg(status,message)
            status = 0

	if(qgaincor) then
		string = 'Gain correction is reliable'
	else
		string = 'Gain correction is NOT reliable'
	endif
           call FTPKYL(ounit, 'GAINGOOD',
     &          qgaincor,
     &          string,
     &          status)
            message = wrnstr // ' Putting GAINGOOD keyword'
            call wt_ferrmsg(status,message)
            status = 0

	call FTPKYF(ounit,'GEOMAREA',
     &		gsdarea,decimals,
     & 		'geometric area on-axis (cm**2)',
     &		status)
	message = wrnstr // ' Putting GEOMAREA keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'OBSFACT',
     &		gsdcol,decimals,
     & 		'obscuration factor (collimator response)',
     &		status)
	message = wrnstr // ' Putting OBSFACT keyword '
	call wt_ferrmsg(status, message)
	status = 0

	do i = 1, 4
	 if(qlinchan(i)) then
	   write(keywrd,'(a7,i1)') 'LINCHAN', i
           call FTPKYJ(ounit, 'LINCHAN',
     &          linchan(i),
     &          'GSPC line channel',
     &          status)
            message = wrnstr // ' Putting '//keywrd//' keyword'
            call wt_ferrmsg(status,message)
            status = 0
	 endif
	enddo

998	if(ierr.NE.0) then
	  message = wrnstr // 'Incomplete execution'
	  call fcecho(message)
	endif

      return
      end
