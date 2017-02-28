*+WTXTG1
      subroutine wtxtg1(ounit,chatter,
     &          x0, y0, width, angle, sscorr,
     &          ierr)

        IMPLICIT NONE
        integer ounit, chatter, ierr, width
        real x0,y0,angle, sscorr
c
c DESCRIPTION 
c   This subroutine writes an EXOSAT TGS-specific keywords to the 
c **CURRENT** header unit of a PHA file (No Detector extension defined
c or necessary for the TGS).
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
c Ian M George (1.0.0: 1994 Feb 22), original
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
	errstr = '** WTXTG1 ERROR :'
	wrnstr = '** WTXTG1 WARNING :'

c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         message = ' ... using WTXTG1 Ver '//version
         call fcecho(message)
      ENDIF

c Add history card related to this programme
        write(message,'(2a)')
     &		' EXOSAT TGS keywords written by WTXTG1 ',
     &                          version
        call FTPHIS(ounit,message,status)
        message = wrnstr // ' Putting HISTORY record'
        call wt_ferrmsg(status, message)
        status = 0


c --- WRITE KEYWORDS 
c

	call FTPKYF(ounit,'STRIP_X0 ',
     &		x0,decimals,
     & 		'Zero location in X (pixels)',
     &		status)
	message = wrnstr // ' Putting STRIP_X0 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'STRIP_Y0 ',
     &		Y0,decimals,
     & 		'Zero location in Y (pixels)',
     &		status)
	message = wrnstr // ' Putting STRIP_Y0 keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYF(ounit,'STRIPANG ',
     &		angle,decimals,
     & 		'Strip angle wrt Y-axis (degrees)',
     &		status)
	message = wrnstr // ' Putting STRIPANG keyword '
	call wt_ferrmsg(status, message)
	status = 0

	call FTPKYJ(ounit,'STRIPWID ',
     &		width,
     & 		'Strip width (pixels)',
     &		status)
	message = wrnstr // ' Putting STRIPWID keyword '
	call wt_ferrmsg(status, message)
	status = 0
	call FTPKYF(ounit,'SSIGCORR ',
     &		sscorr,decimals,
     & 		'Sum Signal Correction',
     &		status)
	message = wrnstr // ' Putting SSIGCORR keyword '
	call wt_ferrmsg(status, message)
	status = 0

998	if(ierr.NE.0) then
	  message = wrnstr // 'Incomplete execution'
	  call fcecho(message)
	endif

      return
      end
