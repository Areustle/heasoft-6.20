*+WTFWRN
        subroutine wtfwrn(subrout, version, chatter, wtchatter, 
     &		status, string)

	IMPLICIT NONE
        character*(*) subrout, version, string
	integer status, chatter, wtchatter
c 
c Description:
c  Writes callib/roslib standard warning message string(s) to STDOUT, 
c  appending the FITSIO error message corresponding to the code 
c  passed down via status
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  CHATTER       i   : (int) actual chatter flag
c  WTCHATTER     i   : (int) chatter level at or above which messages written
c  STATUS        i   : (int) FITSIO status flag
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  subroutine FTGERR     : (FITSIO) Gets FITSIO error string from status
c  subroutine FTVERS     : (FITSIO) Gets FITSIO version number
c  subroutine WTINFO     : (CALLIB) Writes callib/roslib info message
c  subroutine WTWARM     : (CALLIB) Writes callib/roslib warning message
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c	character(7) version
c	parameter (version = '1.0.0')
*- 
c Internals 
	character(80) ftsmsg
	character(160) message
	character(6) cftsver	
	real ftsver

c Return if there's no error
	if(status.EQ.0) return

c Return if running in silent mode
	if(chatter.LT.wtchatter) return

c Dump the user-defined error message
	call wtwarm(subrout, version, chatter, wtchatter, string)
	
c Get & dump the FITSIO message & version
	call ftgerr(status, ftsmsg)
	call ftvers(ftsver)	
	write(cftsver,'(f6.3)') ftsver
	message = 'fitsio'//cftsver//' error message: '//ftsmsg
	call wtinfo(chatter,wtchatter,1,message)

	return
	end
c -------------------------------------------------------------------
