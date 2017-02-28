*+MKCLDR
	subroutine mkcalr

	implicit none

C-----------------------------------------------------------------------
C Description: Reads the file given by the value of the CALDBCONFIG
C              environment variable or logical, and creates the 
C              directories found there.
C
C Arguments:   NONE
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar   (1.0.0:94 Sep 13) Original Version
C  Ron Zellar   (2.0.0:94 Sep 28) Added the mkcifs parameter
C  Ian M George (3.0.0:95 Dec 29) verbose -> chatter & Cleaned up
        character(7) version
        parameter (version = '3.0.0')
C-----------------------------------------------------------------------
*-
c Internals
        character(40) taskname
	logical mkcifs
	integer errstat, chatter
c Initialize
	taskname = 'mkcaldir'
	errstat = 0

C Get the parameters
	call gpmkcd(chatter,mkcifs,errstat)
	if (errstat.ne.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

C Make the Caldb directories
	call mkcdir(chatter,mkcifs,errstat)
        if (errstat .ne. 0) goto 148

c Finish-Off
148     continue
        call wtendm(taskname, version, errstat, chatter)

	return
	end
C -------------------------------------------------------------------------
*+GPMKCD
	subroutine gpmkcd(chatter,mkcifs,status)

	implicit none
	logical mkcifs
	integer status, chatter

C-----------------------------------------------------------------------
C Description: Gets the parameters for the mkcaldir task
C
C Arguments:   chatter (i): Chattiness flag (zero = silent)
C              mkcifs  (i): whether or not the mkcaldir task should 
C                           create Index files.
C              status  (r): the success status of this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar   (1.0.0:94 Sep 13) Original Version
C  Ron Zellar   (2.0.0:94 Sep 28) Added the mkcifs parameter
C  Ian M George (3.0.0:95 Dec 29) verbose -> chatter & Cleaned up
        character(7) version
        parameter (version = '3.0.0')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'gpmkcd')
	integer errstat
	character(160) contxt
C Initialize internal and external status flags
	status = 0
	errstat = 0

c Get the chattiness flag
        call uclgsi('chatter',chatter, errstat)
        if(errstat.NE.0) then
          call wtwarm(subname, version, 1, 1,
     &          'Problem getting CHATTER parameter')
                errstat = 0
                call wtinfo(1,1,1, 'setting CHATTER = 10')
                chatter = 10
        endif

C Get the mkcifs parameter
	call uclgsb('mkcifs',mkcifs,errstat)
	if (errstat.ne.0) then
          call wtwarm(subname, version, 1, 1,
     &          'Problem getting MKCIFS parameter')
                errstat = 0
                call wtinfo(1,1,1, 'setting MKCIFS = FALSE')
                mkcifs = .false.
	endif

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)


999     if(status.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

	return
	end
C-----------------------------------------------------------------------
