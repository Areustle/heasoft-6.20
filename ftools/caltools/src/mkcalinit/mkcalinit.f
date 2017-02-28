*+MCINT
	subroutine mkcalt

	implicit none
C-----------------------------------------------------------------------
C Description: Generates the caldbinit file
C
C Arguments:   NONE
C
C Origin:      written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar    (1.0.0: 94 Sep 20) Original Version
C  Ian M George  (1.1.0: 95 Dec 21) added wtinfo & friends
        character(7) version
        parameter (version = '1.1.0')
C-----------------------------------------------------------------------
*-
c Internals
	character(40) taskname
	integer errstat, chatter
	character(160) configdir

c Initialize
        taskname = 'mkcalinit'
	errstat = 0

c Go get the parameters
        call gmkclnt(configdir, chatter, errstat)
        if(errstat.NE.0) goto 148

c Start-up Main
        call wtbegm(taskname, version, chatter)

C Construct the caldbinit file
        call mkcint(configdir, chatter, errstat)
        if (errstat .ne. 0) goto 148

c Sign-Off
148     continue
        call wtendm(taskname, version, errstat, chatter)

	return
	end
c -----------------------------------------------------------------
*+GMKCLNT
	subroutine gmkclnt(configdir,chatter,status)

	implicit none
	character*(*) configdir
	integer status, chatter

C Description: 
C  Gets the parameters for the task mkcalinit
C
C Arguments:
C   CONFIGDIR        r : Path to the caldb.config file
C   CHATTER          r : Chattiness Flag
C   STATUS           r : Error flag (zero = OK)
C
C Origin:      
C   Written for the Calibration Database by Ron Zellar (as gpmcb), then 
C seriously hacked to include all the latest gizzmos by Ian M George
C
C Authors/Modification History:
C  Ron Zellar    (1.1.0: 94 Sep 28) -- Original Version (of gpmcb)
c  Ian M George  (1.1.1: 95 Dec 29) converted for the mkcalinit task
        character(7) version
        parameter (version = '1.1.1')
C-----------------------------------------------------------------------
*-
c Internals
        character(7) subname
        parameter (subname = 'gmkclnt')
	integer errstat
	character(160) contxt
c Initialize
	status = 0

c Get the configdir
	call uclgst('configdir',configdir,errstat)
        if(errstat.ne.0) then
          call wterrm(subname, version,
     &          'Problem getting CONFIGDIR parameter')
          status = 1
          goto 999
        endif

c Get the chattiness flag
        call uclgsi('chatter',chatter, errstat)
        if(errstat.NE.0) then
          call wtwarm(subname, version, 1, 1,
     &          'Problem getting CHATTER parameter')
                errstat = 0
                call wtinfo(1,1,1, 'setting CHATTER = 10')
                chatter = 10
        endif

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)


999     if(status.ne.0) then
          call wterrm(subname, version, ' unable to continue')
        endif

	return
	end

c -------------------------------------------------------------
