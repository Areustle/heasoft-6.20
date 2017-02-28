*+OCNFG
	subroutine ocnfg(unit,chatter,config,status)

	implicit none
	integer unit, status, chatter
	character*(*) config

C-----------------------------------------------------------------------
C Description: Opens the file specified by the CALDBCONFIG environment
C              variable with a logical unit number specified by unit.
C
C Arguments:   unit   (i): the logical unit number assigned to the
C                          caldbconfig file after it is opened
C              chatter(i): chattiness flag
C              config (r): the name of caldbconfig file opened
C              status (r): the success status of this routine.  Non-zero
C                          indicates failure
C
C Origin:      Based on (and replaces) code of Ron Zellar
C
C Authors/Modification History:
c  Ian M George  (1.0.0: 95 Dec 28) original hack of Ron Zellar's code
c  Ian M George  (1.0.1: 96 Jun 03) Tweaks to internal chatter settings
        character(7) version
        parameter (version = '1.0.1')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'ocnfg')
	integer envlen,fcstln,configlen,errstat
	character(20) envar
	character(160) contxt
C Initialize the internal and external status flags
	errstat = 0
	status = 0
C Set the name of the environment variable used in this routine
	envar = 'CALDBCONFIG'

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,21,2,contxt)

C Translate the caldbconfig environment variable
	envlen = fcstln(envar)
	call ctrlog(envar,envlen,config,configlen)
	if (configlen .eq. 0) then
           call wterrm(subname, version,
     &          'CALDBCONFIG environ-var/logical not set')
           contxt = 'The env-var/logical '//
     &		'CALDBCONFIG must be set to point your local Caldb '//
     &          'configuration file'
           call wtinfo(chatter,5,1, contxt)
           contxt = 'See the Caldb Users Guide (CAL/GEN/94-002)'//
     &          ' for details'
           call wtinfo(chatter,5,2,contxt)
           status = 10
           goto 999
        endif

C Open the caldbconfig file
	call faopnw(unit,config,1,-1,errstat)
	if (errstat .ne. 0) then
	   contxt='Unable to open Caldb configuration file: '
     &	          //config(:fcstln(config))
           call wterrm(subname, version, contxt)
	   status = 20
	   goto 999
	endif


999	if(status.ne.0) then
           call wterrm(subname, version, 
     &		'Serious problem with CALDB set-up')
	endif

	return
	end
