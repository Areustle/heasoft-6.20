*+CGDR
	subroutine cgdr(envar,dir,status)

	implicit none
	character*(*) envar, dir
	integer status

C-----------------------------------------------------------------------
C Description: Returns the current directory in a format acceptible as
C              input to the subroutine cpthnm for the argument 'dir'.  
C
C              On UNIX machines, the envar argument value is translated 
C              as an environment variable and a number of characters
C              equivalent to the length of the translated environment 
C              variable is removed from the beginning of the current 
C              directory.  The remainder of the current directory is 
C              returned to the argument dir.
C              
C              On VMS machines, the envar argument is ignored.  However,
C              everything after the '[' character but before the
C              ']' character is returned to the argument dir after
C              overwriting all '.' characters with '/' characters.
C
C              For example, if envar = '/FTP/caldb', and this subroutine
C              was executed in the directory, 
C              '/FTP/caldb/data/asca/gis/bcf', then the value,
C
C                   'data/asca/gis/bcf'
C
C              would be returned to the dir argument.  If this routine 
C              were executed on a VMS machine in the directory 
C              CALDB:[DATA.ASCA.GIS.BCF], then the same value would be
C              returned.
C
C              If any errors occurr during the execution of this routine
C              a non-zero status flag is returned.
C
C Origin:      Written for the Calibration Database
C
C Arguments:   envar   (i): Environment variable used to strip off 
C                           beginning of current directory.  Ignored on
C                           VMS machines.
C              dir     (r): The current directory in system independent
C                           format.
C              status  (r): The success status of this routine. Non-zero
C                           means that an error occurred.
C
C Authors/Modification History:
C Ron Zellar  Aug  8 1994 -- Original Version.
C MFC         Apr 19, 2006 -- added check to see if $CALDB includes
C               trailing "/" (version 1.0.1)
C-----------------------------------------------------------------------
*-Version 1.0.1

	character(160) cwd,envdir
	integer fcstln,envlen,cwdlen,skip

C	Get the current working directory
	call gtdir(cwd)

C	Translate the environment variable
	envlen = fcstln(envar)
	call ctrlog(envar,envlen,envdir,envlen)


C	Make sure that envar has been defined.
	if (envlen .eq. 0) then
	     status = 1
	     return
	endif

C	See if the environment variable matches the beginning of the cwd
	if (cwd(:envlen) .ne. envdir(:envlen)) then
	     status = 2
	     return
	endif

	cwdlen = fcstln(cwd)
	if (cwdlen .eq. envlen) then
	     dir = '/'
	     return
	endif
        
        
C	Write cwd without envar to dir argument.  (Use +2 because we need
C       to skip over '/' characer after cwd(:envlen).)
C
C       MFC: add check to see if last character is '/' (sometimes
C       $CALDB might include a trailing / by mistake)
C       and adjust skip accordingly
C
        if (cwd(envlen:envlen) .eq. '/') then 
           skip=1 
           else 
           skip=2
           endif
	dir = cwd(envlen+skip:cwdlen)
C        write(*,*) 'MFC: CGDR: DIR ',dir,envlen

	return
	end

