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
C              status  (r): The success status of this routine.  Non-zero
C                           means that an error occurred.
C
C Authors/Modification History:
C Ron Zellar  Aug  8 1994 -- Original Version.
C-----------------------------------------------------------------------
*-Version 1.0

	character(160) cwd
	integer fcstln,cwdlen,index,start,i

C	initialize returned value
	dir = ' '

C	Get the current working directory
	call gtdir(cwd)

C	find beginning of directory names
	start = index(cwd,'[')

C	Copy directory names into dir argument one character at a time
C	changing '.' characters to '/' characters.  I assume that the
C       last characer in cwd is a ']' character.
	cwdlen = fcstln(cwd)
	Do 100 i=1,cwdlen-start-1
	     if (cwd(start+i:start+i) .eq. '.') then
	          dir(i:i) = '/'
	     else
	          dir(i:i) = cwd(start+i:start+i)
	     endif
100	continue

	return
	end
