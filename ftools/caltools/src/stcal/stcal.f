*+STCL
	subroutine stcal

	implicit none

C-----------------------------------------------------------------------
C Description: Moves a user specified list of files into the Caldb
C              based on their storage location as read from the
C              caldbconfig file.
C
C Arguments:   NONE
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar Sep 15 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	character(160) files
	integer errstat
	logical verbose

	character(40) taskname
	common /task/ taskname
	taskname = 'stcal'

	errstat = 0

C	Get parameter values from the par file
	call gpstcl(files,verbose,errstat)

	if (errstat.ne.0) return

C	Store the calibration files
	call strcal(files,verbose,errstat)

	return
	end

*+GPSTCL
	subroutine gpstcl(files,verbose,status)

	implicit none
	character*(*) files
	logical verbose
	integer status 

C-----------------------------------------------------------------------
C Description: Gets the parameters from the stcal par file
C
C Arguments:   files   (i): The value of the files parameter 
C              verbose (i): print info messages to screen
C              status  (r): the success status of this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification history:
C Ron Zellar Sep 15 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	integer errstat
	character(160) contxt

C	Get the list of files to store
	call uclgst('files',files,errstat)
	if (errstat.ne.0) then
	     contxt='Cannot get the files parameter'
	     call fcerr(contxt)
	     status = 1
	     return
	endif

C	Get the verbose parameter
	call uclgsb('verbose',verbose,errstat)
	if (errstat.ne.0) then
	     contxt='Cannot get the verbose parameter'
	     call fcerr(contxt)
	     status = 2
	     return
	endif

	return
	end
