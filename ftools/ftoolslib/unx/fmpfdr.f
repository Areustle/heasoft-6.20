C******************************************************************************
C SUBROUTINE:
C      fmpfdr
C
C DESCRIPTION:
C	System specific subroutine to modify the PFILES environment variable
C       (logical) to point at the refdata directory
C
C AUTHOR/DATE:
C       Ron Zellar  2/16/94 
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call fmpfdr(pfdir,status)
C
C ARGUMENTS:
C      pfdir - the last path in the pfiles env var (logical)
C      status - success status for this routine
C
C PRIMARY LOCAL VARIABLES:
C      slash - location of the last '/' character in pfdir
C
C CALLED ROUTINES:
C
C******************************************************************************
	subroutine fmpfdr(pfdir,status)

	implicit none
	character*(*) pfdir
	integer status,actlen, fcstln,len,i,slash
	character(80) context

C	initialize variables
	slash = 0

C	get actual length of pfdir
	actlen = fcstln(pfdir)

C	search backwards from end of pfdir for '/'
	do 100 i=1,(actlen-1)
		if (pfdir(actlen-i:actlen-i) .eq. '/') then
			slash = actlen - i
			goto 101
		endif
100	continue
	status = 10
	context = 'fmpfdr: Cannot find "/" in PFILES'
	call fcerr(context)
	return

101	continue

C	make sure there is enough room to add 'refdata/'
	if (len(pfdir(slash+1:)) .lt. 8) then
		status = 20
		context = 'fmpfdr: Not enough room in pfdir to modify PFILES'
		call fcerr(context)
		return
	endif

C	append 'refdata/' after '/'
	pfdir(slash+1:) = 'refdata/'

	return
	end
