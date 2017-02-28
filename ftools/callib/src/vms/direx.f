*+DIREX
	subroutine direx(dir,exist)

	implicit none
	character*(*) dir
	logical exist

C-----------------------------------------------------------------------
C Description: If the directory specified by the dir argument exists
C              the argument exist is returned as true.
C
C Arguments:   dir    (i): the name of the directory to check
C              exist  (r): true if dir exists, false otherwise
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C Ron Zellar Sep 14 1994 -- Original Version
C-----------------------------------------------------------------------
*-

	character(160) dirdmy
	integer i,len,fcstln

	dirdmy = dir
	len = fcstln(dirdmy)

	do 100 i=1,len
	     if (dirdmy(len+1-i:len+1-i).eq.'.')then
	          dirdmy(len+1-i:len+1-i) = ']'
	          goto 101
	     endif
100	continue
101	continue

	dirdmy(len:len+3)='.dir'

	inquire(file=dirdmy,exist=exist)
	return
	end
