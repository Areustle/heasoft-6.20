*+WTINFO
        subroutine wtinfo(chatter, wtchatter, level, string)

	IMPLICIT NONE
	integer level, chatter, wtchatter
        character*(*) string
c 
c Description:
c  Noddy routine which passes string and level onto the standard wtout 
c subroutine if chatter.GE.wtchatter
c
c Passed parameters
c  CHATTER       i   : (int) actual chatter flag
c  WTCHATTER     i   : (int) chatter flag at & or above which wtout called
c  LEVEL         i   : (int) importance level (see above)
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  subroutine WTOUT      : (CALLIB) Standard roslib/callib string writer
c
c Compilation & Linking
c  link with CALLIB 
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c	character(7) version
c	parameter (version = '1.0.0')
*- 

	if(chatter.ge.wtchatter) then
		call wtout(level, string)
	endif

	return
	end
c -------------------------------------------------------------------
