	subroutine xdebug
C---
C XPARSE subroutine to handle the #DEBUG condition.
C---
C May 14 1987 - rashafer
C---

	call debugh

C for VAX case runs code :
C	external ss$_debug
C	call lib$signal(ss$_debug)

	return
	end
