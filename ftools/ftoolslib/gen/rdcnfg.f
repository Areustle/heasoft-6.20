*+RDCNFG
	subroutine rdcnfg(mission,instr,quiet,cif,instdir,status)

	implicit none
	character*(*) mission,instr,cif,instdir
	integer status
	logical quiet

C Description
c  Temporary botch routine, so as to maintain backwards compatibility, 
c
C Authors/Modification History:
C  Ron Zellar  Aug  8 1994  Original Version
C  Ian M George (2.0.0: 95 Dec 28) - ripped out out the heart & made that 
C			  that rcnfig.f, & put together this botch
C-----------------------------------------------------------------------
*-Version 2.0.0

	if(quiet) then
	   call rcnfig(mission,instr,0,cif,instdir,status)
	else
	   call rcnfig(mission,instr,20,cif,instdir,status)
	endif

	return
	end
