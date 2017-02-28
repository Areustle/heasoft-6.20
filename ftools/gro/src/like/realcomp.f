c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE REAL_COMP(ax,bx,iitst)
C
C
C  $Id: realcomp.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C
c	Purpose:	Compares two real numbers ax and bx.
c
c	Usage:		Returns value in iitst which indicates which
c			of ax or bx is larger, or if they are equal.
C
c	Return:		iitst = -1; ax < bx
c			iitst =  0; ax = bx
c			iitst =  1; ax > bx
c
c	Programmer:	J.A.Esposito delivered 28 AUG 1995
C
C  $Log: realcomp.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:32:44  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:53:09  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:59  jae
c Subroutine Module for like V5.00
c
C

	SUBROUTINE REAL_COMP(ax,bx,iitst)

        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

	real a        x,bx
	integer       iitst
	character(80)  id

	common /id/id

	id = '$Id: realcomp.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
	LOC='REAL_COMP'

	if (jae_realcomp) write(*,'("In routine ",a)') LOC
	iitst=0
	if (ax.lt.bx) then
	   iitst=-1
	   return
	elseif (ax.gt.bx) then
	   iitst=1
	   return
	else
	   iitst=0
	endif

	return
	end
