c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c
C	SUBROUTINE SWAPREAL(AX,BX)
C
C
C  $Id: swapreal.f,v 1.2 2013/05/21 19:08:27 irby Exp $
c
c	Purpose: 	swaps real variables ax and bx for sorting 
c
c	Programmer:	J.A.Esposito delivered 28 AUG 1995
C
C
C  $Log: swapreal.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:37:51  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:54:06  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:38  jae
c Subroutine Module for like V5.00
c
c
	SUBROUTINE SWAPREAL(AX,BX)

	
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

	character(80) id
	real AX,BX,CX

	common /id/id
c
	id = '$Id: swapreal.f,v 1.2 2013/05/21 19:08:27 irby Exp $'
	LOC='SWAPREAL'

	if (jae_swapreal) write(*,'("In routine ",a)') LOC

	CX=AX
	AX=BX
	BX=CX

	return
	end
c
