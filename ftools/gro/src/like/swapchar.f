c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE swapchar(cax,cbx,M)
C
C
C  $Id: swapchar.f,v 1.2 2013/05/21 19:08:27 irby Exp $
C
c	Purpose: Swaps char* cax with char* cbx.
c
c	Programmer: J.A.Esposito delivered 28 AUG 1995
C
C
C  $Log: swapchar.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:36:18  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:54:03  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:36  jae
c Subroutine Module for like V5.00
c
C
	SUBROUTINE swapchar(cax,cbx,M)

        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

	character(80) id
	character cax(M),cbx(M),ccx

	common /id/id
c
	id = '$Id: swapchar.f,v 1.2 2013/05/21 19:08:27 irby Exp $'
	LOC='SWAPCHAR'

	if (jae_swapchar) write(*,'("In routine ",a)') LOC
c       
	do jk=1,M
	   ccx=cax(jk)
	   cax(jk)=cbx(jk)
	   cbx(jk)=ccx
	enddo

	return
	end
