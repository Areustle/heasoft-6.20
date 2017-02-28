C	SUBROUTINE basis_funcs(theta,afunc,n)
C
C
C  $Id: basis_fu.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C	effect: the basis functions for the elliptical fit are returned.
C=======================================================================
C  $Log: basis_fu.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:27  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/07/31  17:23:01  jae
c *** empty log message ***
c
c Revision 5.2  1996/07/31  17:19:07  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/29  20:46:44  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:01  jae
c Subroutine Module for like V5.00
c
C Installation of RCS lines: 23 AUG 1995 by JAE
C
	SUBROUTINE basis_funcs(theta,afunc,n)

        INCLUDE  '../COMMON/errrep.copy'
        INCLUDE  '../COMMON/cnfrep.copy'

        save

	character(80) id
	common /id/id
	REAL afunc(3)


	id = '$Id: basis_fu.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
	LOC='BASIS_FU'

	if (jae_basis_funcs)write(*,'("In routine ",a)') LOC
	cost=cos(theta)
	sint=sin(theta)
	afunc(1)=cost**2
	afunc(2)=sint**2
	afunc(3)=2.*cost*sint

	return
        END
