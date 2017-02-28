c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE TO_UPPER(AX)
C
C
C  $Id: to_upper.f,v 1.3 2013/05/21 19:08:27 irby Exp $
c
c	PURPOSE: takes lower case letters and returns them as upper
c		 case.  If the input character is not a letter (a-z,A-Z)
c		 there is no effect.  This is a fortran version of the
c		 C finction toupper(char*)
c
c	Programmer: J.A.Esposito delivered 28 AUG 1995
c
c	
C
C
C  $Log: to_upper.f,v $
C  Revision 1.3  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:39:35  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:54:10  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:56  jae
c Subroutine Module for like V5.00
c
C
	SUBROUTINE TO_UPPER(AX)
	
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

	character(80) id
	character AX

	common /id/id


	id = '$Id: to_upper.f,v 1.3 2013/05/21 19:08:27 irby Exp $'
	LOC='TO_UPPER'


	if (jae_to_upper) write(*,'("In routine ",a)') LOC

	if (ICHAR(AX).ge.97.and.ICHAR(AX).le.122)
     1      AX = char(ICHAR(AX)-32)

	return
	end
