C	subroutine intent(determined)
C
C
C  $Id: intent.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: Query user about intention to proceed.
C=======================================================================
C  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: intent.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:33  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:17:02  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:48:12  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:35  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	subroutine intent(determined)

C       Common blocks used:
	include '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/errrep.copy'

	character(80) id
	common /id/id
	logical  determined
	character input*10


	id = '$Id: intent.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
	LOC='INTENT'

	if (jae_intent) write(*,'("In routine ",a)') LOC

	write(6,'(
     1    "          Type C to continue, cr to abort:",$)') 
	READ(LU(12),'(A)')input

	if (input.eq.'C'.or.input.eq.'c') then
	   determined=.true.
	else
	   determined=.false.
	endif

	return
        END

