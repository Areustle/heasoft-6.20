C	subroutine iauname(theL,theB)
C
C
C  $Id: iauname.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++		effect: Derive GRO J Name based on the IAU convention
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C Installation of RCS lines: 23 AUG 1995 by JAE
C=======================================================================
C  $Log: iauname.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:34  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:12:42  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:48:07  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:28  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	subroutine iauname(theL,theB)

C       Common blocks used:
	INCLUDE  '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/likrep.copy'

        save

	character(80) id
	common /id/id
	character(1) lat_sign


	id = '$Id: iauname.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
	LOC='IAUNAME'

	if (jae_iauname)write(*,'("In routine ",a)') LOC
	tmpL = theL
	tmpB = theB
	if (tmpL.lt.0)tmpL = tmpL + 360.
	if (coord_sys.eq.'G') then
	   IRET = 0
	   templ=tmpL*PI180
	   tempb=tmpB*PI180
	   call CELGAL('GC',ra,dec,templ,tempb,IRET)
	   if (IRET.ne.0)stop 87
	   tmpL=ra/PI180
	   tmpB=dec/PI180
	endif

	tmpL = tmpL/15.
	itmphr = tmpL
	itmpmn = (tmpL - float(itmphr))*60
	itmpd = tmpB

	if (itmpd.lt.0) then
	   lat_sign ='-'
	   itmpd = -itmpd
	else
	   lat_sign = '+'
	endif

	write(srcN,'("GRO J",i2.2,i2.2,A1,i2.2)')
     1    itmphr,itmpmn,lat_sign,itmpd

	return
	end
