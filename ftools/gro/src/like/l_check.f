C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE L_CHECK(L)
C
C
C  $Id: l_check.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C       Effect: Check to see if L (longitude) is on the map. 
C	If not, try to adjust 
C       L by +360, or -360 degrees to obtain a longitude on the map. 
C	If this is not possible, declare an error condition.
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c 	real 	L    longitude
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C            Updated: by JRM
C=======================================================================
C
C   Changes:
c	1/31/94 JRM use GTorEQ for lower boundry, LT for upper.
c	6/23/94 JRM add final check.
c
c
C  $Log: l_check.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:35  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/22  22:33:51  jae
c repaired ctlorg and ctlend to reflect ctlscl/2
c >> offset of bin center
c
c Revision 5.1  1996/02/29  20:51:33  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:35  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE L_CHECK(L)

C	Common blocks used:
	INCLUDE  '../COMMON/ctlrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'

        save

	character(80) id
	common /id/id
        real L

	id = '$Id: l_check.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
        LOC='L_CHECK'

	orgL=L
	
	if(L.gt.CTLEND(1)+CTLSCL/2.) then
c       perhaps L - 360. will work
	   if(L - 360..gt.CTLEND(1)+CTLSCL/2.) then
	      goto 10
	   else
	      L=L - 360.
	      if(L.lt.CTLORG(1)-CTLSCL/2.) goto 10
	   endif
	elseif(L.lt.CTLORG(1)-CTLSCL/2.) then
c       perhaps L + 360. will work
	   if(L + 360..lt.CTLORG(1)-CTLSCL/2.) then
	      goto 10
	   else
	      L=L + 360.
	      if(L.gt.CTLEND(1)+CTLSCL/2.) goto 10
	   endif
 	endif
	
c	final check
	I = 1  + INT(0.2500+(L-CTLORG(1))/CTLSCL)
        IF (I.GT.CTLMSZ1) THEN
	   L=L-0.0001
	END IF
C       
	RETURN
	
 10	continue
c	error in longitude
	L=orgL
	write(SIGMSG,'("L_CHECK:L=",f7.1," is off the map.")') L
	SIGNAL='A'
	RETURN
	END
c
