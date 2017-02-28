C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	subroutine GOLDEN(AX,BX,CX,XMIN)
C
C
C  $Id: golden.f,v 1.2 2013/05/21 19:08:25 irby Exp $
c
C=======================================================================
C
C       NUMERICAL RECIPES code used by EGRET LIKE point source analysis
C=======================================================================
C++        EFFECT:
C++        Adapted from Numerical Recipes, Press etal., 1992,
C++        for the EGRET LIKE program. When modifyed, the original code
C++        is left as a comment. Press etal. have given the Compton SSC
C++	   a limited license to distribute these routines for use with
C++	   the EGRET LIKE program. NEITHER THIS FILE NOR ANY OF ITS
C++	   CONTENTS ARE TO PLACED IN AN ANONYMOUS FTP DIRECTORY, NOR
C++	   OTHERWISE BE MADE PUBLIC. To do so would violate the 
C++	   copyright of Press etal. 
c
c Description: Modified NUMERICAL RECIPES golden search.	
c
c
c
C  $Log: golden.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:03  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:19  jae
c Subroutine Module for like V5.00
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	subroutine GOLDEN(AX,BX,CX,XMIN)

        INCLUDE  '../COMMON/errrep.copy'

	character(80) id
	common /id/id
	PARAMETER (R=.61803399,C=.38196602)

C       Common blocks used:
        INCLUDE '../COMMON/cnfrep.copy'

        save

	id = '$Id: golden.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
	iter=0

	X0=AX
	X3=CX

	IF (ABS(CX-BX).GT.ABS(BX-AX)) THEN
	   X1=BX
	   X2=BX+C*(CX-BX)
	ELSE
	   X2=BX
	   X1=BX-C*(BX-AX)
	ENDIF
	F1=FUNC_limit(X1)
	F2=FUNC_limit(X2)
c1     IF (ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2))) THEN

 1	IF (ABS(X3-X0).GT.0.01) THEN
	   iter=iter+1
	   if (iter.gt.100) then
	      signal='L'
	      sigmsg='GOLDEN limit search exceeded maximum iterations.'
	      return
	   endif
	   IF (F2.LT.F1) THEN
	      X0=X1
	      X1=X2
	      X2=R*X1+C*X3
	      F0=F1
	      F1=F2
	      F2=FUNC_limit(X2)
	   ELSE
	      X3=X2
	      X2=X1
	      X1=R*X2+C*X0
	      F3=F2
	      F2=F1
	      F1=FUNC_limit(X1)
	   ENDIF
	   GOTO 1
	ENDIF

	IF (F1.LT.F2) THEN
c       GOLDEN=F1
	   XMIN=X1
	ELSE
c       GOLDEN=F2
	   XMIN=X2
	ENDIF

	RETURN
	END
c
