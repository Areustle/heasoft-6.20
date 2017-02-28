      SUBROUTINE CURFUN (P,S,DSDP)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C	This routine states the differential equation to be solved by
C	DIFSYS4 in order to obtain the curvilinear length of an arc of
C	spline.
C Arguments :
C	P	R*4	Value of parameter			Input
C	S	R*4	Curvilinear length			Input
C	DSDP	R*4	Derivative of S relative to P		Output
C----------------------------------------------------------------------
      REAL P,S,DSDP,H
      include 'curcom.inc'
*
      H=P-PSTO(K-1)
      DSDP=SQRT((X1(K-1)+H*(X2(K-1)+X3(K-1)*H/2.))**2
     $+(Y1(K-1)+H*(Y2(K-1)+Y3(K-1)*H/2.))**2)
      END
