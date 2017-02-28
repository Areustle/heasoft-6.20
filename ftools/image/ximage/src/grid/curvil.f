      SUBROUTINE CURVIL (S,*)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C    	This routine is called by CURV to compute the curvilinear length
C     	S of the spline arc [PSTO(K-1),PSTO(K)].
C	RETURN 1 occurs if DIFSYS4 fails to integrate
C Arguments :
C	S	R*4	Curvilinear length		Output
C	*	*	Alternate error return		Input
C Subroutines :
C	CURFUN, DIFSYS4
C----------------------------------------------------------------------
      REAL D,H,P,P1,P2,Q,S
      include 'curcom.inc'
      EXTERNAL CURFUN
*
      P1 = PSTO(K-1)
      P2 = PSTO(K)
      P  = P1
      D  = P2-P1
      S  = 0.
      H  = D/10.
*
2     CONTINUE
      Q  = (P2-P)/H
      IF (Q.LT.1.E-6) THEN
         RETURN
      ELSEIF (Q.LT.1.1) THEN
         H  = P2-P
      ENDIF
      CALL DIFSYS4 (CURFUN,ACCURD,H,P,S)
      IF (H.EQ.0.) THEN
         CALL XWRITE('E-CURVE,  Integration failure in DIFSYS4', 10)
         RETURN 1
      ENDIF
      GOTO 2
*
      END
