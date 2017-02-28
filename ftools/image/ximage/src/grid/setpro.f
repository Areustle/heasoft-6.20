      SUBROUTINE SETPRO(RA,DEC,ANG,TYPE)
      implicit none
C----------------------------------------------------------------------
C MAP	Internal routine
C	Define a projection
C Arguments
C	RA	R*8	RA of projection center
C	DEC	R*8	Dec of projection center
C	ANG	R*8	Angle of projection line
C	TYPE	I	Projection type
C----------------------------------------------------------------------
      include '../include/pi.inc'
      include 'projec.inc'
      REAL*8 RA,DEC,ANG,X
      INTEGER TYPE
*
* Setup necessary constants
      P_TYPE = TYPE
*
      A0    = RA
      D0    = DEC
      PANGLE= ANG
      SIND0 = SIN(D0)
      COSD0 = COS(D0)
*
* Compute POLE position and values of remarkable points
*
      IF (P_TYPE.EQ.P_GNOMONIC) THEN
         IF (D0.LT.-1.D-30) THEN
            SPOLE = 1.D0/TAN(D0)
            NPOLE = 1.D38
         ELSEIF (D0.GT.1.D-30) THEN
            NPOLE = 1.D0/TAN(D0)
            SPOLE = 1.D38
         ELSE
            NPOLE = 1.D38
            SPOLE = 1.D38
         ENDIF
      ELSEIF (P_TYPE.EQ.P_ORTHO) THEN
         IF (D0.LT.0.D0) THEN
            SPOLE = -COSD0
            NPOLE = 1.D38
         ELSE
            NPOLE = COSD0
            SPOLE = 1.D38
         ENDIF
      ELSEIF (P_TYPE.EQ.P_AZIMUTHAL) THEN
         IF (D0.LT.0.D0) THEN
            SPOLE = -D0-PI*0.5D0
            NPOLE = PI*0.5+D0
         ELSE
            SPOLE = D0-PI*0.5D0
            NPOLE = PI*0.5D0-D0
         ENDIF
      ELSEIF (P_TYPE.EQ.P_STEREO) THEN
         IF (ABS(D0).GT.1.D-30) THEN
            CALL ABS_TO_REL(0.D0,PI*0.5D0,X,NPOLE,1)
            CALL ABS_TO_REL(0.D0,-PI*0.5D0,X,SPOLE,1)
         ELSE
            NPOLE = 1.D0
            SPOLE = -1.D0
         ENDIF
      ELSEIF (P_TYPE.EQ.P_LAMBERT) THEN
*	    IF (ABS(D0).GT.1.D-30) THEN
         CALL ABS_TO_REL(0.D0,PI*0.5D0,X,NPOLE,1)
         CALL ABS_TO_REL(0.D0,-PI*0.5D0,X,SPOLE,1)
*	    ELSE
*		NPOLE = 2.D0
*		SPOLE = -2.D0
*	    ENDIF
      ELSEIF (P_TYPE.EQ.P_AITOFF) THEN
         PANGLE = 0.D0
         NPOLE = SQRT(2.0D0)
         SPOLE = -NPOLE
         D0 = 0.D0
         COSD0 = 1.D0
         SIND0 = 0.D0
      ELSEIF (P_TYPE.EQ.P_RADIO) THEN
         PANGLE = 0.D0
         NPOLE = PI*0.5D0 - D0
         SPOLE = -D0 - PI*0.5D0
      ELSEIF (P_TYPE.EQ.P_NONE) THEN
         NPOLE = PI*0.5D0 - D0
         SPOLE = -D0 - PI*0.5D0
      ENDIF
      CALL SETREM
      END
