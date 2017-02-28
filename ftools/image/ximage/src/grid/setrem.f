      SUBROUTINE SETREM
      implicit none
C----------------------------------------------------------------------
C MAP	Computes the coordinates in the absolute system
C	of the remarkable points of the projection
C
C There are 6 privileged points
C
C	2------------4-------------6
C	|                          |
C	|                          |	RX,RY	Relative coordinates
C	|                          |
C	|                          |	AX,AY	Absolute coordinates
C	|                          |
C	1------------3-------------5
C
C
C En fait en cas d'angle non nul, la methode ci-dessus ne marche plus.
C Une astuce COLOSSALE consiste a considerer le plus petit cercle contenant
C le rectangle et centre sur le centre de projection. Ce cercle est la
C projection de points angulairement equidistants du centre de projection.
C Or, en projection GNOMONIC, les points d'elongations extremes
C correspondent aux tangentes au cercle passant par le pole.
C i.e.	Xgnom	= 	SQRT(Yg(POLE)**2 - D**2) * D / Yg(POLE)
C	Ygnom	=	D**2 / Yg(POLE)
C Les coordonnees absolues s'en deduise trivialement
C
C Pour la declinaison, le probleme est evident.
C
C Bien sur tout ca ne marche pas avec les projections AITOFF et RADIO .
C----------------------------------------------------------------------
      include '../include/pi.inc'
      include 'greg.inc'
      include 'projec.inc'
      REAL*8 PRECISION
      PARAMETER (PRECISION=1.0D-10)
      REAL*8 C,W,AX1,AX2,AY1,AY2
      REAL*8 PRADIUS,GRADIUS,RADIUS,GPOLE,SIND,SINP,SINR,COSR,COSP,
     $EXTRA

c  Initialize to avoid warning
      GRADIUS = 0.d0
      RADIUS = 0.d0
c  --
*
* NULL ANGLE CASE
*----------------------------------------------------------------------
      IF (ABS(PANGLE).LE.PRECISION) THEN
         AX1 = MAX(GUX1,GUX2)
         AX2 = MIN(GUX1,GUX2)
         AY1 = MIN(GUY1,GUY2)
         AY2 = MAX(GUY1,GUY2)
*
* Gnomonic
         CALL REL_TO_ABS (AX1,AY1,AXREM1,AYREM1,1)
         CALL REL_TO_ABS (AX1,AY2,AXREM2,AYREM2,1)
         CALL REL_TO_ABS (0.D0,AY1,AXREM3,AYREM3,1)
         CALL REL_TO_ABS (0.D0,AY2,AXREM4,AYREM4,1)
         CALL REL_TO_ABS (AX2,AY1,AXREM5,AYREM5,1)
         CALL REL_TO_ABS (AX2,AY2,AXREM6,AYREM6,1)
*
* Orthographic, Lambert and Azimuthal have a finite area
         IF (P_TYPE.EQ.P_ORTHO) THEN
            W = AY2**2
            IF (W.LE.1.D0) THEN
               C = SQRT(1.D0-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY2,AXREM6,AYREM6,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY2,AXREM2,AYREM2,1)
            ENDIF
            W = AY1**2
            IF (W.LE.1.D0) THEN
               C = SQRT(1.D0-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY1,AXREM5,AYREM5,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY1,AXREM1,AYREM1,1)
            ENDIF
            IF (AY1+1.D0 .LE. 0.D0) AYREM3 = D0-PI*0.5D0
            IF (AY2-1.D0 .GE. 0.D0) AYREM4 = D0+PI*0.5D0
*
         ELSEIF (P_TYPE.EQ.P_AZIMUTHAL) THEN
            W = AY2**2
            IF (W.LE.PI**2) THEN
               C = SQRT(PI**2-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY2,AXREM6,AYREM6,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY2,AXREM2,AYREM2,1)
            ENDIF
            W = AY1**2
            IF (W.LE.PI**2) THEN
               C = SQRT(PI**2-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY1,AXREM5,AYREM5,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY1,AXREM1,AYREM1,1)
            ENDIF
            IF (AY1+0.5D0*PI .LE. 0.D0) AYREM3 = D0-PI*0.5D0
            IF (AY2-0.5D0*PI .GE. 0.D0) AYREM4 = D0+PI*0.5D0
*
         ELSEIF (P_TYPE.EQ.P_LAMBERT) THEN
            W = AY2**2
            IF (W.LE.4) THEN
               C = SQRT(4-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY2,AXREM6,AYREM6,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY2,AXREM2,AYREM2,1)
            ENDIF
            W = AY1**2
            IF (W.LE.4) THEN
               C = SQRT(4-W)
               IF (C .LT. AX2) CALL REL_TO_ABS(C,AY1,AXREM5,AYREM5,1)
               IF (-C .GT. AX1) CALL REL_TO_ABS(-C,AY1,AXREM1,AYREM1,1)
            ENDIF
            IF (AY1+1.0 .LE. 0.D0) AYREM3 = D0-PI*0.5D0
            IF (AY2-1.0 .GE. 0.D0) AYREM4 = D0+PI*0.5D0
         ENDIF
*
* Find RA min and RA max
         IF (NPOLE.GE.AY1 .AND. NPOLE.LE.AY2) THEN
            IF (SPOLE.GE.AY1 .AND. SPOLE.LE.AY2) THEN
               RAMIN = A0-PI
               RAMAX = A0+PI
               DECMIN = -0.5D0*PI
               DECMAX = 0.5D0*PI
               IPOLE = 3
            ELSE
               RAMIN = A0-PI
               RAMAX = A0+PI
               IPOLE = 1
            ENDIF
         ELSEIF (SPOLE.GE.AY1 .AND. SPOLE.LE.AY2) THEN
            RAMIN = A0-PI
            RAMAX = A0+PI
            IPOLE = 2
         ELSE
            IPOLE = 0
            RAMIN = MIN(AXREM5,AXREM6)
            RAMAX = MAX(AXREM1,AXREM2)
            IF (RAMIN.GT.RAMAX) RAMIN = RAMIN-2.D0*PI
         ENDIF
*
* Find DEC min and DEC max
         IF (IPOLE.EQ.1) THEN
            DECMAX = PI*0.5D0
            DECMIN = MIN(AYREM1,AYREM2,AYREM3,AYREM4,AYREM5,AYREM6)
         ELSEIF (IPOLE.EQ.2) THEN
            DECMIN = -PI*0.5D0
            DECMAX = MAX(AYREM1,AYREM2,AYREM3,AYREM4,AYREM5,AYREM6)
         ELSEIF (IPOLE.NE.3) THEN
            DECMIN = MIN(AYREM1,AYREM3,AYREM5)
            DECMAX = MAX(AYREM2,AYREM4,AYREM6)
         ENDIF
         RETURN
      ENDIF
*----------------------------------------------------------------------
*
*
* Case ANGLE not zero
*
* Compute radius of projected circle
      PRADIUS = SQRT(MAX(GUX1**2,GUX2**2)+MAX(GUY1**2,GUY2**2))
      IF (P_TYPE.EQ.P_ORTHO) THEN
         IF (PRADIUS.LT.1.D0-PRECISION) THEN
            RADIUS = ASIN(PRADIUS)
            GRADIUS = TAN(RADIUS)
         ELSE
            RADIUS = 0.5D0*PI
            GRADIUS = 1.D38
         ENDIF
      ELSEIF (P_TYPE.EQ.P_STEREO) THEN
         RADIUS = 2.D0*ATAN(PRADIUS)
         GRADIUS = TAN(RADIUS)
         IF (GRADIUS.LT.0.D0) GRADIUS = 1.D38
      ELSEIF (P_TYPE.EQ.P_AZIMUTHAL) THEN
         IF (PRADIUS.LT.PI-PRECISION) THEN
            RADIUS = PRADIUS
            GRADIUS = TAN(RADIUS)
         ELSE
            RADIUS = PI
            GRADIUS = 1.D38
         ENDIF
      ELSEIF (P_TYPE.EQ.P_GNOMONIC) THEN
         RADIUS = ATAN(PRADIUS)
         GRADIUS = PRADIUS
      ELSEIF (P_TYPE.EQ.P_NONE) THEN
         RADIUS  = PRADIUS
         GRADIUS = PRADIUS
      ENDIF
*
* Get right ascension extrema
* Get Gnomonic pole distance (1.D0/TAN(D0))
      IF (ABS(D0).GT.PRECISION) THEN
         GPOLE = ABS(1.D0/TAN(D0))
      ELSE
         GPOLE = 1.D38
      ENDIF
*
      IF (GRADIUS.GT.GPOLE-PRECISION) THEN
         RAMIN = A0 - PI
         RAMAX = A0 + PI
      ELSEIF (GPOLE.NE.1.D38) THEN
         IF (RADIUS.GT.PRECISION) THEN
            SINR = SIN(RADIUS)
            COSR = COS(RADIUS)
*		p = atan2 (sqrt(1.d0-(gradius/gpole)**2)),gradius)
*		cosp = cos(p)
*		sinp = sin(p)
            COSP = ABS(GRADIUS/GPOLE)
            SINP = SQRT(1.D0-COSP**2)
            SIND = (ABS(SIND0)*COSR + COSD0*SINR*COSP)
*		delta = asin (sind)
*	    	extra = asin ( sinr*sinp/cos(delta) )
            EXTRA = ASIN ( SINR*SINP/SQRT(1.D0-SIND**2) ) + PRECISION
         ELSE
            EXTRA = 2.D0*PRECISION
         ENDIF
         RAMIN = A0 - EXTRA
         RAMAX = A0 + EXTRA
      ELSE
         EXTRA = MIN(RADIUS+PRECISION,PI)! RADIUS pas PRADIUS....
         RAMIN = A0 - EXTRA
         RAMAX = A0 + EXTRA
      ENDIF
*
* Get declination extreme
      IF (PRADIUS.GT.ABS(NPOLE)-PRECISION) THEN
         DECMAX = 0.5*PI
      ELSE
         DECMAX = D0 + RADIUS
      ENDIF
      IF (PRADIUS.GT.ABS(SPOLE)-PRECISION) THEN
         DECMIN = -0.5*PI
      ELSE
         DECMIN = D0 - RADIUS
      ENDIF
      END
