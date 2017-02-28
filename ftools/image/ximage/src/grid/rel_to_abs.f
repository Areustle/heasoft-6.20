* Last processed by NICE on 10-SEP-1991 17:26:39
* Customized for :  VAX         VMS          
      SUBROUTINE REL_TO_ABS(X,Y,A,D,N)
      implicit none
C----------------------------------------------------------------------
C MAP	Internal routine
C	Convert a set of plane position to spherical coordinates
C	according to the current projection formulae
C
C	This routine has been carefully programmed to allow
C	equivalencing (X,A) and (Y,D) arrays. This feature is
C	used in CONVERT.
C
C Arguments :
C	X	R*8 (*)	Array of offsets in X		Input
C	Y	R*8 (*)	Array of offsets in Y		Input
C	A	R*8 (*)	Array of R.A. (radians)		Output
C	D	R*8 (*)	Array of declinations		Output
C	N	I	Size of arrays			Input
C----------------------------------------------------------------------
      include 'projec.inc'
      include '../include/pi.inc'
      REAL*8 PRECISION
      PARAMETER (PRECISION=1.0D-10)
      REAL*8 X(*),Y(*),A(*),D(*)
      REAL*8 R,P,SINR,COSR,SINP,COSP,SIND,DA,CPANG,SPANG
      INTEGER I,N
      LOGICAL CHANGESIGN
      REAL*8 ARCSIN,ARCCOS
*
      GOTO (001,100,200,300,400,500,600,700) P_TYPE+1
      RETURN
*
* No projection
001   CONTINUE
      IF (ABS(PANGLE).LE.PRECISION) THEN
         DO I = 1,N
            A(I) = A0+X(I)
            D(I) = D0+Y(I)
         ENDDO
      ELSE
         CPANG = COS(PANGLE)
         SPANG = SIN(PANGLE)
         DO I = 1,N
            R = A0+X(I)*CPANG+Y(I)*SPANG
            P = D0-X(I)*SPANG+Y(I)*CPANG
            A(I) = R
            D(I) = P
         ENDDO
      ENDIF
      RETURN
*
* (A,D) = f(A0,D0,R,P)
*	D = ASIN (SIN(D0)*COS(R) + COS(D0)*SIN(R)*COS(P))
*	A = A0 + ATAN2( SIN(R)*SIN(P)*SIN(D0) ,
*	1	 (SIN(D)*COS(D0)-SIN(R)*COS(P)) )
*	ou A = A0 + ASIN ( SIN(R)*SIN(P)/COS(D) )
*
* Gnomonic projection
100   CONTINUE
      IF (SIND0.GT.PRECISION) THEN
         DO I = 1,N
            R = ATAN ( SQRT(X(I)**2+Y(I)**2) )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               IF (Y(I) .LT. NPOLE) THEN
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 + ARCSIN ( SINR*SINP/COS(D(I)) )
               ELSE
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 - ARCSIN ( SINR*SINP/COS(D(I)) ) + PI
               ENDIF
            ELSE
               A(I) = A0
               D(I) = D0
            ENDIF
         ENDDO
      ELSEIF (SIND0.LT.-PRECISION) THEN
         DO I = 1,N
            R = ATAN ( SQRT(X(I)**2+Y(I)**2) )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               IF (Y(I) .GT. SPOLE) THEN
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 + ARCSIN ( SINR*SINP/COS(D(I)) )
               ELSE
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 - ARCSIN ( SINR*SINP/COS(D(I)) ) + PI
               ENDIF
            ELSE
               A(I) = A0
               D(I) = D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            R = ATAN ( SQRT(X(I)**2+Y(I)**2) )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ARCSIN ( SINR*SINP/COS(D(I)) )
            ELSE
               A(I) = A0
               D(I) = D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Orthographic (Dixon)
200   CONTINUE
*
      IF (SIND0.GT.PRECISION) THEN
         DO I = 1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.1.D0) THEN
               SINR = SQRT(R)
               R = ARCSIN ( SINR )
               IF (SINR.GT.PRECISION) THEN
                  P = ATAN2 ( X(I),Y(I) ) - PANGLE
                  COSR = COS(R)
                  SINP = SIN(P)
                  COSP = COS(P)
                  SIND = (SIND0*COSR + COSD0*SINR*COSP)
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $            SIND*COSD0-SINR*COSP )
                  IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
               ELSE
                  A(I) = A0
                  D(I) = D0
               ENDIF
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ELSEIF (SIND0.LT.-PRECISION) THEN
         DO I = 1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.1.D0) THEN
               SINR = SQRT(R)
               R = ARCSIN ( SINR )
               IF (SINR.GT.PRECISION) THEN
                  P = ATAN2 ( X(I),Y(I) ) - PANGLE
                  COSR = COS(R)
                  SINP = SIN(P)
                  COSP = COS(P)
                  SIND = (SIND0*COSR + COSD0*SINR*COSP)
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $            SIND*COSD0-SINR*COSP ) + PI
                  IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
               ELSE
                  A(I) = A0
                  D(I) = D0
               ENDIF
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.1.D0) THEN
               SINR = SQRT(R)
               R = ARCSIN ( SINR )
               IF (SINR.GT.PRECISION) THEN
                  P = ATAN2 ( X(I),Y(I) ) - PANGLE
                  COSR = COS(R)
                  SINP = SIN(P)
                  COSP = COS(P)
                  SIND = (SIND0*COSR + COSD0*SINR*COSP)
                  D(I) = ARCSIN (SIND)
                  A(I) = A0 + ATAN2( SINR*SINP,
     $            COSR + SIND0*SINR*COSP)
               ELSE
                  A(I) = A0
                  D(I) = D0
               ENDIF
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Azimuthal  ou Schmidt
300   CONTINUE
      IF (SIND0.GT.PRECISION) THEN
         DO I = 1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.PI-PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP )
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSEIF (R.LT.PI+PRECISION) THEN
               A(I) = A0
               D(I) = -D0
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ELSEIF (SIND0.LT.-PRECISION) THEN
         DO I = 1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.PI-PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP ) + PI
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSEIF (R.LT.PI+PRECISION) THEN
               A(I) = A0
               D(I) = -D0
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I = 1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.PI-PRECISION) THEN
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP,
     $         COSR + SIND0*SINR*COSP)
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSEIF (R.LT.PI+PRECISION) THEN
               A(I) = A0
               D(I) = -D0
            ELSE
               A(I) = 0.D0
               D(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Stereographic
400   CONTINUE
      IF (SIND0.GT.PRECISION) THEN
         DO I = 1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSE
               R = 2.D0 * ATAN (R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP )
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ENDIF
         ENDDO
      ELSEIF (SIND0.LT.-PRECISION) THEN
         DO I = 1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSE
               R = 2.D0 * ATAN (R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP ) + PI
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            R = SQRT(X(I)**2+Y(I)**2)
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSE
               R = 2.D0 * ATAN (R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               IF (0.5D0*PI-ABS(D(I)).GT.PRECISION) THEN
                  A(I) = A0 + ATAN2( SINR*SINP,
     $            COSR + SIND0*SINR*COSP)
               ELSE
                  A(I) = A0
               ENDIF
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Lambert equal area
500   CONTINUE
*     	U = 2*sin(R)/SQRT(2*(1+cos(R))
*	R = Acos (1-0.5*U**2)
      IF (SIND0.GT.PRECISION) THEN
         DO I = 1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.4D0) THEN
               R = ACOS (1D0-0.5D0*R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP )
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSE
               A(I) = 0.0
               D(I) = 0.0
            ENDIF
         ENDDO
      ELSEIF (SIND0.LT.-PRECISION) THEN
         DO I = 1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.4D0) THEN
               R = ACOS (1D0-0.5D0*R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               A(I) = A0 + ATAN2( SINR*SINP*SIND0,
     $         SIND*COSD0-SINR*COSP ) + PI
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSE
               A(I) = 0.0
               D(I) = 0.0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            R = X(I)**2+Y(I)**2
            IF (R.LE.PRECISION) THEN
               A(I) = A0
               D(I) = D0
            ELSEIF (R.LE.4D0) THEN
               R = ACOS (1.D0-0.5D0*R)
               P = ATAN2 ( X(I),Y(I) ) - PANGLE
               SINR = SIN(R)
               COSR = COS(R)
               SINP = SIN(P)
               COSP = COS(P)
               SIND = (SIND0*COSR + COSD0*SINR*COSP)
               D(I) = ARCSIN (SIND)
               IF (0.5D0*PI-ABS(D(I)).GT.PRECISION) THEN
                  A(I) = A0 + ATAN2( SINR*SINP,
     $            COSR + SIND0*SINR*COSP)
               ELSE
                  A(I) = A0
               ENDIF
               IF (A(I).GT.2.D0*PI) A(I) = A(I)-2.D0*PI
            ELSE
               A(I) = 0.0
               D(I) = 0.0
            ENDIF
         ENDDO
      ENDIF
      RETURN
* Aitoff
600   DO I = 1,N
         CHANGESIGN = Y(I).LT.0.D0
         SINR = 0.5D0*SQRT(0.25D0*X(I)**2+Y(I)**2)
         IF (SINR.GT.1.D0) THEN
            A(I) = A0
            D(I) = 0.5D0*PI
         ELSEIF (SINR.GT.PRECISION) THEN
            R = 2.D0 * ARCSIN(SINR)
            P = ATAN2 ( X(I),2.D0*Y(I) )
            COSR = COS(R)
            DA = ATAN2 (SIN(P)*SIN(R),COSR)
            D(I) = ARCCOS ( COSR/COS(DA) )
            A(I) = A0 + 2.D0*DA
         ELSE
            A(I) = A0              !	+ 2.D0*ARCSIN(SINR)
            D(I) = 0.D0
         ENDIF
         IF (CHANGESIGN) D(I) = -D(I)
      ENDDO
      RETURN
*
* Classical Single-Dish mapping
700   CONTINUE
      DO I=1,N
         IF (Y(I).LE.SPOLE) THEN
            A(I) = A0
            D(I) = -PI*0.5D0
         ELSEIF (Y(I).GE.NPOLE) THEN
            A(I) = A0
            D(I) = PI*0.5D0
         ELSE
            P = D0 + Y(I)
            D(I) = P
            R = X(I)/COS(P)
            IF (R.LE.-PI) THEN
               A(I) = A0-PI
            ELSEIF (R.GE.PI) THEN
               A(I) = A0+PI
            ELSE
               A(I) = A0 + R
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
