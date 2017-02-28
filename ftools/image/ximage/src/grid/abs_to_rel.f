      SUBROUTINE ABS_TO_REL(A,D,X,Y,N)
      IMPLICIT NONE
C----------------------------------------------------------------------
C MAP	Internal routine
C	Convert a set of plane position to spherical coordinates
C	according to the current projection formulae
C
C	This routine has been carefully programmed to allow
C	equivalencing (A,X) and (D,Y) arrays. This feature is
C	used in CONVERT.
C
C Arguments :
C	A	R*8 (*)	Array of R.A. (radians)		Input
C	D	R*8 (*)	Array of declinations		Input
C	X	R*8 (*)	Array of offsets in X		Output
C	Y	R*8 (*)	Array of offsets in Y		Output
C	N	I	Size of arrays			Input
C----------------------------------------------------------------------
      include 'projec.inc'
      include '../include/pi.inc'
      REAL*8 PRECISION
      PARAMETER (PRECISION=1.0D-10)
      REAL*8 X(*),Y(*),A(*),D(*)
      REAL*8 R,P,COSD,SIND,COSA,SINA,EXPR,DA
      INTEGER I,N
      LOGICAL CHANGESIGN
      REAL*8 ARCSIN,ARCCOS
*
      GOTO (001,100,200,300,400,500,600,700) P_TYPE+1
      RETURN
*
*
* (R,P) = f(A0,D0,A,D)
*	R = ACOS ( (SIN(D) - COS(D0) *
*	1 ( SIN(D)*COS(D0)-COS(D)*SIN(D0)*COS(A-A0) ) ) / SIN(D0) )
*	P = ATAN2 ( COS(D)*SIN(A-A0) ,
*	1 ( SIN(D)*COS(D0) - COS(D)*SIN(D0)*COS(A-A0) ) )
*
*
*
* Case D0 very small
*	R = ACOS (COS(D)*COS(A-A0))
*	P = A-A0
*
* No projection
001   CONTINUE
      DO I = 1, N
         X(I) = A(I)-A0
         Y(I) = D(I)-D0
      ENDDO
      RETURN
*
* Gnomonic
100   CONTINUE
c      TYPE *,'ATOR:',SNGL(A(1)),SNGL(D(1)),SNGL(X(1)),SNGL(Y(1)),N
c      TYPE *,A0,D0,SNGL(SIND0),SNGL(PRECISION)
       
      IF (ABS(SIND0).GE.PRECISION) THEN
         DO I = 1,N
            SIND = SIN(D(I))
            COSD = COS(D(I))
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            EXPR = SIND*COSD0-COSD*SIND0*COSA
            R = ARCCOS ( (SIND - COSD0*EXPR)/SIND0 )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( COSD*SINA , EXPR ) + PANGLE
               EXPR = TAN(R)
               X(I) =  EXPR*SIN(P)
               Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            COSD = COS(D(I))
            SIND = SIN(D(I))
            R = ARCCOS ( COSD*COSA )
            IF (R.GE.PRECISION) THEN
               IF(abs(sind).lt.precision)then
                 if(sind.lt.0)then 
                  sind=-precision
                 else
                  sind=precision
                 endif
               endif
                P = ATAN2 ( COSD*SINA,SIND ) + PANGLE
                EXPR = TAN(R)
                X(I) =  EXPR*SIN(P)
                Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.0
               Y(I) = 0.0
            ENDIF
         ENDDO
c
      ENDIF
      RETURN
*
* Dixon ou Orthographic
200   CONTINUE
      IF (ABS(SIND0).GE.PRECISION) THEN
         DO I = 1,N
            SIND = SIN(D(I))
            COSD = COS(D(I))
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            EXPR = SIND*COSD0-COSD*SIND0*COSA
            R = ARCCOS ( (SIND - COSD0*EXPR)/SIND0 )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( COSD*SINA , EXPR ) + PANGLE
               EXPR = SIN(R)
*!!		    if (r.ge.pi/2) r=pi/2
               X(I) = EXPR*SIN(P)
               Y(I) = EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            COSD = COS(D(I))
            SIND = SIN(D(I))
            R = ARCCOS ( COSD*COSA )
            IF (R.GE.PRECISION) THEN
               P = ATAN2 ( COSD*SINA,SIND ) + PANGLE
               EXPR = SIN(R)
*!!		    if (r.ge.pi/2) r=pi/2
               X(I) = EXPR*SIN(P)
               Y(I) = EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Azimuthal or Schmidt
300   CONTINUE
      IF (ABS(SIND0).GE.PRECISION) THEN
         DO I = 1,N
            SIND = SIN(D(I))
            COSD = COS(D(I))
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            EXPR = SIND*COSD0-COSD*SIND0*COSA
            R = ARCCOS ( (SIND - COSD0*EXPR)/SIND0 )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( COSD*SINA , EXPR ) + PANGLE
               X(I) = R*SIN(P)
               Y(I) = R*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            COSD = COS(D(I))
            SIND = SIN(D(I))
            R = ARCCOS ( COSD*COSA )
            IF (R.GE.PRECISION) THEN
               P = ATAN2 ( COSD*SINA,SIND ) + PANGLE
               X(I) = R*SIN(P)
               Y(I) = R*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Stereographic
400   CONTINUE
      IF (ABS(SIND0).GE.PRECISION) THEN
         DO I = 1,N
            SIND = SIN(D(I))
            COSD = COS(D(I))
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            EXPR = SIND*COSD0-COSD*SIND0*COSA
            R = ARCCOS ( (SIND - COSD0*EXPR)/SIND0 )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( COSD*SINA , EXPR ) + PANGLE
               EXPR = TAN(R*0.5D0)
               X(I) =  EXPR*SIN(P)
               Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            COSD = COS(D(I))
            SIND = SIN(D(I))
            R = ARCCOS ( COSD*COSA )
            IF (R.GE.PRECISION) THEN
               P = ATAN2 ( COSD*SINA,SIND ) + PANGLE
               EXPR = TAN(R*0.5D0)
               X(I) =  EXPR*SIN(P)
               Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Lambert equal area
500   CONTINUE
      IF (ABS(SIND0).GE.PRECISION) THEN
         DO I = 1,N
            SIND = SIN(D(I))
            COSD = COS(D(I))
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            EXPR = SIND*COSD0-COSD*SIND0*COSA
            R = ARCCOS ( (SIND - COSD0*EXPR)/SIND0 )
            IF (R.GT.PRECISION) THEN
               P = ATAN2 ( COSD*SINA , EXPR ) + PANGLE
               IF (R.GE.PI-PRECISION) THEN
                  EXPR = 2.0
               ELSE
                  EXPR = 2.D0*SIN(R)/SQRT(2.D0*(1.D0+COS(R)))
               ENDIF
               X(I) =  EXPR*SIN(P)
               Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ELSE
         DO I=1,N
            SINA = SIN(A(I)-A0)
            COSA = COS(A(I)-A0)
            COSD = COS(D(I))
            SIND = SIN(D(I))
            R = ARCCOS ( COSD*COSA )
            IF (R.GE.PRECISION) THEN
               P = ATAN2 ( COSD*SINA,SIND ) + PANGLE
               IF (R.GE.PI-PRECISION) THEN
                  EXPR = 2.0
               ELSE
                  EXPR = 2.D0*SIN(R)/SQRT(2.D0*(1.D0+COS(R)))
               ENDIF
               X(I) =  EXPR*SIN(P)
               Y(I) =  EXPR*COS(P)
            ELSE
               X(I) = 0.D0
               Y(I) = 0.D0
            ENDIF
         ENDDO
      ENDIF
      RETURN
*
* Aitoff
600   CONTINUE
      DO I=1,N
         CHANGESIGN = D(I).LT.0.D0
         DA = A(I) - A0
         DO WHILE (DA.LT.-PI)
            DA = DA + 2.D0*PI
         ENDDO
         DO WHILE (DA.GT.PI)
            DA = DA - 2.D0*PI
         ENDDO
         DA = DA*0.5D0
         IF (DA.EQ.0.D0 .AND. D(I).EQ.0.D0) THEN
            X(I)=0.D0
            Y(I)=0.D0
         ELSE
            COSD = COS(D(I))
            R = ARCCOS( COSD*COS(DA) )
            EXPR = MIN ( MAX(COSD*SIN(DA)/SIN(R),-1.D0), 1.D0)
            R = 2.D0*SIN(R*0.5D0)
            X(I) = 2.D0*R*EXPR
            Y(I) = R*COS(ARCSIN(EXPR))
            IF (CHANGESIGN) Y(I)=-Y(I)
         ENDIF
      ENDDO
      RETURN
*
* Classical Single-Dish mapping
700   DO I=1,N
         P = D(I)
         DA = A(I)-A0
         DO WHILE (DA.LT.-PI)
            DA = DA+2.D0*PI
         ENDDO
         DO WHILE (DA.GT.PI)
            DA = DA-2.D0*PI
         ENDDO
         X(I) = DA*COS(P)
         Y(I) = P-D0
      ENDDO
      RETURN
      END
