      SUBROUTINE PLCURV (NXY,X,Y,Z,ACCUR
     $,ALGORITHM,VARIABLE,PERIODIC,status)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Support routine
C       This routine connects a series of points in the subject space
C	with a solid line or a dashed line. Cubic spline interpolation
C	is available either for functions of X or functions of Y or for
C	curves (X and Y both interpolated, see below). If the number of
C	points exceeds spline-buffer capacity, the total curve is cut
C	into several arcs and care is taken to ensure a good fitting
C	between two adjacent splines.
C
C	Adapted from PIGRA.  P. Valiron  16-OCT-84
C
C     /VARIABLE X : the variable is X (abscissa)
C                  	*** Y = F(X)
C     /VARIABLE Y : the variable is Y (ordinate)
C                  	*** X = F(Y)
C     /VARIABLE Z : the variable is the Z array
C                  	*** explicit parametric form.
C     /VARIABLE NUMBERING : the variable is the numbering of the points
C                  *** The sequence of the points is related to a fixed
C                      increment in an explicit or implicit parametrisa-
C                      tion of the curve (plotting the solution of a dif
C                      -ferential equation for equally spaced steps...)
C     /VARIABLE POLYGONAL_LENGTH OR CURVILINEAR_LENGTH :
C		   *** the variable is an approximation of the
C			curvilinear abscissa
C                  *** The curvilinear abscissa is estimated iteratively.
C                  The starting approximation is the sum of the distan-
C                  ces of the points. For each iteration, the spline
C                  coefficients are computed and the curvilinear abscis-
C                  sa is integrated along the spline curve.
C		   The number of iterations is ITERMX.
C			POLYGONAL_LENGTH	ITERMX=0
C			CURVILINEAR_LENGTH	ITERMX=2
C	SUBROUTINE PLCURV (NXY,X,Y,Z,ACCUR
C     #		,ALGORITHM,VARIABLE,PERIODIC,status)
C Arguments :
C	NXY	I	Number of data points		Input
C	X	R*8 (*)	Array of X coordinates		Input
C	Y	R*8 (*)	Array of Y coordinates		Input
C	Z	R*8 (*)	Array of parameter values	Input
C	ACCUR	R*4	Accuracy of interpolation	Input
C	ALGORITHM C*(*)	Algorithm used by interpolation	Input
C	VARIABLE  C*(*)	Variable used for interpolation	Input
C	PERIODIC  L	Is it a periodic function	Input
C	status	I	Error flag (0=OK)		Output
C Subroutines :
C	RAD2DET, 	! Formerly US8_TO_INT
C	CUBSPL4, CURVIL
C----------------------------------------------------------------------
      INTEGER NXY, status
      REAL*8 X(*),Y(*),Z(*)
      REAL ACCUR
      CHARACTER*(*) ALGORITHM,VARIABLE
      LOGICAL PERIODIC
*----------------------------------------------------------------------
*   VARX / logical; set if Y=F(X)
*   VARY / logical; set if X=F(Y)
*  NLINK / dimension of spline buffers
*  KLINK / *** when number of points exceeds spline-buffer capacity,
*          *** the curve is cut into several arcs.
*          To ensure a good matching of consecutive splines, the KLINK
*          end points of the current spline are not plotted; in the next
*          spline, the first 2*KLINK points are copied from the previous
*          one and the KLINK beginning points are not plotted.
*          (see below, points not plotted are listed as stars)
*
*       I-th spline array IIIIIIIIIIIII*****
*                                 *****JJJJJJJJJJJJJJ next spline array
*              matching points....[2*klink ]
*
* NINIMX / Maximum number of interpolated points between two adjacent
*          data points.
* ITERMX / Maximum number of iterations allowed to obtain the approxi-
*          mation of the curvilinear abscissa on the curve
*
*      N / Number of points stored in the buffer
*      K / Index of the following point
*  NSTOP / Index of last point to process in the buffer.
*          If LINK is set, NSTOP is less than N to allow to the follo-
*          wing spline to have some points in common with the current
*          one.
*----------------------------------------------------------------------
      LOGICAL VARX,VARY,VARNUM,VARPOL,VARCUR,VARZ,LLCUB,LLSMO
     $,LINK,INIT
      INTEGER ITER,ITERMX,KM1,NINI,NADD,I,N1,N2,NPERIOD
     $,IU,IU1,IU2,IS1,IS2,KMAX,JS1,JS2,JU1,JU2,KU
      REAL PKM1,PK,XKM1,XK,YKM1,YK,FIFKM1,FIFK,PADD,DADD,H,ARC,PARA
     $,XP,YP,PERIOD,DENOM
      character(80) CHAIN
      include 'curcom.inc'
      DIMENSION PARA(NLINK)

c  Initialize to avoid warning
      ITERMX = 0
      IU2 = 0
      KMAX = 0
      PKM1 = 0.
      FIFKM1 = 0.
      PERIOD = 0.
c  --
*
* Selection of Options And Initialisation
      INIT=.TRUE.
      ACCURD=ACCUR/10.
      PSTO(1)=0
      LLCUB=.FALSE.
      LLSMO=.FALSE.
      IF (ALGORITHM.EQ.'CUBIC_SPLINE') THEN
         LLCUB=.TRUE.
*/	ELSEIF (ALGORITHM.EQ.'SMOOTHING_SPLINE') THEN
*/	    LLSMO=.TRUE.
      ELSE
         CHAIN = 'E-CURVE,  Algorithm '//ALGORITHM//' not implemented'
         CALL xwrite (CHAIN, 10)
         status = -1
         RETURN
      ENDIF
      VARX=.FALSE.
      VARY=.FALSE.
      VARZ=.FALSE.
      VARNUM=.FALSE.
      VARPOL=.FALSE.
      VARCUR=.FALSE.
      IF (VARIABLE.EQ.'X') THEN
         VARX=.TRUE.
      ELSEIF (VARIABLE.EQ.'Y') THEN
         VARY=.TRUE.
      ELSEIF (VARIABLE.EQ.'NUMBERING') THEN
         VARNUM=.TRUE.
      ELSEIF (VARIABLE.EQ.'POLYGONAL_LENGTH') THEN
         VARPOL=.TRUE.
      ELSEIF (VARIABLE.EQ.'CURVILINEAR_LENGTH') THEN
         VARCUR=.TRUE.
         ITERMX=2
      ELSEIF (VARIABLE.EQ.'Z') THEN
         VARZ=.TRUE.
      ENDIF
*
      IF (PERIODIC) THEN
         N1=2-KLINK
         N2=NXY+KLINK
         IF (VARX) THEN
            IF (Y(1).NE.Y(NXY)) GOTO 600
         ELSEIF (VARY) THEN
            IF (X(1).NE.X(NXY)) GOTO 600
         ELSE
            IF (X(1).NE.X(NXY) .OR. Y(1).NE.Y(NXY)) GOTO 600
         ENDIF
         IF (VARX) THEN
            CALL RAD2DET (X,Y,XSTO,YSTO,1)
            CALL RAD2DET (X(NXY),Y(NXY),XSTO(2),YSTO,1)
            PERIOD=XSTO(2)-XSTO(1)
         ELSEIF (VARY) THEN
            CALL RAD2DET (X,Y,XSTO,YSTO,1)
            CALL RAD2DET (X(NXY),Y(NXY),XSTO,YSTO(2),1)
            PERIOD=YSTO(2)-YSTO(1)
         ELSEIF (VARZ) THEN
            PERIOD=Z(NXY)-Z(1)
         ENDIF
      ELSE
         N1=1
         N2=NXY
      ENDIF
*
* Initialise for first point
      LINK=.FALSE.
      IF (NXY.LE.4 .AND. .NOT.PERIODIC) THEN
         CALL XWRITE ('E-CURVE,  2, 3 or 4 Points available only', 10)
      ENDIF
*
* Multi-buffer Loop
*	N1,N2	Full range addresses of user data to be copied
*	IU1,IU2	Current addresses of user coordinates to be copied
*	IS1,IS2	Corresponding addresses in buffer
*	KM1	Previous plotted address in buffer
*	KMAX	Last address in buffer to be plotted
1000  CONTINUE
      IF (LINK) THEN
*
* LINK .TRUE.
         K=0
         DO I=NLINK-2*KLINK+1,NLINK
            K=K+1
            PSTO(K)=PSTO(I)
            XSTO(K)=XSTO(I)
            YSTO(K)=YSTO(I)
         ENDDO
         IF (N2-IU2.GT.NLINK-2*KLINK) THEN
* LINK remains .TRUE.
            IS1=2*KLINK+1
            IS2=NLINK
            IU1=IU2+1
            IU2=IU2+NLINK-2*KLINK
            KM1=KLINK
*		N,KMAX unchanged. LINK remains .TRUE.
         ELSE
* The end! LINK is reset to .FALSE.
            IS1=2*KLINK+1
            IS2=2*KLINK+N2-IU2
            IU1=IU2+1
            IU2=N2
            KM1=KLINK
            IF (PERIODIC) THEN
               KMAX=IS2-KLINK
            ELSE
               KMAX=IS2
            ENDIF
            N=IS2
            LINK=.FALSE.
         ENDIF
*
      ELSE
*
* LINK is .FALSE.
*
         IF (N2-N1+1.GT.NLINK) THEN
* Buffer overflow. Set LINK to .TRUE.
            IS1=1
            IS2=NLINK
            IU1=N1
            IU2=N1+NLINK-1
            IF (PERIODIC) THEN
               KM1=KLINK
            ELSE
               KM1=1
            ENDIF
            KMAX=NLINK-KLINK
            N=NLINK
            LINK=.TRUE.
         ELSE
* Buffer is large enough. All points will be plotted within one pass
            IS1=1
            IS2=N2-N1+1
            IU1=N1
            IU2=N2
            IF (PERIODIC) THEN
               KM1=KLINK
               KMAX=IS2-KLINK
            ELSE
               KM1=1
               KMAX=IS2
            ENDIF
            N=IS2
            LINK=.FALSE.
         ENDIF
      ENDIF
*
* For periodic interpolation, IU1 and/or IU2 may lie outside the range
* [1...NXY].
* J-indices JS1, JS2, JU1 and JU2 are the restriction of associated
* I-indices to the range [1...NXY] in user addresses
*
      IF (PERIODIC) THEN
         JU1=MAX(IU1,1)
         JU2=MIN(IU2,NXY)
         JS1=IS1+(JU1-IU1)
         JS2=IS2+(JU2-IU2)
      ELSE
         JU1=IU1
         JU2=IU2
         JS1=IS1
         JS2=IS2
      ENDIF
*
* Convert X and Y user coordinates to paper coordinates
      CALL RAD2DET (X(JU1),Y(JU1),XSTO(JS1),YSTO(JS1),JU2-JU1+1)
      IF (VARZ) THEN
         IU=JU1
         DO K=JS1,JS2
            PSTO(K)=Z(IU)
            IU=IU+1
         ENDDO
      ENDIF
*
      IF (PERIODIC) THEN
* Before...
         IU=IU1
         DO K=IS1,JS1-1
            NPERIOD=-(IU-1)/(NXY-1)
            KU=IU+NPERIOD*(NXY-1)
            IF (KU.LE.0) THEN
               NPERIOD=NPERIOD+1
               KU=KU+(NXY-1)
            ENDIF
            CALL RAD2DET (X(KU),Y(KU),XSTO(K),YSTO(K),1)
            IF (VARX) THEN
               XSTO(K)=XSTO(K)-NPERIOD*PERIOD
            ELSEIF (VARY) THEN
               YSTO(K)=YSTO(K)-NPERIOD*PERIOD
            ELSEIF (VARZ) THEN
               PSTO(K)=Z(KU)-NPERIOD*PERIOD
            ENDIF
            IU=IU+1
         ENDDO
* After...
         IU=JU2+1
         DO K=JS2+1,IS2
            NPERIOD=-(IU-1)/(NXY-1)
            KU=IU+NPERIOD*(NXY-1)
            IF (KU.LE.0) THEN
               NPERIOD=NPERIOD+1
               KU=KU+(NXY-1)
            ENDIF
            CALL RAD2DET (X(KU),Y(KU),XSTO(K),YSTO(K),1)
            IF (VARX) THEN
               XSTO(K)=XSTO(K)-NPERIOD*PERIOD
            ELSEIF (VARY) THEN
               YSTO(K)=YSTO(K)-NPERIOD*PERIOD
            ELSEIF (VARZ) THEN
               PSTO(K)=Z(KU)-NPERIOD*PERIOD
            ENDIF
            IU=IU+1
         ENDDO
      ENDIF
*
* Compute the parameter array according to /VARIABLE
      IF (VARX) THEN
         DO K=IS1,IS2
            PSTO(K)=XSTO(K)
         ENDDO
      ELSEIF (VARY) THEN
         DO K=IS1,IS2
            PSTO(K)=YSTO(K)
         ENDDO
      ELSEIF (VARNUM) THEN
         DO K=MAX(IS1,2),IS2
            PSTO(K)=PSTO(K-1)+1
         ENDDO
      ELSEIF (VARPOL.OR.VARCUR) THEN
         DO K=MAX(IS1,2),IS2
            PSTO(K)=PSTO(K-1)
     $      +SQRT((XSTO(K)-XSTO(K-1))**2
     $      +(YSTO(K)-YSTO(K-1))**2)
         ENDDO
      ENDIF
*
* Compute spline coefficients
      IF (VARX) THEN
         CALL CUBSPL4 (N,PSTO,YSTO,Y1,Y2,Y3,0,0,*500)
      ELSEIF (VARY) THEN
         CALL CUBSPL4 (N,PSTO,XSTO,X1,X2,X3,0,0,*500)
      ELSE
         CALL CUBSPL4 (N,PSTO,XSTO,X1,X2,X3,0,0,*500)
         CALL CUBSPL4 (N,PSTO,YSTO,Y1,Y2,Y3,0,0,*500)
      ENDIF
*
* Iterate for Curvilinear Abscissa
      IF (VARCUR) THEN
         DO ITER=1,ITERMX
            PARA(1)=0.
            DO K=2,N
               CALL CURVIL (ARC,*400)
               PARA(K)=PARA(K-1)+ARC
            ENDDO
            PSTO(1) = 0.
            DO K=2,N
               PSTO(K)=PARA(K)
            ENDDO
            PKM1=PSTO(KM1)
            CALL CUBSPL4 (N,PSTO,XSTO,X1,X2,X3,0,0,*500)
            CALL CUBSPL4 (N,PSTO,YSTO,Y1,Y2,Y3,0,0,*500)
         ENDDO
      ENDIF
300   CONTINUE
*
* Loop on K to interpolate the intervals [PKM1...PK]
      IF (INIT) THEN
* Initialisation for point KM1
         INIT=.FALSE.
         PKM1=PSTO(KM1)
         XKM1=XSTO(KM1)
         YKM1=YSTO(KM1)
         IF (VARX) THEN
            FIFKM1=SQRT(Y2(KM1)**2/(1.+Y1(KM1)**2))
         ELSEIF (VARY) THEN
            FIFKM1=SQRT(X2(KM1)**2/(1.+X1(KM1)**2))
         ELSE
            DENOM = (X1(KM1)**2+Y1(KM1)**2)
            IF (DENOM.GT.1.E-10) THEN
               FIFKM1=SQRT((X1(KM1)*Y2(KM1)-X2(KM1)*Y1(KM1))**2
     $         /DENOM)
            ELSE
               FIFKM1 = 0
            ENDIF
         ENDIF
         CALL pgmove (XKM1,YKM1)
      ENDIF
*
* Point KM1 has been already plotted
      DO K=KM1+1,KMAX
         PK=PSTO(K)
         XK=XSTO(K)
         YK=YSTO(K)
*
* Compute the number NINI of points to interpolate.
         IF (VARX) THEN
            FIFK=SQRT(Y2(K)**2/(1.+Y1(K)**2))
         ELSEIF (VARY) THEN
            FIFK=SQRT(X2(K)**2/(1.+X1(K)**2))
         ELSE
            DENOM = (X1(K)**2+Y1(K)**2)
            IF (DENOM.GT.1.E-10) THEN
               FIFK=SQRT((X1(K)*Y2(K)-X2(K)*Y1(K))**2
     $         /DENOM)
            ELSE
               FIFK = 0
            ENDIF
         ENDIF
         NINI=SQRT((PK-PKM1)**2*MAX(FIFKM1,FIFK)/ACCUR)/2.
*
* Interpolation Fork
         IF (NINI.LT.1) THEN
*
* Plot point (XK,YK) with no interpolation
            CALL gdraw (XK,YK)
*
         ELSE
*
* Initialise for interpolation process
            IF (NINI.GT.NINIMX) NINI=NINIMX
            PADD   = PKM1
            DADD   = (PK-PKM1) / FLOAT (NINI+1)
            NADD   = 0
*
* Add NINI Interpolated Points Between (XKM1,YKM1) and (XK,YK)
            DO I=1,NINI
               PADD=PADD+DADD
               H=PADD-PSTO(KM1)
               IF (VARX) THEN
                  XP=PADD
                  YP=YSTO(KM1)+H*(Y1(KM1)+H*(Y2(KM1)
     $            +H*Y3(KM1)/3.)/2.)
               ELSEIF (VARY) THEN
                  XP=XSTO(KM1)+H*(X1(KM1)+H*(X2(KM1)
     $            +H*X3(KM1)/3.)/2.)
                  YP=PADD
               ELSE
                  XP=XSTO(KM1)+H*(X1(KM1)+H*(X2(KM1)
     $            +H*X3(KM1)/3.)/2.)
                  YP=YSTO(KM1)+H*(Y1(KM1)+H*(Y2(KM1)
     $            +H*Y3(KM1)/3.)/2.)
               ENDIF
               CALL gdraw (XP,YP)
            ENDDO
*
* Before Plotting (XK,YK)
            CALL gdraw (XK,YK)
*
* End of Interpolation Fork
         ENDIF
*
* End of Loop on Points to Interpolate
         KM1=K
         PKM1=PK
         XKM1=XK
         YKM1=YK
         FIFKM1=FIFK
      ENDDO
*
      IF (LINK) GOTO 1000
999   RETURN
*
* Error Recovery From Integration Failure in DIFSYS4
400   WRITE(CHAIN,*)
     $'E-CURVE,  Integration failed during iteration',ITER
      CALL XWRITE (CHAIN, 10)
      IF (ITER.EQ.1) THEN
         CHAIN =
     $   'Parametrisation POLYGONAL_LENGTH is used instead'
      ELSE
         WRITE(CHAIN,*) 'Parametrisation of iteration',ITER-1,
     $   ' is used instead'
      ENDIF
      CALL XWRITE (CHAIN, 10)
      GOTO 300
*
* Error...
500   status = -1
      RETURN
*
* Non Periodic Data With /PERIODIC Option
600   CALL XWRITE (
     $'E-CURVE,  Option /PERIODIC needs periodic data', 10)
      status = -1
      RETURN
      END
