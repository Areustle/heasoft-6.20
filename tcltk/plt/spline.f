C---SPLINE.FOR   Contains the FIT Spline model routines.
C---
      SUBROUTINE SPLIM(PAR, PLIM, NT, NTERM)
      REAL      PAR(*), PLIM(3,*)
      INTEGER   NT, NTERM
C---
C This routine must be called every time a parameter is changed.
C---
C PAR(NTERM)    I    Contain X and Y locations of the knots.
C PLIM(1,NTERM) I/O  <0. means parameter is froozen.
C NKNOT         I/O  =NTERM/2
C---
C- MXKNOT=MXPAR/2
C---
      INTEGER   MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      YMIN, YMAX
      INTEGER   IND, IX
C
      REAL      Y2, U, XDEL, YDEL
      INTEGER   NKNOT, IPER
      COMMON /SPLCMN/Y2(MXKNOT),U(MXKNOT),XDEL,YDEL,NKNOT,IPER
C---
      NKNOT=NTERM/2
      IF(NKNOT.GT.MXKNOT) THEN
         WRITE(*,101) MXKNOT
  101    FORMAT(' SPLIM--Maximum number of knots is=',I6)
         NKNOT=MXKNOT
      END IF
      IF ( NT.EQ.-1 ) THEN
         NTERM=2*NKNOT
         RETURN
      END IF
      XDEL=(PAR(NT+NKNOT-1)-PAR(NT))/(20.*NKNOT)
      CALL AJUST(XDEL,PAR,PLIM,NT,NKNOT,YMIN,YMAX)
      YDEL=(YMAX-YMIN)/500.
      IF(YDEL.LE.0.) YDEL=1./500.
      IPER=0
      IND=NT+2*NKNOT-1
      IX=NINT(-PLIM(1,IND))
      IF(IX.EQ.NT+NKNOT .AND. PLIM(2,IND).EQ.1.) THEN
C- If first and last Y values are equal, force periodic boundary
C- condition.
         IPER=1
      END IF
      CALL SPLINE(PAR(NT), PAR(NT+NKNOT), Y2, U, NKNOT, IPER)
      RETURN
      END
C*********
      REAL FUNCTION FNSP(X, PAR)
      REAL      X, PAR(*)
C---
C Evaluate Spline for use in FIT.
C---
      INTEGER   MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      FNECSP
C
      REAL      Y2, U, XDEL, YDEL
      INTEGER   NKNOT, IPER
      COMMON /SPLCMN/Y2(MXKNOT),U(MXKNOT),XDEL,YDEL,NKNOT,IPER
C---
      FNSP=FNECSP(X, PAR, Y2, NKNOT, IPER)
      RETURN
      END
C*********
      REAL FUNCTION FNECSP(X, PAR, Y2, NKNOT, IPER)
      REAL      X, PAR(*), Y2(*)
      INTEGER   NKNOT, IPER
C---
C Evaluates a cubic SPLINE.  This routine finds the value of J such
C that PAR(J)<=XT<PAR(J+1) and calculates the spline at that point.
C---
C XT  I    The abscissae of the point at which the cubic
C     spline is to be evaluated.
C PAR(2*NKNOT) I    PAR(1) to PAR(NKNOT) contains the abscissae
C      of the knots which must be ordered so that
C     PAR(I).LT.PAR(I+1).  PAR(NKNOT+1) to PAR(2*NKNOT)
C     contains the ordinates (or function  values) of
C     the knots.
C-----------------------------------------------------------------------
      INTEGER   MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      FNXLIM
C
      REAL      A, B, H, TMP, XT
      INTEGER   I
C
      SAVE I
      DATA I/1/
C---
      FNECSP=0.
      XT=FNXLIM(X,PAR(1),PAR(NKNOT),IPER)
C---
C- Find the proper interval
      B=XT-PAR(I)
C      IF(B) 5,20,15
      IF(B.GT.0.0) GOTO 15
      IF(B.EQ.0.0) GOTO 20
C---
    5 IF(I.EQ.1) GOTO 20
         I=I-1
         B=XT-PAR(I)
         IF(B.GE.0) GOTO 20
      GOTO 5
C---
   10 I=I+1
      B=TMP
   15 IF(I .GE. NKNOT) THEN
         I=NKNOT-1
         B=XT-PAR(I)
         GOTO 20
      END IF
      TMP=XT-PAR(I+1)
      IF(TMP .GE. 0.0) GOTO 10
C--- Perform evaluation
   20 H=PAR(I+1)-PAR(I)
      A=(PAR(I+1)-XT)/H
      B=B/H
      FNECSP=A*PAR(NKNOT+I)+B*PAR(NKNOT+I+1)+
     :    ((A*A*A-A)*Y2(I)+(B*B*B-B)*Y2(I+1))*H*H/6.
      RETURN
      END
C*********
      SUBROUTINE SPDERI(X, PAR, PLIM, DERIV, NT, NTERM)
      REAL      X, PAR(*), PLIM(3,*), DERIV(*)
      INTEGER   NT, NTERM
C---
C Calculates the derivative of the spline at the point X for
C changes in the location of the knot.
C---
      INTEGER   MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      FNSP, FNXLIM
C
      REAL      XDTMP, PTMP, PXEND, XT, Y0, YT
      INTEGER   J
C
      REAL      Y2, U, XDEL, YDEL
      INTEGER   NKNOT, IPER
      COMMON /SPLCMN/Y2(MXKNOT),U(MXKNOT),XDEL,YDEL,NKNOT,IPER
C---
      XT=FNXLIM(X,PAR(NT),PAR(NT+NKNOT-1),IPER)
      Y0=FNSP(XT,PAR(NT))
C- Do X locations
      DO 130 J=NT,NT+NKNOT-2
         IF(NINT(PLIM(1,J)).NE.-1) THEN
            PTMP=PAR(J)
            XDTMP=XDEL
            PAR(J)=PTMP+XDTMP
            IF(PAR(J).GT.PAR(J+1)) THEN
               XDTMP=(PAR(J+1)-PAR(J))/10.
               PAR(J)=PTMP+XDTMP
            END IF
            IF(J.EQ.NT) THEN
C Both endpoints move together.
               PXEND=PAR(NT+NKNOT-1)
               PAR(J)=PAR(J)+XDTMP
               PAR(NT+NKNOT-1)=PAR(NT+NKNOT-1)+XDTMP
            END IF
            CALL SPLINE(PAR(NT), PAR(NT+NKNOT), Y2, U, NKNOT, IPER)
            YT=FNSP(XT,PAR(NT))
            DERIV(J)=(YT-Y0)/XDTMP
            PAR(J)  =PTMP
            IF(J.EQ.NT) THEN
               PAR(NT+NKNOT-1)=PXEND
               DERIV(NT+NKNOT-1)=0.
            END IF
         END IF
  130 CONTINUE
C- Now do Y values.
      DO 150 J=NT+NKNOT,NT+2*NKNOT-1
         IF(NINT(PLIM(1,J)).NE.-1) THEN
            PTMP=PAR(J)
            PAR(J)=PAR(J)+YDEL
            CALL SPLINE(PAR(NT), PAR(NT+NKNOT), Y2, U, NKNOT, IPER)
            YT=FNSP(XT,PAR(NT))
            DERIV(J)=(YT-Y0)/YDEL
            PAR(J)=PTMP
         END IF
  150 CONTINUE
      CALL SPLINE(PAR(NT), PAR(NT+NKNOT), Y2, U, NKNOT, IPER)
      RETURN
      END
C*********
      SUBROUTINE SPLINE(X, Y, Y2, U, NKNOT, IPER)
      REAL      X(*), Y(*), Y2(*), U(*)
      INTEGER   NKNOT, IPER
C---
C This routine calculates the Y2 array that is used by the UFNY routine
C to interpolate.  The U array is used for scratch space.  This routine
C solves equation 3.3.7 in "Numerical Recipes" by Press, Flannery,
C Teukolsky and Vetterling, 1988 edition.
C If IPER=0 then the Y2(1)=Y2(NKNOT)=0 boundary condition is used.
C If IPER<>0, then the first derivative is forced to match at the
C beginning and ending of the interval.  This creates a smooth
C periodic function.
C---
C X(*)   I    The X locations of the knots
C Y(*)   I    The Y locations of the knots
C---
      REAL      AJ, RDIAG, SLOP, SLOP1, SLOPN
      INTEGER   J
C---
C- Solve the tridiagonal matrix given in 3.3.7 of Press.  In the
C- first pass, eliminate terms below the diagonal, and set terms on
C- the diagonal equal to 1.  The U(*) represent the terms above the
C- diagonal, and Y2(*) the current right-hand side of the equation.
      IF(IPER.EQ.0) THEN
         Y2(1)=0.
         U(1)=0.
      ELSE
         SLOP1=(Y(2)-Y(1))/(X(2)-X(1))
         SLOPN=(Y(NKNOT)-Y(NKNOT-1))/(X(NKNOT)-X(NKNOT-1))
         SLOP=(SLOP1+SLOPN)/2.
         RDIAG=(X(2)-X(1))/3.
         Y2(1)=(SLOP1-SLOP)/RDIAG
         U(1)=1./2.
      END IF
C-
      DO 120 J=2,NKNOT-1
         AJ=(X(J)-X(J-1))/6.
         RDIAG=(X(J+1)-X(J-1))/3.-AJ*U(J-1)
         Y2(J)=((Y(J+1)-Y(J))/(X(J+1)-X(J))-
     :         (Y(J)-Y(J-1))/(X(J)-X(J-1))-AJ*Y2(J-1))/RDIAG
         U(J)=(X(J+1)-X(J))/(6.*RDIAG)
  120 CONTINUE
C-
      IF(IPER.EQ.0) THEN
         Y2(NKNOT)=0.
      ELSE
         AJ=(X(NKNOT)-X(NKNOT-1))/6.
         RDIAG=AJ*(2.-U(NKNOT-1))
         Y2(NKNOT)=(SLOP-SLOPN-AJ*Y2(NKNOT-1))/RDIAG
      END IF
C---
C- Now eliminate terms above the diagonal.
      DO 140 J=NKNOT-1,1,-1
         Y2(J)=Y2(J)-U(J)*Y2(J+1)
  140 CONTINUE
      RETURN
      END
