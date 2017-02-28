C--- PLT user model AKIMA.
C---
      SUBROUTINE AKLIM(PAR, PLIM, NT, NTERM)
      REAL      PAR(*), PLIM(3,*)
      INTEGER   NT, NTERM
C---
C This routine must be called every time a parameter is changed.
C Calculates the coefficients and places them into the AKIMA
C common block for future reference.
C---
C PAR       I/O  Contain X and Y locations of the knots.
C PLIM      I/O  <0. means parameter is froozen.
C NT        I
C NTERM     I
C---
C AFT
C---
      INTEGER    MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      XDEL, YDEL, DIVIDE, YMIN, YMAX
      INTEGER   IND, IX
      REAL      RC
      INTEGER   NKNOT, IPER
      COMMON /AKICMN/RC(3,MXKNOT), NKNOT, XDEL, YDEL, IPER
      DATA DIVIDE/500./
C---
      NKNOT=NTERM/2
      IF(NKNOT.GT.MXKNOT) THEN
         WRITE(*,101) MXKNOT
  101    FORMAT(' AKLIM--Maximum number of knots is=',I6)
         NKNOT=MXKNOT
      END IF
      IF ( NT.EQ.-1 ) THEN
         NTERM=2*NKNOT
         RETURN
      END IF
      XDEL=(PAR(NT+NKNOT-1)-PAR(NT))/(20.*NKNOT)
      CALL AJUST(XDEL,PAR,PLIM,NT,NKNOT,YMIN,YMAX)
      YDEL=(YMAX-YMIN)/DIVIDE
      IF(YDEL.LE.0.) YDEL=1./DIVIDE
      IPER=0
      IND=NT+2*NKNOT-1
      IX=NINT(-PLIM(1,IND))
      IF(IX.EQ.NT+NKNOT .AND. PLIM(2,IND).EQ.1.) THEN
C- If first and last Y values are equal, force periodic boundary
C- condition.
         IPER=1
      END IF
      CALL AKIMA(PAR(NT),RC,NKNOT,IPER)
      RETURN
      END
C*********
      SUBROUTINE AJUST(XDEL, PAR, PLIM, NT, NKNOT, YMIN, YMAX)
      REAL      XDEL, PAR(*), PLIM(3,*), YMIN, YMAX
      INTEGER   NT, NKNOT
C---
C This routine adjusts the X-locations of the knots to enforce the
C condition that they monotonically increase in X.
C---
C XDEL      I
C PAR       I/O
C PLIM      I
C NT        I
C NKNOT     I
C YMIN        O
C YMAX        O
C---
C AFT
C---
      REAL      TMP
      INTEGER   I, IAGAIN
C---
      YMIN=PAR(NT+NKNOT)
      YMAX=YMIN
      DO 110 I=NT+NKNOT+1,NT+2*NKNOT-1
         YMIN=MIN(YMIN,PAR(I))
         YMAX=MAX(YMAX,PAR(I))
  110 CONTINUE
C---
C- Assume the list can be sorted in a single pass.  This should be
C- a good assumption for most cases.  Under no condition should two
C- x-values be the same.
  120 IAGAIN=0
      DO 180 I=NT,NT+NKNOT-2
         IF(PAR(I).EQ.PAR(I+1)) THEN
            IF(PLIM(1,I).GE.0.) THEN
               IF(PLIM(1,I+1).GE.0.) THEN
C Both free
                  PAR(i)=PAR(i)-XDEL/4.
                  PAR(i+1)=PAR(i+1)+XDEL/4.
               ELSE
C 1st free, 2nd frozen
                  PAR(i)=PAR(i)-XDEL/2.
               END IF
            ELSE
               IF(PLIM(1,I+1).GT.0) THEN
C 1st free, 2nd frozen
                  PAR(i+1)=PAR(i+1)+XDEL/2.
               END IF
            END IF
         ELSE IF(PAR(I).GT.PAR(I+1)) THEN
            IF(PLIM(1,I).GE.0.) THEN
               IF(PLIM(1,I+1).GE.0.) THEN
C- Both free, swap positions
                  TMP=PAR(I)
                  PAR(I)  =PAR(I+1)
                  PAR(I+1)=TMP
                  IF(I.GT.NT) THEN
C- If previous value messed up, set flag to go again.
                     IF(PAR(I-1).GE.PAR(I)) IAGAIN=1
                  END IF
               ELSE
C- First free, second frozen, move first
                  PAR(I)=PAR(I+1)-XDEL/2.
               END IF
            ELSE
               IF(PLIM(1,I+1).GT.0) THEN
C- First frozen, second free.
                  PAR(I+1)=PAR(I)+XDEL/2.
               ELSE
C- Both frozen, this can only arise if user incorrectly enters data.
C- Avoid compounding the error.
                  IF(PAR(I).GE.PAR(I+1)) THEN
                     PAR(I+1)=PAR(I)+XDEL/2.
                  END IF
               END IF
            END IF
         END IF
  180 CONTINUE
      IF(IAGAIN.NE.0) GOTO 120
      RETURN
      END
C*********
      REAL FUNCTION FNAKIM(X, PAR)
      REAL      X, PAR(*)
C---
C Uses the coefficient array calculated in the call to AKLIM and
C returns the Akima function value.
C---
C X         I
C PAR       I
C---
C AFT
C---
      INTEGER    MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      FNXLIM, FNSPLN
      REAL      XT
      REAL      RC, XDEL, YDEL
      INTEGER   NKNOT, IPER
      COMMON /AKICMN/RC(3,MXKNOT), NKNOT, XDEL, YDEL, IPER
C---
      XT=FNXLIM(X,PAR(1),PAR(NKNOT),IPER)
      FNAKIM=FNSPLN(XT,PAR,RC,NKNOT)
      RETURN
      END
C*********
      REAL FUNCTION FNXLIM(X, XL, XH, IPER)
      REAL      X, XL, XH
      INTEGER   IPER
C---
C Forces X to lie in the range XL to XH.
C---
C X         I/O
C XL        I
C XH        I
C IPER      I    <>0 Force function to be periodic
C---
C AFT
C---
      REAL      XT, RANGE
C---
      XT=X
      IF(IPER.NE.0) THEN
         IF(XT.LT.XL) THEN
            RANGE=XH-XL
            XT=XT-RANGE*(INT((XT-XL)/RANGE)-1)
         END IF
         IF(XT.GT.XH) THEN
            RANGE=XH-XL
            XT=XT-RANGE* INT((XT-XL)/RANGE)
         END IF
      ELSE
         XT=MIN(MAX(XL,XT),XH)
      END IF
      FNXLIM=XT
      RETURN
      END
C*********
      SUBROUTINE AKIMA(PAR, RC, NKNOT, IPER)
      INTEGER   NKNOT, IPER
      REAL      PAR(2*NKNOT), RC(3,NKNOT)
C---
C Evaluate Akima coefficients.
C See J. of the Ass. for Comp. Mac., 1970, 17, 589.
C or PPC Journal, 1985, 12, no. 10, 11.
C---
C PAR
C C
C NKNOT
C---
C AFT
C---
      REAL      XDEL, RDIF1, RDIF2, RIM2, RIM1, RIP1
      INTEGER   I, IP1
C---
      DO 120 I=1,NKNOT-1
         XDEL=PAR(I+1)-PAR(I)
         RC(3,I)=(PAR(NKNOT+I+1)-PAR(NKNOT+I))/XDEL
  120 CONTINUE
      IF(IPER.NE.0) THEN
         RC(3,NKNOT)=RC(3,1)
      ELSE
         RC(3,NKNOT)=2.*RC(3,NKNOT-1)-RC(3,NKNOT-2)
      END IF
      DO 160 I=1,NKNOT
         IF(IPER.NE.0) THEN
            IF(I.LE.2) THEN
               IF(I.EQ.1) THEN
                  RIM2=RC(3,NKNOT-2)
                  RIM1=RC(3,NKNOT-1)
               ELSE
                  RIM2=RC(3,NKNOT-1)
                  RIM1=RC(3,1)
               END IF
            ELSE
               RIM2=RC(3,I-2)
               RIM1=RC(3,I-1)
            END IF
            IF(I.LT.NKNOT) THEN
               RIP1=RC(3,I+1)
            ELSE
               RIP1=RC(3,2)
            END IF
         ELSE
            IF(I.LE.2) THEN
               XDEL=RC(3,1)-RC(3,2)
               IF(I.EQ.1) THEN
                  RIM2=RC(3,1)+2.*XDEL
                  RIM1=RC(3,1)+   XDEL
               ELSE
                  RIM2=RC(3,1)+   XDEL
                  RIM1=RC(3,1)
               END IF
            ELSE
               RIM2=RC(3,I-2)
               RIM1=RC(3,I-1)
            END IF
            IF(I.LT.NKNOT) THEN
               RIP1=RC(3,I+1)
            ELSE
               RIP1=RC(3,NKNOT)+RC(3,NKNOT-1)-RC(3,NKNOT-2)
            END IF
         END IF
         RDIF1=ABS(RIP1-RC(3,I))
         RDIF2=ABS(RIM1-RIM2)
         IF(RDIF1+RDIF2.GT.0.) THEN
            RC(1,I)=(RDIF1*RIM1+RDIF2*RC(3,I))/(RDIF1+RDIF2)
         ELSE
            RC(1,I)=(RIP1+RC(3,I))/2.
         END IF
  160 CONTINUE
      DO 180 I=1,NKNOT-1
         IP1=I+1
         XDEL=PAR(IP1)-PAR(I)
         RC(2,I)=(3.*RC(3,I)-2.*RC(1,I)-RC(1,IP1))/XDEL
         RC(3,I)=(RC(1,I)+RC(1,IP1)-2.*RC(3,I))/(XDEL*XDEL)
  180 CONTINUE
      RETURN
      END
C*********
      SUBROUTINE AKDERI(X, PAR, PLIM, DERIV, NT, NTERM)
      REAL      X, PAR(*), PLIM(3,*), DERIV(*)
      INTEGER   NT, NTERM
C---
C Calculate the derivitive of the AKIMA function with respect to
C the parameter values
C---
C X
C PAR
C PLIM
C DERIV
C NT
C NTERM
C---
C AFT
C---
      INTEGER    MXKNOT
      PARAMETER (MXKNOT=60)
      REAL      FNXLIM, FNSPLN
      REAL      RTMP(3,MXKNOT), XT, YT, UFNY0, PXEND, PTMP
      REAL      RC, XDTMP, XDEL, YDEL
      INTEGER   J
      INTEGER   NKNOT, IPER
      COMMON /AKICMN/RC(3,MXKNOT), NKNOT, XDEL, YDEL, IPER
C---
      XT=FNXLIM(X,PAR(NT),PAR(NT+NKNOT-1),IPER)
      UFNY0=FNSPLN(XT,PAR(NT),RC,NKNOT)
C- Do X locations
      DO 130 J=NT,NT+NKNOT-2
         IF(NINT(PLIM(1,J)).NE.-1) THEN
            PTMP=PAR(J)
            XDTMP=XDEL
            PAR(J)=PAR(J)+XDTMP
            IF(PAR(J).GT.PAR(J+1)) THEN
               XDTMP=(PAR(J+1)-PAR(J))/20.
               PAR(J)=PTMP+XDTMP
            END IF
            IF(J.EQ.NT) THEN
C Both endpoints move together.
               PXEND=PAR(NT+NKNOT-1)
               PAR(J)=PAR(J)+XDTMP
               PAR(NT+NKNOT-1)=PAR(NT+NKNOT-1)+XDTMP
C Try moving Y knot along spline
C               PYVAL=PAR(NT+NKNOT)
C               PAR(NT+NKNOT)=FNSPLN(PAR(J),PAR(NT),RC,NKNOT)
C               PAR(NT+2*NKNOT-1)=PAR(NT+NKNOT)
            END IF
            CALL AKIMA(PAR(NT),RTMP,NKNOT,IPER)
            YT=FNSPLN(XT,PAR(NT),RTMP,NKNOT)
            DERIV(J)=(YT-UFNY0)/XDTMP
            PAR(J)=PTMP
            IF(J.EQ.NT) THEN
               PAR(NT+NKNOT-1)=PXEND
               DERIV(NT+NKNOT-1)=0.
C Restore Y values
C               PAR(NT+NKNOT)=PYVAL
C               PAR(NT+2*NKNOT-1)=PAR(NT+NKNOT)
            END IF
         END IF
  130 CONTINUE
C- Now do Y values.
      DO 150 J=NT+NKNOT,NT+2*NKNOT-1
         IF(NINT(PLIM(1,J)).NE.-1) THEN
            PTMP=PAR(J)
            PAR(J)=PAR(J)+YDEL
            CALL AKIMA(PAR(NT),RTMP,NKNOT,IPER)
            YT=FNSPLN(XT,PAR(NT),RTMP,NKNOT)
            DERIV(J)=(YT-UFNY0)/YDEL
            PAR(J)=PTMP
         END IF
  150 CONTINUE
      RETURN
      END
C*********
      REAL FUNCTION FNSPLN(X, PAR, RC, NKNOT)
      INTEGER   NKNOT
      REAL      X, PAR(2*NKNOT), RC(3,NKNOT)
C---
C Evaluates a cubic SPLINE.  Returns the value of the spline
C approximation given by:
C   S= ((RC(3,J)*D+RC(2,J))*D+RC(1,J))*D+PAR(NKNOT+J)
C where PAR(J).LE.X .LT. PAR(J+1) and D=X-PAR(J).
C REMARKS:
C 1.  The routine assumes that the abscissae of the NKNOT
C     knots are ordered such that X(I) is less than
C     X(I+1) for I=1,...,NKNOT-1.
C 2.  The ordinate Y(NKNOT) is not used by the routine. For
C     U(K) .GT. X(NKNOT-1), the value of the spline, S(K), is
C     given by
C      S(K)=((RC(3,NKNOT-1)*D+RC(2,NKNOT-1))*D+RC(1,NKNOT-1))*D+Y(NKNOT-1)
C     where D=U(K)-X(NKNOT-1).
C
C---
C X         I    The abscissae of the point at which the cubic spline
C                -is to be evaluated.
C PAR       I    PAR(1) to PAR(NKNOT) contains the abscissae of the
C                -knots which must be ordered so that PAR(I).LT.PAR(I+1).
C                -PAR(NKNOT+1) to PAR(2*NKNOT) contains the
C                -ordinates (or function  values) of the knots.
C RC        I    SPLINE coefficients. RC is an NKNOT-1 by 3 matrix.
C---
C AFT
C---
      REAL      D, DD
      INTEGER   I
      DATA      I/1/
C---
C- Find the proper interval
      D=X-PAR(I)
C      IF(D) 5,20,15
      IF ( D.GT.0.0 ) GOTO 15
      IF ( D.EQ.0.0 ) GOTO 20
C---
    5 IF(I .EQ. 1) GOTO 20
        I=I-1
        D=X-PAR(I)
        IF(D.GE.0) GOTO 20
      GOTO 5
C---
   10 I=I+1
      D=DD
   15 IF(I .GE. NKNOT) THEN
         I=NKNOT-1
         D=X-PAR(I)
         GOTO 20
      END IF
      DD=X-PAR(I+1)
      IF(DD .GE. 0.0) GOTO 10
C- Perform evaluation
   20 FNSPLN=((RC(3,I)*D+RC(2,I))*D+RC(1,I))*D+PAR(NKNOT+I)
      RETURN
      END
