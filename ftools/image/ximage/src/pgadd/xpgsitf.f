CXIM - Allow ITF = 10
CXIM - File generated by ./mkxpg 
CXIM - from original PGPLOT source: /lheabuild/pristine/develop/lheasoft/src/pgplot
CXIM - Solely for the use of XIMAGE
CXIM
C*XPGSITF -- set image transfer function
C%void cxpgsitf(int itf);
C+
      SUBROUTINE XPGSITF (ITF)
      INTEGER  ITF
C
C Set the Image Transfer Function for subsequent images drawn by
C PGIMAG, PGGRAY, or PGWEDG. The Image Transfer Function is used
C to map array values into the available range of color indices
C specified with routine PGSCIR or (for PGGRAY on some devices)
C into dot density.
C
C Argument:
C  ITF    (input)  : type of transfer function:
C                      ITF = 0 : linear
C                      ITF = 1 : logarithmic
C                      ITF = 2 : square-root
C--
C 15-Sep-1994 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
C
      IF (PGNOTO('XPGSITF')) RETURN
      IF ((ITF.LT.0 .OR. ITF.GT.2) .AND. ITF.NE.10) THEN
          PGITF(PGID) = 0
          CALL GRWARN('XPGSITF: argument must be 0, 1, or 2')
      ELSE
          PGITF(PGID) = ITF
      END IF
      END