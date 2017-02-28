C-----------------------------------------------------------------------------
C   slatec.f: --- numerical routines from the SLATEC library
C
C   DESCRIPTION:
C       This file contains a selected set of numerical routines from the
C       public-domain library SLATEC. Comprehensive information about
C       this library and all source codes can be found at:
C       gopher://archives.math.utk.edu:70/11/software/multi-platform/SLATEC
C
C       The complete list of functions directly called by various SAXDAS
C       modules is:
C
C          SLATEC func.  purpose                    used by
C          --------------------------------------------------------------
C          DNLS1E        non-linear least-square    * calcgain (calcgain.c)
C                        minimization
C
C          DPCHIM        calculate cubic Hermite    * evelin (gaincorr.c)
C                        interpolant
C
C          DPCHFE        evaluate cubic Hermite     * evelin (gaincorr.c)
C                        interpolant calculated
C                        with DPCHIM
C
C          DPOLFT        calculate fit polynomial   * foting (OBT_UTC.c)
C                                                   * evelin (gaincorr.c)
C
C          DP1VLU        evaluate fit polynomial    * foting (OBT_UTC.c)
C                        calculated with DPOLFT     * evelin (gaincorr.c)
C
C       Most of the above functions call anciliary SLATEC routines whose
C       source code is comprised in this file as well. Documentation about
C       the calling syntax, parameter description, etc. for each function
C       is contained in comment blocks preceding the actual function bodies.
C
C       C-callable function macros for all the SLATEC routines directly used
C       by SAXDAS modules have been defined using the general C-Fortran
C       interface'cfortran.h'. These macros are contained in file 'slatec.h'
C       and start with the prefix `SLATEC_', e.g., the C-macro for `DNLS1E'
C       is `SLATEC_DNLS1E'.
C
C       Several SLATEC routines call a function `D1MACH' which returns
C       various machine-dependent floating-point constants (machine
C       epsilon, etc.). The original SLATEC-D1MACH defines those
C       constants for a huge collection of different machine types, however,
C       it does require manual editing when being ported to a new platform.
C       For this reason, SLATEC-D1MACH has been replaced by an equivalent
C       generic C-function which utilizes ANSI-C macros from <float.h>.
C       This function is part of the file `slatec_c.c'.
C       The general SLATEC error routine `XERMSG' has been re-coded in
C       Fortran (see end of this file). It is a simple-minded thing which
C       does a WRITE of fatal error to <stdout>.
C
C   CHANGE HISTORY:
C       UL, 05/03/96: 1.0.0: assembly of routines needed by SAXDAS modules
C
C   AUTHOR:
C       see AUTHOR sections of individual routines
C
C-----------------------------------------------------------------------------
       
      SUBROUTINE DPCHIM (N, X, F, D, INCFD, IERR)
C***BEGIN PROLOGUE  DPCHIM
C***PURPOSE  Set derivatives needed to determine a monotone piecewise
C            cubic Hermite interpolant to given data.  Boundary values
C            are provided which are compatible with monotonicity.  The
C            interpolant will have an extremum at each point where mono-
C            tonicity switches direction.  (See DPCHIC if user control
C            is desired over boundary or switch conditions.)
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      DOUBLE PRECISION (PCHIM-S, DPCHIM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHIM:  Piecewise Cubic Hermite Interpolation to
C                  Monotone data.
C
C     Sets derivatives needed to determine a monotone piecewise cubic
C     Hermite interpolant to the data given in X and F.
C
C     Default boundary conditions are provided which are compatible
C     with monotonicity.  (See DPCHIC if user control of boundary con-
C     ditions is desired.)
C
C     If the data are only piecewise monotonic, the interpolant will
C     have an extremum at each point where monotonicity switches direc-
C     tion.  (See DPCHIC if user control is desired in such cases.)
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by DPCHFE or DPCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
C
C        CALL  DPCHIM (N, X, F, D, INCFD, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of dependent variable values to be
C           interpolated.  F(1+(I-1)*INCFD) is value corresponding to
C           X(I).  DPCHIM is designed for monotonic data, but it will
C           work for any F-array.  It will force extrema at points where
C           monotonicity switches direction.  If some other treatment of
C           switch points is desired, DPCHIC should be used instead.
C                                     -----
C     D -- (output) real*8 array of derivative values at the data
C           points.  If the data are monotonic, these values will
C           determine a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that IERR switches in the direction
C                 of monotonicity were detected.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  DPCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820201  1. Introduced  DPCHST  to reduce possible over/under-
C             flow problems.
C           2. Rearranged derivative formula for same reason.
C   820602  1. Modified end conditions to be continuous functions
C             of data when monotonicity switches in next interval.
C           2. Modified formulas so end conditions are less prone
C             of over/underflow problems.
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. name(s).
C   870813  Updated Reference 1.
C   890206  Corrected XERROR calls.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  DPCHIM
C  Programming notes:
C
C     1. The function  DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. To produce a single precision version, simply:
C        a. Change DPCHIM to PCHIM wherever it occurs,
C        b. Change DPCHST to PCHST wherever it occurs,
C        c. Change all references to the Fortran intrinsics to their
C           single precision equivalents,
C        d. Change the double precision declarations to real, and
C        e. Change the constants ZERO and THREE to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      DOUBLE PRECISION  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, DSAVE,
     *      H1, H2, HSUM, HSUMT3, THREE, W1, W2, ZERO
      SAVE ZERO, THREE
      DOUBLE PRECISION  DPCHST
      DATA  ZERO /0.D0/, THREE/3.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHIM
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
      IERR = 0
      NLESS1 = N - 1
      H1 = X(2) - X(1)
      DEL1 = (F(1,2) - F(1,1))/H1
      DSAVE = DEL1
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      H2 = X(3) - X(2)
      DEL2 = (F(1,3) - F(1,2))/H2
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H1 + H2
      W1 = (H1 + HSUM)/HSUM
      W2 = -H1/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         H1 = H2
         H2 = X(I+1) - X(I)
         HSUM = H1 + H2
         DEL1 = DEL2
         DEL2 = (F(1,I+1) - F(1,I))/H2
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( DPCHST(DEL1,DEL2) )  42, 41, 45
C
C        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY.
C
   41    CONTINUE
         IF (DEL2 .EQ. ZERO)  GO TO 50
         IF ( DPCHST(DSAVE,DEL2) .LT. ZERO)  IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
   42    CONTINUE
         IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
   45    CONTINUE
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H1)/HSUMT3
         W2 = (HSUM + H2)/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H2/HSUM
      W2 = (H2 + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( DPCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( DPCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHIM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHIM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHIM',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C------------- LAST LINE OF DPCHIM FOLLOWS -----------------------------
      END



      SUBROUTINE DPCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)
C***BEGIN PROLOGUE  DPCHFE
C***PURPOSE  Evaluate a piecewise cubic Hermite function at an array of
C            points.  May be used by itself for Hermite interpolation,
C            or as an evaluator for DPCHIM or DPCHIC.
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E3
C***TYPE      DOUBLE PRECISION (PCHFE-S, DPCHFE-D)
C***KEYWORDS  CUBIC HERMITE EVALUATION, HERMITE INTERPOLATION, PCHIP,
C             PIECEWISE CUBIC EVALUATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          DPCHFE:  Piecewise Cubic Hermite Function Evaluator
C
C     Evaluates the cubic Hermite function defined by  N, X, F, D  at
C     the points  XE(J), J=1(1)NE.
C
C     To provide compatibility with DPCHIM and DPCHIC, includes an
C     increment between successive values of the F- and D-arrays.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, NE, IERR
C        DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE)
C        LOGICAL  SKIP
C
C        CALL  DPCHFE (N, X, F, D, INCFD, SKIP, NE, XE, FE, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C
C     X -- (input) real*8 array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real*8 array of function values.  F(1+(I-1)*INCFD) is
C           the value corresponding to X(I).
C
C     D -- (input) real*8 array of derivative values.  D(1+(I-1)*INCFD)
C           is the value corresponding to X(I).
C
C     INCFD -- (input) increment between successive values in F and D.
C           (Error return if  INCFD.LT.1 .)
C
C     SKIP -- (input/output) logical variable which should be set to
C           .TRUE. if the user wishes to skip checks for validity of
C           preceding parameters, or to .FALSE. otherwise.
C           This will save time in case these checks have already
C           been performed (say, in DPCHIM or DPCHIC).
C           SKIP will be set to .TRUE. on normal return.
C
C     NE -- (input) number of evaluation points.  (Error return if
C           NE.LT.1 .)
C
C     XE -- (input) real*8 array of points at which the function is to
C           be evaluated.
C
C          NOTES:
C           1. The evaluation will be most efficient if the elements
C              of XE are increasing relative to X;
C              that is,   XE(J) .GE. X(I)
C              implies    XE(K) .GE. X(I),  all K.GE.J .
C           2. If any of the XE are outside the interval [X(1),X(N)],
C              values are extrapolated from the nearest extreme cubic,
C              and a warning error is returned.
C
C     FE -- (output) real*8 array of values of the cubic Hermite
C           function defined by  N, X, F, D  at the points  XE.
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that extrapolation was performed at
C                 IERR points.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C              IERR = -4  if NE.LT.1 .
C             (The FE-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DCHFEV, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811020  DATE WRITTEN
C   820803  Minor cosmetic changes for release 1.
C   870707  Corrected XERROR calls for d.p. name(s).
C   890206  Corrected XERROR calls.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DPCHFE
C  Programming notes:
C
C     1. To produce a single precision version, simply:
C        a. Change DPCHFE to PCHFE, and DCHFEV to CHFEV, wherever they
C           occur,
C        b. Change the double precision declaration to real,
C
C     2. Most of the coding between the call to DCHFEV and the end of
C        the IR-loop could be eliminated if it were permissible to
C        assume that XE is ordered relative to X.
C
C     3. DCHFEV does not assume that X1 is less than X2.  thus, it would
C        be possible to write a version of DPCHFE that assumes a
C        decreasing X-array by simply running the IR-loop backwards
C        (and reversing the order of appropriate tests).
C
C     4. The present code has a minor bug, which I have decided is not
C        worth the effort that would be required to fix it.
C        If XE contains points in [X(N-1),X(N)], followed by points .LT.
C        X(N-1), followed by points .GT.X(N), the extrapolation points
C        will be counted (at least) twice in the total returned in IERR.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, NE, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), XE(*), FE(*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IERC, IR, J, JFIRST, NEXT(2), NJ
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHFE
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      IF ( NE.LT.1 )  GO TO 5004
      IERR = 0
      SKIP = .TRUE.
C
C  LOOP OVER INTERVALS.        (   INTERVAL INDEX IS  IL = IR-1  . )
C                              ( INTERVAL IS X(IL).LE.X.LT.X(IR) . )
      JFIRST = 1
      IR = 2
   10 CONTINUE
C
C     SKIP OUT OF LOOP IF HAVE PROCESSED ALL EVALUATION POINTS.
C
         IF (JFIRST .GT. NE)  GO TO 5000
C
C     LOCATE ALL POINTS IN INTERVAL.
C
         DO 20  J = JFIRST, NE
            IF (XE(J) .GE. X(IR))  GO TO 30
   20    CONTINUE
         J = NE + 1
         GO TO 40
C
C     HAVE LOCATED FIRST POINT BEYOND INTERVAL.
C
   30    CONTINUE
         IF (IR .EQ. N)  J = NE + 1
C
   40    CONTINUE
         NJ = J - JFIRST
C
C     SKIP EVALUATION IF NO POINTS IN INTERVAL.
C
         IF (NJ .EQ. 0)  GO TO 50
C
C     EVALUATE CUBIC AT XE(I),  I = JFIRST (1) J-1 .
C
C       ----------------------------------------------------------------
        CALL DCHFEV (X(IR-1),X(IR), F(1,IR-1),F(1,IR), D(1,IR-1),D(1,IR)
     *              ,NJ, XE(JFIRST), FE(JFIRST), NEXT, IERC)
C       ----------------------------------------------------------------
         IF (IERC .LT. 0)  GO TO 5005
C
         IF (NEXT(2) .EQ. 0)  GO TO 42
C        IF (NEXT(2) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(2) TO THE
C           RIGHT OF X(IR).
C
            IF (IR .LT. N)  GO TO 41
C           IF (IR .EQ. N)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(2)
               GO TO 42
   41       CONTINUE
C           ELSE
C              WE SHOULD NEVER HAVE GOTTEN HERE.
               GO TO 5005
C           ENDIF
C        ENDIF
   42    CONTINUE
C
         IF (NEXT(1) .EQ. 0)  GO TO 49
C        IF (NEXT(1) .GT. 0)  THEN
C           IN THE CURRENT SET OF XE-POINTS, THERE ARE NEXT(1) TO THE
C           LEFT OF X(IR-1).
C
            IF (IR .GT. 2)  GO TO 43
C           IF (IR .EQ. 2)  THEN
C              THESE ARE ACTUALLY EXTRAPOLATION POINTS.
               IERR = IERR + NEXT(1)
               GO TO 49
   43       CONTINUE
C           ELSE
C              XE IS NOT ORDERED RELATIVE TO X, SO MUST ADJUST
C              EVALUATION INTERVAL.
C
C              FIRST, LOCATE FIRST POINT TO LEFT OF X(IR-1).
               DO 44  I = JFIRST, J-1
                  IF (XE(I) .LT. X(IR-1))  GO TO 45
   44          CONTINUE
C              NOTE-- CANNOT DROP THROUGH HERE UNLESS THERE IS AN ERROR
C                     IN DCHFEV.
               GO TO 5005
C
   45          CONTINUE
C              RESET J.  (THIS WILL BE THE NEW JFIRST.)
               J = I
C
C              NOW FIND OUT HOW FAR TO BACK UP IN THE X-ARRAY.
               DO 46  I = 1, IR-1
                  IF (XE(J) .LT. X(I)) GO TO 47
   46          CONTINUE
C              NB-- CAN NEVER DROP THROUGH HERE, SINCE XE(J).LT.X(IR-1).
C
   47          CONTINUE
C              AT THIS POINT, EITHER  XE(J) .LT. X(1)
C                 OR      X(I-1) .LE. XE(J) .LT. X(I) .
C              RESET IR, RECOGNIZING THAT IT WILL BE INCREMENTED BEFORE
C              CYCLING.
               IR = MAX(1, I-1)
C           ENDIF
C        ENDIF
   49    CONTINUE
C
         JFIRST = J
C
C     END OF IR-LOOP.
C
   50 CONTINUE
      IR = IR + 1
      IF (IR .LE. N)  GO TO 10
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHFE', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C
 5004 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5005 CONTINUE
C     ERROR RETURN FROM DCHFEV.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -5
      CALL XERMSG ('SLATEC', 'DPCHFE',
     +   'ERROR RETURN FROM DCHFEV -- FATAL', IERR, 2)
      RETURN
C------------- LAST LINE OF DPCHFE FOLLOWS -----------------------------
      END



      DOUBLE PRECISION FUNCTION DPCHST (ARG1, ARG2)
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DPCHST to PCHST wherever it occurs,
C        b. Change all references to the Fortran intrinsics to their
C           single precision equivalents,
C        c. Change the double precision declarations to real, and
C        d. Change the constants  ZERO  and  ONE  to single precision.
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  ARG1, ARG2
C
C  DECLARE LOCAL VARIABLES.
C
      DOUBLE PRECISION  ONE, ZERO
      SAVE ZERO, ONE
      DATA  ZERO /0.D0/,  ONE/1.D0/
C
C  PERFORM THE TEST.
C
C***FIRST EXECUTABLE STATEMENT  DPCHST
      DPCHST = SIGN(ONE,ARG1) * SIGN(ONE,ARG2)
      IF ((ARG1.EQ.ZERO) .OR. (ARG2.EQ.ZERO))  DPCHST = ZERO
C
      RETURN
C------------- LAST LINE OF DPCHST FOLLOWS -----------------------------
      END



      SUBROUTINE DCHFEV (X1, X2, F1, F2, D1, D2, NE, XE, FE, NEXT, IERR)
C  Programming notes:
C
C     To produce a single precision version, simply:
C        a. Change DCHFEV to CHFEV wherever it occurs,
C        b. Change the double precision declaration to real, and
C        c. Change the constant ZERO to single precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, XE(*), FE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      DOUBLE PRECISION  C2, C3, DEL1, DEL2, DELTA, H, X, XMI, XMA,
     * ZERO
      SAVE ZERO
      DATA  ZERO /0.D0/
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DCHFEV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DCHFEV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DCHFEV', 'INTERVAL ENDPOINTS EQUAL',
     +   IERR, 1)
      RETURN
C------------- LAST LINE OF DCHFEV FOLLOWS -----------------------------
      END



      SUBROUTINE DPOLFT (N, X, Y, W, MAXDEG, NDEG, EPS, R, IERR, A)
C***BEGIN PROLOGUE  DPOLFT
C***PURPOSE  Fit discrete data in a least squares sense by polynomials
C            in one variable.
C***LIBRARY   SLATEC
C***CATEGORY  K1A1A2
C***TYPE      DOUBLE PRECISION (POLFIT-S, DPOLFT-D)
C***KEYWORDS  CURVE FITTING, DATA FITTING, LEAST SQUARES, POLYNOMIAL FIT
C***AUTHOR  Shampine, L. F., (SNLA)
C           Davenport, S. M., (SNLA)
C           Huddleston, R. E., (SNLL)
C***DESCRIPTION
C
C     Abstract
C
C     Given a collection of points X(I) and a set of values Y(I) which
C     correspond to some function or measurement at each of the X(I),
C     subroutine  DPOLFT  computes the weighted least-squares polynomial
C     fits of all degrees up to some degree either specified by the user
C     or determined by the routine.  The fits thus obtained are in
C     orthogonal polynomial form.  Subroutine  DP1VLU  may then be
C     called to evaluate the fitted polynomials and any of their
C     derivatives at any point.  The subroutine  DPCOEF  may be used to
C     express the polynomial fits as powers of (X-C) for any specified
C     point C.
C
C     The parameters for  DPOLFT  are
C
C     Input -- All TYPE REAL variables are DOUBLE PRECISION
C         N -      the number of data points.  The arrays X, Y and W
C                  must be dimensioned at least  N  (N .GE. 1).
C         X -      array of values of the independent variable.  These
C                  values may appear in any order and need not all be
C                  distinct.
C         Y -      array of corresponding function values.
C         W -      array of positive values to be used as weights.  If
C                  W(1) is negative,  DPOLFT  will set all the weights
C                  to 1.0, which means unweighted least squares error
C                  will be minimized.  To minimize relative error, the
C                  user should set the weights to:  W(I) = 1.0/Y(I)**2,
C                  I = 1,...,N .
C         MAXDEG - maximum degree to be allowed for polynomial fit.
C                  MAXDEG  may be any non-negative integer less than  N.
C                  Note -- MAXDEG  cannot be equal to  N-1  when a
C                  statistical test is to be used for degree selection,
C                  i.e., when input value of  EPS  is negative.
C         EPS -    specifies the criterion to be used in determining
C                  the degree of fit to be computed.
C                  (1)  If  EPS  is input negative,  DPOLFT  chooses the
C                       degree based on a statistical F test of
C                       significance.  One of three possible
C                       significance levels will be used:  .01, .05 or
C                       .10.  If  EPS=-1.0 , the routine will
C                       automatically select one of these levels based
C                       on the number of data points and the maximum
C                       degree to be considered.  If  EPS  is input as
C                       -.01, -.05, or -.10, a significance level of
C                       .01, .05, or .10, respectively, will be used.
C                  (2)  If  EPS  is set to 0.,  DPOLFT  computes the
C                       polynomials of degrees 0 through  MAXDEG .
C                  (3)  If  EPS  is input positive,  EPS  is the RMS
C                       error tolerance which must be satisfied by the
C                       fitted polynomial.  DPOLFT  will increase the
C                       degree of fit until this criterion is met or
C                       until the maximum degree is reached.
C
C     Output -- All TYPE REAL variables are DOUBLE PRECISION
C         NDEG -   degree of the highest degree fit computed.
C         EPS -    RMS error of the polynomial of degree  NDEG .
C         R -      vector of dimension at least NDEG containing values
C                  of the fit of degree  NDEG  at each of the  X(I) .
C                  Except when the statistical test is used, these
C                  values are more accurate than results from subroutine
C                  DP1VLU  normally are.
C         IERR -   error flag with the following possible values.
C             1 -- indicates normal execution, i.e., either
C                  (1)  the input value of  EPS  was negative, and the
C                       computed polynomial fit of degree  NDEG
C                       satisfies the specified F test, or
C                  (2)  the input value of  EPS  was 0., and the fits of
C                       all degrees up to  MAXDEG  are complete, or
C                  (3)  the input value of  EPS  was positive, and the
C                       polynomial of degree  NDEG  satisfies the RMS
C                       error requirement.
C             2 -- invalid input parameter.  At least one of the input
C                  parameters has an illegal value and must be corrected
C                  before  DPOLFT  can proceed.  Valid input results
C                  when the following restrictions are observed
C                       N .GE. 1
C                       0 .LE. MAXDEG .LE. N-1  for  EPS .GE. 0.
C                       0 .LE. MAXDEG .LE. N-2  for  EPS .LT. 0.
C                       W(1)=-1.0  or  W(I) .GT. 0., I=1,...,N .
C             3 -- cannot satisfy the RMS error requirement with a
C                  polynomial of degree no greater than  MAXDEG .  Best
C                  fit found is of degree  MAXDEG .
C             4 -- cannot satisfy the test for significance using
C                  current value of  MAXDEG .  Statistically, the
C                  best fit found is of order  NORD .  (In this case,
C                  NDEG will have one of the values:  MAXDEG-2,
C                  MAXDEG-1, or MAXDEG).  Using a higher value of
C                  MAXDEG  may result in passing the test.
C         A -      work and output array having at least 3N+3MAXDEG+3
C                  locations
C
C     Note - DPOLFT  calculates all fits of degrees up to and including
C            NDEG .  Any or all of these fits can be evaluated or
C            expressed as powers of (X-C) using  DP1VLU  and  DPCOEF
C            after just one call to  DPOLFT .
C
C***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
C                 Curve fitting by polynomials in one variable, Report
C                 SLA-74-0270, Sandia Laboratories, June 1974.
C***ROUTINES CALLED  DP1VLU, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   740601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900911  Added variable YP to DOUBLE PRECISION declaration.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920527  Corrected erroneous statements in DESCRIPTION.  (WRB)
C***END PROLOGUE  DPOLFT
      INTEGER I,IDEGF,IERR,J,JP1,JPAS,K1,K1PJ,K2,K2PJ,K3,K3PI,K4,
     * K4PI,K5,K5PI,KSIG,M,MAXDEG,MOP1,NDEG,NDER,NFAIL
      DOUBLE PRECISION TEMD1,TEMD2
      DOUBLE PRECISION A(*),DEGF,DEN,EPS,ETST,F,FCRIT,R(*),SIG,SIGJ,
     * SIGJM1,SIGPAS,TEMP,X(*),XM,Y(*),YP(1),W(*),W1,W11
      DOUBLE PRECISION CO(4,3)
      SAVE CO
      DATA  CO(1,1), CO(2,1), CO(3,1), CO(4,1), CO(1,2), CO(2,2),
     1      CO(3,2), CO(4,2), CO(1,3), CO(2,3), CO(3,3),
     2  CO(4,3)/-13.086850D0,-2.4648165D0,-3.3846535D0,-1.2973162D0,
     3          -3.3381146D0,-1.7812271D0,-3.2578406D0,-1.6589279D0,
     4          -1.6282703D0,-1.3152745D0,-3.2640179D0,-1.9829776D0/
C***FIRST EXECUTABLE STATEMENT  DPOLFT
      M = ABS(N)
      IF (M .EQ. 0) GO TO 30
      IF (MAXDEG .LT. 0) GO TO 30
      A(1) = MAXDEG
      MOP1 = MAXDEG + 1
      IF (M .LT. MOP1) GO TO 30
      IF (EPS .LT. 0.0D0 .AND.  M .EQ. MOP1) GO TO 30
      XM = M
      ETST = EPS*EPS*XM
      IF (W(1) .LT. 0.0D0) GO TO 2
      DO 1 I = 1,M
        IF (W(I) .LE. 0.0D0) GO TO 30
 1      CONTINUE
      GO TO 4
 2    DO 3 I = 1,M
 3      W(I) = 1.0D0
 4    IF (EPS .GE. 0.0D0) GO TO 8
C
C DETERMINE SIGNIFICANCE LEVEL INDEX TO BE USED IN STATISTICAL TEST FOR
C CHOOSING DEGREE OF POLYNOMIAL FIT
C
      IF (EPS .GT. (-.55D0)) GO TO 5
      IDEGF = M - MAXDEG - 1
      KSIG = 1
      IF (IDEGF .LT. 10) KSIG = 2
      IF (IDEGF .LT. 5) KSIG = 3
      GO TO 8
 5    KSIG = 1
      IF (EPS .LT. (-.03D0)) KSIG = 2
      IF (EPS .LT. (-.07D0)) KSIG = 3
C
C INITIALIZE INDEXES AND COEFFICIENTS FOR FITTING
C
 8    K1 = MAXDEG + 1
      K2 = K1 + MAXDEG
      K3 = K2 + MAXDEG + 2
      K4 = K3 + M
      K5 = K4 + M
      DO 9 I = 2,K4
 9      A(I) = 0.0D0
      W11 = 0.0D0
      IF (N .LT. 0) GO TO 11
C
C UNCONSTRAINED CASE
C
      DO 10 I = 1,M
        K4PI = K4 + I
        A(K4PI) = 1.0D0
 10     W11 = W11 + W(I)
      GO TO 13
C
C CONSTRAINED CASE
C
 11   DO 12 I = 1,M
        K4PI = K4 + I
 12     W11 = W11 + W(I)*A(K4PI)**2
C
C COMPUTE FIT OF DEGREE ZERO
C
 13   TEMD1 = 0.0D0
      DO 14 I = 1,M
        K4PI = K4 + I
        TEMD1 = TEMD1 + W(I)*Y(I)*A(K4PI)
 14     CONTINUE
      TEMD1 = TEMD1/W11
      A(K2+1) = TEMD1
      SIGJ = 0.0D0
      DO 15 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = TEMD1*A(K4PI)
        R(I) = TEMD2
        A(K5PI) = TEMD2 - R(I)
 15     SIGJ = SIGJ + W(I)*((Y(I)-R(I)) - A(K5PI))**2
      J = 0
C
C SEE IF POLYNOMIAL OF DEGREE 0 SATISFIES THE DEGREE SELECTION CRITERION
C
      IF (EPS) 24,26,27
C
C INCREMENT DEGREE
C
 16   J = J + 1
      JP1 = J + 1
      K1PJ = K1 + J
      K2PJ = K2 + J
      SIGJM1 = SIGJ
C
C COMPUTE NEW B COEFFICIENT EXCEPT WHEN J = 1
C
      IF (J .GT. 1) A(K1PJ) = W11/W1
C
C COMPUTE NEW A COEFFICIENT
C
      TEMD1 = 0.0D0
      DO 18 I = 1,M
        K4PI = K4 + I
        TEMD2 = A(K4PI)
        TEMD1 = TEMD1 + X(I)*W(I)*TEMD2*TEMD2
 18     CONTINUE
      A(JP1) = TEMD1/W11
C
C EVALUATE ORTHOGONAL POLYNOMIAL AT DATA POINTS
C
      W1 = W11
      W11 = 0.0D0
      DO 19 I = 1,M
        K3PI = K3 + I
        K4PI = K4 + I
        TEMP = A(K3PI)
        A(K3PI) = A(K4PI)
        A(K4PI) = (X(I)-A(JP1))*A(K3PI) - A(K1PJ)*TEMP
 19     W11 = W11 + W(I)*A(K4PI)**2
C
C GET NEW ORTHOGONAL POLYNOMIAL COEFFICIENT USING PARTIAL DOUBLE
C PRECISION
C
      TEMD1 = 0.0D0
      DO 20 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = W(I)*((Y(I)-R(I))-A(K5PI))*A(K4PI)
 20     TEMD1 = TEMD1 + TEMD2
      TEMD1 = TEMD1/W11
      A(K2PJ+1) = TEMD1
C
C UPDATE POLYNOMIAL EVALUATIONS AT EACH OF THE DATA POINTS, AND
C ACCUMULATE SUM OF SQUARES OF ERRORS.  THE POLYNOMIAL EVALUATIONS ARE
C COMPUTED AND STORED IN EXTENDED PRECISION.  FOR THE I-TH DATA POINT,
C THE MOST SIGNIFICANT BITS ARE STORED IN  R(I) , AND THE LEAST
C SIGNIFICANT BITS ARE IN  A(K5PI) .
C
      SIGJ = 0.0D0
      DO 21 I = 1,M
        K4PI = K4 + I
        K5PI = K5 + I
        TEMD2 = R(I) + A(K5PI) + TEMD1*A(K4PI)
        R(I) = TEMD2
        A(K5PI) = TEMD2 - R(I)
 21     SIGJ = SIGJ + W(I)*((Y(I)-R(I)) - A(K5PI))**2
C
C SEE IF DEGREE SELECTION CRITERION HAS BEEN SATISFIED OR IF DEGREE
C MAXDEG  HAS BEEN REACHED
C
      IF (EPS) 23,26,27
C
C COMPUTE F STATISTICS  (INPUT EPS .LT. 0.)
C
 23   IF (SIGJ .EQ. 0.0D0) GO TO 29
      DEGF = M - J - 1
      DEN = (CO(4,KSIG)*DEGF + 1.0D0)*DEGF
      FCRIT = (((CO(3,KSIG)*DEGF) + CO(2,KSIG))*DEGF + CO(1,KSIG))/DEN
      FCRIT = FCRIT*FCRIT
      F = (SIGJM1 - SIGJ)*DEGF/SIGJ
      IF (F .LT. FCRIT) GO TO 25
C
C POLYNOMIAL OF DEGREE J SATISFIES F TEST
C
 24   SIGPAS = SIGJ
      JPAS = J
      NFAIL = 0
      IF (MAXDEG .EQ. J) GO TO 32
      GO TO 16
C
C POLYNOMIAL OF DEGREE J FAILS F TEST.  IF THERE HAVE BEEN THREE
C SUCCESSIVE FAILURES, A STATISTICALLY BEST DEGREE HAS BEEN FOUND.
C
 25   NFAIL = NFAIL + 1
      IF (NFAIL .GE. 3) GO TO 29
      IF (MAXDEG .EQ. J) GO TO 32
      GO TO 16
C
C RAISE THE DEGREE IF DEGREE  MAXDEG  HAS NOT YET BEEN REACHED  (INPUT
C EPS = 0.)
C
 26   IF (MAXDEG .EQ. J) GO TO 28
      GO TO 16
C
C SEE IF RMS ERROR CRITERION IS SATISFIED  (INPUT EPS .GT. 0.)
C
 27   IF (SIGJ .LE. ETST) GO TO 28
      IF (MAXDEG .EQ. J) GO TO 31
      GO TO 16
C
C RETURNS
C
 28   IERR = 1
      NDEG = J
      SIG = SIGJ
      GO TO 33
 29   IERR = 1
      NDEG = JPAS
      SIG = SIGPAS
      GO TO 33
 30   IERR = 2
      CALL XERMSG ('SLATEC', 'DPOLFT', 'INVALID INPUT PARAMETER.', 2,
     +   1)
      GO TO 37
 31   IERR = 3
      NDEG = MAXDEG
      SIG = SIGJ
      GO TO 33
 32   IERR = 4
      NDEG = JPAS
      SIG = SIGPAS
C
 33   A(K3) = NDEG
C
C WHEN STATISTICAL TEST HAS BEEN USED, EVALUATE THE BEST POLYNOMIAL AT
C ALL THE DATA POINTS IF  R  DOES NOT ALREADY CONTAIN THESE VALUES
C
      IF(EPS .GE. 0.0  .OR.  NDEG .EQ. MAXDEG) GO TO 36
      NDER = 0
      DO 35 I = 1,M
        CALL DP1VLU (NDEG,NDER,X(I),R(I),YP,A)
 35     CONTINUE
 36   EPS = SQRT(SIG/XM)
 37   RETURN
      END



      SUBROUTINE DP1VLU (L, NDER, X, YFIT, YP, A)
C***BEGIN PROLOGUE  DP1VLU
C***PURPOSE  Use the coefficients generated by DPOLFT to evaluate the
C            polynomial fit of degree L, along with the first NDER of
C            its derivatives, at a specified point.
C***LIBRARY   SLATEC
C***CATEGORY  K6
C***TYPE      DOUBLE PRECISION (PVALUE-S, DP1VLU-D)
C***KEYWORDS  CURVE FITTING, LEAST SQUARES, POLYNOMIAL APPROXIMATION
C***AUTHOR  Shampine, L. F., (SNLA)
C           Davenport, S. M., (SNLA)
C***DESCRIPTION
C
C     Abstract
C
C     The subroutine  DP1VLU  uses the coefficients generated by  DPOLFT
C     to evaluate the polynomial fit of degree  L , along with the first
C     NDER  of its derivatives, at a specified point.  Computationally
C     stable recurrence relations are used to perform this task.
C
C     The parameters for  DP1VLU  are
C
C     Input -- ALL TYPE REAL variables are DOUBLE PRECISION
C         L -      the degree of polynomial to be evaluated.  L  may be
C                  any non-negative integer which is less than or equal
C                  to  NDEG , the highest degree polynomial provided
C                  by  DPOLFT .
C         NDER -   the number of derivatives to be evaluated.  NDER
C                  may be 0 or any positive value.  If NDER is less
C                  than 0, it will be treated as 0.
C         X -      the argument at which the polynomial and its
C                  derivatives are to be evaluated.
C         A -      work and output array containing values from last
C                  call to  DPOLFT .
C
C     Output -- ALL TYPE REAL variables are DOUBLE PRECISION
C         YFIT -   value of the fitting polynomial of degree  L  at  X
C         YP -     array containing the first through  NDER  derivatives
C                  of the polynomial of degree  L .  YP  must be
C                  dimensioned at least  NDER  in the calling program.
C
C***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
C                 Curve fitting by polynomials in one variable, Report
C                 SLA-74-0270, Sandia Laboratories, June 1974.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   740601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DP1VLU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER I,IC,ILO,IN,INP1,IUP,K1,K1I,K2,K3,K3P1,K3PN,K4,K4P1,K4PN,
     * KC,L,LM1,LP1,MAXORD,N,NDER,NDO,NDP1,NORD
      DOUBLE PRECISION A(*),CC,DIF,VAL,X,YFIT,YP(*)
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  DP1VLU
      IF (L .LT. 0) GO TO 12
      NDO = MAX(NDER,0)
      NDO = MIN(NDO,L)
      MAXORD = A(1) + 0.5D0
      K1 = MAXORD + 1
      K2 = K1 + MAXORD
      K3 = K2 + MAXORD + 2
      NORD = A(K3) + 0.5D0
      IF (L .GT. NORD) GO TO 11
      K4 = K3 + L + 1
      IF (NDER .LT. 1) GO TO 2
      DO 1 I = 1,NDER
 1      YP(I) = 0.0D0
 2    IF (L .GE. 2) GO TO 4
      IF (L .EQ. 1) GO TO 3
C
C L IS 0
C
      VAL = A(K2+1)
      GO TO 10
C
C L IS 1
C
 3    CC = A(K2+2)
      VAL = A(K2+1) + (X-A(2))*CC
      IF (NDER .GE. 1) YP(1) = CC
      GO TO 10
C
C L IS GREATER THAN 1
C
 4    NDP1 = NDO + 1
      K3P1 = K3 + 1
      K4P1 = K4 + 1
      LP1 = L + 1
      LM1 = L - 1
      ILO = K3 + 3
      IUP = K4 + NDP1
      DO 5 I = ILO,IUP
 5      A(I) = 0.0D0
      DIF = X - A(LP1)
      KC = K2 + LP1
      A(K4P1) = A(KC)
      A(K3P1) = A(KC-1) + DIF*A(K4P1)
      A(K3+2) = A(K4P1)
C
C EVALUATE RECURRENCE RELATIONS FOR FUNCTION VALUE AND DERIVATIVES
C
      DO 9 I = 1,LM1
        IN = L - I
        INP1 = IN + 1
        K1I = K1 + INP1
        IC = K2 + IN
        DIF = X - A(INP1)
        VAL = A(IC) + DIF*A(K3P1) - A(K1I)*A(K4P1)
        IF (NDO .LE. 0) GO TO 8
        DO 6 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
 6        YP(N) = DIF*A(K3PN) + N*A(K3PN-1) - A(K1I)*A(K4PN)
C
C SAVE VALUES NEEDED FOR NEXT EVALUATION OF RECURRENCE RELATIONS
C
        DO 7 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
          A(K4PN) = A(K3PN)
 7        A(K3PN) = YP(N)
 8      A(K4P1) = A(K3P1)
 9      A(K3P1) = VAL
C
C NORMAL RETURN OR ABORT DUE TO ERROR
C
 10   YFIT = VAL
      RETURN
C
   11 WRITE (XERN1, '(I8)') L
      WRITE (XERN2, '(I8)') NORD
      CALL XERMSG ('SLATEC', 'DP1VLU',
     *   'THE ORDER OF POLYNOMIAL EVALUATION, L = ' // XERN1 //
     *   ' REQUESTED EXCEEDS THE HIGHEST ORDER FIT, NORD = ' // XERN2 //
     *   ', COMPUTED BY DPOLFT -- EXECUTION TERMINATED.', 8, 2)
      RETURN
C
   12 CALL XERMSG ('SLATEC', 'DP1VLU',
     +   'INVALID INPUT PARAMETER.  ORDER OF POLYNOMIAL EVALUATION ' //
     +   'REQUESTED IS NEGATIVE.', 2, 2)
      RETURN
      END



      SUBROUTINE DNLS1E (FCN, IOPT, M, N, X, FVEC, TOL, NPRINT, INFO,
     +   IW, WA, LWA)
C***BEGIN PROLOGUE  DNLS1E
C***PURPOSE  An easy-to-use code which minimizes the sum of the squares
C            of M nonlinear functions in N variables by a modification
C            of the Levenberg-Marquardt algorithm.
C***LIBRARY   SLATEC
C***CATEGORY  K1B1A1, K1B1A2
C***TYPE      DOUBLE PRECISION (SNLS1E-S, DNLS1E-D)
C***KEYWORDS  EASY-TO-USE, LEVENBERG-MARQUARDT, NONLINEAR DATA FITTING,
C             NONLINEAR LEAST SQUARES
C***AUTHOR  Hiebert, K. L., (SNLA)
C***DESCRIPTION
C
C 1. Purpose.
C
C       The purpose of DNLS1E is to minimize the sum of the squares of M
C       nonlinear functions in N variables by a modification of the
C       Levenberg-Marquardt algorithm.  This is done by using the more
C       general least-squares solver DNLS1.  The user must provide a
C       subroutine which calculates the functions.  The user has the
C       option of how the Jacobian will be supplied.  The user can
C       supply the full Jacobian, or the rows of the Jacobian (to avoid
C       storing the full Jacobian), or let the code approximate the
C       Jacobian by forward-differencing.  This code is the combination
C       of the MINPACK codes (Argonne) LMDER1, LMDIF1, and LMSTR1.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE DNLS1E(FCN,IOPT,M,N,X,FVEC,TOL,NPRINT,
C      *                  INFO,IW,WA,LWA)
C       INTEGER IOPT,M,N,NPRINT,INFO,LWAC,IW(N)
C       DOUBLE PRECISION TOL,X(N),FVEC(M),WA(LWA)
C       EXTERNAL FCN
C
C
C 3. Parameters. ALL TYPE REAL parameters are DOUBLE PRECISION
C
C       Parameters designated as input parameters must be specified on
C       entry to DNLS1E and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from DNLS1E.
C
C      FCN is the name of the user-supplied subroutine which calculates
C         the functions.  If the user wants to supply the Jacobian
C         (IOPT=2 or 3), then FCN must be written to calculate the
C         Jacobian, as well as the functions.  See the explanation
C         of the IOPT argument below.
C         If the user wants the iterates printed (NPRINT positive), then
C         FCN must do the printing.  See the explanation of NPRINT
C         below.  FCN must be declared in an EXTERNAL statement in the
C         calling program and should be written as follows.
C
C
C         SUBROUTINE FCN(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
C         INTEGER IFLAG,LDFJAC,M,N
C         DOUBLE PRECISION X(N),FVEC(M)
C         ----------
C         FJAC and LDFJAC may be ignored       , if IOPT=1.
C         DOUBLE PRECISION FJAC(LDFJAC,N)      , if IOPT=2.
C         DOUBLE PRECISION FJAC(N)             , if IOPT=3.
C         ----------
C           If IFLAG=0, the values in X and FVEC are available
C           for printing.  See the explanation of NPRINT below.
C           IFLAG will never be zero unless NPRINT is positive.
C           The values of X and FVEC must not be changed.
C         RETURN
C         ----------
C           If IFLAG=1, calculate the functions at X and return
C           this vector in FVEC.
C         RETURN
C         ----------
C           If IFLAG=2, calculate the full Jacobian at X and return
C           this matrix in FJAC.  Note that IFLAG will never be 2 unless
C           IOPT=2.  FVEC contains the function values at X and must
C           not be altered.  FJAC(I,J) must be set to the derivative
C           of FVEC(I) with respect to X(J).
C         RETURN
C         ----------
C           If IFLAG=3, calculate the LDFJAC-th row of the Jacobian
C           and return this vector in FJAC.  Note that IFLAG will
C           never be 3 unless IOPT=3.  FVEC contains the function
C           values at X and must not be altered.  FJAC(J) must be
C           set to the derivative of FVEC(LDFJAC) with respect to X(J).
C         RETURN
C         ----------
C         END
C
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of DNLS1E.  In this case,
C         set IFLAG to a negative integer.
C
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=2 or 3, then the user must supply the
C         Jacobian, as well as the function values, through the
C         subroutine FCN.  If IOPT=2, the user supplies the full
C         Jacobian with one call to FCN.  If IOPT=3, the user supplies
C         one row of the Jacobian with each call.  (In this manner,
C         storage can be saved because the full Jacobian is not stored.)
C         If IOPT=1, the code will approximate the Jacobian by forward
C         differencing.
C
C       M is a positive integer input variable set to the number of
C         functions.
C
C       N is a positive integer input variable set to the number of
C         variables.  N must not exceed M.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length M which contains the functions
C         evaluated at the output X.
C
C       TOL is a non-negative input variable.  Termination occurs when
C         the algorithm estimates either that the relative error in the
C         sum of squares is at most TOL or that the relative error
C         between X and the solution is at most TOL.  Section 4 contains
C         more details about TOL.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iterations thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN (see example) and
C         FVEC should not be altered.  If NPRINT is not positive, no
C         special calls of FCN with IFLAG = 0 are made.
C
C       INFO is an integer output variable.  If the user has terminated
C        execution, INFO is set to the (negative) value of IFLAG.  See
C        description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters.
C
C         INFO = 1  algorithm estimates that the relative error in the
C                   sum of squares is at most TOL.
C
C         INFO = 2  algorithm estimates that the relative error between
C                   X and the solution is at most TOL.
C
C         INFO = 3  conditions for INFO = 1 and INFO = 2 both hold.
C
C         INFO = 4  FVEC is orthogonal to the columns of the Jacobian to
C                   machine precision.
C
C         INFO = 5  number of calls to FCN has reached 100*(N+1)
C                   for IOPT=2 or 3 or 200*(N+1) for IOPT=1.
C
C         INFO = 6  TOL is too small.  No further reduction in the sum
C                   of squares is possible.
C
C         INFO = 7  TOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       IW is an INTEGER work array of length N.
C
C       WA is a work array of length LWA.
C
C       LWA is a positive integer input variable not less than
C         N*(M+5)+M for IOPT=1 and 2 or N*(N+5)+M for IOPT=3.
C
C
C 4. Successful Completion.
C
C       The accuracy of DNLS1E is controlled by the convergence parame-
C       ter TOL.  This parameter is used in tests which make three types
C       of comparisons between the approximation X and a solution XSOL.
C       DNLS1E terminates when any of the tests is satisfied.  If TOL is
C       less than the machine precision (as defined by the function
C       R1MACH(4)), then DNLS1E only attempts to satisfy the test
C       defined by the machine precision.  Further progress is not usu-
C       ally possible.  Unless high precision solutions are required,
C       the recommended value for TOL is the square root of the machine
C       precision.
C
C       The tests assume that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian are coded consistently.  If these conditions
C       are not satisfied, then DNLS1E may incorrectly indicate conver-
C       gence.  If the Jacobian is coded correctly or IOPT=1,
C       then the validity of the answer can be checked, for example, by
C       rerunning DNLS1E with tighter tolerances.
C
C       First Convergence Test.  If ENORM(Z) denotes the Euclidean norm
C         of a vector Z, then this test attempts to guarantee that
C
C               ENORM(FVEC) .LE. (1+TOL)*ENORM(FVECS),
C
C         where FVECS denotes the functions evaluated at XSOL.  If this
C         condition is satisfied with TOL = 10**(-K), then the final
C         residual norm ENORM(FVEC) has K significant decimal digits and
C         INFO is set to 1 (or to 3 if the second test is also satis-
C         fied).
C
C       Second Convergence Test.  If D is a diagonal matrix (implicitly
C         generated by DNLS1E) whose entries contain scale factors for
C         the variables, then this test attempts to guarantee that
C
C               ENORM(D*(X-XSOL)) .LE.  TOL*ENORM(D*XSOL).
C
C         If this condition is satisfied with TOL = 10**(-K), then the
C         larger components of D*X have K significant decimal digits and
C         INFO is set to 2 (or to 3 if the first test is also satis-
C         fied).  There is a danger that the smaller components of D*X
C         may have large relative errors, but the choice of D is such
C         that the accuracy of the components of X is usually related to
C         their sensitivity.
C
C       Third Convergence Test.  This test is satisfied when FVEC is
C         orthogonal to the columns of the Jacobian to machine preci-
C         sion.  There is no clear relationship between this test and
C         the accuracy of DNLS1E, and furthermore, the test is equally
C         well satisfied at other critical points, namely maximizers and
C         saddle points.  Therefore, termination caused by this test
C         (INFO = 4) should be examined carefully.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of DNLS1E can be due to improper input
C       parameters, arithmetic interrupts, or an excessive number of
C       function evaluations.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1
C         or IOPT .GT. 3, or N .LE. 0, or M .LT. N, or TOL .LT. 0.E0,
C         or for IOPT=1 or 2 LWA .LT. N*(M+5)+M, or for IOPT=3
C         LWA .LT. N*(N+5)+M.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by DNLS1E.  In this
C         case, it may be possible to remedy the situation by not evalu-
C         ating the functions here, but instead setting the components
C         of FVEC to numbers that exceed those in the initial FVEC.
C
C       Excessive Number of Function Evaluations.  If the number of
C         calls to FCN reaches 100*(N+1) for IOPT=2 or 3 or 200*(N+1)
C         for IOPT=1, then this indicates that the routine is converging
C         very slowly as measured by the progress of FVEC, and INFO is
C         set to 5.  In this case, it may be helpful to restart DNLS1E,
C         thereby forcing it to disregard old (and possibly harmful)
C         information.
C
C
C 6. Characteristics of the Algorithm.
C
C       DNLS1E is a modification of the Levenberg-Marquardt algorithm.
C       Two of its main characteristics involve the proper use of
C       implicitly scaled variables and an optimal choice for the cor-
C       rection.  The use of implicitly scaled variables achieves scale
C       invariance of DNLS1E and limits the size of the correction in
C       any direction where the functions are changing rapidly.  The
C       optimal choice of the correction guarantees (under reasonable
C       conditions) global convergence from starting points far from the
C       solution and a fast rate of convergence for problems with small
C       residuals.
C
C       Timing.  The time required by DNLS1E to solve a given problem
C         depends on M and N, the behavior of the functions, the accu-
C         racy requested, and the starting point.  The number of arith-
C         metic operations needed by DNLS1E is about N**3 to process
C         each evaluation of the functions (call to FCN) and to process
C         each evaluation of the Jacobian DNLS1E takes M*N**2 for IOPT=2
C         (one call to JAC), M*N**2 for IOPT=1 (N calls to FCN) and
C         1.5*M*N**2 for IOPT=3 (M calls to FCN).  Unless FCN
C         can be evaluated quickly, the timing of DNLS1E will be
C         strongly influenced by the time spent in FCN.
C
C       Storage.  DNLS1E requires (M*N + 2*M + 6*N) for IOPT=1 or 2 and
C         (N**2 + 2*M + 6*N) for IOPT=3 single precision storage
C         locations and N integer storage locations, in addition to
C         the storage required by the program.  There are no internally
C         declared storage arrays.
C
C *Long Description:
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), and X(3)
C       which provide the best fit (in the least squares sense) of
C
C             X(1) + U(I)/(V(I)*X(2) + W(I)*X(3)),  I = 1, 15
C
C       to the data
C
C             Y = (0.14,0.18,0.22,0.25,0.29,0.32,0.35,0.39,
C                  0.37,0.58,0.73,0.96,1.34,2.10,4.39),
C
C       where U(I) = I, V(I) = 16 - I, and W(I) = MIN(U(I),V(I)).  The
C       I-th component of FVEC is thus defined by
C
C             Y(I) - (X(1) + U(I)/(V(I)*X(2) + W(I)*X(3))).
C
C       **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for DNLS1E example.
C C
C       INTEGER I,IOPT,M,N,NPRINT,JNFO,LWA,NWRITE
C       INTEGER IW(3)
C       DOUBLE PRECISION TOL,FNORM,X(3),FVEC(15),WA(75)
C       DOUBLE PRECISION DENORM,D1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 1
C       M = 15
C       N = 3
C C
C C     The following starting values provide a rough fit.
C C
C       X(1) = 1.E0
C       X(2) = 1.E0
C       X(3) = 1.E0
C C
C       LWA = 75
C       NPRINT = 0
C C
C C     Set TOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       TOL = SQRT(R1MACH(4))
C C
C       CALL DNLS1E(FCN,IOPT,M,N,X,FVEC,TOL,NPRINT,
C      *            INFO,IW,WA,LWA)
C       FNORM = ENORM(M,FVEC)
C       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' EXIT
C      *        5X,' FINAL APPROXIMATE SOLUTION' // 5X,3E15.7)
C       END
C       SUBROUTINE FCN(IFLAG,M,N,X,FVEC,DUM,IDUM)
C C     This is the form of the FCN routine if IOPT=1,
C C     that is, if the user does not calculate the Jacobian.
C       INTEGER I,M,N,IFLAG
C       DOUBLE PRECISION X(N),FVEC(M),Y(15)
C       DOUBLE PRECISION TMP1,TMP2,TMP3,TMP4
C       DATA Y(1),Y(2),Y(3),Y(4),Y(5),Y(6),Y(7),Y(8),
C      *     Y(9),Y(10),Y(11),Y(12),Y(13),Y(14),Y(15)
C      *     /1.4E-1,1.8E-1,2.2E-1,2.5E-1,2.9E-1,3.2E-1,3.5E-1,3.9E-1,
C      *      3.7E-1,5.8E-1,7.3E-1,9.6E-1,1.34E0,2.1E0,4.39E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     Insert print statements here when NPRINT is positive.
C C
C       RETURN
C     5 CONTINUE
C       DO 10 I = 1, M
C          TMP1 = I
C          TMP2 = 16 - I
C          TMP3 = TMP1
C          IF (I .GT. 8) TMP3 = TMP2
C          FVEC(I) = Y(I) - (X(1) + TMP1/(X(2)*TMP2 + X(3)*TMP3))
C    10    CONTINUE
C       RETURN
C       END
C
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.9063596E-01
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C        0.8241058E-01  0.1133037E+01  0.2343695E+01
C
C
C       For IOPT=2, FCN would be modified as follows to also
C       calculate the full Jacobian when IFLAG=2.
C
C       SUBROUTINE FCN(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
C C
C C     This is the form of the FCN routine if IOPT=2,
C C     that is, if the user calculates the full Jacobian.
C C
C       INTEGER I,LDFJAC,M,N,IFLAG
C       DOUBLE PRECISION X(N),FVEC(M),FJAC(LDFJAC,N),Y(15)
C       DOUBLE PRECISION TMP1,TMP2,TMP3,TMP4
C       DATA Y(1),Y(2),Y(3),Y(4),Y(5),Y(6),Y(7),Y(8),
C      *     Y(9),Y(10),Y(11),Y(12),Y(13),Y(14),Y(15)
C      *     /1.4E-1,1.8E-1,2.2E-1,2.5E-1,2.9E-1,3.2E-1,3.5E-1,3.9E-1,
C      *      3.7E-1,5.8E-1,7.3E-1,9.6E-1,1.34E0,2.1E0,4.39E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     Insert print statements here when NPRINT is positive.
C C
C       RETURN
C     5 CONTINUE
C       IF(IFLAG.NE.1) GO TO 20
C       DO 10 I = 1, M
C          TMP1 = I
C          TMP2 = 16 - I
C          TMP3 = TMP1
C          IF (I .GT. 8) TMP3 = TMP2
C          FVEC(I) = Y(I) - (X(1) + TMP1/(X(2)*TMP2 + X(3)*TMP3))
C    10    CONTINUE
C       RETURN
C C
C C     Below, calculate the full Jacobian.
C C
C    20    CONTINUE
C C
C       DO 30 I = 1, M
C          TMP1 = I
C          TMP2 = 16 - I
C          TMP3 = TMP1
C          IF (I .GT. 8) TMP3 = TMP2
C          TMP4 = (X(2)*TMP2 + X(3)*TMP3)**2
C          FJAC(I,1) = -1.E0
C          FJAC(I,2) = TMP1*TMP2/TMP4
C          FJAC(I,3) = TMP1*TMP3/TMP4
C    30    CONTINUE
C       RETURN
C       END
C
C
C       For IOPT = 3, FJAC would be dimensioned as FJAC(3,3),
C         LDFJAC would be set to 3, and FCN would be written as
C         follows to calculate a row of the Jacobian when IFLAG=3.
C
C       SUBROUTINE FCN(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
C C     This is the form of the FCN routine if IOPT=3,
C C     that is, if the user calculates the Jacobian row by row.
C       INTEGER I,M,N,IFLAG
C       DOUBLE PRECISION X(N),FVEC(M),FJAC(N),Y(15)
C       DOUBLE PRECISION TMP1,TMP2,TMP3,TMP4
C       DATA Y(1),Y(2),Y(3),Y(4),Y(5),Y(6),Y(7),Y(8),
C      *     Y(9),Y(10),Y(11),Y(12),Y(13),Y(14),Y(15)
C      *     /1.4E-1,1.8E-1,2.2E-1,2.5E-1,2.9E-1,3.2E-1,3.5E-1,3.9E-1,
C      *      3.7E-1,5.8E-1,7.3E-1,9.6E-1,1.34E0,2.1E0,4.39E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     Insert print statements here when NPRINT is positive.
C C
C       RETURN
C     5 CONTINUE
C       IF( IFLAG.NE.1) GO TO 20
C       DO 10 I = 1, M
C          TMP1 = I
C          TMP2 = 16 - I
C          TMP3 = TMP1
C          IF (I .GT. 8) TMP3 = TMP2
C          FVEC(I) = Y(I) - (X(1) + TMP1/(X(2)*TMP2 + X(3)*TMP3))
C    10    CONTINUE
C       RETURN
C C
C C     Below, calculate the LDFJAC-th row of the Jacobian.
C C
C    20 CONTINUE
C
C       I = LDFJAC
C          TMP1 = I
C          TMP2 = 16 - I
C          TMP3 = TMP1
C          IF (I .GT. 8) TMP3 = TMP2
C          TMP4 = (X(2)*TMP2 + X(3)*TMP3)**2
C          FJAC(1) = -1.E0
C          FJAC(2) = TMP1*TMP2/TMP4
C          FJAC(3) = TMP1*TMP3/TMP4
C       RETURN
C       END
C
C***REFERENCES  Jorge J. More, The Levenberg-Marquardt algorithm:
C                 implementation and theory.  In Numerical Analysis
C                 Proceedings (Dundee, June 28 - July 1, 1977, G. A.
C                 Watson, Editor), Lecture Notes in Mathematics 630,
C                 Springer-Verlag, 1978.
C***ROUTINES CALLED  DNLS1, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DNLS1E
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER M,N,NPRINT,INFO,LWA,IOPT
      INTEGER INDEX,IW(*)
      DOUBLE PRECISION TOL
      DOUBLE PRECISION X(*),FVEC(*),WA(*)
      EXTERNAL FCN
      INTEGER MAXFEV,MODE,NFEV,NJEV
      DOUBLE PRECISION FACTOR,FTOL,GTOL,XTOL,ZERO,EPSFCN
      SAVE FACTOR, ZERO
      DATA FACTOR,ZERO /1.0D2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DNLS1E
      INFO = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 3 .OR.
     1    N .LE. 0 .OR. M .LT. N .OR. TOL .LT. ZERO
     2    .OR. LWA .LT. N*(N+5) + M) GO TO 10
      IF (IOPT .LT. 3 .AND. LWA .LT. N*(M+5) + M) GO TO 10
C
C     CALL DNLS1.
C
      MAXFEV = 100*(N + 1)
      IF (IOPT .EQ. 1) MAXFEV = 2*MAXFEV
      FTOL = TOL
      XTOL = TOL
      GTOL = ZERO
      EPSFCN = ZERO
      MODE = 1
      INDEX = 5*N+M
      CALL DNLS1(FCN,IOPT,M,N,X,FVEC,WA(INDEX+1),M,FTOL,XTOL,GTOL,
     1           MAXFEV,EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2           IW,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),WA(5*N+1))
      IF (INFO .EQ. 8) INFO = 4
   10 CONTINUE
      IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNLS1E',
     +   'INVALID INPUT PARAMETER.', 2, 1)
      RETURN
C
C     LAST CARD OF SUBROUTINE DNLS1E.
C
      END



      SUBROUTINE DNLS1 (FCN, IOPT, M, N, X, FVEC, FJAC, LDFJAC, FTOL,
     +   XTOL, GTOL, MAXFEV, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO,
     +   NFEV, NJEV, IPVT, QTF, WA1, WA2, WA3, WA4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IOPT,M,N,LDFJAC,MAXFEV,MODE,NPRINT,INFO,NFEV,NJEV
      INTEGER IJUNK,NROW,IPVT(*)
      DOUBLE PRECISION FTOL,XTOL,GTOL,FACTOR,EPSFCN
      DOUBLE PRECISION X(*),FVEC(*),FJAC(LDFJAC,*),DIAG(*),QTF(*),
     1     WA1(*),WA2(*),WA3(*),WA4(*)
      LOGICAL SING
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,L,MODECH
      DOUBLE PRECISION ACTRED,DELTA,DIRDER,EPSMCH,FNORM,FNORM1,GNORM,
     1     ONE,PAR,PNORM,PRERED,P1,P5,P25,P75,P0001,RATIO,SUM,TEMP,
     2     TEMP1,TEMP2,XNORM,ZERO
      DOUBLE PRECISION D1MACH,DENORM,ERR(1),CHKLIM
      CHARACTER*8 XERN1
      CHARACTER*16 XERN3
      SAVE CHKLIM, ONE, P1, P5, P25, P75, P0001, ZERO
C
      DATA CHKLIM/.1D0/
      DATA ONE,P1,P5,P25,P75,P0001,ZERO
     1     /1.0D0,1.0D-1,5.0D-1,2.5D-1,7.5D-1,1.0D-4,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DNLS1
      EPSMCH = D1MACH(4)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 3 .OR. N .LE. 0 .OR.
     1    M .LT. N .OR. LDFJAC .LT. N .OR. FTOL .LT. ZERO
     2    .OR. XTOL .LT. ZERO .OR. GTOL .LT. ZERO
     3    .OR. MAXFEV .LE. 0 .OR. FACTOR .LE. ZERO) GO TO 300
      IF (IOPT .LT. 3 .AND. LDFJAC .LT. M) GO TO 300
      IF (MODE .NE. 2) GO TO 20
      DO 10 J = 1, N
         IF (DIAG(J) .LE. ZERO) GO TO 300
   10    CONTINUE
   20 CONTINUE
C
C     EVALUATE THE FUNCTION AT THE STARTING POINT
C     AND CALCULATE ITS NORM.
C
      IFLAG = 1
      IJUNK = 1
      CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
      NFEV = 1
      IF (IFLAG .LT. 0) GO TO 300
      FNORM = DENORM(M,FVEC)
C
C     INITIALIZE LEVENBERG-MARQUARDT PARAMETER AND ITERATION COUNTER.
C
      PAR = ZERO
      ITER = 1
C
C     BEGINNING OF THE OUTER LOOP.
C
   30 CONTINUE
C
C        IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
         IF (NPRINT .LE. 0) GO TO 40
         IFLAG = 0
         IF (MOD(ITER-1,NPRINT) .EQ. 0)
     1      CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
         IF (IFLAG .LT. 0) GO TO 300
   40    CONTINUE
C
C        CALCULATE THE JACOBIAN MATRIX.
C
      IF (IOPT .EQ. 3) GO TO 475
C
C     STORE THE FULL JACOBIAN USING M*N STORAGE
C
      IF (IOPT .EQ. 1) GO TO 410
C
C     THE USER SUPPLIES THE JACOBIAN
C
         IFLAG = 2
         CALL FCN(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
         NJEV = NJEV + 1
C
C             ON THE FIRST ITERATION, CHECK THE USER SUPPLIED JACOBIAN
C
         IF (ITER .LE. 1) THEN
            IF (IFLAG .LT. 0) GO TO 300
C
C           GET THE INCREMENTED X-VALUES INTO WA1(*).
C
            MODECH = 1
            CALL DCKDER(M,N,X,FVEC,FJAC,LDFJAC,WA1,WA4,MODECH,ERR)
C
C           EVALUATE FUNCTION AT INCREMENTED VALUE AND PUT IN WA4(*).
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA1,WA4,FJAC,LDFJAC)
            NFEV = NFEV + 1
            IF(IFLAG .LT. 0) GO TO 300
            DO 350 I = 1, M
               MODECH = 2
               CALL DCKDER(1,N,X,FVEC(I),FJAC(I,1),LDFJAC,WA1,
     1              WA4(I),MODECH,ERR)
               IF (ERR(1) .LT. CHKLIM) THEN
                  WRITE (XERN1, '(I8)') I
                  WRITE (XERN3, '(1PE15.6)') ERR(1)
                  CALL XERMSG ('SLATEC', 'DNLS1', 'DERIVATIVE OF ' //
     *               'FUNCTION ' // XERN1 // ' MAY BE WRONG, ERR = ' //
     *               XERN3 // ' TOO CLOSE TO 0.', 7, 0)
               ENDIF
  350       CONTINUE
         ENDIF
C
         GO TO 420
C
C     THE CODE APPROXIMATES THE JACOBIAN
C
410      IFLAG = 1
         CALL DFDJC3(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN,WA4)
         NFEV = NFEV + N
  420    IF (IFLAG .LT. 0) GO TO 300
C
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
         CALL DQRFAC(M,N,FJAC,LDFJAC,.TRUE.,IPVT,N,WA1,WA2,WA3)
C
C        FORM (Q TRANSPOSE)*FVEC AND STORE THE FIRST N COMPONENTS IN
C        QTF.
C
         DO 430 I = 1, M
            WA4(I) = FVEC(I)
  430         CONTINUE
         DO 470 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 460
            SUM = ZERO
            DO 440 I = J, M
               SUM = SUM + FJAC(I,J)*WA4(I)
  440          CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 450 I = J, M
               WA4(I) = WA4(I) + FJAC(I,J)*TEMP
  450          CONTINUE
  460       CONTINUE
            FJAC(J,J) = WA1(J)
            QTF(J) = WA4(J)
  470       CONTINUE
         GO TO 560
C
C        ACCUMULATE THE JACOBIAN BY ROWS IN ORDER TO SAVE STORAGE.
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN MATRIX
C        CALCULATED ONE ROW AT A TIME, WHILE SIMULTANEOUSLY
C        FORMING (Q TRANSPOSE)*FVEC AND STORING THE FIRST
C        N COMPONENTS IN QTF.
C
  475    DO 490 J = 1, N
            QTF(J) = ZERO
            DO 480 I = 1, N
               FJAC(I,J) = ZERO
  480          CONTINUE
  490        CONTINUE
         DO 500 I = 1, M
            NROW = I
            IFLAG = 3
            CALL FCN(IFLAG,M,N,X,FVEC,WA3,NROW)
            IF (IFLAG .LT. 0) GO TO 300
C
C            ON THE FIRST ITERATION, CHECK THE USER SUPPLIED JACOBIAN.
C
            IF(ITER .GT. 1) GO TO 498
C
C            GET THE INCREMENTED X-VALUES INTO WA1(*).
C
            MODECH = 1
            CALL DCKDER(M,N,X,FVEC,FJAC,LDFJAC,WA1,WA4,MODECH,ERR)
C
C            EVALUATE AT INCREMENTED VALUES, IF NOT ALREADY EVALUATED.
C
            IF(I .NE. 1) GO TO 495
C
C            EVALUATE FUNCTION AT INCREMENTED VALUE AND PUT INTO WA4(*).
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA1,WA4,FJAC,NROW)
            NFEV = NFEV + 1
            IF(IFLAG .LT. 0) GO TO 300
495         CONTINUE
            MODECH = 2
            CALL DCKDER(1,N,X,FVEC(I),WA3,1,WA1,WA4(I),MODECH,ERR)
            IF (ERR(1) .LT. CHKLIM) THEN
               WRITE (XERN1, '(I8)') I
               WRITE (XERN3, '(1PE15.6)') ERR(1)
               CALL XERMSG ('SLATEC', 'DNLS1', 'DERIVATIVE OF FUNCTION '
     *            // XERN1 // ' MAY BE WRONG, ERR = ' // XERN3 //
     *            ' TOO CLOSE TO 0.', 7, 0)
            ENDIF
498         CONTINUE
C
            TEMP = FVEC(I)
            CALL DWUPDT(N,FJAC,LDFJAC,WA3,QTF,TEMP,WA1,WA2)
  500       CONTINUE
         NJEV = NJEV + 1
C
C        IF THE JACOBIAN IS RANK DEFICIENT, CALL DQRFAC TO
C        REORDER ITS COLUMNS AND UPDATE THE COMPONENTS OF QTF.
C
         SING = .FALSE.
         DO 510 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) SING = .TRUE.
            IPVT(J) = J
            WA2(J) = DENORM(J,FJAC(1,J))
  510       CONTINUE
         IF (.NOT.SING) GO TO 560
         CALL DQRFAC(N,N,FJAC,LDFJAC,.TRUE.,IPVT,N,WA1,WA2,WA3)
         DO 550 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 540
            SUM = ZERO
            DO 520 I = J, N
               SUM = SUM + FJAC(I,J)*QTF(I)
  520         CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 530 I = J, N
               QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  530          CONTINUE
  540       CONTINUE
            FJAC(J,J) = WA1(J)
  550       CONTINUE
  560    CONTINUE
C
C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
         IF (ITER .NE. 1) GO TO 80
         IF (MODE .EQ. 2) GO TO 60
         DO 50 J = 1, N
            DIAG(J) = WA2(J)
            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   50       CONTINUE
   60    CONTINUE
C
C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
C        AND INITIALIZE THE STEP BOUND DELTA.
C
         DO 70 J = 1, N
            WA3(J) = DIAG(J)*X(J)
   70       CONTINUE
         XNORM = DENORM(N,WA3)
         DELTA = FACTOR*XNORM
         IF (DELTA .EQ. ZERO) DELTA = FACTOR
   80    CONTINUE
C
C        COMPUTE THE NORM OF THE SCALED GRADIENT.
C
         GNORM = ZERO
         IF (FNORM .EQ. ZERO) GO TO 170
         DO 160 J = 1, N
            L = IPVT(J)
            IF (WA2(L) .EQ. ZERO) GO TO 150
            SUM = ZERO
            DO 140 I = 1, J
               SUM = SUM + FJAC(I,J)*(QTF(I)/FNORM)
  140          CONTINUE
            GNORM = MAX(GNORM,ABS(SUM/WA2(L)))
  150       CONTINUE
  160       CONTINUE
  170    CONTINUE
C
C        TEST FOR CONVERGENCE OF THE GRADIENT NORM.
C
         IF (GNORM .LE. GTOL) INFO = 4
         IF (INFO .NE. 0) GO TO 300
C
C        RESCALE IF NECESSARY.
C
         IF (MODE .EQ. 2) GO TO 190
         DO 180 J = 1, N
            DIAG(J) = MAX(DIAG(J),WA2(J))
  180       CONTINUE
  190    CONTINUE
C
C        BEGINNING OF THE INNER LOOP.
C
  200    CONTINUE
C
C           DETERMINE THE LEVENBERG-MARQUARDT PARAMETER.
C
            CALL DMPAR(N,FJAC,LDFJAC,IPVT,DIAG,QTF,DELTA,PAR,WA1,WA2,
     1                 WA3,WA4)
C
C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
            DO 210 J = 1, N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
  210          CONTINUE
            PNORM = DENORM(N,WA3)
C
C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA2,WA4,FJAC,IJUNK)
            NFEV = NFEV + 1
            IF (IFLAG .LT. 0) GO TO 300
            FNORM1 = DENORM(M,WA4)
C
C           COMPUTE THE SCALED ACTUAL REDUCTION.
C
            ACTRED = -ONE
            IF (P1*FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C           COMPUTE THE SCALED PREDICTED REDUCTION AND
C           THE SCALED DIRECTIONAL DERIVATIVE.
C
            DO 230 J = 1, N
               WA3(J) = ZERO
               L = IPVT(J)
               TEMP = WA1(L)
               DO 220 I = 1, J
                  WA3(I) = WA3(I) + FJAC(I,J)*TEMP
  220             CONTINUE
  230          CONTINUE
            TEMP1 = DENORM(N,WA3)/FNORM
            TEMP2 = (SQRT(PAR)*PNORM)/FNORM
            PRERED = TEMP1**2 + TEMP2**2/P5
            DIRDER = -(TEMP1**2 + TEMP2**2)
C
C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C           REDUCTION.
C
            RATIO = ZERO
            IF (PRERED .NE. ZERO) RATIO = ACTRED/PRERED
C
C           UPDATE THE STEP BOUND.
C
            IF (RATIO .GT. P25) GO TO 240
               IF (ACTRED .GE. ZERO) TEMP = P5
               IF (ACTRED .LT. ZERO)
     1            TEMP = P5*DIRDER/(DIRDER + P5*ACTRED)
               IF (P1*FNORM1 .GE. FNORM .OR. TEMP .LT. P1) TEMP = P1
               DELTA = TEMP*MIN(DELTA,PNORM/P1)
               PAR = PAR/TEMP
               GO TO 260
  240       CONTINUE
               IF (PAR .NE. ZERO .AND. RATIO .LT. P75) GO TO 250
               DELTA = PNORM/P5
               PAR = P5*PAR
  250          CONTINUE
  260       CONTINUE
C
C           TEST FOR SUCCESSFUL ITERATION.
C
            IF (RATIO .LT. P0001) GO TO 290
C
C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
            DO 270 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
  270          CONTINUE
            DO 280 I = 1, M
               FVEC(I) = WA4(I)
  280          CONTINUE
            XNORM = DENORM(N,WA2)
            FNORM = FNORM1
            ITER = ITER + 1
  290       CONTINUE
C
C           TESTS FOR CONVERGENCE.
C
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     1          .AND. P5*RATIO .LE. ONE) INFO = 1
            IF (DELTA .LE. XTOL*XNORM) INFO = 2
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     1          .AND. P5*RATIO .LE. ONE .AND. INFO .EQ. 2) INFO = 3
            IF (INFO .NE. 0) GO TO 300
C
C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
            IF (NFEV .GE. MAXFEV) INFO = 5
            IF (ABS(ACTRED) .LE. EPSMCH .AND. PRERED .LE. EPSMCH
     1          .AND. P5*RATIO .LE. ONE) INFO = 6
            IF (DELTA .LE. EPSMCH*XNORM) INFO = 7
            IF (GNORM .LE. EPSMCH) INFO = 8
            IF (INFO .NE. 0) GO TO 300
C
C           END OF THE INNER LOOP. REPEAT IF ITERATION UNSUCCESSFUL.
C
            IF (RATIO .LT. P0001) GO TO 200
C
C        END OF THE OUTER LOOP.
C
         GO TO 30
  300 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
      IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
      IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'INVALID INPUT PARAMETER.', 2, 1)
      IF (INFO .EQ. 4) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'THIRD CONVERGENCE CONDITION, CHECK RESULTS BEFORE ACCEPTING.',
     +   1, 1)
      IF (INFO .EQ. 5) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
      IF (INFO .GE. 6) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'TOLERANCES TOO SMALL, NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
      RETURN
C
C     LAST CARD OF SUBROUTINE DNLS1.
C
      END



      SUBROUTINE DCKDER (M, N, X, FVEC, FJAC, LDFJAC, XP, FVECP, MODE,
     +   ERR)
      INTEGER I, J, LDFJAC, M, MODE, N
      DOUBLE PRECISION D1MACH, EPS, EPSF, EPSLOG, EPSMCH, ERR(*),
     1     FACTOR, FJAC(LDFJAC,*), FVEC(*), FVECP(*), ONE, TEMP, X(*),
     2     XP(*), ZERO
      SAVE FACTOR, ONE, ZERO
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
C
C     EPSMCH IS THE MACHINE PRECISION.
C
C***FIRST EXECUTABLE STATEMENT  DCKDER
      EPSMCH = D1MACH(4)
C
      EPS = SQRT(EPSMCH)
C
      IF (MODE .EQ. 2) GO TO 20
C
C        MODE = 1.
C
         DO 10 J = 1, N
            TEMP = EPS*ABS(X(J))
            IF (TEMP .EQ. ZERO) TEMP = EPS
            XP(J) = X(J) + TEMP
   10       CONTINUE
         GO TO 70
   20 CONTINUE
C
C        MODE = 2.
C
         EPSF = FACTOR*EPSMCH
         EPSLOG = LOG10(EPS)
         DO 30 I = 1, M
            ERR(I) = ZERO
   30       CONTINUE
         DO 50 J = 1, N
            TEMP = ABS(X(J))
            IF (TEMP .EQ. ZERO) TEMP = ONE
            DO 40 I = 1, M
               ERR(I) = ERR(I) + TEMP*FJAC(I,J)
   40          CONTINUE
   50       CONTINUE
         DO 60 I = 1, M
            TEMP = ONE
            IF (FVEC(I) .NE. ZERO .AND. FVECP(I) .NE. ZERO
     1          .AND. ABS(FVECP(I)-FVEC(I)) .GE. EPSF*ABS(FVEC(I)))
     2         TEMP = EPS*ABS((FVECP(I)-FVEC(I))/EPS-ERR(I))
     3                /(ABS(FVEC(I)) + ABS(FVECP(I)))
            ERR(I) = ONE
            IF (TEMP .GT. EPSMCH .AND. TEMP .LT. EPS)
     1         ERR(I) = (LOG10(TEMP) - EPSLOG)/EPSLOG
            IF (TEMP .GE. EPS) ERR(I) = ZERO
   60       CONTINUE
   70 CONTINUE
C
      RETURN
C
C     LAST CARD OF SUBROUTINE DCKDER.
C
      END



      DOUBLE PRECISION FUNCTION DENORM (N, X)
      INTEGER I, N
      DOUBLE PRECISION AGIANT, FLOATN, ONE, RDWARF, RGIANT, S1, S2, S3,
     1     X(*), X1MAX, X3MAX, XABS, ZERO
      SAVE ONE, ZERO, RDWARF, RGIANT
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
C***FIRST EXECUTABLE STATEMENT  DENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = ABS(X(I))
         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70
            IF (XABS .LE. RDWARF) GO TO 30
C
C              SUM FOR LARGE COMPONENTS.
C
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
C
C              SUM FOR SMALL COMPONENTS.
C
               IF (XABS .LE. X3MAX) GO TO 40
                  S3 = ONE + S3*(X3MAX/XABS)**2
                  X3MAX = XABS
                  GO TO 50
   40          CONTINUE
                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
   50          CONTINUE
   60       CONTINUE
            GO TO 80
   70    CONTINUE
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
C
C     CALCULATION OF NORM.
C
      IF (S1 .EQ. ZERO) GO TO 100
         DENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         DENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         DENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            DENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
C
C     LAST CARD OF FUNCTION DENORM.
C
      END



      SUBROUTINE DFDJC3 (FCN, M, N, X, FVEC, FJAC, LDFJAC, IFLAG,
     +   EPSFCN, WA)
      INTEGER M,N,LDFJAC,IFLAG
      DOUBLE PRECISION EPSFCN
      DOUBLE PRECISION X(*),FVEC(*),FJAC(LDFJAC,*),WA(*)
      INTEGER I,J
      DOUBLE PRECISION EPS,EPSMCH,H,TEMP,ZERO
      DOUBLE PRECISION D1MACH
      SAVE ZERO
      DATA ZERO /0.0D0/
C***FIRST EXECUTABLE STATEMENT  DFDJC3
      EPSMCH = D1MACH(4)
C
      EPS = SQRT(MAX(EPSFCN,EPSMCH))
C      SET IFLAG=1 TO INDICATE THAT FUNCTION VALUES
C           ARE TO BE RETURNED BY FCN.
      IFLAG = 1
      DO 20 J = 1, N
         TEMP = X(J)
         H = EPS*ABS(TEMP)
         IF (H .EQ. ZERO) H = EPS
         X(J) = TEMP + H
         CALL FCN(IFLAG,M,N,X,WA,FJAC,LDFJAC)
         IF (IFLAG .LT. 0) GO TO 30
         X(J) = TEMP
         DO 10 I = 1, M
            FJAC(I,J) = (WA(I) - FVEC(I))/H
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DFDJC3.
C
      END



      SUBROUTINE DMPAR (N, R, LDR, IPVT, DIAG, QTB, DELTA, PAR, X,
     +   SIGMA, WA1, WA2)
      INTEGER N,LDR
      INTEGER IPVT(*)
      DOUBLE PRECISION DELTA,PAR
      DOUBLE PRECISION R(LDR,*),DIAG(*),QTB(*),X(*),SIGMA(*),WA1(*),
     1 WA2(*)
      INTEGER I,ITER,J,JM1,JP1,K,L,NSING
      DOUBLE PRECISION DXNORM,DWARF,FP,GNORM,PARC,PARL,PARU,P1,P001,
     1 SUM,TEMP,ZERO
      DOUBLE PRECISION D1MACH,DENORM
      SAVE P1, P001, ZERO
      DATA P1,P001,ZERO /1.0D-1,1.0D-3,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DMPAR
      DWARF = D1MACH(1)
C
C     COMPUTE AND STORE IN X THE GAUSS-NEWTON DIRECTION. IF THE
C     JACOBIAN IS RANK-DEFICIENT, OBTAIN A LEAST SQUARES SOLUTION.
C
      NSING = N
      DO 10 J = 1, N
         WA1(J) = QTB(J)
         IF (R(J,J) .EQ. ZERO .AND. NSING .EQ. N) NSING = J - 1
         IF (NSING .LT. N) WA1(J) = ZERO
   10    CONTINUE
      IF (NSING .LT. 1) GO TO 50
      DO 40 K = 1, NSING
         J = NSING - K + 1
         WA1(J) = WA1(J)/R(J,J)
         TEMP = WA1(J)
         JM1 = J - 1
         IF (JM1 .LT. 1) GO TO 30
         DO 20 I = 1, JM1
            WA1(I) = WA1(I) - R(I,J)*TEMP
   20       CONTINUE
   30    CONTINUE
   40    CONTINUE
   50 CONTINUE
      DO 60 J = 1, N
         L = IPVT(J)
         X(L) = WA1(J)
   60    CONTINUE
C
C     INITIALIZE THE ITERATION COUNTER.
C     EVALUATE THE FUNCTION AT THE ORIGIN, AND TEST
C     FOR ACCEPTANCE OF THE GAUSS-NEWTON DIRECTION.
C
      ITER = 0
      DO 70 J = 1, N
         WA2(J) = DIAG(J)*X(J)
   70    CONTINUE
      DXNORM = DENORM(N,WA2)
      FP = DXNORM - DELTA
      IF (FP .LE. P1*DELTA) GO TO 220
C
C     IF THE JACOBIAN IS NOT RANK DEFICIENT, THE NEWTON
C     STEP PROVIDES A LOWER BOUND, PARL, FOR THE ZERO OF
C     THE FUNCTION. OTHERWISE SET THIS BOUND TO ZERO.
C
      PARL = ZERO
      IF (NSING .LT. N) GO TO 120
      DO 80 J = 1, N
         L = IPVT(J)
         WA1(J) = DIAG(L)*(WA2(L)/DXNORM)
   80    CONTINUE
      DO 110 J = 1, N
         SUM = ZERO
         JM1 = J - 1
         IF (JM1 .LT. 1) GO TO 100
         DO 90 I = 1, JM1
            SUM = SUM + R(I,J)*WA1(I)
   90       CONTINUE
  100    CONTINUE
         WA1(J) = (WA1(J) - SUM)/R(J,J)
  110    CONTINUE
      TEMP = DENORM(N,WA1)
      PARL = ((FP/DELTA)/TEMP)/TEMP
  120 CONTINUE
C
C     CALCULATE AN UPPER BOUND, PARU, FOR THE ZERO OF THE FUNCTION.
C
      DO 140 J = 1, N
         SUM = ZERO
         DO 130 I = 1, J
            SUM = SUM + R(I,J)*QTB(I)
  130       CONTINUE
         L = IPVT(J)
         WA1(J) = SUM/DIAG(L)
  140    CONTINUE
      GNORM = DENORM(N,WA1)
      PARU = GNORM/DELTA
      IF (PARU .EQ. ZERO) PARU = DWARF/MIN(DELTA,P1)
C
C     IF THE INPUT PAR LIES OUTSIDE OF THE INTERVAL (PARL,PARU),
C     SET PAR TO THE CLOSER ENDPOINT.
C
      PAR = MAX(PAR,PARL)
      PAR = MIN(PAR,PARU)
      IF (PAR .EQ. ZERO) PAR = GNORM/DXNORM
C
C     BEGINNING OF AN ITERATION.
C
  150 CONTINUE
         ITER = ITER + 1
C
C        EVALUATE THE FUNCTION AT THE CURRENT VALUE OF PAR.
C
         IF (PAR .EQ. ZERO) PAR = MAX(DWARF,P001*PARU)
         TEMP = SQRT(PAR)
         DO 160 J = 1, N
            WA1(J) = TEMP*DIAG(J)
  160       CONTINUE
         CALL DQRSLV(N,R,LDR,IPVT,WA1,QTB,X,SIGMA,WA2)
         DO 170 J = 1, N
            WA2(J) = DIAG(J)*X(J)
  170       CONTINUE
         DXNORM = DENORM(N,WA2)
         TEMP = FP
         FP = DXNORM - DELTA
C
C        IF THE FUNCTION IS SMALL ENOUGH, ACCEPT THE CURRENT VALUE
C        OF PAR. ALSO TEST FOR THE EXCEPTIONAL CASES WHERE PARL
C        IS ZERO OR THE NUMBER OF ITERATIONS HAS REACHED 10.
C
         IF (ABS(FP) .LE. P1*DELTA
     1       .OR. PARL .EQ. ZERO .AND. FP .LE. TEMP
     2            .AND. TEMP .LT. ZERO .OR. ITER .EQ. 10) GO TO 220
C
C        COMPUTE THE NEWTON CORRECTION.
C
         DO 180 J = 1, N
            L = IPVT(J)
            WA1(J) = DIAG(L)*(WA2(L)/DXNORM)
  180       CONTINUE
         DO 210 J = 1, N
            WA1(J) = WA1(J)/SIGMA(J)
            TEMP = WA1(J)
            JP1 = J + 1
            IF (N .LT. JP1) GO TO 200
            DO 190 I = JP1, N
               WA1(I) = WA1(I) - R(I,J)*TEMP
  190          CONTINUE
  200       CONTINUE
  210       CONTINUE
         TEMP = DENORM(N,WA1)
         PARC = ((FP/DELTA)/TEMP)/TEMP
C
C        DEPENDING ON THE SIGN OF THE FUNCTION, UPDATE PARL OR PARU.
C
         IF (FP .GT. ZERO) PARL = MAX(PARL,PAR)
         IF (FP .LT. ZERO) PARU = MIN(PARU,PAR)
C
C        COMPUTE AN IMPROVED ESTIMATE FOR PAR.
C
         PAR = MAX(PARL,PAR+PARC)
C
C        END OF AN ITERATION.
C
         GO TO 150
  220 CONTINUE
C
C     TERMINATION.
C
      IF (ITER .EQ. 0) PAR = ZERO
      RETURN
C
C     LAST CARD OF SUBROUTINE DMPAR.
C
      END



      SUBROUTINE DQRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, SIGMA,
     +   ACNORM, WA)
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(*)
      LOGICAL PIVOT
      SAVE ONE, P05, ZERO
      DOUBLE PRECISION A(LDA,*),SIGMA(*),ACNORM(*),WA(*)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DOUBLE PRECISION AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
      DOUBLE PRECISION D1MACH,DENORM
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DQRFAC
      EPSMCH = D1MACH(4)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO 10 J = 1, N
         ACNORM(J) = DENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      MINMN = MIN(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX)
         IPVT(KMAX) = K
   40    CONTINUE
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
         AJNORM = DENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J)
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = DENORM(M-J,A(JP1,K))
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DQRFAC.
C
      END



      SUBROUTINE DWUPDT (N, R, LDR, W, B, ALPHA, COS, SIN)
      INTEGER N,LDR
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION R(LDR,*),W(*),B(*),COS(*),SIN(*)
      INTEGER I,J,JM1
      DOUBLE PRECISION COTAN,ONE,P5,P25,ROWJ,TAN,TEMP,ZERO
      SAVE ONE, P5, P25, ZERO
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DWUPDT
      DO 60 J = 1, N
         ROWJ = W(J)
         JM1 = J - 1
C
C        APPLY THE PREVIOUS TRANSFORMATIONS TO
C        R(I,J), I=1,2,...,J-1, AND TO W(J).
C
         IF (JM1 .LT. 1) GO TO 20
         DO 10 I = 1, JM1
            TEMP = COS(I)*R(I,J) + SIN(I)*ROWJ
            ROWJ = -SIN(I)*R(I,J) + COS(I)*ROWJ
            R(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES W(J).
C
         COS(J) = ONE
         SIN(J) = ZERO
         IF (ROWJ .EQ. ZERO) GO TO 50
         IF (ABS(R(J,J)) .GE. ABS(ROWJ)) GO TO 30
            COTAN = R(J,J)/ROWJ
            SIN(J) = P5/SQRT(P25+P25*COTAN**2)
            COS(J) = SIN(J)*COTAN
            GO TO 40
   30    CONTINUE
            TAN = ROWJ/R(J,J)
            COS(J) = P5/SQRT(P25+P25*TAN**2)
            SIN(J) = COS(J)*TAN
   40    CONTINUE
C
C        APPLY THE CURRENT TRANSFORMATION TO R(J,J), B(J), AND ALPHA.
C
         R(J,J) = COS(J)*R(J,J) + SIN(J)*ROWJ
         TEMP = COS(J)*B(J) + SIN(J)*ALPHA
         ALPHA = -SIN(J)*B(J) + COS(J)*ALPHA
         B(J) = TEMP
   50    CONTINUE
   60    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DWUPDT.
C
      END



      SUBROUTINE DQRSLV (N, R, LDR, IPVT, DIAG, QTB, X, SIGMA, WA)
      INTEGER N,LDR
      INTEGER IPVT(*)
      DOUBLE PRECISION R(LDR,*),DIAG(*),QTB(*),X(*),SIGMA(*),WA(*)
      INTEGER I,J,JP1,K,KP1,L,NSING
      DOUBLE PRECISION COS,COTAN,P5,P25,QTBPJ,SIN,SUM,TAN,TEMP,ZERO
      SAVE P5, P25, ZERO
      DATA P5,P25,ZERO /5.0D-1,2.5D-1,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DQRSLV
      DO 20 J = 1, N
         DO 10 I = J, N
            R(I,J) = R(J,I)
   10       CONTINUE
         X(J) = R(J,J)
         WA(J) = QTB(J)
   20    CONTINUE
C
C     ELIMINATE THE DIAGONAL MATRIX D USING A GIVENS ROTATION.
C
      DO 100 J = 1, N
C
C        PREPARE THE ROW OF D TO BE ELIMINATED, LOCATING THE
C        DIAGONAL ELEMENT USING P FROM THE QR FACTORIZATION.
C
         L = IPVT(J)
         IF (DIAG(L) .EQ. ZERO) GO TO 90
         DO 30 K = J, N
            SIGMA(K) = ZERO
   30       CONTINUE
         SIGMA(J) = DIAG(L)
C
C        THE TRANSFORMATIONS TO ELIMINATE THE ROW OF D
C        MODIFY ONLY A SINGLE ELEMENT OF (Q TRANSPOSE)*B
C        BEYOND THE FIRST N, WHICH IS INITIALLY ZERO.
C
         QTBPJ = ZERO
         DO 80 K = J, N
C
C           DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C           APPROPRIATE ELEMENT IN THE CURRENT ROW OF D.
C
            IF (SIGMA(K) .EQ. ZERO) GO TO 70
            IF (ABS(R(K,K)) .GE. ABS(SIGMA(K))) GO TO 40
               COTAN = R(K,K)/SIGMA(K)
               SIN = P5/SQRT(P25+P25*COTAN**2)
               COS = SIN*COTAN
               GO TO 50
   40       CONTINUE
               TAN = SIGMA(K)/R(K,K)
               COS = P5/SQRT(P25+P25*TAN**2)
               SIN = COS*TAN
   50       CONTINUE
C
C           COMPUTE THE MODIFIED DIAGONAL ELEMENT OF R AND
C           THE MODIFIED ELEMENT OF ((Q TRANSPOSE)*B,0).
C
            R(K,K) = COS*R(K,K) + SIN*SIGMA(K)
            TEMP = COS*WA(K) + SIN*QTBPJ
            QTBPJ = -SIN*WA(K) + COS*QTBPJ
            WA(K) = TEMP
C
C           ACCUMULATE THE TRANSFORMATION IN THE ROW OF S.
C
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 70
            DO 60 I = KP1, N
               TEMP = COS*R(I,K) + SIN*SIGMA(I)
               SIGMA(I) = -SIN*R(I,K) + COS*SIGMA(I)
               R(I,K) = TEMP
   60          CONTINUE
   70       CONTINUE
   80       CONTINUE
   90    CONTINUE
C
C        STORE THE DIAGONAL ELEMENT OF S AND RESTORE
C        THE CORRESPONDING DIAGONAL ELEMENT OF R.
C
         SIGMA(J) = R(J,J)
         R(J,J) = X(J)
  100    CONTINUE
C
C     SOLVE THE TRIANGULAR SYSTEM FOR Z. IF THE SYSTEM IS
C     SINGULAR, THEN OBTAIN A LEAST SQUARES SOLUTION.
C
      NSING = N
      DO 110 J = 1, N
         IF (SIGMA(J) .EQ. ZERO .AND. NSING .EQ. N) NSING = J - 1
         IF (NSING .LT. N) WA(J) = ZERO
  110    CONTINUE
      IF (NSING .LT. 1) GO TO 150
      DO 140 K = 1, NSING
         J = NSING - K + 1
         SUM = ZERO
         JP1 = J + 1
         IF (NSING .LT. JP1) GO TO 130
         DO 120 I = JP1, NSING
            SUM = SUM + R(I,J)*WA(I)
  120       CONTINUE
  130    CONTINUE
         WA(J) = (WA(J) - SUM)/SIGMA(J)
  140    CONTINUE
  150 CONTINUE
C
C     PERMUTE THE COMPONENTS OF Z BACK TO COMPONENTS OF X.
C
      DO 160 J = 1, N
         L = IPVT(J)
         X(L) = WA(J)
  160    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DQRSLV.
C
      END



      SUBROUTINE XERMSG(LIBNAME, SUBNAME, ERRMSG, ERRCODE, RETCODE)
      CHARACTER*(*) LIBNAME, SUBNAME, ERRMSG
      INTEGER       ERRCODE, RETCODE
C
C     only print fatal errors
C
      IF (RETCODE .GT. 0) THEN
          WRITE(*, FMT='(1X, A, '':'', A, '':'', 1X, A)') LIBNAME,
     +          SUBNAME, ERRMSG
          WRITE(*, FMT='(1X, ''ERRORCODE = '', I3)') ERRCODE
          WRITE(*, FMT='(1X, ''RETCODE   = '', I3)') RETCODE
      ENDIF
C
      END

C***BEGIN PROLOGUE  DGECO
C***PURPOSE  Factor a matrix using Gaussian elimination and estimate
C            the condition number of the matrix.
C***LIBRARY   SLATEC (LINPACK)
C***CATEGORY  D2A1
C***TYPE      DOUBLE PRECISION (SGECO-S, DGECO-D, CGECO-C)
C***KEYWORDS  CONDITION NUMBER, GENERAL MATRIX, LINEAR ALGEBRA, LINPACK,
C             MATRIX FACTORIZATION
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGECO factors a double precision matrix by Gaussian elimination
C     and estimates the condition of the matrix.
C
C     If  RCOND  is not needed, DGEFA is slightly faster.
C     To solve  A*X = B , follow DGECO by DGESL.
C     To compute  INVERSE(A)*C , follow DGECO by DGESL.
C     To compute  DETERMINANT(A) , follow DGECO by DGEDI.
C     To compute  INVERSE(A) , follow DGECO by DGEDI.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an INTEGER vector of pivot indices.
C
C        RCOND   DOUBLE PRECISION
C                an estimate of the reciprocal condition of  A .
C                For the system  A*X = B , relative perturbations
C                in  A  and  B  of size  EPSILON  may cause
C                relative perturbations in  X  of size  EPSILON/RCOND .
C                If  RCOND  is so small that the logical expression
C                           1.0 + RCOND .EQ. 1.0
C                is true, then  A  may be singular to working
C                precision.  In particular,  RCOND  is zero  if
C                exact singularity is detected or the estimate
C                underflows.
C
C        Z       DOUBLE PRECISION(N)
C                a work vector whose contents are usually unimportant.
C                If  A  is close to a singular matrix, then  Z  is
C                an approximate null vector in the sense that
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DASUM, DAXPY, DDOT, DGEFA, DSCAL
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGECO
      SUBROUTINE DGECO (A, LDA, N, IPVT, RCOND, Z)
      INTEGER LDA,N,IPVT(*)
      DOUBLE PRECISION A(LDA,*),Z(*)
      DOUBLE PRECISION RCOND
C
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
C
C     COMPUTE 1-NORM OF A
C
C***FIRST EXECUTABLE STATEMENT  DGECO
      ANORM = 0.0D0
      DO 10 J = 1, N
         ANORM = MAX(ANORM,DASUM(N,A(1,J),1))
   10 CONTINUE
C
C     FACTOR
C
      CALL DGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = 1.0D0
      DO 20 J = 1, N
         Z(J) = 0.0D0
   20 CONTINUE
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0D0) EK = SIGN(EK,-Z(K))
         IF (ABS(EK-Z(K)) .LE. ABS(A(K,K))) GO TO 30
            S = ABS(A(K,K))/ABS(EK-Z(K))
            CALL DSCAL(N,S,Z,1)
            EK = S*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = ABS(WK)
         SM = ABS(WKM)
         IF (A(K,K) .EQ. 0.0D0) GO TO 40
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         GO TO 50
   40    CONTINUE
            WK = 1.0D0
            WKM = 1.0D0
   50    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GO TO 90
            DO 60 J = KP1, N
               SM = SM + ABS(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + ABS(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*A(K,J)
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
C     SOLVE TRANS(L)*Y = W
C
      DO 120 KB = 1, N
         K = N + 1 - KB
         IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0D0) GO TO 110
            S = 1.0D0/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
      YNORM = 1.0D0
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0D0) GO TO 130
            S = 1.0D0/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (ABS(Z(K)) .LE. ABS(A(K,K))) GO TO 150
            S = ABS(A(K,K))/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0
         T = -Z(K)
         CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END


C***BEGIN PROLOGUE  DGEDI
C***PURPOSE  Compute the determinant and inverse of a matrix using the
C            factors computed by DGECO or DGEFA.
C***LIBRARY   SLATEC (LINPACK)
C***CATEGORY  D3A1, D2A1
C***TYPE      DOUBLE PRECISION (SGEDI-S, DGEDI-D, CGEDI-C)
C***KEYWORDS  DETERMINANT, INVERSE, LINEAR ALGEBRA, LINPACK, MATRIX
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGEDI computes the determinant and inverse of a matrix
C     using the factors computed by DGECO or DGEFA.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DGECO or DGEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from DGECO or DGEFA.
C
C        WORK    DOUBLE PRECISION(N)
C                work vector.  Contents destroyed.
C
C        JOB     INTEGER
C                = 11   both determinant and inverse.
C                = 01   inverse only.
C                = 10   determinant only.
C
C     On Return
C
C        A       inverse of original matrix if requested.
C                Otherwise unchanged.
C
C        DET     DOUBLE PRECISION(2)
C                determinant of original matrix if requested.
C                Otherwise not referenced.
C                Determinant = DET(1) * 10.0**DET(2)
C                with  1.0 .LE. ABS(DET(1)) .LT. 10.0
C                or  DET(1) .EQ. 0.0 .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains
C        a zero on the diagonal and the inverse is requested.
C        It will not occur if the subroutines are called correctly
C        and if DGECO has set RCOND .GT. 0.0 or DGEFA has set
C        INFO .EQ. 0 .
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DAXPY, DSCAL, DSWAP
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGEDI
      SUBROUTINE DGEDI (A, LDA, N, IPVT, DET, WORK, JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),DET(2),WORK(*)
C
      DOUBLE PRECISION T
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
C***FIRST EXECUTABLE STATEMENT  DGEDI
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0D0
         DET(2) = 0.0D0
         TEN = 10.0D0
         DO 50 I = 1, N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
            IF (DET(1) .EQ. 0.0D0) GO TO 60
   10       IF (ABS(DET(1)) .GE. 1.0D0) GO TO 20
               DET(1) = TEN*DET(1)
               DET(2) = DET(2) - 1.0D0
            GO TO 10
   20       CONTINUE
   30       IF (ABS(DET(1)) .LT. TEN) GO TO 40
               DET(1) = DET(1)/TEN
               DET(2) = DET(2) + 1.0D0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(U)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 150
         DO 100 K = 1, N
            A(K,K) = 1.0D0/A(K,K)
            T = -A(K,K)
            CALL DSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0D0
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM INVERSE(U)*INVERSE(L)
C
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 140
         DO 130 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = 0.0D0
  110       CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL DAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION DASUM (N, DX, INCX)
      DOUBLE PRECISION DX(*)
      INTEGER I, INCX, IX, M, MP1, N
C***FIRST EXECUTABLE STATEMENT  DASUM
      DASUM = 0.0D0
      IF (N .LE. 0) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        DASUM = DASUM + ABS(DX(IX))
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increment equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 6.
C
   20 M = MOD(N,6)
      IF (M .EQ. 0) GOTO 40
      DO 30 I = 1,M
        DASUM = DASUM + ABS(DX(I))
   30 CONTINUE
      IF (N .LT. 6) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        DASUM = DASUM + ABS(DX(I)) + ABS(DX(I+1)) + ABS(DX(I+2)) +
     1          ABS(DX(I+3)) + ABS(DX(I+4)) + ABS(DX(I+5))
   50 CONTINUE
      RETURN
      END

      SUBROUTINE DGEFA (A, LDA, N, IPVT, INFO)
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION A(LDA,*)
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
C***FIRST EXECUTABLE STATEMENT  DGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END

      SUBROUTINE DAXPY (N, DA, DX, INCX, DY, INCY)
      DOUBLE PRECISION DX(*), DY(*), DA
C***FIRST EXECUTABLE STATEMENT  DAXPY
      IF (N.LE.0 .OR. DA.EQ.0.0D0) RETURN
      IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 4.
C
   20 M = MOD(N,4)
      IF (M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF (N .LT. 4) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I+1) = DY(I+1) + DA*DX(I+1)
        DY(I+2) = DY(I+2) + DA*DX(I+2)
        DY(I+3) = DY(I+3) + DA*DX(I+3)
   50 CONTINUE
      RETURN
C
C     Code for equal, positive, non-unit increments.
C
   60 NS = N*INCX
      DO 70 I = 1,NS,INCX
        DY(I) = DA*DX(I) + DY(I)
   70 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION DDOT (N, DX, INCX, DY, INCY)
      DOUBLE PRECISION DX(*), DY(*)
C***FIRST EXECUTABLE STATEMENT  DDOT
      DDOT = 0.0D0
      IF (N .LE. 0) RETURN
      IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DDOT = DDOT + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 M = MOD(N,5)
      IF (M .EQ. 0) GO TO 40
      DO 30 I = 1,M
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF (N .LT. 5) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
      DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) + DX(I+2)*DY(I+2) +
     1              DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   50 CONTINUE
      RETURN
C
C     Code for equal, positive, non-unit increments.
C
   60 NS = N*INCX
      DO 70 I = 1,NS,INCX
        DDOT = DDOT + DX(I)*DY(I)
   70 CONTINUE
      RETURN
      END


      SUBROUTINE DSWAP (N, DX, INCX, DY, INCY)
      DOUBLE PRECISION DX(*), DY(*), DTEMP1, DTEMP2, DTEMP3
C***FIRST EXECUTABLE STATEMENT  DSWAP
      IF (N .LE. 0) RETURN
      IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP1 = DX(IX)
        DX(IX) = DY(IY)
        DY(IY) = DTEMP1
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 3.
C
   20 M = MOD(N,3)
      IF (M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        DTEMP1 = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP1
   30 CONTINUE
      IF (N .LT. 3) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        DTEMP1 = DX(I)
        DTEMP2 = DX(I+1)
        DTEMP3 = DX(I+2)
        DX(I) = DY(I)
        DX(I+1) = DY(I+1)
        DX(I+2) = DY(I+2)
        DY(I) = DTEMP1
        DY(I+1) = DTEMP2
        DY(I+2) = DTEMP3
   50 CONTINUE
      RETURN
C
C     Code for equal, positive, non-unit increments.
C
   60 NS = N*INCX
      DO 70 I = 1,NS,INCX
        DTEMP1 = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP1
   70 CONTINUE
      RETURN
      END

      INTEGER FUNCTION IDAMAX (N, DX, INCX)
      DOUBLE PRECISION DX(*), DMAX, XMAG
      INTEGER I, INCX, IX, N
C***FIRST EXECUTABLE STATEMENT  IDAMAX
      IDAMAX = 0
      IF (N .LE. 0) RETURN
      IDAMAX = 1
      IF (N .EQ. 1) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increments not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DMAX = ABS(DX(IX))
      IX = IX + INCX
      DO 10 I = 2,N
        XMAG = ABS(DX(IX))
        IF (XMAG .GT. DMAX) THEN
          IDAMAX = I
          DMAX = XMAG
        ENDIF
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increments equal to 1.
C
   20 DMAX = ABS(DX(1))
      DO 30 I = 2,N
        XMAG = ABS(DX(I))
        IF (XMAG .GT. DMAX) THEN
          IDAMAX = I
          DMAX = XMAG
        ENDIF
   30 CONTINUE
      RETURN
      END

      SUBROUTINE DSCAL (N, DA, DX, INCX)
      DOUBLE PRECISION DA, DX(*)
      INTEGER I, INCX, IX, M, MP1, N
C***FIRST EXECUTABLE STATEMENT  DSCAL
      IF (N .LE. 0) RETURN
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        DX(IX) = DA*DX(IX)
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increment equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 M = MOD(N,5)
      IF (M .EQ. 0) GOTO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF (N .LT. 5) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I+1) = DA*DX(I+1)
        DX(I+2) = DA*DX(I+2)
        DX(I+3) = DA*DX(I+3)
        DX(I+4) = DA*DX(I+4)
   50 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION DRAND (R)
      DOUBLE PRECISION R
C***BEGIN PROLOGUE  RAND
C***PURPOSE  Generate a uniformly distributed random number.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  L6A21
C***TYPE      SINGLE PRECISION (RAND-S)
C***KEYWORDS  FNLIB, RANDOM NUMBER, SPECIAL FUNCTIONS, UNIFORM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C      This pseudo-random number generator is portable among a wide
C variety of computers.  RAND(R) undoubtedly is not as good as many
C readily available installation dependent versions, and so this
C routine is not recommended for widespread usage.  Its redeeming
C feature is that the exact same random numbers (to within final round-
C off error) can be generated from machine to machine.  Thus, programs
C that make use of random numbers can be easily transported to and
C checked in a new environment.
C
C      The random numbers are generated by the linear congruential
C method described, e.g., by Knuth in Seminumerical Methods (p.9),
C Addison-Wesley, 1969.  Given the I-th number of a pseudo-random
C sequence, the I+1 -st number is generated from
C             X(I+1) = (A*X(I) + C) MOD M,
C where here M = 2**22 = 4194304, C = 1731 and several suitable values
C of the multiplier A are discussed below.  Both the multiplier A and
C random number X are represented in double precision as two 11-bit
C words.  The constants are chosen so that the period is the maximum
C possible, 4194304.
C
C      In order that the same numbers be generated from machine to
C machine, it is necessary that 23-bit integers be reducible modulo
C 2**11 exactly, that 23-bit integers be added exactly, and that 11-bit
C integers be multiplied exactly.  Furthermore, if the restart option
C is used (where R is between 0 and 1), then the product R*2**22 =
C R*4194304 must be correct to the nearest integer.
C
C      The first four random numbers should be .0004127026,
C .6750836372, .1614754200, and .9086198807.  The tenth random number
C is .5527787209, and the hundredth is .3600893021 .  The thousandth
C number should be .2176990509 .
C
C      In order to generate several effectively independent sequences
C with the same generator, it is necessary to know the random number
C for several widely spaced calls.  The I-th random number times 2**22,
C where I=K*P/8 and P is the period of the sequence (P = 2**22), is
C still of the form L*P/8.  In particular we find the I-th random
C number multiplied by 2**22 is given by
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0
C Thus the 4*P/8 = 2097152 random number is 2097152/2**22.
C
C      Several multipliers have been subjected to the spectral test
C (see Knuth, p. 82).  Four suitable multipliers roughly in order of
C goodness according to the spectral test are
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1
C
C      In the table below LOG10(NU(I)) gives roughly the number of
C random decimal digits in the random numbers considered I at a time.
C C is the primary measure of goodness.  In both cases bigger is better.
C
C                   LOG10 NU(I)              C(I)
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
C
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
C   Best
C    Possible   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
C
C             Input Argument --
C R      If R=0., the next random number of the sequence is generated.
C        If R .LT. 0., the last generated number will be returned for
C          possible use in a restart procedure.
C        If R .GT. 0., the sequence of random numbers will start with
C          the seed R mod 1.  This seed is also returned as the value of
C          RAND provided the arithmetic is done exactly.
C
C             Output Value --
C RAND   a pseudo-random number between 0. and 1.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   960708  Changed to double precision for usage in SAXDAS (UL)
C***END PROLOGUE  RAND
      SAVE IA1, IA0, IA1MA0, IC, IX1, IX0
      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/
      DATA IC /1731/
      DATA IX1, IX0 /0, 0/
C***FIRST EXECUTABLE STATEMENT  RAND
      IF (R.LT.0.) GO TO 10
      IF (R.GT.0.) GO TO 20
C
C           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)
C                   + IA0*IX0) + IA0*IX0
C
      IY0 = IA0*IX0
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0
      IY0 = IY0 + IC
      IX0 = MOD (IY0, 2048)
      IY1 = IY1 + (IY0-IX0)/2048
      IX1 = MOD (IY1, 2048)
C
 10   DRAND = IX1*2048 + IX0
      DRAND = DRAND / 4194304.D0
      RETURN
C
 20   IX1 = MOD(R,1.D0)*4194304.D0 + 0.5D0
      IX0 = MOD (IX1, 2048)
      IX1 = (IX1-IX0)/2048
      GO TO 10
C
      END
