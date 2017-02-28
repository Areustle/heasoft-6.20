
      SUBROUTINE SA(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,
     1              ISEED1,ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     2              FSTAR,XP,NACP)

C  Version: 3.1
C  Date: 6/10/91.
C  Differences compared to Version 2.0:
C     1. If a trial is out of bounds, a point is randomly selected
C        from LB(i) to UB(i). Unlike in version 2.0, this trial is
C        evaluated and is counted in acceptances and rejections.
C        All corresponding documentation is changed as well.
C  Differences compared to Version 3.0:
C     1. If VM(i) > (UB(i) - LB(i)), VM is set to UB(i) - LB(i).
C        The idea is that if T is high relative to LB & UB, most
C        points will be accepted, causing VM to rise. But, in this
C        situation, VM has little meaning; particularly if VM is
C        larger than the acceptable region. Setting VM to this size
C        still allows all parts of the allowable region to be selected.
C  Call to SA changed?
C     Yes. Integer work array IWK2 has been removed. In addition,
C          IWK1 has been renamed IWK.
C
C  Synopsis:
C  This routine implements the continuous simulated annealing global
C  optimization algorithm described in Corana et al.'s article
C  "Minimizing Multimodal Functions of Continuous Variables with the
C  "Simulated Annealing" Algorithm" in the September 1987 (vol. 13,
C  no. 3, pp. 262-280) issue of the ACM Transactions on Mathematical
C  Software.
C
C  A very quick (perhaps too quick) overview of SA:
C     SA tries to find the global optimum of an N dimensional function.
C  It moves both up and downhill and as the optimization process
C  proceeds, it focuses on the most promising area.
C     To start, it randomly chooses a trial point within the step length
C  VM (a vector of length N) of the user selected starting point. The
C  function is evaluated at this trial point and its value is compared
C  to its value at the initial point.
C     In a maximization problem, all uphill moves are accepted and the
C  algorithm continues from that trial point. Downhill moves may be
C  accepted; the decision is made by the Metropolis criteria. It uses T
C  (temperature) and the size of the downhill move in a probabilistic
C  manner. The smaller T and the size of the downhill move are, the more
C  likely that move will be accepted. If the trial is accepted, the
C  algorithm moves on from that point. If it is rejected, another point
C  is chosen instead for a trial evaluation.
C     Each element of VM periodically adjusted so that half of all
C  function evaluations in that direction are accepted.
C     A fall in T is imposed upon the system with the RT variable by
C  T(i+1) = RT*T(i) where i is the ith iteration. Thus, as T declines,
C  downhill moves are less likely to be accepted and the percentage of
C  rejections rise. Given the scheme for the selection for VM, VM falls.
C  Thus, as T declines, VM falls and SA focuses upon the most promising
C  area for optimization.
C
C  The importance of the parameter T:
C     The parameter T is crucial in using SA successfully. It influences
C  VM, the step length over which the algorithm searches for optima. For
C  a small intial T, the step length may be too small; thus not enough
C  of the function might be evaluated to find the global optima. The user
C  should carefully examine VM in the intermediate output (set IPRINT =
C  1) to make sure that VM is appropriate. The relationship between the
C  initial temperature and the resulting step length is function
C  dependent.
C     To determine the starting temperature that is consistent with
C  optimizing a function, it is worthwhile to run a trial run first. Set
C  RT = 1.5 and T = 1.0. With RT > 1.0, the temperature increases and VM
C  rises as well. Then select the T that produces a large enough VM.
C
C  For modifications to the algorithm and many details on its use,
C  (particularly for econometric applications) see Goffe, Ferrier
C  and Rogers, "Global Optimization of Statistical Functions with
C  the Simulated Annealing Algorithm," October, 1991.
C  For a copy, contact
C              Bill Goffe
C              Department of Economics
C              Southern Methodist University
C              Dallas, TX  75275
C              h2zr1001 @ smuvm1 (Bitnet)
C              h2zr1001 @ vm.cis.smu.edu (Internet)
C
C  As far as possible, the parameters here have the same name as in
C  the description of the algorithm on pp. 266-8 of Corana et al.
C
C  In this description, SP is single precision, DP is double precision,
C  INT is integer, L is logical and (N) denotes an array of length n.
C  Thus, DP(N) denotes a double precision array of length n.
C
C  Input Parameters:
C    Note: The suggested values generally come from Corana et al. To
C          drastically reduce runtime, see Goffe et al., pp. 17-8 for
C          suggestions on choosing the appropriate RT and NT.
C    N - Number of variables in the function to be optimized. (INT)
C    X - The starting values for the variables of the function to be
C        optimized. (DP(N))
C    MAX - Denotes whether the function should be maximized or
C          minimized. A true value denotes maximization while a false
C          value denotes minimization. Intermediate output (see IPRINT)
C          takes this into account. (L)
C    RT - The temperature reduction factor. The value suggested by
C         Corana et al. is .85. See Goffe et al. for more advice. (DP)
C    EPS - Error tolerance for termination. If the final function
C          values from the last neps temperatures differ from the
C          corresponding value at the current temperature by less than
C          EPS and the final function value at the current temperature
C          differs from the current optimal function value by less than
C          EPS, execution terminates and IER = 0 is returned. (EP)
C    NS - Number of cycles. After NS*N function evaluations, each
C         element of VM is adjusted so that approximately half of
C         all function evaluations are accepted. The suggested value
C         is 20. (INT)
C    NT - Number of iterations before temperature reduction. After
C         NT*NS*N function evaluations, temperature (T) is changed
C         by the factor RT. Value suggested by Corana et al. is
C         MAX(100, 5*N). See Goffe et al. for further advice. (INT)
C    NEPS - Number of final function values used to decide upon termi-
C           nation. See EPS. Suggested value is 4. (INT)
C    MAXEVL - The maximum number of function evaluations. If it is
C             exceeded, IER = 1. (INT)
C    LB - The lower bound for the allowable solution variables. (DP(N))
C    UB - The upper bound for the allowable solution variables. (DP(N))
C         If the algorithm chooses X(I) .LT. LB(I) or X(I) .GT. UB(I),
C         I = 1, N, a point is from inside is randomly selected. This
C         This focuses the algorithm on the region inside UB and LB.
C         Unless the user wishes to concentrate the search to a par-
C         ticular region, UB and LB should be set to very large positive
C         and negative values, respectively. Note that the starting
C         vector X should be inside this region. Also note that LB and
C         UB are fixed in position, while VM is centered on the last
C         accepted trial set of variables that optimizes the function.
C    C - Vector that controls the step length adjustment. The suggested
C        value for all elements is 2.0. (DP(N))
C    IPRINT - controls printing inside SA. (INT)
C             Values: 0 - Nothing printed.
C                     1 - Function value for the starting value and
C                         summary results before each temperature
C                         reduction. This includes the optimal
C                         function value found so far, the total
C                         number of moves (broken up into uphill,
C                         downhill, accepted and rejected), the
C                         number of out of bounds trials, the
C                         number of new optima found at this
C                         temperature, the current optimal X and
C                         the step length VM. Note that there are
C                         N*NS*NT function evalutations before each
C                         temperature reduction. Finally, notice is
C                         is also given upon achieveing the termination
C                         criteria.
C                     2 - Each new step length (VM), the current optimal
C                         X (XOPT) and the current trial X (X). This
C                         gives the user some idea about how far X
C                         strays from XOPT as well as how VM is adapting
C                         to the function.
C                     3 - Each function evaluation, its acceptance or
C                         rejection and new optima. For many problems,
C                         this option will likely require a small tree
C                         if hard copy is used. This option is best
C                         used to learn about the algorithm. A small
C                         value for MAXEVL is thus recommended when
C                         using IPRINT = 3.
C             Suggested value: 1
C             Note: For a given value of IPRINT, the lower valued
C                   options (other than 0) are utilized.
C !! note IPRINT is obsolete in XSPEC implementation because the chatter !!
C !! flag determines this.                                               !!
C    ISEED1 - The first seed for the random number generator RANMAR.
C             0 .LE. ISEED1 .LE. 31328.
C    ISEED2 - The second seed for the random number generator RANMAR.
C             0 .LE. ISEED2 .LE. 30081. Different values for ISEED1
C             and ISEED2 will lead to an entirely different sequence
C             of trial points and decisions on downhill moves (when
C             maximizing). See Goffe et al. on how this can be used
C             to test the results of SA. (INT)
C
C  Input/Output Parameters:
C    T - On input, the initial temperature. See Goffe et al. for advice.
C        On output, the final temperature. (DP)
C    VM - The step length vector. On input it should encompass the
C         region of interest given the starting value X. For point
C         X(I), the next trial point is selected is from X(I) - VM(I)
C         to  X(I) + VM(I). Since VM is adjusted so that about half
C         of all points are accepted, the input value is not very
C         important (i.e. is the value is off, SA adjusts VM to the
C         correct value). (DP(N))
C
C  Output Parameters:
C    XOPT - The variables that optimize the function. (DP(N))
C    FOPT - The optimal value of the function. (DP)
C    NACC - The number of accepted function evaluations. (INT)
C    NFCNEV - The total number of function evaluations. In a minor
C             point, note that the first evaluation is not used in the
C             core of the algorithm; it simply initializes the
C             algorithm. (INT).
C    NOBDS - The total number of trial function evaluations that
C            would have been out of bounds of LB and UB. Note that
C            a trial point is randomly selected between LB and UB.
C            (INT)
C    IER - The error return number. (INT)
C          Values: 0 - Normal return; termination criteria achieved.
C                  1 - Number of function evaluations (NFCNEV) is
C                      greater than the maximum number (MAXEVL).
C                  2 - The starting value (X) is not inside the
C                      bounds (LB and UB).
C                  99 - Should not be seen; only used internally.
C
C  Work arrays that must be dimensioned in the calling routine:
C       RWK1 (DP(NEPS))  (FSTAR in SA)
C       RWK2 (DP(N))     (XP    "  " )
C       IWK  (INT(N))    (NACP  "  " )
C
C  Required Functions (included):
C    EXPREP - Replaces the function EXP to avoid under- and overflows.
C             It may have to be modified for non IBM-type main-
C             frames. (DP)
C    RMARIN - Initializes the random number generator RANMAR.
C    RANMAR - The actual random number generator. Note that
C             RMARIN must run first (SA does this). It produces uniform
C             random numbers on [0,1]. These routines are from
C             Usenet's comp.lang.fortran, article 2605. For
C             reference, see "Toward a Universal Random Number
C             Generator" by George Marsaglia and Arif Zaman.
C             Florida State University Report: FSU-SCRI-87-50 (1987).
C             It was later modified by F. James and published in
C             "A Review of Pseudo-random Number Generators." For
C             further information, contact stuart@ads.com. These
C             routines are designed to be portable on any machine
C             with a 24-bit or more mantissa. I have found it produces
C             identical results on a IBM 3081 and a Cray Y-MP.
C
C  Required Subroutines (included):
C    PRTVEC - Prints vectors.
C    PRT0 ... PRT8 - Prints intermediate output.
C    ANNFCN - Function to be optimized. The form is (SACalcStat - C++ implementation)
C            SUBROUTINE ANNFCN(N,X,F)
C            INTEGER N
C            DOUBLE PRECISION  X(N), F
C            ...
C            function code with F = F(X)
C            ...
C            RETURN
C            END
C          Note: This is the same form used in the multivariable
C          minimization algorithms in the IMSL edition 10 library.
C
C  Machine Specific Features:
C    1. EXPREP may have to be modified if used on non-IBM type main-
C       frames. Watch for under- and overflows in EXPREP.
C    2. Some FORMAT statements use G25.18; this may be excessive for
C       some machines.
C    3. RMARIN and RANMAR are designed to be protable; they should not
C       cause any problems.

C  Type all external variables.
      DOUBLE PRECISION  X(*), LB(*), UB(*), C(*), VM(*), FSTAR(*),
     1                  XOPT(*), XP(*), T, EPS, RT, FOPT
      INTEGER  NACP(*), N, NS, NT, NEPS, NACC, MAXEVL,
     1         IPRINT, NOBDS, IER, NFCNEV, ISEED1, ISEED2
      LOGICAL  MAX

C  Type all internal variables.
      DOUBLE PRECISION  F, FP, P, PP, RATIO
      INTEGER  NUP, NDOWN, NREJ, NNEW, LNOBDS, H, I, J, M, IEPS
      LOGICAL  QUIT

C  Type all functions.
      DOUBLE PRECISION  EXPREP
      REAL  RANMAR

C  Initialize the random number generator RANMAR.
      CALL RMARIN(ISEED1,ISEED2)

C  Set initial values.
      NACC = 0
      NOBDS = 0
      NFCNEV = 0
      IER = 99

      DO 10, I = 1, N
         XOPT(I) = X(I)
         NACP(I) = 0
10    CONTINUE

      DO 20, I = 1, NEPS
         FSTAR(I) = 1.0D+20
20    CONTINUE

C  If the initial value is out of bounds, notify the user and return
C  to the calling routine.
      DO 30, I = 1, N
         IF ((X(I) .GT. UB(I)) .OR. (X(I) .LT. LB(I))) THEN
            CALL PRT1
            IER = 2
            RETURN
         END IF
30    CONTINUE

C  Evaluate the function with input X and return value as F.
      CALL SACalcStat(N,X,F)

C  If the function is to be minimized, switch the sign of the function.
C  Note that all intermediate and final output switches the sign back
C  to eliminate any possible confusion for the user.
      IF(.NOT. MAX) F = -F
      NFCNEV = NFCNEV + 1
      FOPT = F
      IEPS = 1
      FSTAR(IEPS) = F
      CALL PRT2(MAX,N,X,F)

C  Start the main loop. Note that it terminates if (i) the algorithm
C  succesfully optimizes the function or (ii) there are too many
C  function evaluations (more than MAXEVL).
100   NUP = 0
      NREJ = 0
      NNEW = 0
      NDOWN = 0
      LNOBDS = 0

      DO 400, M = 1, NT
         DO 300, J = 1, NS
            DO 200, H = 1, N

C  Generate XP, the trial value of X. Note use of VM to choose XP.
               DO 110, I = 1, N
                  IF (I .EQ. H) THEN
                     XP(I) = X(I) + (RANMAR()*2.- 1.) * VM(I)
                  ELSE
                     XP(I) = X(I)
                  END IF

C  If XP is out of bounds, select a point in bounds for the trial.
                  IF((XP(I) .LT. LB(I)) .OR. (XP(I) .GT. UB(I))) THEN
                    XP(I) = LB(I) + (UB(I) - LB(I))*RANMAR()
                    LNOBDS = LNOBDS + 1
                    NOBDS = NOBDS + 1
                    CALL PRT3(MAX,N,XP,X,FP,F)
                  END IF
110            CONTINUE

C  Evaluate the function with the trial point XP and return as FP.
               CALL SACalcStat(N,XP,FP)
               IF(.NOT. MAX) FP = -FP
               NFCNEV = NFCNEV + 1
               CALL PRT4(MAX,N,XP,X,FP,F)

C  If too many function evaluations occur, terminate the algorithm.
               IF(NFCNEV .GE. MAXEVL) THEN
                  CALL PRT5
                  IF (.NOT. MAX) FOPT = -FOPT
                  IER = 1
                  RETURN
               END IF

C  Accept the new point if the function value increases.
               IF(FP .GE. F) THEN
                  CALL xwrite('  POINT ACCEPTED', 25)
                  DO 120, I = 1, N
                     X(I) = XP(I)
120               CONTINUE
                  F = FP
                  NACC = NACC + 1
                  NACP(H) = NACP(H) + 1
                  NUP = NUP + 1
                  IEPS = IEPS + 1
                  IF ( IEPS .GT. NEPS ) IEPS = 1
                  FSTAR(IEPS) = FP

C  If greater than any other point, record as new optimum.
                  IF (FP .GT. FOPT) THEN
                     CALL xwrite('  NEW OPTIMUM', 25)
                     DO 130, I = 1, N
                        XOPT(I) = XP(I)
130                  CONTINUE
                     FOPT = FP
                     NNEW = NNEW + 1
                  END IF

C  If the point is lower, use the Metropolis criteria to decide on
C  acceptance or rejection.
               ELSE
                  P = EXPREP((FP - F)/T)
                  PP = RANMAR()
                  IF (PP .LT. P) THEN
                     CALL PRT6(MAX)
                     DO 140, I = 1, N
                        X(I) = XP(I)
140                  CONTINUE
                     F = FP
                     NACC = NACC + 1
                     NACP(H) = NACP(H) + 1
                     NDOWN = NDOWN + 1
                     IEPS = IEPS + 1
                     IF ( IEPS .GT. NEPS ) IEPS = 1
                     FSTAR(IEPS) = FP
                  ELSE
                     NREJ = NREJ + 1
                     CALL PRT7(MAX)
                  END IF
               END IF

200         CONTINUE
300      CONTINUE

C  Adjust VM so that approximately half of all evaluations are accepted.
         DO 310, I = 1, N
            RATIO = DFLOAT(NACP(I)) /DFLOAT(NS)
            IF (RATIO .GT. .6) THEN
               VM(I) = VM(I)*(1. + C(I)*(RATIO - .6)/.4)
            ELSE IF (RATIO .LT. .4) THEN
               VM(I) = VM(I)/(1. + C(I)*((.4 - RATIO)/.4))
            END IF
            IF (VM(I) .GT. (UB(I)-LB(I))) THEN
               VM(I) = UB(I) - LB(I)
            END IF
310      CONTINUE

         CALL PRT8(N,VM,XOPT,X)

         DO 320, I = 1, N
            NACP(I) = 0
320      CONTINUE

400   CONTINUE

      CALL PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)

C  Check termination criteria.
      QUIT = .FALSE.
      IF ((FOPT - FSTAR(IEPS)) .LE. EPS) QUIT = .TRUE.
      DO 410, I = 1, NEPS
         IF (ABS(F - FSTAR(I)) .GT. EPS) QUIT = .FALSE.
410   CONTINUE
      CALL PRT11(MAX,NEPS,FSTAR,FOPT,EPS)

C  Terminate SA if appropriate.
      IF (QUIT) THEN
         DO 420, I = 1, N
            X(I) = XOPT(I)
420      CONTINUE
         IER = 0
         IF (.NOT. MAX) FOPT = -FOPT
         CALL PRT10
         RETURN
      END IF

C  If termination criteria is not met, prepare for another loop.
      T = RT*T
      DO 430, I = NEPS, 2, -1
         FSTAR(I) = FSTAR(I-1)
430   CONTINUE
      F = FOPT
      DO 440, I = 1, N
         X(I) = XOPT(I)
440   CONTINUE

C  Loop again.
      GO TO 100

      END

      FUNCTION  EXPREP(RDUM)
C  This function replaces exp to avoid under- and overflows and is
C  designed for IBM 370 type machines. It may be necessary to modify
C  it for other machines. Note that the maximum and minimum values of
C  EXPREP are such that they has no effect on the algorithm.

      DOUBLE PRECISION  RDUM, EXPREP

      IF (RDUM .GT. 50.) THEN
         EXPREP = 5.18D+21
      ELSE IF (RDUM .LT. -50.) THEN
         EXPREP = 0.0
      ELSE
         EXPREP = EXP(RDUM)
      END IF

      RETURN
      END

      subroutine RMARIN(IJ,KL)
C  This subroutine and the next function generate random numbers. See
C  the comments for SA for more information. The only change from the
C  orginal code is that the test to make sure that RMARIN runs first
C  was taken out since SA assures that this is done (this test didn't
C  compile under IBM's VS Fortran). All follwing lines are original.

C This is the initialization routine for the random number generator
C     RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
      real U(97), C, CD, CM
      integer I97, J97
      real s, t
      integer ij, kl, i, j, k, l, ii, jj, m

      common /raset1/ U, C, CD, CM, I97, J97

      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or.
     *    KL .lt. 0  .or.  KL .gt. 30081 ) then
          WRITE(*, '(a,a)') ' The first random number seed must', 
     &                      ' have a value between 0 and 31328'
          WRITE(*, '(a,a)') ' The second seed must have a value',
     &                      ' between 0 and 30081'
            stop
      endif
      i = mod(IJ/177, 177) + 2
      j = mod(IJ    , 177) + 2
      k = mod(KL/169, 178) + 1
      l = mod(KL,     169)
      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      return
      end

      function ranmar()

      real ranmar
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
      real uni

         uni = U(I97) - U(J97)
         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         RANMAR = uni
      return
      END

      SUBROUTINE PRT1
C  This subroutine prints intermediate output, as does PRT2 through
C  PRT10. Note that if SA is minimizing the function, the sign of the
C  function value and the directions (up/down) are reversed in all
C  output to correspond with the actual function optimization. This
C  correction is because SA was written to maximize functions and
C  it minimizes by maximizing the negative a function.

      CALL xwrite(' ',5)
      CALL xwrite('  THE STARTING VALUE (X) IS OUTSIDE THE BOUNDS ',5)
      CALL xwrite('  (LB AND UB). EXECUTION TERMINATED WITHOUT ANY',5)
      CALL xwrite('  OPTIMIZATION. RESPECIFY X, UB OR LB SO THAT  ',5)
      CALL xwrite('  LB(I) .LT. X(I) .LT. UB(I), I = 1, N.        ',5)
      CALL xwrite(' ',5)

      RETURN
      END

      SUBROUTINE PRT2(MAX,N,X,F)

      DOUBLE PRECISION  X(*), F
      INTEGER  N
      LOGICAL  MAX
      CHARACTER WRTSTR*255

      CALL xwrite('  ',10)
      CALL PRTVEC(X,N,'INITIAL X', 10)
      IF (MAX) THEN
         WRITE(WRTSTR,'(''  INITIAL F: '',G25.18)') F
      ELSE
         WRITE(WRTSTR,'(''  INITIAL F: '',G25.18)') -F
      END IF
      CALL xwrite(WRTSTR, 10)
      CALL xwrite('  ',10)

      RETURN
      END

      SUBROUTINE PRT3(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX
      CHARACTER WRTSTR*255

      CALL xwrite('  ', 25)
      CALL PRTVEC(X,N,'CURRENT X', 25)
      IF (MAX) THEN
         WRITE(WRTSTR,'(''  CURRENT F: '',G25.18)') F
      ELSE
         WRITE(WRTSTR,'(''  CURRENT F: '',G25.18)') -F
      END IF
      CALL xwrite(WRTSTR, 25)
      CALL PRTVEC(XP,N,'TRIAL X', 25)
      CALL xwrite('  POINT REJECTED SINCE OUT OF BOUNDS', 25)

      RETURN
      END

      SUBROUTINE PRT4(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX
      CHARACTER WRTSTR*255

      CALL xwrite('  ', 25)
      CALL PRTVEC(X,N,'CURRENT X', 25)
      IF (MAX) THEN
         WRITE(WRTSTR,'(''  CURRENT F: '',G25.18)') F
         CALL xwrite(WRTSTR, 25)
         CALL PRTVEC(XP,N,'TRIAL X', 25)
         WRITE(WRTSTR,'(''  RESULTING F: '',G25.18)') FP 
         CALL xwrite(WRTSTR, 25)
      ELSE
         WRITE(WRTSTR,'(''  CURRENT F: '',G25.18)') -F
         CALL xwrite(WRTSTR, 25)
         CALL PRTVEC(XP,N,'TRIAL X', 25)
         WRITE(WRTSTR,'(''  RESULTING F: '',G25.18)') -FP

         CALL xwrite(WRTSTR, 25)
      END IF

      RETURN
      END

      SUBROUTINE PRT5

      CALL xwrite(' ', 5)
      CALL xwrite('  TOO MANY FUNCTION EVALUATIONS; CONSIDER ', 5)
      CALL xwrite('  INCREASING MAXEVL OR EPS, OR DECREASING ', 5)
      CALL xwrite('  NT OR RT. THESE RESULTS ARE LIKELY TO BE ', 5)
      CALL xwrite('  POOR.', 5)
      CALL xwrite(' ', 5)

      RETURN
      END

      SUBROUTINE PRT6(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         CALL xwrite('  THOUGH LOWER, POINT ACCEPTED', 25)
      ELSE
         CALL xwrite('  THOUGH HIGHER, POINT ACCEPTED', 25)
      END IF

      RETURN
      END

      SUBROUTINE PRT7(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         CALL xwrite('  LOWER POINT REJECTED', 25)
      ELSE
         CALL xwrite('  HIGHER POINT REJECTED', 25)
      END IF

      RETURN
      END

      SUBROUTINE PRT8(N,VM,XOPT,X)

      DOUBLE PRECISION  VM(*), XOPT(*), X(*)
      INTEGER  N

      CALL xwrite(' ', 15)
      CALL xwrite(' INTERMEDIATE RESULTS AFTER STEP LENGTH ADJUSTMENT',
     &            15)
      CALL xwrite(' ', 15)
      CALL PRTVEC(VM,N,'NEW STEP LENGTH (VM)', 15)
      CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X', 15)
      CALL PRTVEC(X,N,'CURRENT X', 15)
      CALL xwrite(' ', 15)

      RETURN
      END

      SUBROUTINE PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)

      DOUBLE PRECISION  XOPT(*), VM(*), T, FOPT
      INTEGER  N, NUP, NDOWN, NREJ, LNOBDS, NNEW, TOTMOV
      LOGICAL  MAX
      CHARACTER WRTSTR*255

      TOTMOV = NUP + NDOWN + NREJ

      CALL xwrite(' ', 10)
      CALL xwrite(
     & ' INTERMEDIATE RESULTS BEFORE NEXT TEMPERATURE REDUCTION', 10)
      CALL xwrite(' ', 10)
      WRITE(WRTSTR,'(''  CURRENT TEMPERATURE:            '',G12.5)') T
      CALL xwrite(WRTSTR, 10)
      IF (MAX) THEN
         WRITE(WRTSTR,'(''  MAX FUNCTION VALUE SO FAR:  '',G25.18)') 
     &         FOPT
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     UPHILL:                  '',I8)') NUP
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     ACCEPTED DOWNHILL:       '',I8)') NDOWN
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     REJECTED DOWNHILL:       '',I8)') NREJ
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  OUT OF BOUNDS TRIALS:       '',I8)') LNOBDS
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  NEW MAXIMA THIS TEMPERATURE:'',I8)') NNEW
         CALL xwrite(WRTSTR, 10)
      ELSE
         WRITE(WRTSTR,'(''  MIN FUNCTION VALUE SO FAR:  '',G25.18)')
     &         -FOPT
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     DOWNHILL:                '',I8)') NUP
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     ACCEPTED UPHILL:         '',I8)') NDOWN
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''     REJECTED UPHILL:         '',I8)') NREJ
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  TRIALS OUT OF BOUNDS:       '',I8)') LNOBDS
         CALL xwrite(WRTSTR, 10)
         WRITE(WRTSTR,'(''  NEW MINIMA THIS TEMPERATURE:'',I8)') NNEW
         CALL xwrite(WRTSTR, 10)
      END IF
      CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X', 10)
      CALL PRTVEC(VM,N,'STEP LENGTH (VM)', 10)
      CALL xwrite(' ', 10)

      RETURN
      END

      SUBROUTINE PRT10

      CALL xwrite(' ', 10)
      CALL xwrite('  SA ACHIEVED TERMINATION CRITERIA. IER = 0. ', 10)
      CALL xwrite(' ', 10)

      RETURN
      END

      SUBROUTINE PRT11(MAX,NEPS,FSTAR,FOPT,EPS)

      INTEGER NEPS
      DOUBLE PRECISION FSTAR(NEPS),FOPT,EPS
      LOGICAL MAX

      INTEGER i, switch
      CHARACTER WRTSTR*255

      CALL xwrite(' ', 10)
      WRITE(wrtstr,'(a, g25.18)') 
     &  ' Test for convergence : criterion = ', EPS
      CALL xwrite(wrtstr, 10)

      IF ( max ) THEN
         switch = 1
      ELSE
         switch = -1
      ENDIF

      WRITE(wrtstr, '('' Function optimum = '', g25.18)') fopt*switch
      CALL xwrite(wrtstr, 10)
      CALL xwrite(' Last Function evaluations : ', 10)
      DO i = 1, NEPS
         WRITE(wrtstr, '(15x, g25.18)') fstar(i)*switch
         CALL xwrite(wrtstr, 10)
      ENDDO

      RETURN
      END

      SUBROUTINE PRTVEC(VECTOR,NCOLS,NAME,ICHAT)
C  This subroutine prints the double precision vector named VECTOR.
C  Elements 1 thru NCOLS will be printed. NAME is a character variable
C  that describes VECTOR. Note that if NAME is given in the call to
C  PRTVEC, it must be enclosed in quotes. If there are more than 10
C  elements in VECTOR, 10 elements will be printed on each line.

      INTEGER NCOLS,ICHAT
      DOUBLE PRECISION VECTOR(NCOLS)
      CHARACTER *(*) NAME

      INTEGER LINES, LL, I, J
      CHARACTER WRTSTR*255

      CALL xwrite(' ',ICHAT)
      WRITE(WRTSTR,'(25X,A)') NAME
      CALL xwrite(WRTSTR,ICHAT)

      IF (NCOLS .GT. 10) THEN
         LINES = INT(NCOLS/10.)

         DO 100, I = 1, LINES
            LL = 10*(I - 1)
            WRITE(WRTSTR,1000) (VECTOR(J),J = 1+LL, 10+LL)
            CALL xwrite(WRTSTR,ICHAT)
  100    CONTINUE

         WRITE(WRTSTR,1000) (VECTOR(J),J = 11+LL, NCOLS)
         CALL xwrite(WRTSTR,ICHAT)
      ELSE
         WRITE(WRTSTR,1000) (VECTOR(J),J = 1, NCOLS)
         CALL xwrite(WRTSTR,ICHAT)
      END IF

 1000 FORMAT( 10(G12.5,1X))

      RETURN
      END

