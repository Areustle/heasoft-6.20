      SUBROUTINE CURFIT(Rdat, Igrp, iy0, Iery, Mxrow, Low, Npts,
     &   Xmin, Xmax, Ichat, Niter, Istat, Icomp, Idoun, Nterm,
     &   Fdelmn, Pval, Plim, Chisq)
      INTEGER   Igrp, iy0, Iery, Mxrow, Low, Npts
      INTEGER   Ichat, Niter, Icomp(*), Idoun, Nterm, Istat
      REAL      Rdat(*), Xmin(*), Xmax(*), Pval(*), Plim(3, *)
      REAL      Fdelmn, Chisq
C---
C Closely parallels CURFIT given in Bevington p.238
C Contains a vector-subset capability, used by setting Plim(1, I)<0.
C---
C Rdat    I    The data array
C Igrp    I    Used by pltxc to calculate the X coordinate(s).
C Iy0     I    Y(iy0+I) is the y-value of the Ith data point.
C Iery    I    Errors,  <0 statistical,  =0 none,  >0 explicit errors.
C Mxrow   I    Used by function WEIGHT (needed for explicit errors)
C Low     I    The first data point in the fit range (typically=1).
C Npts    I    The last data point in the fit range.
C Xmin    I    The smallest X to include in fit.
C Xmax    I    The largest  X to include in fit.
C Ichat   I    >0 display messages and questions,  =0 display messages
C              -but no questions,  -1 no messages or questions
C              -2 total quiet
C Niter   I    Number of iterations (0 defaults to 10)
C Istat   I    The statistic to be used: 0=Chi-squared,  1=M-L
C Icomp   I    Passed to FITLIM,  FNFIT and MDERIV.
C Idoun   I    =1 estimate uncertainties, =0 don't estimate uncertainties.
C Nterm   I    The number of terms.
C Fdelmn  I    Stop when delta less than (0 defaults to 0.05 or 0.001 2D)
C Pval    I/O  The whole reason for calling CURFIT.
C Plim    I/O  PLIM(1, I)<0 indicates parameter is frozen.
C Chisq     O  The value of Chi-squared. -N insensitive to parameter N.
C---
C 1998-09-30 - Changed the ML statistic to the current version used
C              by xspec (the famous Castor priv comm function).  Multiplied
C              by Rdat*Weight to convert to rates to counts.  Finally
C              do not allow the model to fall below 1.0E-6 counts/bin.
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXDIM, MXPAR, MXFREE
      PARAMETER (MXDIM=2, MXPAR=120, MXFREE=100)
      REAL      FNFIT, WEIGHT
C
      REAL      xt(MXDIM)
      REAL      chisq1, chil, del, delmin, derj, dif, flamda, fudge
      REAL      rcon, rlndat, rlnmod, tot, wtmp, rmod
      INTEGER   idef
      SAVE      idef
      INTEGER   i, ians, icnt, ihit, iycol, iyi, iyoff
      INTEGER   j, ji, k, ndim, nidim, ntry, nvar
C Length of CURFIT common block is 3*MXFREE*(MXFREE+2).
      INTEGER   Index, Ik, Jk
      REAL      Beta, B, Deriv, Alpha
      DOUBLE PRECISION Aray
      COMMON /CURCMN/ Index(MXFREE), Ik(MXFREE), Jk(MXFREE),
     &                Beta(MXFREE), B(MXFREE), Deriv(MXPAR),
     &                Alpha(MXFREE, MXFREE), Aray(MXFREE, MXFREE)
      DATA idef/ -1/
C---
      IF ( Fdelmn.GT.0.0 ) THEN
         delmin=Fdelmn
      ELSE
         delmin = 0.05
         CALL PLTXCC(Rdat, 1, Igrp, xt, nidim, iyoff)
         IF ( nidim.GT.1 ) delmin=0.001
      END IF
      ntry = 10
      IF ( Niter.NE.0 ) ntry = Niter
      flamda = .001
      Chisq = 0.
      icnt = 0
      nvar = 0
      DO j = 1, Nterm
         IF ( Plim(1,j).GE.0. ) THEN
            nvar = nvar + 1
            IF ( nvar.GT.MXFREE ) THEN
               WRITE(*, 101) MXFREE
 101           FORMAT(' CURFIT--Can only vary', I6,
     &              ' parameters at a time.')
               RETURN
            END IF
            Index(nvar) = j
         END IF
      END DO
C- Clear BETA and ALPHA.
 120  CONTINUE
      chil = Chisq
      DO j = 1, nvar
         Beta(j) = 0.
         DO k = 1, j
            Alpha(j, k) = 0.
         END DO
      END DO
C- Evaluate derivatives,  CHISQ,  ALPHA and BETA at starting point.
      chisq1 = 0.
      rcon = 1.0
      DO 160 i = Low, Npts
         CALL PLTXCC(Rdat, i, Igrp, xt, ndim, iyoff)
         IF ( xt(1).EQ.NO ) GOTO 160
         IF ( xt(1).LT.XMIN(1) .OR. xt(1).GT.XMAX(1) ) GOTO 160
         IF ( nidim.GT.1 ) THEN
            IF ( xt(2).LT.XMIN(2) .OR. xt(2).GT.XMAX(2) ) GOTO 160
         END IF
         iyi = iy0 + iyoff
         rmod = FNFIT(xt, Icomp, Pval, Nterm)
         IF ( rmod.EQ.NO .OR. Rdat(iyi).EQ.NO ) GOTO 160
         wtmp = WEIGHT(Rdat(iyi), Mxrow, Iery)
         IF ( Istat.EQ.0 ) THEN
            dif = Rdat(iyi) - rmod
            IF ( ABS(dif).LT.1.E15 ) THEN
               chisq1 = chisq1 + wtmp*dif*dif
            ELSE
               chisq1 = chisq1 + 1.E32
            END IF
         ELSE
C rcon is the divisor that converted counts to flux.  For Iery.LE.0
C rcon=1.0 which is set outside the loop.
            IF ( Rdat(iyi).EQ.0.0 ) THEN
               IF ( Iery.GT.0 ) rcon = SQRT(wtmp)
               rlndat = 0.0
            ELSE
               IF ( Iery.GT.0 ) rcon = wtmp*Rdat(iyi)
               rlndat = LOG(rcon*Rdat(iyi))
            END IF
C Assume exact value of the model becomes useless below 1.0E-6 count/bin.
            rmod = MAX(rmod, 1.0E-6/rcon)
            dif = Rdat(iyi)/rmod
            rlnmod = LOG(rcon*rmod)
C            chisq1 = chisq1 + 2*((rcon*rmod+
C     &        GAMMLN(rcon*Rdat(iyi)+1))-rcon*Rdat(iyi)*rlnmod)
            chisq1 = chisq1 + 2*rcon*(rmod+
     &          Rdat(iyi)*(rlndat-rlnmod-1.0))
         END IF
         IF ( nvar.LE.0 ) GOTO 160
         CALL MDERIV(xt, Icomp, Pval, Plim, Nterm, Deriv)
         DO j = 1, nvar
            ji = Index(j)
            IF ( Istat.EQ.0 ) THEN
               Beta(j) = Beta(j) + wtmp*dif*Deriv(ji)
               derj = wtmp*Deriv(ji)
            ELSE
               Beta(j) = Beta(j) + rcon*(dif-1.)*Deriv(ji)
               derj = rcon*dif*Deriv(ji)/rmod
            END IF
            DO k = 1, j
               Alpha(j, k) = Alpha(j, k) + derj*Deriv(Index(k))
            END DO
         END DO
 160  CONTINUE
      IF ( nvar.LE.0 ) GOTO 900
C- Symmetrize ALPHA.
      DO j = 1, nvar
         IF ( Alpha(j, j).EQ.0. ) GOTO 810
         DO k = 1, j
            Alpha(k, j) = Alpha(j, k)
         END DO
      END DO
      DO j = 1, Nterm
         B(j) = Pval(j)
      END DO
C- Modify and invert the curvature matrix.
 190  CONTINUE
      DO j = 1, nvar
         DO k = 1, nvar
            Aray(j, k) = Alpha(j, k)/SQRT(Alpha(j, j)*Alpha(k, k))
         END DO
         Aray(j, j) = 1. + flamda
      END DO
      CALL MATINV(Aray, MXFREE, nvar, Ik, Jk)
C- Construct the trial parameter set.
      DO j = 1, nvar
         tot = 0.
         DO k = 1, nvar
            tot = tot + Beta(k)*Aray(j, k)/SQRT(Alpha(j, j)*Alpha(k, k))
         END DO
         Pval(Index(j)) = B(Index(j)) + tot
      END DO
      CALL FITLIM(Icomp, Pval, Plim, Nterm, ihit)
C---
C- Compute new CHISQ.
      Chisq = 0.
      rcon = 1.0
      DO 280 i = Low, Npts
         CALL PLTXCC(Rdat, i, igrp, xt, ndim, iyoff)
         IF ( xt(1).EQ.NO ) GOTO 280
         IF ( xt(1).LT.XMIN(1) .OR. xt(1).GT.XMAX(1) ) GOTO 280
         IF ( nidim.GT.1 ) THEN
            IF ( xt(2).LT.XMIN(2) .OR. xt(2).GT.XMAX(2) ) GOTO 280
         END IF
         iyi = iy0 + iyoff
         rmod = FNFIT(xt, Icomp, Pval, Nterm)
         IF ( rmod.EQ.NO .OR. Rdat(iyi).EQ.NO ) GOTO 280
         wtmp = WEIGHT(Rdat(iyi), Mxrow, Iery)
         IF ( Istat.EQ.0 ) THEN
            dif = MIN(ABS(Rdat(iyi)-rmod), 1.E15)
            Chisq = Chisq + wtmp*dif*dif
         ELSE
            IF ( Rdat(iyi).EQ.0.0 ) THEN
               IF ( Iery.GT.0 ) rcon = SQRT(wtmp)
               rlndat = 0.0
            ELSE
               IF ( Iery.GT.0 ) rcon = wtmp*Rdat(iyi)
               rlndat = LOG(rcon*Rdat(iyi))
            END IF
            rmod = MAX(rmod, 1.0E-6/rcon)
            rlnmod = LOG(rcon*rmod)
            Chisq = Chisq + 2*rcon*(rmod+
     &          Rdat(iyi)*(rlndat-rlnmod-1.0))
         END IF
 280  CONTINUE
C- If CHI SQUARED increased,  increase FLAMDA and try again.
      IF ( Chisq.GT.chisq1 ) THEN
         IF ( flamda.LT.0.9E10 ) THEN
            flamda = 10.*flamda
            GOTO 190
         END IF
C- Trick to force termination condition.
         chil = Chisq
      END IF
C---
      IF ( Ichat.GE.0 ) WRITE (*, 301) NINT(LOG10(flamda)), Chisq
 301  FORMAT (' (', I3, ')   W-VAR=', 1PG10.4)
C-
      IF ( flamda.GT.1.E-20 ) flamda = flamda/10.
      icnt = icnt + 1
      del = ABS(chil-Chisq)
C- Unweighted,  consider relative change.
      IF ( Iery.EQ.0 .AND. Chisq.GT.0. ) del = del/Chisq
      IF ( Chisq.LE.0. .OR. del.LE.delmin ) GOTO 330
C- Keep trying until ICNT.GE.NTRY.
      IF ( icnt.LT.ntry ) GOTO 120
      icnt = 0
      IF ( Ichat.GT.0 ) THEN
         CALL YORN('Continue fitting', idef, ians)
         IF ( ians.GT.0 ) GOTO 120
      END IF
      WRITE (*, 321)
 321  FORMAT (' CURFIT--Minimum not found.')
C---
C- Exit.  Evaluate uncertainties by inverting matrix with FLAMDA=0.
 330  CONTINUE
      IF ( Idoun.NE.0 ) THEN
         DO j = 1, nvar
            DO k = 1, nvar
               Aray(j, k) = Alpha(j, k)
            END DO
         END DO
         CALL MATINV(Aray, MXFREE, nvar, Ik, Jk)
         fudge = 1.0
         IF ( Iery.EQ.0 .AND. Istat.EQ.0) THEN
C For unweighted chisq, scale the uncertainty.
            IF ( Chisq.GT.0. ) fudge=(Pval(nterm+2)-nvar)/Chisq
         END IF
         DO j = 1, nvar
            ji = Index(j)
            Plim(1, ji) = SQRT(ABS(Aray(j, j))/fudge)
         END DO
      END IF
      RETURN
C---
 810  CONTINUE
      IF ( ichat.GT.-2 ) THEN
         WRITE(*, 811) Index(j)
 811     FORMAT(' CURFIT--Model insensitive to parameter', I6)
      END IF
      Chisq = -Index(j)
      RETURN
C---
 900  CONTINUE
      Chisq = chisq1
      IF ( Ichat.GE.0 ) WRITE (*, 301) NINT(LOG10(flamda)), Chisq
      RETURN
      END
C*********
      SUBROUTINE MATINV(Aray, Mxfree, Norder, Ik, Jk)
      INTEGER   Mxfree, Norder, Ik(Mxfree), Jk(Mxfree)
      DOUBLE PRECISION Aray(Mxfree, Mxfree)
C---
C This subroutine replaces a real,  square,  symmetric matrix with
C its inverse see P.R.Bevington,  "Data Reduction and Error Analysis
C For the Physical Sciences"  McGraw-Hill,  N.Y.,  1969 p.302.
C---
C Aray    I/O
C Mxfree  I
C Norder  I
C Ik        O
C Jk        O
C---
      DOUBLE PRECISION amax, save
      INTEGER   i, j, k, l
C---
      DO k = 1, Norder
C- Find largest element in matrix.
         amax = 0.
         DO 150 i = k, Norder
            DO j = k, Norder
C               IF ( DABS(amax)-DABS(Aray(i, j)) ) 130, 130, 150
               IF ( DABS(amax)-DABS(Aray(i, j)).GT.0.0 ) GOTO 150
 130           amax = Aray(i, j)
               Ik(k) = i
               Jk(k) = j
            END DO
 150     CONTINUE
C- Interchange rows and columns to put AMAX in ARAY(K, K)
C         IF ( amax ) 180, 420, 180
         IF ( amax.EQ.0.0 ) GOTO 420
 180     i = Ik(k)
C         IF ( i-k ) 210, 210, 190
         IF ( i-k.LE.0 ) GOTO 210
 190     CONTINUE
         DO j = 1, Norder
            save = Aray(k, j)
            Aray(k, j) = Aray(i, j)
            Aray(i, j) = -save
         END DO
 210     j = Jk(k)
C         IF ( j-k ) 240, 240, 220
         IF ( j-k.LE.0 ) GOTO 240
 220     CONTINUE
         DO i = 1, Norder
            save = Aray(i, k)
            Aray(i, k) = Aray(i, j)
            Aray(i, j) = -save
         END DO
C- Accumulate the elements of the inverse martix.
 240     CONTINUE
         DO i = 1, Norder
            IF ( i.NE.k ) Aray(i, k) = -Aray(i, k)/amax
         END DO
         DO i = 1, Norder
            DO j = 1, Norder
               IF ( i.NE.k .AND. j.NE.k ) Aray(i, j) = Aray(i, j)
     &              + Aray(i, k)*Aray(k, j)
            END DO
         END DO
         DO j = 1, Norder
            IF ( j.NE.k ) Aray(k, j) = Aray(k, j)/amax
         END DO
         Aray(k, k) = 1./amax
      END DO
C---
C- Restore ordering of the matrix.
      DO 410 l = 1, Norder
         k = Norder - l + 1
         j = Ik(k)
C         IF ( j-k ) 380, 380, 360
         IF ( j-k.LE.0 ) GOTO 380
 360     CONTINUE
         DO i = 1, Norder
            save = Aray(i, k)
            Aray(i, k) = -Aray(i, j)
            Aray(i, j) = save
         END DO
 380     CONTINUE
         i = Jk(k)
C         IF ( i-k ) 410, 410, 390
         IF ( i-k.LE.0 ) GOTO 410
 390     CONTINUE
         DO j = 1, Norder
            save = Aray(k, j)
            Aray(k, j) = -Aray(i, j)
            Aray(i, j) = save
         END DO
 410  CONTINUE
 420  RETURN
      END
