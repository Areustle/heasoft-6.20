      SUBROUTINE FITIT(Y, Lery, Mxrow, Igroup,
     :         Iyoff0, Npts, Xmin, Xmax, Isgood, Iegood,
     :         Niter, Istat, Icomp, Par, Plim, Nterm, Chi)
      REAL      Y(*), Xmin(*), Xmax(*), Par(*), Plim(3,*), Chi
      INTEGER   Lery, Mxrow, Igroup
      INTEGER   Iyoff0, Npts, Isgood, Iegood
      INTEGER   Niter, Icomp(*), Nterm, Istat
C---
C Calculate the number of points in the range, set Isgood and Iegood
C to be first and last points in the range.  Report number of points
C to user and then call CURFIT.
C---
C Y
C Lery
C Mxrow
C Igroup
C Iyoff0
C Npts
C Xmin    I    The smallest X to include in fit.
C Xmax    I    The largest  X to include in fit.
C Isgood    O   The smallest index included in fit.
C Iegood    O   The largest  index included in fit.
C Niter
C Istat
C Icomp
C Par
C Plim
C Nterm
C Chi
C---
C 1990-Mar-06 - Extracted from FIT [AFT]
C---
      INTEGER   MXDIM
      PARAMETER (MXDIM=2)
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      REAL      WEIGHT
C
      REAL      xt(MXDIM)
      REAL      wtmp
      INTEGER   iyoff
      INTEGER   I, iyi, K, ndim, nfit, nidim
C---
C The following code finds the first and last points in the range that
C are to be included in the fit.  This greatly speeds up the fitting
C of short segments of data.
      nfit=0
      Isgood=0
      Iegood=0
      CALL PLTXCC(Y, 1, igroup, xt, nidim, iyoff)
      DO 190 I=1,Npts
         CALL PLTXCC(Y, i, igroup, xt, ndim, iyoff)
         IF ( xt(1).EQ.NO ) GOTO 190
         IF ( xt(1).LT.Xmin(1) .OR. xt(1).GT.Xmax(1) ) GOTO 190
         IF ( nidim.GT.1 ) THEN
            IF ( xt(2).LT.Xmin(2) .OR. xt(2).GT.Xmax(2) ) GOTO 190
         END IF
         iyi = iyoff0 + iyoff
         wtmp = WEIGHT(Y(iyi), Mxrow, Lery)
         IF ( wtmp.LE.0. ) GOTO 190
         nfit=nfit+1
         IF ( Isgood.EQ.0 ) Isgood=I
         Iegood=I
  190 CONTINUE
      IF(nfit.LE.0) THEN
         WRITE(*,231)
  231    FORMAT('FITIT--No points in allowed range.')
         RETURN
      END IF
      WRITE(*,251) nfit,Iegood-Isgood+1
  251 FORMAT(' Fitting',I8,' points in a band of',I8,'.')
      Par(Nterm+2)=nfit
      WRITE(*,*) (Par(K),K=1,Nterm)
C---
C Blast off
      CALL CURFIT(Y, Igroup, Iyoff0, Lery, Mxrow, Isgood, Iegood,
     :   Xmin, Xmax, 1, Niter, Istat, Icomp, 1, Nterm,
     &   0.0, Par, Plim, Chi)
C---
      Par(Nterm+1)=Chi
      WRITE(*,*) (Par(K),K=1,Nterm)
      RETURN
      END
