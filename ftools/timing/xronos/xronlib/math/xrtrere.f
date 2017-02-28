c
      SUBROUTINE xrtrere(y, sy, n1, n2, itrend, imod, iwei)
c
c  ls 11/7/88 to remove polynomial trend
c
c     I/R  y = time series to be detrended (return regulated by imod)
c               (data gaps are identified by y<-1.1e34)
c     I/R  sy  = errors on time series above
c     I    n1 = index of first valid point in series
c     I    n2 = index of last  valid point in series
c     I    itrend = 1,2,3,4 = order of polynomial trend to be removed
c     I    imod = 1 to return series detrended by subtraction,
c               = 2 to return series detrended by division
c               = 3 to return fitted trend
c               (use corresponding negative values not to
c                renormalise the avg. to the value before trend removal)
c     I    iwei = mode for weights on errors (not used yet)(please set=0)
c
c Assumes that x (=times) are equispaced (except for gaps)
c (N.B.: average should not be subtracted from series before calling this)
c
c   xrporegr = function used
c   xrregr   = subroutine used
c
      INTEGER*4 imod, itrend, n1, n2, nd, k, n, iwei, m(4)
      REAL*4 y(*), sy(*), xrporegr, a(4), xtemp, avg, btime,
     &       a0
c
c dummy binning time
      DATA btime/1./
c set array of powers used in regression
      DATA m/1, 2, 3, 4/
c
      IF (itrend.LE.0 .OR. itrend.GE.5) RETURN
c
c set coefficients to zero
      a0 = 0.
      DO k = 1, 4
         a(k) = 0.
      ENDDO
c
c evaluate data average
      avg = 0.
      nd = 0
      DO n = n1, n2
         IF (y(n).GT.-1.1E34) THEN
            avg = avg + y(n)
            nd = nd + 1
         ENDIF
      ENDDO
c if no good data
      IF (nd.EQ.0) RETURN
      avg = avg/float(nd)
c
c evaluate coefficients of polynomia with regression routine from Bevington
c
      CALL xrregr(y, sy, n1, n2, itrend, m, iwei, a0, a)
c
c remove trend
c
      IF (imod.EQ.-1) avg = 0.
      IF (imod.EQ.-2) avg = 1.
      DO n = n1, n2
c check that point is not a gap
         IF (y(n).GT.-1.1E34) THEN
            xtemp = a0
c         calculate value of least square apporx. at point xn
            DO k = 1, itrend
               xtemp = xtemp + a(k)*xrporegr(btime, n, k, m)
            ENDDO
c         remove trend
c subtract trend (data only)
            IF (iabs(imod).EQ.1) y(n) = y(n) - xtemp + avg
            IF (iabs(imod).EQ.2) THEN
c divide data by trend
               y(n) = y(n)/xtemp*avg
c divide errors by trend
               sy(n) = sy(n)/xtemp*avg
            ENDIF
            IF (iabs(imod).EQ.3) THEN
c replace data with trend
               y(n) = xtemp
c errors=0
c            sy(n)=0.
c errors = trend/1000
               sy(n) = xtemp/1000.
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
c
c
c
