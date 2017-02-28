 
C
      SUBROUTINE xrREGR(Y, sy, n1, n2, NTERMS, M, MODE, A0, A)
c
c ls 17/9/88  polynomial regression routine
c             (adapted from hp1000 version and from bevington)
c
c  I     y = values for time series (<-1.1e34 for gaps)
c  I     sy = errors on time series
c  I     n1 = index of first good point in series
c  I     n2 = index of last good point in series
c  I     nterms = no. of coefficients (excluding d.c. level)
c  I     m = array of powers used in the regression
c  I     mode = 0 for no weigths, -1 for weight with inverse variance
c  O     a0 = d.c. level
c  O     a = array of coefficients
c
C Dimension statement valid for NTERMS up to 10
c
c  xrporegr = function used
c  xrmminv = subr used
C
c      DIMENSION Y(*), sy(*)
C     EMA Y(5000)
c      DOUBLEPRECISION ARRAY, SUM, YMEAN, SIGMA, XMEAN, SIGMAX, FREE1
c      DIMENSION M(*), A(*), R(10)
c      DIMENSION XMEAN(10), SIGMAX(10), ARRAY(10, 10)
      REAL*8 xmean(10), sigmax(10), free1
      REAL*8 array(10, 10), sum, ymean, sigma
      REAL*4 y(*), sy(*), xrporegr, a0, a(*), r(10), det, sigma0
      REAL*4 wi, btime, fnpts, wmean, yi, pxijm
      INTEGER*4 n1, n2, m(*), mode, npts, j, nterms, k, inew
c dummy binning time
      DATA btime/1./
C
C initialize arrays
      npts = 0
      SUM = 0.D0
      YMEAN = 0.D0
      SIGMA = 0.D0
      DO J = 1, NTERMS
         XMEAN(J) = 0.D0
         SIGMAX(J) = 0.D0
         A(J) = 0.
         R(J) = 0.
         DO K = 1, NTERMS
            ARRAY(J, K) = 0.D0
30002    ENDDO
30001 ENDDO
C
      DO Inew = n1, n2
         IF (y(inew).GT.-1.1E34) THEN
            npts = npts + 1
            WI = 1.
            IF (MODE.LT.0 .AND. sY(INEW).NE.0.) WI = 1./sy(inew)**2
c         IF(MODE.LT.0.AND.Y(INEW).NE.0.) WI=1./(ABS(Y(INEW)+BK*IFBK))
            SUM = SUM + WI
            YMEAN = YMEAN + WI*Y(INEW)
            DO J = 1, NTERMS
               XMEAN(J) = XMEAN(J) + WI*xrporegr(BTIME, INEW, J, M)
30004       ENDDO
         ENDIF
30003 ENDDO
      YMEAN = YMEAN/SUM
C
      DO J = 1, NTERMS
         XMEAN(J) = XMEAN(J)/SUM
30005 ENDDO
      FNPTS = NPTS
      WMEAN = SUM/FNPTS
C
C
      DO Inew = n1, n2
         IF (y(inew).GT.-1.1E34) THEN
            WI = 1./WMEAN
c         IF(MODE.LT.0.AND.Y(INEW).NE.0.)WI=1./(ABS(Y(INEW)+BK*IFBK)
            IF (MODE.LT.0 .AND. sY(INEW).NE.0.)
     &           WI = 1./(sy(inew)**2*WMEAN)
            YI = Y(INEW)
            SIGMA = SIGMA + WI*(YI-YMEAN)**2
            DO J = 1, NTERMS
               PXIJM = xrporegr(BTIME, INEW, J, M)
               SIGMAX(J) = SIGMAX(J) + WI*(PXIJM-XMEAN(J))**2
               R(J) = R(J) + WI*(PXIJM-XMEAN(J))*(YI-YMEAN)
               DO K = 1, J
                  ARRAY(J, K) = ARRAY(J, K) + WI*(PXIJM-XMEAN(J))
     &                          *(xrporegr(BTIME,INEW,K,M)-XMEAN(K))
30009          ENDDO
30008       ENDDO
         ENDIF
30007 ENDDO
C
      FREE1 = NPTS - 1
      SIGMA = DSQRT(SIGMA/FREE1)
C
      DO J = 1, NTERMS
         SIGMAX(J) = DSQRT(SIGMAX(J)/FREE1)
         R(J) = R(J)/(FREE1*SIGMAX(J)*SIGMA)
         DO K = 1, J
            ARRAY(J, K) = ARRAY(J, K)/(FREE1*SIGMAX(J)*SIGMAX(K))
            ARRAY(K, J) = ARRAY(J, K)
30011    ENDDO
30010 ENDDO
C
C
      CALL xrMMINV(ARRAY, NTERMS, DET)
      IF (DET) 101, 91, 101
 91   A0 = 0.
      SIGMA0 = 0.
      GOTO 150
 101  CONTINUE
      A0 = YMEAN
      DO J = 1, NTERMS
         DO K = 1, NTERMS
            A(J) = A(J) + R(K)*ARRAY(J, K)
30013    ENDDO
         A(J) = A(J)*SIGMA/SIGMAX(J)
         A0 = A0 - A(J)*XMEAN(J)
30012 ENDDO
 150  RETURN
      END
C
C
C
