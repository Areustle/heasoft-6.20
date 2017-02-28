c
      SUBROUTINE xrccffast(data1, data2, n, fft, ans, yp)
c
c LS 13/6/90 fast cross correlation (inspired to "correl" of numrical recip.)
c
c  I   data1 = data of series 1
c  I   data2 = data of series 2
c  I   n = 2 x no. of data points (must be a power of 2)
c  W   fft = complex work area
c  W   ans = complex work area
c  O   yp  = return ccf
c
c      SUBROUTINE CORREL(DATA1,DATA2,N,ANS)
c      PARAMETER(NMAX=8192)
c      DIMENSION DATA1(N),DATA2(N),yp(n/2)
c      COMPLEX FFT(NMAX),ANS(N)
      INTEGER*4 N, no2, i, in
      REAL*4 data1(1), data2(1), yp(n/2)
      COMPLEX FFT(n), ANS(N)
      CALL TWOFFT(DATA1, DATA2, FFT, ANS, N)
      NO2 = FLOAT(N)/2.0
      DO 11 I = 1, N/2 + 1
         ANS(I) = FFT(I)*CONJG(ANS(I))/NO2
 11   CONTINUE
      ANS(1) = CMPLX(REAL(ANS(1)), REAL(ANS(N/2+1)))
      CALL REALFT(ANS, N/2, -1)
c
c Disentangle wraparound order
c
      DO i = 1, n/4
         in = n/4 - i + 1
         yp(2*i) = real(ans(in))
         yp(2*i-1) = aimag(ans(in))
      ENDDO
c
      DO i = n/4 + 1, n/2
         in = n/2 - i + n/4 + 1
         yp(2*i) = real(ans(in))
         yp(2*i-1) = aimag(ans(in))
      ENDDO
c
      RETURN
      END
