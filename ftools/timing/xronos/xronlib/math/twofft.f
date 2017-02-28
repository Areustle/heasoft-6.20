      SUBROUTINE TWOFFT(DATA1,DATA2,FFT1,FFT2,N)
c      DIMENSION DATA1(N),DATA2(N)
      INTEGER*4 n
      INTEGER*4 n2,j
      REAL*4  data1(n), data2(n)
      COMPLEX FFT1(N),FFT2(N),H1,H2,C1,C2
      C1=CMPLX(0.5,0.0)
      C2=CMPLX(0.0,-0.5)
      DO 11 J=1,N
        FFT1(J)=CMPLX(DATA1(J),DATA2(J))
11    CONTINUE
      CALL FOUR1(FFT1,N,1)
      FFT2(1)=CMPLX(AIMAG(FFT1(1)),0.0)
      FFT1(1)=CMPLX(REAL(FFT1(1)),0.0)
      N2=N+2
      DO 12 J=2,N/2+1
        H1=C1*(FFT1(J)+CONJG(FFT1(N2-J)))
        H2=C2*(FFT1(J)-CONJG(FFT1(N2-J)))
        FFT1(J)=H1
        FFT1(N2-J)=CONJG(H1)
        FFT2(J)=H2
        FFT2(N2-J)=CONJG(H2)
12    CONTINUE
      RETURN
      END
