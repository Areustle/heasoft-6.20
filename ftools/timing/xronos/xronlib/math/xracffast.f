c
      subroutine xracffast (cofft,data,n)
c
c LS 12/6/90  autocorrelation function with fast Fourier transform algorithm
c             (inspired to numerical recipes routine "correl") 
c Rev.1 12/5/93 LS : resent unused 2nd half of array to avoid problems in acf
c
c   W     cofft = work area
c   I/O   data = input series/ return acf
c   I     n = no. of data points x2 (must be a power of 2)
c             (i.e. data are contained from 1 to n/2)
c         NOTE that second half of array must be set=0 to avoid problems !! 
c
      INTEGER*4 i,no2,in
      INTEGER*4 n
      REAL*4 data(n)
      COMPLEX cofft(n)
c      DIMENSION data(n)
c
c  Rev.1 start reset 2nd half of array to avoid problems 
      do i=n/2+1,n
        data(i) = 0.
      end do
c  Rev.1 stop
c
      do i=1,n
        cofft(i)=cmplx(data(i),0.)
      end do
      call four1 (cofft,n,+1) 
      NO2=N/2
      DO 11 I=1,NO2+1
        cofft(I)=cofft(I)*CONJG(cofft(I))/float(NO2)
11    CONTINUE
      cofft(1)=CMPLX(REAL(cofft(1)),REAL(cofft(N/2+1)))
      CALL REALFT(cofft,N/2,-1)
      data(1)=real(cofft(1))
      do i=1,n/4
        in=n/2-i+1
        data(2*i+1)= real(cofft(in))
        data(2*i)  =aimag(cofft(in))
      end do
      RETURN
      END
c
