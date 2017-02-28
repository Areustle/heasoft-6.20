      FUNCTION wavelet(I,a)
      REAL*4 a, wavelet 
c     +,CONVOLUTION KERNEL 
c     Used in wavelet smoothing
C     symmetric wavelet KERNEL 
      integer*4 i
      wavelet=max( (2.-float(I*I)/a/a)*EXP(-0.5*FLOAT(I*I)/a/a), 0.)
      RETURN
      END 
