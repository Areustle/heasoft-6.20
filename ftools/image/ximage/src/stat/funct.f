      FUNCTION FUNCT(I,Sig)
      REAL*4 Sig , FUNCT
c     +,CONVOLUTION KERNEL
c     Used in gaussian smoothing
C     GAUSSIAN KERNEL
      INTEGER*4 I
      FUNCT = EXP(-0.5*FLOAT(I*I)/Sig/Sig)
      RETURN
      END
