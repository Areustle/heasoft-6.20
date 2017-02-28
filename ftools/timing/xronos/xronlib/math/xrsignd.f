c
      INTEGER FUNCTION xrsignd(x)
c
c  ls 19/7/88 function to return sign of x (+1 if x>0, -1 if x<0, 0 if x=0)
c
c   I   x = real*8 variable
c
      implicit none
      REAL*8 x,xsign
      xrsignd = 0
      IF (x.LT.0.D0) xrsignd = -1
      IF (x.GT.0.D0) xrsignd = +1
      RETURN
      END
c
c
c
