c
      REAL*4 FUNCTION xrchithr(n, x)
c
c  ls 4/12/89 function to calculate the chisq. (threshold) value for a given
c                     probability x
c
c  I   n = no. of deg. of freedom for the chisq distr.
c  I   x = probability
c
c function used : gammq (see numerical recipes)
c

c Rev.1 el  01/94  to change gammq to double precision.
      INTEGER*4 k, n, iflag
      REAL*4 rv, x, prec, sdev, rmin, rmax, prob
      REAL*8  gammq
c
c precision
      DATA prec/1.E-2/

c Flag value to indicate error
      xrchithr = -1000.
c ---
c
c error conditions
      IF (n.EQ.0 .OR. x.GT.1. .OR. x.LT.1.E-37) THEN
         WRITE (*, *) ' xrchithr> error in input parameters '
         RETURN
      ENDIF
c
c to be used in gammq call
      rv = float(n)/2.
c stand. dev. of chisq distr.
      sdev = sqrt(float(2*n))
c
c set start values of min and max (0.)
      rmin = 0.
c 2 sigma
      rmax = 2.*sdev
      k = 0
      iflag = 0
c
 1    xrchithr = (rmin+rmax)/2.
      k = k + 1
c Rev.1:
c      prob = gammq(rv, xrchithr/2.)
      prob = gammq(dble(rv), dble(xrchithr/2.))
c      write(*,*)' k, rmin, rmax ,prob',  k, rmin, rmax ,prob      !!!!!!!
c if precision is achieved
      IF (abs(prob-x)/x.LE.prec) RETURN
c
c limit to no. of iterations
      IF (k.EQ.100) THEN
         WRITE (*, *) ' xrchithr> no convergence after 100 iterations '
         RETURN
      ENDIF
c
      IF (prob.GT.x) THEN
c increse rmax to find upper extreme
         IF (iflag.EQ.0.) THEN
            rmax = rmax*2.
         ELSE
            rmin = xrchithr
         ENDIF
      ELSE
c means upper extreme has been found
         iflag = 1
         rmax = xrchithr
      ENDIF
      GOTO 1
      END
c
c
c
