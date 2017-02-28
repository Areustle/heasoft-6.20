      SUBROUTINE pearsn(x,y,n,r,prob,z)
      real*4 r, prob, z, tiny, df, t, betai, erfcc
      real*4 ax, ay, xt, yt, sxx, syy, sxy
      integer*4 j, n
      PARAMETER (tiny=1.E-20)
      real*4 x(n), y(n)
      ax = 0.
      ay = 0.
      DO j = 1, n
        ax = ax + x(j)
        ay = ay + y(j)
      END DO
      ax = ax/n
      ay = ay/n
      sxx = 0.
      syy = 0.
      sxy = 0.
      DO j = 1, n
        xt = x(j) - ax
        yt = y(j) - ay
        sxx = sxx + xt**2
        syy = syy + yt**2
        sxy = sxy + xt*yt
      END DO
      r = sxy/sqrt(sxx*syy)
      z = 0.5*alog(((1.+r)+tiny)/((1.-r)+tiny))
      df = n - 2
      t = r*sqrt(df/(((1.-r)+tiny)*((1.+r)+tiny)))
      IF ( n.LE.50 ) THEN
        prob = betai(0.5*df,0.5,df/(df+t**2))
      ELSE
        prob = erfcc(abs(z*sqrt(n-1.))/1.4142136)
      END IF
      RETURN
      END
