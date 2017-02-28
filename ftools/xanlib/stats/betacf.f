c
      real*4 FUNCTION betacf(a,b,x)
      integer*4 itmax
      real*4 eps, am, bm, az, qab, qap, qam, bz, em, ap, bp
      real*4 a, b, d, x, app, aold, tem, bpp
      integer*4 m
      PARAMETER (itmax=100,eps=3.E-7)
      am = 1.
      bm = 1.
      az = 1.
      qab = a + b
      qap = a + 1.
      qam = a - 1.
      bz = 1. - qab*x/qap
      DO m = 1, itmax
        em = m
        tem = em + em
        d = em*(b-m)*x/((qam+tem)*(a+tem))
        ap = az + d*am
        bp = bz + d*bm
        d = -(a+em)*(qab+em)*x/((a+tem)*(qap+tem))
        app = ap + d*az
        bpp = bp + d*bz
        aold = az
        am = ap/bpp
        bm = bp/bpp
        az = app/bpp
        bz = 1.
        IF ( abs(az-aold).LT.eps*abs(az) ) GO TO 100
      END DO
      PAUSE 'A or B too big, or ITMAX too small'
 100  CONTINUE
      betacf = az
      RETURN
      END
