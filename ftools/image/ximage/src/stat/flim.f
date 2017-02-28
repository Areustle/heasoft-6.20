      FUNCTION FLIM(X,Arycon)
c
c  Used as argument to zlim to determine upper limit
c
c  I  X       (r) x of function
c  I  Arycon  (r) function constants (Cntsrc,Cntbg,Conflev)
c
      real*4 X, Arycon(3)
c
c  Local variables
c
      REAL*8 ffff
      real*4 amea, cntsrc, cntbg, conflev, cl1
      REAL*4 XPOLOG , FLIM
      INTEGER*4 mmme
c
      cntsrc = Arycon(1)
      mmme = INT(cntsrc)
      cntbg = Arycon(2)
      amea = cntbg + X
      conflev = Arycon(3)
      cl1 = 1.0 - conflev
      ffff = 1. - XPOLOG(mmme,amea,1) + XPOLOG(mmme,amea,2)
      FLIM = ffff - cl1
c
      RETURN
      END
