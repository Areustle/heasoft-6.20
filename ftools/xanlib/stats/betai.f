c
      real*4 FUNCTION betai(a,b,x)
      real*4 a, b, x, bt, betacf, gamlin

      IF ( x.LT.0. .OR. x.GT.1. ) PAUSE 'bad argument X in BETAI'
      IF ( x.EQ.0. .OR. x.EQ.1. ) THEN
        bt = 0.
      ELSE
        bt = exp(gamlin(a+b)-gamlin(a)-gamlin(b)+a*alog(x)+b*alog(1.-x))
      END IF
      IF ( x.LT.(a+1.)/(a+b+2.) ) THEN
        betai = bt*betacf(a,b,x)/a
        RETURN
      ELSE
        betai = 1. - bt*betacf(b,a,1.-x)/b
        RETURN
      END IF
      END
