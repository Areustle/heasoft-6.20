**==gamma.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION RS_GAMMA(A,X)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL A , fn , fn1 , fn2 , RS_GAMMA , X
      INTEGER in , mn , n , nn
C*** End of declarations inserted by SPAG
      DOUBLE PRECISION can1 , can2 , cbn1 , cbn2 , an , bn , can , cbn
C     EXP(-X) * X ** A   TAKEN OUTSIDE
      can2 = 1.
      can1 = 0.
      cbn2 = 0.
      cbn1 = 1.
      an = 1.
      bn = X
      n = 1
      can = bn*can1 + an*can2
      cbn = bn*cbn1 + an*cbn2
      fn1 = SNGL(can/cbn)
      DO 100 n = 2 , 1000
         IF ( can.GT.1.E30 ) THEN
            can1 = can1*1.E-30
            can = can*1.E-30
            cbn1 = cbn1*1.E-30
            cbn = cbn*1.E-30
         ENDIF
         in = n
         can2 = can1
         cbn2 = cbn1
         can1 = can
         cbn1 = cbn
         mn = MOD(n,2)
         bn = mn*X + (1.-mn)
         nn = n/2
         an = nn - (1.-mn)*A
         can = bn*can1 + an*can2
         cbn = bn*cbn1 + an*cbn2
         fn = SNGL(can/cbn)
         IF ( ABS((fn-fn1)/fn1).LE..0001 .AND. n.GE.20 ) GOTO 200
         fn2 = fn1
         fn1 = fn
 100  CONTINUE
      RS_GAMMA = (fn+fn2)/2.
      RETURN
 200  RS_GAMMA = fn
      RETURN
      END
 
