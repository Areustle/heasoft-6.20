C
      real*8 FUNCTION polog(ib,amea,iflag)
c      IMPLICIT REAL*8(a-h,o-z)
C**iflag=1 for integral distribution  p(n>=ib,amea)
C**iflag=2 for differential distribution   p(n=ib,amea)
      real*8 amea, ba, sum, par, ar, dfacl
      integer*4 ib, iflag, ir
      ba = 1.
      IF ( iflag.EQ.2 ) THEN
        IF ( ib.EQ.0 ) THEN
          polog = dexp(-amea)
          RETURN
        ELSE
          polog = dexp(ib*dlog(amea)-dfacl(ib)-amea)
          RETURN
        END IF
      ELSE IF ( ib.EQ.1 ) THEN
        polog = 1. - dexp(-amea)
        RETURN
      ELSE IF ( ib.EQ.0 ) THEN
        polog = 1.
        RETURN
      ELSE
        IF ( amea.LT.86. ) THEN
          sum = dexp(-amea)
        ELSE
          sum = 0.
        END IF
        par = 0.
        DO ir = 1, ib - 1
          ar = ir
          par = par + dlog(ar)
          sum = sum + dexp(ir*dlog(amea)-par-amea)
        END DO
        polog = 1. - sum
        RETURN
      END IF
      END
