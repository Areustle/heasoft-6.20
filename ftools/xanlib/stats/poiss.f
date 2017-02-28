C
      real*4 FUNCTION poiss(ib,amea,iflag)
      real*8 sum, ba, back
      real*4 amea
      integer*4 ib, iflag, i, ibm, ir
C**iflag=1 for integral distribution  p(>=n)
C**iflag=2 for differential distribution   p(n)
C**amea must be < 87 **
      back = amea
      ba = 1.
      IF ( iflag.EQ.2 ) THEN
        DO i = 1, ib
          ba = ba*back/i
        END DO
        poiss = ba*dexp(-back)
        IF ( ib.EQ.0 ) poiss = dexp(-back)
        RETURN
      ELSE IF ( ib.EQ.1 ) THEN
        poiss = 1. - dexp(-back)
        RETURN
      ELSE IF ( ib.EQ.0 ) THEN
        poiss = 1.
        RETURN
      ELSE
        ibm = ib - 1
        sum = 1.
        DO ir = 1, ibm
          ba = ba*back/ir
          sum = sum + ba
        END DO
        poiss = 1. - sum*dexp(-back)
        RETURN
      END IF
      END
