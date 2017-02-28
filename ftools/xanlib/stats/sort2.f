      SUBROUTINE sort2(n,ra,rb)
      real*4 ra(*), rb(*), rra, rrb
      integer*4 n, l, ir, i, j
      l = n/2 + 1
      ir = n
 100  CONTINUE
      IF ( l.GT.1 ) THEN
        l = l - 1
        rra = ra(l)
        rrb = rb(l)
      ELSE
        rra = ra(ir)
        rrb = rb(ir)
        ra(ir) = ra(1)
        rb(ir) = rb(1)
        ir = ir - 1
        IF ( ir.EQ.1 ) THEN
          ra(1) = rra
          rb(1) = rrb
          RETURN
        END IF
      END IF
      i = l
      j = l + l
 200  CONTINUE
      IF ( j.LE.ir ) THEN
        IF ( j.LT.ir ) THEN
          IF ( ra(j).LT.ra(j+1) ) j = j + 1
        END IF
        IF ( rra.LT.ra(j) ) THEN
          ra(i) = ra(j)
          rb(i) = rb(j)
          i = j
          j = j + j
        ELSE
          j = ir + 1
        END IF
        GO TO 200
      END IF
      ra(i) = rra
      rb(i) = rrb
      GO TO 100
      END
