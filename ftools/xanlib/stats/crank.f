      SUBROUTINE crank(n,w,s)
      real*4 w(*)
      integer*4 n, j, ji, jt
      real*4 rank, t, s
      s = 0.
      j = 1
 100  CONTINUE
      IF ( j.LT.n ) THEN
        IF ( w(j+1).NE.w(j) ) THEN
          w(j) = j
          j = j + 1
        ELSE
          DO jt = j + 1, n
            IF ( w(jt).NE.w(j) ) GO TO 120
          END DO
          jt = n + 1
 120      CONTINUE
          rank = 0.5*(j+jt-1)
          DO ji = j, jt - 1
            w(ji) = rank
          END DO
          t = jt - j
          s = s + t**3 - t
          j = jt
        END IF
        GO TO 100
      END IF
      IF ( j.EQ.n ) w(n) = n
      RETURN
      END
