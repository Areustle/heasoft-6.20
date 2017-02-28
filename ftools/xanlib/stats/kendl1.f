      SUBROUTINE kendl1(data1,data2,n,tau,z,prob)
c    
      integer*4 n, j, k, n1, n2, is

      real*4 aa, a1, a2, tau, var, z, prob, erfcc
      real*4 data1(*), data2(*)
      n1 = 0
      n2 = 0
      is = 0
      DO j = 1, n - 1
        DO k = j + 1, n
          a1 = data1(j) - data1(k)
          a2 = data2(j) - data2(k)
          aa = a1*a2
          IF ( aa.NE.0. ) THEN
            n1 = n1 + 1
            n2 = n2 + 1
            IF ( aa.GT.0. ) THEN
              is = is + 1
            ELSE
              is = is - 1
            END IF
          ELSE
            IF ( a1.NE.0. ) n1 = n1 + 1
            IF ( a2.NE.0. ) n2 = n2 + 1
          END IF
        END DO
      END DO
      tau = float(is)/sqrt(float(n1)*float(n2))
      var = (4.*n+10.)/(9.*n*(n-1.))
      z = tau/sqrt(var)
      prob = erfcc(abs(z)/1.4142136)
      RETURN
      END
