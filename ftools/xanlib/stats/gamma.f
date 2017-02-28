C
      real*4 FUNCTION gamma(zee)   
c computes gamma function
C this function calculates the value of the gamma function
C for zee in the range  0.+ -- 34.
c      IMPLICIT REAL*8(a-h,o-z)
      real*8 coef(26), dlo
      real*8 zee, z,zt,ouga,zlog,zeta,abcoe,ggg,co
      integer*4 izee,i
      DATA coef/1.0000000D0, 0.5772156649015329D0,
     &     -0.6558780715202538D0, -0.0420026350340952D0,
     &     0.1665386113822915D0, -0.0421977345555443D0,
     &     -0.0096219715278770D0, 0.0072189432466630D0,
     &     -0.0011651675918591D0, -0.0002152416741149D0,
     &     0.0001280502823882D0, -0.0000201348547807D0,
     &     -0.0000012504934821D0, 0.0000011330272320D0,
     &     -0.0000002056338417D0, 0.0000000061160950D0,
     &     0.0000000050020075D0, -0.0000000011812746D0,
     &     0.0000000001043427D0, 0.0000000000077823D0,
     &     -0.0000000000036968D0, 0.0000000000005100D0,
     &     -0.0000000000000206D0, -0.0000000000000054D0,
     &     0.0000000000000014D0, 0.0000000000000001D0/
      izee = zee
      zt = zee - izee
      z = 1. + zt
      IF ( izee.EQ.0 ) z = zee
      ouga = 0.
      zlog = dlog(z)
      zeta = 0.
      DO i = 1, 26
        zeta = zeta + zlog
        abcoe = dabs(coef(i))
        dlo = dlog(abcoe)
        ouga = ouga + coef(i)/abcoe*dexp(zeta+dlo)
      END DO
      ggg = 1./ouga
      co = 1.
      IF ( izee.NE.1 .AND. izee.NE.0 ) THEN
        DO i = 1, izee - 1
          co = co*(izee-i+zt)
        END DO
      END IF
      gamma = co*ggg
      RETURN
      END
