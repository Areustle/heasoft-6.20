      SUBROUTINE momnts(data,n,ave,adev,sdev,var,skew,curt)

c     calculates moments of a distribution,

      integer*4 n, j
      real*8 s, temp
      real*4 ave, adev, sdev, var, skew, curt, p
      REAL*4 data(*), ss
      IF ( n.LE.1 ) THEN
        call xwrite(' Less then two points in routine moments',10)
        RETURN
      END IF
      s = 0.
      DO j = 1, n
        s = s + data(j)
      END DO
      temp = s/float(n)
      ave = temp
      adev = 0.
      var = 0.
      skew = 0.
      curt = 0.
      DO j = 1, n
        ss = data(j) - ave
        adev = adev + abs(ss)
        p = ss*ss
        var = var + p
        p = p*ss
        skew = skew + p
        p = p*ss
        curt = curt + p
      END DO
      adev = adev/float(n)
      var = var/float(n-1)
      sdev = sqrt(var)
      IF ( var.NE.0. ) THEN
        skew = skew/(float(n)*sdev**3)
        curt = curt/(float(n)*var**2) - 3.
      ELSE
        call 
     &xwrite(' Variance is zero, skew and curt not calculated',10)
      END IF
      RETURN
      END
