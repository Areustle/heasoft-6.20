C
      logical function dnear(d1,d2,tol)

c Boolean test for approximate equality -- double precision arguments.

c  I  d1,d2 (d) Arguments
c  I  tol   (d) Tolerance
c  O  dnear (l) = .true. if 2 * |d1| - |d2| / (|d1| + |d2|) <= tol,
c                 or if both are zero; = .false. otherwise.

      double precision d1,d2,test,tol,denom

      denom = dabs(d1) + dabs(d2)

      if(denom.eq.0.d0) then
         dnear = .true.
      else
         test = 2 * dabs(dabs(d1) - dabs(d2)) / denom
         dnear = test .le. dabs(tol)
      endif

      return
      end
