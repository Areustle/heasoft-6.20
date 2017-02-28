      SUBROUTINE simp1(akern,a,b,aint,r,n,accur,nmax)
C
C      calculate definite integrals of a function
C      using simpson's rule
C      inputs :
C      akern  : function to be integrated, must be a function subroutine
C               and must be declared 'external' in main program.
C      a,b    : limits of integration
C      accur  : relative accuracy required
C      nmax   : max number of iterations allowed
C      outputs:
C      aint   : integral of function
C      r      : actual relative accuracy achieved
C      n      : actual no of iterations performed
C      error messages
C      exit 14 : no convergence to accuracy requird achieved
C      exit 15 : a > b
c
      real*8 t, r, oldint, accur
      real*8 dh, b, a, aint, akern, aone, atwo, afour
      integer*4 n, nmax, i

      dh = (b-a)/4.D0
      IF ( dh.EQ.0.D0 ) THEN
        aint = 0.D0
        RETURN
      END IF
      IF ( dh.LT.0.D0 ) THEN
        WRITE (*,*) ' Simp1 exit 15 , a > b ! ', dh
        STOP
      END IF
      n = 2
      aone = akern(a) + akern(b)
      atwo = akern(a+dh+dh)
      afour = akern(a+dh) + akern(b-dh)
      aint = dh/3.D0*(aone+2.*atwo+4.D0*afour)
 100  CONTINUE
      dh = dh/2.D0
      n = n + n
      atwo = atwo + afour
      afour = 0.D0
      t = a - dh
      DO i = 1, n
        t = t + 2.D0*dh
        afour = afour + akern(t)
      END DO
      oldint = aint
      aint = dh/3.D0*(aone+2.D0*atwo+4.D0*afour)
      r = abs((aint-oldint)/aint)
      IF ( r.LE.accur ) RETURN
      IF ( n.LT.nmax ) GO TO 100
      WRITE (*,*) ' Simp1 exit 14, accuracy required not achieved ', n,
     &            aint, r
      STOP
      END
