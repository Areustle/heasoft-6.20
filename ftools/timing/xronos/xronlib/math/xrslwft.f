c
      SUBROUTINE xrslwft(y, ndata, c, s)
c
c  ls 15/5/90 calculates slow Fouries Transform (adapted from N.White's
c              subr. slwft). Sin and cos are calculated with Chebychev
c              recursion formulae. Note that transform at zero freq. is not
c              calculated. Single precision routine.
c
c   I   y     = r*4 input intensities (must be at least ndata elements)
c   I   ndata = i*4 no. of data
c   O   c     = cos transform (must be at least ndata/2 elements)
c   O   s     = sin transform (must be at least ndata/2 elements)
c
c
      INTEGER*4 ndata, k, j
      REAL*4 y(*), c(*), s(*)
      REAL*4 rj, szero, czero, rv, sdom, cdom, skm1, ckm1, skm2, ckm2,
     &       sk, ck, twopi
c
c
      DATA twopi/6.283185308/
c
      rv = twopi/float(ndata)
      DO k = 1, ndata/2
         s(k) = 0.
         c(k) = 0.
      ENDDO
      DO j = 1, ndata
         rj = float(j)
         szero = sin(rj*rv)
         czero = cos(rj*rv)
         sdom = sin(rj*rv)
         cdom = cos(rj*rv)
         k = 1
         skm2 = 0.
         ckm2 = 1.
         s(k) = s(k) + szero*y(j)
         c(k) = c(k) + czero*y(j)
         k = 2
         skm1 = sdom
         ckm1 = cdom
         s(k) = s(k) + (szero*ckm1+czero*skm1)*y(j)
         c(k) = c(k) + (czero*ckm1-szero*skm1)*y(j)
         DO k = 3, ndata/2
            sk = 2.*skm1*cdom - skm2
            ck = 2.*ckm1*cdom - ckm2
            s(k) = s(k) + (szero*ck+czero*sk)*y(j)
            c(k) = c(k) + (czero*ck-szero*sk)*y(j)
            skm2 = skm1
            ckm2 = ckm1
            skm1 = sk
            ckm1 = ck
         ENDDO
      ENDDO
c      do k=1,ndata/2
c        s(k)=2.*s(k)/float(ndata)
c        c(k)=2.*c(k)/float(ndata)
c      end do
c
      RETURN
      END
c
c
c
