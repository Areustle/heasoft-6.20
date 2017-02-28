c******************************************************************************
c******************************************************************************
      double precision function splint(x,xk,f,c1,c2,c3,n)
c------------------------------------------------------------------------------
c     evaluate spline interpolation using arrays produced by
c     splinb.  descriptions of the parameters are as follows:
c
c               x  -- independent variable
c               xk -- x values at knots
c               f  -- function values at knots
c
c      
c               c1 -- coefficients of x in spline function (also
c                       derivative at knots)
c               c2 -- coefficients of x**2
c               c3 -- coefficients of x**3
c               n -- number of knots
c               splint returns f + c1*a + c2*a**2 + c3*a**3 where a is
c                       x - xi
c               splnd1 returns the first derivative
c               splnd2 returns the second derivative
c * * *
c     name    -- splint
c------------------------------------------------------------------------------
      implicit none

      double precision x,xk(*),f(*),c1(*),c2(*),c3(*),
     &   a
      integer n,ider,nl,nt,i
c
c
      ider=-1
c     the following is to shut up a "might be used uninitialized"
c     warning and should never actually be the returned value
      splint=0
c-----------------------------------------------------------------------
c     go to 100
c     entry splnd1(x,xk,f,c1,c2,c3,n)
c     ider=0
c     go to 100
c     entry splnd2(x,xk,f,c1,c2,c3,n)
c     ider=1
c100  continue
c-----------------------------------------------------------------------
c     find interval using binary search
c-----------------------------------------------------------------------
      nl= n - 1
      if(x .ge. xk(n)) go to 50
      nl=1
      if(x .le. xk(1)) go to 50
      nt=n
  10  i=(nl + nt)/2
      if(abs(x-xk(i)).lt.1.0d-06) goto 90
      if(x - xk(i)) 20,90,30
  20  nt=i
      go to 40
  30  nl=i
  40  if((nt - nl) .gt. 1) go to 10
c-----------------------------------------------------------------------
c     evaluate polynomial
c-----------------------------------------------------------------------
  50  a=x - xk(nl)
      if(ider) 60,70,80
  60  splint=((c3(nl)*a + c2(nl))*a + c1(nl))*a +f(nl)
      return
  70  splint=c1(nl) + a*(2.0d0*c2(nl) + 3.0d0*a*c3(nl))
      return
  80  splint=2.0d0*c2(nl) + 6.0d0*a*c3(nl)
      return
  90  continue
      if(ider .eq. -1) splint=f(i)
      if(ider .eq. 0) splint=c1(i)
      if(ider .eq. 1) splint=2.0d0*c2(i)
      return
      end
