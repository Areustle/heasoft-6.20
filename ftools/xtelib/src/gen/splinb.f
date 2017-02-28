c******************************************************************************
c******************************************************************************
      subroutine splinb(x,f,c1,c2,c3,n)
c------------------------------------------------------------------------------
c     evaluate coefficient matrices for spline fit.
c * * *
c     name --splinb
c     this version evaluates drv1,drvn using divided differences
c * * *
c     parameters:
c               x       -- array of x values
c               f       -- array of function values
c               c1      -- coefficient of x     (output)
c               c2      --                x**2
c               c3      --                x**3
c               drv1    -- derivative at x(1)
c               drvn    -- derivative at x(n)
c               n       -- number of points
c------------------------------------------------------------------------------
      implicit none

      double precision x(*),f(*),c1(*),c2(*),c3(*),
     &   f21,f32,dtst,dxl,drv1,zero,dxh,a,tp1,tp2,drvn,dfl,
     &   dfh,fp,c
      integer n,i,nm1,j

      zero=0.0d0
c-----------------------------------------------------------------------
c     find drv1 using divided differences
c-----------------------------------------------------------------------
      f21=(f(2) - f(1))/(x(2) - x(1))
      f32=(f(3) - f(2))/(x(3) - x(2))
      drv1=f21 - 0.5d0*(f32 - f21)
      
c-----------------------------------------------------------------------
c     find drvn using divided differences
c-----------------------------------------------------------------------
      f21=(f(n) - f(n-1))/(x(n) - x(n-1))
      f32=(f(n-1) - f(n-2))/(x(n-1) - x(n-2))
      drvn=f21 - 0.5d0*(f32 - f21)
      
c-----------------------------------------------------------------------
c     find derivatives at the knots, and place in c1(i)
c-----------------------------------------------------------------------
      dtst=1000.0d0
      nm1=n - 1
      dxl=x(2) - x(1)
      dfl=(f(2) - f(1))*3.0d0/dxl
      c1(1)=drv1
      c2(1)=zero
      c1(2)=drv1
      c2(2)=1.0d0
      do 30, i=2,nm1
        dxh=x(i + 1) - x(i)
        dfh=(f(i+1) - f(i))*3.0d0/dxh
        c1(i+1)=dfh-2.0d0*c1(i)+(dfl-2.0d0*c1(i)-c1(i-1))/
     &     dxl*dxh
        c2(i+1)=-(2.0d0*c2(i)+c2(i-1))*dxh/dxl-2.0d0*c2(i)
        dxl=dxh
        if(dabs(c2(i+1)) .lt. dtst) go to 20
        fp=(dfh + dfl)/6.0d0
        a=(fp - c1(i+1))/c2(i+1)

        do 10, j=1,i
          c1(j)=c1(j) + a*c2(j)
          c2(j)=c2(j)/c2(i + 1)
10      continue

        c1(i + 1)=fp
        c2(i + 1)=1.0d0
20      dfl=dfh

30    continue
      a=(drvn - c1(n))/c2(n)
      
c-----------------------------------------------------------------------
c     fix derivatives and produce c2 and c3
c-----------------------------------------------------------------------
      c1(1)=c1(1) + a*c2(1)
      do 40, j=2,n
        c1(j)=c1(j) + a*c2(j)
        c=x(j) - x(j-1)
        tp1=(f(j) - f(j-1))/c
        tp2=c1(j) + c1(j - 1)
        c3(j - 1)=(-2.0d0*tp1 + tp2)/c**2
        c2(j - 1)=(3.0d0*tp1 - tp2 - c1(j - 1))/c
  40    continue
      c1(n)=drvn
      return
      end

