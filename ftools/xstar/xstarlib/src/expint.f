      subroutine expint(x,em1)
c
c expint is a subroutine to calculate the value of e1, the exponential
c integral or em1=x*expo(x)*e1 at the point x.  the polynomial
c expressions that are used come from abromowitz and stegen
c     author:  T. Kallman
c
      implicit none
      real*8 x,em1,b1,b2,b3,b4,c1,c2,c3,c4,a0,a1,a2,a3,a4,a5,e1,expo
c
      if(x.le.1.) go to 100
c
      b1=9.5733223454
      b2=25.6329561486
      b3=21.0996530827
      b4=3.9584969228
      c1=8.5733287401
      c2=18.0590169730
      c3=8.6347608925
      c4=0.2677737343
      em1=x**4+c1*x**3+c2*x*x+c3*x+c4
      em1=em1/(x**4+b1*x*x*x+b2*x*x+b3*x+b4)
c      e1=em1/x/expo(x)
      go to 200
c
 100   continue
      a0=-0.57721566
      a1=0.99999193
      a2=-0.24991055
      a3=0.05519968
      a4=-0.00976004
      a5=0.00107857
      if (x.gt.0)then
      e1= a0+a1*x+a2*x*x+a3*x**3+a4*x**4+a5*x**5-log(x)
      else
      e1=-a0+a1*x+a2*x*x+a3*x**3+a4*x**4+a5*x**5-log(-x)
      endif
      em1=e1*x*expo(x)
c
 200   continue
c
      return
      end
