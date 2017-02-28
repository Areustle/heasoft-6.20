      real*8 function ee1expo(x)
c
c     this routine computes the first exponential integral.
c
      implicit none
c
      real*8 x,expo
c
      if ( x.ge.1. ) then
      ee1expo=(1./x)*(0.250621+x*(2.334733+x))/(1.68153+x*(3.330657+x))
      return
      endif
c
      ee1expo = (-log(x)-0.57721566+
     &      x*(0.99999193+x*(-0.24991055+x*(0.05519968+
     &      x*(-0.00976004+x*0.0010707857)))))*expo(x)
c
      return
      end
