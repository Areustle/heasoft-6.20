      real*8 function ee1exp(x)
c
c     this routine computes the first exponential integral.
c
      implicit none
c
      real*8 x
c
      if ( x.ge.1. ) then
        ee1exp=(1./x)*(0.250621+x*(2.334733+x))/(1.68153+x*(3.330657+x))
        return
      endif
c
      ee1exp = (-log(x)-0.57721566+
     &      x*(0.99999193+x*(-0.24991055+x*(0.05519968+
     &      x*(-0.00976004+x*0.0010707857)))))*exp(x)
c
      return
      end
