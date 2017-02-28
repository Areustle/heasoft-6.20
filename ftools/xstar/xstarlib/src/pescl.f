      real*8 function pescl(tau)
c
c     this routine calculates escape probability for a line transition
c     inputs: optical depths-- tau for a line transition
c
      implicit none
c
      real*8 tau
      real*8 pi,tauw,aa,bb
c
      data pi/3.1415927/
c
      tauw=1.e5
c     *** need to determine tauw from line profiles?***
      if(tau.lt.1.0) then
        if(tau.lt.1.e-5) then
          pescl=1.0
          go to 10
        end if
        aa=2.0*tau
        pescl=(1.0-exp(-aa))/aa
        go to 10
      end if
      bb=0.5*sqrt(log(tau))/(1.0+tau/tauw)
      pescl=1./(tau*sqrt(pi)*(1.2+bb))
10    continue
      pescl=pescl/2.0
c
      return
      end
