      subroutine impcfn(x,xsi,phi)
c
c data for functions used in the impact parameter method are generated
c using polynomials fitted to seaton's (1962) values using least square
c     author:  M. Bautista
c
      implicit none
c
      real*8  a(6),b(6),x,xsi,phi,pi,y
      integer n
c
      pi=2.d0*dacos(0.d0)
      a(1)=0.9947187d0
      a(2)=0.6030883d0
      a(3)=-2.372843d0
      a(4)=1.864266d0
      a(5)=-0.6305845d0
      a(6)=8.1104480d-02
      b(1)=0.2551543d0
      b(2)=-0.5455462d0
      b(3)=0.3096816d0
      b(4)=4.2568920d-02
      b(5)=-2.0123060d-02
      b(6)=-4.9607030d-03
c
      if(x.gt.2.d0) go to 25
      xsi=0.d0
      phi=0.d0
      do 20 n=1,6
      xsi=xsi+a(n)*x**(n-1)
      y=dlog(x)
      phi=phi+b(n)*y**(n-1)
 20    continue
      if(x.eq.1.d0) phi=b(1)
      if(x.lt.0.05d0) then
      xsi=1.0d0+0.01917d0/0.05d0*x
      y=dlog(1.1229d0/x)
      phi=y+x*x/4.d0*(1.d0-2.d0*y*y)
      endif
      go to 30
c
 25    xsi=pi*x*dexp(-2.d0*x)*(1.d0+0.25d0/x+1.d0/32.d0/x/x)
      phi=pi/2.d0*dexp(-2.d0*x)*(1.d0+0.25d0/x-3.d0/32.d0/x/x)
c
 30    continue
c
      return
      end
