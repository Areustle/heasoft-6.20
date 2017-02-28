      function splinem(p1,p2,p3,p4,p5,x)
      implicit none
c
      real*8 p1, p2, p3, p4, p5, x
      real*8 s, s2, s3, s4, x0
      real*8 t0, t1, t2, t3, splinem

c
c 5-point spline interpolation of y(x), for x in the range (0,1)
c knot values p1=y(0), p2=y(1/4), p3=y(1/2), p4=y(3/4), p5=y(1)
c     author:  M. Bautista
c
       s=1./30.
       s2=32.*s*(19.*p1-43.*p2+30.*p3-7.*p4+p5)
       s3=160.*s*(-p1+7.*p2-12.*p3+7.*p4-p5)
       s4=32.*s*(p1-7.*p2+30.*p3-43.*p4+19.*p5)
       if (x.gt.0.25) goto 1
       x0=x-0.125
       t3=0.0
       t2=0.5*s2
       t1=4.*(p2-p1)
       t0=0.5*(p1+p2)-0.015625*t2
       goto 4
 1     if (x.gt.0.5) goto 2
       x0=x-0.375
       t3=20.*s*(s3-s2)
       t2=0.25*(s2+s3)
       t1=4.*(p3-p2)-0.015625*t3
       t0=0.5*(p2+p3)-0.015625*t2
       goto 4
 2     if (x.gt.0.75) goto 3
       x0=x-0.625
       t3=20.*s*(s4-s3)
c      nb this was an error
c       t2=0.25*(s3-s4)
       t2=0.25*(s3+s4)
       t1=4.*(p4-p3)-0.015625*t3
       t0=0.5*(p3+p4)-0.015625*t2
       goto 4
 3     x0=x-0.875
       t3=0.0
       t2=0.5*s4
       t1=4.*(p5-p4)
       t0=0.5*(p4+p5)-0.015625*t2
 4     splinem=t0+x0*(t1+x0*(t2+x0*t3))
       return
       end
