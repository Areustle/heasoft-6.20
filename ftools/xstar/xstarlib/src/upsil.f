      function upsil(k,eij,c,p1,p2,p3,p4,p5,t)
      implicit none
      real*8 e, eij, c, p1, p2, p3, p4, p5, t
      real*8 y, splinem, upsil, x
      integer k
c
c     this routine calculates upsilons for Burgess and Tully
c     author:  M. Bautista
c
c     t = electron temperature in Kelvin
c     p# = spline knot values
c     c = abscissa scale parameter
c     k = transition type
c     eij = transition energy (Ryd)
c
       e=abs(t/(1.57888e5*eij))                  !<<<<<< CORRECTED LINE
       if ((k.eq.1).or.(k.eq.4.)) x=log((e+c)/c)/log(e+c)
       if ((k.eq.2).or.(k.eq.3)) x=e/(e+c)
       y=splinem(p1,p2,p3,p4,p5,x)
       if (k.eq.1) y=y*log(e+2.71828)
       if (k.eq.3) y=y/(e+1)
       if (k.eq.4) y=y*log(e+c)
       upsil=y
c
      return
      end
