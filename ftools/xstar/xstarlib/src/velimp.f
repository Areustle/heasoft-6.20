      subroutine velimp(n,l,temp,ic,z1,rm,ne,sum,cn)
      implicit none
c
c     impact parameter collision rate calculated following the method of
c     pengelly & seaton (1964) but using the lowest cross-section at every
c     velocity.
c     note that cn is the rate for nl -> nl-1 and hence l > 0 *
c     cne(l+1)=cn
c     cen(l)=cn*(2.*l+1)/(2.*l-1)
c     author:  M. Bautista
c
      real*8 ne, temp, z1, rm, sum, cn
      real*8 pi, pa, pd, alfa, b, dnl, bb
      real*8 va, vd, ava, vb, avb, avd, eb
      real*8 ea, ed, xa, xb, xd, expo, den
      real*8 ca, cad, cd
      integer n, l, ic
c
      cn=0.
      if((l.eq.0).or.(sum.eq.0.)) go to 50
      den=l*(n*n-l*l)+(l+1)*(n*n-(l+1)*(l+1))
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      pi=2.*acos(0.)
      pa=0.72/sum
      pd=6.90*sqrt(temp/ne)
      alfa=3.297e-12*rm/temp
      b=1.157*sqrt(dnl)
      bb=b*b
c
      va=pd/pa
      vd=b/pd
      vb=sqrt(va*vd)
c
           ava=alfa*va*va
           avb=alfa*vb*vb
           avd=alfa*vd*vd
      ea=0.
      ed=0.
      xa=expo(-ava)
      xb=expo(-avb)
      xd=expo(-avd)
           if(ava.lt.50.) call expint(ava,ea)
c           call expint(ava,ea)
      ea=ea/ava*xa
           call expint(avb,eb)
      eb=eb/avb*xb
           if(avd.lt.50.) call expint(avd,ed)
c           call expint(avd,ed)
      ed=ed/avd*xd
c
      if(va.gt.vd) then
      if(avb.gt.1.e-3) then
      cn=sqrt(pi*alfa)*(pa*pa*(2./alfa/alfa-xb*(vb**4+2.*vb*vb/alfa+
     #2./alfa/alfa))+bb*xb+2.*bb*eb-bb*ea)
           else
      cn=sqrt(pi*alfa)*bb*(1.+avb*(1./3.-avb/4.)+2.*eb-ea)
                endif
c
           else
      if(ava.gt.1.e-3) then
      ca=sqrt(pi*alfa)*pa*pa*(2./alfa/alfa-xa*(va**4+2.*va*va/alfa+
     #2./alfa/alfa))
           else
      ca=sqrt(pi*alfa)*pd*pd*va**4*alfa*(1/3.-ava/4.+ava*ava/10.)
                 endif
c
      cad=sqrt(pi*alfa)*pd*pd/alfa*(xa*(1.+ava)-xd*(1.+avd))
      cd=sqrt(pi*alfa)*bb*(xd+ed)
      cn=ca+cad+cd
                 endif
c
      cn=cn*l*(n*n-l*l)/den
c
 50      return
c
      end
