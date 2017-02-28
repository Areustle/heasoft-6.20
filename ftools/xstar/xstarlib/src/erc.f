      subroutine erc(n,m,t,ic,se,sd,a,lun11,lpri)
c
c erc calculates the excitation rate, se [cm**3/s],  for atomic
c transitions between lower state n and upper state m in hydrogen
c due to electron collisions.  the energy loss rate, sl [ev*cm**3/s],
c from the electron gas is also determined.  (cf. johnson,1972)
c sd = deexcitation rate;   sg = energy gained by electron gas
c sm is a quantity symmetrical in n and m, used in models
c ***  the quantity em1 is required from subr. expint in this program
c     author:  M. Bautista
c
      implicit none
c
      integer n,m,ic,lun11,lpri
      real*8 t,a,ym,xn,s,sd,se,f,z,yn,dif,e1y,e1z,e2,rn,rm,
     $     ann,bnn,sm,expo,bn,ric
c
c
      rn=float(n)
      rm=float(m)
      ric=float(ic)
      if (lpri.gt.1)
     $ write(lun11,*)'erc',n,m,t,ic,a
      sm=0.
      if(ic.ne.1) then
       if (ic.lt.10) then
        ym=157803.*ic*ic/t/m/m
        if (ym.gt.40.) then
         sd=0.
         se=0.
         return
        endif
        if (lpri.gt.1)
     $   write (lun11,*)'before impactn:',
     $       n,m,t,ic,a,sm
        call impactn(n,m,t,ic,a,sm,lun11,lpri)
        if (lpri.gt.1)
     $   write (lun11,*)'after impactn:',
     $       n,m,t,ic,a,sm
        ym=157803.*ric*ric/t/(rm*rm)
        xn=(1./(rn*rn)-1./(rm*rm))
        yn=157803.*ric*ric*xn/t
        s=sm/(rn*rn)/exp(ym)
        sd=s*rn*rn/(rm*rm)
        if (yn.lt.40.) then
         se=s*exp(-yn)
        else
         se=0.e0
        endif
        if (lpri.gt.1)
     $    write (lun11,*)ym,xn,yn,s,sd,se
       else
        if (lpri.gt.1)
     $   write (lun11,*)'calling szcoll:',
     $       n,m,t,se,ic
        call szcoll(n,m,t,se,ic)
        ym=157803.*ric*ric/t/(rm*rm)
        xn=(1./(rn*rn)-1./(rm*rm))
        yn=157803.*ric*ric*xn/t
        sd=se*exp(min(50.,yn))*(rn*rn)/(rm*rm)
        if (lpri.gt.1)
     $   write (lun11,*)'after szcoll:',
     $       ym,xn,yn,sd,ric,rm,xn,rn,t
       endif
      else
      xn=(1./(rn*rn)-1./(rm*rm))
      f=-1.2456e-10*a/xn/xn
      yn=157803.*xn/t
      ym=157803./t/(rm*rm)
      z=1.94*xn*rn**0.43+yn
      if(n.eq.1) z=yn+0.45*xn
      dif=z-yn
c
      if (lpri.gt.1)
     $ write(lun11,*)'before expint:',yn,z
       call expint(yn,e1y)
       call expint(z,e1z)
      if (lpri.gt.1)
     $ write(lun11,*)'after expint:',yn,e1y,z,e1z
      e2=(1.-e1y)/yn-expo(-dif)*(1.-e1z)/z
      ann=-2.*f*rm*rm/xn/rn/rn
      bn=(4.-18.63/rn+36.24/rn/rn-28.09/(rn**3))/rn
      if(n.eq.1) bn=-0.603
      bnn=(1.+4./(xn*rn*rn*3)+bn/(rn**4*xn*xn))*4./(rm**3*xn*xn)
      if (lpri.gt.1)
     $ write(lun11,*)e2,rn,rm,ann,bn,bnn
c
      s=ann*((1./yn+0.5)*e1y/yn-(1./z+0.5)*e1z*expo(-dif)/z)
      s=s+e2*(bnn-ann*log(2./xn))
      s=1.095e-10*yn*yn*sqrt(t)*s/xn
      sm=s*rn*rn*expo(ym)
      if (lpri.gt.1)
     $  write(lun11,*)s,sm,yn,xn,t,dif
       sd=s*rn/rm*rn/rm
       se=s*expo(-yn)
      if (lpri.gt.1)
     $  write(lun11,*)'erc=',se,sd
c      sg=13.60*sq*xn
c      sl=13.60*s*xn
        endif
      if (lpri.gt.1)
     $  write(lun11,*)'erc return:',se,sd
c
      return
      end
