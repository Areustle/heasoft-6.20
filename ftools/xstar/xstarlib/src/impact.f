      subroutine impact(en,l,temp,ic,z1,rm,ecm,psi,cr)
c
c impact parameter collision cross-sections using the method of seaton.
c     author:  M. Bautista
c
      implicit none
c
      real*8 en,temp,z1,rm,ecm,psi,cr
      integer l,ic
c
      real*8  b,xsi,phi,bo,xsw,phw,del
      real*8 tk,fi,wo,ev,po,w,wi,ff,crinc,ric
      integer inc,jm,j
c
      tk=8.617e-5*temp
      inc=1
      jm=90*inc
      cr=0.
      fi=0.
      wo=0.
      b=10.d0
      ric=float(ic)
      ev=abs(ecm)/8065.48
c
      po=(3.*en*en-l*(l+1))/2./ic
c
c strong coupling
c
 21    del=b/100.d0/inc
      do 20 j=1,jm
      b=b-del
      call impcfn(b,xsi,phi)
c     write (lun11,*)b,xsi,phi
      w=ric*rm*ev/sngl(b)*sqrt(2.*sngl(xsi)*psi)
      wi=w+ecm/8065.48/2.
c     write (lun11,*)wi/tk
      if(wi/tk.ge.100.) go to 13
      if(wi.le.0.) go to 20
c
c weak coupling
c
      bo=dble(po*ev/2./w*sqrt(wi*rm/13.60))
      call impcfn(bo,xsw,phw)
c
c the minimum of the weak and strong coupling x-sections is used
      ff=min(sngl(xsi/2.d0+phi),sngl(phw))
      ff=ff*exp(-wi/tk)
c
      crinc=(fi+ff)/2.*(wi-wo)
      cr=crinc+cr
      if(cr.lt.1.e-20) go to 20
      fi=ff
      wo=wi
      if(crinc/cr.lt.1.e-5) go to 13
c
 20    continue
      go to 21
 13       cr=6.900e-5*z1*z1*sqrt(rm/temp)*psi*cr/tk
c
      return
      end
