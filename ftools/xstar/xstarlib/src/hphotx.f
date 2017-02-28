      subroutine hphotx(ener,ic,nq,xsec,lun11,lpri)
c
c     ener   is the photon energy in ryds with respect to the ionization
c         threshold
c     xsec   is an array containing the cross section in mb (18^{-18} cm^2)
c         for all l=[0,nq-1]
c     ic     ion charge
c     np     principal quantum number
c     ll     angular momentum number
c        real*8  en,cons,r,rk,theta1,theta2,gu,gl
c     author:  M. Bautista
c
c
      implicit none
c
      real*8 gu(100),gl(100),xsec(100)
      real*8 cons,ener,en,r,rk,theta1,theta2
      integer ic,nq,lun11,lpri,lm
c
      cons=.54492*acos(0.)
c        write (lun11,*)'in hphotx:',ener,ic,nq,cons
        en=ener
c        r=dsqrt(en)
        r=sqrt(en)
        rk=r/float(ic*ic)
        if (lpri.ne.0)
     $   write (lun11,*)'before call gull1:',nq,rk,r,en
        call gull1(nq,rk*rk,gu,gl,lpri,lun11)
c        call gull1(nq,rk,gu,gl,lun11)
       do lm=0,nq-1
        theta1=(1.+nq*nq*rk*rk)*exp(gu(lm+1))
        theta2=(1.+nq*nq*rk*rk)*exp(gl(lm+1))
        if (lpri.ne.0)
     $   write (lun11,*)'after call gull1:',lm,gu(lm+1),gl(lm+1),
     $           theta1,theta2
        xsec(lm+1)=cons*((lm+1)*theta1+lm*theta2)/(2.*lm+1.)
        xsec(lm+1)=float(nq*nq)/float(ic*ic)*xsec(lm+1)
       enddo
c
      return
      end
