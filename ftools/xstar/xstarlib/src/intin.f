      subroutine intin(x1,x2,x0,t,ri2,ri3,lpri,lun11)
c
c     this routine does the integrals needed by milne
c     author:  M. Bautista
c
      implicit none
c
      real*8  x1,x2,x0,t,ri2,ri3
      real*8  ryk,s1,s2,s0,del,rr
      integer lpri,lun11
c
       ryk=7.2438d+15
       s1=x1*ryk/t
       s2=x2*ryk/t
       s0=x0*ryk/t
       del=ryk/t
       if (lpri.gt.1)
     $  write (lun11,*)'in intin:',s1,s2,s0,del
       if ((s1-s0).lt.90.d0) then
c           ri2=dexpo(s0-s1)*(s1*s1+2.*s1+2.)-dexpo(s0-s2)*(s2*s2+2.*s2+2.)
           ri2=dexp(s0-s1)*((s1*s1+2.d0*s1+2.d0)
     $        -dexp(s1-s2)*(s2*s2+2.d0*s2+2.d0))/del/dsqrt(del)
           if (lpri.gt.1)
     $     write (lun11,*)'ri2=',ri2
           if ((s0.lt.1.d-3).and.(s2.lt.1.d-3).and.(s1.lt.1.d-3))
     $       ri2=0.d0
         else
           ri2=0.d0
         endif
c       ri2=ri2/(del**1.5)
       if (lpri.gt.1)
     $     write (lun11,*)'ri2=',ri2
c       rr=dexpo(s0-s1)*(s1**3)-dexpo(s0-s2)*(s2**3)
       rr=dexp(s0-s1)*((s1**3)-dexp(s1-s2)*(s2**3))
       if (lpri.gt.1)
     $     write (lun11,*)'rr=',rr
       ri3=(rr/del/dsqrt(del)+3.d0*ri2)/del
       if (lpri.gt.1)
     $  write (lun11,*)'in intin:',s1,s2,s0,del,ri2,rr,ri3
       return
       end
