      subroutine szirc(nn,T,rz,rno,cii,lpri,lun11)
c
c     calculates electron impact ionizition rates from semiempirical
c     formula (eq.35) from Smpson & Zhang (1988, ApJ 335, 516)
c     author:  M. Bautista
c
       implicit none
c
       real*8 abethe(11), hbethe(11), rbethe(11)
       integer nn,i
       real*8 t,rz,rno,cii,boltz,eion,const,rc,an,hn,rrn,tt,rn,yy,
     $      e1,e2,e3
       integer lpri,lun11
c
       DATA(abethe(i),i=1,11)/ 1.134, 0.603, 0.412, 0.313, 0.252,
     1       0.211, 0.181, 0.159, 0.142, 0.128, 1.307 /
       DATA(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       DATA(rbethe(i),i=1,11)/ 2.20, 1.90, 1.73, 1.65, 1.60, 1.56,
     1       1.54, 1.52, 1.52, 1.52, 1.52 /
c
       Boltz=1.38066e-16
       Eion=2.179874e-11
       const=4.6513e-3
C
       rc=float(int(rno))
       if (nn.lt.11) then
         an=abethe(nn)
         hn=hbethe(nn)
         rrn=rbethe(nn)
       else
         an=abethe(11)/float(nn)
         hn=hbethe(11)*float(nn)
         rrn=rbethe(11)
       endif
       tt= T*Boltz
       rn=float(nn)
c      yy=rz*rz/(rn*rn)*Eion/tt
       yy=rz*rz*Eion/tt*(1./rn/rn-1./rc/rc-.25*(1./(rc-1.)**2-
     c    1./rc/rc))
       call eint(yy,e1,e2,e3)
       cii=const*sqrt(tt)*(rn**5)/(rz**4)*an*yy* (
     1   e1/rn-(exp(-yy)-yy*e3)/(3.*rn)+(yy*e2-2.*yy*e1+exp(-yy))*
     2   3.*hn/rn/(3.-rrn)+(e1-e2)*3.36*yy)
      if (lpri.ne.0) write (lun11,*)'in szirc',nn,t,rz,rno,an,hn,
     $  rrn,tt,rn,yy,e1,e2,e3,cii
c
c       write (lun11,*)YY,1./yy,cii
C      give result
c
      end
