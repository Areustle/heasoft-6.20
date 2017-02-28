      subroutine szirco(nn,t,rz,cii)
c
c     calculates electron impact ionizition rates from semiempirical
c     formula (eq.35) from smpson & zhang (1988, apj 335, 516)
c     author:  M. Bautista
c
c
       implicit none
c
       real*8 abethe(11), hbethe(11), rbethe(11)
       integer nn,i
       real*8 t,rz,cii,boltz,eion,const,an,hn,rrn,tt,rn,yy,
     $      e1,e2,e3,term1,term2,term3
c
       data(abethe(i),i=1,11)/ 1.134, 0.603, 0.412, 0.313, 0.252,
     1       0.211, 0.181, 0.159, 0.142, 0.128, 1.307 /
       data(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       data(rbethe(i),i=1,11)/ 2.20, 1.90, 1.73, 1.65, 1.60, 1.56,
     1       1.54, 1.52, 1.52, 1.52, 1.52 /
c
       boltz=1.38066e-16
       eion=2.179874e-11
       const=4.6513e-3
c
       if (nn.lt.11) then
         an=abethe(nn)
         hn=hbethe(nn)
         rrn=rbethe(nn)
       else
         an=abethe(11)/float(nn)
         hn=hbethe(11)*float(nn)
         rrn=rbethe(11)
       endif
       tt= t*boltz
       rn=float(nn)
c       rz=float(nz)
       yy=rz*rz/(rn*rn)*eion/tt
       call eint(yy,e1,e2,e3)
       term1=e1/rn-(exp(-yy)-yy*e3)/(3.*rn)
       term2=(yy*e2-2.*yy*e1+exp(-yy))*3.*hn/rn/(3.-rrn)
       term3=(e1-e2)*3.36*yy
       cii=const*sqrt(tt)*(rn**5)/(rz**4)*an*yy* (
     1   term1+term2+term3)
c       write (lun11,*)'in szirc:',nn,t,an,hn,rrn,rn,yy,e1,e2,e3,term1,
c     $    term2,term3,cii
c      give result
c
      end
