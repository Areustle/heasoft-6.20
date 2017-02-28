      subroutine szcoll(ni,nj,tt,rate,ic)
c
c     calculates electron impact excitation rates from semiempirical
c     formula (eq.35) from smpson & zhang (1988, apj 335, 516)
c       real*8  abethe(11), hbethe(11), rbethe(11)
c       real*8  fvg1(5),fvg2(5),fvg3(5)
c     author:  M. Bautista
c
       implicit none
c
       real*8 abethe(11), hbethe(11), rbethe(11)
       real*8 fvg1(5),fvg2(5),fvg3(5)
       integer ni,nj,ic,i
       real*8 tt,rate,eion,const,rn2,g1,g2,g3,xx,gaunt,an,hn,rrn,
     $      ann,dnn,cnn,yy,e2,e3,eint1,fnn
c
       data(abethe(i),i=1,11)/ 1.30, 0.59, 0.38, 0.286, 0.229, 0.192,
     1       0.164, 0.141, 0.121, 0.105, 0.100 /
       data(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       data(rbethe(i),i=1,11)/ 1.83, 1.60, 1.53, 1.495, 1.475, 1.46,
     1       1.45, 1.45, 1.46, 1.47, 1.48 /
       data(fvg1(i),i=1,5)/ 1.133, 1.0785, 0.9935, 0.2328, -0.1296/
       data(fvg2(i),i=1,5)/ -0.4059, -0.2319, 0.6282, -0.5598,0.5299/
       data(fvg3(i),i=1,5)/ 0.07014, 0.02947, 0.3887, -1.181, 1.47/
c
       eion=1.578203e+5
       const=8.63e-6
c
       rn2=(float(ni)/float(nj))**2
c  computes fvalue as in johnson 1972)
       g1=0.
       g2=0.
       g3=0.
       if (ni.eq.1 ) then
        g1=fvg1(1)
        g2=fvg2(1)
        g3=fvg3(1)
       endif
       if (ni.eq.2 ) then
        g1=fvg1(2)
        g2=fvg2(2)
        g3=fvg3(2)
       endif
       if (ni.ge.3) then
        g1=fvg1(3)+fvg1(4)/ni+fvg1(5)/ni/ni
        g2=(fvg2(3)+fvg2(4)/ni+fvg2(5)/ni/ni)/ni*(-1.)
        g3=(fvg3(3)+fvg3(4)/ni+fvg3(5)/ni/ni)/ni/ni
       endif
       xx=1.-rn2
       gaunt=g1+g2/xx+g3/xx/xx
       fnn=1.9603*gaunt/(xx**3)*ni/(nj**3)
c
       if (ni.lt.11) then
         an=abethe(ni)
         hn=hbethe(ni)
         rrn=rbethe(ni)
       else
         an=abethe(11)/float(ni)
         hn=hbethe(11)*float(ni)
         rrn=rbethe(11)
       endif
       ann=fnn*4.*(ni**4)/(1.-rn2)
       dnn=ann*hn*((1.-rn2)**rrn - an*rn2)
c       write (lun11,*)ann,hn,rn2,rrn,an,fnn,dnn
       cnn=1.12*ni*ann*(1.-rn2)
       if ((nj-ni).eq.1) cnn=cnn*exp(-0.006*((ni-1)**6)/ic)
       yy=eion*ic*ic*(1./float(ni*ni)-1./float(nj*nj))/tt
       call eint(yy,eint1,e2,e3)
       rate=const/sqrt(tt)/ni/ni/ic/ic*(dnn*exp(-yy)+(ann+
     1   yy*(cnn-dnn))*eint1)
c       write (lun11,*)'in szcoll:',const,ni,nj,tt,ic,dnn,yy,
c     $     ann,cnn,dnn,eint1
c
       return
       end
