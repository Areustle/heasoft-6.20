       subroutine calt73(temp,np1r,np1i,rdat1,idat1,crate)
c
c   Takes coefficients in data type 73 and returns excitation rate times
c   the statistical weight of the lower level (w_i C(i,j) in s^-1
c   cm^3).
c      author: M. Bautista
c
       implicit none
       include './PARAM'
c
       real*8 rdat1(nrdat1)
       integer idat1(nidat1)
       real*8 temp,crate,boltzk,const,z,y,gam,zeff,z2s
       real*8 a,co,cr,cr1,r,e1,em1,ee1,ee2,ee3,er,er1,qij
       integer np1r,np1i
c
       boltzk=1.578876e+05
       const=5.46538e-11
       z=float(idat1(np1i-1+3))
       y=z*z*rdat1(np1r)*boltzk/temp
c
       gam=0.
       if (rdat1(np1r-1+2).ge. 0.1) gam=-.2
       if (rdat1(np1r-1+2).gt. 0.01 .and. rdat1(np1r-1+2).lt. 0.1) 
     $    gam=0.
       if (rdat1(np1r-1+2).le. 0.01) gam=0.2
       zeff=float(idat1(np1i+2))-gam
       z2s=rdat1(np1r-1+2)
       a=rdat1(np1r-1+3)
       co=rdat1(np1r-1+4)
       cr=rdat1(np1r-1+5)
       cr1=rdat1(np1r-1+6)
       r=rdat1(np1r-1+7)
       if (y.gt.40.) then
        crate=0.
        return
       endif
        call expint(y,em1)
        e1=em1/y*exp(-y)
       if (y*a+y.le.80) then
        call eint(y*a+y,ee1,ee2,ee3)
       else
        ee1=0.
        ee2=0.
        ee3=0.
       endif
       er=0.
       er1=0.
       if (r.eq.1.) then
        er=ee1
        er1=ee2
       endif
       if (r.eq.2.) then
        er=ee2
        er1=ee3
       endif
       if (y*a+y.le.40) then
        qij=co*exp(-y)+1.55*z2s*e1+y*exp(y*a)*(cr*er/(a+1.)**(r-1.)
     #      +cr1*er1/(a+1.)**r)
       else
        qij=co*exp(-y)+1.55*z2s*e1
       endif
       crate=qij*boltzk/temp*sqrt(temp)/zeff/zeff*const
       if (crate.lt.0.) crate=0.
c
       return
       end
