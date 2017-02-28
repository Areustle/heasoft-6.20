       subroutine calt69(temp,m,np1r,rdat1,gamma,lpri,lun11)
c
c   Takes coefficients in data type 69 and returns effective collision
c    strenghts for He-like ions according to Kato & Nakazaki (1989)
c    eq. (6). m is the dimension of dtype69
c      author: M. Bautista
c
       implicit none
       include './PARAM'
c
       integer m
       real*8 rdat1(nrdat1)
       real*8 gamma,temp,eboltz,dele,y
       real*8 a,b,c,d,e,p,q,x1,em1,gnr,gr
       integer np1r
       integer lpri,lun11
c
       eboltz=1.160443e+04
       dele=rdat1(np1r)
       y=dele/temp*eboltz
       if (lpri.gt.1) write (lun11,*)'in calt69',dele,temp,y
c     
       if (y.lt.1.e-20) then
        print*,'error in calt69. y too low. y=',y
        return
       endif
       if (y.gt.1.e+20) then
        gamma=0.
        return
       endif
c
       if (y.gt.77.) y=77.
       if (y.lt.5.e-2) y=5.e-2
       call expint(y,em1)
       a=rdat1(np1r-1+2)
       b=rdat1(np1r-1+3)
       c=rdat1(np1r-1+4)
       d=rdat1(np1r-1+5)
       e=rdat1(np1r-1+6)
       if (m.eq.6) then
        gamma=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
        if (lpri.gt.1) write(lun11,*)'m=6',temp,y,em1,gamma
       else
        p=rdat1(np1r-1+7)
        q=rdat1(np1r-1+8)
        x1=rdat1(np1r-1+9)
        call expint(y*x1,em1)
        gnr=a/y+c/x1+d*.5*(1./x1/x1-y/x1)+e/y*log(x1)+
     #   em1/y/x1*(b-c*y+d*y*y*.5+e/y)
        gnr=gnr*y*exp(y*(1.-x1))
        gr=p*(1.+1./y)*(1.-exp(y*(1.-x1))*(x1+1/y)/(1.+1./y)) +
     #     q*(1.-exp(y*(1.-x1)))
        gamma=gnr+gr
        if (lpri.gt.1) write(lun11,*)'m>6',temp,y,em1,gamma,q,
     $     gnr,gr,x1,a,b,c,d,e,p,q
       endif
c
      return
      end
