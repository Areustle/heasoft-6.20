      subroutine calt66(temp,np1r,rdat1,nrdt,gamma)
c
c   Takes coefficients in data type 66 and returns effective collision
c    strenghts for He-like ions according to Kato & Nakazaki (1989)
c    eq. (6).
c      author: M. Bautista
c
       implicit none
       include './PARAM'
c
       real*8 rdat1(nrdat1)
       real*8 temp,gamma
       real*8 eboltz,y,a,b,c,d,e,em1,gam1,dele,gam2,gam3
       integer np1r,nrdt
c
       eboltz=1.160443e+04
       dele=rdat1(np1r)
       y=dele/temp*eboltz
c
       if (y.lt.1.e-20) then
        print*,'error in calt66. y too low. y=',y
        return
       endif
       if (y.gt.1.e+20) then
        gamma=0.
        return
       endif
c
       if (y.gt.77.) y=77.
       call expint(y,em1)
       a=rdat1(np1r-1+2)
       b=rdat1(np1r-1+3)
       c=rdat1(np1r-1+4)
       d=rdat1(np1r-1+5)
       e=rdat1(np1r-1+6)
       gam1=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
       gam2=0.
       gam3=0.
       if (nrdt.gt.6) then
         dele=rdat1(np1r-1+7)
         y=dele/temp*eboltz
         if (y.gt.77.) y=77.
         call expint(y,em1)
         a=rdat1(np1r-1+8)
         b=rdat1(np1r-1+9)
         c=rdat1(np1r-1+10)
         d=rdat1(np1r-1+11)
         e=rdat1(np1r-1+12)
         gam2=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
         dele=rdat1(np1r-1+13)
         y=dele/temp*eboltz
         if (y.gt.77.) y=77.
         call expint(y,em1)
         a=rdat1(np1r-1+14)
         b=rdat1(np1r-1+15)
         c=rdat1(np1r-1+16)
         d=rdat1(np1r-1+17)
         e=rdat1(np1r-1+18)
         gam3=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
         endif
       gamma=gam1+gam2+gam3
       return
       end
