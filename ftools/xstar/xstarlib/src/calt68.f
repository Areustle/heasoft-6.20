       subroutine calt68(temp,np1r,np1i,rdat1,idat1,gamma)
c
c    Takes coefficients in data type 68 and returns effective collision
c    strenghts for He-like ions according to Sanpson & Zhang.
c      author: M. Bautista
c
       implicit none
       include './PARAM'
c
       real*8 rdat1(nrdat1)
       integer idat1(nidat1)
       real*8 z,tt,gamma,temp
       integer np1r,np1i
c
       z=float(idat1(np1i-1+3))
       tt=log10(temp/z/z/z)
       gamma=rdat1(np1r-1+1)+rdat1(np1r-1+2)*tt+rdat1(np1r-1+3)*tt*tt
c
       return
       end
