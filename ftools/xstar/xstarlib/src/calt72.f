      subroutine calt72(temp,np1r,rdat1,nrdt,rate,lun11,lpri)
c
c   Takes coefficients in data type 72 and returns capture rates
c   (in s^-1) for DR through satellite levels considered explicitly.
c      author: M. Bautista
c
       implicit none
       include './PARAM'
c
c
       real*8 rdat1(nrdat1),temp,rate,dele,s,rtmp,temp4,ekt
       integer np1r,nrdt
       integer lpri,lun11
c     
       dele=rdat1(np1r-1+2)
       s=4.141292e-22/(temp**1.5)
c      nb this constant differs from Bely-Dubau et al. 1982 by 10^6
c       s=s*1.e+6
       rtmp=rdat1(np1r+2)
       temp4=temp/1.e+4
       ekt=0.861707*temp4
       if (nrdt.lt.3) rtmp=1.
       rate=s*exp(-dele/ekt)*rdat1(np1r-1+1)*rtmp
c     $          /2.*.5
       if (lpri.gt.1) write (lun11,*)'in calt72:',dele,s,temp,rtmp,rate
c     
       return
       end
