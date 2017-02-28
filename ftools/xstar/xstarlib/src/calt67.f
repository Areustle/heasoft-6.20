       subroutine calt67(temp,np1r,rdat1,gamma)
c
c   Takes coefficients in data type 67 and returns effective collision
c    strenghts for He-like ions according to Keenan, McCann, & Kingston
c    (1987) eq. (2)
c      author: M. Bautista
c
       implicit none
c
      include './PARAM'
c
       real*8 rdat1(nrdat1)
       real*8 gamma,temp,tp
       integer np1r
c
       tp=log10(temp)
       gamma=rdat1(np1r-1+1)+rdat1(np1r-1+2)*tp+rdat1(np1r-1+3)*tp*tp
c
       return
       end
