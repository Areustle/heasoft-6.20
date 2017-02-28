       subroutine calt60_62(temp,m,idata,np1r,np1i,rdat1,idat1,Upsilon)
c
c  This rutine takes the coefficients in data type 60 and 62 (reals
c  as dtype and integers as itype) and return effective collision
c  strengths according to fits by Callaway (1994).
c  "temp" is the temperature in Kelvin and "m" is the number of
c  reals in dtype. "idata" is the data type either 60 or 62.
c      author: M. Bautista
c
        implicit none
        include './PARAM'        
c
        integer m
        real*8 rdat1(nrdat1)
        integer idat1(nidat1)
c        real*8 dtype(m)
        real*8 temp,Upsilon
c        integer itype(m)
        integer i,idata,np1i,np1r
        real*8 t1,de,tmax,tt,rat
c
        t1=temp*6.33652e-6
        if (temp.gt.1.e9) t1=6.33652e+3
        de=1./float(idat1(np1i))**2-1./float(idat1(np1i-1+2))**2
        tmax=4.*de
        tmax=1.
        tt=t1
        if (t1.gt.tmax) tt=tmax
        if (idata.eq.60) then
         rat=0.
         do i=1,m-2
          rat=rat+rdat1(np1r-1+i+2)*(tt**(i-1))
         enddo
         Upsilon=rat
        else
         rat=0.
         do i=1,m-5
          rat=rat+rdat1(np1r-1+i+2)*(tt**(i-1))
         enddo
         Upsilon=rat+rdat1(np1r-1+m-2)*log(rdat1(np1r-1+m-1)*tt)
     $               *exp(-rdat1(np1r-1+m)*tt)
        endif
c
         if (t1.gt.tt) then
          upsilon=Upsilon*(1.+log(t1/tmax)/(log(t1/tmax)+1.))
         endif
c
      return
      end
