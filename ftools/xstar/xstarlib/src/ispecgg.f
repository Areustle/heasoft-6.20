      subroutine ispecgg(xlum,epi,ncn2,zremsz,
     $               lpri,lun11)
c
c     this subroutine generates the initial spectrum.
c     renormalization
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      real*8 epi(ncn),zremsz(ncn)
      integer numcon,ncn2,i
      real*8 ergsev,sum,const,xlum
      integer lpri, lun11
c
      data ergsev/1.602197e-12/
c
      numcon=ncn2
      sum=0.
      if (lpri.gt.1) write (lun11,*)'in ispec',xlum
      do i=1,numcon
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsz(i)
         if ((epi(i).ge.13.6).and.(epi(i).le.1.36e+4)
     $        .and.(i.gt.1))
     $    sum=sum+(zremsz(i)+zremsz(i-1))*(epi(i)-epi(i-1))/2.
         enddo
c
      const=xlum/sum/ergsev
      do i=1,numcon
         zremsz(i)=zremsz(i)*const
         if (lpri.gt.1)
     $        write (lun11,*)i,epi(i),const,zremsz(i)
         enddo
c
      return
      end
