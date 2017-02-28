      subroutine ispec4(tp,xlum,epi,ncn2,zremsz,lpri,lun11)
c
c     this subroutine generates the initial spectrum.
c     power law spectrum
c     brems stores the flux to be used
c     author:  T. Kallman
c
c
      implicit none
c
      include './PARAM'
c
      real*8 zremsz(ncn),epi(ncn)
      integer ncn2,lpri,lun11
      real*8 zremsi(ncn),ergsev,const,xlum
      real*8 sum,ecut
      integer i,numcon,nb1,nb2,nbinc,lprisv
      real*8 tp
c
      data ergsev/1.602197e-12/
c
      numcon=ncn2
      ecut=0.01
      sum=0.
      lprisv=lpri
      if (lpri.ge.1) write (lun11,*)'in ispec4',tp,xlum
      nb1=nbinc(13.6d0,epi,ncn2)
      nb2=nbinc(1.36d+4,epi,ncn2)
      do i=1,numcon
         zremsi(i)=1.e-24
         if (epi(i).gt.ecut)
     $    zremsi(i)=epi(i)**tp
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsi(i)
         if ((i.ge.nb1).and.(i.le.nb2))
     $    sum=sum+(zremsi(i)+zremsi(i-1))*(epi(i)-epi(i-1))/2.
         enddo
c
      const=xlum/sum/ergsev
      do i=1,numcon
         zremsz(i)=zremsz(i)+zremsi(i)*const
         if (lpri.ge.1)
     $        write (lun11,*)i,epi(i),zremsi(i),const,zremsz(i)
         enddo
      lpri=lprisv
c
      return
      end
