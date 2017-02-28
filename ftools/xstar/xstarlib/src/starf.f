      subroutine starf(tp,xlum,epi,ncn2,zremsz,lpri,lun11)
c
c     this subroutine generates the initial spectrum.
c      optically thin bremsstrahlung spectrum
c     brems stores the flux to be used
c     author:  T. Kallman (from xstar1)
c
      implicit none
c
      include './PARAM'
c
      real*8 epi(ncn),zremsz(ncn)
      real*8 zremsi(ncn)
      real*8 ergsev,del,q,xkt,sum,tempp,sum2,xlum,tp,const
      integer numcon,ncn2,lpri,lprisv,i,lun11
c
      data ergsev/1.602197e-12/
c
      numcon=ncn2
      del=1.
      q=7.49e+08*del*xlum/(1.e-37+tp)
      xkt=1.16e-03/(1.e-37+tp)
      sum=0.
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1) write (lun11,*)'in starf',tp,xlum,q,xkt
      do i=1,numcon
         tempp=epi(i)*xkt
         zremsi(i)=0.
c         zremsi(i)=(3.1415e+22)*epi(i)**3/exp(tempp)
         if (tempp.lt.1.e-3) then
             zremsi(i)=epi(i)**3/tempp
           else
             if (tempp.lt.150.)
     $         zremsi(i)=epi(i)**3/(exp(tempp)-1.)
             if (tempp.gt.150.) zremsi(i)=xlum/epi(i)/ergsev/1.e+37
           endif
         zremsi(i)=(3.1415e+22)*zremsi(i)
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsi(i)
         if (.not.((epi(i).lt.13.6).or.(epi(i).gt.1.36e+4)
     $        .or.(i.le.1)))
     $    sum=sum+(zremsi(i)+zremsi(i-1))*(epi(i)-epi(i-1))/2.
         enddo
c
      const=xlum/sum/ergsev
      sum2=0.
      do  i=1,numcon
         zremsz(i)=zremsz(i)+zremsi(i)*const
         if (i.gt.1)
     $    sum2=sum2+(zremsz(i)+zremsz(i-1))*(epi(i)-epi(i-1))/2.
         if (lpri.gt.1)
     $        write (lun11,*)i,epi(i),zremsi(i),const,zremsz(i)
         enddo
      sum2=sum2*ergsev
c      write (lun11,*)'normalization:',sum2
      lpri=lprisv
c
      return
      end
