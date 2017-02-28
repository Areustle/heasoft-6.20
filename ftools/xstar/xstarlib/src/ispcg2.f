      subroutine ispcg2(zremsz,epi,ncn2,enlum,lpri,lun11)
c
c     this subroutine calculates number luminosity
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      real*8 zremsz(ncn),epi(ncn)
      integer ncn2,lpri,lun11
      real*8 enlum
      real*8 sum2,sum3,sum4,sum5
      integer jk
      integer numcon
c
      if (lpri.ge.1) write (lun11,*)'in ispec2'
      numcon=ncn2
      sum2 = 0.
      sum3 = 0.
      sum4 = 0.
      sum5 = 0.
      do jk = 1,numcon
         if (jk.gt.1) 
     $     sum5 = sum5+(zremsz(jk)+zremsz(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.
         if ( epi(jk).ge.13.6 ) then
            sum2 = sum2+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.
           if ( epi(jk).le.24.48 )
     $        sum3 = sum3+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.

         endif
         if ((epi(jk).ge.24.48).and.(epi(jk).le.54.4))
     $     sum4 = sum4+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.
          if (lpri.ge.1)
     $     write (lun11,*)jk,epi(jk),zremsz(jk),sum2
          enddo
      enlum = sum2
      write (lun11,*)'U(1-1.8),U(1.8-4):',sum3,sum4
      write (lun11,*)'Lbol=',sum5*1.602197e-12
c
      return
      end
