      subroutine freef(lpri,lun11,epi,ncn2,t,xpx,xee,opakc)
c
c     this sub-routine computes the free-free opacity and 
c     include it into the total one (opakc)
c     author:  J. Garcia (July 2008)
c
      implicit none
c
      include './PARAM'
c
      real*8 opakc(ncn),epi(ncn)
      real*8 t, opaff, ekt, t6, temp
      real*8 xpx, xee, xnx, enz2, cc
      real*8 gam, gau, fbg, zz
      integer numcon,lpri,lun11,ncn2,kk
c
c
      data cc/2.614e-37/
c
      if (lpri.gt.0) write (lun11,*)'in freef',t
c
      numcon=ncn2
      xnx=xpx*xee
      ekt = t*(0.861707)
      t6 = t/100.
      enz2=(1.4)*xnx
      zz=1.
      do kk=1,numcon
         temp = epi(kk)/ekt
         gam = zz*zz*(0.158)/t6
         gau = 1.
!         if ( temp.lt.100. ) gau = fbg(temp,gam)
!         if ( temp.lt.100. )
!     1    gau = 10.**(0.2258*epi(kk)**(0.08)*(4.094-log10(epi(kk)))    !JG
!     2      +log10(t6)*(0.133*(4.094-log10(epi(kk)))-0.2)-0.538)   !JG

         opaff = cc*xnx*enz2*gau/sqrt(t)/epi(kk)**3.
     1           *(1. - exp(-temp))

         opakc(kk) = opakc(kk) + opaff
c
!!! THIS IS A TEST
!         if(opakc(kk).gt.6.65e-7)opakc(kk)=6.65e-7
c
      enddo
c
      return
      end
