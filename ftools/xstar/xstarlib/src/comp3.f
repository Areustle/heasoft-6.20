      subroutine comp3(lpri,lun11,epi,ncn2,bremsa,r,cmp1,cmp2)
c
c
c     this subroutine computes the heating - cooling due to compton
c     scattering.  the rate is returned in the common block coheat.
c      using ferland expression
c
      parameter (ncn=999999)
c
      real*8 bremsa(ncn),epi(ncn),r,cmp1,cmp2
c
c
      data c2/8.219e-06/
c
      lprisv=lpri
c      lpri=3
      if (lpri.ge.1) write (lun11,*)'in comp'
c
      sigth = 6.65e-25
      c1=1.95639e-6
      tmp1 = 0.
      tmp2 = 0.
      c2 = 0.
c     trying the old expression
c      c2 = 21./5./5.11e+5
      r19=r/1.e+19
      fpr2=r19*r19
c
c     due to continuum.
      ery=epi(1)/13.6
      alpha=1./(1.+ery*(1.1792e-4+7.084e-10*ery))
      beta=1.-alpha*ery*(1.1792e-4+2.*7.084e-10*ery)/4.
      fac1 = sigth*bremsa(1)*epi(1)*alpha
      fac3 = sigth*bremsa(1)*4.*alpha*beta
      numcon=ncn2
      do 100 i = 2,numcon
         ery=epi(i)/13.6
         alpha=1./(1.+ery*(1.1792e-4+7.084e-10*ery))
         beta=1.-alpha*ery*(1.1792e-4+2.*7.084e-10*ery)/4.
         delt = epi(i) - epi(i-1)
         fac2 = sigth*bremsa(i)*epi(i)*alpha
         tmp1 = tmp1 + (fac1+fac2)*delt/2.
         fac1 = fac2
         fac4 = 4.*sigth*bremsa(i)*alpha*beta
         tmp2 = tmp2 + (fac3+fac4)*delt/2.
         fac3 = fac4
         if ( lpri.gt.2 ) write (lun11,99001) i,epi(i),bremsa(i),
     &                           fac1,fac3,tmp1,tmp2
 100  continue
c
      ebar = tmp1*4./(1.e-30+tmp2)
      if ( lpri.gt.2 ) write (lun11,*) 'ebar=',ebar
c
c
      if (lpri.gt.2)  write (lun11,*)c1,tmp1,tmp2
      cmp1 = c1*tmp1
      cmp2 = c1*tmp2
      if (lpri.gt.2) write (lun11,*)cmp1,cmp2
      lpri=lprisv
c
      return
99001 format (' ',i4,6e12.4)
      end
