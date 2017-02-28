      subroutine comp2(lpri,lun11,epi,ncn2,bremsa,t,
     $  r,decomp,ecomp,sxcomp,cmp1,cmp2)
c
c     this sub-routine computes the heating - cooling due to compton
c     scattering.  the rate is returned in the common block coheat.
c     relativistic version, using rates from Guilbert (1986)c
c     author:  T. Kallman (from xstar1)
c
      implicit none
c

      include './PARAM'
c
      real*8 decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
      real*8 bremsa(ncn),epi(ncn)
      real*8 t,r,cmp1,cmp2,emc2,sigth0,tmp1,
     $     ekt,xx,sxx,zrmstp,eee,ee,sum1,sum2,sum3,tmp1o,eeeo,
     $     eeo,ans,cfake,hfake,cohc,cmpfnc
      integer lpri,lun11,ncn2,lprisv,numcon,kl
c
c     Not used
      real*8 javir
c
      data emc2/5.11e+5/,sigth0/6.65e-25/
c
      javir=r
c      r=javir
c
      lprisv=lpri
c      lpri=2
      if (lpri.ge.1) write (lun11,*)'in comp2'
c
      sigth0 = 6.65e-25
      tmp1 = 0.
c
      ekt = t*0.861707
      xx = emc2/(ekt+1.e-10)
      sxx = 1./xx
      zrmstp = bremsa(1)
      eee = epi(1)
      ee = eee/emc2
      tmp1 = zrmstp*cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
      sum1 = 0.
      sum2 = 0.
      sum3 = 0.
      numcon=ncn2
      do kl = 2,numcon
         tmp1o = tmp1
         eeeo = eee
         eeo = ee
         eee = epi(kl)
         ee = eee/emc2
         zrmstp = bremsa(kl)
         tmp1 = zrmstp*cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
         sum1 = sum1 + (tmp1+tmp1o)*(eee-eeeo)/2.
         sum2 = sum2 + (bremsa(kl)+bremsa(kl-1))*(eee-eeeo)/2.
         sum3 = sum3 + (bremsa(kl)*ee+bremsa(kl-1)*eeo)*(eee-eeeo)/2.
         if (lpri.ne.0) write (lun11,*)kl,eee,ee,zrmstp,tmp1,sum1
         enddo
      ans = sum1
      cfake=sum2*sigth0
      hfake=sum3*sigth0
      cohc = -ans*sigth0
      cmp1=hfake
      cmp2=(-cohc+hfake)/ekt

       
      if (lpri.ne.0)
     $ write (lun11,*)'cmp1,cmp2:',cmp1,cmp2,cfake,hfake
c
      lpri=lprisv
c
      return
      end
