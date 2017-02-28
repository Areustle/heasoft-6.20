c_______________________________________________________________
      real*8 function calc_sampson_s(om, Z, Te) 

      real*8 om(14)
      integer Z
      real*8 Te
      
c  /* These routines come from Sampson, Goett, & Clark, ADNDT 29, 467 */
        
      real*8 result
      real*8 kT, y
      real*8 dE, z2s_h, a1_gam, a1e_gam, a2, c0, c1, c2
      real*8 a2e, cere, cere1, s, se
      real*8 a1, a1y, E1y, Ery, Er1y
      real*8 Zeff, Zeff_e, term
      integer re
      real*8 Z2gamma, Z2gamma_e,exint_n,pow
      real*8 KBOLTZ,fre,frem
      data KBOLTZ/8.617385e-8/
c      /* kB = (keV/K) */
      
      Z2gamma=0.0
      Z2gamma_e=0.0
      dE     = om(1)
      a1_gam = om(2)
      a1e_gam= om(3)
      z2s_h  = om(4)
      a2     = om(5)
      c0     = om(6)
      c1     = om(7)
      c2     = om(8)
      a2e    = om(9)
      cere   = om(10)
      cere1  = om(11)
      re     = om(12)
      s      = om(13)
      se     = om(14)
        
      kT = KBOLTZ*Te
      y = dE/kT
      
      a1 = a2+1.0
      a1y = a1*y
      E1y = exint_n(y,-1.d0,1)
      Ery = exint_n(a1y,-1.d0,1)
      Er1y= exint_n(a1y,Ery,2)
        
      term = (c1*Ery + c2*Er1y/a1)
      if ((a1_gam .ne.0.0).and.(term.gt.0)) then
          Z2gamma = c0 + 1.333*z2s_h*E1y*exp(y) + y*exp(a1y)*term
       else
c          { /* Avoid doing exponential */
          Z2gamma = c0 + 1.333*z2s_h*E1y*exp(y)
       endif
      
      a1 = a2e+1
      a1y = a1*y
      Ery = exint_n(a1y,-1.d0,re)
      Er1y = exint_n(a1y,-1.d0,re+1)
      
      fre=re
      frem=re-1
      term = (cere*Ery/(pow(a1,frem)) + cere1*Er1y/(pow(a1,fre)))
      if ((a1e_gam .ne. 0.0).and. (term .gt. 0)) then
          Z2gamma_e = y*exp(a1y)*term
         else 
          Z2gamma_e = 0.0
         endif
      
      Zeff = Z - s
      Zeff_e = Z - se
        
      result = a1_gam*Z2gamma/(Zeff*Zeff)
     $  + a1e_gam*Z2gamma_e/(Zeff_e*Zeff_e)
      
      calc_sampson_s=result
      return
      end
