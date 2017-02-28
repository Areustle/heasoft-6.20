c_______________________________________________________________
      real*8 function calc_sampson_p(om, Z,Te) 
      
      real*8 om(8)
      real*8 result
      real*8 kT, y
      real*8 dE, a, z2s, c0, cr, cr1, r, s
      real*8 Z2gamma, Zeff, a1, a1y, E1y, Ery, Er1y
      real*8 term,te,exint_n,pow
      integer z
      
      dE  = om(1)
      a   = om(2)
      z2s = om(3)
      c0  = om(4)
      cr  = om(5)
      cr1 = om(6)
      r   =  om(7)
      s   = om(8)
      
      kT = KBOLTZ*Te
      y  = dE/kT
      
      a1 = a+1
      a1y = a1*y
      
      E1y = exint_n(y,-1.d0,1)
      Ery = exint_n(a1y,-1.d0,int(r))
      Er1y= exint_n(a1y,-1.d0,int(r+1))
      
      term = (cr*Ery/pow(a1,r-1) + cr1*Er1y/pow(a1,r))
      if (term.gt. 0) then
          Z2gamma = c0 + 1.333*z2s*E1y*exp(y) + y*exp(a1y)*term
        else 
          Z2gamma = c0 + 1.333*z2s*E1y*exp(y)
        endif
      
      Zeff = Z - s
          
      result = Z2gamma / (Zeff*Zeff)
      
      calc_sampson_p=result
c
      return
      end
