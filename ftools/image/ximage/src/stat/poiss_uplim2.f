      real*4 function poiss_uplim2(S,n)
c 
c Poisson Upper Limit Aproximate expression 
c Mon.Not.Roy.Astron.Soc. 349 (2004) 768, astro-ph/0301285
c
c import :
c     S = Confidence level
c     n = number of events
c
      REAL*4 S
      REAL*4 n
c
c output :
c pois_uplim2 = Eq (4) result 
c
c local variables :
c
      REAL*4 tmpn,den1,den2,ngterm
      REAL*4 bfun, cfun      
      REAL*4 b0,b1,b2,b3,b4,b5,b6,b7,b8
      REAL*4 c10,c11,c12,c13,c14
      REAL*4 c20,c21,c22,c23,c24
      REAL*4 c30,c31,c32,c33
      REAL*4 c40,c41,c42,c43,c44,c45,c46,c47
      REAL*4 S01,S02,Stemp1,Stemp2,dStemp1,dStemp2,lStemp1,lStemp2

      den1 = 0.
      den2 = 0.
      tmpn = n+1.
      
      S01=0.50688
      S02=2.27532
      
c     b coeficients
      b0=-3.8954E-3
      b1= 6.2328E-3
      b2= 5.2345E-3
      b3=-5.3096E-3
      b4= 1.3093E-3
      b5=-2.0344E-4
      b6= 2.0393E-5
      b7=-1.1974E-6
      b8= 3.1161E-8

c     b as a function of s (b(S)):
      
      bfun=S*(b8+b7+b6+b5+b4+b3+b2+b1+b0)
       
c     c coeficients:
      
      c10=-2.0799E+0
      c11=-7.1925E-1
      c12=-4.0064E-1
      c13=-7.3386E-2
      c14=-5.4791E-3
      
      c20=-1.4354E+0
      c21=-6.3188E-1
      c22=-1.6177E-1
      c23=-5.6966E-1
      c24=-2.2835E-1
      
      c30=-8.4098E-1
      c31= 6.8766E-1
      c32= 2.0358E-1
      c33= 3.9965E-2
     
      c40=-1.0120E+0
      c41=-2.8853E-1
      c42= 4.2013E-1
      c43=-5.3310E-2
      c44=-1.6319E-2
      c45= 4.8667E-2
      c46=-5.5299E-2
      c47=-3.3361E-2
      
c     c as a function of S (c(S)):
      
      Stemp1= S-S01
      dStemp1=1./Stemp1
      lStemp1=LOG10(Stemp1)
      
      Stemp2= S-S02
      dStemp2=1./Stemp2
      lStemp2=LOG10(Stemp2)
      
      
      IF (S.LT.S01) then
         cfun=c10+c11*dStemp1+c12*(dStemp1)**2
         cfun=cfun+c13*(dStemp1)**3+c14*(dStemp1)**4
      ENDIF
      IF ((S.GT.S01).AND.(S.LT.1.2)) then
         cfun=c20+c21*lStemp1
         cfun=cfun+c22*((lStemp1)**2)
         cfun=cfun+c23*((lStemp1)**3)+c24*((lStemp1)**4)
      ENDIF
      IF ((S.GT.1.2).AND.(S.LT.S02)) then
         cfun=c30+c31*dStemp2+c32*(dStemp2)**2
         cfun=cfun+c33*(dStemp2)**3
      ENDIF
      IF (S.GT.S02) then
         cfun=c40+c41*lStemp2
         cfun=cfun+c42*((lStemp2)**2)
         cfun=cfun+c43*((lStemp2)**3)+c44*((lStemp2)**4)
         cfun=cfun+c45*((lStemp2)**5)
         cfun=cfun+c46*((lStemp2)**6)+c47*((lStemp2)**7)
      ENDIF
      
      IF((S.EQ.S01).OR.(S.EQ.S02)) then
         cfun=0
         bfun=0         
      ENDIF
            
      IF (tmpn.GT.-1) then
         den1 = 9.*(tmpn)
         den2 = 3.*SQRT(tmpn)
         ngterm=1.-1./den1+S/den2
         poiss_uplim2 = (tmpn)*(ngterm+bfun*((tmpn)**cfun))**3 
         poiss_uplim=(tmpn)*(ngterm)**3
  
      ELSE
         poiss_uplim2 = 0.0
      ENDIF
      return
      END
