      function voigte(vs,a)
c      real*8 function voigtevoigt(vs,a)
      implicit none
c
c     computes a voigt function  h = h(a,v)
c     a=gamma/(4*pi*dnud)   and  v=(nu-nu0)/dnud.  this  is  done after
c     traving (landolt-b\rnstein, p. 449).
c     author:  tlusty
c
c     the integral of this function over vs (-infinity -> infinity) is sqrt(pi)
c
      real*8 vs, a, un, two, voigte, sqp, sq2
      real*8 v, u, v2, ex, quo, h1, h, pqs, h1p
      real*8 h2p, h3p, h4p, psi, a2, u2
      real*8 ak(19),a1(5)
      integer k, m, i
      parameter (un=1., two=2.)
c
      data ak      /-1.12470432, -0.15516677,  3.28867591, -2.34357915,
     ,  0.42139162, -4.48480194,  9.39456063, -6.61487486,  1.98919585,
     , -0.22041650, 0.554153432, 0.278711796,-0.188325687, 0.042991293,
     ,-0.003278278, 0.979895023,-0.962846325, 0.532770573,-0.122727278/
      data sqp/1.772453851/,sq2/1.414213562/
c
      v = abs(vs)
      u = a + v
      v2 = v*v
      if (a.eq.0.0) go to 140
      if (a.gt.0.2) go to 120
      if (v.ge.5.0) go to 121
c
      ex=0.
      if(v2.lt.100.)ex = exp(-v2)
      k = 1
c
  100 quo = un
      if (v.lt.2.4) go to 101
      quo = un/(v2 - 1.5)
      m = 11
      go to 102
c
  101 m = 6
      if (v.lt.1.3) m = 1
  102 do 103 i=1,5
         a1(i) = ak(m)
         m = m + 1
  103 continue
      h1 = quo*(a1(1) + v*(a1(2) + v*(a1(3) + v*(a1(4) + v*a1(5)))))
      if (k.gt.1) go to 110
c
c a le 0.2  and v lt 5.
c
      h = h1*a + ex*(un + a*a*(un - two*v2))
      voigte=h
      return
c
  110 pqs = two/sqp
      h1p = h1 + pqs*ex
      h2p = pqs*h1p - two*v2*ex
      h3p = (pqs*(un - ex*(un - two*v2)) - two*v2*h1p)/3. + pqs*h2p
      h4p = (two*v2*v2*ex - pqs*h1p)/3. + pqs*h3p
      psi = ak(16) + a*(ak(17) + a*(ak(18) + a*ak(19)))
c
c 0.2 lt a le 1.4  and  a + v le 3.2
c
      h = psi*(ex + a*(h1p + a*(h2p + a*(h3p + a*h4p))))
      voigte=h
      return
c
  120 if (a.gt.1.4.or.u.gt.3.2) go to 130
      ex=0.
      if(v2.lt.100.)ex = exp(-v2)
      k = 2
      go to 100
c
c a le 0.2  and  v ge 5.
c
  121 h = a*(15. + 6.*v2 + 4.*v2*v2)/(4.*v2*v2*v2*sqp)
      voigte=h
      return
c
  130 a2 = a*a
      u = sq2*(a2 + v2)
      u2 = un/(u*u)
c
c a gt 1.4  or  a + v gt 3.2
c
      h = sq2/sqp*a/u*(1. + u2*(3.*v2 - a2) +
     ,        u2*u2*(15.*v2*v2 - 30.*v2*a2 + 3.*a2*a2))
      voigte=h
      return
c
c a eq 0.
c
  140 h=0.
      if(v2.lt.100.)h=exp(-v2)
      voigte=h
      return
      end
