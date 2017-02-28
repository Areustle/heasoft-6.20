      subroutine amcrs(n,l,temp,ic,z1,rm,ne,sum,ecm,psi,il,cn,
     $                 lpri,lun11)
c
c the angular momentum changing collision rates are calculated using
c either the pengelly & seaton (1964) formula (amcol) or the impact
c parameter method of seaton (1962) (impact) if the energy levels are
c non-degenerate.  the ps routine is used if the ratio of amcol/impact
c is greater than 0.94 since this code is faster.  ** beware - there
c may be problems if ne is too large ( > 1.e+7).  pc1 will be used in
c amcol rather than pc3 and the change will not occur.
c      author: M. Bautista
c
c     n = principal quantum number of initial state
c     l = orbital quantum number of initial state
c     temp = temperature in kelvin
c     ic = ionic charge of target particle
c     z1 = charge of incident particle
c     rm = mass of incident particle in units of electron mass me
c     ne = electron number density
c     sum = total spontaneous transition rate out of n,l
c     cn = transition rate for nl -> nl-1
c     ecm = energy difference between nl and nl-1
c     psi = see notes for defn
c     il = flag to decide whether to use impact or amcol
c     cn = transition rate for nl -> nl-1
c
      implicit none
c
      real*8 ne
      real*8 temp,z1,rm,sum,ecm,psi,cr
      real*8 en,dnl,rho1,rhom,cn,rat
      integer n,l,ic,il,lun11,lpri
c
      en=real(n)
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      rho1=0.72/sum
      if(ecm.ne.0.) rho1=min(rho1,5.946e-12/ecm)
      rhom=3.929e11*rho1*temp/sqrt(dnl)/rm
      if(rhom.lt.10.) go to 30
      call amcol(n,l,temp,ic,z1,rm,ne,sum,ecm,cn)
      cn=0
c mab il=0
        il=0
        if(ecm.ne.0.) then
          if(il.eq.0) then
          if (lpri.gt.1)
     $    write (lun11,*)'call impact 1',en,l,temp,ic,z1,rm,ecm,psi
          call impact(en,l,temp,ic,z1,rm,ecm,psi,cr)
          rat=cn/cr
          cn=cr
          if(rat.gt. 0.94) il=1
          endif
        endif
c     go to 40
c
 30    if(ecm.eq.0.) then
      call velimp(n,l,temp,ic,z1,rm,ne,sum,cn)
      else
      if (lpri.gt.1)
     $write (lun11,*)'call impact 2',en,l,temp,ic,z1,rm,ecm,psi
      call impact(en,l,temp,ic,z1,rm,ecm,psi,cn)
      endif

c      if(ne.gt.1.e14) then
c     call impact(en,l,temp,ic,z1,rm,ecm,psi,cn)
c      endif
c
c 40    continue
c
      return
      end
