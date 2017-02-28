      subroutine amcol(n,l,temp,ic,z1,rm,ne,sum,ecm,cn)
c
c subroutine amcol determines the rate of angular momentum changing
c collisions in hydrogenic atoms due to collisions with ions.
c the codes are based on the method of hummer & storey (1987)
c      author: M. Bautista
c
c        z1=charge of colliding ion
c        ic=ionic charge of hydrogenic atom
c        rm=reduced mass of colliding system in units of electron mass
c        ne=electron number density
c        sum = sum of spontaneous transitions out of level n,l
c        cn = transition rate for nl -> nl-1
c        cn(nl -> nl-1) = cn
c        cn(nl -> nl+1) = cn*(2.*l+1)/(2.*l-1)
c
      implicit none
c
      real*8 ne,temp,z1,rm,sum,ecm,cn
      real*8 pc1,pc2,pc3,dnl,pc,qnl,den
      integer n,l,ic
c
      pc1=1.181+log10(temp/ne)
      pc2=pc1
      pc3=pc1
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      if(sum.ne.0.) pc2=10.95+log10(temp/rm/sum/sum)
      if(ecm.ne.0.) pc3=log10(temp/rm/ecm/ecm)-11.22
      pc=min(pc1,pc2,pc3)
      qnl=9.933e-6*sqrt(rm/temp)*dnl*(11.538+log10(temp/dnl/rm)+pc)
c
      den=l*(n*n-l*l)+(l+1)*(n*n-(l+1)*(l+1))
      cn=qnl*l*(n*n-l*l)/den
c
      return
      end
