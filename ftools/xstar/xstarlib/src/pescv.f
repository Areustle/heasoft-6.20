      real*8 function pescv(tau)
c
c     this routine calculates escape probability for
c       continuum
c     inputs: optical depths-- tau, energy
c     author:  T. Kallman
c
      implicit none
c
      real*8 tau
      real*8 taubar,eps
c
c     NB optically thin rrcs 
c     test for lte
      pescv=0.5
c      return
c      if (e.lt.13.6) return
c     fudge because of too much case b
c      taubar=tau/200.
c      taubar=tau/5.
c      taubar=tau*100.
c      pescv=1./(taubar+1.)
c      pescv=expo(-taubar)
      pescv=exp(-tau)
c     fudge because of numerical problem with large tau.
      eps=1.e-12
      pescv=max(pescv,eps)
      pescv=pescv/2.
c
      return
      end
