
      subroutine pexs(nmin,kdim,zc,eion,far,gam,scal,
     +                e,axs,ierr,lpri,lun11)
c
c     Compute photoexcitation cross-section assuming one
c     Rydberg serie converging to a threshold.
c
c     nmin = starting princ. quant. num. of the serie
c     zc = effective charge, Z-Ne+1
c     eion = threshold energy in Ry
c     far = oscillator strength of the serie member with
c            n=nmin
c     gam = resonance width in Ry
c     e = external energy grid in Ry
c     kdim = dimension of the external energy grid
c     axs = cross section
c     scal = scaling factor
c     ierr = error indicator (=0: OK; .ne.0:error)
c     author:  P. Palmeri
c
      implicit none
c
      integer nmax,nmin
      real*8  pi
c
      parameter(pi=3.14159d0,nmax=30)
c
      real*8  x(nmax),a(nmax),e(*),axs(*)
      real*8  zc,eion,far,gam,scal
      integer lpri,ierr,lun11,kdim,im
      integer nres,jmin,jmax,jres,jj,i,ii,ij,kk,n
      real*8  del,xmin,xres,axtp,res
c
      data x,a/nmax*0.,nmax*0./
c
      if (lpri.ne.0) write (lun11,*)'in pexs',nmin,kdim,zc,
     $        eion,far,gam,scal
c
      ierr=0
      if(nmin.ge.nmax) then
       ierr=1
       return
      endif
      nres=nmax
      do 10 n=nmin,nmax
c
c   energy of the resonance ...
c
       x(n)=-(zc/dble(n))**2.d0
c
c   area of the resonance ...
c
       a(n)=8.06725d0*far*dble(nmin**3)/dble(n**3)
c
c   search for unresolved limit in term of member ...
c
       if(n.gt.nmin) then
        del=x(n)-x(n-1)
         res=gam/2.d0
        if(del.gt.res) nres=n
       endif

        if (lpri.ne.0) write (lun11,*)n,x(n),a(n),del,res

   10 continue
c
c   define shifted energy range ...
c
      xmin=x(nmin)-30.d0*gam
      xres=x(nres)
      jmin=1
      jmax=kdim
      jres=jmax
      do 20 i=1,kdim
        axs(i)=0.d0
       e(i)=e(i)-eion
       im=max(1,i-1)
       if(i.gt.1.and.e(im).le.xmin.and.e(i).gt.xmin)
     +    jmin=i-1
       if(i.gt.1.and.e(im).le.xres.and.e(i).gt.xres)
     +    jres=i-1
       if(i.gt.1.and.e(im).lt.0.d0.and.e(i).ge.0.d0)
     +    jmax=i-1
   20 continue
      if(jmin.eq.jmax) jmax=jmin+1
      if (lpri.ne.0) write (lun11,*)'jmin,jres:',jmin,jres,
     $              jmax,nmin,nmax
      do 30 ii=nmin,nmax
       do 30 jj=jmin,jres
c
c   constant-width Lorentzian resonances ...
c
        axtp=a(ii)/pi*gam/2.d0
     +  /((e(jj)-x(ii))**2+(gam/2.d0)**2)
     +  +a(ii)/pi*gam/2.d0/((abs(e(jj))-x(ii))**2
     +   +(gam/2.d0)**2)
c
c   near-threshold pill-up (oscill. strength conservation)
c
        axs(jj)=axs(jj)+axtp
      if ((lpri.ne.0).and.(axs(jj).gt.1.d-24))
     $  write (lun11,*)'30 loop',jj,e(jj),axtp,axs(jj)
   30 continue
c
c   near-threshold extrapolation ...
c
      do ij=jres+1,jmax
        axs(ij)=axs(jres)
        if (lpri.ne.0) write (lun11,*)ij,axs(ij)
      enddo
c
c   scaling of the xs ...
c
      do 40 kk=1,kdim
       axs(kk)=scal*axs(kk)
c
c   return to the "usual" energy grid ...
c
       e(kk)=e(kk)+eion
       if ((lpri.ne.0).and.(axs(kk).gt.1.d-24))
     $  write (lun11,*)kk,e(kk),axs(kk)
   40 continue
c
c
      return
      end
