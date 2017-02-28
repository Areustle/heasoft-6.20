      subroutine heatf(jkk,lpri,lun11,
     $       t,r,cfrac,delr,xee,xpx,abel,
     $       epi,ncn2,bremsa,
     $       idat1,rdat1,kdat1,nptrs,
     $       npar,npnxt,npfi,npfirst,nplin,nlsvn,
     $       npconi2,ncsvn,rlev,ilev,nlev,klev,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       rcem,oplin,rccemis,opakc,opakscatt,cemab,fline,flinel,
     $       brcems,cmp1,cmp2,httot,cltot,hmctot,
     $             cllines,clcont,htcomp,clcomp,clbrems)
c
c     this routine calculates heating and cooling.
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
c
c     master data
c     global xstar data
c     master data
      integer idat1(nidat1)
      real*8 rdat1(nrdat1)
      integer nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data
      integer npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data
      integer nplin(nnnl)
c     pointers to line data
      integer npconi2(ndat2)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
c     energy bins
      real*8 epi(ncn)
c     continuum flux
      real*8 bremsa(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     state variables
      real*8 r,t,xpx,delr,delrl
c     heating-cooling variables
c     input parameters
      real*8 xee
      integer ncn2,lpri,lun11
      integer nlsvn,ncsvn
      real*8 fline(2,nnnl),flinel(ncn)
c     level populations
      real*8 abel(nl)
      real*8 cemab(2,nnml)
      character(1) kblnk
      real*8 zrems(4,ncn),zremso(4,ncn)
      real*8 elum(3,nnnl),elumo(3,nnnl)
      real*8 elumab(2,nnml),elumabo(2,nnml)
      real*8 xeltp,cllines,clcont,cmp1,cmp2,cltot,
     $     hmctot,htcomp,clcomp,clbrems,etst,ekt,epiio,
     $     epii,fac,fpr2,optpp,optp2,tmpc1,tautmp,cfrac,
     $     tmpc2,tmpho,xnx,httot,tmpc,tmph,tmpco,hmctmp
      real*8 elin,ener,eth,r19,ergsev,tmpscat,hmctmpo
      real*8 hpctot,hpctmp,hpctmpo
      character(1) klev(100,nd)
      real*8 tmp2,tmp2o
      integer lskp
      real*8 rlev(10,nd)
      integer ilev(10,nd)
      integer idest1,jk,klel,kl,
     $     klion,mlleltp,mllel,mlel,mlion,mt2,nilin,nnz,numcon,
     $     nblin
      integer np1i,np1r,np1k,np1ki
      integer nlev,nlevmx,mltype,ml,mllz,jkk,ltyp,
     $     lrtyp,lcon,nrdt,nidt,nkdt,lk,kkkl,
     $     lprisv,mm,nbinc,mlpar,mlm
c
c
      data kblnk/' '/
      data ergsev/1.602197e-12/
c
      lprisv=lpri
c
      xnx=xpx*xee
      if (lpri.ge.1) lpri=2
      if (lpri.gt.1) write (lun11,*)'in heatt',httot,cltot,delr,r
      if (lpri.gt.1) write (lun11,*)ncsvn
      numcon=ncn2
      r19=r*(1.e-19)
      fpr2=12.56*r19*r19
c
c     comment these out to implement scattering
      clbrems=0.
      lskp=1
      tmp2=0.
      do kl=1,numcon
        tmp2o=tmp2
        tmp2 =brcems(kl)
        if ( kl.ge.2 ) clbrems=clbrems+(tmp2+tmp2o)
     &                *(epi(kl)-epi(kl-lskp))*ergsev/2.
        enddo
c
      delrl=delr
c
      fac=delrl
      ekt = t*(0.861707)
      htcomp = cmp1*xnx*ergsev
      clcomp = ekt*cmp2*xnx*ergsev
      httot=httot+htcomp
      cltot=cltot+clcomp+clbrems
      if (lpri.ge.1) write (lun11,9953)htcomp,clcomp,cmp1,cmp2,
     $   clbrems,httot,cltot
      hmctot=2.*(httot-cltot)/(1.e-37+httot+cltot)
      if (lpri.ge.1) write (lun11,*)hmctot
 9953 format (1h , ' compton heating, cooling=',8e12.4)
      lpri=lprisv
c

      return
      end
