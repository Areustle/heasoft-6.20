      subroutine xstarcalc(lpri2,lnerrd,nlimdt,
     $       lpri,lprid,lun11,tinf,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,ababs,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)

      implicit none
c     
c
      include './PARAM'
c
c
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
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real*8 elum(3,nnnl),elumo(3,nnnl)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
      real*8 fline(2,nnnl),flinel(ncn)
c     line optical depths
      real*8 tau0(2,nnnl)
c     energy bins
      real*8 epi(ncn)
c      continuum lum
      real*8 zrems(4,ncn),zremso(4,ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     level populations
      real*8 xilev(nnml),bilev(nnml),rniss(nnml)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 elumab(2,nnml),elumabo(2,nnml)
      real*8 tauc(2,nnml)
c     ion abundances
      real*8 xii(nni)
c     heating and cooling
      real*8 htt(nni),cll(nni)
      real*8 rrrt(nni),pirt(nni)
      integer nlevs(nni)
c     element abundances
      real*8 ababs(nl)
c     the saved rates
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
c     compton heating data
      real*8 decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
c     state variables
      real*8 p,r,t,xpx,delr
c     heating-cooling variables
      real*8 httot,cltot,htcomp,clcomp,clbrems,elcter,cllines,
     $     clcont,hmctot
      real*8 trad
      real*8 cfrac,critf,vturbi,xee,tinf
      integer lcdd,ncn2
c     variables associated with thermal equilibrium solution
      integer nmat,ntotit,lnerrd
c     switches
      integer lprid,lpri,nlimdt
c     strings for atomic data read
      integer nlsvn,ncsvn,lun11,np2,lprisv,lpri2
c
c
      lprisv=lpri
      lpri=0
      if (nlimdt.ne.0) then
        call dsec(lnerrd,nlimdt,
     $       lpri,lprid,lun11,tinf,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,ababs,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)
        endif
c       do ll=1,nnml
c         xilev(ll)=0.
c         enddo
c
       if (lpri2.eq.1) lpri=1
       call func(lpri,lun11,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,ababs,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)
       call funcsyn(lpri,lun11,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,ababs,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)
c
c     
       lpri=lprisv
c
c
      return
      end
