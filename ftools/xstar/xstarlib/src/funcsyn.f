      subroutine funcsyn(lpri,lun11,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xiin,rrrts,pirts,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilevt,bilevt,rnist,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)

c
c     calculates opacities and emissivities and does transfer 
c     level populations and integrates continuum emissivities
c     and opacities are assumed as input
c     
c     author: T. Kallman
c
      implicit none
c
      include './PARAM'
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
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
      real*8 zrems(4,ncn),zremso(4,ncn)
      real*8 elum(3,nnnl),elumo(3,nnnl)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
c     line optical depths
      real*8 tau0(2,nnnl)
c     energy bins
      real*8 epi(ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
      real*8 fline(2,nnnl),flinel(ncn)
c     level populations
      real*8 xilevt(nnml),bilevt(nnml),rnist(nnml)
c     ion abundances
      real*8 xiin(nni)
      real*8 rrrts(nni),pirts(nni)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 elumab(2,nnml),elumabo(2,nnml)
      real*8 tauc(2,nnml)
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd)
      real*8 htt(nni),cll(nni)
      integer nlevs(nni)
c     element abundances
      real*8 abel(nl)
c     the saved rates
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
      real*8 decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
      character(1) klev(100,nd)
c
      real*8 rrrt(31),pirt(31),xin(31),xitmp(31)
      real*8 ajisb(2,ndb),cjisb(ndb)
      integer indb(2,ndb)
      real*8 xilev(nd),rniss(nnml)
      real*8 rrcor(nni),pirtt(31)
      real*8 bmat(nd),bmatl(nd)
      real*8 rnisl(nd)
      real*8 x(nd)
      integer ipsv(31),nsup(nd)
      integer lpri,lun11,lcdd,ncn2,np2,ncsvn,nmat,nlsvn
      real*8 httotd,cltotd,hmctotd,elcterd,
     $     htcompd,clcompd,clbremsd
      real*8 vturbi,critf,t,trad,r,delr,xee,xpx,cfrac,p,
     $     hmctot,elcter,cllines,clcont,htcomp,clcomp,clbrems
      real*8 xh1,xh0,httot,cltot
      real*8 rnisum,crith,cltmp,cmp1,cmp2,
     $     cltot2,enelec,httot2,httmp,pirtsum,rniss2,rrrtt,
     $     rtdm,tt1,tt2,xintp,xeltp,ximax,xilast,xintp2,
     $     xisum,xipp,cl,ht
      integer nlev,nindb,lprisv,
     $     jkk,ipmat,ltyp,ldir,llp,imax,ilimh,
     $     lrtyp,lcon,nrdt,nidt,nkdt,ll,jkkl,ipmatsv,
     $     iliml,jk,kl1,mm,kl,kl2,klion,klel,klp,llm,lp,lm,
     $     lprim,lprif,lpril,lpritp,lsum,lsumt,ndtmp,mlel,
     $     ml1,mmt,mllel,mlion,mleltp,mmtmp,nit,nit2,nit3,nitmx,
     $     nitmx2,nlevm,nnz,nnzp,nsp,mlm,np1i,np1r,np1k
c
      lprisv=lpri
      lprif=lpri
      lpritp=0
      if (lprif.ne.0)
     $  write (lun11,*)'in funcsyn, inputs:',t,
     $         xee,xpx,lcdd,p,abel(1),delr
       if (lcdd.ne.1)
     $   xpx = p/1.38e-12/max(t,1.e-24)
c
      xh0=xpx*xiin(1)*abel(1)
      xh1=xpx*(1.-xiin(1))*abel(1)
c
c      zero emissivitiesd and opacities
c      note that here variables 
c      on the level grid  are 
c      already calculated in func3p
       do ll=1,nnnl
         fline(1,ll)=0.
         fline(2,ll)=0.
         rcem(1,ll)=0.
         rcem(2,ll)=0.
         oplin(ll)=0.
         enddo
       do ll=1,ncn2
         rccemis(1,ll)=0.
         rccemis(2,ll)=0.
         opakc(ll)=0.
         opakscatt(ll)=0.
         enddo
       do ll=1,ncn2
         flinel(ll)=0.
         enddo
c
c
c      now calculate.  first step thru elements
       jkk=0
       jkkl=0
       klel=11
       mlel=npfirst(klel)
       jk=0
       xilast=0.
       do while (mlel.ne.0)
         mlm=mlel-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         mllel=0
         if (nidt.gt.0) then
           if (lprif.ne.0)
     $         write (lun11,9339)(kdat1(np1k-1+mm),mm=1,nkdt)
 9339      format (1x, ' element:',12a1)
           mllel=idat1(np1i)
           jk=mllel
           nnz=idat1(np1i)
           nnzp=nnz+1
           xeltp=0.
           if (jk.gt.0) xeltp=abel(jk)
           if (xeltp.gt.1.e-24) then           
c
c            find ion indeces
             if (lprif.ne.0) write (lun11,*)' finding ion indeces'
             klion=12
             mlion=npfirst(klion)
             jkk=0
             kl=0
             do while ((mlion.ne.0).and.(kl.lt.nnz))
               jkk=jkk+1
               mlm=mlion-1
               call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
               mleltp=npar(mlion)
               if (mleltp.eq.mlel) then
                 kl=kl+1                
                 endif
               mlion=npnxt(mlion)
               enddo
c
c            unpack ion fractions
             xisum=0.
             ldir=+1
             do mm=1,kl
                 kl1=mm
                 xin(kl1)=xiin(jkk-kl+kl1)
                 xisum=xisum+xin(kl1)
                 enddo
             xipp=1.-xisum
             klp=kl+1
             xin(klp)=xipp
c
c            find iliml, ilimh
             iliml=0
             ilimh=0
             imax=0
             ximax=0.
             do mm=1,klp
               if (xin(mm).gt.ximax) then
                 ximax=xin(mm)
                 imax=mm
                 endif
               enddo
             imax=max(min(nnz,imax),1)
             llp=imax
             llm=imax
             iliml=imax
             ilimh=imax
             lp=0
             lm=0
             if (imax.ne.klp) then
                 lsumt=nlevs(jkk-kl+imax)
               else
                 lsumt=0
               endif
             ndtmp=nd
             mmt=0
             do while ((lsumt.lt.ndtmp).and.(ldir.ne.0))
               mmt=mmt+1
               lsum=lsumt
               iliml=min(iliml,llm)
               ilimh=max(ilimh,llp)
               if ((llp.ge.klp).or.(xin(llp).lt.critf)) then
                 ldir=-1
                 lp=1
                 endif
               if ((llm.le.1).or.(xin(llm).lt.critf)) then
                 ldir=+1
                 lm=1
                 endif
               if ((lm.ne.1).and.(lp.ne.1)) then
                 if (xin(llp+1).gt.xin(llm-1)) then
                     ldir=+1
                   else
                     ldir=-1
                   endif
                 endif
               if ((lp.eq.1).and.(lm.eq.1)) ldir=0
               if (ldir.eq.+1) then
                   llp=llp+1
                   if (llp.ne.klp) then
                       lsumt=lsum+nlevs(jkk-kl+llp)
                     else
                       lsumt=lsum
                     endif
                   endif
               if (ldir.eq.-1) then
                   llm=llm-1
                   lsumt=lsum+nlevs(jkk-kl+llm)
                   endif
               ilimh=max(ilimh-1,iliml+1)
               enddo
             if (lpri.ne.0) write (lun11,*)'iliml,ilimh:',iliml,ilimh
c
c            step thru ions
             if (lprif.ne.0) write (lun11,*)' third pass',ipmat
             mlion=npfirst(klion)
             ipmat=ipmat+1
             ipmat=0
             jkk=jkk-nnz
             kl=0
             do while ((mlion.ne.0).and.(kl.lt.nnz))
               ltyp=klion
               mlm=mlion-1
               call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
               mleltp=npar(mlion)
               if (mleltp.eq.mlel) then
                 kl=kl+1
                 jkk=jkk+1
                 call func2i(jkk,
     $             idat1,rdat1,kdat1,nptrs,
     $             npfi,npar,npnxt,nlev)
                 if ((kl.ge.iliml).and.(kl.le.ilimh)) then
                   if (lprif.ne.0)
     $              write (lun11,9338)(kdat1(np1k-1+mm),mm=1,nkdt)
9338               format (1x, ' ion:',8a1)
                   call func2l(jkk,lpri,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)                
                   nlevm=nlev-1
c                  retrieve saved abundances
                   do mm=1,nlevm
                     bmatl(mm+ipmat)=xilev(mm)
                     mmtmp=npilev(mm,jkk)
                     if (mmtmp.gt.0) then
                       if (mmtmp.gt.nnml) stop 'mmtmp error'
                       bmatl(mm+ipmat)=xilevt(mmtmp)
                       if (lpri.gt.1)
     $                  write (lun11,*)mm,mmtmp,xilevt(mmtmp)
                       endif
c
c                    nb this code makes H and He fully ionized
c                     if (jkk.le.3) then
c                       bmatl(mm+ipmat)=0.
c                       endif
c
                     enddo
                   mmtmp=npilev(nlev,jkk)
                   if (mmtmp.gt.nnml) stop 'mmtmp error'
                   bmatl(ipmat+nlev)=xilevt(mmtmp)
c
c                  nb this code makes H and He fully ionized
c                   if ((jkk.eq.1).or.(jkk.eq.3))
c     $               bmatl(nlev+ipmat)=1.
c
                   rrcor(jkk)=1.
                   ipmatsv=ipmat+nlev
                   call func3(jkk,jkkl,lpri,lun11,vturbi,
     $                 t,trad,r,delr,xee,xpx,xh1,xh0,cfrac,
     $                 epi,ncn2,bremsa,bremsint,tau0,tauc,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,
     $                 rniss,rlev,ilev,
     $                    nlpt,iltp,nlev,klev,
     $                 xeltp,rrcor(jkk),httmp,cltmp,cllines,clcont,rtdm,
     $                 bmatl,ipmat,ipmatsv,
     $                 rcem,oplin,rccemis,opakc,opakscatt,
     $                 cemab,cabab,opakab,fline,flinel)
c
                   ipmat=ipmat+nlev-1
                   endif
                 endif
c
               mlion=npnxt(mlion)
               enddo
c
             endif

           endif
c
         if  (mlel.ne.0) mlel=npnxt(mlel)
c
         enddo
c
      lpril=0
c     do tranfer.  assumes comp2 and brems have been called 
c     already
      call heatt(jkk,lpri,lun11,
     $       t,r,cfrac,delr,xee,xpx,abel,
     $       epi,ncn2,bremsa,
     $       idat1,rdat1,kdat1,nptrs,
     $       npar,npnxt,npfi,npfirst,nplin,nlsvn,
     $       npconi2,ncsvn,rlev,ilev,nlev,klev,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       rcem,oplin,rccemis,opakc,opakscatt,cemab,fline,flinel,
     $       brcems,cmp1,cmp2,httotd,cltotd,hmctotd,
     $             cllines,clcont,htcompd,clcompd,clbremsd)
c
      if (lprif.ne.0) write (lun11,*)'leaving funcsyn'
c
      lprisv=lpri
c
      return
      end
