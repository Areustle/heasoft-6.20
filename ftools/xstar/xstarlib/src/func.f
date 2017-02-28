      subroutine func(lpri,lun11,vturbi,critf,
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
c     this routine steps through data and calculates
c     new version attempts to avoid rates for unabundant ions
c     author: T. Kallman
c
c     with data structures designed for Lucy's iterative method
c       nsup is a pointer from level n to superlevel N
c
c     no longer calls full func3 in main loop.
c     func3 calls moved to funcsyn
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
      real*8 vturbi,critf,t,trad,r,delr,xee,xpx,cfrac,p,
     $     hmctot,elcter,cllines,clcont,htcomp,clcomp,clbrems
      real*8 xh1,xh0,httot,cltot
      real*8 rnisum,crith,cltmp,cmp1,cmp2,
     $     cltot2,enelec,httot2,httmp,pirtsum,rniss2,rrrtt,
     $     rtdm,tt1,tt2,xintp,xeltp,ximax,xilast,xintp2,
     $     xisum,xipp,cl,ht
      real*8 httotd,cltotd,hmctotd,elcterd,
     $     htcompd,clcompd,clbremsd
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
c      if (lpri.ge.1) lpritp=2
      if (lprif.ne.0)
     $  write (lun11,*)'in func, inputs:',t,
     $         xee,xpx,lcdd,p,abel(1),delr
       if (lcdd.ne.1)
     $   xpx = p/1.38e-12/max(t,1.e-24)
c
      xh0=xpx*xiin(1)*abel(1)
      xh1=xpx*(1.-xiin(1))*abel(1)
c
c      zero emissivitiesd and opacities
       do ll=1,nni
         htt(ll)=0.
         cll(ll)=0.
         xiin(ll)=0.
         enddo
       do ll=1,29
         rrrt(ll)=0.
         pirt(ll)=0.
         enddo
       do ll=1,nnml
         xilevt(ll)=0.
         bilevt(ll)=0.
         cemab(1,ll)=0.
         cemab(2,ll)=0.
         opakab(ll)=0.
         enddo
       elcter=0.
       httot=0.
       cltot=0.
       clcont=0.
       cllines=0.
       do ll=1,nnnl
         rcem(2,ll)=0.
         rcem(1,ll)=0.
         oplin(ll)=0.
         fline(1,ll)=0.
         fline(2,ll)=0.
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
       httot=0.
       cltot=0.
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
c            now step thru ions first pass: func1
             if (lprif.ne.0) write (lun11,*)' first pass'
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
                 call func2l(jkk,lpritp,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)
                 if (lprif.ne.0)
     $            write (lun11,9338)(kdat1(np1k-1+mm),mm=1,nkdt)
9338             format (1x, ' ion:',8a1)
                 if (lprif.ne.0) write (lun11,9328)
                 call func1(jkk,kl,nnz,
     $               lpri,lun11,vturbi,
     $               t,trad,r,delr,xee,xpx,xh1,xh0,
     $               epi,ncn2,bremsa,bremsint,
     $               idat1,rdat1,kdat1,nptrs,np2,
     $               npar,npnxt,npfi,npfirst,
     $               nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $               npconi2,ncsvn,rates,vsav,idrates,
     $               rniss,rlev,ilev,
     $               nlpt,iltp,nlev,klev,
     $               pirtt,rrrtt)
                 rrcor(jkk)=1.
                 pirtsum=0.
                 do mm=kl,nnzp
                   pirtsum=pirtsum+pirtt(mm)
                   enddo
                 pirt(kl)=pirtsum
                 rrrt(kl)=rrrtt
                 endif
               mlion=npnxt(mlion)
               enddo
c
c            do ion balance
             kl2=kl
             call istruc(pirt,rrrt,xin,kl2,lpritp,lun11)
             xisum=0.
             ldir=+1
             do mm=1,kl
                 kl1=mm
                 xiin(jkk-kl+kl1)=xin(kl1)
                 xisum=xisum+xin(kl1)
                 enddo
             xipp=1.-xisum
             klp=kl+1
             xin(klp)=xipp
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
             if (lprif.ne.0) then
                 write (lun11,*)'ion fractions:',iliml,ilimh,lsum
                 write (lun11,*)'ion, pi rate,    rec rate,   fraction'
                 do mm=1,kl
                   write (lun11,9023)mm,pirt(mm),rrrt(mm),xin(mm)
 9023              format (1x,i4,3(1pe10.2))
                   enddo
                 endif
c
c
c            now step thru ions for second pass
             if (lprif.ne.0) write (lun11,*)' second pass'
 9328        format ('     ion    process     d1    d2 ',
     $         '    rec use    ans1      ans2  ',
     $     '   ionization  recombination')
 9329        format ('     ion    process     d1    d2 ',
     $  '             rec use     ans1      ans2     ans3       ans4  ',
     $         '  aji(lo,up) aji(up,lo)',
     $         ' aji(lo,lo) aji(up,up)')
 9330        format ('     ion    process     d1    d2 ',
     $   '             rec use     ans1      ans2     ans3       ans4 ',
     $      '  emiss.    opac.     energy   rec   heat      cool    ')
c
             if (lprif.ne.0) write (lun11,*)'zeroing:',nmat
             do ml1=1,nmat
               bmat(ml1)=0.
               nsup(ml1)=0
               enddo
             klion=12
             mlion=npfirst(klion)
             jkk=0
             kl=0
             nindb=0
             ipmat=0
             nsp=1
             do while ((mlion.ne.0).and.(kl.lt.nnz))
               jkk=jkk+1
               mlm=mlion-1
               call drd(ltyp,lrtyp,lcon,
     $           nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $           nptrs,0,lun11)
               mleltp=npar(mlion)
               if (mleltp.eq.mlel) then
                 kl=kl+1
                 ipsv(kl)=-1
                 if (lprif.ne.0)
     $             write (lun11,9338)(kdat1(np1k-1+mm),mm=1,nkdt)
                 if (lprif.ne.0) write (lun11,9329)
                 if ((kl.ge.iliml).and.(kl.le.ilimh)) then
                   call func2l(jkk,lpritp,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)
                   if (lprif.ne.0) write (lun11,*)'ipmat=',ipmat
                   if (lprif.ne.0) write (lun11,*)'before func2',nindb
                   ipsv(kl)=ipmat
                   rrrtt=rrrt(kl)              
                   call func2(jkk,kl,ilimh,
     $                   lpri,lun11,vturbi,
     $                   t,trad,r,delr,xee,xpx,xh1,xh0,cfrac,
     $                   epi,ncn2,bremsa,bremsint,tau0,tauc,
     $                   idat1,rdat1,kdat1,nptrs,np2,
     $                   npar,npnxt,npfi,npfirst,
     $                   nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                   npconi2,ncsvn,rates,vsav,idrates,
     $                   rniss,rlev,ilev,nmat,
     $                   nlpt,iltp,nlev,klev,ajisb,cjisb,indb,
     $                   rrrtt,ipmat,nindb,
     $                   rcem,oplin,opakc,opakscatt,
     $                   cemab,cabab,opakab,fline,flinel)
c                   call func2a(jkk,kl,ilimh,
c       $                 lpri,lun11,lfpi,vturbi,
c       $                 t,trad,r,xee,xpx,xh1,xh0,
c       $                 epi,ncn2,bremsa,bremsint,
c       $                 idat1,rdat1,kdat1,nptrs,np2,
c       $                 npar,npnxt,npfi,npfirst,
c       $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
c       $                 npconi2,ncsvn,rates,vsav,idrates,
c       $                 rlev,ilev,
c       $                 nlpt,iltp,nlev,klev,
c       $                 ajis,cjis,ipmat,ipsv)
c
c                  condense the reaction matrix to omit levels without type 7 data
c                    some definition:
c                       nlev is the number of levels in the ion (including cont.)
c                       nmat is the maximum index of levels linked by this ion
c                       ipmat is the base index into the master arrays for this ion
c                          i.e. the array elements for  this ion start at ipmat+1
c                          and go to ipmat+nmat
c
                   if (lpri.ge.2) write (lun11,*)'level mapping:'
                   do mm=1,nmat
                     if (mm.le.nlev) then
                       mmtmp=npilev(mm,jkk)
                       if (mmtmp.gt.0) then
                         x(mm+ipmat)=xilevt(mmtmp)
                         endif
                       endif
                     enddo
c                   
c                  set up superlevel pointers
                   nsup(1+ipmat)=nsp
                   nsp=nsp+1
                   do mm=2,nlev-1
                     nsup(mm+ipmat)=nsp
                     enddo
                   nsp=nsp+1
                   ipmat=ipmat+nlev-1
                   if (ipmat.gt.nd) stop 'ipmat too large.
     $                                    Increase critf value.'
                   endif
                 endif
               mlion=npnxt(mlion)
               enddo
c
c
c
             nsup(ipmat+1)=nsp
             x(ipmat+1)=0.
             ipmat=ipmat+1
             nmat=ipmat
c
c            now calculate populations:  second pass, full list
             call remtms(tt1)
             nitmx=200
             nitmx2=200
             lprim=0
c             if (lpri.ne.0) lprim=4
             if (lpri.ne.0) 
     $         write (lun11,*)'before msolvelucy',ipmat
             call msolvelucy(ajisb,cjisb,indb,nindb,nsup,nsp,ipmat,
     $          bmat,x,ht,cl,nit,nit2,nit3,nitmx,nitmx2,lun11,lprim)
             if (lprim.ge.2) 
     $         call chisq(ajisb,cjisb,indb,nindb,
     $                    ipmat,x,lun11,lpri)
             call remtms(tt2)
             if (lpri.gt.0)
     $        write (lun11,981)abs(tt2-tt1),nit,nit2,nit3,ht,cl
 981         format (1x,'after msolvelucy',(1pe11.3),3i4,2(1pe11.3))
             do mm=1,ipmat
               bmatl(mm)=x(mm)
               enddo
             cltot=cltot+cl*xeltp
             httot=httot+ht*xeltp
             htt(jk)=ht*xeltp
             cll(jk)=cl*xeltp
c
c            now calculate lte abundances
c            step thru ions
             if (lprif.ne.0) write (lun11,*)'calculating lte abundances'
             ipmatsv=ipmat
             ipmat=0
             mlion=npfirst(klion)
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
                 if ((kl.ge.iliml).and.(kl.le.ilimh)) then
                   call func2l(jkk,lpri,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)
                   nlevm=nlev-1
                   rnisum=0.
                   do mm=1,nlevm
                     rnisum=rnisum+rniss(mm)
                     enddo
                   rniss2=rniss(nlev)
                   rrrt(kl)=pirt(kl)*rnisum/(rniss2+1.e-28)
                   endif
                 endif
               mlion=npnxt(mlion)
               enddo
             call istruc(pirt,rrrt,xitmp,kl2,lpritp,lun11)
             mlion=npfirst(klion)
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
                 if ((kl.ge.iliml).and.(kl.le.ilimh)) then
                   call func2l(jkk,lpri,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)                
c                  func2l returns the saha levels relative to the continuum
c                  in rniss.  we calculate the lte populations..
                   nlevm=nlev-1
                   if (kl.eq.nnz) nlevm=nlev
                   do mm=1,nlevm
                     rnisl(mm+ipmat)=rniss(mm)
     $                    *(xitmp(kl)+xitmp(kl+1))
                     enddo
                   ipmat=ipmat+nlev-1
                   endif
                 endif
               mlion=npnxt(mlion)
               enddo
c
c            step thru ions
             if (lprif.ne.0) write (lun11,*)' third pass',ipmat
             mlion=npfirst(klion)
             ipmat=ipmat+1
             ipmatsv=ipmat
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
c                this loop avoids a messy error
                 do mm=1,nlev
                   mmtmp=npilev(mm,jkk)
                   if (mmtmp.gt.0) then
                     xilevt(mmtmp)=0.
                     if (lpri.gt.1)
     $                write (lun11,*)mm,mmtmp,xilevt(mmtmp),ipmat,
     $                               mm+ipmat
                     endif
                   enddo
                 pirts(jkk)=pirt(kl)
                 rrrts(jkk)=rrrt(kl)
                 if ((kl.ge.iliml).and.(kl.le.ilimh)) then
                   if (lprif.ne.0)
     $              write (lun11,9338)(kdat1(np1k-1+mm),mm=1,nkdt)
                   if (lprif.ne.0) write (lun11,9330)
                   call func2l(jkk,lpri,lun11,t,xee,xpx,
     $                  idat1,rdat1,kdat1,nptrs,
     $                  npar,npnxt,npfi,
     $                  rniss,rlev,ilev,
     $                  nlpt,iltp,nlev,klev)                
                   nlevm=nlev-1
c                  retrieve saved abundances
                   xintp=0.
                   if (lpri.gt.1) write (lun11,*)'saving populations'
c
c                  nb this code makes H and He fully ionized
c                   if ((jkk.eq.1).or.(jkk.eq.3))
c     $               bmatl(nlev+ipmat)=1.
c
                   do mm=1,nlevm
c
c                    nb this code makes H and He fully ionized
c                     if (jkk.le.3) then
c                       bmatl(mm+ipmat)=0.
c                       endif
c
                     xilev(mm)=bmatl(mm+ipmat)
                     xintp=xintp+xilev(mm)
                     mmtmp=npilev(mm,jkk)
                     if (mmtmp.gt.0) then
                       rnist(mmtmp)=rnisl(mm+ipmat)
                       if (mmtmp.gt.nnml) stop 'mmtmp error'
                       xilevt(mmtmp)=bmatl(mm+ipmat)
                       if (lpri.gt.1)
     $                  write (lun11,*)mm,mmtmp,xilevt(mmtmp)
                       endif
                     enddo
                   xiin(jkk)=xintp
                   xin(kl)=xintp
                   mmtmp=npilev(nlev,jkk)
                   if (mmtmp.gt.nnml) stop 'mmtmp error'
                   xilevt(mmtmp)=bmatl(ipmat+nlev)
                   rnist(mmtmp)=rnisl(ipmat+nlev)
c                  this is an ungraceful solution to this problem
                   xintp2=bmatl(ipmat+nlev)
c                  this is a bad approxmiation...
c                   if (kl.lt.nnz)
c       $              xintp2=xintp2
c       $                 +bmatl(ipmat+nlev+1)
c       $                 +bmatl(ipmat+nlev+2)
                   xilev(nlev)=xintp2
c
                   rrrts(jkk)=pirts(jkk)*xintp/(xintp2+1.e-28)
                   if (lpri.ne.0) then
                     write (lun11,*)'ipmat=',ipmat
                   call func3p(jkk,jkkl,lpri,lun11,vturbi,
     $                 t,trad,r,xee,xpx,xh1,xh0,cfrac,
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
                     endif
c
                   ipmat=ipmat+nlev-1
                   endif
                 endif
c
               mlion=npnxt(mlion)
               enddo
c
             xisum=0.
             do kl1=1,kl
               xisum=xisum+xin(kl1)
               enelec=float(kl1-1)
               elcter=elcter+xin(kl1)*enelec*xeltp
               enddo
             enelec=float(kl)
             elcter=elcter+max(0.,1.-xisum)*enelec*xeltp
c           
             endif

           endif
c
         if  (mlel.ne.0) mlel=npnxt(mlel)
c
         enddo
c
      lpril=0
      call comp2(lpril,lun11,epi,ncn2,bremsa,t,
     $   r,decomp,ecomp,sxcomp,cmp1,cmp2)
c     nonrelativistic compton
c     call comp(lpri,lun11,epi,ncn2,bremsa,r,cmp1,cmp2)
c     ferland compton
c      call comp3(lpri,lun11,epi,ncn2,bremsa,r,cmp1,cmp2)
c      call freef(lpri,lun11,epi,ncn2,t,xpx,xee,opakc)
      call bremem(lpril,lun11,xee,xpx,t,epi,ncn2,brcems,opakc)
      call heatf(jkk,lpri,lun11,
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
       elcter=xee-elcter
c
      if (lprif.ne.0) write (lun11,*)'leaving func'
c
      lprisv=lpri
c
c
      return
      end
