      subroutine func3p(jkk,jkkl,lpri,lun11,vturbi,
     $       t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $       epi,ncn2,bremsa,bremsint,tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rniss,rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       xeltp,rrcor,htt,cll,cllines,clcont,rrrt,
     $       xilev,ipmat,ipmatsv,
     $       rcem,oplin,rccemis,opakc,opakscatt,
     $       cemab,cabab,opakab,fline,flinel)
c
c     this routine prints level populations
c     author: T. Kallman
c
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
c     line emissivities
      real*8 rcem(2,nnnl)
c     line optical depths
      real*8 tau0(2,nnnl)
c     energy bins
      real*8 epi(ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn)
c     line opacities
      real*8 oplin(nnnl)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     element abundances
c     the saved rates
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
c     state variables
      real*8 r,t,xpx
c     heating-cooling variables
c     input parameters
      real*8 trad
      real*8 vturbi,xee
      integer ncn2,lpri,lun11,np2
      integer nlsvn,ncsvn
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd)
      character(1) klev(100,nd)
      character(49) kdesc2
      real*8 fline(2,nnnl),flinel(ncn)
c     level populations
      real*8 xilev(nd)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 tauc(2,nnml)
      character(1) kblnk
      real*8 tsq,ans1,ans2,xh1,xh0,cfrac
      real*8 abund1,abund2,ptmp1,ptmp2,ans3,ans4,opakb1,
     $     xeltp,rrcor,cllines,clcont,htt,cll
      integer idest1,idest2,idest3,idest4
      real*8 rniss(nd)
      real*8 abundtot,rrrt
      real*8 tau1,tau2,e1,e2,pescl,pescv,
     $     cemtmp1,cemtmp2,czzz,elin,ener,htsum,eth,opakbb,
     $     rcemm,rcsum,ergsev
      integer nlev,nlevmx,mltype,ml,mllz,mlrdesc,lpriu,
     $     llo,lup,jkk,ipmat,ltyp,
     $     lrtyp,lcon,nrdt,nidt,nkdt,lk,kkkl,jkkl,ipmatsv,
     $     lprisv,ml3,mm,nb1,nbinc,mlpar,mlm
      integer np1i,np1r,np1k
c
c
      data kblnk/' '/
c
      ergsev=1.602197e-12
c
      abundtot=0.
      rrrt=0.
      tsq=sqrt(t)
c
      lprisv=lpri
c
      if (lpri.gt.0)
     $  write (lun11,*)'in func3p, inputs:',t,
     $         xee,xpx
c
c
      htt=0.
      cll=0.
      if (lpri.ne.0) then
        write (lun11,*)'level populations:'
        do mm=1,nlev
          write (lun11,9022)mm,(klev(ml,mm),ml=1,20),
     $      rlev(1,mm),rlev(2,mm),
     $      xilev(mm+ipmat),rniss(mm)
 9022     format (i4,20a1,4(1pe10.3))
          enddo
        endif
c
c     now do other  rates
      mltype=7
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      mllz=0
      if (ml.ne.0) mllz=npar(ml)
      mlpar=mllz
      do while ((ml.ne.0).and.(mlpar.eq.mllz))
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        idest1=idat1(np1i+nidt-2)
        idest2=nlev+idat1(np1i-1+nidt-3)-1
        kkkl=npconi2(ml)
        if ((kkkl.ne.0).and.(kkkl.le.ndat2)
     $     .and.(idest1.gt.0)) then
          llo=idest1+ipmat
          lup=idest2+ipmat
          eth=rlev(4,idest1)-rlev(1,idest1)
          abund1=xilev(llo)*xeltp
          abund2=xilev(lup)*xeltp
          cabab(kkkl)=cabab(kkkl)*abund1*xpx
          cemab(1,kkkl)=cemab(1,kkkl)*abund2*xpx
          cemab(2,kkkl)=cemab(2,kkkl)*abund2*xpx
          if ((lpri.ge.1))
     $        write (lun11,9002)jkk,lrtyp,ltyp,idest1,idest2,
     $        llo,lup,ml,
     $        cemab(1,kkkl)+cemab(2,kkkl),opakab(kkkl),eth,
     $        kkkl,cll,htt
 9002         format (1x,8i6,' h-cp',
     $          40x,3(1pe10.3),i6,2(1pe10.3),4(1pe10.3))
          endif
        ml=npnxt(ml)
        mlpar=0
        if (ml.ne.0) mlpar=npar(ml)
        enddo
c
      mltype=4
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      if (ml.ne.0) then
        mllz=npar(ml)
        mlpar=npar(ml)
        do while ((ml.ne.0).and.(mlpar.eq.mllz))
             mlm=ml-1
             call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $            nptrs,0,lun11)
             idest1=idat1(np1i)
             idest2=idat1(np1i+1)
             jkkl=nplini(ml)
             if ((rdat1(np1r).gt.0.01).and.(jkkl.ne.0)
     $          .and.(idest1.gt.0).and.(idest2.gt.0).and.
     $          (idest1.lt.nlev).and.(idest2.lt.nlev)) then
               e1=rlev(1,idest1)
               e2=rlev(1,idest2)
               if (e1.lt.e2) then
                   lup=idest2+ipmat
                   llo=idest1+ipmat
                 else
                   lup=idest1+ipmat
                   llo=idest2+ipmat
                 endif
c               if (lpri.ne.0) write (lun11,*)idest1,idest2
               if ((xilev(llo)/(1.e-36+xilev(1+ipmat)).gt.1.e-24).or.
     $             (xilev(lup)/(1.e-36+xilev(1+ipmat)).gt.1.e-24)) then
                 abund1=xilev(llo)*xpx*xeltp
                 abund2=xilev(lup)*xpx*xeltp
                 tau1=tau0(1,jkkl)
                 tau2=tau0(2,jkkl)
                 ptmp1=
     $                pescl(tau1)*(1.-cfrac)
                 ptmp2=pescl(tau2)*(1.-cfrac)+2.*pescl(tau1+tau2)*cfrac
c                 ptmp2=pescl(tau2)*(1.-cfrac)+2.*pescl(tau2)*cfrac
                 lpriu=lpri
                 rcem(1,jkkl)=rcem(1,jkkl)*abund2
                 rcem(2,jkkl)=rcem(2,jkkl)*abund2
                 oplin(jkkl)=oplin(jkkl)*abund1
                 if ((lpri.ge.1))
     $               write (lun11,9002)jkk,lrtyp,
     $                 ltyp,idest1,idest2,
     $                 llo,lup,ml,
     $                 rcem(1,jkkl)+rcem(2,jkkl),oplin(jkkl),
     $                 rdat1(np1r),jkkl,cll,htt
     $                  ,ptmp1,ptmp2
                 endif
               endif
             ml=npnxt(ml)
             if (ml.ne.0) mlpar=npar(ml)
             enddo
        endif
c
      lpri=lprisv
c
c
      return
      end
