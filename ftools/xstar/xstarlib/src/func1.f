      subroutine func1(jkk,kl,nnz,lpri,lun11,vturbi,
     $       t,trad,r,delr,xee,xpx,xh1,xh0,
     $       epi,ncn2,bremsa,bremsint,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rniss,rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       pirt,rrrt2)
c
c     this routine calculates rates affecting ion balance
c     author: T. Kallman
c
      implicit none
c
      include './PARAM'
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
c     energy bins
      real*8 epi(ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rcdum(2,ncn)
      real*8 fline(2,nnnl)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     element abundances
c     the saved rates
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
c     state variables
      real*8 r,t,xpx,delr
c     heating-cooling variables
c     input parameters
      real*8 trad
      real*8 vturbi,xee
      integer ncn2,lpri,lun11,np2
      integer nlsvn,ncsvn
      real*8 rniss(nd)
      real*8 rlev(10,nd)
      integer ilev(10,nd),nlpt(nd),iltp(nd)
      real*8 pirt(31)
      character(1) klev(100,nd)
      character(49) kdesc2
      real*8 tsq,ans1,ans2,xh1,xh0,rrrt2
      real*8 abund1,abund2,ptmp1,ptmp2,ans3,ans4,opakb1
      integer idest1,idest2,idest3,idest4
      integer np1i,np1r,np1k
      integer nnzp,nlevmx,mltype,ml,mllz,nlev,mlpar,
     $  ltyp,lrtyp,lcon,nrdt,nidt,nkdt,mlrdesc,llo,lup,
     $  nnz,mm,jkk,lk,lpriu,kl,mlm,lfpi
c
c      if (lpri.ne.0)
c     $  write (lun11,*)'in func1, inputs:',t,
c     $         xee,xpx,xnx
c
c     lfpi value:  calculate photoionization rates only
      lfpi=1
c
c     zero temporaries
      tsq=sqrt(t)
      nnzp=nnz+1
      do mm=1,nnzp
        pirt(mm)=0.
        enddo
      rrrt2=0.
c     now find all the rates affecting this ion
c     step thru types
      mltype=1
      do while (mltype.lt.ntyp)
        mlrdesc=mltype
        ml=npfi(mltype,jkk)
        if (((mlrdesc.eq.6).or.(mlrdesc.eq.1).or.(mlrdesc.eq.8)
     $    .or.(mlrdesc.eq.15).or.(mlrdesc.eq.7).or.(mlrdesc.eq.42))
     $    .and.(ml.ne.0)) then
          mllz=npar(ml)
          mlpar=npar(ml)
          do while ((ml.ne.0).and.(mlpar.eq.mllz))
c           step thru records of this type
            if (nptrs(3,ml).eq.mlrdesc) then
              mlm=ml-1
              call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $          nptrs,0,lun11)
c              if (lpri.ne.0)  call dprinto(ltyp,lrtyp,lcon,
c     $          nrdt,np1r,nidt,np1i,nkdt,np1k,
c     $          rdat1,idat1,kdat1,lun11)
c             calculate rates
              lpriu=lpri
              abund1=0.
              abund2=0.
              ptmp1=0.
              ptmp2=0.
              idest1=0
              if (lrtyp.eq.7) idest1=idat1(np1i+nidt-2)
              if (((lrtyp.eq.1).and.(ltyp.ne.53))
     $             .or.((lrtyp.eq.7).and.(idest1.eq.1))
     $             .or.(lrtyp.eq.15).or.(lrtyp.eq.42)) then
                 call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $             nrdt,np1r,nidt,np1i,nkdt,np1k,ans1,ans2,
     $             ans3,ans4,idest1,idest2,idest3,idest4,
     $             abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $             opakc,opakscatt,rcdum,fline,lpriu,kdesc2,
     $             r,delr,t,trad,tsq,xee,xh1,xh0,
     $             epi,ncn2,bremsa,bremsint,
     $             rniss,rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates)
                 if (idest1.eq.1) then
                   llo=idest3
                   lup=idest4-idest3
                   pirt(kl+lup)=pirt(kl+lup)+ans1
                   if (lpri.ge.1)
     $              write (lun11,9001)jkk,lrtyp,ltyp,llo,lup+idest3,
     $                      ml,ans1,pirt(kl+lup)
 9001               format (1x,6i6,' ion ',1pe10.3,14x,1pe10.3)
                   endif
                 endif
              if ((lrtyp.eq.6).or.(lrtyp.eq.8)) then
                 call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $             nrdt,np1r,nidt,np1i,nkdt,np1k,ans1,ans2,
     $             ans3,ans4,idest1,idest2,idest3,idest4,
     $             abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $             opakc,opakscatt,rcdum,fline,lpriu,kdesc2,
     $             r,delr,t,trad,tsq,xee,xh1,xh0,
     $             epi,ncn2,bremsa,bremsint,
     $             rniss,rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates)
                 rrrt2=rrrt2+ans1
c                Commented... llo has a funny value...    jg
                 llo=1
                 if (lpri.ge.1)                   
     $              write (lun11,9002)jkk,lrtyp,ltyp,llo,ml,
     $                    ans1,rrrt2
 9002            format (1x,4i6,6x,i6,' ion ',14x,
     $                              1pe10.3,14x,1pe10.4)
                 endif
              endif
            ml=npnxt(ml)
            if (ml.ne.0) mlpar=npar(ml)
            enddo
          endif
        mltype=mltype+1
        enddo
c
c
      return
      end
