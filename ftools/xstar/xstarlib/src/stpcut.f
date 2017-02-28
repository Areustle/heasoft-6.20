      subroutine stpcut(ldirt,lpri,lun11,vturbi,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $      epi,ncn2,opakc,oplin,opakab,delr,t,
     $      dpthc,tau0,tauc,eliml,elimh)
c
c     this routine updates.  calculates depths, etc.
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
      integer nbtpp
      parameter (nbtpp=10000)
c
c     master data
      integer idat1(nidat1),
     $      nptrs(nptt,ndat2)
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
      integer nplin(nnnl),nplini(nnnl)
      real*8 epi(ncn),opakc(ncn),dpthc(2,ncn)
      real*8 tauc(2,nnml)
      real*8 opakab(nnml)
c     line opacities
      real*8 oplin(nnnl)
c     line optical depths
      real*8 tau0(2,nnnl)
      real*8 optmp(ncn)
      integer ldon(2)
c
      real*8 vturbi,delr,t,eliml,elimh,
     $  dpcrit,bbb,optpp,delea,aatmp,elin,etmp,vth,
     $  vturb,deleturb,deleth,dele,aasmall,
     $  deleused,deleepi,delet,deletpp,e00,dpthmx,
     $  dpthtmp,e0,tst,opsum,optmpo,profile,optp2,
     $  tmpopmx,tmpopo,etptst
      integer ldirt,lpri,lun11,
     $  ncn2,llk,lnn,ln,ml,lup,nilin,nelin,
     $  nbtmp,iion,nitmp,ndtmp,mllz,
     $  iltmp,nlsvn,nlsv,i,ij,lind,
     $  lcon,ldir,mlm,ml1m,ltyp,lrtyp,ml1,ml2,ml1min,
     $  ml1max,mlc,mloff,mlmin,mlmax,ncut,nidt,
     $  nkdt,nrdt,np2,ncsvn
      integer nbinc,mlpar
      real*8 voigte
c     arrays containing printout info
      integer ilsv(nnnl)
      real*8 ewsv(nnnl)
      integer np1i,np1r,np1k
      real*8 oplin2(nnnl)
c
c     temporary grid for use in calculating profile
      real*8 etpp(nbtpp),optpp2(nbtpp)
c
c      real*8  tmpew,tmpewo,tmpop,tmpe,sum,sume
      real*8 tmpew,tmpewo,tmpop,tmpe,sum,sume,rnormchk
c
c     Not used
      integer javi
c
      javi=np2
      javi=npfirst(1)
      javi=nplini(1)
      javi=npcon(1)
      javi=npconi(1)
      javi=npilev(1,1)
      javi=npilevi(1)
      javi=npconi2(1)
      npconi2(1)=javi
c
      if (lpri.ne.0)
     $ write (lun11,*)'in stpcut:',delr,nlsvn
c
      do llk=1,ncn2
        optmp(llk)=0.
        enddo
c
c
c     calculate continuum depths
      if (lpri.ne.0) write (lun11,*)'calculating depths in stpcuta'
      lind=1
      if (ldirt.gt.0) lind=2
      dpthmx=0.
      optpp=0.
      do  i = 1,ncn2
c         opakc(i)=opakc(i)+optmp(i)
         optpp=min(optmp(i),1.e+3/(1.e-24+delr))
         dpthtmp=(opakc(i)+optpp)*delr
         if (lpri.ne.0) write (lun11,*)i,epi(i),opakc(i),optmp(i),
     $        dpthc(lind,i),dpthtmp
         dpthc(lind,i) = dpthc(lind,i) + dpthtmp
         if (dpthtmp.gt.dpthmx) then
           dpthmx=dpthtmp
           endif
         enddo
c
c     calculate line depths
      do  i = 1,nlsvn
         tau0(lind,i) = tau0(lind,i) + oplin(i)*delr
         if (lpri.ne.0)
     $    write (lun11,*)i,lind,oplin(i),delr,tau0(lind,i)
         enddo
c
c     calculate level depths
      if (lpri.ne.0)
     $ write (lun11,*)'in stpcut:',delr,lind
      do i = 1,ncsvn
         tauc(lind,i) = tauc(lind,i) + opakab(i)*delr
         if (lpri.ne.0)
     $    write (lun11,*)i,opakab(i),tauc(lind,i)
         enddo
c
c
      return
      end
