      subroutine savd(jkstep,ldir,
     $       lpri,iunit,iunit2,iunit3,iunit4,
     $       idat1,rdat1,kdat1,nptrs,npnxt,npfi,
     $       npfirst,npar,npilev,npconi2,ncsvn,
     $       t,p,r,rdel,delr,xcol,xee,xpx,zeta,
     $       xilev,bilev,rniss,abel,
     $       nplin,nlsvn,rcem,oplin,tau0,
     $       cemab,cabab,opakab,tauc,
     $       epi,ncn2,dpthc,opakc,rccemis,nloopctl,
     $       lunlog,status)
c
c     this routine  saves only depths for iterative calculation
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
C     Allocation for passed parameters
      real*8 rdat1(nrdat1)
      real*8 r,delr,rdel, t, p, xcol,xee,xpx,zeta
      integer unit,hdunum, nrows, status, nloopctl
      integer iunit,iunit2,iunit3,iunit4
      integer idat1(nidat1),nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
      integer nlsvn,ncsvn
c     energy bins
      real*8 epi(ncn)
c     continuum opacities
      real*8 opakc(ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn)
      integer ncn2
c     line opacities
      real*8 oplin(nnnl)
      real*8 abel(nl)
      real*8 tauc(2,nnml)
      real*8 cemab(2,nnml),opakab(nnml),cabab(nnml)
c     pointers to master data
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer npilev(nd,nni)
      integer nplin(nnnl)
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      real*8 xilev(nnml),bilev(nnml),rniss(nnml)
      real*8 tau0(2,nnnl), rcem(2,nnnl)

c     continuum optical depths
      integer ldir,lpri,lun,lunlog,jkstep
      integer lind1,lind2,kl,ll,nlyc,nry,nbinc
c
      if (lpri.ge.1)
     $ write (lunlog,*)'in savd',jkstep,iunit,iunit2,iunit3,iunit4
      call fstepr(iunit,jkstep,r,delr,rdel,t,p,
     $          xcol,xee,xpx,zeta,
     $          idat1,rdat1,kdat1,nptrs,npnxt,npfi,
     $          npfirst,npar,npilev,
     $          xilev,bilev,rniss,nloopctl,
     $          lunlog,lpri,status)
      call fstepr2(iunit2,jkstep,r,delr,rdel,t,p,
     $          xcol,xee,xpx,zeta,
     $          idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $          nplin,nlsvn,rcem,oplin,tau0,nloopctl,
     $          lunlog,lpri,status)
      call fstepr3(iunit3,jkstep,r,delr,rdel,t,p,abel,
     $          xcol,xee,xpx,zeta,
     $          idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $          npfirst,npilev,npconi2,ncsvn,
     $          rniss,cemab,cabab,opakab,tauc,nloopctl,
     $          lunlog,lpri,status)
      call fstepr4(iunit4,jkstep,r,delr,rdel,t,p,
     $          xcol,xee,xpx,zeta,
     $          idat1,rdat1,kdat1,nptrs,npnxt,npfi,npar,
     $          epi,ncn2,dpthc,opakc,rccemis,nloopctl,
     $          lunlog,lpri,status)
      if (status .gt. 0)call printerror(lunlog,status)
      nlyc=nbinc(13.7d0,epi,ncn2)
      nry=nlyc+1
      if (lpri.ne.0) write (lunlog,*)'in savd',rdel,t,tauc(1,25),
     $                ldir,dpthc(1,nry),dpthc(2,nry)
c
      return
      end
